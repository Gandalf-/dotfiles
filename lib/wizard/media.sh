#!/bin/bash

cloudflare::request() {

  method="$1"
  target="$2"
  shift
  shift

  case "$target" in
    https://*)
      ;;
    *)
      target="https://api.cloudflare.com/client/v4/$target"
      ;;
  esac

  local token; token="$( d wizard cloudflare token )"
  [[ $token ]] ||
    common::error "Need token at: wizard cloudflare token"

  curl \
    --silent \
    -X "$method" "$target" \
    -H "Authorization: Bearer $token" \
    -H "Content-Type:application/json" \
    "$@"
}

cloudflare::fetch-zone() {

  cloudflare::request GET zones \
    | python3 -c '
import json
import sys
from apocrypha.client import Client

data = json.load(sys.stdin)
zone = data.get("result", [None])[0]
Client().set("wizard", "cloudflare", "zones", value=zone)
'
}

cloudflare::purge() {

  local zone; zone="$( d wizard cloudflare zones id )"
  [[ $zone ]] || {
    cloudflare::fetch-zone
    zone="$( d wizard cloudflare zones id )"
  }

  [[ $zone ]] || common::error "Couldn't find the Zone ID"

  cloudflare::request POST \
    zones/"$zone"/purge_cache \
    --data '{"purge_everything":true}'
}

# public

common::dir-exists ~/google_drive &&
wizard_media_public_create() {

  common::optional-help "$1" "

  this calls indexer, which produces the html and thumbnails for
  public.anardil.net.
  "
  common::program-exists -f 'indexer'

  common::cd ~/google_drive/share/
  indexer ~/google_drive/code/haskell/indexer/thumbnail.py || exit 0
}

common::dir-exists ~/google_drive &&
wizard_media_public_upload_all() {

  common::optional-help "$1" "[s3cmd config]

  synchronize state between public.anardil.net and ~/google_drive/share.

  default config is ~/.s3cfg-sfo
  "
  common::program-exists -f 's3cmd'

  local config="$HOME"/.s3cfg-sfo
  [[ $1 ]] && config="$1"

  upload() {
    common::do s3cmd sync -c "$config" \
        --no-mime-magic \
        --guess-mime-type \
        --delete-removed \
        --follow-symlinks \
        --recursive \
        --exclude-from ~/working/config/s3cmd-exclude \
        --acl-public \
        "$1" \
        s3://anardil-public/share/
  }

  upload ~/google_drive/share/.indexes &
  upload ~/google_drive/share/.thumbnails &

  for folder in ~/google_drive/share/* ; do
    common::dir-exists "$folder" || {
      echo "ignoring $folder"
      continue
    }
    upload "$folder" &
  done

  wait
}

common::dir-exists ~/working &&
wizard::upload() {
  [[ $config ]] || common::error "programming error"

  common::do s3cmd sync -c "$config" \
    --acl-public \
    --delete-removed \
    --exclude-from ~/working/config/s3cmd-exclude \
    --follow-symlinks \
    --guess-mime-type \
    --no-mime-magic \
    --recursive \
    "$1" \
    s3://anardil-public/share/
}

# dynamic commands for each directory in share

common::dir-exists ~/google_drive/share/ && {

  cd ~/google_drive/share/ ||
    common::error "does it exist or not?"

  for directory in * .*; do

    [[ "$directory" == . ]] && continue
    [[ "$directory" == .. ]] && continue
    common::dir-exists "$directory" || continue

    eval '
wizard_media_public_upload_'"$directory"'() {

  common::optional-help "$1" "[s3cmd config]

  synchronize '"$directory"' to public.anardil.net
  "
  local config="$HOME"/.s3cfg-sfo

  wizard::upload ~/google_drive/share/'"$directory"'
}
    '
  done
}


# diving

common::dir-exists ~/google_drive &&
wizard_media_diving_create() {

  common::optional-help "$1" "

  generate the html and thumbnails for diving.anardil.net.
  "
  common::program-exists -f 'convert'

  if common::dir-exists /mnt/zfs/Media/; then
    base=/mnt/zfs/Media
  elif common::dir-exists "$HOME"/media/; then
    base="$HOME"/media
  else
    common::error "Can't find media directory"
  fi

  common::cd ~/working/object-publish/diving-web

  common::do \
    bash \
    ~/google_drive/code/shell/diving/runner.sh \
    "$base"/Pictures/Diving/

  common::do \
    python3 \
    ~/google_drive/code/shell/diving/gallery.py \
    "$base"/Pictures/Diving/
}

wizard_media_diving_purge_cloudflare() {

  common::optional-help "$1" "

  purge the CDN for cloudflare
  "
  common::echo "purging cloudflare cache"
  cloudflare::purge
}

wizard_media_diving_purge_nginx() {

  common::optional-help "$1" "

  purge the caches for nginx
  "
  ssh walnut \
    docker exec diving /bin/sh -c 'rm -rf /tmp/example-cache/*'
}

wizard_media_diving_purge_all() {
  common::optional-help "$1" "

  purge everything in order
  "
  wizard_media_diving_purge_digital-ocean ''
  common::sleep 60

  wizard_media_diving_purge_cloudflare ''
  common::sleep 30

  wizard_media_diving_purge_nginx ''
}

wizard_media_diving_upload() {
  media::sync diving
}


# photos, artwork

media::create() {
  local name="$1"
  local data="$2"
  common::program-exists -f 'convert'

  common::cd ~/working/object-publish/"$name"-web
  common::do bash \
    ~/google_drive/code/shell/"$name"/runner.sh \
    /mnt/zfs/Media/Pictures/"$data"/
}

media::sync() {
  local name="$1"
  rsync -avL --delete \
    ~/working/object-publish/"$name"-web/ \
    walnut:local/"$name"-web
}

wizard_media_photos_create() {
  media::create photos Photography
}

wizard_media_artwork_create() {
  media::create artwork Artwork
}

wizard_media_photos_upload() {
  media::sync photos
}

wizard_media_artwork_upload() {
  media::sync artwork
}


# sensors

common::dir-exists ~/google_drive &&
wizard_media_sensors_create_basic() {

  common::optional-help "$1" "

  create the html charts for all sensor data.
  "
  common::do MPLBACKEND=Agg python3 \
    ~/google_drive/code/python/sensors/sensors.py
}


common::dir-exists ~/google_drive &&
wizard_media_sensors_create_extremes() {

  common::optional-help "$1" "

  create the html charts for weekly extremes
  "
  common::do MPLBACKEND=Agg python3 \
    ~/google_drive/code/python/sensors/sensor-extremes.py \
    ~/google_drive/share/sensors/extremes/
}


# blog

wizard_media_blog_dependencies() {

   common::do python3 -m pip \
     install --user --upgrade \
     markdown \
     pelican \
     s3cmd
}


common::program-exists 'pelican' &&
wizard_media_blog_create() {

  common::file-exists pelicanconf.py ||
    common::error "Not in directory with source content"

  local tmp=/dev/shm/www/

  common::do mkdir -p "$tmp"
  common::do pelican --ignore-cache -o "$tmp" -t alchemy
}

common::program-exists 's3cmd' &&
wizard_media_blog_upload() {

  common::file-exists robots.txt ||
    common::error "Not in directory with html output"

  common::do s3cmd sync -c ~/.s3cfg-sfo \
    --no-mime-magic \
    --guess-mime-type \
    --delete-removed \
    --recursive \
    --acl-public \
    "$(pwd)"/* \
    s3://mirror/web/blog/
}
