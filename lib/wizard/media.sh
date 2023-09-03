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
  indexer \
    /mnt/ssd/hosts/web/public \
    ~/google_drive/code/haskell/indexer/thumbnail.py \
    || exit 0
}

common::dir-exists ~/google_drive &&
wizard_media_public_upload() {

  common::optional-help "$1" "

  synchronize state between public.anardil.net and ~/google_drive/share.
  "
  rclone sync \
    --exclude-from ~/working/config/rclone-exclude.conf \
    --copy-links \
    --fast-list \
    --progress \
    ~/google_drive/share \
    sfo3:public-anardil/share
}

# diving

common::dir-exists ~/google_drive &&
wizard_media_diving_create() {

  common::optional-help "$1" "

  generate the html and thumbnails for diving.anardil.net.
  "
  common::program-exists -f 'convert'

  if common::dir-exists /mnt/zfs/Media/; then
    base=/mnt/zfs/Media/Pictures/Diving/

  elif common::dir-exists "$HOME"/Pictures/; then
    base="$HOME"/Pictures/diving/

  else
    common::error "Can't find media directory"
  fi

  common::cd ~/working/object-publish/diving-web

  common::do \
    bash \
    ~/google_drive/code/shell/diving/runner.sh \
    "$base"

  common::do \
    python3 \
    ~/google_drive/code/shell/diving/gallery.py \
    "$base"
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

  common::cd /mnt/ssd/hosts/web/"$name"
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

  common::do s3cmd sync \
    --no-mime-magic \
    --guess-mime-type \
    --delete-removed \
    --recursive \
    --acl-public \
    "$(pwd)"/* \
    s3://mirror/web/blog/
}
