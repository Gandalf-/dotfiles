#!/bin/bash


# public

wizard_media_public_create() {

  common::optional-help "$1" "

  this calls indexer, which produces the html and thumbnails for
  public.anardil.net.
  "
  common::program-exists -f 'indexer'

  common::cd ~/google_drive/share/
  indexer ~/google_drive/code/haskell/indexer/thumbnail.py || exit 0
}

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
        --no-check-md5 \
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

wizard::upload() {
  [[ $config ]] || common::error "programming error"

  common::do \
    s3cmd sync -c "$config" \
    --no-mime-magic --guess-mime-type \
    --delete-removed --follow-symlinks --recursive \
    --exclude-from ~/working/config/s3cmd-exclude \
    --acl-public \
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

  common::cd ~/working/diving-web

  common::do \
    bash \
    ~/google_drive/code/shell/diving/runner.sh \
    "$base"/Pictures/Diving/

  common::do \
    python3 \
    ~/google_drive/code/shell/diving/gallery.py \
    "$base"/Pictures/Diving/
}

wizard_media_diving_upload() {

  common::optional-help "$1" "[s3cmd config]

  upload the generated html files for diving.anardil.net.

  default config is ~/.s3cfg-sfo
  "
  common::program-exists -f 's3cmd'

  local config="$HOME"/.s3cfg-sfo
  [[ $1 ]] && config="$1"

  common::do s3cmd sync \
    -c "$config" \
    --no-mime-magic \
    --guess-mime-type \
    --delete-removed \
    --acl-public \
    --follow-symlinks \
    ~/working/diving-web/ \
    s3://diving/
}


# photos

wizard_media_photos_create() {

  common::optional-help "$1" "

  generate the html and thumbnails for photos.anardil.net.
  "
  common::program-exists -f 'convert'

  if common::dir-exists /mnt/zfs/Media/; then
    base=/mnt/zfs/Media
  elif common::dir-exists "$HOME"/media/; then
    base="$HOME"/media
  else
    common::error "Can't find media directory"
  fi

  common::cd ~/working/photos-web
  common::do \
    bash \
    ~/google_drive/code/shell/photos/runner.sh \
    "$base"/Pictures/Photography/
}

wizard_media_photos_upload() {

  common::optional-help "$1" "[s3cmd config]

  upload the generated html files for photos.anardil.net.

  default config is ~/.s3cfg-sfo
  "
  common::program-exists -f 's3cmd'

  local config="$HOME"/.s3cfg-sfo
  [[ $1 ]] && config="$1"

  common::do s3cmd sync \
    -c "$config" \
    --no-mime-magic --guess-mime-type \
    --delete-removed --acl-public \
    ~/working/photos-web/ \
    s3://anardil-photos/
}


# sensors

wizard_media_sensors_create_basic() {

  common::optional-help "$1" "

  create the html charts for all sensor data.
  "
  common::do MPLBACKEND=Agg python3 \
    ~/google_drive/code/python/sensors/sensors.py
}


wizard_media_sensors_create_extremes() {

  common::optional-help "$1" "

  create the html charts for weekly extremes
  "
  common::do MPLBACKEND=Agg python3 \
    ~/google_drive/code/python/sensors/sensor-extremes.py \
    ~/google_drive/share/sensors/extremes/
}


wizard_media_sensors_upload() {

  common::optional-help "$1" "[s3cmd config]

  upload sensor data to public.anardil.net.

  default config is ~/.s3cfg-sfo
  "
  sync() {
    local config="$1"

    common::do s3cmd sync -c "$config" \
      --no-mime-magic --guess-mime-type \
      --delete-removed --follow-symlinks --recursive \
      --exclude-from ~/working/config/s3cmd-exclude \
      --acl-public --quiet \
      --no-check-md5 \
      ~/google_drive/share/sensors/ \
      s3://anardil-public/share/sensors/
  }

  local config="$HOME"/.s3cfg-sfo
  [[ $1 ]] && config="$1"

  sync "$config"
}


# blog

wizard_media_blog_dependencies() {

   common::do python3 -m pip \
     install --user --upgrade \
     markdown \
     pelican \
     s3cmd
}

wizard_media_blog_create() {

  common::program-exists -f 'pelican'

  common::file-exists pelicanconf.py ||
    common::error "Not in directory with source content"

  local tmp=/dev/shm/www/

  common::do mkdir -p "$tmp"
  common::do pelican --ignore-cache -o "$tmp" -t alchemy
}

wizard_media_blog_upload() {

  common::program-exists -f 's3cmd'

  common::file-exists robots.txt ||
    common::error "Not in directory with html output"

  common::do s3cmd sync \
    -c ~/.s3cfg-sfo \
    --no-mime-magic \
    --guess-mime-type \
    --delete-removed \
    --recursive \
    --acl-public \
    "$(pwd)"/* \
    s3://mirror/web/blog/
}
