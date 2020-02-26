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

wizard_media_public_upload() {

  common::optional-help "$1" "[s3cmd config]

  synchronize state between public.anardil.net and ~/google_drive/share.

  default config is ~/.s3cfg-sfo
  "
  common::program-exists -f 's3cmd'

  sync() {
    local config="$1"

    common::do \
      s3cmd sync -c "$config" \
      --no-mime-magic --guess-mime-type \
      --delete-removed --follow-symlinks --recursive \
      --exclude-from ~/working/s3cmd-exclude \
      --include '*/.indexes/*' --acl-public \
      ~/google_drive/share/ \
      s3://anardil-public/share/
  }

  local config="$HOME"/.s3cfg-sfo
  [[ $1 ]] && config="$1"

  sync "$config"
}


# diving

wizard_media_diving_create() {

  common::optional-help "$1" "

  generate the html and thumbnails for diving.anardil.net.
  "
  common::program-exists -f 'convert'

  common::cd ~/working/diving-web
  common::do \
    bash \
    ~/google_drive/code/shell/diving/runner.sh \
    ~/media/Pictures/Diving/
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
    --no-mime-magic --guess-mime-type \
    --delete-removed --acl-public \
    ~/working/diving-web/ \
    s3://diving/
}


# sensors

wizard_media_sensors_create() {

  common::optional-help "$1" "

  create the html charts for all sensor data. extremely CPU heavy
  "
  grep time ~/working/pi/sensors/elm-pings.csv \
    | tr -cd '[:digit:]. \n' \
    | awk '{print $1, $6}' > ~/working/pi/ping-data.csv

  common::do python3 \
    ~/google_drive/code/python/sensors/sensor-web.py \
    2>/dev/null
}

wizard_media_sensors_upload() {

  common::optional-help "$1" "[s3cmd config]

  upload sensor data to public.anardil.net.

  default config is ~/.s3cfg-sfo
  "
  sync() {
    local config="$1"

    common::do \
      s3cmd sync -c "$config" \
      --no-mime-magic --guess-mime-type \
      --delete-removed --follow-symlinks --recursive \
      --exclude-from ~/working/s3cmd-exclude \
      --acl-public --quiet \
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
    "$(pwd)" \
    s3://mirror/web/blog/
}