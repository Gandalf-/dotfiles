#!/bin/bash

# public

common::dir-exists ~/google_drive &&
wizard_media_public_create() {

  common::optional-help "$1" "

  this calls indexer, which produces the html and thumbnails for
  public.anardil.net.
  "
  common::program-exists -f 'indexer'

  common::cd ~/working/public/
  indexer \
    /mnt/ssd/hosts/web/public \
    ~/google_drive/code/haskell/indexer/thumbnail.py \
    || exit 0
}

common::dir-exists ~/google_drive &&
wizard_media_public_upload() {

  common::optional-help "$1" "

  synchronize state between public.anardil.net and ~/working/share.
  "
  rclone sync \
    --exclude-from ~/working/config/rclone-exclude.conf \
    --copy-links \
    --fast-list \
    --progress \
    ~/working/public \
    sfo3:public-anardil/share
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
