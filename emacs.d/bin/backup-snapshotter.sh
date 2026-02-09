#!/bin/bash

# Checks the backup directory hourly and create a snapshot zip file
# daily.
BACKUP_DIRNAME="backup"
BACKUP_DIR="$HOME/$BACKUP_DIRNAME"
SNAPSHOT_DIR="$HOME/backup-snapshots"
TMP_COMPARE_DIR="$HOME/tmp/backup-snapshot-to-compare"
SNAPSHOTS_LIMIT=90

mkdir -p $SNAPSHOT_DIR
while true; do
    SNAPSHOT_FILE="$SNAPSHOT_DIR/$BACKUP_DIRNAME-$(date +%Y%m%d).zip"
    if [[ ! -f "$SNAPSHOT_FILE" ]]; then
        while true; do
	    echo "$(date): Making snapshot $SNAPSHOT_FILE"
	    cd
	    zip -r "$SNAPSHOT_FILE" "$BACKUP_DIRNAME" && echo "Done.  Waiting for integrity check."
            # Wait for a minute, unzip the archive and check for
            # integrity.  This is to detect race condition where the
            # files are being written at the same time the snapshot is
            # made.
            sleep 60
            rm -rf "$TMP_COMPARE_DIR"
            mkdir -p "$TMP_COMPARE_DIR"
            cd "$TMP_COMPARE_DIR" &&\
                unzip "$SNAPSHOT_FILE" && cd "$BACKUP_DIRNAME" && diff -rq . "$BACKUP_DIR" &&\
                echo "Integrity check successful.  Waiting for next scheduled snapshot." &&\
                break
            echo "Integrity check failed.  Will retry."
            rm "$SNAPSHOT_FILE"
        done
        # Delete old files if number has reached limit
	cd "$SNAPSHOT_DIR"
	while [[ $(ls -1 | wc -l) -gt $SNAPSHOTS_LIMIT ]]; do
	    OLDEST_FILE="$(ls -1 | sort | head -1)"
	    echo "Removing $OLDEST_FILE"
	    rm "$OLDEST_FILE"
	done
    else
	echo "$(date): Checked: $SNAPSHOT_FILE already exists"
    fi
    sleep 3600
done
