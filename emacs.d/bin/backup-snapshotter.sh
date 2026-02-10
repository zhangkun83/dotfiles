#!/bin/bash

# Checks the backup directory hourly and create a snapshot zip file
# daily.
BACKUP_DIRNAME="backup"
BACKUP_DIR="$HOME/$BACKUP_DIRNAME"
SNAPSHOT_DIR="$HOME/backup-snapshots"
TMP_COMPARE_DIR="$HOME/tmp/backup-snapshot-to-compare"
SNAPSHOTS_LIMIT=90

check_consistency () {
    if [[ -z "$1" ]]; then
        echo "No snapshot file specified."
        return 1
    fi
    rm -rf "$TMP_COMPARE_DIR" &&\
        mkdir -p "$TMP_COMPARE_DIR" &&\
        unzip -q -d "$TMP_COMPARE_DIR" "$1" &&\
        diff -rq "$TMP_COMPARE_DIR/$BACKUP_DIRNAME" "$BACKUP_DIR"
}

mkdir -p $SNAPSHOT_DIR
while true; do
    SNAPSHOT_FILE="$SNAPSHOT_DIR/$BACKUP_DIRNAME-$(date +%Y%m%d-%H).zip"
    if [[ ! -f "$SNAPSHOT_FILE" ]]; then
        LATEST_FILE=$(find "$SNAPSHOT_DIR/" -type f | sort | tail -1)
        echo "$(date): Time to create $SNAPSHOT_FILE"
        if check_consistency "$LATEST_FILE"; then
	    echo "$(date): No change since last snapshot $LATEST_FILE"
        else
            while true; do
	        echo "$(date): Making snapshot $SNAPSHOT_FILE"
	        cd
	        zip -rq "$SNAPSHOT_FILE" "$BACKUP_DIRNAME" &&\
                    echo "$(date): Done.  Waiting for integrity check."
                # Wait for a minute, unzip the archive and check for
                # integrity.  This is to detect race condition where the
                # files are being written at the same time the snapshot is
                # made.
                sleep 60
                check_consistency "$SNAPSHOT_FILE" &&\
                    echo "$(date): Integrity check successful." &&\
                    break
                echo "$(date): Integrity check failed.  Will retry."
                rm "$SNAPSHOT_FILE"
            done
            # Delete old files if number has reached limit
	    cd "$SNAPSHOT_DIR"
	    while [[ $(ls -1 | wc -l) -gt $SNAPSHOTS_LIMIT ]]; do
	        OLDEST_FILE="$(ls -1 | sort | head -1)"
	        echo "$(date): Removing $OLDEST_FILE"
	        rm "$OLDEST_FILE"
	    done
        fi
    else
	echo "$(date): $SNAPSHOT_FILE already exists"
    fi
    echo "$(date): Waiting for next scheduled snapshot."
    sleep 1800
done
