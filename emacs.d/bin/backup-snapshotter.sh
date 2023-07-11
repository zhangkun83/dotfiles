#!/bin/bash

# Checks the backup directory hourly and create a snapshot zip file
# daily.
BACKUP_DIRNAME=backup
SNAPSHOT_DIR=~/backup-snaphots
SNAPSHOTS_LIMIT=90
mkdir -p $SNAPSHOT_DIR
while true; do
    SNAPSHOT_FILE="$SNAPSHOT_DIR/$BACKUP_DIRNAME-$(date +%Y%m%d).zip"
    if [[ ! -f "$SNAPSHOT_FILE" ]]; then
	echo "$(date): Making snapshot $SNAPSHOT_FILE"
	cd
	zip -r "$SNAPSHOT_FILE" "$BACKUP_DIRNAME" && echo "Done"
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
