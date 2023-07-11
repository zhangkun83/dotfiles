#!/bin/bash
BACKUP_DIRNAME=backup
SNAPSHOT_DIR=~/backup-snaphots
mkdir -p $SNAPSHOT_DIR
cd
while true; do
    SNAPSHOT_FILE="$SNAPSHOT_DIR/$BACKUP_DIRNAME-$(date +%Y%m%d).zip"
    if [[ ! -f "$SNAPSHOT_FILE" ]]; then
	echo "Making snapshot $SNAPSHOT_FILE"
	zip -r "$SNAPSHOT_FILE" "$BACKUP_DIRNAME" && echo "Done"
    fi
    sleep 3600
done
