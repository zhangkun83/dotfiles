#!/bin/bash
script_path="$(readlink -f "$0")"
script_dir="$(dirname "$script_path")"
parentdir="$(dirname "$script_dir")"
WALLPAPER_SRC_DIR="$parentdir/runtime/wallpapers"
wallpaper_download_dir="$parentdir/runtime"
wallpaper_download_path="$wallpaper_download_dir/downloaded_wallpaper"

# Try to download a random wallpaper from a website
mkdir -p "$wallpaper_download_dir"
wget https://source.unsplash.com/random/1920x1080 --no-verbose \
     --output-document="$wallpaper_download_path" \
    && wallpaper_path="$wallpaper_download_path"

if [[ -z "$wallpaper_path" ]]; then
    echo "prepare-wallpaper.sh: Download failed. Trying to use local wallpapers"
    wallpaper=$(ls -1 "$WALLPAPER_SRC_DIR" | shuf | head -1)
    if [[ -z "$wallpaper" ]]; then
        wallpaper="wallpaper.jpg"
        wallpaper_path="$script_dir/$wallpaper"
    else
        wallpaper_path="$WALLPAPER_SRC_DIR/$wallpaper"
    fi
else
    echo "prepare-wallpaper.sh: Using downloaded wallpaper".
fi
# If the file name ends with "-tile", it's a tile image
if [[ "$wallpaper" == *-tile.* ]]; then
    option="--bg-tile"
else
    option="--bg-fill"
fi
feh "$option" "$wallpaper_path"
