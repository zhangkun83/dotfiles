#!/bin/bash
# A simple interface that let you input a word and look it up in Youdao Web dict

default_query=$(xsel -op)

ans=$(zenity --entry  --title 'Youdao Dictionary' \
             --text 'Enter a word or phrase:' \
             --ok-label 'Look up' --entry-text "$default_query" \
   ) && (
    encoded_query=$(python2 -c "import urllib; print(urllib.quote(raw_input()))" <<< "$ans")
    google-chrome --app="http://dict.youdao.com/search?q=$encoded_query" &)
