#!/bin/bash
# A simple interface that let you input a word and look it up in Youdao Web dict

default_query=$(xsel -op)

ans=$(zenity --entry  --title 'Youdao Dictionary' \
             --text 'Enter a word or phrase:' \
             --ok-label 'Look up' --entry-text "$default_query" \
   ) && (
    encoded_query=$(python3 -c "import urllib.parse; print(urllib.parse.quote(input()))" <<< "$ans")
    google-chrome --app="https://dict.youdao.com/w/eng/$encoded_query" &)
