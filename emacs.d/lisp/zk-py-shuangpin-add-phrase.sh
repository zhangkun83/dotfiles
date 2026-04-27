#!/bin/bash
set -e
if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <new_phrase>"
    exit 1
fi

cp zk-py-shuangpin-phrases-list.txt zk-py-shuangpin-phrases-list.tmp
echo "$1" >> zk-py-shuangpin-phrases-list.tmp
sort zk-py-shuangpin-phrases-list.tmp | uniq > zk-py-shuangpin-phrases-list.txt
rm zk-py-shuangpin-phrases-list.tmp
bash zk-py-shuangpin-rebuild.sh
