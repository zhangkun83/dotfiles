#!/usr/bin/python3
# Remove the leading $ZK_PROJECT_ROOT from each line

import os
import sys

ZK_PROJECT_ROOT = os.getenv('ZK_PROJECT_ROOT')

for line in sys.stdin:
    if line.startswith(ZK_PROJECT_ROOT):
        line = line[len(ZK_PROJECT_ROOT):]
    sys.stdout.write(line)
