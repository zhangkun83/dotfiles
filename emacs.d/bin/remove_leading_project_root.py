#!/usr/bin/python3
# Remove the leading $ZK_PROJECT_ROOT from each line

import os
import sys

ZK_PROJECT_ROOT = os.getenv('ZK_PROJECT_ROOT')

for line in sys.stdin:
    if line.startswith(ZK_PROJECT_ROOT):
        line = line[len(ZK_PROJECT_ROOT):]
        # Remove leading slashes of the remaining part in case there
        # are more than one slashes after ZK_PROJECT_ROOT due to path
        # concatenation.  Keeping those slashes will make the result
        # look like an absolute path which is incorrect.
        while line.startswith("/"):
            line = line[1:]
    sys.stdout.write(line)
