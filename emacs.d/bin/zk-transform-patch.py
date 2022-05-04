#!/usr/bin/python3
# Transforms a patch (standard diff output) into a format that can be
# recognized by zk's custom compile error pattern

import os
import re
import sys

ZK_PROJECT_ROOT = os.getenv('ZK_PROJECT_ROOT')
FILENAME_LINE_PATTERN = re.compile(u'^==== \S+ - (\S+) ====$')
HUNK_HEADER_PATTERN = re.compile(u'^[0-9,]+[cad]([0-9]+)(,[0-9]+)?$')
current_file = None
for line in sys.stdin:
    filename_m = re.match(FILENAME_LINE_PATTERN, line)
    if filename_m:
        current_file = filename_m.group(1)
        if current_file.startswith(ZK_PROJECT_ROOT):
            current_file = current_file[len(ZK_PROJECT_ROOT):]
    hunk_m = re.match(HUNK_HEADER_PATTERN, line)
    if hunk_m:
        line_number = hunk_m.group(1)
        sys.stdout.write('*** ' + current_file + ':' + line_number + '\n')
    if not filename_m:
        sys.stdout.write(line)
