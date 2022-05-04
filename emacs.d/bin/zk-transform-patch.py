#!/usr/bin/python3
# Transforms a patch (standard diff output) into a format that can be
# recognized by zk's custom compile error pattern

import re
import sys

FILENAME_LINE_PATTERN = re.compile(u'^==== \S+ - (\S+) ====$')
HUNK_HEADER_PATTERN = re.compile(u'^[0-9,]+[cad]([0-9]+)(,[0-9]+)?$')
current_file = None
for line in sys.stdin:
    m = re.match(FILENAME_LINE_PATTERN, line)
    if m:
        current_file = m.group(1)
    m = re.match(HUNK_HEADER_PATTERN, line)
    if m:
        line_number = m.group(1)
        sys.stdout.write('#ZK#' + current_file + ':' + line_number + '\n')
    sys.stdout.write(line)
