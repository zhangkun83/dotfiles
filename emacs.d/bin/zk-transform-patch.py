#!/usr/bin/python3
# Annotates a patch (standard diff output or git diff) so that it can be
# annotated by the ZK Diff Navigation mode

import os
import re
import sys

ZK_PROJECT_ROOT = os.getenv('ZK_PROJECT_ROOT')
FILENAME_LINE_PATTERNS = {
    "diff-u-p4": re.compile(u'^\+\+\+ (\S*)\s+.+$',),  # P4DIFF="diff -u" g4 d
    "git": re.compile(u'^\+\+\+ b/(.*)$'),  # git diff
}

HUNK_HEADER_PATTERNS = {
    "diff-u-p4": re.compile(u'^@@ -[0-9,]+ \+([0-9]+),[0-9]+ @@'),
    "git": re.compile(u'^@@ -[0-9,]+ \+([0-9]+),[0-9]+ @@'),
}

format = None
current_file = None
for line in sys.stdin:
    filename_m = None
    if not format:
        for f,p in FILENAME_LINE_PATTERNS.items():
            filename_m = re.match(p, line)
            if filename_m:
                format = f
                break
    else:
        filename_m = re.match(FILENAME_LINE_PATTERNS[format], line)
    if filename_m:
        current_file = filename_m.group(1)
        if current_file.startswith(ZK_PROJECT_ROOT):
            current_file = current_file[len(ZK_PROJECT_ROOT):]
    if format:
        hunk_m = re.match(HUNK_HEADER_PATTERNS[format], line)
        if hunk_m:
            line_number = hunk_m.group(1)
            # ZK Diff Navigation annotation
            sys.stdout.write('*** ' + current_file + ':' + line_number + '\n')
    sys.stdout.write(line)
