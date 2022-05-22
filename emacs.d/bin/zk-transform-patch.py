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

COMMON_LINE_PATTERNS = {
    "diff-u-p4": re.compile(u'^ '),
    "git": re.compile(u'^ '),
}

def write(line):
    sys.stdout.write(line)

format = None
current_file = None
# Has value if we are only seeing the first common lines of the
# current hunk, and still waiting for the first diff line of the
# current hunk to appear.
hunk_start_line_number = None
hunk_buffered_common_lines = []
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
            hunk_start_line_number = int(hunk_m.group(1))
        elif hunk_start_line_number:
            # The first few lines of a hunk are common
            # lines. hunk_start_line_number != None means we are still
            # processing those common lines.  We will buffer those
            # common lines and wait until the first diff line to
            # appear and write its line number in the navigation
            # annotation.
            common_line_m = re.match(COMMON_LINE_PATTERNS[format], line)
            if common_line_m:
                hunk_buffered_common_lines.append(line)
            else:
                # ZK Diff Navigation annotation
                write('=' * 80 + '\n')
                write('*** %s:%d\n' %
                      (current_file,
                       (hunk_start_line_number + len(hunk_buffered_common_lines))))
                write('=' * 80 + '\n')
                # Write the buffered common lines
                for buffered_line in hunk_buffered_common_lines:
                    write(buffered_line)
                # Subsequent lines of this hunk will not be buffered
                hunk_start_line_number = None
                hunk_buffered_common_lines = []
    write(line)

assert not hunk_start_line_number
assert not hunk_buffered_common_lines
