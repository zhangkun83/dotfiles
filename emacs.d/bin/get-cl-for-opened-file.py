#!/usr/bin/python3
# Gets the CL in which a file is g4 opened

import re
import subprocess
import sys

# Execute a command and return stdout as a string array
def execute(args):
    p = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout, stderr = p.communicate()
    if p.returncode != 0:
        raise Exception("%s failed with %d. stdout=%s, stdin=%s" %
                    (str(args), p.returncode, stdout, stderr))
    stdout_str = stdout.decode()
    return stdout_str.split("\n")

CL_LINE_PATTERN = re.compile(r'(?:\* )?Change (\d+) : .*')

filename = sys.argv[1]
g4_output = execute(["g4", "p", "-s", "relativepath"])
current_cl = None

for line in g4_output:
    line = line.strip()
    if len(line) == 0:
        current_cl = None
    cl_m = CL_LINE_PATTERN.match(line)
    if cl_m:
        current_cl = int(cl_m.group(1))
    else:
        if line.startswith(filename + " ") and current_cl:
            print(current_cl)
            break
