#!/usr/bin/python3
# Absolute Move Mouse version 2 (move one axis at a time)

import sys
import os
import subprocess

# Execute a command and return stdout as a string array
def execute(args):
    p = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout, stderr = p.communicate()
    if p.returncode != 0:
        raise Exception("%s failed with %d. stdout=%s, stdin=%s" %
                    (str(args), p.returncode, stdout, stderr))
    stdout_str = stdout.decode()
    return stdout_str.split("\n")

keys = "ghjkl;'"

screen_width = int(sys.argv[1])
screen_height = int(sys.argv[2])
axis = sys.argv[3]
key = sys.argv[4]

mouselocation = execute(['xdotool', 'getmouselocation', '--shell'])
x = int(mouselocation[0][2:])
y = int(mouselocation[1][2:])

index = keys.index(key)
if axis == 'x':
    x = screen_width / len(keys) * (index + 0.5)
elif axis == 'y':
    y = screen_height / len(keys) * (index + 0.5)

os.system("xdotool mousemove --sync %d %d" % (x, y))
