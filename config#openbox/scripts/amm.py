#!/usr/bin/python3
# Absolute Move Mouse

import sys
import os

keys_matrix = ["67890", "yuiop", "hjkl;", "nm,./"]
width = 5
height = len(keys_matrix)

screen_width = int(sys.argv[1])
screen_height = int(sys.argv[2])
key = sys.argv[3]

matrix_x = -1
matrix_y = -1
key_valid = False
for y in range(height):
    keys_row = keys_matrix[y]
    x = keys_row.find(key)
    if x > -1:
        key_valid = True
        matrix_x = x
        matrix_y = y
        break

if key_valid:
    x = screen_width / width * (x + 0.5)
    y = screen_height / height * (y + 0.5)
    os.system("xdotool mousemove --sync %d %d" % (x, y))
else:
    raise Exception("Invalid key " + key)
