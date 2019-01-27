#!/usr/bin/python3
# Cascade windows of current desktop using "wmctrl"
# Usage: cascade.py <xstep> <ystep>

import re
import sys
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


xstep = int(sys.argv[1])
ystep = int(sys.argv[2])

desktop_infos = execute(["wmctrl", "-d"])

# '*' indicates the current desktop
curr_desktop_info = [ i for i in desktop_infos if "*" in i ][0]

desktop_info_pattern = re.compile(r'(^\d+) .* WA: (\d+),(\d+) (\d+)x(\d+) ')
desktop_info_match = desktop_info_pattern.match(curr_desktop_info)

# Current desktop ID and working area dimensions
curr_desktop_id, wa_x, wa_y, wa_w, wa_h = desktop_info_match.groups()
wa_x, wa_y, wa_w, wa_h = [ int(s) for s in [wa_x, wa_y, wa_w, wa_h] ]

# Get window IDs of current desktop, but is not sorted by stack.
win_infos = execute(["wmctrl", "-l"])
win_info_pattern = re.compile("^(0x[0-9a-z]+) +(" + curr_desktop_id + ") ") 
curr_desktop_win_ids = [ int(match.group(1), 16) for match in
                               [ win_info_pattern.match(info) for info in win_infos ]
                               if match]


# Get all window IDs sorted by stack through a different way.
root_prop_output = execute(["xprop", "-root"])
stacking_list_line = [ i for i in root_prop_output if "_NET_CLIENT_LIST_STACKING(WINDOW)" in i ][0]

stacking_win_ids = [ int(match.group(0), 16) for match
                        in re.finditer("0x[0-9a-z]+", stacking_list_line) ]

# Get windows IDs of current desktop sorted by stack
curr_stacking_win_ids = [ id for id in stacking_win_ids if id in curr_desktop_win_ids]

# Calculate new window sizes.  All windows are resized to the same.
# Windows may be shrinked so no parts of any window would go out of
# working area boundaries, but can't shrink to smaller than the 1/3 of
# the working area dimensions.
num_curr_windows = len(curr_stacking_win_ids)
win_width = max(wa_w - xstep * num_curr_windows, wa_w / 3)
win_height = max(wa_h - ystep * num_curr_windows, wa_h / 3)

x = wa_x
y = wa_y

# Don't let any part of the window out of the working area.
min_x = wa_x
min_y = wa_y
max_x = wa_x + wa_w - win_width
max_y = wa_y + wa_h - win_height
dir_x = 1
dir_y = 1
# The window stack goes from the bottom to the top.
for win_id in curr_stacking_win_ids:
    # Un-maximize and un-minimize, then move
    win_id_str = str(win_id)
    execute(["wmctrl", "-ir", win_id_str, "-b", "remove,maximized_vert,maximized_horz"])
    execute(["wmctrl", "-ir", win_id_str, "-b", "remove,hidden"])
    execute(["wmctrl", "-ir", win_id_str, "-e", "0,%d,%d,%d,%d" % (x, y, win_width, win_height)])
    x += xstep * dir_x
    y += ystep * dir_y
    # If out of boundary, bounce back
    if x < min_x or x > max_x:
        dir_x = -dir_x
        x += xstep * dir_x * 2
    if y < min_y or y > max_y:
        dir_y = -dir_y
        y += ystep * dir_y * 2

