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
x_span = max_x - min_x

# The algorithm
#
# The cascade is done in "strides". Each stride starts from the top
# and increment (xstep, ystep) for each step, until it reaches the
# bottom boundary, which marks the end of this stride.  The next
# stride will start at a point right to the start of this stride, far
# enough (calculated as stride_x_offset below) so that the next stride
# won't cover the titles of this stride.
#
# During a stride, if right boundary is reached, wrap to the left and
# continue the stride.

stride_x_offset = win_width + (win_height / ystep) * xstep

# Initial position of the current stride
stride_init_x = wa_x

# The window stack goes from the bottom to the top.
for win_id in curr_stacking_win_ids:
    # Un-maximize and un-minimize, then move
    win_id_str = str(win_id)
    execute(["wmctrl", "-ir", win_id_str, "-b", "remove,maximized_vert,maximized_horz"])
    execute(["wmctrl", "-ir", win_id_str, "-b", "remove,hidden"])
    execute(["wmctrl", "-ir", win_id_str, "-e", "0,%d,%d,%d,%d" % (x, y, win_width, win_height)])
    x += xstep
    y += ystep
    assert x_span > 0
    if y > max_y:
        # End of stride
        x = stride_init_x + stride_x_offset
        y = min_y
        while x > max_x:
            x -= x_span
        stride_init_x = x
    while x > max_x:
        x -= x_span
