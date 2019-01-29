#!/usr/bin/python3
# Cascade windows of current desktop using "wmctrl"
# Usage:
#   To cascade: cascade.py <xstep> <ystep>
#   To restore the states from previous cascade: cascade.py

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

# Fetch a xwin property, or None if the property doesn't exist
def get_xwin_property(xprop_output, key):
    prefix = key + " = "
    property_lines = [ i for i in xprop_output if i.startswith(prefix) ]
    if len(property_lines) == 0:
        return None
    return property_lines[0][len(prefix):]

encoded_win_state_pattern = re.compile(r'"?(\d+):(\d+):(\d+):(\d+) (\w*)"?')

# _NET_FRAME_EXTENTS: left, right, top, bottom
border_widths_pattern = re.compile(r'(\d+), (\d+), (\d+), (\d+)')

class WinState:
    def __init__(self, win_id, x, y, w, h):
        self.win_id = win_id
        self.x = x
        self.y = y
        self.w = w
        self.h = h
        self.xprops = None
        self.state = ""
        self.adjusted = False

    def __repr__(self):
        return "win_id=0x%x, encoded=%s" % (
            self.win_id, self.encode())

    def get_props(self):
        if self.xprops == None:
            self.xprops = execute(["xprop", "-id", win_id_str])
        return self.xprops

    def fetch_state(self):
        self.state = get_xwin_property(self.get_props(), "_NET_WM_STATE(ATOM)")

    # The positions returned by wmctrl is for the part inside window decoration,
    # we need to actually get the positions of the window decoration for use in uncascade.
    def adjust_position(self):
        assert not self.adjusted
        border_widths_raw = get_xwin_property(self.get_props(), "_NET_FRAME_EXTENTS(CARDINAL)")
        m = border_widths_pattern.match(border_widths_raw)
        left = int(m.group(1))
        top = int(m.group(3))
        # TODO: the actual margin appears to be the double of what I
        # get from the _NET_FRAME_EXTENTS property. Don't know why.
        self.x -= left*2
        self.y -= top*2
        self.adjusted = True

    def encode(self):
        return "%d:%d:%d:%d %s" % (self.x, self.y, self.w, self.h, self.state)

    def decode_from(self, encoded):
        m = encoded_win_state_pattern.match(encoded)
        assert m != None
        decoded = WinState(self.win_id, int(m.group(1)), int(m.group(2)), int(m.group(3)), int(m.group(4)))
        decoded.state = m.group(5)
        return decoded

    def write_xwin_property(self, key, value):
        execute(["xprop", "-id", str(self.win_id), "-format", key, "8s", "-set", key, value])

    def remove_xwin_property(self, key):
        execute(["xprop", "-id", str(self.win_id), "-remove", key])

desktop_infos = execute(["wmctrl", "-d"])

# '*' indicates the current desktop
curr_desktop_info = [ i for i in desktop_infos if "*" in i ][0]

desktop_info_pattern = re.compile(r'(^\d+) .* WA: (\d+),(\d+) (\d+)x(\d+) ')
desktop_info_match = desktop_info_pattern.match(curr_desktop_info)

# Current desktop ID and working area dimensions
curr_desktop_id, wa_x, wa_y, wa_w, wa_h = desktop_info_match.groups()
wa_x, wa_y, wa_w, wa_h = [ int(s) for s in [wa_x, wa_y, wa_w, wa_h] ]

# Get window IDs of current desktop, but is not sorted by stack.
win_infos = execute(["wmctrl", "-lG"])
win_info_pattern = re.compile(r'^(0x[0-9a-z]+) +' + curr_desktop_id +
                              r' +(\d+) +(\d+) +(\d+) +(\d+) +(.*)')
curr_desktop_wins = [ WinState(int(match.group(1), 16),
                               int(match.group(2)),
                               int(match.group(3)),
                               int(match.group(4)),
                               int(match.group(5)))
                      for match in [ win_info_pattern.match(info) for info in win_infos ] if match]
curr_desktop_wins_dict = { win.win_id: win for win in curr_desktop_wins }

# Get all window IDs sorted by stack through a different way.
root_prop_output = execute(["xprop", "-root"])
stacking_list_line = [ i for i in root_prop_output if "_NET_CLIENT_LIST_STACKING(WINDOW)" in i ][0]

stacking_win_ids = [ int(match.group(0), 16) for match
                        in re.finditer("0x[0-9a-z]+", stacking_list_line) ]

# Get windows IDs of current desktop sorted by stack
curr_stacking_wins = [ curr_desktop_wins_dict[id] for id in stacking_win_ids if id in curr_desktop_wins_dict]

if len(sys.argv) < 3:
    # Un-cascade
    # The window stack goes from the bottom to the top.
    for win in curr_stacking_wins:
        win_id_str = str(win.win_id)
        encoded_uncascade_state = get_xwin_property(win.get_props(), "_ZK_UNCASCADE_STATE(STRING)")
        print(encoded_uncascade_state)
        if encoded_uncascade_state:
            s = win.decode_from(encoded_uncascade_state)
            # I don't use vert and horz separately.
            if "MAXIMIZED" in s.state:
                execute(["wmctrl", "-ir", win_id_str, "-b", "add,maximized_vert,maximized_horz"])
            if "HIDDEN" in s.state:
                execute(["wmctrl", "-ir", win_id_str, "-b", "add,hidden"])
            execute(["wmctrl", "-ir", win_id_str, "-e", "0,%d,%d,%d,%d" % (s.x, s.y, s.w, s.h)])
        win.remove_xwin_property("_ZK_UNCASCADE_STATE")
else:
    # Cascade
    xstep = int(sys.argv[1])
    ystep = int(sys.argv[2])

    # Calculate new window sizes.  All windows are resized to the
    # same.  Windows may be shrinked so no parts of any window
    # would go out of working area boundaries, but can't shrink to
    # smaller than the 1/3 of the working area dimensions.
    num_curr_windows = len(curr_stacking_wins)
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
    # The cascade is done in "strides". Each stride starts from
    # the top and increment (xstep, ystep) for each step, until it
    # reaches the bottom boundary, which marks the end of this
    # stride.  The next stride will start at a point right to the
    # start of this stride, far enough (calculated as
    # stride_x_offset below) so that the next stride won't cover
    # the titles of this stride.
    #
    # During a stride, if right boundary is reached, wrap to the
    # left and continue the stride.

    stride_x_offset = win_width + (win_height / ystep) * xstep

    # Initial position of the current stride
    stride_init_x = wa_x

    # The window stack goes from the bottom to the top.
    for win in curr_stacking_wins:
        # Un-maximize and un-minimize, then move
        win_id_str = str(win.win_id)
        win.fetch_state()
        win.adjust_position()
        win.write_xwin_property("_ZK_UNCASCADE_STATE", win.encode())
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
