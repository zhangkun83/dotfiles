#!/usr/bin/python3

import re
import sys

PARAMS = {
    'config#openbox/rc.xml' : {
        'hidpi' : {
            'THEME_NAME' : 'Nodoka-HiDPI',
            'WINDOW_LIST_ICON_SIZE' : '96',
            'WORKAREA_MARGIN' : '75',
            'CASCADE_STEP' : '100',
            'WINDOW_MOVE_STEP' : '60',
        },
        'middpi' : {
            'THEME_NAME' : 'Bear2-MidDPI',
            'WINDOW_LIST_ICON_SIZE' : '72',
            'WORKAREA_MARGIN' : '75',
            'CASCADE_STEP' : '100',
            'WINDOW_MOVE_STEP' : '60',
        },
        'lodpi' : {
            'THEME_NAME' : 'Clearlooks-Phenix',
            'WINDOW_LIST_ICON_SIZE' : '48',
            'WORKAREA_MARGIN' : '35',
            'CASCADE_STEP' : '50',
            'WINDOW_MOVE_STEP' : '30',
        },
    },
    'gtkrc-2.0' : {
        'hidpi' : {
            'THEME_NAME' : 'Nodoka-HiDPI',
            'FONT_SIZE_LARGE' : '64',
            'FONT_SIZE_SMALL' : '32',
        },
        'middpi' : {
            'THEME_NAME' : 'Clearlooks-Phenix-MidDPI',
            'FONT_SIZE_LARGE' : '48',
            'FONT_SIZE_SMALL' : '24',
        },
        'lodpi' : {
            'THEME_NAME' : 'Clearlooks-Phenix',
            'FONT_SIZE_LARGE' : '32',
            'FONT_SIZE_SMALL' : '24',
        },
    },
    'Xresources' : {
        'hidpi' : {
            'XFT_DPI' : '200',
            'XCURSOR_SIZE' : '64',
        },
        'middpi' : {
            'XFT_DPI' : '150',
            'XCURSOR_SIZE' : '64',
        },
        'lodpi' : {
            'XFT_DPI' : '96',
            'XCURSOR_SIZE' : '32',
        },
    },
}

KEY_PATTERN = re.compile(r'%([A-Z_]+)%')

if len(sys.argv) < 3:
    sys.exit('Usage: process_template.py <path> <config>')

path = sys.argv[1]
config = sys.argv[2]

if path not in PARAMS:
    sys.exit('Configs for path "%s" not found' % (path))
params_for_path = PARAMS[path]

if config not in params_for_path:
    sys.exit('Config "%s" for path "%s" not found' % (config, path))

params = params_for_path[config]

input_path = path + '_template'
print('Reading "%s" with config "%s"' % (input_path, config))

line_number = 1
with open(input_path, 'r') as in_file, open(path, 'w') as out_file:
    for line in in_file:
        output_line = line
        for key_match in re.finditer(KEY_PATTERN, line):
            key = key_match.group(1)
            value = params[key]
            if not value:
                sys.exit('Key %s not found on line %d' % (key, line_number))
            output_line = output_line.replace('%' + key + '%', value)
        out_file.write(output_line)

print('Written "%s"' % (path))
