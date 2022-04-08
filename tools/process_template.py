#!/usr/bin/python3

import re
import sys

PARAMS = {
    'config#openbox/rc.xml' : {
        'hidpi' : {
            'THEME_NAME' : 'Clearlooks-Phenix-HiDPI',
            'WINDOW_LIST_ICON_SIZE' : '96',
            'CASCADE_STEP' : '100',
            'WINDOW_MOVE_STEP' : '60',
        },
        'middpi' : {
            'THEME_NAME' : 'Clearlooks-Phenix-MidDPI',
            'WINDOW_LIST_ICON_SIZE' : '72',
            'CASCADE_STEP' : '100',
            'WINDOW_MOVE_STEP' : '60',
        },
        'lodpi' : {
            'THEME_NAME' : 'Clearlooks-Phenix',
            'WINDOW_LIST_ICON_SIZE' : '48',
            'CASCADE_STEP' : '50',
            'WINDOW_MOVE_STEP' : '30',
        },
    },
    'gtkrc-2.0' : {
        'hidpi' : {
            'THEME_NAME' : 'Clearlooks-Phenix-HiDPI',
            'FONT_SIZE_LARGE' : '64',
            'FONT_SIZE_SMALL' : '32',
        },
        'middpi' : {
            'THEME_NAME' : 'Clearlooks-Phenix-MidDPI',
            'FONT_SIZE_LARGE' : '48',
            'FONT_SIZE_SMALL' : '24',
        },
        'lodpi' : {
            'THEME_NAME' : 'HighContrast',
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

path = None
config = None

if len(sys.argv) > 1:
    path = sys.argv[1]

if len(sys.argv) > 2:
    config = sys.argv[2]

if path == None:
    sys.exit('Usage: %s <path> <config>\nAvailable paths: %s' % (sys.argv[0], ', '.join(PARAMS)))

if path not in PARAMS:
    sys.exit('Configs for path "%s" not found\nAvailable paths: %s' % (path, ', '.join(PARAMS)))

params_for_path = PARAMS[path]

if config == None:
    sys.exit('Usage: %s <path> <config>\nAvailable configs for %s: %s' % (sys.argv[0], path, ', '.join(params_for_path)))

if config not in params_for_path:
    sys.exit('Config "%s" for path "%s" not found\nAvailable configs: %s' % (config, path, ', '.join(params_for_path)))

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
