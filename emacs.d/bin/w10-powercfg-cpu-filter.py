#!/usr/bin/python3

# Filter the output of the Windows 10 command "powercfg /Q" and
# convert the output of the SUB_PROCESSOR results to human readable
# formats.

import re
import sys

def writeln(line):
    sys.stdout.write(line)
    sys.stdout.write('\n')

class Setting:
    def __init__(self, name):
        self.name = name
        self.alias = None
        self.ac_value = None
        self.dc_value = None

    def format_value(self, value):
        decimal_value = int(value, 16)
        if self.alias == 'SYSCOOLPOL':
            if decimal_value == 0:
                return "Passive"
            elif decimal_value == 1:
                return "Active"
            else:
                return str(value)
        else:
            return str(decimal_value)

    def __str__(self):
        return "[%30s]   DC: %10s  AC: %10s" % (
            self.name,
            self.format_value(self.dc_value),
            self.format_value(self.ac_value))

SETTING_NAME_PATTERN = re.compile(r'.*Power Setting GUID: [^(]*\(([^)]*)\)')
SETTING_ALIAS_PATTERN = re.compile(r'.*GUID Alias: ([A-Z]*)')
DC_VALUE_PATTERN = re.compile(r'.*Current DC Power Setting Index: 0x([0-9a-f]*)')
AC_VALUE_PATTERN = re.compile(r'.*Current AC Power Setting Index: 0x([0-9a-f]*)')

current_setting = None
settings = []
for line in sys.stdin:
    setting_name_m = re.match(SETTING_NAME_PATTERN, line)
    if setting_name_m:
        current_setting = Setting(setting_name_m.group(1))
        settings.append(current_setting)
        continue
    if not current_setting:
        continue
    setting_alias_m = re.match(SETTING_ALIAS_PATTERN, line)
    if setting_alias_m:
        current_setting.alias = setting_alias_m.group(1)
        continue
    dc_value_m = re.match(DC_VALUE_PATTERN, line)
    if dc_value_m:
        current_setting.dc_value = dc_value_m.group(1)
        continue
    ac_value_m = re.match(AC_VALUE_PATTERN, line)
    if ac_value_m:
        current_setting.ac_value = ac_value_m.group(1)
        continue

settings.sort(key=lambda s : s.alias)
for setting in settings:
    writeln(str(setting))
