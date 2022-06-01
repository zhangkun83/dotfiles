#!/usr/bin/python3
# The client for desktop-helper-server.py
# The command string is read from the first argument, while the data string is
# either the rest of the arguments if given, or read from stdin.

import socket
import sys
import net_messaging

HOST = "localhost"
PORT = 5032

command = sys.argv[1]

if len(sys.argv) > 2:
    data = ' '.join(sys.argv[2:])
else:
    data = ''
    for line in sys.stdin:
        data += line

with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as socket:
    socket.connect((HOST, PORT))
    if command == 'store-to-clipboard':
        sys.stderr.write(f"desktop-helper-client: {command} (content redacted)\n")
    else:
        sys.stderr.write(f"desktop-helper-client: {command} {data}\n")
    net_messaging.write_msg(socket, command)
    net_messaging.write_msg(socket, data)
    status = net_messaging.read_msg(socket)
    data = net_messaging.read_msg(socket)
    if status == "OK" and command == 'retrieve-from-clipboard':
        sys.stderr.write("OK: (content retrieved)\n")
        sys.stdout.write(data)
    else:
        sys.stderr.write(f"{status}: {data}\n")
