#!/usr/bin/python3
# The client for desktop-helper-server.py
# The command string is read from the first argument, while the data string is
# either the rest of the arguments if given, or read from stdin.

import socket
import struct
import sys

HOST = "localhost"
PORT = 5032
SIZE_FORMAT = "!i"

command = sys.argv[1]

def write_msg(socket, msg):
    size = len(msg)
    socket.sendall(struct.pack(SIZE_FORMAT, size))
    socket.sendall(str.encode(msg))

if len(sys.argv) > 2:
    data = ' '.join(sys.argv[2:])
else:
    data = ''
    for line in sys.stdin:
        data += line

with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as socket:
    socket.connect((HOST, PORT))
    write_msg(socket, command)
    write_msg(socket, data)

print(f"desktop-helper-client: {command} {data}")
