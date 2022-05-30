#!/usr/bin/python3
# A server that handles interactions with the desktop environment.
# It receives commands via TCP, thus allows a remote host to send
# commands to this server (via SSH tunnel) to accomplish tasks like
# opening URLs.
#
# The protocol: the client makes a new TCP connection for each request.
# A request consists of a command string and a data string, both
# prefixed with a 4-byte header that represent the string length.

import socket
import struct
import os

HOST = "localhost"
PORT = 5032
SIZE_FORMAT = "!i"

BROWSER_PATHS = [
    "/cygdrive/c/Program Files/Google/Chrome/Application/chrome.exe",
]

def recv_all(socket, size):
    buf = b''
    while len(buf) < size:
        buf += socket.recv(size - len(buf))
    return buf

def read_msg(socket):
    buffer = recv_all(socket, struct.calcsize(SIZE_FORMAT))
    size = struct.unpack(SIZE_FORMAT, buffer)[0]
    buffer = recv_all(socket, size)
    return buffer.decode()

def handle(command, data):
    if command == "open-url":
        handle_open_url(data)
    else:
        print("Unsupported command: " + command)

def handle_open_url(url):
    for browser_path in BROWSER_PATHS:
        if os.path.exists(browser_path):
            print(f"Opening {url} with {browser_path}")
            os.spawnl(os.P_NOWAIT, browser_path, browser_path, url)
            return
    print("Cannot find a usable browser to open the URL")

with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as server_socket:
    server_socket.bind((HOST, PORT))
    print(f"desktop-helper-server started on port {PORT}")
    server_socket.listen()
    while True:
        socket, addr = server_socket.accept()
        print("New connection")
        with socket:
            command = read_msg(socket)
            print("[C]" + command)
            data = read_msg(socket)
            print("[D]" + data)
            handle(command, data)
        print("Connection closed")
