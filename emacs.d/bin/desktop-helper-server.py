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
import os
import net_messaging
from subprocess import Popen, PIPE, STDOUT, TimeoutExpired

HOST = "localhost"
PORT = 5032

BROWSER_PATHS = [
    "/cygdrive/c/Program Files/Google/Chrome/Application/chrome.exe",
    "/usr/bin/google-chrome",
]

def handle(command, data):
    if command == "open-url":
        return handle_open_url(data)
    if command == "store-to-clipboard":
        return handle_store_to_clipboard(str.encode(data))
    if command == "retrieve-from-clipboard":
        return handle_retrieve_from_clipboard()
    else:
        return ("ERROR", "Unsupported command: " + command)

def handle_open_url(url):
    for browser_path in BROWSER_PATHS:
        if os.path.exists(browser_path):
            print(f"Opening {url} with {browser_path}")
            os.spawnl(os.P_NOWAIT, browser_path, browser_path, url)
            return ("OK", "URL sent to " + browser_path)
    return ("ERROR", "Cannot find a usable browser to open the URL")

def handle_store_to_clipboard(data):
    if os.path.exists("/usr/bin/xclip"):
        print("Using xclip")
        p = Popen(['xclip', '-i', '-selection', 'clip-board'], stdout=None, stdin=PIPE, stderr=None)
        p.communicate(input=data)
        p.stdin.close()
        try:
            p.wait()
        except TimeoutExpired:
            p.kill()
            print("xclip timed out")
        if p.returncode == 0:
            return ("OK", "xclip suceeded")
        else:
            return ("ERROR", f"xclip failed: {p.returncode}")

def handle_retrieve_from_clipboard():
    if os.path.exists("/usr/bin/xclip"):
        print("Using xclip")
        p = Popen(['xclip', '-o', '-selection', 'clip-board'], stdout=PIPE, stdin=None, stderr=None)
        outs, errs = p.communicate(timeout=15)
        try:
            p.wait()
        except TimeoutExpired:
            p.kill()
            print("xclip timed out")
        if p.returncode == 0:
            return ("OK", outs.decode('utf-8'))
        else:
            return ("ERROR", f"xclip failed: {p.returncode}")

with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as server_socket:
    server_socket.bind((HOST, PORT))
    print(f"desktop-helper-server started on port {PORT}")
    print("Local clients will work. To make remote clients work, use")
    print(f"'ssh -L{PORT}:localhost:{PORT}' to create a forwarding tunnel")
    server_socket.listen()
    while True:
        socket, addr = server_socket.accept()
        print("New connection")
        with socket:
            command = net_messaging.read_msg(socket)
            print("[C]" + command)
            data = net_messaging.read_msg(socket)
            print("[D]" + data)
            status, data = handle(command, data)
            net_messaging.write_msg(socket, status)
            net_messaging.write_msg(socket, data)
        print("Connection closed\n")
