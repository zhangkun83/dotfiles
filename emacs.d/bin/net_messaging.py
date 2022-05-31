# A simple messaging protocol that sends strings that are prefixed
# with 4-byte integers for their lengths
import struct

SIZE_FORMAT = "!i"

def recv_all(socket, size):
    buf = b''
    while len(buf) < size:
        buf += socket.recv(size - len(buf))
    return buf

def read_msg(socket):
    buffer = recv_all(socket, struct.calcsize(SIZE_FORMAT))
    size = struct.unpack(SIZE_FORMAT, buffer)[0]
    buffer = recv_all(socket, size)
    return buffer.decode('utf-8')

def write_msg(socket, msg):
    msg_bytes = msg.encode('utf-8')
    size = len(msg_bytes)
    socket.sendall(struct.pack(SIZE_FORMAT, size))
    socket.sendall(msg_bytes)
