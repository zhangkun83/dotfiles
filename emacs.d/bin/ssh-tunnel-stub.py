#!/usr/bin/python3
import sys
import time

print("ssh-tunnel-stub started. This is used to keep an ssh tunnel.")
print("Press Ctrl-C to terminate.")
idling_strings = ["-", "\\", "|", "/"]
sys.stdout.write("Rotating means connection is good:  ")

try:
    while True:
        for idling_string in idling_strings:
            # ANSI escape sequence \033[<N>D means move cursor N
            # columns backwards
            sys.stdout.write(f"\033[1D{idling_string}")
            sys.stdout.flush()
            time.sleep(1);
except KeyboardInterrupt:
    print("\nExiting ...")
