#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

(XSECURELOCK_FONT=--courier-bold-r-normal--34------- XSECURELOCK_WANT_FIRST_KEYPRESS=1 xsecurelock; $DIR/hci-adjustments) &
