#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

echo Enter password to hibernate.
echo Ctrl+C to cancel.
sudo pm-hibernate
$DIR/lock.sh &> /dev/null
read -p "Press enter to continue"
