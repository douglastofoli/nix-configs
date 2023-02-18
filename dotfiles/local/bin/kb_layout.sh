#!/run/current-system/sw/bin/bash

localectl status | awk -F"[-.]" '/Keymap/{print $1}' | cut -d : -f 2 | sed 's/^ *//g'
