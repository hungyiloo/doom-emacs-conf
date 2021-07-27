#!/bin/bash
source ~/.profile
export DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2; exit;}'):0.0
setxkbmap "us(colemak)"
if ! pgrep -f syncthing &> /dev/null 2>&1; then
	/usr/bin/syncthing -no-browser -logfile=default &>/dev/null &
	disown
fi
cd ~
~/.config/emacs/bin/doom env
# setsid emacsclient -c -a=""
emacs
