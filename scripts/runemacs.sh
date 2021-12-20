#!/bin/bash
source ~/.profile
if ! pgrep -f syncthing &> /dev/null 2>&1; then
	/usr/bin/syncthing -no-browser -logfile=default &>/dev/null &
	disown
fi
cd ~
~/.config/emacs/bin/doom env
emacs
