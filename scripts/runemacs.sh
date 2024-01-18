#!/bin/bash
source ~/.bashrc
if ! pgrep -f dropbox &> /dev/null 2>&1; then
	nohup ~/.dropbox-dist/dropboxd </dev/null >/dev/null 2>&1  & # completely detach
	disown
fi
cd ~
setsid emacs
