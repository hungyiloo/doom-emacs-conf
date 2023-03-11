#!/bin/bash
source ~/.bashrc
source ~/.profile
if ! pgrep -f dropbox &> /dev/null 2>&1; then
	~/.dropbox-dist/dropboxd &>/dev/null &
	disown
fi
cd ~
emacs
