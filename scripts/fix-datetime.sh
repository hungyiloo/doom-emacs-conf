#!/usr/bin/env bash

# This fixes datetime issues in WSL after hibernation/sleep on my laptop
sudo ntpdate -sb time.windows.com
