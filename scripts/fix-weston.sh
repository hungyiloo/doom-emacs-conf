#!/bin/bash
if [ ! -f /mnt/wsl/state-wslg-config-initialized ]; then
echo DISPLAY=$DISPLAY
echo Patching weston...
sleep 3
KEYMAP_LAYOUT=us
KEYMAP_VARIANT=colemak
read -r -d '' GWSL_SYSTEM_CONFIG_COMMANDS <<EOF
KEYMAP_LAYOUT=\`grep -r '^keymap_layout=.*\$' /home/wslg/.config/weston.ini\`;if [ -z \$KEYMAP_LAYOUT ]; then sed -i '$ a\[keyboard]\nkeymap_layout=`echo ${KEYMAP_LAYOUT}`\nkeymap_variant=`echo ${KEYMAP_VARIANT}`\n' /home/wslg/.config/weston.ini;pkill -HUP weston;touch /mnt/wsl/state-wslg-config-initialized;fi
EOF
wsl.exe -d $WSL_DISTRO_NAME --system ${GWSL_SYSTEM_CONFIG_COMMANDS}
fi
