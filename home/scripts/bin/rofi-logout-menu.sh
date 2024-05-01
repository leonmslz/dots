#!/usr/bin/env bash

options="Shutdown\0icon\x1fsystem-shutdown\nReboot\0icon\x1fsystem-reboot\nSuspend\0icon\x1fsystem-suspend\nLogout\0icon\x1fsystem-log-out"
result=$(echo -en $options | rofi -dmenu -i -show-icons -p "")

case $result in
 "Shutdown")
     shutdown now ;;
 "Reboot")
     reboot ;;
 "Logout")
     hyprctl dispatch exit ;;
 "Suspend")
     systemctl suspend ;;
esac
