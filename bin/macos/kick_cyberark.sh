#! /usr/bin/env zsh
set -u
CPU_THRESHOLD=25
set -x
cyber_pid="$(pgrep 'com.cyberark.CyberArkEPMEndpointSecurityExtension')"
awk_program='{ if ($1 > '$CPU_THRESHOLD') { print "true" } else { print "false" } }'
needs_kick="$(ps -o '%cpu=' -p "$cyber_pid" | awk "$awk_program")"

if [[ "$needs_kick" == "true" ]]; then
    printf 'CyberArk is misbehaving, giving it a good kick...\n'
:    sudo kill -HUP "$cyber_pid"
else
    printf 'No kick required\n'
fi