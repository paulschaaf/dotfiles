#!/bin/sh

# Extracted from /etc/DIR_COLORS ------------------------------
# A color init string consists of one or more of the following 
# numeric codes:
#
# Attribute codes: 
# 00=none 01=bold 04=underscore 05=blink 07=white_background
#
# FG color codes:
# 30=black 31=red 32=green 33=yellow 34=blue 35=magenta 36=cyan 37=white
#
# BG color codes:
# FG color codes + 10
# -------------------------------------------------------------
# emph=`tput setaf 1` noemph=`tput setaf 7`
# echo "${emph}Emphasized text${noemph} regular text"

echoVar () {
    # $3 is an optional comment
    if [ -n "$3" ]; then
        echo
        echo \# $3
    fi
    export $1="$2"
    echo setenv $1 \"$2\"
}

echo \# This file automatically generated by `whoami`
echo \# on `date` using:
echo \# $0 "$@"

echoVar COLOR_NAMES \
    "BLACK RED GREEN YELLOW BLUE MAGENTA CYAN WHITE" \
    "The color names must be listed here in ASCII ascending order"

echoVar COLOR_ATTRIBUTES \
    "BOLD ULINE BLINK INV" \
    "Attribute names (order is unimportant)"
echoVar NO_COLOR \
    "[0;0m" \
    "Turns off all coloration"
echoVar FIRST_COLOR_VAL \
    30 \
    "ASCII value of the lowest color"

BOLD=1 ULINE=4 BLINK=5 INV=7
color_val=$FIRST_COLOR_VAL

echo
for color_name in `echo $COLOR_NAMES`; do
    # no attribute, foreground and background
    eval "echo setenv ${color_name} \\\"\"[0;${color_val}m\"\\\""
    eval "echo setenv ${color_name}_BG \\\"\"[0;\`expr $color_val + 10\`m\"\\\""

    for attrib in `echo $COLOR_ATTRIBUTES`; do
	# foreground then background
        eval "echo setenv ${color_name}_${attrib} \\\"\"[\$$attrib;${color_val}m\"\\\""
        eval "echo setenv ${color_name}_BG_${attrib} \\\"\"[\$$attrib;\`expr $color_val + 10\`m\"\\\""
    done

    echo
    color_val=`expr $color_val + 1`
done