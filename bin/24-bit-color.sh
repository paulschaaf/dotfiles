#!/bin/bash

awk 'BEGIN{
    s="/\\/\\/\\/\\/\\";
    printf s;
    s=s s s s s s s s "\n" s s s s s s s s "\n" s s s s s s s;
    for (colnum = 0; colnum<256; colnum++) {
        r = 255-colnum;
        g = colnum * 2;
        b = colnum;
        if (g > 255) g = 510 - g;
        printf "\033[48;2;%d;%d;%dm", r,g,b;
        printf "\033[38;2;%d;%d;%dm", 255-r,255-g,255-b;
        printf "%s\033[0m", substr(s,colnum+1,1);
    }
    printf "\n";
}'
