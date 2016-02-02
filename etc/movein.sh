PATH=/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:/usr/local/sbin:/usr/X11R6/bin

# ================================================
# ======= Functions

make_fifo_for_syslog () {
    mkfifo -m 0664 $2
    printf "\n%s\t\t\t\t\t\t\t\t|%s" "$1" $2 >> /etc/syslog.conf
}

comment_out_lines_beginning() {
    perl -i.orig -pe "s/^($1)/# \\1/g" $2
}

# ================================================
# ======= Create fifo's to "record" all logs (FL, #54)

make_fifo_for_syslog "*.=debug" /var/log/debug
make_fifo_for_syslog "*.*"      /var/log/monitor

#/etc/init.d/syslog restart
/sbin/service --full-restart syslog


# ================================================
# ======= Disallow nonlocal root login

comment_out_lines_beginning '[^#]*pty' /etc/securetty

# ================================================
# ======= Remove extra virtual terminals

comment_out_lines_beginning '[^#]+mingetty tty[3-9]' /etc/inittab


# ================================================
# ======= Prevent remote binding to X (LO, #9)

perl -pe 's/(:0 local \/usr\/X11R6\/bin\/X)/\1 -nolisten tcp/g' \
                /etc/X11/xdm/Xservers \
                /usr/share/config/kdm/Xservers


# ================================================
# ======= Bibliography

<<EOF
[BA] Barrett, Daniel, Silverman, Richard, Byrnes, Robert "Linux Security Cookbook", O'Reilly & Associates, 2003, ISBN: 0-596-00391-9, http://www.oreilly.com/catalog/linuxsckbk/

[FL] Flickenger, Rob, "Linux Server Hacks", O'Reilly & Associates, 2003, ISBN: 0-596-00461-3, http://www.oreilly.com/catalog/linuxsvrhack/

[LO] Lockhart, Andrew, "Network Security Hacks", O'Reilly & Associates, 2004, ISBN: 0-596-00643-8, http://www.oreilly.com/catalog/netsechacks/
EOF


# ================================================
# ======= 
