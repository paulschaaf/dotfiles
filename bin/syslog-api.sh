#!/bin/sh

# Source:
# r9.32, Barrett, Daniel, Silverman, Richard, Byrnes, Robert "Linux Security Cookbook"
# O'Reilly & Associates, Sebastapol, CA, 2003
# http://www.oreilly.com/catalog/linuxsckbk/

# To use the functions in a shell script:
#   source syslog-api.sh
#   openlog `basename "$0"` pid local3
#   syslog warning "%d connections from %s" $count $host
#   syslog authpriv.err "intruder alert!"
#   closelog


ident="$USER"
facility="user"


# Specify the identifier prepended to each message, conventionally the basename
# of the program or script. An option is provided to add the process ID as
# well; other options are less commonly used. Finally, a default facility is 
# established for subsequent messages: local0 through local6 are good choices.

openlog( ) {
    if [ $# -ne 3 ]
    then
        echo "usage: openlog ident option[,option,...] facility" 1>&2
        return 1
    fi
    ident="$1"
    local option="$2"
    facility="$3"
    case ",$option," in
        *,pid,*)     ident="$ident[$$]";;
    esac
}


# Send messages. It is used like printf, with an added message priority. Specify
# a facility to override the default established by openlog: this should be done
# sparingly, e.g., to send security messages to authpriv. Each message should be
# a single line--omit newlines at the end of the messages too. As always with
# any printf-type API, you should never allow the format string to be supplied 
# by an untrusted source. Use at least "%s" as the format string.

syslog( ) {
    if [ $# -lt 2 ]
    then
        echo "usage: syslog [facility.]priority format ..." 1>&2
        return 1
    fi
    local priority="$1"
    local format="$2"
    shift 2
    case "$priority" in
        *.*)     ;;
        *)       priority="$facility.$priority";;
    esac
    printf "$format" "$@" | logger -t "$ident" -p "$priority"
}


# Close the socket used to communicate with the system logger. This function can
# be employed to clean up file descriptors before forking, but in most cases is
# optional.

closelog( ) {
    ident="$USER"
    facility="user"
}
