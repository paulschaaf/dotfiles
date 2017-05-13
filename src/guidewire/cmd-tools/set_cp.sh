#!/bin/sh

if [ "$1" = "-d" ]
then
    shift
    set -x
fi

# if [ -d lib ]; then
#     lib=lib
# elif [ -d ../lib ]; then
#     lib=../lib
# elif [ -d ../../lib ]; then
#     lib=../../lib
# fi

lib=`find . .. ../.. ../../.. -maxdepth 1 -type d -name lib -print -quit`

# echo lib=$lib

cp="${lib}/log4j-1.2.8.jar:${lib}/commons-cli-1.0.jar:${lib}/GLUE-STD-4.1.2.jar:${lib}/gw-toolkit.jar:${lib}/gw-util.jar:${lib}/servlet.jar:${lib}/dom.jar:${lib}/jsse.jar:${lib}/jcert.jar:${lib}/jnet.jar"

case `uname`
in
    CYGWIN*)
        cp=`cygpath -wp "$cp"`
        ;;
    *)
        ;;
esac

# echo cp=$cp; echo
