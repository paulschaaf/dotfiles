#!/bin/sh

# $Source: e:/MyDocuments/cvsroot/src/Makefile,v $
# $Revision: 1.1 $
# $Date: 2003/08/30 00:16:28 $
# $Author: pschaaf $
# $State: Exp $
# $Name:  $

case "`uname -s`" in
	CYGWIN*)
		# CYGWIN detected
		separator=\;
		;;
	*)
		# Unix detected
		separator=:
		;;
esac

for dirOrJar in `echo "$CLASSPATH" | sed "s/\$separator/ /g"`
do
	if [ `grep --files-with-matches "$1" $dirOrJar` ]; then
		echo $dirOrJar
		exit
	fi
done
