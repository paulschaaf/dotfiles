#!/bin/sh

# $Source: e:/MyDocuments/cvsroot/src/Makefile,v $
# $Revision: 1.1 $
# $Date: 2003/08/30 00:16:28 $
# $Author: pschaaf $
# $State: Exp $
# $Name:  $

#  cgrep - multiline context grep using sed
#  Usage: cgrep [-context] pattern [file...]
# Taken from the O'Reilly book "Unix Power Tools"

n=3
case $1 in -[1-9]*)
  n=`expr 1 - "$1"`
  shift
esac
re=${1?}; shift

sed -n "
  1b start
  : top
  \~$re~{
		h; n; p; H; g
		b endif
  }
		N
		: start
		//{ =; p; }
  : endif
  $n,\$D
  b top
" "$@"