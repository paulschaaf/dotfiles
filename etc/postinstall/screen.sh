#!/bin/sh
prefix=/usr
infodir=${prefix}/share/info
cd ${infodir}
for fn in screen.info ; do 
  install-info --dir-file=./dir --info-file=${fn}
done

