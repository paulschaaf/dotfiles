#!/usr/bin/env zsh

## enumerate associative array
# for key value in ${(@kv)associativeArray}; do ...; done

# shrink PDF: gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.6 -dPDFSETTINGS=/ebook -dNOPAUSE -dQUIET -dBATCH -sOutputFile=output.pdf input.pdf

## brace expansion - example
#  $ X=(A B C)
#  $ Y=(+ -)
#  $ print -r -- $^X.$^Y
#  A.+ A.- B.+ B.- C.+ C.-
#
## Lists every executable in PATH
#  $ print -l ${^path}/*(-*N)

## recursive chmod
#  $ chmod 700 **/(.) # Only files
#  $ chmod 700 **/(/) # Only directories

## List files beginning at `foo23' upwards (foo23, foo24, foo25, ..)
#  $ ls -l foo<23->

## List files in the current directory are not writable by the owner
#  $ print -l ~/*(ND.^w)


# Test on modification times
#  $ autoload -U age

## files modified today
#  $ print *(e:age today now:)

## files modified since 5 pm
#  $ print *(e-age 17:00 now-)

## ... since 5 o'clock yesterday
#  $ print *(e-age yesterday,17:00 now-)

## ... from last Christmas before today
#  $ print *(e-age 2006/12/25 today-)

## ... before yesterday
#  $ print *(e-age 1970/01/01 yesterday-)

## all files modified between the start of those dates
#  $ print *(e:age 2006/10/04 2006/10/09:)

## all files modified on that date
#  $ print *(e:age 2006/10/04:)

## Supply times.
#  $ print *(e-age 2006/10/04:10:15 2006/10/04:10:45-)

## MacOS kill listener on port 8081
# /bin/kill -9 $(lsof -i tcp:8081 -t) 2> /dev/null && print -P "%K{42}Killed%k" || print -P "%K{20}Not found%k"
