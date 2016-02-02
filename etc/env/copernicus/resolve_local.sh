# $Source: e:/MyDocuments/cvsroot/etc/env/resolve_local.sh,v $
# $Revision: 1.11 $
# $Date: 2004/06/11 01:25:54 $
# $Author: pschaaf $
# $State: Exp $
# $Name:  $

_extn=`basename $0 | sed 's/^-//g; s/^\.\(.*\)rc$/\1/g'`

export HOSTNAME=`hostname | sed 's/\..*//g'`
#export HOST=$HOSTNAME

_file=$HOME/etc/env/${HOSTNAME}.$_extn

if [ -r  $_file ]; then
    source $_file
else
    #echo No custom startup file found \("$_file"\).
    #echo Using generic startup file $HOME/etc/env/generic.$_extn
    source $HOME/etc/env/generic.$_extn || echo ERROR: Can not source $HOME/etc/env/generic.$_extn
fi

unset _extn _file