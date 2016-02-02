# -*- mode: shell-script; outline-regexp: "^[\t ]*#[\t ]+[^=]\\|^# +\\|^[\t ]*..." -*-

loginsh=1

if [ -n "$PS1" ]; then
    interactive=true
fi

typeset -U CDPATH FPATH MANPATH PATH

PATH=$HOME/bin/win:$PATH:$HOME/bin

# ======== Terminal Settings
# stty eol '^-' werase '^?' eol2 '^-' erase '^H' eof '^D' start '^Q' intr '^C' rprnt '^R' time '0' lnext '^V' stop '^S' quit '^\\' min '1' flush '^O' susp '^Z' kill '^U' 

# ======== Default Settings
# export HOSTNAME=$COMPUTERNAME   # $COMPUTERNAME is all caps
export HOSTNAME=`hostname`

: ${USER:=$LOGNAME}
export USER

export TECHREF=$HOME/doc/TechRef

# ======== Environment and Shell Variables
# : ${ANT_HOME:=/usr/local/ant}
# export ANT_HOME

export BROWSER=mozclient
export CLHSROOT=$TECHREF/languages/lisp/HyperSpec/
export COLUMNS

# ======== Remote sessions
if [ -n "$SSH_TTY" ]; then
    export CONNECTION=ssh
    export CONNECTION_COLOR='[1;33m'
elif [ -n "$REMOTEHOST" ]; then
    export CONNECTION=tlnt
    export CONNECTION_COLOR='[1;31m'
elif [ -n "$SUDO_USER" ]; then
    export CONNECTION=sudo
    export SUDO_COLOR='[43;30m'
elif [ -n "$REMOTEUSER" ]; then
    export CONNECTION=tlnt
    export CONNECTION_COLOR='[1;31m'
fi

export CYGROOT='c:\cygwin'
export CVS_SOURCEFORGE=:pserver:anonymous@cvs.sourceforge.net:/cvsroot
export SVNROOT=":extssh:${USER}@service01:/cvsroot"
: ${EDITOR=/usr/bin/emacs}
export EDITOR
case $USER
in
    vschaaf)
        export EMAIL=designer007@excite.com
        ;;
    pschaaf)
        export EMAIL=paul.schaaf@gmail.com
        ;;
    root)
        export EMAIL=paul.schaaf@gmail.com
        ;;
esac

# if [ "$USER" = "pschaaf" ]; then
#     export ESHELL=`$HOME/bin/pref_shell`
# else
    export ESHELL=$SHELL
# fi

export FCEDIT=zed
export FIGNORE=.class:.o:\~:.stackdump:dump.txt:nohup.out
export GREP_COLOR="[1;33"
export GREP_OPTIONS=--color
export HELPDIR=$HOME/local/sync/zsh_help
export HISTSIZE=1000

case $HOSTNAME in
    ausable)
        export JAVA_HOME='c:\j2sdk1.4.2_04'
        ;;
    menominee)
        export JAVA_HOME='c:\j2sdk1.4.2_05'
        ;;
    styx)
        export JAVA_HOME='c:\j2sdk1.4.2_03'
        ;;
    *)
        ;;
esac


export LESS=' --RAW-CONTROL-CHARS --hilite-unread --ignore-case --jump-target=2 --no-init --status-column -Ps%dt/%D ?f%f::STDIN:$'
export LESSKEY=/home/pschaaf/.less_$HOSTNAME
export LIBPATH_NAME=LD_LIBRARY_PATH

export LS_FLAGS='-BCFHb'
export LA_FLAGS="${LS_FLAGS}A"
export LL_FLAGS="${LS_FLAGS}hl"

export LS_COLORS="no=00:fi=00:di=01;34:ln=01;36:pi=40;33:so=01;35:bd=40;33;01:cd=40;33;01:or=01;37;41:mi=01;37;41:ex=01;32:*.btm=01;32:*.tar=01;31:*.tgz=01;31:*.tbz2=01;31:*.arc=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.lha=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.bz=01;31:*.tz=01;31:*.rpm=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.xbm=01;35:*.xpm=01;35:*.png=01;35:*.tif=01;35:*.tiff=01;35"

unalias l la ll ls lsd d s p rd 2>/dev/null
alias l='/bin/ls'

alias la='ls ${LA_FLAGS} --color'
alias ls='ls ${LS_FLAGS} --color'
alias ll='ls ${LL_FLAGS} --color'
alias lls=ll
alias lr='ls -R'
alias lla='la -l'
alias llt='ll -rt'

export OFFICE_DRIVE=C:
export OFFICE_PATH="Program Files//Microsoft Office//Office"
export PAGER=less
export PDA_ROOT=:palm
export PDA_INSTALL_DIR=$PDA_ROOT/Install
export PLUCKERHOME=$HOME/local/sync/software/man.html
export PRINTER=lj2100
    export MOZ_PRINTER_NAME=$PRINTER

export RSYNC_RSH=ssh

: ${RUBYLIB:=$HOME/lib/ruby:$HOME/bin}
export RUBYLIB

: ${SCREENDIR:=$HOME/tmp}
export SCREENDIR

export SHELL=zsh

# export VERSANT_ROOT=/usr/local/versant/vds605
# if [ -n "$VERSANT_ROOT" ]; then
#     : ${VERSANT_DB:=`dirname $VERSANT_ROOT`/db}
#     export VERSANT_DB

#     : ${VERSANT_DBID:=$VERSANT_DB}
#     export VERSANT_DBID

#     : ${VERSANT_DBID_NODE:=congo}
#     export VERSANT_DBID_NODE
# fi

# if [ -n "$SCRIPT_ARGS" ]; then
#     export SCRIPTING_MSG='[cc] '
# fi

export SVN_EDITOR='emacs -nw'
export UNAME=CYGWIN
export USE_COLOR=true


if [ -n "$EDITOR" ]; then
    export VISUAL=$EDITOR
fi

# Any punctuation char in WORDCHARS is considered a part of the adjacent word.
# The remaining punctuation chars are considered separate words, regardless of
# what may be adjacent:  !"#&'()+,./:;<=>?@[]`{|}
export WORDCHARS='*?.-~\\$%^'

export ZDOTDIR=/home/pschaaf