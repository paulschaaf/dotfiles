# -*- outline-regexp: "^[\t ]*#[\t ]+[^=]\\|^# +\\|^[\t ]*..." -*-

# Built:	Wed Sep 22 22:19:12 PDT 2004
# By:  	pschaaf@congo
# For:
#    host     = congo   shell    = zsh
#    uname    = Linux   term     = xterm

stty start '' stop ''

if [ -n "$CYGROOT" ]; then
    export HOME=/home/pschaaf
fi

: ${CLASSPATH:=.}
export CLASSPATH

typeset -U CDPATH FPATH MANPATH PATH

if [ -n "$CD_TO" ]; then
    builtin cd "$CD_TO" && unset CD_TO
fi

if [ -n "$PS1" ]; then
    interactive=true
fi

export COLUMNS

if [ "$TERM" = "emacs" ]; then
    unset USE_COLOR
fi

prependToList () {
    local each var=$1; shift
    for each; do
        member $each $var || eval "$var=\"$each\${$var:+:\$$var}\""
    done
}

appendToList () {
    local each var=$1; shift
    for each; do
        member $each $var || eval "$var=\"\${$var:+\$$var:}$each\""
    done
}

_prependToList () {
    local each var=$1; shift
    for each; do
        eval "$var=\"$each\${$var:+:\$$var}\""
    done
}

_appendToList () {
    local each var=$1; shift
    for each; do
        eval "$var=\"\${$var:+\$$var:}$each\""
    done
}

prependToVar () {
	local var=$1; shift; eval "$var=$*\${$var}"
}

appendToVar () {
	local var=$1; shift; eval "$var=\${$var}$*"
}

gwssh () {
    action=`~/bin/gwssh $@`
    echo $action; echo
    eval $action
}

llocate () {
    if [ -e locatedb ]; then
        command locate -d locatedb "$@"
    else
        command locate "$@"
    fi
}

member () {
	eval echo :"\$$2": | grep ":$1:" >&/dev/null
}

cd () {
    # [ -f $1 ] && builtin cd `dirname $1` || builtin cd $1
    if (( $# != 1 )); then
        builtin cd "$@"
    elif [[ -f "$1" ]]; then
        builtin cd "$1:h"
    else
        builtin cd "$1"
    fi
}

pcd () {
    cd "$@"
    popd +1
}

rcd () {
    pcd "$@"
}

rmpath () {
	PATH=`echo :$PATH | sed "s~:$1~~; s~^:~~"`
}

setenv () {
    local _label=$1; shift
    export $_label="$@"
}

exportDefault () {
    eval : \${$1:='$2'}
    export $1
}

removeFromList () {
    local each var=$1; shift
    for each; do
        eval "$var=\"\${$var//:\$each:/:}\""
    done
}

typeset -U CDPATH FPATH MANPATH PATH
# set -x
# ======== umask
case `hostname`:`uname` in
    stone:*)
        umask 22
        ;;
    *:CYGWIN*)
        umask 0
        ;;
    *)
        umask 22
        ;;
esac

# ======== Fix the Path

# echo a path without double slashes, or empty or relative paths
cleanpath () {
	echo :$PATH: | sed '
     s/:::*/:/g;
     s,///*,/,g;
     s,:c:/,:/c/,g;
     s/:[^/][^:]*//g;
     s_:[^:]*/pschaaf/bin/win__g;
     s/^://; s/:$//
   '
}
PATH=`cleanpath`


# ======== Terminal Settings
if [ "$TERM" = "screen" ]; then
export TERM=xterm
fi
if [ -n "$interactive" ]; then
: stty lnext '^V' eof '^D' quit '^\\' susp '^Z' start '^Q' eol '^-' flush '^O' werase '^W' eol2 '^-' erase '^H' stop '^S' min '1' time '0' intr '^C' rprnt '^R' kill '^U'
fi
# ======== Colors
if [ -n "$interactive" ]; then
if [ "$TERM" != "emacs" ]; then
export USE_COLOR=true
fi
fi
# ======== Unalias
unalias l la ll ls lsd rm d s p rd 2>/dev/null
# ======== Default Settings
exportDefault USER $LOGNAME
# ======== Shell Options
# limit coredumpsize 0
setopt  \
    ignore_eof \

unsetopt \
    beep \
    clobber \
    flow_control \
    hup \
    notify \
    print_exit_value \
    rm_star_silent \



# ======== Completion, Hashing
if [ -n "$interactive" ]; then
    echoVar () {	
        local var
        for var; do
            eval echo -E $var=\\\'\$$var\\\'
        done
    }

    alias mv='nocorrect mv'
    alias cp='nocorrect cp'

    # ======== Zsh Compinstall

    zstyle ':completion:*' expand prefix suffix
    zstyle ':completion:*' file-sort name
    zstyle ':completion:*' group-name ''
    zstyle ':completion:*' ignore-parents parent ..
    zstyle ':completion:*' list-colors ''
    zstyle ':completion:*' list-prompt '%SAt %p: Hit TAB for more, or the character to insert%s'
    zstyle ':completion:*' menu select=0
    zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
    zstyle ':completion:*' squeeze-slashes true
    zstyle ':zftp:' titlebar true
    zstyle ':completion:*' verbose true
    zstyle  :compinstall filename "$HOME/etc/env/$HOST.zsh"

    if [ ! "$HOST" = "ausable" ]; then
        autoload -U compinit
        compinit
    fi
    autoload zed
    alias fned='zed -f'
    unalias run-help 2>/dev/null
    autoload run-help


    # ======== Shell Options

    setopt \
        auto_list \
        auto_menu \
        auto_param_slash \
        auto_pushd \
        auto_remove_slash \
        bang_hist \
        correct \
        extended_history \
        function_argzero \
        hash_cmds \
        hash_dirs \
        hist_find_no_dups \
        hist_ignore_dups \
        hist_ignore_space \
        hist_no_functions \
        hist_reduce_blanks \
        inc_append_history \
        pushd_silent \
        pushd_to_home \


    # ======== Cd, Dirs, Popd, Pushd

    pushd () {
        # if target is a file, change to its location
        if [ -f $1 ]; then
            builtin pushd $1:h > /dev/null
        else
            builtin pushd $1 > /dev/null
        fi
    }

    cdl () {
        cd $*
        ls
    }

    frotz () {
        pushd ~/local/games/int-fic
        command frotz $* && popd
    }

    setfont () {
        local name
        case $1
        in
            unreadable)
            name=nil2
            ;;
        tiny)
            name=5x7
            ;;
        small)
            name=6x10
            ;;
        medium)
            name=7x13
            ;;
        large)
            name=9x15
            ;;
        huge)
            name=10x20
            ;;
        *)
            name=$1
            ;;
        esac
        echo "]50;$name"
    }

    wcat () {
        cat $(which $*)
    }

    wedit () {
        local cmd file x
        for x in $*
        do
            file=$(which $x) && cmd="${cmd:-edit} $file" || echo $file
        done
        eval $cmd
    }

    # ======== Aliases
    any_alias () {
        local label=$1; shift
        alias $label="$*"
    }

    alias basename='collect command\ basename'
    # alias createdb='$VERSANT_ROOT/bin/createdb'
    dirs () {
       builtin dirs ${*:--p}
    }

    alias ..='cd ..'
    alias -g ...=../..
    alias -g ....=../../..
    alias dirname='collect command\ dirname'
    alias df >& /dev/null || alias df='df -h -x supermount'
    alias du >& /dev/null || alias du='du -h'
    alias ECHO='echo'
    alias EHCO='echo'
    alias ehco='echo'
    alias exp='explorer'
    alias freshen='source ~/'
    alias igrep='grep -i'
    alias   isql='isql -S localhost -U sa -P 123'
    alias isqlw='isqlw -S localhost -U sa -P 123'
    alias jpegthumb='jegscale 128 $*'

    alias pstree='pstree -G'

    rlib () {
       links $@ $TECHREF/languages/ruby/stdlib/index.html
    }

    if [ "$UID" = "0" ]; then
        alias rpm-update='urpmi --noclean --update --auto-select'
    fi

    alias rm='rm -i'
    alias tree='tree --dirsfirst -ACF --ignore_backups --ignore_cvs'
    alias visudo='EDITOR=emacs visudo'
    alias which-command='path'

    bindkey -e

    # ======== Function Keys
    bindkey "[17~" kill-whole-line	# F6 cut
    bindkey "[18~" copy-region-as-kill	# F7 copy
    bindkey "[19~" yank	# F8 paste
    bindkey "[20~" history-incremental-search-backward	# F9 search
    if [ -z "$inScreen" ]; then
        bindkey -s "[24~" "exit\n"	# F12 exit
    fi

    # ======== Cursor Keys
    bindkey "\033[1;5D" backward-word	# ctrl_left
    bindkey "\033[1;5C" forward-word	# ctrl_right

    # ======== Other Extra Keys
    bindkey "\033[3~" delete-char	# delete

    bindkey "\177"    backward-kill-word	# delete

    # if [ -z "$inScreen" ]; then
        bindkey "\033[F"  end-of-line	      # end
        bindkey "\033[H"  beginning-of-line	# home
    # else
        bindkey "\033[4~" end-of-line	      # end
        bindkey "\033[1~" beginning-of-line	# home
    # fi


    # ======== Macro Keys
    bindkey -s ""^_"" "\C-apath \C-e\C-m"	# C-? prepends 'whence' to command line
#     if [ -z "$CONNECTION" ]; then
#         #bindkey -r "\C-h" 	# remove binding
#         #bindkey "\C-h\C-k" describe-key-briefly	#
#     fi

fi

# ======== ls Aliases
unalias l la ll ls lsd d s p rd 2>/dev/null
alias l='/bin/ls'

alias la='ls ${LA_FLAGS} --color'
alias ls='ls ${LS_FLAGS} --color'
alias ll='ls ${LL_FLAGS} --color'
alias lls=ll
alias lr='ls -R'
alias lla='la -l'
alias llt='ll -rt'
lld () {
	ll  $* | grep --color=never "/$\\|^$\\|^[^ ][^ ]*$"
}

lltt () {
	llt $* | grep "date +%b.%d"
}

lsd () {
    for x in ${*:-.}
    do
        pushd $x
        ls -BCHbd --color=always */
        popd
    done
}

lsda () {
    for x in ${*:-.}
    do
        pushd $x
        ls -BCHabd --color=always */ .*/
        popd
    done
}

alias lsad='lsda'
lsf () {
    local x
    for x in ${*:-*}
    do
        echo `file -N -F: -- $x`:`ls -d -- $x` | sed 's/^[^:]*: //'
    done | column -t -s ':'
}

classpath () {
   echo Examining \$CLASSPATH
    echo
    echoList `cygpath -aup $CLASSPATH` $*
}

fpath () {
    echo Examining \$FPATH
    echo
    echoList $FPATH $*
}

infopath () {
    echo Examining \$INFOPATH
    echo
    echoList $INFOPATH $*
}

libpath () {
    echo Examining \$\$LD_LIBRARY_PATH
    echo
    echoList $LD_LIBRARY_PATH $*
}

manpath () {
    echo Examining \$MANPATH
    echo
    echoList ${MANPATH:-`man -w`} $*
}

path () {
    local regex="${*// /\|}"
    whence $* |
    sed '/^$\|^\/\|not found$/d; s:^\('$regex'\):[1;40;33m\1[0;0m:g' | pr -rto 2
    echo
    echoList $PATH $*
}

rubylib () {
    echo Examining \$RUBYLIB
    echo
    echoList `ruby -e "print $:.join(':'); %w($*).each {|e| print %Q< #{e} #{e}.rb>}"`
}

appendLibpath () {
    appendToList LD_LIBRARY_PATH $*
}

prependLibpath () {
    prependToList LD_LIBRARY_PATH $*
}

appendManpath () {
    appendToList MANPATH $*
}

prependManpath () {
    prependToList MANPATH $*
}

appendPath () {
    appendToList PATH $*
}

prependPath () {
    prependToList PATH $*
}

appendClasspath () {
    local cp=`cygpath -apu $CLASSPATH`
    appendToList cp `cygpath -au $*`
    CLASSPATH=`cygpath -apw $cp`
}

prependClasspath () {
    local cp=`cygpath -apu $CLASSPATH`
    prependToList cp `cygpath -au $*`
    CLASSPATH=`cygpath -apw $cp`
}

appendRubyLib () {
    appendToList RUBYLIB $*
}

prependRubyLib () {
    prependToList RUBYLIB $*
}

updateManpath () {
    appendManpath `echo $PATH | awk '/\/bin$/ {gsub(/\/bin$/, "/man"); print}' RS=':'`
}


# ======== Functions
clean_gutenberg () {
    ruby -e "File.read(%q-$1-).instance_eval {
        gsub!(/\r/,             %q--);
        gsub!(/(\S)\n(\S)/,     %q-\1 \2-);
        gsub!(/(\n)(\S)/,       %q-\1 \2-);
        gsub!(/^\t(chapter )/i, %q-\1-);
        puts self;
    }"
}

cleanusr () {
    su -l cleanusr
}

collect () {
    local cmd x
    cmd=$1; shift
    for x
    do
        eval $cmd $x
    done
}

cvs-commit () {
    echo
    TRAPEXIT () {
        popd
    }
    pushd $HOME
    [ -x bin/color-cvs-output.rb ] || alias color-cvs-output='cat'
    for x in `find $HOME -type d ! -regex ".*/\(courses\|sections\|cvsroot\|etc\).*" -name CVS -maxdepth 2 -printf '%h '` $HOME/etc
    do
        echo "[1;33m"\=============== $x"[0m"
        pushd $x
        cvs -q commit $*
        make 2>&1 | color-cvs-output.rb
        popd
    done
}

cvs-diff () {
    echo
    TRAPEXIT () {
        popd
    }
    pushd $HOME
    [ -x bin/color-cvs-output.rb ] || alias color-cvs-output='cat'
    for x in `find $HOME -type d ! -regex ".*/\(courses\|sections\|cvsroot\|etc\).*" -name CVS -maxdepth 2 -printf '%h '` $HOME/etc
    do
        echo "[1;33m"\=============== $x"[0m"
        pushd $x
        cvs -q diff -R $*
        echo | color-cvs-output.rb
        popd
    done
}

cvs-update () {
    echo
    TRAPEXIT () {
        popd
    }
    pushd $HOME
    [ -x bin/color-cvs-output.rb ] || alias color-cvs-output='cat'
    for x in `find $HOME -type d ! -regex ".*/\(courses\|sections\|cvsroot\|etc\).*" -name CVS -maxdepth 2 -printf '%h '` $HOME/etc
    do
        echo "[1;33m"\=============== $x"[0m"
        pushd $x
        cvs -q update -d -kb $*
        echo
        make 2>&1 | color-cvs-output.rb
        popd
    done
}

ddu () {
    find ${*:-.} -type d -maxdepth 1 -exec du -hs {} \; | sort -n
}

homeToTilde () {
    echo "$@" | sed 's,\(^\|:\)'$HOME',\1~,g'
}

echoList () {
    local list=$1
    shift
    echo $list | awk '
    NR == 1 {split(file, files, " ")}

    function output(string) {outstr = outstr string}

    { output(sprintf(" %2d ", NR))

      for (idx in files) {
        fullname = " " $1 "/" files[idx]
        if (! system("test -f" fullname)) {
            output("*")
            matches = matches fullname
            break
        }
        }

        gsub(ENVIRON["HOME"],"~")
        output($0 "\n")
    }
    END {
        system("echo \"" outstr "\"|column")
        if (matches) {
            print "\n" cmd matches "\n"
            system("for x in" matches "\ndo\n" cmd " $x\ndone")
        }
    }' RS=: file="$@" cmd="ls $LL_FLAGS" | sed 's/ \*\([^ 	]*\)/ [1;33m\1[0m/g'
}

jpegscale () {
    boundary=$1; shift
    for file in $*
    do
        jpegtopnm $file | pnmscale -xysize $boundary $boundary | pnmtojpeg >| ${file/./_$boundary.}
    done
}

palm () {
	 ruby -I\"$RUBYLIB\" bin/palm $*
}

# Save script args in variable to allow access inside script
script () {
	SCRIPT_ARGS="script $*" command script $*
}

signals () {
    echo "   \$signals="
    echo $signals | tr " " "
" | cat -n | awk ' {print "    ", $1-1, $2}'  | column
}

smspaul () {
    sendSMS.pl -r 4086444762 -p TMOBILE -s $EMAIL
}

smsvic () {
    sendSMS.pl -r 4086444769 -p TMOBILE -s $EMAIL
}

syncdir () {
    make ${1:+-C $1} -ef ~/etc/makefiles/syncdir.make
}

wcd () {
    cd $(which $1)
}

whence () {
    builtin whence -ac $*
}

wless () {
    $PAGER `which $*` 2>/dev/null || echo ${args} not found
}

if [ "$UID" = "0" ]; then
    emacs () {
        command emacs --user pschaaf $*
    }
    xemacs () {
        command xemacs --user pschaaf $*
    }
fi

vicdo () {
    eval sudo /bin/su -l vschaaf -- --restricted ${*:+-c "'$*'"}
}

# ======== ssh
vftp () {
    ssh-add -l | grep -q 'ftp.versant.com' || ssh-add ~/.ssh/ftp
    ssh -Cqt ftp.versant.com exec gnu/zsh ${*:--l}
}

cvsls () {
    local files=( $( cvs status -l "$*" 2>/dev/null | awk "/File: / { print $2 }" ) )
    ls -d "$*" *(/) "$files[@]"
}

# ======== Printers
export PRINTER=//gwiredc1/hplj2100
export MOZ_PRINTER_NAME=$PRINTER
# ======== Cygwin Path
   #if_defined('__PWD')
     # Perhaps win could not successfully change to this dir,
     # e.g. it is a UNC path. We must get there ourselves.
     #eval cd $(ruby -e "puts Regexp.escape(ENV['__PWD'].gsub('-', '__MINUS__')).gsub('__MINUS__', '-')")
   #shell_endif
   #if there are spaces in pwd, convert to short (mangled) path
   if [ `expr $PWD : '.* '` -gt 0 ]; then
     cd $(cygpath -u $(cygpath -asw "$PWD"))
   fi
   unset __PWD

# ======== Classpath
appendClasspath $ANT_HOME/lib/ant.jar


# ======== Path
# prependPath /bin /usr/bin /usr/local/bin /usr/X11R6/bin
# appendPath /sbin /usr/sbin /usr/local/sbin $JAVA_HOME/bin $HOME/bin /opt/cxoffice/bin   #
# appendPath /opt/bin /opt/$USER/firefox /opt/$USER/thunderbird /opt/cxoffice/bin         #
# if [ -n "$VERSANT_ROOT" ]; then
#     appendPath $HOME/bin/vsnt $VERSANT_ROOT/bin
# fi


# ======== Library Path
appendLibpath /lib /usr/lib $HOME/lib
appendRubyLib $HOME/lib/ruby $HOME/bin
export RUBYLIB

# ======== Prompt
if [ -n "$CONNECTION" ]; then
    CONNECTION_END=' '
fi
if [ -n "$interactive" ]; then
    case $TERM
    in
        emacs)
            chpwd () {
                local dirs
                dirs="`builtin dirs -p |
                awk 'NR == 2 {printf("%s", "")}; {print}' ORS=' '`"
                PS1="$CTVIEW
%{%(!.[41;1;37m.)%}%T %n@%m %62>\>>$dirs%<<
%2(L.%L-.)%{k\%(!.[41;1;37m.)%}%! %{$CONNECTION_COLOR%}$CONNECTION$CONNECTION_END$SCRIPTING_MSG%(!.#.$) "
            }

            PS2="(%_) %(!.#.$) "
            ;;
        *)
            chpwd () {
                local dirs
                dirs="`builtin dirs -p |
                awk 'NR == 2 {printf("%s", "[0;33m")}; {print}' ORS=' '`"
                PS1="$CTVIEW
%{[0m%}%T %{%(!.[1;41;37m.[1;42;37m)%}%n@%m%{[0m%} %{[1;33m%}%62>\>>$dirs%<<%{[0m%}
%2(L.%L-.)%{k\[1;36m%}%!%{[0m%} %{$CONNECTION_COLOR%}$CONNECTION%{[0m%}$CONNECTION_END$SCRIPTING_MSG%(!.#.$) "
            }

            PS2="(%{[1;35m%}%_%{[0m%}) %(!.#.$) "
            ;;
    esac

    chpwd
fi

# ======== Cygwin
if [ -n "$CYGROOT" ]; then
    prependPath $HOME/bin/win
fi

# ======== Manpath

# Move this heredoc around in the file to temporarily disable sections
# of code. Make sure to keep the opening tag first, and the closing tag
# against the left margin on a line by itself. Don't forget that any
# edits are lost if this file is re-generated.
: <<DEBUG_NO_EXEC
DEBUG_NO_EXEC
#

# Used packages:
#	ant		column		cvs		cxoffice	emacs
#	gnuclient	grep_color	grep_gnu	info		java_sdk
#	less		ls_color	ls_gnu		msoffice	ruby
#	screen		sms		ssh		suroot		tree
#	versant		vi		vim		x_contrib	xemacs
