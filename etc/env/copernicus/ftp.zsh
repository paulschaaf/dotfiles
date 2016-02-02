# For:
#    host     = ftp          shell    = /export/home/ftp/gnu/zsh
#    uname    = SunOS        term     = xterm     

if [ -n "$PS1" ]; then
interactive=true
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

export HOSTNAME=ftp
echoVar () {
	
      local var
      for var
      do
        eval echo -E $var=\\\'\$$var\\\'
      done
}

member () {
	eval echo :"\$$2": | grep ":$1:" >&/dev/null
}

rmpath () {
	PATH=`echo :$PATH | sed "s~:$1~~; s~^:~~"`
}

setenv () {
	local _label=$1; shift; export $_label="$@"
}

exportDefault () {
	eval : \${$1:='$2'}; export $1
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
# This umask gives these access permissions:
# user=rwx, group=rx, others=rx
umask 22

# ======== Fix the Path
# echo a path without double slashes, or empty or relative paths
cleanpath () {
	echo :$PATH: | sed 's/:::*/:/g; s/:[^/][^:]*//g; s/^://; s/:$//'
}

# ======== Terminal Settings
if [ "$TERM" = "screen" ]; then
export TERM=xterm
fi
if [ -n "$interactive" ]; then
: stty eol '^-' werase '^W' eol2 '^-' erase '^?' eof '^D' start '^Q' intr '^C' rprnt '^R' time '0' lnext '^V' stop '^S' quit '^\\' min '1' flush '^O' susp '^Z' kill '^U' 
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
setopt no_hup
setopt ignore_eof
setopt no_clobber
setopt nonotify
setopt no_beep
bindkey -e
limit coredumpsize 0
unsetopt rm_star_silent
unsetopt print_exit_value
# ======== Completion, Hashing
if [ -n "$interactive" ]; then
setopt hash_cmds hash_dirs
setopt auto_list auto_menu auto_param_slash auto_remove_slash
export FIGNORE=.class:.o:\~:.stackdump:dump.txt:nohup.out
setopt   correct; alias mv='nocorrect mv'; alias cp='nocorrect cp'
fi
# ======== Zsh Compinstall
if [ -n "$interactive" ]; then
autoload zed
alias fned='zed -f'
unalias run-help 2>/dev/null
autoload run-help
export HELPDIR=$HOME/local/sync/zsh_help
fi
# ======== Cd, Dirs, Popd, Pushd
if [ -n "$interactive" ]; then
setopt auto_pushd


setopt pushd_silent pushd_to_home
pushd () {
	# if target is a file, change to its location
         if [ -f $1 ]; then
           builtin pushd $1:h > /dev/null
         else
           builtin pushd $1 > /dev/null
         fi
         
}

fi
# ======== Shell History
if [ -n "$interactive" ]; then
setopt hist_ignore_dups hist_ignore_space hist_reduce_blanks
export HISTSIZE=1000
setopt extended_history
setopt bang_hist
setopt function_argzero
fi
# ======== Aliases
if [ -n "$interactive" ]; then
any_alias () {
	local label=$1; shift; alias $label="$*"
}

alias basename='collect command\ basename'
alias createdb='$VERSANT_ROOT/bin/createdb'
dirs () {
	builtin dirs ${*:--p}
}

alias dirname='collect command\ dirname'
alias df >& /dev/null || alias df='df -h'
alias du >& /dev/null || alias du='du -h'
alias ECHO='echo'
alias EHCO='echo'
alias ehco='echo'
alias file='/usr/ucb/file'
alias freshen='source ~/'
alias igrep='grep -i'

alias man='man -F'
export PAGER=less
alias rm='rm -i'
alias which-command='path'
alias whoami='who am i | awk "{print \$1}"'
fi
# ======== Clearcase Aliases
# ======== ls Aliases
export LS_BIN=$HOME/gnu/ls
alias l='$LS_BIN'
export LS_COLORS="no=00:fi=00:di=01;34:ln=01;36:pi=40;33:so=01;35:bd=40;33;01:cd=40;33;01:or=01;05;37;41:mi=01;05;37;41:ex=01;32:*.btm=01;32:*.tar=01;31:*.tgz=01;31:*.tbz2=01;31:*.arc=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.lha=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.bz=01;31:*.tz=01;31:*.rpm=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.xbm=01;35:*.xpm=01;35:*.png=01;35:*.tif=01;35:*.tiff=01;35"
export LA_FLAGS='-ACFb --color=always'
export LS_FLAGS='-BCFb --color=always'
export LL_FLAGS='-BCFbhl --color=always'
alias la='$HOME/gnu/ls -ACFb --color=always'
alias ls='$HOME/gnu/ls -BCFb --color=always'
alias ll='$HOME/gnu/ls -BCFbhl --color=always'
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
            $HOME/gnu/ls -BCbd --color=always */
	    popd
        done
}

lsda () {
	        for x in ${*:-.}
        do
            pushd $x
            $HOME/gnu/ls -BCabd --color=always */ .*/
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

# ======== Path Functions
if [ -n "$interactive" ]; then
classpath () {
	echo Examining \$CLASSPATH; echo; echoList $CLASSPATH $*
}

fpath () {
	echo Examining \$FPATH; echo; echoList $FPATH $*
}

libpath () {
	echo Examining \$\$LD_LIBRARY_PATH; echo; echoList $LD_LIBRARY_PATH $*
}

manpath () {
	echo Examining \$MANPATH; echo; echoList ${MANPATH:-`man -w`} $*
}

path () {
	local regex="${*// /\|}"
                            whence $* | 
                              sed '/^$\|^\/\|not found$/d; s:^\('$regex'\):[1;40;33m\1[0;0m:g' | pr -rto 2; echo; echoList $PATH $*
}

fi
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
	appendToList CLASSPATH $*
}

prependClasspath () {
	prependToList CLASSPATH $*
}

updateManpath () {
	appendManpath `echo $PATH | awk '/\/bin$/ {gsub(/\/bin$/, "/man"); print}' RS=':'`
}

# ======== Functions
cdl () {
	cd $*; ls
}


collect () {
	local cmd x
cmd=$1; shift
for x
do
eval $cmd $x
done
}

ctset () {
	source ~/bin/ctset
}

ddu () {
	find ${*:-.} -type d -maxdepth 1 -exec du -hs {} \; | sort -n
}

echoList () {
	    local list=$1
    shift
    echo $list | sed 's,\(^\|:\)'$HOME',\1~,g' | awk '
    NR == 1 {split(file, files, " ")}

    function output(string) {outstr = outstr string}

    {
      output(sprintf(" %2d ", NR))

      for (idx in files) {
        fullname = " " $1 "/" files[idx]
        if (! system("test -f" fullname)) {
          output("*")
          matches = matches fullname
          break
        }
      }

      output($0 "\n")
    }
    END {
      printf("%s", outstr)
      if (matches) {
        print "\n" cmd matches "\n"
        system("for x in" matches "\ndo\n" cmd " $x\ndone")
      }
    }' RS=: file="$*" cmd="$HOME/gnu/ls $LL_FLAGS" | sed 's/ \*\([^ 	]*\)/[1;33m \1[0m/g'
}

edit () {
	$EDITOR $* &
}


lite () {
	egrep -a -C 99999 ${*:-''}
}




readlink () {
	file $1 | grep ymbolic.link | sed s/^.*ymbolic.link.to.//
}

# Save script args in variable to allow access inside script
script () {
	SCRIPT_ARGS="script $*" command script $*
}


signals () {
	echo "   \$signals="; echo $signals | tr " " "
" | cat -n | awk ' {print "    ", $1-1, $2}' 
}

syncdir () {
	make ${1:+-C $1} -ef ~/etc/makefiles/syncdir.make
}

wcat () {
	cat $(which $*)
}

wcd () {
	cd $(which $1)
}

wedit () {
	local cmd file x
         for x in $*
         do
           file=$(which $x) && cmd="${cmd:-edit} $file" || echo $file
         done
       eval $cmd
}

whence () {
	builtin whence -ac $*
}

wless () {
	$PAGER `which $*` 2>/dev/null || echo ${args} not found
}

# ======== ssh
# ======== Remote sessions
if [ -n "$SSH_TTY" ]; then
export CONNECTION_COLOR='[1;33m'
export CONNECTION=ssh
elif [ -n "$REMOTEHOST" ]; then
export CONNECTION_COLOR='[1;31m'
export CONNECTION=tlnt
elif [ -n "$SUDO_USER" ]; then
export SUDO_COLOR='[43;30m'
export CONNECTION=sudo
elif [ -n "$inScreen" ]; then
export CONNECTION_COLOR='[1;31m'
export CONNECTION=tlnt
fi
if [ -n "$CONNECTION" ]; then
CONNECTION_END=' '
fi
# ======== Environment and Shell Variables

export COLUMNS
if [ -n "$CLEARCASE_ROOT" ]; then
export CTVIEW=`$HOME/bin/getCTView`
if [ -n "$CTVIEW" ]; then
CTVIEW="
[1;35m$CTVIEW[0m"
fi
expr "`oscp -v`" \< "6" > /dev/null && any_alias makedb "makedb -g"
fi
export EDITOR=vi
export ENSCRIPT='-M letter'
EXIT=exit
export FCEDIT=zed
export GREP_COLOR="[1;33"
export GREP_OPTIONS=--color
export JAVA_COMPILER=none
export LESS='-EXeiw -j 2 -Ps%dt/%D ?f%f::STDIN:$'
export LIBPATH_NAME=LD_LIBRARY_PATH
exportDefault SCREENDIR $HOME/tmp
if [ -n "$SCRIPT_ARGS" ]; then
export SCRIPTING_MSG='[cc] '
fi
export SHELL=/export/home/ftp/gnu/zsh
export THREADS_FLAG=native
export UNAME=SunOS
if [ -n "$EDITOR" ]; then
export VISUAL=$EDITOR
fi
# Any punctuation char in WORDCHARS is considered a part of the adjacent word.
export WORDCHARS='*_-~\$%^'
# The remaining punctuation chars are considered separate words, regardless of what may be adjacent:
# 	!"#&'()+,./:;<=>?@[]`{|}
export VERSANT_ROOT=/usr/local/versant/vds605
# ======== Printers
export PRINTER=lj1
export LP_DEST=$PRINTER
# ======== Cygwin Path
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
bindkey "\033[4~" end-of-line	# end
bindkey "\033[1~" beginning-of-line	# home



# ======== Macro Keys
bindkey -s ""^_"" "\C-apath \C-e\C-m"	# C-? prepends 'whence' to command line
if [ -z "$CONNECTION" ]; then
bindkey -r "\C-h" 	# remove binding
bindkey "\C-h\C-k" describe-key-briefly	# 
fi
# ======== Lesskey
# ======== Classpath
# ======== Path
prependPath /bin /usr/bin /usr/local/bin $HOME/gnu
appendPath /sbin /usr/sbin $HOME/bin $HOME/bin/usr-local-bin
if [ -n "$VERSANT_ROOT" ]; then
appendPath $HOME/bin/vsnt $VERSANT_ROOT/bin
exportDefault VERSANT_DB `dirname $VERSANT_ROOT`/db
exportDefault VERSANT_DBID $VERSANT_DB
exportDefault VERSANT_DBID_NODE ftp
fi
# ======== Library Path
appendLibpath /lib /usr/lib $HOME/lib
# ======== Prompt
if [ -n "$interactive" ]; then
case $TERM
in
emacs)
	
chpwd () {
	local dirs
       dirs="`builtin dirs | tr -d '\012' |
       awk 'NR == 2 {printf("%s", "")}
            {print}' ORS=' '`"
PS1="$CTVIEW
%{%(!.[41;1;37m.)%}%T %n@ftp %69>\>>$dirs%<<
%2(L.$SHLVL-.)%{k\%(!.[41;1;37m.)%}%! %{$CONNECTION_COLOR%}$CONNECTION$CONNECTION_END$SCRIPTING_MSG$ "
}

PS2="(%_) %(!.#.$) "
;;
*)
	
chpwd () {
	local dirs
       dirs="`builtin dirs | tr -d '\012' |
       awk 'NR == 2 {printf("%s", "[0;33m")}
            {print}' ORS=' '`"
PS1="$CTVIEW
%{%(!.[41;1;37m.[0m)%}%T%{[0m%} %{[1;33m%}%n@%{[1;33m%}ftp%{[0m%} %{[1;33m%}%69>\>>$dirs%<<%{[0m%}
%2(L.$SHLVL-.)%{k\%(!.[41;1;37m.[1;36m)%}%!%{[0m%} %{$CONNECTION_COLOR%}$CONNECTION%{[0m%}$CONNECTION_END$SCRIPTING_MSG$ "
}

PS2="(%{[1;35m%}%_%{[0m%}) %(!.#.$) "
;;
esac

chpwd
fi
# ======== Cygwin
# ======== Manpath
updateManpath

# Used packages: 
#	grep_color	grep_gnu	less		ls_color	ls_gnu
#	ssh		versant		vi
