# For:
#    host     = ftp          shell    = sh        
#    uname    = SunOS        term     = xterm     

if [ -n "$PS1" ]; then
interactive=true
fi
prependToList () {
	local each var=$1; shift
for each
	do
			member $each $var || eval "$var=\"$each\${$var:+:\$$var}\""
		done
}

appendToList () {
	local each var=$1; shift
for each
	do
			member $each $var || eval "$var=\"\${$var:+\$$var:}$each\""
		done
}

_prependToList () {
	local each var=$1; shift
for each
	do
			eval "$var=\"$each\${$var:+:\$$var}\""
		done
}

_appendToList () {
	local each var=$1; shift
for each
	do
			eval "$var=\"\${$var:+\$$var:}$each\""
		done
}

prependToVar () {
	local var=$1; shift; eval "$var=$*\${$var}"
}

appendToVar () {
	local var=$1; shift; eval "$var=\${$var}$*"
}

HOSTNAME=ftp; export HOSTNAME
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
TERM=xterm; export TERM
fi
if [ -n "$interactive" ]; then
: stty eol '^-' werase '^W' eol2 '^-' erase '^?' eof '^D' start '^Q' intr '^C' rprnt '^R' time '0' lnext '^V' stop '^S' quit '^\\' min '1' flush '^O' susp '^Z' kill '^U' 
fi
# ======== Colors
if [ -n "$interactive" ]; then
if [ "$TERM" != "emacs" ]; then
USE_COLOR=true; export USE_COLOR
fi
fi
# ======== Unalias
unalias l la ll ls lsd rm d s p rd 2>/dev/null
# ======== Default Settings
exportDefault USER $LOGNAME
# ======== Shell Options






ulimit -c 0


# ======== Completion, Hashing
if [ -n "$interactive" ]; then

FIGNORE=.class:.o:\~:.stackdump:dump.txt:nohup.out; export FIGNORE
fi
# ======== Zsh Compinstall
# ======== Cd, Dirs, Popd, Pushd
if [ -n "$interactive" ]; then
cd () {
	builtin cd $*; pushd -n $OLDPWD > /dev/null
}

popd () {
	builtin popd #{args} && chpwd
}

pushd () {
	# if target is a file, change to its location
         if [ -f $1 ]; then
           pushd $(dirname $1) > /dev/null
         else
           pushd $1 > /dev/null
         fi
         chpwd
}

fi
# ======== Shell History
if [ -n "$interactive" ]; then
fi
# ======== Aliases
if [ -n "$interactive" ]; then
any_alias () {
	local label=$1; shift; alias $label="$*"
}

alias basename='collect command\ basename'
alias bye='exit'
alias createdb='$VERSANT_ROOT/bin/createdb'

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
PAGER=less; export PAGER
alias rm='rm -i'
alias which-command='path'
alias whoami='who am i | awk "{print \$1}"'
fi
# ======== Clearcase Aliases
# ======== ls Aliases
LS_BIN=$HOME/gnu/ls; export LS_BIN
alias l='$LS_BIN'
LS_COLORS="no=00:fi=00:di=01;34:ln=01;36:pi=40;33:so=01;35:bd=40;33;01:cd=40;33;01:or=01;05;37;41:mi=01;05;37;41:ex=01;32:*.btm=01;32:*.tar=01;31:*.tgz=01;31:*.tbz2=01;31:*.arc=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.lha=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.bz=01;31:*.tz=01;31:*.rpm=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.xbm=01;35:*.xpm=01;35:*.png=01;35:*.tif=01;35:*.tiff=01;35"; export LS_COLORS
LA_FLAGS='-ACFb --color=always'; export LA_FLAGS
LS_FLAGS='-BCFb --color=always'; export LS_FLAGS
LL_FLAGS='-BCFbhl --color=always'; export LL_FLAGS
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
	echo Examining \$CLASSPATH
	echo
	echoList $CLASSPATH $*
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
	type $*
}

wless () {
	$PAGER `which $*` 2>/dev/null || echo ${args} not found
}

# ======== ssh
# ======== Remote sessions
if [ -n "$SSH_TTY" ]; then
CONNECTION_COLOR='[1;33m'; export CONNECTION_COLOR
CONNECTION=ssh; export CONNECTION
elif [ -n "$REMOTEHOST" ]; then
CONNECTION_COLOR='[1;31m'; export CONNECTION_COLOR
CONNECTION=tlnt; export CONNECTION
elif [ -n "$SUDO_USER" ]; then
SUDO_COLOR='[43;30m'; export SUDO_COLOR
CONNECTION=sudo; export CONNECTION
elif [ -n "$inScreen" ]; then
CONNECTION_COLOR='[1;31m'; export CONNECTION_COLOR
CONNECTION=tlnt; export CONNECTION
fi
if [ -n "$CONNECTION" ]; then
CONNECTION_END=' '
fi
# ======== Environment and Shell Variables

export COLUMNS
if [ -n "$CLEARCASE_ROOT" ]; then
CTVIEW=`$HOME/bin/getCTView`; export CTVIEW
if [ -n "$CTVIEW" ]; then
CTVIEW="
[1;35m$CTVIEW[0m"
fi
expr "`oscp -v`" \< "6" > /dev/null && any_alias makedb "makedb -g"
fi
EDITOR=vi; export EDITOR
ENSCRIPT='-M letter'; export ENSCRIPT
EXIT=exit
FCEDIT=vi; export FCEDIT
GREP_COLOR="[1;33"; export GREP_COLOR
GREP_OPTIONS=--color; export GREP_OPTIONS
JAVA_COMPILER=none; export JAVA_COMPILER
LESS='-EXeiw -j 2 -Ps%dt/%D ?f%f::STDIN:$'; export LESS
LIBPATH_NAME=LD_LIBRARY_PATH; export LIBPATH_NAME
exportDefault SCREENDIR $HOME/tmp
if [ -n "$SCRIPT_ARGS" ]; then
SCRIPTING_MSG='[cc] '; export SCRIPTING_MSG
fi
SHELL=sh; export SHELL
THREADS_FLAG=native; export THREADS_FLAG
UNAME=SunOS; export UNAME
if [ -n "$EDITOR" ]; then
VISUAL=$EDITOR; export VISUAL
fi
VERSANT_ROOT=/usr/local/versant/vds605; export VERSANT_ROOT
# ======== Printers
PRINTER=lj1; export PRINTER
LP_DEST=$PRINTER; export LP_DEST
# ======== Cygwin Path
# ======== Function Keys
: '"[17~":kill-whole-line'	# F6 cut
: '"[18~":copy-region-as-kill'	# F7 copy
: '"[19~":yank'	# F8 paste
: '"[20~":history-incremental-search-backward'	# F9 search
if [ -z "$inScreen" ]; then
: -x '"[24~":"exit\n"'	# F12 exit
fi
# ======== Cursor Keys
: '"[1;5D":backward-word'	# ctrl_left
: '"[1;5C":forward-word'	# ctrl_right
# ======== Other Extra Keys
: '"[3~":delete-char'	# delete
: '"[4~":end-of-line'	# end
: '"[1~":beginning-of-line'	# home



# ======== Macro Keys
: -s '""^_"":"\C-apath \C-e\C-m"'	# C-? prepends 'whence' to command line
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
	


;;
*)
	


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
