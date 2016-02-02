# -*- outline-regexp: "^[\t ]*#[\t ]+[^=]\\|^# +\\|^[\t ]*..." -*-

# ****** WARNING: This file is automatically generated. ****** 
# ******          Edits may be overwritten at any time! ****** 

# Built:	Wed Sep 22 22:19:14 PDT 2004
# By:  	pschaaf@copernicus
# For:
#    host     = copernicus   shell    = tcsh      
#    uname    = Linux        term     = xterm     

if ( $?prompt ) then
set interactive=true
endif
alias 'prependToVar' 'setenv \!:1 \!:2${\!:1}'
alias 'appendToVar' 'setenv \!:1 ${\!:1}\!:2'
alias 'echoVar' 'echo -n \$; (env | grep -i "^\!^=") || (set | grep -i "^\!^[^A-Za-z0-9]") || (echo \!^=)'
alias 'member' 'echo - ${\!:2} - | grep " \!:1 " >&/dev/null'
alias 'rmpath' 'echo rmpath not yet implemented for csh'
set -f path=($path)
# set echo
# ======== umask
# This umask gives these access permissions:
# user=rwx, group=rx, others=rx
umask 22


# ======= Debugging Code

# rm -f $HOME/test
# 
# echo '----------''$?_' >> $HOME/test
# if ($?_) then
#   echo $_ >> $HOME/test
# endif
# 
# echo '----------''$?0' >> $HOME/test
# if ($?0) then
#   echo $0 >> $HOME/test
# endif
# 
# echo '----------'tty   >> $HOME/test
# tty   >> $HOME/test
# 
# echo '----------'set   >> $HOME/test
# set   >> $HOME/test
# 
# echo '----------'env   >> $HOME/test
# env   >> $HOME/test
# ======== Fix the Path
# echo a path without double slashes, or empty or relative paths

# ======== Terminal Settings
if ( -e TERM ) then
if ( "$TERM" == "screen" ) then
endif
endif
if ( $?interactive ) then
true  stty eol '^-' werase '^W' eol2 '^-' erase '^?' eof '^D' start '^Q' intr '^C' rprnt '^R' time '0' lnext '^V' stop '^S' quit '^\\' min '1' flush '^O' susp '^Z' kill '^U' 
endif
# ======== Colors
if ( $?interactive ) then
if ( "$TERM" != "emacs" ) then
setenv USE_COLOR true
endif
endif
# ======== Unalias
unalias l la ll ls lsd rm d s p rd 2>/dev/null
# ======== Default Settings
if (! $?USER) setenv USER $LOGNAME
if (! $?LD_LIBRARY_PATH) setenv LD_LIBRARY_PATH /lib
setenv UID $uid
if (! $?RUBYLIB) setenv RUBYLIB $HOME/lib/ruby:$HOME/bin
setenv TECHREF $HOME/doc/TechRef
# ======== Shell Options

set    ignoreeof
unset autologout
set    noclobber
unset  notify


limit coredumpsize 0
set      rmstar
unset    printexitvalue
# ======== Completion, Hashing
if ( $?interactive ) then

set filec
setenv FIGNORE '(.class .o \~ .stackdump dump.txt nohup.out)'
endif
# ======== Zsh Compinstall
# ======== Cd, Dirs, Popd, Pushd
if ( $?interactive ) then
alias 'cd' 'pushd'

set    pushdsilent  pushdtohome
alias 'pushd' 'pushd \!* > /dev/null'
set symlinks ignore
endif
# ======== Shell History
if ( $?interactive ) then
set histdup=all
set history=1000 savehist
set histlit
endif
# ======== Aliases
if ( $?interactive ) then
alias 'any_alias' 'alias \!*'
alias 'basename' 'collect command\ basename'
alias 'bye' 'exit'
alias 'createdb' '$VERSANT_ROOT/bin/createdb'
alias 'dirs' 'dirs -p'
alias 'dirname' 'collect command\ dirname'
alias df >& /dev/null || alias 'df' 'df -h -x supermount'
alias du >& /dev/null || alias 'du' 'du -h'
alias 'ECHO' 'echo'
alias 'EHCO' 'echo'
alias 'ehco' 'echo'
alias 'freshen' 'source ~/'
alias 'igrep' 'grep -i'
alias 'jpegthumb' 'jegscale 128 \!*'

setenv PAGER less
alias 'pstree' 'pstree -G'
alias 'rlib' 'links $@ $TECHREF/languages/ruby/stdlib/index.html'
if ( "$UID" == "0" ) then
alias 'rpm-update' 'urpmi --noclean --update --auto-select'
endif
alias 'rm' 'rm -i'
alias 'tree' 'tree --dirsfirst -ACF --ignore_backups --ignore_cvs'
alias 'which-command' 'path'
alias 'visudo='EDITOR' xemacs visudo'
endif
# ======== Clearcase Aliases
# ======== ls Aliases
setenv LS_BIN ls
alias 'l' '$LS_BIN'
setenv LS_COLORS "no=00:fi=00:di=01;34:ln=01;36:pi=40;33:so=01;35:bd=40;33;01:cd=40;33;01:or=01;05;37;41:mi=01;05;37;41:ex=01;32:*.btm=01;32:*.tar=01;31:*.tgz=01;31:*.tbz2=01;31:*.arc=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.lha=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.bz=01;31:*.tz=01;31:*.rpm=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.xbm=01;35:*.xpm=01;35:*.png=01;35:*.tif=01;35:*.tiff=01;35"
setenv LA_FLAGS '-ACFHb --color=always'
setenv LS_FLAGS '-BCFHb --color=always'
setenv LL_FLAGS '-BCFHbhl --color=always'
alias 'la' 'ls -ACFHb --color=always'
alias 'ls' 'ls -BCFHb --color=always'
alias 'll' 'ls -BCFHbhl --color=always'
alias 'lr' 'ls -R'
alias 'lla' 'la -l'
alias 'llt' 'll -rt'
alias 'lld' 'll  \!* | grep --color=never "/$\\|^$\\|^[^ ][^ ]*$"'
alias 'lltt' 'llt \!* | grep "date +%b.%d"'

# ======== Path Functions
if ( $?interactive ) then
alias 'classpath' 'echo Examining \$CLASSPATH; echo; echoList $CLASSPATH \!*'
alias 'infopath' 'echo Examining \$INFOPATH; echo; echoList $INFOPATH \!*'
alias 'libpath' 'echo Examining \$\$LD_LIBRARY_PATH; echo; echoList $LD_LIBRARY_PATH \!*'
alias 'manpath' 'echo Examining \$MANPATH; echo; echoList ${MANPATH:-`man -w`} \!*'
alias 'path' '; echo; echoList $path \!*'
endif
alias 'appendLibpath' 'eval setenv LD_LIBRARY_PATH "`$HOME/bin/echoMaybeAppendPath.sh 1 $LD_LIBRARY_PATH \!*`"'
alias 'prependLibpath' 'setenv LD_LIBRARY_PATH "`$HOME/bin/echoMaybeAppendPath.sh 0 $LD_LIBRARY_PATH \!*`"'
alias 'appendManpath' 'eval setenv MANPATH "`$HOME/bin/echoMaybeAppendPath.sh 1 $MANPATH \!*`"'
alias 'prependManpath' 'setenv MANPATH "`$HOME/bin/echoMaybeAppendPath.sh 0 $MANPATH \!*`"'
alias 'appendPath' 'eval setenv path "`$HOME/bin/echoMaybeAppendPath.sh 1 $path \!*`"'
alias 'prependPath' 'setenv path "`$HOME/bin/echoMaybeAppendPath.sh 0 $path \!*`"'
alias 'appendClasspath' 'eval setenv CLASSPATH "`$HOME/bin/echoMaybeAppendPath.sh 1 $CLASSPATH \!*`"'
alias 'prependClasspath' 'setenv CLASSPATH "`$HOME/bin/echoMaybeAppendPath.sh 0 $CLASSPATH \!*`"'
if ( $?interactive ) then
alias 'rubylib' 'echo Examining \$RUBYLIB; echo; echoList `ruby -e "print $:.join(':'); %w(\!*).each {|e| print %Q< #{e} #{e}.rb>}"`
 '
endif
alias 'appendRubyLib' 'eval setenv RUBYLIB "`$HOME/bin/echoMaybeAppendPath.sh 1 $RUBYLIB \!*`"'
alias 'prependRubyLib' 'setenv RUBYLIB "`$HOME/bin/echoMaybeAppendPath.sh 0 $RUBYLIB \!*`"'

# ======== Functions
alias 'cdl' 'cd \!*; ls'
alias 'clean_gutenberg' 'ruby -e "File.read(%q-$1-).instance_eval {    gsub!(/\r/,             %q--);    gsub!(/(\S)\n(\S)/,     %q-\1 \2-);    gsub!(/(\n)(\S)/,       %q-\1 \2-);    gsub!(/^\t(chapter )/i, %q-\1-);    puts self;  }"'
alias 'cleanusr' 'su -l cleanusr'
alias 'collect' 'set cmd=$1; shift; echo \!* | xargs -n 1 $cmd'


alias 'echoList' 'echo \!:1 | tr : \\012 | cat -n'
alias 'edit' '(gnuclient -f 'raise-frame' -q \!* 2>/dev/null || xemacs \!*) &'
alias 'frotz' 'pushd ~/local/games/int-fic; command frotz \!* && popd'



alias 'palm' ' ruby -I\"$RUBYLIB\" bin/palm $*'

# Save script args in variable to allow access inside script



alias 'smspaul' 'sendSMS.pl -r 4086444762 -p TMOBILE -s $EMAIL'
alias 'smsvic' 'sendSMS.pl -r 4086444769 -p TMOBILE -s $EMAIL'

alias 'wcat' 'which_meta cat'

alias 'wedit' 'which_meta $PAGER'
alias 'whence' 'alias \!*; sh -c "type \!*"'
alias 'wless' 'which_meta $PAGER'
if ( "$UID" == "0" ) then
alias 'emacs' 'command emacs -r -u pschaaf \!*'
endif
if ( "$UID" == "0" ) then
alias 'xemacs' 'command xemacs -xrm 'Emacs*menubar*Foreground: white' -xrm 'Emacs*menubar*Background: red3' -u pschaaf \!*'
endif
alias 'vicdo' 'eval sudo /bin/su -l vschaaf -- --restricted ${*:+-c "'$*'"}'
# ======== ssh
alias 'vftp' 'ssh -Cqt ftp exec gnu/zsh ${*:--l}'
alias 'intra' 'ssh -Cq intra'
setenv RSYNC_RSH ssh
# ======== Remote sessions
if ( $?CONNECTION ) then
set CONNECTION_END=' '
endif
# ======== Environment and Shell Variables
if (! $?ANT_HOME) setenv ANT_HOME /usr/local/ant
setenv BROWSER mozclient
setenv CLHSROOT file:///usr/local/doc/TechRef/languages/lisp/HyperSpec/
setenv CTVIEW "[1;35m`$HOME/bin/getCTView`[0m"

setenv CVSEDITOR gnuclient
setenv CVS_COPERNICUS :pserver:${USER}@COPERNICUS:${HOME}/cvsroot
setenv CVS_LAPTOP :pserver:${USER}@:/e//MyDocuments\\\cvsroot
setenv CVS_SOURCEFORGE :pserver:anonymous@cvs.sourceforge.net:/cvsroot
setenv CVSIGNORE '.class .o \~ .stackdump dump.txt nohup.out'
setenv CVSROOT $CVS_LAPTOP
setenv EDITOR gnuclient
if ( "$USER" == "pschaaf" ) then
setenv ESHELL `$HOME/bin/pref_shell`
else
setenv ESHELL $SHELL
endif
set EXIT=exit
setenv FCEDIT gnuclient
setenv GREP_COLOR "[1;33"
setenv GREP_OPTIONS --color
setenv JAVA_HOME /usr/java/current
setenv LESS ' --RAW-CONTROL-CHARS --hilite-unread --ignore-case --jump-target=2 --no-init --quit-at-eof --quit-if-one-screen --status-column -Ps%dt/%D ?f%f::STDIN:$'
setenv LIBPATH_NAME LD_LIBRARY_PATH
setenv OFFICE_DRIVE C:
setenv OFFICE_PATH "Program Files//Microsoft Office//Office"
setenv PDA_ROOT :palm
setenv PDA_INSTALL_DIR $PDA_ROOT/Install
setenv PLUCKERHOME $HOME/local/sync/software/man.html
if (! $?SCREENDIR) setenv SCREENDIR $HOME/tmp
if ( $?SCRIPT_ARGS ) then
setenv SCRIPTING_MSG '[cc] '
endif
setenv SHELL tcsh
setenv UNAME Linux
if ( $?EDITOR ) then
setenv VISUAL $EDITOR
endif
setenv VERSANT_ROOT /usr/local/versant/vds605
# ======== Printers
setenv PRINTER lj1100
setenv MOZ_PRINTER_NAME $PRINTER
# ======== Cygwin Path
# ======== Function Keys
bindkey "[17~" kill-whole-line	# F6 cut
bindkey "[18~" copy-region-as-kill	# F7 copy
bindkey "[19~" yank	# F8 paste
bindkey "[20~" history-search-backward	# F9 search
if ( ! $?inScreen ) then
bindkey -x "[24~" "exit\n"	# F12 exit
endif
# ======== Cursor Keys
bindkey "[1;5D" backward-word	# ctrl_left
bindkey "[1;5C" forward-word	# ctrl_right
# ======== Other Extra Keys
bindkey "[3~" delete-char	# delete
bindkey "[4~" end-of-line	# end
bindkey "[1~" beginning-of-line	# home


# ======== Macro Keys
bindkey -s ""^_"" "\C-apath \C-e\C-m"	# C-? prepends 'whence' to command line
# ======== Lesskey
setenv LESSKEY /home/pschaaf/.less_copernicus
# ======== Classpath
appendClasspath $ANT_HOME/lib/ant.jar
# ======== Path
prependPath /bin /usr/bin /usr/local/bin /usr/X11R6/bin
appendPath /sbin /usr/sbin /usr/local/sbin $JAVA_HOME/bin $HOME/bin /opt/cxoffice/bin
if ( $?VERSANT_ROOT ) then
appendPath $HOME/bin/vsnt $VERSANT_ROOT/bin
if (! $?VERSANT_DB) setenv VERSANT_DB `dirname $VERSANT_ROOT`/db
if (! $?VERSANT_DBID) setenv VERSANT_DBID $VERSANT_DB
if (! $?VERSANT_DBID_NODE) setenv VERSANT_DBID_NODE copernicus
endif
appendPath /opt/bin /opt/$USER/firefox /opt/$USER/thunderbird /opt/cxoffice/bin
# ======== Library Path
appendLibpath /lib /usr/lib $HOME/lib
appendRubyLib $HOME/lib/ruby $HOME/bin
# ======== Prompt
if ( $?interactive ) then
setenv HOST_COLOR "[1;32m"
if ( "$UID" == "0" ) then
prependToVar HOST_COLOR "[41m### "
endif
set dirs=
switch ($TERM)
case emacs:
	
setenv PROMPT_HEAD "%T %$HOST_COLOR%m@%m%$NO_COLOR %$PATH_COLOR"
alias 'chpwd' 'set dirs=`dirs`; set dirs=${dirs[1]}; echo "`date +%H:%M` $dirs"'

breaksw
case default:
	
setenv PROMPT_HEAD "%T %$HOST_COLOR%m@%m%$NO_COLOR %$PATH_COLOR"
alias 'chpwd' 'set dirs=`dirs`; set dirs=${dirs[1]}; echo "`date +%H:%M` [1;33m$dirs[0m"'

breaksw
endsw

chpwd
endif
# ======== Cygwin
# ======== Manpath

# Move this heredoc around in the file to temporarily disable sections
# of code. Make sure to keep the opening tag first, and the closing tag
# against the left margin on a line by itself. Don't forget that any
# edits are lost if this file is re-generated.
# : <<DEBUG_NO_EXEC
# DEBUG_NO_EXEC
# 

# Used packages: 
#	ant		column		cvs		cxoffice	emacs
#	gnuclient	grep_color	grep_gnu	info		java_sdk
#	less		ls_color	ls_gnu		msoffice	ruby
#	screen		sms		ssh		suroot		tree
#	versant		vi		vim		x_contrib	xemacs
