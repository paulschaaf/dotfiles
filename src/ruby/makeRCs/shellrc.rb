# $Source: e:/MyDocuments/cvsroot/etc/env/shellrc.rb,v $
# $Revision: 1.352 $
# $Date: 2004/07/14 01:20:28 $
# $Author: pschaaf $
# $State: Exp $
# $Name:  $

# todo: fix command history

# ================================================
# ======= Simple Messaging Service (SMS)

def sms_to(id)
  contact = $contacts[id]
  "sendSMS.pl -r #{contact.cell} -p #{contact.cell_provider} -s $EMAIL"
end


# ================================================
# ======= Misc. Global Settings

$Libpath_Name = {
  any   => :LD_LIBRARY_PATH,
  aix   => :LIBPATH,
  hpux  => :SHLIB_PATH,
}[true]

$browser_file = {
  cygwin => :explorer,
  linux  => :konqueror,
}[true]

$browser_www = {
  any => :'mozclient',
  ftp => nil
}[true]

$browser_light = {
  any    => $browser_www,
  cygwin => :links,
  linux  => :links,
}[true]


# ================================================
# -====== umask

umask 022


# ================================================
# ======= Debugging Code

if csh
  test_file='$HOME/test'
  newline
  comment('======= Debugging Code')
  newline
  comment("rm -f #{test_file}")
  commentedTestCode("'$?_' >> #{test_file}", "if ($?_) then;  echo $_ >> #{test_file};endif")
  commentedTestCode("'$?0' >> #{test_file}", "if ($?0) then;  echo $0 >> #{test_file};endif")
  commentedTestCode("tty   >> #{test_file}")
  commentedTestCode("set   >> #{test_file}")
  commentedTestCode("env   >> #{test_file}")
end


# ================================================
# -====== Fix the Path

comment('echo a path without double slashes, or empty or relative paths')
function(:cleanpath) {
  sh   "echo :$PATH: | sed 's/:::*/:/g; s,///*,/,g; s,:c:/,:/c/,g; s/:[^/][^:]*//g; s/^://; s/:$//'"
}

# setenv(self.path_varname, "/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:/usr/local/sbin:/usr/X11R6/bin:$#{self.path_varname}") if ausable

if path_needs_fixing
  setenv(self.path_varname, '`cleanpath`')
end


# ================================================
# -====== Terminal Settings

shell_if_exists(:TERM) if csh
  shell_if_eql(:TERM, :screen)
    #setenv(:inScreen,  true)
    setenv(:TERM) {
      any    :xterm
      linux  nil
      cygwin :linux
    }
  shell_endif
shell_endif if csh

if_interactive
  stty={
    :eof    => '^D',   :eol    => '^-',   :eol2   => '^-',   :intr   => '^C',
    :kill   => '^U',   :lnext  => '^V',   :min    =>   1,    :quit   => '^\\\\',
    :start  => '^Q',   :stop   => '^S',   :susp   => '^Z',   :time   =>   0,
    :werase => '^W',
    :erase  => {
      any   => '^?',
      cygwin=> '^H',
    }[true],
    :flush  => {
      any   => '^O',
      hpux  =>  nil,
    }[true],
    :rprnt  => {
      any   => '^R',
      hpux  =>  nil,
    }[true],
  }.collect {|pair| pair[1] ? (sprintf "%s '%s' ", *pair) : ''}

any "#{no_op_command} stty #{stty}"

  #zsh 'ttyctl -f # freeze terminal modes. to unfreeze, use ttyctl -u'
shell_endif


# ================================================
# -====== Colors

$HIGHLIGHT_BEGIN = '__'
$HIGHLIGHT_END   = '__'

$NO_COLOR         =  Color.no_color

if self.use_color
  if_interactive
    shell_if_not_eql(:TERM, :emacs)
      setenv(:USE_COLOR, :true)
    shell_endif
  shell_endif
  $HIGHLIGHT_BEGIN = Color.yellow.bold
  $HIGHLIGHT_END   = $NO_COLOR
end

$CTVIEW_COLOR     = Color.magenta.bold
$GREP_COLOR       = $HIGHLIGHT_BEGIN if package?(:grep_color)
$HIST_NUM_COLOR   = Color.cyan.bold
$HOST_COLOR       = eval "Color.#{`shortUname -s #{host}`.chomp.gsub(/ /,'.')}"
$MINOR_PATH_COLOR = Color.reset << Color.yellow
$PATH_COLOR       = Color.yellow.bold
$PS2_COLOR        = Color.magenta.bold
$ROOT_COLOR       = Color.white.bold.on_red
$SSH_COLOR        = Color.yellow.bold
$SUDO_COLOR       = Color.black.on_yellow
$TELNET_COLOR     = Color.red.bold
$TIME_COLOR       = $NO_COLOR
$USERNAME_COLOR   = $HOST_COLOR


# ================================================
# -====== Unalias

unalias %w(l la ll ls lsd rm d s p rd)


# ================================================
# -====== Default Settings

exportDefault(:USER, '$LOGNAME')

if csh
  exportDefault($Libpath_Name, '/lib')
  setenv(:UID) {
    any    '`id | sed \'s/^uid=\([0-9][0-9]*\).*/\1/g\'`' #sunos
    cygwin '`id -u`'
    tcsh   '$uid'
  }
end

exportDefault(:RUBYLIB, '$HOME/lib/ruby:$HOME/bin') if local_ruby

setenv(:TECHREF, '$HOME/doc/TechRef') if owned


# ================================================
# -====== Shell Options

opt_comment('Do not HUP jobs on exit'); any {
  bash  'shopt -u huponexit' unless sunos5_6
  zsh   'setopt no_hup'
}

opt_comment('Do not accidentally exit the shell'); any {
  csh   'set    ignoreeof' + $/ + 'unset autologout'
  ksh   'set -o ignoreeof'
  zsh   'setopt ignore_eof'
}

opt_comment('Do not overwrite existing files'); any {
  csh   'set    noclobber'
  ksh   'set -o noclobber'
  zsh   'setopt no_clobber'
}

opt_comment('Do not use flow control'); any {
  zsh   'unset flow_control'
}

opt_comment('Do not notify me until jobs complete'); any {
  csh   'unset  notify'
  ksh   'set -o notify off'
  zsh   'setopt nonotify'
}

opt_comment('I want silence\!'); any {
  ksh   'set    bell-style none'
  zsh   'setopt no_beep'
}

opt_comment('Use emacs key bindings'); any {
  ksh   'set -o  emacs'
  zsh   'bindkey -e'
}

opt_comment('Do not bother to make core files'); any {
  any    'limit coredumpsize 0'
  sh     'ulimit -c 0'
  zsh    'limit coredumpsize 0'
  cygwin  nil
}

opt_comment('Prompt before executing \'rm *\''); any {
  csh   'set      rmstar'
  zsh   'unsetopt rm_star_silent'
}

opt_comment('Do not display any nonzero exit value'); any {
  csh   'unset    printexitvalue'
  zsh   'unsetopt print_exit_value'
}


# ================================================
# -====== Completion, Hashing

if_interactive
  opt_comment('Hash each time a command is used'); any {
    ksh  'set -o track_all' unless bash or zsh
    bash 'set -o hashall; shopt -s checkhash' unless sunos5_6
    zsh  'setopt hash_cmds hash_dirs'
  }

  opt_comment('Use filename completion')
    csh  'set filec'
    bash 'shopt -s dotglob extglob' unless sunos5_6
    zsh  'setopt auto_list auto_menu auto_param_slash auto_remove_slash'

  opt_comment('Ignore case in completion')
    bash 'set completion-ignore-case on; shopt -s nocaseglob' unless sunos5_6

  opt_comment('Ignore these files for completion purposes')
    ignore_globs=%w(.class .o \~ .stackdump dump.txt nohup.out)
    setenv(:FIGNORE) {
      sh  ignore_globs.join(':')
      csh "'(" + ignore_globs.join(' ') + ")'"
    }

  opt_comment('Correct spelling')
    bash 'shopt -s cdspell' unless sunos5_6
    zsh  'setopt   correct; alias mv=\'nocorrect mv\'; alias cp=\'nocorrect cp\''
shell_endif


# ================================================
# -====== Zsh Compinstall

if zsh
  if_interactive
    if linux or cygwin
      zsh <<ZSH_COMPINSTALL
###
# Don't edit below this line.
# The following lines were added by compinstall

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
zstyle  :compinstall filename "$HOME/etc/env/#{host}.zsh"

autoload -U compinit
compinit#{' -u' if cygwin}
# End of lines added by compinstall
ZSH_COMPINSTALL
    end

    autoload :zed
    shell_alias(:fned, 'zed -f')

    unalias  'run-help'
    autoload 'run-help'
    setenv(:HELPDIR, '$HOME/local/sync/zsh_help')
  shell_endif
end


# ================================================
# -====== Cd, Dirs, Popd, Pushd

if_interactive
  opt_comment('cd is the same as pushd')
    zsh   'setopt auto_pushd'
    function(:cd) {
      csh 'pushd'
      sh  "builtin cd #{args}; pushd -n $OLDPWD > /dev/null"
      zsh  nil
    }

  function(:popd) {
    #csh "popd #{args} && chpwd"
    sh  'builtin popd #{args} && chpwd' unless zsh
  }

  opt_comment('pushd is quiet, and with no args goes \$HOME')
#    function(:pushd, "#{'builtin ' if bash or zsh}pushd #{args} && chpwd")
    csh  'set    pushdsilent  pushdtohome'
    zsh  'setopt pushd_silent pushd_to_home'

  function(:pushd) {
    sh  "# if target is a file, change to its location
         if [ -f $1 ]; then
           #{'builtin ' if bash or zsh}pushd #{zsh ? '$1:h' : '$(dirname $1)'} > /dev/null
         else
           #{'builtin ' if bash or zsh}pushd $1 > /dev/null
         fi
         #{'chpwd' unless zsh}"
    csh "pushd #{args} > /dev/null" # && chpwd"
  }

  opt_comment('Treat directory symlinks as if they were directories')
    csh  'set symlinks ignore'
shell_endif


# ================================================
# -====== Shell History

if_interactive
  opt_comment('Do not put into history list lines that begin with a space or that duplicate the previous line')
    csh   'set histdup=all'
    bash  'set HISTCONTROL=ignoreboth hist_ignore_dups'
    zsh   'setopt hist_ignore_dups hist_ignore_space hist_reduce_blanks'
    zsh   'setopt hist_find_no_dups' unless sunos or generic

  opt_comment('Set history size')
    csh   'set history=1000 savehist'
    setenv(:HISTSIZE, 1000) if bash or zsh

  opt_comment('Add timestamp and duration to history file')
    zsh   'setopt extended_history'

  opt_comment('Use csh-style \! history')
    bash  'set -o histexpand'
    zsh   'setopt bang_hist'

  opt_comment('Use literal values in history')
    csh   'set histlit'

  opt_comment('Append history')
    bash  'shopt -s histappend'  unless sunos5_6
    zsh   'setopt inc_append_history' unless ftp

  opt_comment('No functions in history')
    zsh   'setopt hist_no_functions' unless ftp

  setenv(:HISTFILE, '$HOME/.bash_history') if bash

  bash  'shopt -s histreedit'  unless sunos5_6

  if zsh
    opt_comment('Set \$0 to the name of the shell function or currently sourcing script')
    zsh 'setopt function_argzero'
  end
shell_endif


# ================================================
# -====== Aliases

if_interactive
  function(:any_alias) {
    sh  "local label=$1; shift; alias $label=\"#{args}\""
    csh "alias #{args}"
  }

  shell_alias(:basename, 'collect command\ basename')

  shell_alias(:bye, :exit) unless zsh

  if cygwin
    shell_alias(:call, '')
    #function(:createdb, 'cmd /C createdb #{args}')
  end

  shell_alias(:createdb, '$VERSANT_ROOT/bin/createdb') if package?(:versant)

  function(:dirs) {
    if linux or cygwin
      sh  "builtin dirs ${*:--p}"
      csh 'dirs -p'
    else
      zsh "builtin dirs ${*:--p}"
    end
  }

  shell_alias(:dirname, 'collect command\ dirname')

  shell_alias_default(:df) {
    any 'df -h'
    any 'df -h -x supermount' if linux or cygwin
  }

  shell_alias_default(:du, 'du -h')

  %w(ECHO EHCO ehco).each {|e| shell_alias(e, :echo)}

  shell_alias(:file, '/usr/ucb/file') if sunos

  shell_alias(:freshen, 'source ~/')

  shell_alias(:igrep, 'grep -i')

  shell_alias(:irb, %q!irb -r 'util/yaml_edit.rb'!) if package?(:local_ruby)

  shell_alias(:jpegthumb, "jegscale 128 #{args}") if linux or cygwin

  shell_alias(:less, $pager_bin) if package?(:less)

  shell_alias(:man, 'man -F') if sunos

  setenv(:PAGER, $pager_bin || package?(:less) && :less)

  shell_alias(:pstree, 'pstree -G') if linux or cygwin

  if local_ruby
    function(:rlib, "#$browser_light $@ $TECHREF/languages/ruby/stdlib/index.html")
  end

  if linux
    shell_if_root
      shell_alias('rpm-update', 'urpmi --noclean --update --auto-select')
    shell_endif
  end

  shell_alias(:rm, 'rm -i')

  shell_alias(:tree) {
    any   '"tree --dirsfirst -ACFI \'CVS*\'"'
    cygwin 'tree --dirsfirst -ACF --ignore_backups --ignore_cvs'
    linux  'tree --dirsfirst -ACF --ignore_backups --ignore_cvs'
  } if package?(:tree)

 # shell_alias(:vantive, 'ssh -Xf vsnt cd /net/muave/vol0/vantive/xclient7.0.6.7\; exec /bin/csh -f ./runvantive')
  shell_alias('which-command', 'path')

#   if $Editor == :gnuclient || $Editor == :winclient
#     shell_alias(:visudo, 'EDITOR=xemacs visudo')
#   end

  shell_alias(:whoami, %q<who am i | awk "{print \$1}">) if sunos or generic

shell_endif


# ================================================
# -====== ls Aliases

#if_interactive
  setenv(:LS_BIN, $ls_bin)

  shell_alias(:l, '$LS_BIN') # use the unadorned primitive command

  $ls = Command.new($ls_bin, :F)

  if package?(:ls_color)
    $ls.add_flags('color=always')
    setenv(:LS_COLORS, '"' + %w(
      no=00
      fi=00

      di=01;34

      ln=01;36

      pi=40;33

      so=01;35

      bd=40;33;01
      cd=40;33;01

      or=01;05;37;41
      mi=01;05;37;41

      ex=01;32
      *.btm=01;32

      *.tar=01;31
      *.tgz=01;31
      *.tbz2=01;31
      *.arc=01;31
      *.arj=01;31
      *.taz=01;31
      *.lzh=01;31
      *.lha=01;31
      *.zip=01;31
      *.z=01;31
      *.Z=01;31
      *.gz=01;31
      *.bz2=01;31
      *.bz=01;31
      *.tz=01;31
      *.rpm=01;31

      *.jpg=01;35
      *.jpeg=01;35
      *.gif=01;35
      *.bmp=01;35
      *.xbm=01;35
      *.xpm=01;35
      *.png=01;35
      *.tif=01;35
      *.tiff=01;35
    ).join(':') + '"')
    setenv(:LS_COLORS, '"$LS_COLORS:*.bat=01;32:*.cmd=01;32:*.com=01;32:*.exe=01;32"') if cygwin
  end

  if package?(:ls_gnu)
    $ls.add_flags(:B, :b, :C)
    $ls.add_flags(:H) unless cygwin || sunos5_8
  end

  # or just 'a' for SunOS?
  $la = $ls.with_flags(:A).without_flags(:B)

  $ll = $ls.with_flags(:l)
  $ll.add_flags(:h) if package?(:ls_gnu)

  setenv(:LA_FLAGS, "'#{$la.flags}'")
  setenv(:LS_FLAGS, "'#{$ls.flags}'")
  setenv(:LL_FLAGS, "'#{$ll.flags}'")

  shell_alias(:la,  %Q<#$ls_bin #{$la.flags}>)
  shell_alias(:ls,  %Q<#$ls_bin #{$ls.flags}>)
  shell_alias(:ll,  %Q<#$ls_bin #{$ll.flags}>)

  shell_alias(:lr,  'ls -R')

  shell_alias(:lla, 'la -l')
  shell_alias(:llt, 'll -rt')

  function(   :lld, "ll  #{args} | grep #{'--color=never' if $GREP_COLOR} \"/$\\\\|^$\\\\|^[^ ][^ ]*$\"")
  function(   :lltt,"llt #{args} | grep \"date +%b.%d\"")
  if sh
    lsd = <<-LSD
        for x in ${*:-.}
        do
            pushd $x
            %s
	    popd
        done
    LSD
    lsd_cmd = $ls.without_flags(:F).with_flags!(:d).with_args!('*/')
    function(:lsd,  sprintf(lsd, lsd_cmd))
    function(:lsda, sprintf(lsd, lsd_cmd.with_flags(:a).with_args!('.*/')))
    shell_alias(:lsad, :lsda)
  end

  function(:lsf) {
    sh "local x
        for x in ${*:-*}
        do
          echo `file -N -F: -- $x`:`ls -d -- $x` | sed 's/^[^:]*: //'
        done | column -t -s ':'"
  }

#shell_endif


# ================================================
# -====== Path Functions

if_interactive
  if cygwin
    echoListFunction(:classpath, '$(cygpath -aup $CLASSPATH)')
  else
    echoListFunction(:classpath)
  end

  echoListFunction(:fpath)    if zsh
  echoListFunction(:infopath) if package?(:info)
  echoListFunction(:libpath,  "$#$Libpath_Name")
  echoListFunction(:manpath)
  if cygwin
    echoListFunction(:path, '{PATH//:\/bin:/:/bin [-> /usr/bin]:}')
  else
    echoListFunction(:path)
  end

shell_endif

appendAndPrependListFunctions(
   :Classpath => :CLASSPATH,
   :Libpath   => $Libpath_Name,
   :Manpath   => :MANPATH,
   :Path      => self.path_varname
)

if package?(:ruby)
  if_interactive
    echoListFunction(:rubylib)
  shell_endif
  appendAndPrependListFunctions(:RubyLib => :RUBYLIB)
else
  ignore_appendAndPrependListFunctions(:RubyLib => :RUBYLIB)
end

function(:updateManpath) {
#   sh %q!  local x
#   for x in `echo $PATH | tr ':' '\012'`; do
#     appendManpath `echo $x | sed 's,/bin$,/man,'`
#   done!
  sh %q!appendManpath `echo $PATH | awk '/\/bin$/ {gsub(/\/bin$/, "/man"); print}' RS=':'`!
#   sh %q!appendManpath `echo $PATH | awk '{
#         if (match($0, /\/s?bin\/?/)) {
#                 dir=substr($0,0,RSTART)
#                 if (uniq[dir])
#                         next
#                 uniq[dir]=1
#                 print dir
#         }}' RS=':' ORS='man '`! if sunos
}


# ================================================
# -====== Functions

function(:cdl,   "cd #{args}; ls")

function(:clean_gutenberg) {
  code = 'ruby -e "File.read(%q-$1-).instance_eval {
    gsub!(/\r/,             %q--);
    gsub!(/(\S)\n(\S)/,     %q-\1 \2-);
    gsub!(/(\n)(\S)/,       %q-\1 \2-);
    gsub!(/^\t(chapter )/i, %q-\1-);
    puts self;
  }"'

  any   nil
  ruby  code
  csh   code.gsub(/[\r\n\t]+/, '') if ruby
  ftp   nil
}

function(:cleanusr, 'su -l cleanusr') if owned

function(:collect) {
  any "set cmd=$1; shift; echo #{args} | xargs -n 1 $cmd"
  sh  "local cmd x\ncmd=$1; shift\nfor x\ndo\neval $cmd $x\ndone"
}

# function(:cppda) {
#   sh 'local src=$1
#       local name=${2:-`basename ${src%.*}`}
#       local dest=${3:-`basename ${src%.*}`.pdb}
#       txt2pdbdoc $name $src $dest
#       scp $dest $PDA_INSTALL_DIR'
# }

function(:ctset) {
  sh 'source ~/bin/ctset'
} if package?(:versant)

if sh && package?(:cvs)
  cvs_do = [%Q<
    echo
    TRAPEXIT () {
      popd
    }
    pushd $HOME
    [ -x bin/color-cvs-output.rb ] || alias color-cvs-output='cat'
    for x in `find $HOME -type d ! -regex ".*/\\(courses\\|sections\\|cvsroot\\|etc\\).*" -name CVS -maxdepth 2 -printf '%h '` $HOME/etc
    do
      echo "#{Color.yellow.bold}"\\=============== $x"#{Color.no_color}"
      pushd $x
      cvs -q >, ' | color-cvs-output.rb
      popd
    done']

  function(:'cvs-commit', cvs_do.join('commit        $*;       make 2>&1'))
  function(:'cvs-diff',   cvs_do.join('diff   -R     $*; echo           '))
  function(:'cvs-update', cvs_do.join('update -d -kb $*; echo; make 2>&1'))
end

function(:ddu) {
  sh  'find ${*:-.} -type d -maxdepth 1 -exec du -hs {} \\; | sort -n'
}


# fileToFunction () {
#   local defn x
#   for x
#   do
#     defn="`basename $x` () {
# (`sed '/^$/d; /^#/d' $x`) &
# }"
#     echo $defn
#     eval "$defn"
#   done
# }


function(:echoList) {
#     local list=$1
#     shift
#     echo $list | awk '
#     NR == 1 {split(file, files, " ")}

#     function output(string) {outstr = outstr string}

#     { output(sprintf(" %2d ", NR))

#       for (idx in files) {
#         fullname = " " $1 "/" files[idx]
#         if (! system("test -f" fullname)) {
#           output("*")
#           matches = matches fullname
#           break
#         }
#       }

#       output($0 "\n")
#     }
#     END {
#       gsub(ENVIRON["HOME"],"~",outstr)
#       system("echo \"" outstr "\"|column")
#       if (matches) {
#         print "\n" cmd matches "\n"
#         system("for x in" matches "\ndo\n" cmd " $x\ndone")
#       }
#     }' RS=: file="$@" cmd="ls $LL_FLAGS" | sed 's/ \*\([^ 	]*\)/ [1;33m\1[0m/g'

  if csh
    any 'echo \!:1 | tr : \\\\012 | cat -n'
  else
    src = <<-'EOF'
   local list=$1
    shift
    echo $list | awk '
    NR == 1 {split(file, files, " ")}

    function output(string) {outstr = outstr string}

    {
      output(sprintf(" %%2d ", NR))

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
      gsub(ENVIRON["HOME"],"~",outstr)
      %s
      if (matches) {
        print "\n" cmd matches "\n"
        system("for x in" matches "\ndo\n" cmd " $x\ndone")
      }
    }' RS=: file="$@"
    EOF
    src.chomp!

    code = []
    code << sprintf(src,
                   if package?(:column)
                     'system("echo \"" outstr "\"|column")'
                   else
                     'printf("%s", outstr)'
                   end)

    code << sprintf('cmd="%s $LL_FLAGS"', $ls_bin)

    src = '| sed \'s/ \*\([^ 	]*\)/%s \1%s/g\''
    code << sprintf(src, Color.yellow.bold, $NO_COLOR)

    any code.join(' ')
  end
}

function(:frotz, "pushd ~/local/games/int-fic; command frotz #{args} && popd") if home_desktop or laptop

# function(:grep, "if [ \"$1\" = \"--color=never\" ]; then shift; fi; #$grep_bin #{args}") unless package?(:grep_color)

function(:jpegscale) {
  sh "boundary=$1; shift
  for file in #{args}
  do
    jpegtopnm $file | pnmscale -xysize $boundary $boundary | pnmtojpeg >| ${file/./_$boundary.}
  done"
  ftp nil
}

function(:lite) {
  remote_ruby "ruby bin/usr-local-bin/lite #{args}"
  sh   %Q{
         if [ -z "#{args}" ]; then
           cat
         elif [ "$1" = "--color=never" ]; then
           shift
           cat #{args}
         else
           local pattern=$1; shift
           sed "s,$pattern,#$HIGHLIGHT_BEGIN${pattern}#$HIGHLIGHT_END,g"
         fi}
  any  "egrep -a -C 99999 #{sh ? "${*:-''}" : args}" if package?(:grep_gnu)
  local_ruby   nil
}

function(:llocate,
         "if [ -e locatedb ]
          then
            command locate -d locatedb #{args}
          else
            command locate #{args}
          fi")

function(:makealias) {
  csh   %q<"quote | /bin/sed 's/^/alias \!:1 /' \!:2*">
  linux nil
}

# function(:manz) {
#   targets='zsh'
#   zsh "MANPATH=`man --path #{targets} | sed 's~/man[0-9]/.*~~g'#{" | sort -u | tr '\012' ':' | sed 's/:$//g'" if targets.size > 1}` man -K #{args}"
# }

#function(:messageline) {
#sh   %Q<echo "\\e^#{args}\\e\\">
#csh  %Q<echo "\\e[0000m\\e^#{args}\\e\\">
#}

function(:palm) {
  ruby   resolve_host_aliases(:laptop)[0].to_s + %q< ruby -I\"$RUBYLIB\" bin/palm $*>
  laptop nil
  ftp    nil
}

function(:quote) {
  csh   %q<"/bin/sed -e 's/\\!/\\\\\!/g' -e 's/'\\\''/'\\\'\\\\\\\'\\\''/g' -e 's/^/'\''/' -e 's/"\$"/'\''/'">
  linux nil
}

function(:rcd, 'cd $*; popd +1')

function(:readlink, "file #{args[1]} | grep ymbolic.link | sed s/^.\*ymbolic.link.to.//") if sunos

comment '# Save script args in variable to allow access inside script'
function(:script) {
  sh %Q!SCRIPT_ARGS="script $*" command script $*!
}

function(:setfont) {
  sh %Q<local name
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
        echo "\e]50;$name">
  ftp nil
}

function(:signals) {
  sh %Q<echo "   \\$signals="; echo $signals | tr " " "\012" | cat -n | awk \' {print "    ", $1-1, $2}\' #{' | column' if linux or cygwin}>
}

if package?(:sms)
  function(:smspaul, sms_to(:pschaaf) << ' $*')
  function(:smsvic,  sms_to(:vschaaf) << ' $*')
end

# function(:startx, '/d/Progra~1/Exceed.nt/exceed') if cygwin

function(:svc) {
  sh 'grep "\<${*// /\\|}\>" /etc/services'
}

function(:syncdir) {
  sh 'make ${1:+-C $1} -ef ~/etc/makefiles/syncdir.make'
}

function(:wcat) {
  csh 'which_meta cat'
  sh  "cat $(which #{args})"
}

function(:wcd) {
  sh  'cd $(which $1)'
}

function(:wedit) {
  csh 'which_meta $PAGER'
  sh  "local cmd file x
         for x in #{args}
         do
           file=$(which $x) && cmd=\"${cmd:-edit} $file\" || echo $file
         done
       eval $cmd"
}

function(:whence) {
  sh   "type #{args}"
  bash "type -a #{args}"
  csh  "alias #{args}; sh -c \"type #{args}\""
  zsh  "builtin whence -ac #{args}"
}

if cygwin
  function(:winclient, 'eval command winclient `winlongpath $*`')

  winpath = lambda {|is_short|
    "(for x in $*; do
       echo \"'`cygpath -am#{'s' if is_short} $x`' \"
     done) | sed 's-/-\\\\-g'
     echo"
  }

  function(:winlongpath,  winpath.call(false))
  function(:winshortpath, winpath.call(true))
end

function(:wless) {
  csh 'which_meta $PAGER'
  #sh  "$PAGER `which #{args}` 2>/dev/null || echo #{args} not found"
  sh  "local arg answer
        for arg
        do
                answer=`whence $arg | sed '
1 {
        /: aliased to \| not found$\|: shell built-in /q
        / () {$/! {d; q}
}'`
                if [ -z $answer ]
                then
                        whence $arg | xargs $PAGER
                else
                        echo $answer | $PAGER
                fi
        done"
}

if package?(:emacs)
  shell_if_root
    function(:emacs, "command emacs -r -u pschaaf #{args}")
  shell_endif
end

if package?(:xemacs)
  shell_if_root
    function(:xemacs, "command xemacs -xrm 'Emacs*menubar*Foreground: white' -xrm 'Emacs*menubar*Background: red3' -u pschaaf #{args}")
  shell_endif
end

function(:vicdo, 'eval sudo /bin/su -l vschaaf -- --restricted ${*:+-c "\'$*\'"}') if copernicus or ramsesii


# ================================================
# -====== ssh

if ssh_server and not ftp
#   sshToFunction(:gwmail, '-t', 'exec /usr/bin/zsh ${*:--l}') unless gwmail
#   sshToFunction(:home,   '-C', '${*}')                       unless :home_desktop
#   sshToFunction(:vftp,   '-Ct', 'exec gnu/zsh      ${*:--l}')

#   $remote_ssh_servers.each {|e|
#     args = ['-C'] #, '-t', $ssh_gateway, 'ssh']
#     args << '-q' if (linux || cygwin) && (uname_of(e) !~ /(linux|cygwin).*/i)
      
#     sshToFunction(e, *args)
#   }

  setenv(:RSYNC_RSH, :ssh)
end


# ================================================
# -====== Remote sessions

if sh
  if_ssh_session
    setenv(:CONNECTION_COLOR, "'#$SSH_COLOR'")
    setenv(:CONNECTION, :ssh)
  elsif_defined(:REMOTEHOST)
    setenv(:CONNECTION_COLOR, "'#$TELNET_COLOR'")
    setenv(:CONNECTION, :tlnt)
  elsif_defined(:SUDO_USER)
    setenv(:SUDO_COLOR, "'#$SUDO_COLOR'")
    setenv(:CONNECTION, :sudo)
  elsif_defined((linux or cygwin) ? :REMOTEUSER : :inScreen)
    setenv(:CONNECTION_COLOR, "'#$TELNET_COLOR'")
    setenv(:CONNECTION, :tlnt)
  shell_endif
end
if_defined(:CONNECTION)
  set(:CONNECTION_END, "' '")
shell_endif


# ================================================
# -====== Environment and Shell Variables

exportDefault(:ANT_HOME, '/usr/local/ant') if package?(:ant)

setenv(:BROWSER, $browser_www || $browser_local)

setenv(:CLHSROOT, 'file:///usr/local/doc/TechRef/languages/lisp/HyperSpec/') if linux or cygwin

sh 'export COLUMNS'

if csh
  setenv(:CTVIEW, "\"#$CTVIEW_COLOR`$HOME/bin/getCTView`#$NO_COLOR\"")
elsif sh
  if_defined(:CLEARCASE_ROOT)
    setenv(:CTVIEW, '`$HOME/bin/getCTView`')
    if_defined(:CTVIEW)
       set(:CTVIEW, "\"\n#$CTVIEW_COLOR$CTVIEW#$NO_COLOR\"")
    shell_endif
    any('expr "`oscp -v`" \< "6" > /dev/null && any_alias makedb "makedb -g"')
  shell_endif
end

if package?(:cvs)
  function(:cvsls) {
    zsh "local files=( $( cvs status -l \"#{args}\" 2>/dev/null | awk \"/File: / { print \$2 }\" ) )
    ls -d \"#{args}\" *(/) \"$files[@]\""
  }

  setenv(:CVSEDITOR, $Editor)

  setenv(:CVS_COPERNICUS,  ':pserver:${USER}@COPERNICUS:${HOME}/cvsroot')
  setenv(:CVS_LAPTOP,      ":pserver:${USER}@#{resolve_host_aliases(:laptop)[0].to_s.to_s}:/e//MyDocuments\\\\\\cvsroot")
  #setenv(:CVS_SATORI,      ':pserver:${USER}@satori:/home/${USER}/cvsroot')
  setenv(:CVS_SOURCEFORGE, ':pserver:anonymous@cvs.sourceforge.net:/cvsroot')
  setenv(:CVSIGNORE,       "'" + ignore_globs.join(' ') + "'")
  setenv(:CVSROOT,         '$CVS_LAPTOP')
end

if_not_defined(:EDITOR)
  setenv(:EDITOR, $Editor)
shell_endif

# sh ": ${EDITOR=#$Editor}"
# setenv(:EDITOR)

if sh and ! ftp
  any "case $USER\nin"
  $contacts.each_value {|contact|
    if contact.login and contact.email
      any "  #{contact.login})"
      any "    export EMAIL=#{contact.email}"
      any '    ;;'
    end
  }
  any 'esac'
end

if package?(:emacs)
  shell_if_eql(:USER, :pschaaf)
    setenv(:ESHELL, "`$HOME/bin/pref_shell 2>/dev/null || echo $SHELL`")
  shell_else
    setenv(:ESHELL, '$SHELL')
  shell_endif
end

setenv(:ENSCRIPT, "'-M letter'") unless owned and (linux or cygwin)

set(:EXIT, 'exit')

setenv(:FCEDIT) {
  any $Editor
  zsh :zed
}

if $GREP_COLOR
  setenv(:GREP_COLOR, '"' + $GREP_COLOR.chop + '"')
  setenv(:GREP_OPTIONS, '--color')
end

setenv(:ignoreeof, 3) if bash

setenv(:JAVA_COMPILER, :none) if sunos

if package?(:java_sdk)
  setenv(:JAVA_HOME, '/usr/java/current') if linux
end

if package?(:less)
  less = Command.new
  [ [:w,     'hilite-unread'     ],
    [:i,     'ignore-case'       ],
    ['-j 2', 'jump-target=5'     ],
    [:J,     'status-column'     ],
    [:e,     'quit-at-eof'       ],
    [:E,     'quit-if-one-screen'],
    [:X,     'no-init'           ],
    [:R,     'RAW-CONTROL-CHARS' ],
  ].each {|e|
    flag = e[sunos ? 0 : -1]
    less.add_flags(flag)
  }

  if sunos
    less.without_flags!(:J)
    less.add_flags(:r) unless less.delete_flags(:R).empty?
  end

  setenv(:LESS, "'#{less} -Ps%dt/%D\ ?f%f\::STDIN\:$'")
end

setenv(:LIBPATH_NAME, $Libpath_Name)

setenv(:MAKE_MODE, :unix) if cygwin

if package?(:msoffice)
  setenv(:OFFICE_DRIVE) {
    any    'C:'
    cygwin '/d'
    congo  '/c'
  }
  setenv(:OFFICE_PATH) {
    any '"//Program Files//Microsoft Office//Office"' # clients should assume this
    cygwin '/PROGRA~1/MICROS~2/OFFICE'
  }
end

if ssh_server and not ftp
  setenv(:PDA_ROOT, resolve_host_aliases(:laptop)[0].to_s + ':palm')
  setenv(:PDA_INSTALL_DIR, '$PDA_ROOT/Install')
end

setenv(:PLUCKERHOME, "$HOME/local/sync/software/man.html") if owned

exportDefault(:SCREENDIR, "$HOME/tmp")

if_defined(:SCRIPT_ARGS)
  setenv(:SCRIPTING_MSG, "'[cc] '")
shell_endif

setenv(:SHELL, shell)

setenv(:THREADS_FLAG, :native) if sunos

setenv(:UNAME, uname)

if_defined(:EDITOR)
  setenv(:VISUAL, '$EDITOR')
shell_endif

if bash or zsh
  punctuation = ((33..126).to_a \
                 - (?A..?Z).to_a \
                 - (?a..?z).to_a \
                 - (?0..?9).to_a
                 ).collect {|e| e.chr}

  comment('Any punctuation char in WORDCHARS is considered a part of the adjacent word.')
  wordchars = %w(* _ - ~ \\ $ % ^)
  setenv(:WORDCHARS, "'#{wordchars}'")

  comment('The remaining punctuation chars are considered separate words, regardless of what may be adjacent:', "\t#{punctuation - wordchars}")
end

setenv(:VERSANT_ROOT) {
  any    '/usr/local/versant/vds605'
  cygwin nil
}


# ================================================
# -====== Printers

setenv(:PRINTER) {
  any          '//gwiredc1/hplj2100'
  home_desktop :lj1100
}

if sunos
  setenv(:LP_DEST, '$PRINTER')
elsif linux or cygwin
  setenv(:MOZ_PRINTER_NAME, '$PRINTER')
end


# ================================================
# -====== Cygwin Path

if cygwin
  cygwin <<EOF
   #if_defined('__PWD')
     # Perhaps win could not successfully change to this dir,
     # e.g. it is a UNC path. We must get there ourselves.
     #eval cd $(ruby -e "puts Regexp.escape(ENV['__PWD'].gsub('-', '__MINUS__')).gsub('__MINUS__', '-')")
   #shell_endif
   #if there are spaces in pwd, convert to short (mangled) path
   if [ `expr $PWD : '.* '` -gt 0 ]; then
     cd $(cygpath -u $(cygpath -asw "$PWD"))
#| sed 's~^/cygdrive~~')
   fi
   unset __PWD

   appendPath /bin
EOF
end


# ================================================
# ======= Shell Key Bindings

# -====== Function Keys
bindkey('kill-whole-line',     'F6 cut',        Keymap[:F6])
bindkey('copy-region-as-kill', 'F7 copy',       Keymap[:F7])
bindkey('yank',                'F8 paste',      Keymap[:F8])
if csh
  bindkey('history-search-backward', 'F9 search', Keymap[:F9])
else
  bindkey('history-incremental-search-backward', 'F9 search', Keymap[:F9])
end

if_not_inScreen
  bindkey('"exit\\n"',         'F12 exit',     "-#{zsh ? 's' : 'x'} #{Keymap[:F12]}")
shell_endif

# -====== Cursor Keys
bindkey('backward-word',       :ctrl_left,      Keymap[:ctrl_left])
bindkey('forward-word',        :ctrl_right,     Keymap[:ctrl_right])

# -====== Other Extra Keys
bindkey('delete-char',         :delete,         Keymap[:delete])
bindkey('end-of-line',         :end,            Keymap[:end])
bindkey('beginning-of-line',   :home,           Keymap[:home])

bindkey('kill-word',           :ctrl_delete,    Keymap[:ctrl_delete])
bindkey('undo',                :alt_backspace,  Keymap[:alt_backspace]) unless csh
bindkey('backward-kill-word',  :ctrl_backspace, Keymap[:ctrl_backspace])

# -====== Macro Keys
=begin
  # found in /usr/share/zsh/4.1.0-dev-5/zshrc_default
  bindkey -s "^xs" '\C-e"\C-asu -c "' # go to end, put '"', go to beginning, insert 'su -c "'
  bindkey -s "^xd" "$(date '+-%d_%b')"
  bindkey -s "^xf" "$(date '+-%D'|sed 's|/||g')"
  bindkey -s "^xw" "\C-a \$(which \C-e\)\C-a"
=end
  #bindkey('"\C-e | $PAGER"', 'append \'| \$PAGER\' to command line', '-s "^xl")

bindkey('"\C-apath \C-e\C-m"', 'C-? prepends \'whence\' to command line', "-#{bash ? 'x' : 's'} ^_") unless sunos5_6

if zsh and not cygwin
  # -====== Emacs Compatibility
  if_not_defined(:CONNECTION)
    bindkey('', 'remove binding', '-r \C-h')
    bindkey('describe-key-briefly', '', '\C-h\C-k')
  shell_endif
end


# ================================================
# -====== Lesskey

if package?(:less)
  less_keys={
    :home       =>  'goto-line',
    :end        =>  'goto-end',
    :exit       =>  'quit',
    :page_up    =>  'back-screen',
    :page_down  =>  'forw-screen',
    :up         =>  'back-line',
    :down       =>  'forw-line',
    :right      =>  'right-scroll',
    :left       =>  'left-scroll',
  }.collect {|pair| "#{Keymap[pair[0]]}   #{pair[1]}"}.join($/)

  unless less_keys.empty?
    $LESSKEY="#{ENV['HOME']}/.less_#@host"
    setenv(:LESSKEY, $LESSKEY) unless ftp

    # note that this following is executed at compile-time
    if sh and ! (bash or ksh or zsh)
      File.empty_then_open_safely($LESSKEY, 'w') {|file|
        file.chmod(0644)
        STDERR.puts "\# === Making #$LESSKEY"
        `echo '#{less_keys}' | lesskey -o #{file.path} -`
      }
    end
  end
end


# ================================================
# -====== Classpath

appendClasspath "$ANT_HOME/lib/ant.jar" if package?(:ant)


# ================================================
# -====== Path

$gnu_tool_path << '/usr/X11R6/bin' if package?(:x_contrib)

if cygwin
  $gnu_tool_path.push('/usr/local/bin/i686-pc-cygwin',
                      "$HOME/bin/win")
elsif ftp
  $gnu_tool_path << '$HOME/gnu'
end

prependPath *$gnu_tool_path

appendPath *Array.join(' ') {|path|
  path.concat %w(/sbin /usr/sbin)

  path.concat %w(/usr/local/sbin) unless cygwin or sunos

  path.concat %w($JAVA_HOME/bin)              if is_var?(:JAVA_HOME)
  path.concat %w($ANT_HOME/bin)               if is_var?(:ANT_HOME)
  path.concat %w($HOME/bin)
  path.concat %w($HOME/bin/usr-local-bin)     unless owned
  path.push      $cxoffice_home + '/bin'      if $cxoffice_home
}

if package?(:versant)
  if_defined(:VERSANT_ROOT)
    if cygwin
      vsnt_root = '$(cygpath -au $VERSANT_ROOT)'
      sep       = '\\\\'
    else
      vsnt_root = '$VERSANT_ROOT'
      sep       = path_separator
    end
    
    appendPath('$HOME/bin/vsnt', "#{vsnt_root}#{sep}bin")

    exportDefault(:VERSANT_DB) {
      any    '`dirname $VERSANT_ROOT`/db'
      cygwin "$(cygpath -aw $(dirname #{vsnt_root})/db)"
    }

    exportDefault :VERSANT_DBID,     '$VERSANT_DB'
    exportDefault :VERSANT_DBID_NODE, host        
  shell_endif
end

appendPath %w(/opt/bin /opt/$USER/firefox /opt/$USER/thunderbird /opt/cxoffice/bin) if owned and not cygwin

# zsh ": ${#{self.path_varname}##:}"


# ================================================
# -====== Library Path

appendLibpath *Array.join(' ') {|path|
  path.concat %w(/lib /usr/lib $HOME/lib)
}

appendRubyLib *%w($HOME/lib/ruby $HOME/bin)

appendRubyLib *%w(site_ruby/1.8
                 site_ruby/1.8/i686-linux
                 site_ruby
                 1.8
                 1.8/i686-linux).collect {|e| "#$RUBY_ROOT/usr/local/lib/ruby/" + e} if $RUBY_ROOT


# ================================================
# -====== Prompt

if_interactive
#    function(:precmd, "print -Pn '#$ESC]2;%m\\a'") if zsh

#    function(:precmd, <<PRECMD
#          echo $CTVIEW\n#$TIME_COLOR#{linux ? '\\A' : '`date +%R`'}#$NO_COLOR #$USERNAME_COLOR\\u@#$HOST_COLOR#@host#$NO_COLOR #$PATH_COLOR
#          builtin dirs |awk '{print strftime("%H:%M", systime()), "pschaaf@kappa7 \033[0m\033[1;33m" $1, "\033[0m\033[33m" $2, $3, $4, $5, $6, $7, $8, $9}'
# PRECMD
#    ) if zsh

  if bash || csh
    if self.use_color
      setenv(:HOST_COLOR, "\"#$HOST_COLOR\"")
      shell_if_root
        prependToVar(:HOST_COLOR, "\"#{Color.on_red}### \"")
      shell_endif
    else
      setenv(:HOST_COLOR, '""')
    end
  end

  if bash
     if_inScreen
       set(:PROMPT_COMMAND, "'echo -n -e \"#{$ESC}k#$ESC\\134\"'")
     shell_endif

    function(:dirs_status, 'source $HOME/lib/sh/dirs_status')
    function(:precmd, 'dirs_status')

    $PROMPT_HEAD = "$CTVIEW\n#$TIME_COLOR#{linux ? '\\A' : '`date +%R`'}#$NO_COLOR #$USERNAME_COLOR\\u@#$HOST_COLOR#@host#$NO_COLOR #$PATH_COLOR"

    $PROMPT_TAIL ="\n\\[#$HIST_NUM_COLOR\\]\\!\\[#$NO_COLOR\\] \\[${CONNECTION_COLOR}\\]${CONNECTION}\\[#$NO_COLOR\\]${CONNECTION_END}${SCRIPTING_MSG}b\\$ "

  elsif zsh
    $PROMPT_HEAD = "$CTVIEW\n%{%(!.#$ROOT_COLOR.#$TIME_COLOR)%}%T%{#$NO_COLOR%} %{#$USERNAME_COLOR%}%n@%{#$HOST_COLOR%}#@host%{#$NO_COLOR%} %{#$PATH_COLOR%}"

    $PROMPT_TAIL ="%{#$NO_COLOR%}\n%2(L.#{sunos ? '$SHLVL' : '%L'}-.)%{\ek\e\\%(!.#$ROOT_COLOR.#$HIST_NUM_COLOR)%}%!%{#$NO_COLOR%} %{$CONNECTION_COLOR%}$CONNECTION%{#$NO_COLOR%}$CONNECTION_END$SCRIPTING_MSG#{linux || cygwin ? '%(!.#.$)' : '$'} "

  end

  setdirs =
    if csh
      set(:dirs, '')
      'set dirs=`dirs`; set dirs=${dirs[1]}'
    else
      %Q{local dirs
       dirs="`builtin dirs #{"-p" if cygwin}#{"| tr -d '\\012'" if sunos} |
       awk 'NR == 2 {printf("%s", \"#$MINOR_PATH_COLOR\")}
            {print}' ORS=' '`"}
    end

  chpwd = csh ? :cwdcmd : :chpwd

  setprompts = lambda {
    opt_comment("Use a #{'non-' unless self.use_color}color prompt")

    if tcsh
      setenv(:PROMPT_HEAD, "\"%T %$HOST_COLOR%m@%m%$NO_COLOR %$PATH_COLOR\"")
    elsif csh
      setenv(:prompt, "\"$HOST_COLOR${USER}@#{generic ? `hostname` : @host}#$NO_COLOR\\\\
! c$ \"")
    end

    function(:chpwd) {
      sh   nil
      csh  %Q!#{setdirs}; echo "`date +%H:%M` #$PATH_COLOR$dirs#$NO_COLOR"!
      bash %Q!#{setdirs}\n#{prompt_name}="#$PROMPT_HEAD$dirs#$PROMPT_TAIL"!
      zsh  %Q!#{setdirs}\n#{prompt_name}="#$PROMPT_HEAD%#{72 - host.size}>\\>>$dirs%<<#$PROMPT_TAIL"!
    }

    set(:PS2) {
      bash %Q["(#$PS2_COLOR\\!#$NO_COLOR) \\$ "]
      zsh  %Q["(%{#$PS2_COLOR%}%_%{#$NO_COLOR%}) %(!.#.$) "]
    }
  }

  if self.use_color
    shell_case('$TERM')

      shell_when(:emacs)
        self.muteColorsWhile(&setprompts)
        shell_nextwhen

      shell_case_else
        setprompts.call
        shell_nextwhen

    shell_endcase
  else
    setprompts.call
  end

  setenv(:PROMPT_NOCOLOR, stripColorsFrom(env[prompt_name]))

  any :chpwd

#  unset(:CONNECTION_END)

shell_endif # interactive


# ================================================
# -====== Cygwin

if cygwin
#   function(:fixpath) {
#     sh 'local name=$1; local value=$2
#         if [ $value ]; then
#            export $name=$(cygpath -au $value)
#         fi'
#   }
  shell_if_eql(:SHLVL, '1')

  if_not_defined(:CONNECTION)
    pvn = self.path_varname
    setenv(pvn, ":$#{pvn}")
    setenv(pvn, "${#{pvn}//:$USERPROFILE/:$HOME}")
    setenv(pvn, "${#{pvn}//:`cygpath -asw $USERPROFILE`/:$HOME}")
    setenv(pvn, "${#{pvn}#:}")
  shell_endif

#     any %w(ANT_HOME JAVA_HOME WL_HOME).collect {|e| "fixpath #{e} $#{e}"}.join($/)
#     any 'fixpath VERSANT_PATH $VERSANT_PATH' if package?(:versant)
    sh  'hash cmd'
  shell_endif
end


# ================================================
# ======= Manpath

# sunos :updateManpath
