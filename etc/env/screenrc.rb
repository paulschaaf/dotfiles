# -====== $Source: e:/MyDocuments/cvsroot/etc/env/screenrc.rb,v $
# -====== $Revision: 1.44 $
# -====== $Date: 2004/02/24 22:25:25 $
# -====== $Author: pschaaf $
# ======= $State: Exp $
# ======= $Name:  $

$Max_Screen_Num = 39
$initialWindows = 3

# ================================================
# -====== Setup Environment

setenv          :inScreen, 'true'
unsetenv        :SHLVL

# caption :always, '"%{= wk}%-Lw%{= bw}%n%f %t%{-}%+Lw %{-}"'
caption         :always, '"%-Lw%{= bw}%n%f %t%{-}%+Lw"'

defflow         :off
defmonitor      :on
defscrollback   1000
ignorecase      :on unless cygwin
shell           '/bin/zsh'
shelltitle      "'$ |-'"
startup_message :off

hardstatus      :on
hardstatus      :lastline
hardstatus      :string, '"%{=br yk}%m/%d %c %-= %H %l"'

# ================================================
# -====== Switch Between Windows

bindkey('select 1',     'F1: goto window 1',       Keymap[:F1 ])
bindkey('select 2',     'F2: goto window 2',       Keymap[:F2 ])
bindkey('select 3',     'F3: goto window 3',       Keymap[:F3 ])
bindkey('select 4',     'F4: goto window 4',       Keymap[:F4 ])
bindkey('select 5',     'F5: goto window 5',       Keymap[:F5 ])

bindkey(:screen,        'F11: new screen',         Keymap[:F11])
#indkey(:screen,        'Alt-F1 == F1',             shift(:F1 ))     if copernicus
bindkey(stuffln(:exit), 'F12: exit',               Keymap[:F12])
#indkey(stuffln(:exit), 'Alt-F2: == F2',            shift(:F2 ))     if copernicus

bindkey(:prev,          'Alt-left:  prev window',  Keymap[:alt_left])
bindkey(:next,          'Alt-right: next window',  Keymap[:alt_right])
bindkey(:next,          'Alt-Tab:   next window',  Keymap[:alt_tab]) if Keymap[:alt_tab]

bind(:o, :focus)

# ================================================
# -====== Other Keys

# emacs navigation for copy mode
# markkeys '"h=^B:l=^F:$=^E"'

bindkey(stuffln('^Upopd'),
        'clear line then popd',
        Keymap[:shift_tab])

bindkey(:windowlist,
        'Win key shows list of windows',
        Keymap[:windows])

bindkey("screen -t \"Connect to?\" #$Max_Screen_Num $HOME/bin/menu-rlogin.bash",
        'Menu key',
        Keymap[:menu])

# ================================================
# -====== Create Some Windows

new_screen {
  any    '-t \'#|su:\' 0 suroot -l'
  cygwin '-t \'>\'     0 cmd /T:1e'
}

# chdir '$HOME/etc/env'
# new_screen(2, '-t \'$|env:\'')
#  -= 1
# chdir ''

$initialWindows.downto(screens_created) {|num| new_screen(num)}

