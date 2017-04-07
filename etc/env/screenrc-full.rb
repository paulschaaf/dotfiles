
# ================================================
# ======= Tracing

comment {
  sh  'set -x'
  csh 'set echo'
}

# ================================================
# ======= Hostname Aliases

alias_host(
   :home_desktop => :copernicus,
   :laptop       => :sed
   :vftp         => :ftp,
)

# ================================================
# ======= Uname Aliases

allowMethod(:sunos5_8) {ftp}
allowMethod(:sunos5_6) {false}

# ================================================
# ======= Server Lists

$Owned_Servers   = HostNameList.new([:home_desktop, :laptop]).freeze
$Unowned_Servers = ($Servers - $Owned_Servers).freeze

$Clearcase_Servers = []
$Ruby_Servers      = $Owned_Servers
$Ssh_Servers       = $Owned_Servers  + [:ftp, :intra, :gwmail]
$Versant_Servers   = [] # $Servers        - [:home_desktop, :intra]

[$Clearcase_Servers, $Ruby_Servers, $Ssh_Servers, $Versant_Servers].each {|e| e.freeze}

allowMethod(:gnu_os)     {cygwin || linux}

allowMethod(:owned)      {$Owned_Servers.include?(host.to_sym)}

allowMethod(:path_needs_fixing) {false}


# ================================================
# ======= Packages

add_package(:ant)         if owned

add_package(:column)      if linux

add_package(:cxoffice)    if owned && linux

add_package(:emacs)       if owned or gnu_os
add_package(:emacsclient) if package?(:emacs)


add_package(:grep_gnu)    if gnu_os
add_package(:grep_color)  if package?(:grep_gnu)

add_package(:info)        if gnu_os

add_package(:java_sdk)

add_package(:less)

add_package(:ls_gnu)      if gnu_os
add_package(:ls_color)    if package?(:ls_gnu)

add_package(:msoffice)    if cygwin || package?(:cxoffice)

add_package(:ruby)        if owned

add_package(:screen)      if gnu_os

add_package(:sms)         if package?(:svn)

add_package(:ssh)         if $Ssh_Servers.include?(host.to_sym)

add_package(:suroot)      if owned

add_package(:svn)         if owned

add_package(:tree)        if owned || package?(:svn)

add_package(:x_contrib)   if owned

add_package(:vi)
add_package(:vim)         if package?(:vi) and (owned or gnu_os)

add_package(:xemacs)      if owned or gnu_os
add_package(:gnuclient)   if package?(:xemacs)
add_package(:spawn_gnuclient) if package?(:xemacs)


# ================================================
# ======= GNU Tools

$cxoffice_home = '/opt/cxoffice'   if package?(:cxoffice)

$gnu_tool_path = [] << '/bin'           \
                    << '/usr/bin'       \
                    << '/usr/local/bin'

$pager_bin     = cond(package?(:less), '/usr/bin/less',
                  'more') unless owned

$ls_bin        = 'ls'


# ================================================
# ======= SSH Servers

allowMethod(:ssh_server) {package?(:ssh)}

if ssh_server
  if home_desktop
    $local_ssh_servers = HostNameList.new([:home_desktop, :laptop])
    $ssh_gateway       = :mail
  else
    $local_ssh_servers = $Ssh_Servers.dup
    $local_ssh_servers.delete(:home_desktop) unless laptop
    $ssh_gateway       = nil
  end
  $remote_ssh_servers  = $Ssh_Servers - $local_ssh_servers
end


# ================================================
# ======= Ruby Servers

allowMethod(:local_ruby)  {package?(:ruby)}
allowMethod(:remote_ruby) {ssh_server && ! local_ruby}
allowMethod(:ruby)        {local_ruby ||   remote_ruby}


# ================================================
# ======= Editor

if package?(:xemacs)
  add_package(:gnuclient)
  add_package(:spawn_gnuclient)
end

$Editor = if package?(:emacsclient)
            'emacsclient -n -a emacs'
          elsif package?(:gnuclient)
            "gnuclient -f 'raise-frame' -q"
          else
            [:emacs, :xemacs, :vim, :vi].detect {|e| package?(e)}
          end
# ================================================
# ======= Key Definitions

xterm, rxvt = linux, false

# ======= Escape

$ESC = {
  any  => 033.chr,
  zsh  => '\033',
}[true]

(Keymap = Hash.new)[:esc]=$ESC

# ======= Modifiers


Keymap[:SHIFT] = ';2'
Keymap.add_successors_of_value_at(:SHIFT, :ALT,        :ALT_SHIFT,
                                  :CTRL,  :CTRL_SHIFT, :ALT_CTRL, :ALT_CTRL_SHIFT)

def modify(keyname, with)
  keystroke, mod = Keymap[keyname], Keymap[with]
  case keyname
  when :up, :down, :right, :left
    mod = "1#{mod}"
  else
  end
  keystroke.dup.insert(-2, mod)
end

def alt(key);   modify(key, :ALT);   end
def ctrl(key);  modify(key, :CTRL);  end
def shift(key); modify(key, :SHIFT); end

# ======= Function Keys

FunKeys = Array.new

def FunKeys.[]=(num, val)
  Keymap["F#{num}".to_sym] = val
  super
end

if screen_term
  %w(k1 k2 k3 k4 k5 k6 k7 k8 k9 k; F1 F2).inject(1) {|num, keyname|
    FunKeys[num] = "-k #{keyname}"
    num + 1
  }
else
  #codes = %w([[A [[B [[C [[D [[E) # if term == :cygwin
  codes = %w( OP  OQ  OR  OS [15) # if term == :xterm under cygwin

  codes += %w([17 [18 [19 [20 [21 [23 [24)

  codes.inject(1) {|num, code|
    FunKeys[num] = "#{$ESC}#{code}"
    FunKeys[num] += '~' #unless cygwin
    num + 1
  }
end

# ======= Arrow Keys

Keymap[:up]      = "#{$ESC}[A"
Keymap[:alt_up]  = alt( :up)
Keymap[:ctrl_up] = ctrl(:up)

Keymap.add_successors_of_value_at(     :up,      :down,      :right,      :left)
Keymap.add_successors_of_value_at( :alt_up,  :alt_down,  :alt_right,  :alt_left)
Keymap.add_successors_of_value_at(:ctrl_up, :ctrl_down, :ctrl_right, :ctrl_left)


# ======= Command Keys

Keymap[:home]    = "#{$ESC}[1~"
Keymap.add_successors_of_value_at(:home, :insert, :delete, :end, :page_up, :page_down)


Keymap[:ctrl_delete] = {
  any    => ctrl(:delete),
  rxvt   => "#{$ESC}[3^",
  linux  => nil,
  cygwin => "#{$ESC}[3;5~",
}[true]

Keymap[:alt_backspace]  = {
  any    => nil,
