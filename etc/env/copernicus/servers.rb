# $Source: e:/MyDocuments/cvsroot/etc/env/servers.rb,v $
# $Revision: 1.15 $
# $Date: 2004/07/07 02:26:21 $
# $Author: pschaaf $
# $State: Exp $
# $Name:  $

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
   :vftp         => :ftp
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
$Ssh_Servers       = $Owned_Servers  + [:ftp, :intra]
$Versant_Servers   = $Servers        - [:home_desktop, :intra]

[$Clearcase_Servers, $Ruby_Servers, $Ssh_Servers, $Versant_Servers].each {|e| e.freeze}

allowMethod(:gnu_os)     {cygwin || linux}

allowMethod(:owned)      {$Owned_Servers.include?(host.to_sym)}

allowMethod(:path_needs_fixing) {false}

# ================================================
# ======= Packages

add_package(:ant)        if owned

add_package(:cleartool)  if $Clearcase_Servers.include?(host.to_sym)

add_package(:column)     if linux

add_package(:cxoffice)   if linux && owned

add_package(:cvs)        if owned

add_package(:emacs)      if owned or linux or cygwin

add_package(:grep_gnu)   if gnu_os || ftp
add_package(:grep_color) if package?(:grep_gnu)

add_package(:info)       if gnu_os

add_package(:java_sdk)   if owned

add_package(:less)

add_package(:ls_gnu)     if gnu_os || ftp
add_package(:ls_color)   if package?(:ls_gnu)

add_package(:msoffice)   if cygwin || package?(:cxoffice)

add_package(:ruby)       if owned

add_package(:screen)     if linux || cygwin

add_package(:sms)        if package?(:cvs)

add_package(:ssh)        if $Ssh_Servers.include?(host.to_sym)

add_package(:suroot)     if owned

add_package(:tree)       if package?(:cvs) || owned

add_package(:versant)    if $Versant_Servers.include?(host.to_sym)

add_package(:x_contrib)  if owned

add_package(:vi)

add_package(:vim)        if package?(:vi) and (owned or linux or cygwin)

add_package(:xemacs)     if owned or linux or cygwin

# ================================================
# ======= GNU Tools

$cxoffice_home = '/opt/cxoffice'   if package?(:cxoffice)

$gnu_tool_path  =  []
$gnu_tool_path << '/bin'           \
               << '/usr/bin'       \
               << '/usr/local/bin'

$pager_bin = cond(ftp,             nil,
                  package?(:less), '/usr/bin/less',
                  'more') unless owned

$ls_bin    = cond(ftp,        '$HOME/gnu/ls',
                  'ls')

$grep_bin  = cond(ftp,         nil)

$ssh_path  = cond(ftp,        '/usr/local/bin/ssh')

# ================================================
# ======= SSH Servers

allowMethod(:ssh_server) {package?(:ssh)}

if ssh_server
  if home_desktop
    $local_ssh_servers = HostNameList.new([:home_desktop, :laptop])
    $ssh_gateway       = :intra
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
allowMethod(:ruby)        {local_ruby || remote_ruby}

# ================================================
# ======= Editor

if package?(:xemacs)
  add_package(:gnuclient)
  add_package(:winclient) if cygwin
end

$Editor = [:winclient, :gnuclient, :xemacs, :emacs, :vim, :vi].detect {|e| package?(e)}
