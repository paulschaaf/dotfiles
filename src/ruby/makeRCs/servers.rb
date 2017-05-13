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
#   add_package(:winclient) if cygwin
end

$Editor = if package?(:emacsclient)
            'emacsclient -n -a emacs'
          elsif package?(:gnuclient)
            "gnuclient -f 'raise-frame' -q"
          else
            [:emacs, :xemacs, :vim, :vi].detect {|e| package?(e)}
          end
