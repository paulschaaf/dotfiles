#!/usr/bin/ruby

# $Source: e:/MyDocuments/cvsroot/etc/env/make_shells.rb,v $
# $Revision: 1.75 $
# $Date: 2004/07/07 02:05:56 $
# $Author: pschaaf $
# $State: Exp $
# $Name:  $

require 'util/platform'
require 'set'
require 'Makefile'

$config_files = %w(screenrc.rb shellrc.rb)

$common_components = %w(servers.rb keyboard.rb)

$port_script = 'port.rb'

$Irrelevant_Hosts    = [:generic, :laptop, :ruby]
$HidePersonalDetails = [:ftp]
$DeployToHosts       = [:ausable, :ftp, :laptop, :mail, :menominee, :satori, :sed, :styx]

SHOW_INFO=false

RUBYLIB_DEFAULT=$: + %w($HOME/lib/ruby $HOME/bin)
RUBY='ruby'

PORT_BIN="#{RUBY} ${RUBY_SWITCHES} ${RUBYLIB_ARGS} #$port_script#{' -i' if SHOW_INFO}"

$THIS_HOST=(ENV['HOST'] || `hostname #{'-s' unless $ON_WINDOWS}`).chomp.to_sym

$environments = {
  :ausable    => %w(bash tcsh screen zsh),
  :copernicus => %w(bash tcsh screen zsh),
  :ftp        => %w(bash             zsh),
  :generic    => %w(bash  csh        zsh),
  :laptop     => %w(bash tcsh screen zsh),
  :mail       => %w(                 zsh),
  :satori     => %w(bash tcsh screen zsh),
  :sed        => %w(bash tcsh screen zsh),
  :styx       => %w(bash tcsh screen zsh),
}

automatic_hosts = [$THIS_HOST]
case $THIS_HOST
  when :copernicus
    automatic_hosts << :isis
  when :laptop
    automatic_hosts << :ftp << :mail
end
automatic_hosts.uniq!

relevant_hosts = automatic_hosts.dup
case $THIS_HOST
  when :sed
    relevant_hosts << :ausable << :ftp
  when :copernicus
    relevant_hosts << :ftp
end
relevant_hosts.uniq!

$Irrelevant_Hosts += $environments.keys
$Irrelevant_Hosts -= relevant_hosts
$Irrelevant_Hosts.uniq!

LEGALIZE = $Irrelevant_Hosts

$environments.each {|host, shells|
  shells.delete('bash') if shells.include?('zsh')
  shells.delete('csh')  if shells.include?('tcsh')
  shells.delete('ksh')  if shells.include?('bash') || shells.include?('zsh')
  shells.sort!
}

unless MAKE_FILE=ARGV[0]
  puts "Usage: #{File.basename($0)} filename#$/Where filename is the name of the makefile to generate."
  exit 1
end

class Symbol
  def <=> other
    -(other <=> self.to_s)
  end
end

make=Makefile.new
make.comment("This makefile has been generated on #$THIS_HOST. Do not edit it by hand!")

make.newline
make[:AUTOMATIC_HOSTS]  = automatic_hosts.sort_by {|e| e.to_s}
make[:RELEVANT_HOSTS]   = (relevant_hosts - automatic_hosts).sort_by {|e| e.to_s}.unshift(make[:AUTOMATIC_HOSTS])
make[:IRRELEVANT_HOSTS] = $Irrelevant_Hosts.sort_by {|e| e.to_s}

make.newline
make.if_not_defined(:RUBYLIB) {
  make[:RUBYLIB_ARGS] = "-I #{RUBYLIB_DEFAULT.join(' -I').gsub(/\$$/, '$')} "
}

make.newline
make.if_defined(:CYGROOT) {
  make.comment('Ignore warnings about insecure directories under Cygwin')
  make[:RUBY_SWITCHES] = '-W$${RUBY_WARN_LEVEL:=0}'
}

make[:LEGALIZE_ARGS]  = "--legalize='#{LEGALIZE.join(' ')}'" unless LEGALIZE.empty?

make.newline
make[:CMD_START]      = "@#{'rm -f $@; ' if $ON_CYGWIN}echo \\\\\\# === Making $@"
make[:CMD]            = "@#{PORT_BIN} #{make[:LEGALIZE_ARGS] if make[:LEGALIZE_ARGS]} --host=$(basename $@)"
make[:CMD_END]        = " >#{'|' unless $ON_CYGWIN} $@"

make.newline
make.comment("==================== Common Rules")
make.newline

make.suffixes
make.phony

make.new_rule(:all, [:localhost, make[:AUTOMATIC_HOSTS]])
make.new_rule(:localhost, [$THIS_HOST])
make.new_rule(make[:ALL_SHELLS], $port_script)
make.new_rule(:clean, nil, 'rm -rf ${ALL_SHELLS}')
deploy=make.new_rule(:deploy)

$config_files.inject(make.new_rule) {|all_files_rule, str|
  full_filename = '.' + str.gsub(/(.rb)$/, '-full\1')
  all_files_rule.addTarget(full_filename)
  make.new_rule(full_filename, $common_components + str.to_a)
  all_files_rule
}.addCmd <<'QUOTE'
temp=`mktemp $$$$.XXXXXX`; ruby -n -e 'puts gsub(/^(# )-(====== .*)/, %q{comment \1==\2}) unless /^#([^ ]| [^=-])/ =~ $$_' $^ >> $$temp; mv -f $$temp $@
QUOTE

$all_shell_rules = Hash.new {|aHash, key| 
  shell_file = ".#{(key =~ /screen$/) ? :screen : :shell}rc-full.rb"
  rule = (aHash[key] = make.new_rule)
  rule.addDep(shell_file)
  rule.addCmd(make[:CMD_START], "#{make[:CMD]} --shell=$(suffix $@) $? #{make[:CMD_END]}")
  rule
}

make[:ALL_SHELLS] # reference this to force declaration 

make.newline
make.comment("==================== Hosts")
make.newline

relevant_hosts.each {|host|
  shells = $environments[host].sort

  preferred_shell_full = `pref_shell #{host}`.chomp

  if preferred_shell_full.empty?
    preferred_shell      = nil
    preferred_shell_full = nil
  else
    preferred_shell      = `basename #{preferred_shell_full}`.chomp
    preferred_shell_full = nil if preferred_shell_full =~ %r[(/usr)?/bin/#{preferred_shell}]
  end

  # ensure sh and preferred shell get compiled first
  shells.delete(preferred_shell)

  shells.unshift('sh')
  shells.unshift(preferred_shell) unless preferred_shell == 'sh' or preferred_shell.empty?

  rule = make.new_rule(host,
                       shells.collect {|aShell|
                         host_shell = "#{host}.#{aShell}"
                         make[:ALL_SHELLS] << host_shell
                         
                         $all_shell_rules[if preferred_shell_full and 
                                              aShell == preferred_shell
                                            preferred_shell_full
                                          elsif aShell == 'screen'
                                            'screen'
                                          else
                                            '$(suffix $@)'
                                          end].addTarget(host_shell)
                       })
  if $HidePersonalDetails.include?(host)
    rule.addCmd('perl -pi -e \'\'s/^.*schaaf.*//g\'\' $?')
  end
  if $DeployToHosts.include?(host)
    $environments[host].each {|sh| rule.addCmd("scp $@.#{sh} $@:.#{sh}rc")}
    deploy.addDep(host) unless host = :ftp
  end
}

make[:ALL_SHELLS].value.sort!

File.delete(MAKE_FILE) if $ON_WINDOWS && FileTest.exists?(MAKE_FILE)

File.open(MAKE_FILE, 'w') {|outfile| outfile.puts(make)}

`attrib +H #{MAKE_FILE}` if $ON_WINDOWS

puts "Rebuilt '#{MAKE_FILE}'."
