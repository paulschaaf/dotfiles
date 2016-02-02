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

$Irrelevant_Hosts = [:laptop, :ruby]

SHOW_INFO=false

RUBYLIB_DEFAULT=%w($HOME/lib/ruby $HOME/bin)
RUBY='ruby'

LEGALIZE = $Irrelevant_Hosts

PORT_BIN="#{RUBY} ${RUBY_SWITCHES} ${RUBYLIB_ARGS} #$port_script#{' -i' if SHOW_INFO}"

$THIS_HOST=(ENV['HOST'] || `hostname #{'-s' unless $ON_WINDOWS}`).chomp.to_sym

$environments = {
  :copernicus => %w(bash csh     screen tcsh zsh),
  :ftp        => %w(                         zsh),
  :generic    => %w(bash csh                    ),
  :ramsesii   => %w(             screen      zsh),
}

$environments.each {|host, shells|
  shells.delete('bash') if shells.include?('zsh')

  shells.delete('ksh') if shells.include?('bash') || shells.include?('zsh')

  shells.delete('csh') if shells.include?('tcsh')
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

relevant_hosts = ($environments.keys << :generic).uniq.sort_by {|e| e.to_s}

make=Makefile.new
make.comment('-*- eval: (outline-minor-mode) -*-')
make.comment("This makefile has been generated on #$THIS_HOST. Do not edit it by hand!")

make.newline
make[:RELEVANT_HOSTS]   = relevant_hosts
make[:IRRELEVANT_HOSTS] = ($Irrelevant_Hosts + $environments.keys - relevant_hosts).collect {|e| e.to_s}.sort

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
make.new_rule('.SUFFIXES')
make.phony

make.new_rule(:all, [:localhost, make[:RELEVANT_HOSTS]])
make.new_rule(:localhost, [$THIS_HOST])

$config_files.inject(make.new_rule) {|all_files_rule, str|
  full_filename = '.' + str.gsub(/(.rb)$/, '-full\1')
  all_files_rule.addTarget(full_filename)
  make.new_rule(full_filename, $common_components + str.to_a)
  all_files_rule
}.addCmd %q{temp=`mktemp $$$$.XXXXXX`; ruby -n -e "puts gsub(/^(# )-(====== .*)/, \$$/ + 'comment %q{\1==\2}') unless \$$_ =~ /^#([^ ]| [^=-])/" $^ >> $$temp; mv -f $$temp $@}

$all_shell_rules = Hash.new {|aHash, key| 
  rule = (aHash[key] = make.new_rule)
  shell_file = ".#{(key =~ /screen$/) ? :screen : :shell}rc-full.rb"
  rule.addCmd(make[:CMD_START], "#{make[:CMD]} --shell=#{key} #{shell_file} #{make[:CMD_END]}")
  rule.addDep(shell_file)
  rule
}

make[:ALL_SHELLS] # reference this to force declaration 

make.newline
make.comment("==================== Hosts")

relevant_hosts.each {|host|
  shells = $environments[host].sort

  preferred_shell_full = `pref_shell #{host}`.chomp

  if preferred_shell_full.empty?
    preferred_shell      = nil
    preferred_shell_full = nil
  else
    preferred_shell      = `basename #{preferred_shell_full}`.chomp
    preferred_shell_full = nil if preferred_shell_full == "/bin/#{preferred_shell}"
  end

  # ensure sh and preferred shell get compiled first
  shells.delete(preferred_shell)

  shells.unshift('sh')
  shells.unshift(preferred_shell) unless preferred_shell == 'sh' or preferred_shell.empty?

  make.new_rule(host,
                shells.collect {|aShell|
                  host_shell = "#{host}.#{aShell}"
                  make[:ALL_SHELLS] << host_shell
                  
                  $all_shell_rules[if aShell == 'screen'
                                     aShell
                                   elsif preferred_shell_full and aShell == preferred_shell
                                     preferred_shell_full
                                   else
                                     '$(suffix $@)'
                                   end].addTarget(host_shell)
                })
}

make.new_rule('ramsesii', nil, '-scp $@.zsh $@:.zshrc')
make.new_rule('vftp', nil, '-perl -pi -e ''s/^.*schaaf.*//g'' $@.*')
make.new_rule('deploy', nil, '-scp ftp.zsh ftp:.zshrc', '-scp ~/.less_ftp ftp:.less')

make[:ALL_SHELLS].value.sort!

make.comment("==================== Common Rules")
# make[:ALL_SCREEN_SHELLS], make[:ALL_NONSCREEN_SHELLS] = *make[:ALL_SHELLS].value.partition {|e| e =~ /\.screen$/}
make.newline

make.new_rule(make[:ALL_SHELLS], $port_script)

# make.new_rule(make[:ALL_SCREEN_SHELLS],    [$port_script, '.screenrc-full.rb'])
# make.new_rule(make[:ALL_NONSCREEN_SHELLS], [$port_script, '.shellrc-full.rb'])

make.new_rule(:clean, nil, 'rm -rf ${ALL_SHELLS}')

File.delete(MAKE_FILE) if $ON_WINDOWS && FileTest.exists?(MAKE_FILE)

File.open(MAKE_FILE, 'w') {|outfile| outfile.puts(make)}

`attrib +H #{MAKE_FILE}` if $ON_WINDOWS

puts "Rebuilt '#{MAKE_FILE}'."
