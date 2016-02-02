#!/usr/bin/env ruby

# $Source: e:/MyDocuments/cvsroot/etc/env/port.rb,v $
# $Revision: 1.183 $ 
# $Date: 2004/07/07 02:05:56 $
# $Author: pschaaf $
# $State: Exp $
# $Name:  $


# ================================================
# ======= Libraries

require 'extn/Array'
require 'extn/Boolean'
require 'extn/Enumerable'
require 'extn/Module'
require 'extn/Object'
require 'lib/ansicolor'
require 'lib/SortedCollection'
require 'set'
require 'stringio'
require 'contacts.rb'

# ================================================
# ======= Globals

$DEBUG=true
$heredoc_name = :DEBUG_NO_EXEC
$opt_comments_visible = false

$Emacs_Settings = Hash.new
def $Emacs_Settings.to_s
  '-*- ' + (self.collect {|k, v| "#{k}: #{v}"}.join('; ')) << ' -*-'
end

$Hostname_Aliases = Hash.new

# Declare any names here to make them legal at runtime (thus avoiding compiler warnings).
$Legalize = [].instance_eval {
  # operating systems
  push(*%w(aix cygwin hpux linux sunos sunos5_6 sunos5_8))

  # shells
  push(*%w(ash bash csh eshell ksh pdksh screen sh tcsh zsh))

  # terminals
  push(*%w(aterm eterm linux rxvt screen_term xterm vt100))

  # other
  push(*%w(generic login))
}

$show_section_headings_as_comments = false


# ================================================
# ======= Extensions to existing classes

class Array
  def stripColorsUsing(&block)
    self.collect {|e| e.stripColorsUsing(&block)}
  end
end

class File
  def self.mktemp
    name = `mktemp $HOME/tmp/$$.XXXXXX`.chomp
    raise(IOError, $?) unless $? == 0
    name
  end

  def self.empty_then_open_safely(filename, *args, &block)
    temp = mktemp()
    open(temp, *args, &block)
  ensure
    delete(filename) if exists?(filename)
    rename(temp, filename)
  end
end

class Hash
  def add_successors_of_value_at(initial_key, *keys)
    self.add_successors_from_value(self[initial_key].succ, *keys)
  end

  def add_successors_from_value(initial_value, *keys)
    keys.inject(initial_value) {|value, key| (self[key] = value).succ}
    self
  end
end

module Kernel
  def dump(*args, &block)
    demarcate {
      args.each {|e| STDERR.puts(e.inspect)}
      STDERR.instance_eval(&block) if block_given?
    }
  end

  def demarcate(name=nil, show_messages=true)
    if show_messages
      STDERR.puts
      STDERR.print "before #{name} " if name
      STDERR.puts '------------------------------'
      STDERR.flush
    end
    answer = yield
    if show_messages
      STDERR.print "after  #{name} " if name
      STDERR.puts '------------------------------'
      STDERR.puts
      STDERR.flush
    end
    answer
  end

end

class Object
  def stripColorsUsing
    self
  end
  # def tput(capname)
  #   `tput #{capname}`
  # end
end

# class Proc
#   def negation
#     Proc.new {! self.call}
#   end
# end

class String
  include Term::ANSIColor

  def read
    self
  end

  def stripColorsUsing
    yield self
  end
end

class StringIO
  def print_words(*words)
    words.each {|word|
      unless word.nil? or word.empty?
        self.putc(?\ ) unless self.size == 0
        self.print(word)
      end
    }
    nil
  end
end


# ================================================
# ======= Emacs settings

$Emacs_Settings[:'outline-regexp'] = '"^[\t ]*#[\t ]+[^=]\\\|^# +\\\|^[\t ]*..."'


# ================================================
# ======= Hostnames

def alias_host(args)
  args.each_pair {| _new, _old |
    new, old = _new.to_sym, _old.to_sym
    $Hostname_Aliases[new] = old
    allowMethod(new) {send(old)}
    eval "$#{new} = old"
  }
end

def resolve_host_aliases(*aliases)
  aliases.collect {|name| ($Hostname_Aliases[name] || name).to_sym}
end

class Symbol
  def <=>(other)
    -(other <=> self.to_s)
  end

  def resolvedHostname
   resolve_host_aliases(self)
  end
end

class Host
  attr_accessor :name, :uname

  def initialize(aName, aUname)
    @name, @uname = aName.to_sym, aUname
    printf("New host with: %s, %s\n", @name, @uname)
  end

  def resolvedHostname
    resolve_host_aliases(self.name)
  end

  def uname
    @uname.to_s[/[A-Za-z0-9]+/]
  end
  alias_method :shortUname, :uname

  def longUname
    @uname
  end

  def to_s
    @name.to_s
  end
end

class HostNameList < Set
  def add(arg)
    self << arg
  end

  def <<(arg)
    super(arg)
    other = arg.resolvedHostname
    if other != arg.name
      (newhost = arg.dup).name = other
      super(newhost)
    end
    self
  end
end

$Servers = HostNameList.new

File.readlines(ENV["HOME"] + %q-/etc/hosts-).each {|e|
  e.strip!
  e.gsub!(/#.*/, '')
  $Servers.add(Host.new(*e.split)) unless e.empty?
}

$Servers = ENV['ALL_HOSTS'].chomp.split.inject(HostNameList.new) {|coll, e| coll << e.to_sym}

# ================================================
# ======= Supporting classes

=begin
== ((OptionFlags))
blah blah blah
=end #'#"#`#
class OptionFlags < Hash
=begin
=== Class methods
=end #'#"#`#
=begin private
=== Instance methods
--- OptionFlags#to_s
    Make a concise printable version of the flags.
=end #'#"#`#
#   def to_s
#     answer = (self.keys.sort_by {|e| e.to_s}.collect {|key|
#                 switch_str, value_s, sep = key.to_s, self[key].to_s, '='

#                 if switch_str.size > 1
#                   switch_str = if switch_str[0] == ?-
#                                  switch_str[1..-1]
#                                else
#                                  '-' + switch_str
#                                end
#                   sep = ' '
#                 end

#                 switch_str << sep << value_s unless value_s.empty?
#                 switch_str
#               }.join(' -'))
#     answer = '-' + answer unless answer.empty?
#     answer
#   end

  def to_s
    one_char, multi_char = '', ''

    self.keys.sort_by {|e| e.to_s}.each {|key|
      string, value_s = key.to_s, self[key].to_s
      sep = '='

      if string.size > 1
        string = if string[0] == ?-
                   string[1..-1]
                 else
                   '-' + string
                 end
        sep = ' '
      end

      string << sep << value_s unless value_s.empty?

      (if string.size == 1
         one_char
       else
         multi_char << ' -'
       end) << string
    }

    one_char = '-' + one_char unless one_char.empty?

    one_char + multi_char
  end
end


# ================================================
class Command
  attr_accessor :args, :binary, :env, :flags

  def initialize(bin='', *flags)
    @args   = Array.new
    @env    = Array.new
    @binary = bin
    @flags  = OptionFlags.new
    self.add_flags(*flags) unless flags.empty?
  end

  def +(other);  self.with_flags(*other.flags);     end
  def -(other);  self.without_flags(*other.flags);  end

  def initialize_copy(from)
    @args    = from.args.clone
    @binary  = from.binary.clone
    @env     = from.env.clone
    @flags   = from.flags.clone
  end

  def add_env(*env)
    self.with_env!(*env)
    env
  end

  def add_flags(*flags)
    self.with_flags!(*flags)
    flags
  end

  def add_args(*args)
    self.with_args!(*args)
    args
  end

  def delete_flags(*old)
    old.select {|e| @flags.delete(e)}
  end

  def with_args!(*new);        @args.push(*new);      self;  end
  def with_env!(*new);         @env.push( *new);      self;  end
  def with_binary!(new);       @binary =   new;       self;  end
  def with_flags!(*new)
    new.each {|e| @flags[e] = nil}
    self
  end
  def without_flags!(*flags); 
    @flags.delete(*flags)
    self
  end

  def with_args(*args);        clone.with_args!(     *args );  end
  def with_env(*env);          clone.with_env!(      *env  );  end
  def with_binary(bin);        clone.with_binary!(    bin  );  end
  def with_flags(*new);        clone.with_flags!(    *new  );  end
  def without_flags(*flags);   clone.without_flags!( *flags);  end

  def empty?
    @args.empty? && @binary.empty? && @flags.empty?
  end

  def to_s
    answer = StringIO.new
    answer.print_words(*env)
    answer.print_words(binary, flags, *args)
    answer.string
  end
end

# ================================================
class Color
  class << self
    include Term::ANSIColor
  end
end


# ================================================
# ======= Main class

class ShellScriptMaker
  attr_reader   :_sourceCode, :host, :shell, :term
  attr_accessor :env, :packages_header, :shellVars, :showInfo, :use_color

  def self.legalize(*args)
    #dump(args)
    self.ignoreMethod(*args)
  rescue Exception => ex
    puts ex.backtrace
    raise(ex.class, ex.message, ex.backtrace)
  end

  def self.uname_of(machine='')
    host = machine.to_sym.resolvedHostname
    $Servers.detect {|e| p e; e.name == host}.uname
    # `shortUname #{machine.to_sym.resolvedHostname}`.chomp
  rescue
    $stderr.printf("## --- Error: no uname defined for '%s'\n", host.inspect)
    exit 1
  end

  legalize(*$Legalize)
  # legalize(*$Servers.inject {|arr, e| arr << e.name.to_s << e.uname.to_s})
  # legalize(*self.uname_of('-a').downcase.split(' '))
  $Servers.each {|e| legalize(e.name.to_s, e.shortUname.to_s)}

  def self.default
    self.for()
  end

  def self.readFrom(aHash)
    self.legalize(*aHash[:legalize].split(' ').collect {|ea| ea.to_sym}) if aHash[:legalize]

    self.for(aHash[:shell],
             :host  => aHash[:host],
             :uname => aHash[:uname],
             :term  => aHash[:term],
             :login => aHash[:login].to_b)
  end

  def self.for(shell, args)
    (eval "ScriptMaker_#{File.basename(shell).chomp.downcase}").new(shell, args)
  end

  def self.allowMethod(*names)
    answer=names.collect {|e| "alias_method :#{e.to_s}, :getMaybeSetSourceCode"}.join('; ')
    eval answer
  end

  def self.compatibleShells
    return [] unless (this_shell = self.shell)
    self.superclass.compatibleShells.unshift(this_shell)
  end

  def self.shell
    basename=self.name[/_.*/]
    basename[1..-1] if basename
  end

  def uname_of(*args)
    self.class.uname_of(*args)
  end

  def uname
    @uname || (self.uname= self.uname_of(self.host))
  end

  def uname=(an_uname)
    return self.uname if an_uname.empty?
    @uname = an_uname.downcase
    allowMethod(@uname.gsub(/-/, '_').gsub(/_NT_.*/, ''))
    @uname
  end

  def default_term
    'xterm'
  end

  def correct_term(*termlist)
    termlist << ENV['TERM']
    answer = termlist.detect {|e| !e.empty?}
    case answer
    when 'screen'
      'screen_term'
    when 'linux'
      'linux_term'
    else
      answer || self.default_term
    end
  end

  def shell=(a_shell)
    @shell = a_shell
    allowMethod(*self.compatibleShells)
    @shell
  end

  def term=(a_term)
    @term = self.correct_term(a_term)
    allowMethod(@term)
    @term
  end

  def host=(a_host)
    @host = (a_host[/^[^.]+/]).downcase
    allowMethod(@host)
    @host
  end

  def initialize(shell, args)
    self.shell = shell
    self.host  = (args[:host]  || `shortHostname`.chomp)
    self.uname =  args[:uname]
    self.term  = screen ? 'screen_term' : args[:term]

    @declared_functions        = []
    @outputEnabled, @use_color = true, true
    @env, @shellVars           = {}, {}
    @packages                  = Hash.new {|list, host| list[host] = Set.new}
    
    allowMethod(:any, :last_valid)
    self.allowMethod(:login) if args[:login]
    self.ignoreMethod(:none)

    if $opt_comments_visible
      self.class.alias_method(:opt_comment, :comment)
    else
      self.ignoreMethod(:opt_comment)
    end

    @packages_header = 'Used packages: '
  end

  def packages(host=self.host)
    @packages[host]
  end

  def add_package(name, *hosts)
    hosts << self.host if hosts.empty?
    hosts.each {|host| self.packages(host).add(name)}
    #dump(name, hosts, @packages)
    name
  end
    
  def package?(name, host=self.host)
    self.packages(host).include?(name)
  end

  def suspendOutputWhile
    initial, @outputEnabled = @outputEnabled, false
    code = yield
  ensure
    @outputEnabled = initial
    puts code.to_s if @outputEnabled
    code
  end

  def compatibleShells
    self.class.compatibleShells
  end

  def function?(name)
    @declared_functions.include?(name)
  end

  def env_var?(name)
    self.env[name]
  end

  def shell_var?(name)
    self.shellVars[name]
  end

  def is_var?(name)
    self.shell_var?(name) || self.env_var?(name)
  end

  def statement_sep
    "\n\t"
  end

  def onlyAllowMethodsIf(value, *meths)
    if value
      self.class.allowMethod(*meths)
    else
      self.class.ignoreMethod(*meths)
    end
  end

  def allowMethod(*args)
    deny = block_given? && (! yield)
    self.onlyAllowMethodsIf(! deny, *args)
  end

  def ignoreMethod(*args)
    allow = block_given? && (! yield)
    self.onlyAllowMethodsIf(allow, *args)
  end

  def newline
    any ''
  end

  def generateHeader
    if self.host.to_s != 'ftp'
      comment $Emacs_Settings

      newline
      comment('****** WARNING: This file is automatically generated. ****** ',
              '******          Edits may be overwritten at any time! ****** ')

      newline
      comment(sprintf("Built:\t%s", `date`.chomp),
              sprintf("By:  \t%s@%s", `whoami`.chomp, `hostname`[/^\w+/]))
    end

    two_columns = '   %-8s = %-10s'*2

    output = comment('For:',
                     sprintf(two_columns, 'host',  self.host,  'shell', self.shell),
                     sprintf(two_columns, 'uname', self.uname, 'term',  self.term))
    STDERR.puts('', output, '') if showInfo

    newline
#     function(:unique,
#       %Q<awk "! times_seen[\$0]++ {uniq = uniq ORS \$0}; END {skip = length(ORS) + 1; printf substr(uniq, skip)}" ${*//-/}>
#     )
  end

  def generateFooter
    unless self.host.to_s == 'ftp'
      newline
      comment('Move this heredoc around in the file to temporarily disable sections',
              'of code. Make sure to keep the opening tag first, and the closing tag',
              'against the left margin on a line by itself. Don\'t forget that any',
              'edits are lost if this file is re-generated.')
      comment(": <<#$heredoc_name")
      comment("#$heredoc_name#{$/*2}")
    end

    newline
    comment(self.packages_header)
    `echo "#{self.packages.sort.join(' ')}" | tr ' ' '\n' | column -x`.split("\n").each {|line|
      any("#\t" + line)
    }
  end

  def preprocess(aString)
    aString.gsub!(/# -(====== .*)/, 'section_break "=\1"') if $show_section_headings_as_comments
  end

  def process(*files)
    self.generateHeader
    files.each {|filename| self.process_file(filename)}
  ensure
    self.generateFooter
  end

  def process_file(filename)
    aString = File.read(filename)
    self.preprocess(aString)
    self.eval_string(aString, filename)
    self.postprocess(aString)
  end

  def eval_string(aString, filename=nil)
    eval(aString)
  rescue Exception => ex
    begin
      message = ex.message.split(/:/)

      file = message.shift
      file = filename if file == '(eval)'

      line_number = message.shift.to_i

#       loc = if message.size == 1
#               message[0].slice!(/^in [^\']+.:?/)
#             else
#               message.shift
#             end

      message = [message.join(':').strip.gsub(/^\(eval\)/, filename)]

      line = (if file == $0
                File.open($0, 'r') {|f| f.readlines}
              else
                aString.split($/)
              end)[line_number - 1].to_s

      hide_lines = Regexp.new("#$0.*in \`eval(_string)?\'$")

      backtrace = ex.backtrace.inject(["#{file}:#{line_number}"]) {|trace, entry|
        # keep entries referring to the eval'd string
        if entry.gsub!(/^\(eval\)/, filename)
          num = entry.split(':')[1].to_i
          entry.gsub!(/in .eval_string.$/, 
                      " ->#$/" + File.open(filename, 'r') {|f|f.readlines}[num - 1])
          trace << entry.strip

        # keep all other lines that do not mention 'eval' or 'eval_string'
        elsif entry !~ /\`eval(_string)?\'$/
          trace << entry

        end
        trace
      }

      line.each {|e| e.strip!}

      message << line.join("$/+  ")

    ensure
      dump {
        puts $/, "ex.message =\n   #{ex.message.inspect}"
        puts $/, "ex.backtrace =\n   #{ex.backtrace.inspect}"
        puts $/, "filename =   #{filename.inspect}"

        puts $/, "file =   #{file.inspect}"
        puts $/, "line_number = #{line_number.inspect}"
#         puts $/, "loc =   #{loc.inspect}"

        puts $/, "line =\n   #{line.inspect}"
        puts $/, "message =\n   #{message.inspect}"
        puts $/, "backtrace =\n   #{backtrace.inspect}"
        puts
      } if $DEBUG #or true
      puts
      raise(ex.class, message.join($/) + $/, backtrace)
    end
  end

  def postprocess(aString); end

  def stringColorPattern
    '\e\[\d+(;\d+)?m'
  end

  def stripColorsFrom(anObject)
    anObject.stripColorsUsing {|e| e.gsub(Regexp.new(self.stringColorPattern),'') if e}
  end

  def method_missing(label, *args)
    self.class.ignoreMethod(label)
    STDERR.puts("# ************ Warning: Ignoring settings for '#{label}' on line #{caller[0][/\d+/]}")
  end

  def setUseColorsWhile(aValue)
    initial, @use_color = @use_color, aValue
    answer = yield
  ensure
    self.use_color = initial
    answer
  end

  def useColorsWhile(&block)
    self.setUseColorsWhile(true, &block)
  end

  def muteColorsWhile(&block)
    self.setUseColorsWhile(false, &block)
  end

  # ======= Functions

  def notYetImplemented(*args)
    self.comment("#{self.class} does not yet implement #{caller[0].sub(/^.*:in\s+/, '')}")
  end

  def bindkey(action, comment, *keystrokes, &block)
    action_str = action.to_s
    keystrokes.each {|default_keystroke|
      self.emitCode(block, default_keystroke) {|keystroke|
        if keystroke
          switches = keystroke.slice!(/(-\w+ *)*/)
          unless keystroke.empty?
            keystroke = "'\"#{keystroke}\":"
            action_str += "'"
          end
        end
        self.emit_bindkey(:action => action_str,
                          :comment => comment,
                          :keystroke => keystroke,
                          :switches => switches)
      }
    }
  end

  def bindkey_cmd
    self.no_op_command
  end

  def self.no_op_command
    ':'
  end

  def no_op_command
    self.class.no_op_command
  end

  def self.basicEmitterCode(name, args={})
    [emitMethodName = "emit_#{name}",
     %Q!def #{name}(label=''#{', default=nil' if args[:with_default]}, &block)
        dump(%Q<in #{name}(\#{label}#{', \#{default}' if args[:with_default]})>)
        self.emitCode(block#{', default' if args[:with_default]}) {|code|
          self.#{emitMethodName}(#{'label, ' if args[:fixed_label]}code)
        }
      end!]
  end

  def self.simpleEmitter(*names)
    names.collect {|name|
      emitMethodName, code = basicEmitterCode(name)
      #$stderr.puts code
      module_eval code #if self.method_defined?(emitMethodName)
    }
  end

  def self.fixedLabelEmitter(*names)
    names.collect {|name|
      emitMethodName, code = basicEmitterCode(name, :fixed_label => true)
      module_eval code #if self.method_defined?(emitMethodName)
    }
  end

  def self.emitterWithDefault(*names)
    names.collect {|name|
      emitMethodName, code = basicEmitterCode(name, :fixed_label => true, :with_default => true)
      module_eval code #if self.method_defined?(emitMethodName)
    }
  end

  def self.labeledMacro(*names)
    names.collect {|name|
      emitMethodName="emit_#{name}"
      code = %Q!def #{name}(label='', string=nil, &block)
              self.emitCode(block, string) {|code|
                if self.respond_to?('#{emitMethodName}')
                  self.#{emitMethodName}(label, code)
                else
                  \"#{name} \#{label} \#{code}\"
	        end
              }
            end!
      module_eval code #if self.method_defined?(emitMethodName)
    }
  end

#   def self.delimitedMacro(name, lineHeading=name, separator="\n#{lineHeading}")
#     code = <<-CODE
#     def #{name}(*strings, &block)
#       self.emitCode(block, *strings) {"#{lineHeading} " + self._sourceCode.join('#{separator}')}
#     end
#     CODE
#     module_eval code
#   end

  def self.delimitedMacro(name, separator=' ', label=name)
    separator << name.to_s if separator == "\n"

    code = <<-CODE
    def #{name}(*strings, &block)
      self.emitCode(block, *strings) {
        '#{label}#{" " unless label =~ /\s$/}' + self._sourceCode.join('#{separator}')
      }
    end
    CODE
    #$stderr.puts code
    module_eval code
  end

  def simpleEmitter(*args)
    self.class.simpleEmitter(*args)
  end

  def emitterWithDefault(*args)
    self.class.emitterWithDefault(*args)
  end

  def labeledMacro(*args)
    self.class.labeledMacro(*args)
  end

  def delimitedMacro(*args)
    self.class.delimitedMacro(*args)
  end

  def comment(*strings, &block)
    comment_marker = '# '
    self.emitCode(block, *strings) {
      self._sourceCode.
        join($/ + comment_marker).
        gsub(/^([\t ]*)([^\#])/, "\\1#{comment_marker}\\2")
    }
  end

#   emitterWithDefault(:no_op)
#  def no_op(default='', &block)
#    dump(%Q<in no_op(#{label})>)
#    $stderr.puts caller if label == true
#    self.emitCode(block, default) {|code|
#      self.emit_no_op(code)
#    }
#  end


  delimitedMacro(:echo, "\n")
  delimitedMacro(:exec, "\n")
  #delimitedMacro(:no_op, " ", self.no_op_command)
  delimitedMacro(:source, "\n")
#  delimitedMacro(:unalias)
  delimitedMacro(:unset)

   def unalias(*strings, &block)
     self.emitCode(block, *strings) {'unalias ' + self._sourceCode.join(' ') + ' 2>/dev/null'}
   end

#   def section_break(*args, &block)
#     newline
#   end

  def declare_function(*labels)
    labels.each {|ea| @declared_functions.push(ea)}
  end

  def function(label, default=nil, &block)
    #unless self.login
    self.emitCode(block, default) {|code|
      self.declare_function(label)
      self.emit_function(label, code.to_s)
    }
    #end
  end

  def echoListFunction(name, varname=nil)
    function(name, Array.join(statement_sep) {|code|
               code << if name == :path
                         %q!local regex="${*// /\|}"
                            whence $* | 
                              sed '/^$\|^\/\|not found$/d; s:^\('$regex'\):[1;40;33m\1[0;0m:g' | pr -rto 2! unless csh
                       else
                         "echo Examining \\$#{Regexp.quote((varname || name.to_s.upcase))}"
                       end
               code << 'echo'

               cmd, list, args = 'echoList', varname || "$#{name.to_s.upcase}", self.args

               case name
               when :manpath
                 list = "${#{varname || 'MANPATH'}:-`man -w`}"

               when :path
                 list = "$#{varname || self.path_varname}"

               when :rubylib
                 #cmd  = 'echoList'
                 args += <<CODE

echo "Examining Ruby variable '\$:'"
echo
echoList `unset #{name.to_s.upcase}; ruby -e "print $:.join(':')"` $*
CODE
               end

               code << [cmd, list, args].join(' ')
             })
  end

  def appendAndPrependListFunctions(args)
    self.maybeIgnore_appendAndPrependListFunctions(false, args)
  end

  def ignore_appendAndPrependListFunctions(args)
    self.maybeIgnore_appendAndPrependListFunctions(true, args)
  end

  def maybeIgnore_appendAndPrependListFunctions(ignore, args)
    args.each_pair {|name, varname|
      prepend_name, append_name = "prepend#{name}", "append#{name}"

      if ignore
        self.ignoreMethod(prepend_name, append_name)
      else
        self.appendListFunction(append_name, varname)
        self.prependListFunction(prepend_name, varname)
      end
    }
  end

  def appendListFunction(name, varname)
    delimitedMacro(name)
  end

  def prependListFunction(name, varname)
    delimitedMacro(name)
  end

  def sshToFunction(dest, *arguments)
    cmd = Command.new('ssh')

    cmd.add_env('TERM=${TERM/cygwin/xterm}') if cygwin

    arguments.reject! {|e|
      case e.to_s
      when /^--.*$/
        cmd.add_flags(e)
      when /^-(.*)/
        cmd.add_flags(Regexp.last_match[1].to_sym)
      else
        false
      end
    }

    cmd.with_args!(dest.resolvedHostname).with_args!(*arguments)
#     cmd.add_flags(:q) if (linux || cygwin) && (uname_of(dest) !~ /(linux|cygwin).*/i)

#     if dest == :vftp
#       cmd.with_flags!(:t).add_args('exec gnu/zsh ${*:--l}')
    function(dest, "ssh-add -l | grep -q #{dest.resolvedHostname} || ssh-add ~/.ssh/#{dest.resolvedHostname}; #{cmd}")
#     else
#       shell_alias(dest, cmd)
#     end
  end

  def set(label, default=nil, &block)
    #unless self.login
    self.shellVars[label]=
      self.emitCode(block, default) {|value| self.emit_set(label, value)}
    #end
  end

  def setenv(label, default=nil, &block)
    #if self.login
    self.env[label]=
      self.emitCode(block, default) {|value| self.emit_setenv(label, value)}
    #end
  end

  def shell_alias(label, default=nil, &block)
    self.emitCode(block, default) {|value| self.emit_shell_alias(label, value)}
  end

  def shell_alias_default(label, default=nil, &block)
    self.emitCode(block, default) {|value| self.emit_shell_alias_default(label, value)}
  end

#   def cond(test, params)
#     self.shell_if(&test)
#       params[:then].value
#     self.shell_else if params[:else]
#       params[:else].value
#     self.shell_endif
#   end

  simpleEmitter      :shell_endcase

  def shell_case(label, &block)
    self.emitCode(block, label) {|value| self.emit_shell_case(label || value) if value}
  end

  def shell_when(label, &block)
    self.emitCode(block, label) {|value| self.emit_shell_when(label || value) if value}
  end

  def shell_nextwhen(label='', &block)
    self.emitCode(block, label) {|value| self.emit_shell_nextwhen(label || value)}
  end

  def shell_case_else(&block)
    self.emitCode(block) {self.emit_shell_case_else}
  end

  def shell_endcase(&block)
    self.emitCode(block) {self.emit_shell_endcase}
  end

  def shell_if_root
    shell_if(shellvar_eql?(:UID, 0))
  end

  def shell_if_not_root
    shell_if(shellvar_not_eql?(:UID, 0))
  end

  def shell_if(default=nil, &block)
    self.emitCode(block, default) {|cond| self.emit_shell_if(cond)}
  end

  def shell_unless(default=nil, &block)
    self.emitCode(block, default) {|cond| self.emit_shell_not_if(cond)}
  end

  def shell_if_exists(default=nil, &block)
    self.emitCode(block, default) {|cond| self.emit_shell_if_exists(cond)}
  end

  def shell_if_dir_exists(default=nil, &block)
    self.emitCode(block, default) {|cond| self.emit_shell_if_dir_exists(cond)}
  end

  def shell_if_eql(*args)
    self.shell_if(self.shellvar_eql?(*args))
  end

  def shell_if_not_eql(*args)
    self.shell_if(self.shellvar_not_eql?(*args))
  end

  def shell_not_exists(default=nil, &block)
    self.emitCod(block, default) {|cond| self.emit_shell_not_exists(cond)}
  end

  def shell_elsif(default=nil, &block)
    self.emitCode(block, default) {|cond| self.emit_shell_elsif(cond)}
  end

  def shellvar_eql?(varname, value)
    "\"$#{varname}\" = \"#{value}\""
  end

  def shellvar_not_eql?(varname, value)
    "\"$#{varname}\" \!= \"#{value}\""
  end

  def shell_else(&block)
    self.emitCode(block) {self.emit_shell_else}
  end

  def shell_endif(&block)
    self.emitCode(block) {self.emit_shell_endif}
  end

  def shell_or(code, &block)
    self.emitCode(block, code) {|code| self.emit_shell_or(code)}
  end

  def commentedTestCode(label, string=label, &block)
    self.emitCode(block, string) {|code| self.emit_commentedTestCode(label, code)}
  end

  def args
    unless @args
      @args = '$*'
      def @args.[](num)
        "$#{num}"
      end
    end
    @args
  end

  def path_separator
    '/'
  end

  def classpath_path_separator
    cygwin ? '\\\\' : self.path_separator
  end

  def list_separator
    ':'
  end

  def classpath_list_separator
    cygwin ? ';' : self.list_separator
  end

  def defined?(varname)
    "-n \"$#{varname}\""
  end

  def not_defined?(varname)
    "-z \"$#{varname}\""
  end

  def if_ssh_session
    self.if_defined(:SSH_TTY)
  end

  def if_defined(varname)
    self.shell_if(self.defined?(varname))
  end

  def if_not_defined(varname)
    self.shell_if(self.not_defined?(varname))
  end

  def elsif_defined(varname)
    self.shell_elsif(self.defined?(varname))
  end

  def if_inScreen
    self.shell_if(self.defined?(:inScreen))
  end

  def if_not_inScreen
    self.shell_if(self.not_defined?(:inScreen))
  end

  def ignoreDuplicatesList
    @ignoreDuplicatesList ||= []
  end

  def ignoreDuplicatesInList(*names)
    self.ignoreDuplicatesList.concat(names)
  end

  # ======= Private

  def _sourceCode=(aValue)
    @_sourceCode= use_color ? aValue : self.stripColorsFrom(aValue)
  end

  def emitCode(proc, *default, &emitter)
    self.suspendOutputWhile {
      code = self.getMaybeSetSourceCode(*default, &proc)
      emitter.call(*code) unless [[], [nil]].include?(code)
    }
  end

  def getMaybeSetSourceCode(*values, &block)
    return true if ! block && values.empty?
    self.suspendOutputWhile {
      self._sourceCode=values
      block.call if block
      self._sourceCode
    }
  end

  def emit_bindkey(args)
    Array.join {|answer|
      answer.push('# ') if args[:keystroke].to_s.empty?
      answer.push(self.bindkey_cmd, ' ',
                  args[:switches],
                  args[:keystroke],
                  args[:action],
                  "\t# ", args[:comment])
    }
  end

  def emit_commentedTestCode(label, code)
    self.comment('', "echo '----------'#{label}", code.split(';'))
  end

  def emit_set(label, value)
    "#{label}=#{value}"
  end

  def emit_shell_else
    'else'
  end

  def emit_shell_alias_default(label, value)
    "alias #{label} >& /dev/null || #{self.emit_shell_alias(label, value)}"
  end

  def emit_shell_alias(label, value)
    "alias #{label}=" << (if value =~ /^[\'\"].*[\'\"]$/
                            value
                          else
                            "'#{value}'"
                          end)
  end

  def emit_shell_or(code)
    " || #{code}"
  end

end

# ================================================
# ======= Specialization classes

# ================================================
class ScriptMaker_Possibly_Interactive < ShellScriptMaker
  def generateHeader
    super

    if_defined(self.prompt_name)
      set(:interactive, :true)
    shell_endif

    unless csh
      function(:prependToList,  concatWithList(true,  true))
      function(:appendToList,   concatWithList(false, true))
      function(:_prependToList, concatWithList(true,  false))
      function(:_appendToList,  concatWithList(false, false))
    end

    function(:prependToVar,   concatWithVar(true))
    function(:appendToVar,    concatWithVar(false))
    %w( prependToList appendToList 
        prependToVar  appendToVar).each {|each| labeledMacro(each)}

    allowMethod(:bourne) {self.class.to_s =~ /_sh$/}
  end

  def umask_values(umask)
    `sh -c 'umask -S #{umask.to_s(8)}'`.instance_eval {
      chomp!
      gsub!(/u=/,  'user=')
      gsub!(/g=/,  'group=')
      gsub!(/o=/,  'others=')
      gsub!(/,\b/, ', ')
      self
    }
  end

  def umask(mode='077')
    comment "This umask gives these access permissions:", *umask_values(mode)
    any(if block_given?
          "umask #{yield}"
        else
          'umask ' + mode.to_s(8)
        end)
    newline
  end

  def prompt_name
    :PS1
  end

  def if_interactive
    self.if_defined(:interactive)
  end

  def if_not_interactive
    self.if_not_defined(:interactive)
  end

  def if_login
    self.shell_if_eql('0', '-' + self.shell)
  end

  def if_not_login
    self.shell_if_not_eql('0', '-' + self.shell)
  end
end


# ================================================
class ScriptMaker_sh < ScriptMaker_Possibly_Interactive
  alias_method :unsetenv, :unset

  def generateHeader
    super
    setenv(:HOSTNAME, self.host)

# ============================================
<<EOF
echo -h --e -e --help bar foo -fe | awk '
function printvar(name, value) {
   printf("%s=%c%s%c%s", name, 39, value, 39, ORS)
}
{
   if (switches_done == 1)
      params = params OFS $0
   else if ($0 == "--")
      switches_done = 1
   else if ($0 ~ /^-.*/)
      switches = switches OFS $0
   else {  
      switches_done = 1
      params = $0
   }
}

END {   
   printvar("params", params)
   printvar("switches", switches)
   print "\n"
}
' RS='[\n ]' ORS='; ' OFS=' '


# ============================================
echo -h --e -e -- --help bar foo -fe | awk '

function printvar(name, value) {
   printf("%s=%c%s%c%s", name, 39)
   for(i=0; i<=length(value); , gensub(/^  *|  **$/, "", g, value), 39, ORS) 
}

function print_params()   {printvar("params",   input[params])}
function print_switches() {printvar("switches", input[switches])}

BEGIN {
   RS=/[\n ]/
   OFS=" "
   ORS="; "
   switches = 0
   params = 1
   length = 0
   using = switches
}

function swap() {
   if (using == switches)
      using = params
   else
      using = switches
   len = length(input[using])
}

function append(string) {input[using] << string}

/^--$/ {
   if (using == switches) {
      using = params
      next
   }
}

/^[^-]/ {using = params}

{append($0)}

END {
   print_params
   print_switches
   print "\n"
}
' RS='[\n ]' ORS='; ' OFS=' '
EOF
    function(:echoVar, '
      local var
      for var
      do
        eval echo -n $var=\\\\\\\'\\$$var\\\\\\\' | cat -A
        echo
      done'
    )
    function(:member, 'eval echo :"\$$2": | grep ":$1:" >&/dev/null')
    function(:rmpath, "#{self.path_varname}=`echo :$#{self.path_varname} | sed \"s~:$1~~; s~^:~~\"`")
    function(:setenv, "local _label=$1; shift; export $_label=\"$@\"")

    function(:exportDefault, "eval : \\${$1:='$2'}; export $1")
    labeledMacro(:exportDefault)
  end

  def emit_setenv(label, value)
    self.emit_set(label,value) + "; export #{label}"
  end

  def emit_function(label, code)
    "#{label} () {\n#{"\t" unless code[0] == ?\t}#{code}#{"\n" unless code[-1] == ?\n}}\n\n"
  end

  def emit_no_op(code)
    #$stderr.puts self.no_op_command
    #$stderr.puts code
    #$stderr.flush
    "#{self.no_op_command}#{code}"
  end

  def emit_shell_case(value)
    "case #{value}\nin"
  end

  def emit_shell_when(value)
    "#{value})\n\t"
  end

  def emit_shell_nextwhen(value)
    answer = ';;'
    answer << "\n" + self.emit_shell_when(value) unless value.empty?
    answer
  end

  def emit_shell_case_else
    self.emit_shell_when(:*)
  end

  def emit_shell_endcase
    'esac'
  end

  def emit_shell_if(cond)
    "if [ #{cond} ]; then"
  end

  def emit_shell_not_if(cond)
    "if [ ! #{cond} ]; then"
  end

  def emit_shell_if_exists(varname)
    self.emit_shell_if("-f #{varname}")
  end

  def emit_shell_if_dir_exists(varname)
    self.emit_shell_if("-d #{varname}")
  end

  def emit_shell_not_exists(varname)
    self.emit_shell_not_if("-f #{varname}")
  end

  def emit_shell_elsif(cond)
    "el" + self.emit_shell_if(cond)
  end

  def emit_shell_endif
    'fi'
  end

  def appendListFunctionCallFor(varname)
    Array.join {|code|
      #code << '_' if self.ignoreDuplicatesList.include?(varname)
      code << "appendToList #{varname} #{args}"
      if varname == :PATH
        code << "; echo #{code}; echo PATH=; echoList $PATH" if false
      end
    }
  end

  def appendListFunction(name, varname)
    function(name, appendListFunctionCallFor(varname))
    super
  end

  def prependListFunction(name, varname)
    function(name, appendListFunctionCallFor(varname).sub('append', 'prepend'))
    super
  end

  def concatWithVar(prepend=false)
    newValue=['\\${$var}', args]
    newValue.reverse! if prepend
    "local var=$1; shift; eval \"$var=#{newValue}\""
  end

  def concatWithList(prepend=false, check_inlist=false)
    newValue= if prepend
      ['$each', '\\${$var:+:\\$$var}']
    else
      ['\\${$var:+\\$$var:}', '$each']
    end
    code = ['local each var=$1; shift']
    if cygwin
      code << %Q<if [ "$var" = "CLASSPATH" ]#{statement_sep}then
		\t\tfor each#{statement_sep}do
			#{'member $each $var || ' if check_inlist}eval "$var=\\"#{newValue.to_s.sub(/(:.*):/, '\1;')}\\""
		done
	else>
    end
    code << %Q<for each#{statement_sep}do
			#{'member $each $var || ' if check_inlist}eval "$var=\\"#{newValue}\\""
		done>
    code << "\tfi" if cygwin
    code.join($/)
  end

  def path_varname
    :PATH
  end

end

# ================================================
class ScriptMaker_ksh < ScriptMaker_sh
  def generateHeader
    super
    function(:removeFromList,
         'local each var=$1; shift
	  for each; do
	    eval "$var=\"\${$var//:\$each:/:}\""
          done')
  end

#  def emit_setenv(label, value)
#    'export ' + self.emit_set(label, value)
#  end
end

# ================================================
class ScriptMaker_bash < ScriptMaker_ksh
=begin
# bash sets the $- variable, and puts 'c' in it if it is a non-interactive
# shell
# For other shells, I assume $- is not available
if [ "$SHELL" = "/bin/bash" ]; then
  if echo $- | grep 'c' >/dev/null; then
    return  #non-interactive
  fi
fi
=end

  def bindkey_cmd
    :bind
  end

  def if_login
    any "shopt -q login_shell\nif [ $? -eq 0 ]"
  end

  def if_not_login
    any "shopt -q login_shell\nif [ $? -ne 0 ]"
  end

  def statement_sep
    '; '
  end
end

# ================================================
class ScriptMaker_zsh < ScriptMaker_ksh
  def generateHeader
    super
    #self.ignoreDuplicatesInList(:CDPATH, :FPATH, :MANPATH, self.path_varname)
    delimitedMacro(:autoload)
  end

  def ignoreDuplicatesInList(*names)
    super
    puts("typeset -U #{names.join(' ')}")
  end
  
  def bindkey(action, comment, default_keystroke='', &block)
    self.emitCode(block, default_keystroke) {|keystroke|
      if keystroke
        switches  = keystroke.slice!(/(-\w+ *)*/)
        keystroke = "\"#{keystroke}\" " unless keystroke.empty?
      end
      self.emit_bindkey(:action => action,
                        :comment => comment,
                        :keystroke => keystroke,
                        :switches => switches)
    }
  end
  
  def bindkey_cmd
    :bindkey
  end

  def emit_setenv(label, value)
    'export ' + self.emit_set(label, value)
  end

  def stringColorPattern
    "(%{)?#{super}(%})?"
  end
  
  def statement_sep
    '; '
  end

  def if_login
    any "shopt -q login_shell\nif [ $? -eq 0 ]"
  end

  def if_not_login
    any "shopt -q login_shell\nif [ $? -ne 0 ]"
  end
end

# ================================================
class ScriptMaker_csh < ScriptMaker_Possibly_Interactive
  attr_accessor :lowercase_varnames

  def bindkey_cmd
    :bindkey
  end

  def self.no_op_command
    'true '
  end

  def prompt_name
    :prompt
  end

  def path_varname
    :path
  end

  def initialize(*args)
    super
    self.lowercase_varnames=[:cdpath, :path, :prompt]
  end

  def generateHeader
    super
    function(:echoVar,  'echo -n \$; (env | grep -i "^\!^=") || (set | grep -i "^\!^[^A-Za-z0-9]") || (echo \!^=)')
    function(:member,   'echo - ${\!:2} - | grep " \!:1 " >&/dev/null')
    function(:rmpath,   'echo rmpath not yet implemented for csh')
    self.ignoreDuplicatesInList(:path)
    self.ignoreMethod(:bindkey) unless linux or cygwin
    delimitedMacro(:unsetenv)
  end

  def bindkey(action, comment, default_keystroke='', &block)
    return super unless linux
    self.emitCode(block, default_keystroke) {|keystroke|
      if keystroke
        switches  = keystroke.slice!(/(-\w+ *)*/)
        keystroke = "\"#{keystroke}\" " unless keystroke.empty?
      end
      self.emit_bindkey(:action => action,
                        :comment => comment,
                        :keystroke => keystroke,
                        :switches => switches)
    }
  end

  def ignoreDuplicatesInList(*names)
    super
    names.each {|name|
      any "set #{'-f ' if cygwin or linux or (sunos and ! vsnt)}#{name}=($#{name})"
    } unless sunos
  end

  def args
    '\!*'
  end

  def args
    unless @args
      @args = '\!*'
      def @args.[](num)
        '\!:' + num.to_s
      end
    end
    @args
  end

  def defined?(varname)
    "$?#{varname}"
  end

  def not_defined?(varname)
    '! ' + self.defined?(varname)
  end

  def appendListFunction(name, varname)
    if varname == self.path_varname.to_s
      function(name, "set path=($path #{args})")
    else
      function(name, "#{'eval' if csh} setenv #{varname} \"`$HOME/bin/echoMaybeAppendPath.sh 1 $#{varname} #{args}`\"")
    end
    super
  end

  def prependListFunction(name, varname)
    if varname == self.path_varname.to_s
      function(name, "set path=(#{args} $path)")
    else
      function(name, "setenv #{varname} \"`$HOME/bin/echoMaybeAppendPath.sh 0 $#{varname} #{args}`\"")
    end
    super
  end

  def exportDefault(label, default=nil, &block)
    self.emitCode(block, default) {|value| self.emit_exportDefault(label, value)}
  end

  def emit_exportDefault(label, value)
    "if (! $?#{label}) setenv #{label} #{value}"
  end

  def maybeDowncase(string)
    lc_string=string.downcase
    return lc_string if self.lowercase_varnames.include?(lc_string.to_sym)
    string
  end

  def emit_set(label, value)
    'set ' + super(self.maybeDowncase(label.to_s), value)
  end

  def emit_setenv(label, value)
    self.emit_set(label, value).sub(/^set /, 'setenv ').sub(/=/, ' ')
  end

  def emit_shell_alias(label, value)
    super.sub(/^alias\s+(\S+)=/, 'alias \'\1\' ')
  end
  alias :emit_function :emit_shell_alias

  def emit_shell_case(value)
    "switch (#{value})"
  end

  def emit_shell_when(value)
    "case #{value}:\n\t"
  end

  def emit_shell_nextwhen(value)
    answer = 'breaksw'
    answer << "\n" + self.emit_shell_when(value) unless value.empty?
    answer
  end

  def emit_shell_case_else
    self.emit_shell_when(:default)
  end

  def emit_shell_endcase
    'endsw'
  end

  def emit_shell_if(cond)
    "if ( #{cond} ) then"
  end

  def emit_shell_not_if(cond)
    "if ( ! #{cond} ) then"
  end

  def emit_shell_if_exists(varname)
    self.emit_shell_if("-e #{varname}")
  end

  def emit_shell_not_exists(varname)
    self.emit_shell_not_if("-e #{varname}")
  end

  def emit_shell_elsif(cond)
    'else ' + self.emit_shell_if(cond)
  end

  def emit_shell_endif
    'endif'
  end

  def shellvar_eql?(varname, value)
    "\"$#{varname}\" == \"#{value}\""
  end

  def shellvar_not_eql?(varname, value)
    "\"$#{varname}\" \!= \"#{value}\""
  end

  def concatWithVar(prepend=nil)
    newValue=['${\!:1}', '\!:2'] # maybe insert single-space string as 2nd value
    newValue.reverse! if prepend
    "setenv \\!:1 #{newValue}"
  end

  def concatWithList(prepend=false, check_inlist=false)
    newValue=['${\!:1}', '\!:2']
    newValue.reverse! if prepend
    "#{'member \!:2 \!:1 && ' if check_inlist}setenv \\!:1 \"#{newValue.join(' ')}\""
  end

  def statement_sep
    '; '
  end

  def if_login
    # this logic may not be correct
    shell_if("! $?_#{' | $?loginsh ' if linux}")
  end

  def if_not_login
    shell_if("$?_#{' && ! $?loginsh ' if linux}")
  end
end

# ================================================
class ScriptMaker_tcsh < ScriptMaker_csh
end

# ================================================
class ScriptMaker_screen < ShellScriptMaker
  attr_accessor :screens_created
  legalize :concatWithList, :function, :concatWithVar

  def default_term
    'screen_term'
  end

  def stuff(string)
    'stuff "' + string + '"'
  end

  def stuffln(string)
    self.stuff(string.to_s + '^M')
  end

  def term=(a_term)
    if a_term == 'screen'
       super(self.default_term)
    else
      super
    end
  end

  def screens_created
    @screens_created
  end

  def initialize(*args)
    super
    @screens_created = 0
  end

  def new_screen(*args, &block)
    self.emitCode(block) {|value|
      # if screen number given, move it to the end of the options
      args.push(args.shift) if args[0].kind_of?(Integer)

      args.unshift(:screen)
      args << value unless value == true
      args.join(' ')
    } && @screens_created += 1
  end

  def generateHeader
    super
    [ :bind,
      :caption,
      :chdir,
      :defmonitor,
      :defscrollback,
      :hardstatus,
      :ignorecase,
      :markkeys,
      :shell,
      :startup_message,
      :shelltitle,
      :unsetenv].each {|e| delimitedMacro e}
  end

  def emit_set(label, value)
    "#{label} #{value}"
  end

  def emit_setenv(*args)
    'setenv ' + self.emit_set(*args)
  end

  def bindkey_cmd
    :bindkey
  end

  def bindkey(action, comment, default_keystroke='', &block)
    self.emitCode(block, default_keystroke || '') {|keystroke|
      if keystroke and not keystroke.empty?
        keystroke = '"' + keystroke + '"' unless keystroke =~ /^[-\"\']/
        keystroke += ' '
      end
      self.emit_bindkey(:action => action,
                        :comment => comment,
                        :keystroke => keystroke,
                        :switches => '')
    }
  end

end

# ================================================
# ================================================
# ================================================
# ================================================


# If this is run as stand-alone script
if __FILE__ == $0 # ////////////////////////////////////////////

require 'extn/Options'

headerString = <<HEADER_STRING
Usage: #{File.basename($0)} [options...] [input_file]

Processes shell script input_file (or STDIN) and sends to STDOUT a version
customized for a particular machine environment. The default is the current
environment; use the switches below to override as necessary.
HEADER_STRING

switchString = <<SWITCH_STRING
   -d, --demo           Demonstrates this utility.
   -g, --login=false    Declare whether this is a login shell.
   -h, --help           Displays this help text.
   -i, --info           Display summary info on STDERR (useful when redirecting
                        STDIN)
   -l, --legalize='a b' Declare tags 'a' and 'b' to be legal, and therefore pre-
                        vent an error message. Normally, an unrecognized tag
                        (like the uname of some OS other than the local one) is
                        assumed legal but not immediately applicable, and is
                        ignored. A warning is printed to let you know that this
                        has happened. Declare here any tags for which you do
                        not want to see a warning.
   -o, --host=name      Specify the host name, default is \`hostname\`.
   -s, --shell=name     Specify the shell, default is \$SHELL.
   -t, --term=name      Specify the terminal, default is \$TERM.
   -u, --uname=name     Specify the uname, default is \`uname\`.
SWITCH_STRING

tailString = <<TAILSTRING

Report bugs to <paul_schaaf@yahoo.com>.
TAILSTRING

options = ExtOptions.new(headerString, switchString, tailString)
options[:shell].sub!(/^\./, '') if options[:shell]
maker=ShellScriptMaker.readFrom(options)
maker.showInfo=options[:info] || $DEBUG

if options[:demo]
  pos= DATA.pos
  puts <<OUTPUT
# =============================== Demo of #{File.basename($0)}

# =============================== Demo input
#{DATA.readlines}

# =============================== Demo output
OUTPUT
  DATA.pos= pos
  maker.process(DATA)
else
  maker.process(*($*))
end

end

#__END__
# Each built-in function will output the proper syntax for the destination
# shell. code, comment, and unset take a variable number of arguments.

# 'code' passes each string verbatim. Note how the Ruby custom single-quote
# delimiters \%q followed by the delimiter (angle brackets and exclamation
# points are shown) save us the trouble of escaping the embedded quotes as we
# must do in the third term (terms 1 and 2 are on the same line).
#any %q<stty erase '^H' kill '^U' intr '^C'>, %q!stty eof '^D' eol '^-'!,
      'stty  eol2 \'^-\' stop \'^-\''

#unset :FOO, :BAR

# 'comment' makes each arg string a separate commented line. Note that normal
# comments in the source file--such as this one--are not put in the
# destination file.
#comment "Remove '.' (current directory)", 'from path statement.'

# Names of shells, hosts, terminals, unames, etc can be used as boolean
# specifiers for conditional execution
#set    :PATH_COLOR, '$YELLOW_BOLD'    # works everywhere...
#setenv :PATH_COLOR, '$GREEN' if hp_ux # but is overridden when in hpux...

#if zsh                                # or when in zsh
#  set :PATH_COLOR,  '$RED'
#  set :PATH_COLOR2, '$BLUE'
#end

# The following commands accept an optional block:
#   any, exec, function, set, setenv, shell_alias
# Each line with a valid condition is evaluated. The *last* valid one wins,
# so in the example below a specialization for bash should be specified last,
# otherwise it would be overridden by the one for sh. Use the condition 'any'
# to name a default value.
#any {
#  any 'In a shell';    sh  'In sh'
#  csh 'In csh';        zsh 'In zsh'
#}

# Multiple conditions on the same line are OR'd.
#shell_alias(:la) {
#  any          'l -Fa'
#  cygwin linux 'l -FAC --color=auto'
#}

# The 'any' condition can also be supplied as an argument, though its explicit
# use is preferred for clarity. This is equivalent to the example above:
#shell_alias(:la, 'l -Fa') {
#  cygwin 'l -FAC --color=auto'
#  linux  'l -FAC --color=auto'
#}

# Here's a third way to do it, except in this case linux and cygwin files would
# contain *both* of the definitions below. This is harmless unless the first
# entry has side effects, but again, the first format ('any') is preferred.
#shell_alias :la, 'l -Fa'
#shell_alias :la, 'l -FAC --color=auto' if cygwin || linux
=begin
=end
