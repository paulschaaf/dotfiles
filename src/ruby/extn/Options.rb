#!/usr/bin/env ruby

require 'getoptlong'  # Library for handling command line options
require 'debugMethod'
require 'extn/Enumerable'
#require 'tempfile'

# //////////////////////////////////////////////////////////////
class Switch
	attr_reader :arg
	attr_accessor :shortName, :longName, :desc, :argType, :present

	def Switch.readSwitchOrNilFrom(aSwitchString)
		sw = Switch.readFrom(aSwitchString)
		return nil if (sw.shortName.empty? and sw.longName.empty?)
		sw
	end
	def Switch.readFrom(aSwitchString)
		# short switch ends with comma
		shortName = aSwitchString.slice!(/^\s*-\w,/).to_s
		shortName.chop!

		# long switch ends with non-char or digit
		longName = aSwitchString.slice!(/^\s--\w+/).to_s

		# arg begins with equals, optarg is same, but surrounded by brackets
		if (arg = aSwitchString.slice!(/^\[=.+\]/))
			argType = GetoptLong::OPTIONAL_ARGUMENT
		elsif (arg = aSwitchString.slice!(/^=.+/))
			argType = GetoptLong::REQUIRED_ARGUMENT
		else
			arg = nil
			argType = GetoptLong::NO_ARGUMENT
		end

		Switch.new(shortName, longName, arg, argType, aSwitchString)
	end
	def initialize(shortName, longName, arg, argType, desc)
		self.shortName= shortName
		self.longName= longName
		self.arg= arg
		self.argType= argType
		self.desc= desc
		self.present= false
	end
	def arg=(aValue)
		@arg = aValue.to_s
	end
	def switchNames
		self.shortName + ',' + self.longName
	end
	def to_s
		self.switchNames + self.arg + self.desc
	end
end

class ExtOptions
        include Enumerable
	attr_accessor :headerString, :footerString
	attr_reader   :switches, :switchesString

	def initialize(aHeaderOrSwitchesString, aSwitchesString='', aFooterString='')
		if aSwitchesString.empty?
			self.headerString= ''
			self.switchesString= aHeaderOrSwitchesString
		else
			self.headerString= aHeaderOrSwitchesString
			self.switchesString= aSwitchesString
		end
		self.footerString= aFooterString
		self.setSuppliedOptions
	end
	def exitShowUsage
		puts self.usageString
		exit
	end
	def usageString
		['', self.headerString, self.switchesString, self.footerString].join("\n")
	end
	def switchesString=(aString)
		@switchesString= aString
		@switches= []
		self.addSwitchesFrom(aString)
	end
	def addSwitch(aSwitch)
		self.switches << aSwitch if aSwitch
	end
	def addSwitchesFrom(switchString)
		switchString.split('\n').each do |aLine|
			self.addSwitch(Switch.readSwitchOrNilFrom(aLine))
		end
	end
	def each_pair(&block)
	  self.suppliedOptions.each_pair(&block)
	end
	def delete(*args, &block)
	  self.suppliedOptions.delete(*args, &block)
	end
	def[](aValue)
		(self.suppliedOptions)[aValue]
	end
	def[]=(aKey, aValue)
		(self.suppliedOptions)[aKey]=aValue
	end
	def switchesAndArgsDo(&proc2)
		self.suppliedOptions.each(&proc2)
	end
	def empty?
		self.suppliedOptions.empty?
	end
	def debugString
		answer = ''
		self.switchesAndArgsDo {|switch, arg|
			answer << "switch #{switch.inspect} = #{arg.inspect}\n"
		}
		answer
	end
	def suppliedOptions
		return @suppliedOptions if @suppliedOptions
		self.setSuppliedOptions
		@suppliedOptions
	end
	def setSuppliedOptions
		begin
			suppliedOptions = Hash.new
			getoptArgs = self.switches.collect {|aSwitch|
				[aSwitch.longName.strip, aSwitch.shortName.strip, aSwitch.argType]
			}
			options = GetoptLong.new(*getoptArgs)
			options.each do |name, arg|
				key = name.delete!('-').to_sym
				#value = arg || true
				suppliedOptions[key] = arg #value
			end
		rescue GetoptLong::InvalidOption, GetoptLong::MissingArgument => opt
			$stderr.puts self.usageString
			exit 1
		end
		if suppliedOptions[:help]
			self.exitShowUsage
		end
		@suppliedOptions= suppliedOptions
	end
end


# //////////////////////////////////////////////////////////////
# If this script is run as stand-alone script
if __FILE__ == $0 # ////////////////////////////////////////////

header = <<HEADER
Usage: #{File.basename($0)} [OPTION]... [FILE]...
Provides a library that wraps GetoptLong with a nicer interface. This is not
normally called from the command line.

What follows are some sample switches that are provided to test the inteface of
this library. Supplied switches should be echoed, along with any arguments.
Missing or extra arguments will raise an exception.
HEADER

usage = <<USAGE
  -h, --help          Use these lines...
  -a, --alpha         to document each switch.
  -b, --bravo         Use spaces to separate the columns.
  -c, --charlie
  -d, --delta
  -e, --echo[=FOO]    FOO is an optional argument, default is empty string.
  -f, --foxtrot=BAR   BAR is a required argument.
USAGE

tail = <<TAIL
Arguments can be reached programmatically using the element reference operator
'[]', with the long switch symbol as the key. The value is the empty string,
the supplied argument string, or nil if the switch was not specified. For exam-
ple, the command

    opts = #{File.basename($0)} -b --echo --foxtrot=hello

would set elements as follows:

    opts[:bravo]= ''
    opts[:echo]= ''
    opts[:foxtrot]= 'hello'
    opts[:alpha]= nil

Report bugs to <paul_schaaf@yahoo.com>.
TAIL

opt = ExtOptions.new(header, usage, tail)

puts opt.usageString
puts
puts '--------------'
puts opt.debugString
end
