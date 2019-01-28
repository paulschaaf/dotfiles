#!/usr/bin/env ruby

=begin comment =================================================
Version:
Created:
Updated:
Copyright: Copyright 2001-2002
License: GPL
Description:
Usage:
Requirements:
    Ruby (Tested with Ruby 1.6.7)
Known Problem:
Links:
    Ruby:   http://www.ruby-lang.org/
History:
Things to do:
=end comment ===================================================

# //////////////////////////////////////////////////////////////
# require statements
require 'extn/Win32ole'
#require 'cygwin'
require 'extn/Enumerable'
require 'Proxy'

# //////////////////////////////////////////////////////////////
class Outlook < WIN32App

	# in Outlook there are several
	#def documentClass
	#	Presentation
	#end

	# in Outlook there are several
	#def basicOpenFile(aPath)
	#	self.Presentations.open(aPath)
	#end

	def restoreWindow
	end
	def minimize
	end
	def maximize
	end
end

# //////////////////////////////////////////////////////////////
# //////////////////////////////////////////////////////////////
# If this is run as stand-alone script
if __FILE__ == $0
require 'extn/Options'

headerString = <<HEADER_STRING
Usage: #{File.basename($0)} [option]... [files]...

Command-line interface to MS Outlook.
HEADER_STRING

switchString = <<SWITCH_STRING
Reporting:
   -h, --help              Show this help text

Action:
   -c, --close             Close running instance of application
   -o, --open              Open the named files
SWITCH_STRING

tailString = <<TAILSTRING
Report bugs to <paul_schaaf@yahoo.com>.
TAILSTRING

options = ExtOptions.new(headerString, switchString, tailString)

options.exitShowUsage if options.empty?

files= (ARGV.collect {|filespec| Dir[filespec]}).flatten.sort

if options[:close]
	Outlook.close
end

end # if __FILE__ == $0
