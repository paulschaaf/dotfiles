#!/usr/bin/env ruby

# $Source: e:/MyDocuments/cvsroot/src/Makefile,v $
# $Revision: 1.1 $
# $Date: 2003/08/30 00:16:28 $
# $Author: pschaaf $
# $State: Exp $
# $Name:  $

require 'extn/Options'
require 'extn/Module'
require 'cygwin'

headerString = <<HEADER_STRING
Usage: #{File.basename($0)} [option]... [files]...
Lists classes in target files.
HEADER_STRING

switchString = <<SWITCH_STRING
   -d, --date     Show the date
   -l, --length   Show the length
   -t, --time     Show the time
   -a, --all      Show all columns
   -p, --packages  Show packages only
   -i, --hidden   Show hidden classes (see below)
   -r, --directories Show directories as separate entities
   -o, --other    Show other (non-class) files
   -h, --help     Show this help text
SWITCH_STRING

tailString = <<TAILSTRING
Classes whose name contains a contains a dollar sign ("$") are hidden by
default. Use --hidden to show them.

Report bugs to <pschaaf@versant.com>.
TAILSTRING

options = ExtOptions.new(headerString, switchString, tailString)

files = ARGV.collect {|filespec| Dir[filespec]}.flatten

if ARGV.empty?
	$stderr.puts 'No files specified!'
	options.exitShowUsage
elsif files.empty?
	$stderr.puts 'File(s) not found!'
	options.exitShowUsage
end

onWindows {files = files.collect {|ea| CygPath.new(ea).unxAbsolute}}

files.each { |file|

	if options[:all]
		filterColumns = ''
	else
		filterColumns   = '| awk \'{ print '
		filterColumns += ' $1 "\t" ' if options[:length]
		filterColumns += ' $2,' if options[:date]
		filterColumns += ' $3,' if options[:time]
		filterColumns += ' $4 }\''
	end

	filterRows = ''

	# drop all class files whose name contains the dollar sign '$'
	filterRows += "| grep -v '\$.*\.class$'" unless options[:hidden]
	filterRows += "| grep -v '/$'" unless options[:directories]

	unless options[:other]
		# strip everything not a class, drop the extension, and change directory separators into periods
		filterRows   = "| grep 'class$' | sed 's/\.class$//g' | sed 's~/~\.~g'"

		# remove the classname from the end, leaving the package name
		filterRows += "| sed 's/\.[^.]*$//g'" if options[:packages]
	end

	puts %x(unzip -l #{file} #{filterRows} #{filterColumns} | sort -u)

}
