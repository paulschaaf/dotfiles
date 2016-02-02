#!/usr/bin/env ruby

# $Source: e:/MyDocuments/cvsroot/src/Makefile,v $
# $Revision: 1.1 $
# $Date: 2003/08/30 00:16:28 $
# $Author: pschaaf $
# $State: Exp $
# $Name:  $

#require 'debugMethod'
require '/usr/local/bin/rren'
require 'extn/Options'

$defaultInfoFileName = 'CourseInfo.txt'

# //////////////////////////////////////////////////////////////
# DOCUMENTATION
#
headerString = <<HEADER_STRING
Usage: #{File.basename($0)} [-m mapFile] [-x] [files]...
Renames a set of files such that their names sort in order according to the
section map given in infoFile.

By default, this will display the list of renames. Use -x to make them happen.
HEADER_STRING

switchString = <<SWITCH_STRING
   -h, --help               Show this help text
   -f, --showFrom           Show the old name of the affected files
   -t, --showTo             Show the new name of the affected files
   -i, --infoFile=aName     Name of infofile, default is #{$defaultInfoFileName}.
   -x, --execute            Performs the renaming
   -m, --mapFile=aName      (Deprecated) Name of mapfile.
SWITCH_STRING

tailString = <<TAILSTRING
infoFile is a text file with course sections listed in chronological
(presentation) order on a single line that begins with "/Components". For
example, given an infoFile with the line:
	/Components=Intro VDS ODBA

#{File.basename($0)} will rename these files
   SG_ODBA_Intro-01_CorpOverview.pdf
   SG_ODBA_ODBA-01_BuildingADatabase.pdf
   SG_ODBA_VDS-04_BasicVsntDBA.pdf

to
   SG_ODBA_01_Intro-01_CorpOverview.pdf
   SG_ODBA_02_VDS-04_BasicVsntDBA.pdf
   SG_ODBA_03_ODBA-01_BuildingADatabase.pdf

Notice that both lists above are shown sorted, but the second list now is
now also chronological.

mapFile is a text file with the course sections listed in chronological
(presentation) order. If it looks like this:
   Intro
   VDS
   ODBA

Report bugs to <paul_schaaf@yahoo.com>.
TAILSTRING
# //////////////////////////////////////////////////////////////

options = ExtOptions.new(headerString, switchString, tailString)

files = if ARGV.empty?
	Dir["*"]
else
	(ARGV.collect {|filespec| Dir[filespec]}).flatten
end

coursemap = if options[:mapfile]
	$stderr.puts "--mapFile is depreprecated!"
	open(options[:mapFile] || 'CourseMap.txt').readlines
else
	courseInfo = open(options[:infoFile] || $defaultInfoFileName).readlines
	courseInfo.detect {|e| e =~ "^/?Components="}.gsub("^[^=]*=", "").chomp.split
end

p coursemap

counter = '01'
coursemap.each {|aLine|
	renamer = Renamer.new(files)
	renamer.showFrom = options[:showFrom]
	renamer.showTo   = options[:showTo]

	renamer.renameFiles("(#{aLine.chomp}-)", "#{counter}_\\1")

	renamer.print
	renamer.execute if options[:execute]

	counter = counter.succ
}
