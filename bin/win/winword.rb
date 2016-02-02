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
require 'extn/Enumerable'
require 'extn/Numeric'
require 'extn/Win32ole'
require 'Proxy'

# //////////////////////////////////////////////////////////////
class Word < WIN32App

	def documentClass
		Document
	end

	def documents
		self.Documents
	end

	def buildPDFFrom(template, source, dest)
		self.openDoc(source) {|doc|
			self.Selection.WholeStory
			self.Selection.Copy
		}

		self.create(template) {|doc|
			self.cd('`pwd`'.chomp)
			self.Selection.Paste#Special
			self.Selection.WholeStory
			self.Selection.Style= self.ActiveDocument.Styles("Normal")
			self.selection.endKey
			self.selection.insertBreak if doc.numberOfPages.even?
			self.ActivePrinter= "Acrobat PDFWriter"
			`rm -f /homes/pschaaf/courses/Document1.pdf #{dest}`
			self.Application.PrintOut
			#("FileName:="", Range:=wdPrintAllDocument, Item:= wdPrintDocumentContent, Copies:=1, Pages:="", PageType:=wdPrintAllPages, Collate:=True, Background:=True, PrintToFile:=False")
			`mv /homes/pschaaf/courses/Document1.pdf #{dest}`
		}
	end

	def buildIndex(template)
		self.buildPDFFrom(template, 'Index.txt', 'Index.pdf')
	end

	def buildOutline(template)
		self.buildPDFFrom(template, 'Outline.txt', 'Outline.pdf')
	end

=begin
self.openDoc('Index.txt') {|doc|
			self.Selection.WholeStory
			self.Selection.Copy
		}

		self.create(template) {|doc|
			self.cd('`pwd`'.chomp)
			self.Selection.Paste#Special
			self.Selection.WholeStory
			self.Selection.Style= self.ActiveDocument.Styles("Normal")
			self.selection.endKey
			self.selection.insertBreak if doc.numberOfPages.even?
			self.ActivePrinter= "Acrobat PDFWriter"
			`rm -f Document1.pdf Index.pdf`
			self.Application.PrintOut
			#("FileName:="", Range:=wdPrintAllDocument, Item:= wdPrintDocumentContent, Copies:=1, Pages:="", PageType:=wdPrintAllPages, Collate:=True, Background:=True, PrintToFile:=False")
			`mv ../../Document1.pdf Index.pdf`
		}
	end
=end
end

class Document < Win32Doc
end

# //////////////////////////////////////////////////////////////
# //////////////////////////////////////////////////////////////
# If this is run as stand-alone script
if __FILE__ == $0
require 'extn/Options'

headerString = <<HEADER_STRING
Usage: #{File.basename($0)} [option]... [files]...

Command-line interface to MS Word.
HEADER_STRING

switchString = <<SWITCH_STRING
Reporting:
   -h, --help              Show this help text
   -n, --newFrom=template  Create a new document based upon template

Action:
   -c, --close             Close running instance of application
   -d, --docsDo=code       Evaluate the supplied code against each doc
   -o, --open              Open the named files
   -i, --buildIndex=tmpl   Builds the index document using template
   -u, --buildOutline=tmpl Builds the outline document using template
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

if options[:open]
	$app= Word.open
	$app.openDoc(*files)
	$app.restoreWindow
elsif options[:newFrom]
	Word.openWhile {|app| app.create(options[:newFrom])}
elsif options[:buildIndex]
	Word.openWhile {|app| app.buildIndex(options[:buildIndex])}
elsif options[:buildOutline]
	Word.openWhile {|app| app.buildOutline(options[:buildOutline])}
else
	Word.docsDo(files) {|aDocument|
		if options[:docsDo]
			code = 'Proc.new {|each|' + options[:docsDo] + '}'
			eval(code).call(aDocument)
		end
	}
end

end # if __FILE__ == $0
