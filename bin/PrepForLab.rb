#!/usr/bin/env ruby

# $Source: e:/MyDocuments/cvsroot/src/Makefile,v $
# $Revision: 1.1 $
# $Date: 2003/08/30 00:16:28 $
# $Author: pschaaf $
# $State: Exp $
# $Name:  $

# //////////////////////////////////////////////////////////////
require 'extn/Numeric'
require 'extn/Array'
require 'extn/Range'
require 'extn/File'
require 'extn/Module'
require 'debugMethod'

# if /cygwin/ =~ RUBY_PLATFORM
#   # Cygwin
#   $line_end = $/ #"\r\n"
# else
#   # UNIX and other platform
#   $line_end = $/
# end

class Numeric
  foo = "clm-123"
  alias_method :solution, :slightlyMore
end

class Object
  def infinity
    9999999
  end
end

class PrepForLab

  attr_accessor :fileName, :fileContents, :labNumber, :hereDocumentStart, :hereDocumentEnd

  def PrepForLab.for(fileName)
    properClass = case File.extension(fileName).downcase
                  when 'java'; 	PrepJavaForLab
                  when 'xml'; 	PrepXMLForLab
                  when 'jsp'; 	self
                  when 'jvi'; 	self
                  when 'html';	self
                  else 		PrepUnknownForLab
                  end
    properClass.new(fileName)
  end

  #//////////////////////////////////////////////////////////////////////////

  def comment(labRange, string)
    show(labRange, makeComment(string))
  end

  def show(labRange, string, elseString='')
    if labRange === self.labNumber
      string.chomp
    else
      elseString.chomp
    end
  end

  def solution(labNumber, sol)
    if sol
      self.show(labNumber.solution.andUp, $/ + sol)
    else
      ''
    end
  end

  # show this as a comment in the lab and lab solution
  def problem(labNumber, problemStatement, solution=nil)
    # note that the endpoint is excluded
    comment(labNumber...labNumber.succ, problemStatement) +
      self.solution(labNumber, solution)
  end

  def suggestion(labNumber, code)
    self.show(labNumber, code)
  end

  def debug
    @fileContents
  end

  #//////////////////////////////////////////////////////////////////////////
  # Markup

  def markupHead
    '#\{'
  end
  def markupHeaderPreComment
    markupHead + '(\\.|<[^<]|[^"\'<])*'
  end
  def markupHeader
    markupHeaderPreComment + '("|' + hereDocumentStart + $/ + ')'
  end
  def markupTail
    # doesn't allow for char '}' to be embedded in string unless escaped
    '(\\.|[^}])*}'
  end
  def entireMarkup
    '(' + markupHead + markupTail + ')'
  end
  def markedUp?
    @fileContents =~ self.markupHead
  end

  def commentHead
    '<!--'
  end
  def commentTail
    '-->'
  end
  def commented?(aString)
    aString.strip =~ Regexp.escape(commentHead)
  end

  #//////////////////////////////////////////////////////////////////////////

  def makeComment(aString)
    # leave alone if already commented
    return aString if commented?(aString)
    commented = aString.gsub(/^(\s*)(.*)/m,  '\1' + commentHead + '!!! \2')
    commented.chomp.chomp + ' ' + commentTail
  end

  def uncommentMarkup(commentHead_r = Regexp.escape(commentHead), commentTail_r = Regexp.escape(commentTail))
    # strip comment marks immediately surrounding replacement text
    self.fileContents= self.fileContents.gsub('(' + commentHead_r + ')?' + entireMarkup + '(' + commentTail_r + ')?', '\2')

    # remove trailing comment close from opening here-document tags
    self.fileContents= self.fileContents.gsub(hereDocumentStart + '[\t ]*' + commentTail_r, hereDocumentStart)

    # remove comment marks from around trailing here-document tags
    self.fileContents= self.fileContents.gsub('(' + commentHead_r + '[\t ]*)?' + hereDocumentEnd + '}(' + commentTail_r + ')?', hereDocumentEnd + '}')
  end

  def initialize(fileName)
    self.fileContents= File.open(fileName, 'r').read
    self.fileName= fileName
    self.hereDocumentEnd= 'CODE'
    self.hereDocumentStart= '<<' + hereDocumentEnd

    begin
      self.uncommentMarkup

      # move trailing here-document tags to left margin
      self.fileContents= self.fileContents.gsub('^[\t ]*(' + hereDocumentEnd + '})', '\1')

      # move leading indentation into quoted strings
      #self.fileContents= self.fileContents.gsub('^([\t ]*)(' + markupHead + '[^"\n\r]*")', '\2\1')
      #self.fileContents= self.fileContents.gsub('^([\t ]*)(' + markupHead + '[^\'\n\r]*\')', '\2\1')

      # move here document markup headers to left margin
      self.fileContents= self.fileContents.gsub('^[\t ]*(' + markupHeaderPreComment + hereDocumentStart + ')', '\1')

      # move leading whitespace into markup text, since text is not always shown
      self.fileContents= self.fileContents.gsub('(' + $/ + '[\t ]*)(' + markupHeader + ')', '\2\1')

      # Replace each backslash with three so that we don't strip them out during processing. Ideally, this should only be done within strings.
      self.fileContents= self.fileContents.gsub(/\\/, '\\\\\\')

      # make the whole thing a here document
      self.fileContents= "<<_____SOURCE_FILE" + $/ + self.fileContents + $/ + "_____SOURCE_FILE" + $/
    rescue
      $stderr.puts "Failed processing #{self.fileName}"
      raise
    end
  end

  def descriptionForLab(labNumber)
    versionDesc = commentHead + ' Versant Corporation, Lab ' + labNumber.to_i.to_s
    versionDesc << ' solution' unless labNumber.floor == labNumber
    versionDesc + ' ' + commentTail + $/
  end

  def prepFor(labNumber)
    @labNumber = labNumber
    begin
      self.descriptionForLab(labNumber) + eval(@fileContents)
    rescue
      $stderr.puts "Failed processing #{@fileName} for lab ##{@labNumber}"
      raise
    end
  end

end

class PrepXMLForLab<PrepForLab
  def prepFor(labNumber)
    # move the header comment to after the xml version number
    super.to_a.swap!(0,1).to_s
  end
end

class PrepJavaForLab<PrepForLab
  def commentHead
    '/*'
  end
  def commentTail
    '*/'
  end
  def commented?(aString)
    super or aString.strip =~ '//'
  end

  def uncommentMarkup
    super
    super('//', '(//)?')
  end
end

class PrepUnknownForLab<PrepForLab
  def commentHead
    ''
  end
  def commentTail
    ''
  end
  def commented?(aString)
    false
  end

  def uncommentMarkup
    # do nothing
  end
end

# //////////////////////////////////////////////////////////////
# If this script is run as stand-alone script
if __FILE__ == $0 # ////////////////////////////////////////////

require 'extn/Options'

headerString = <<HEADER_STRING
Usage: #{File.basename($0)} [option]... [files]...
HEADER_STRING

switchString = <<SWITCH_STRING
  -d, --debug        Dump debug info
  -n, --labNum=N     Number of the lab
  -o, --overwrite    Overwrite original file
  -h, --help         Show this help text
  -e, --example      Show some example code
SWITCH_STRING

exampleString = <<EXAMPLE_STRING
Example Marked-up Java code:

   /**
   /*  A single string is displayed within the specified range. The second
   /*  optional string is displayed whenever the first is not. Note that
   /*/ only the last string may be specified using a "here document".

   //\#{show 2.andUp, 'System.out.println("I am in lab 2 or higher.");',
   <<CODE
   System.out.println("I am before lab 2.");
   //CODE\}


   //***
   //*** Show in lab n where 0<=n<=6.

   /*\#{show 0..6, <<CODE
   Vector departments = new Vector();
   Vector employees = new Vector();
   CODE\}*/


   //***
   //*** Show in lab n where 0<n<6. While '6.solution' could also be written
   //*** as '6.slightlyMore', the former is preferred for readability.

   /*\#{show 0.solution..6.slightlyLess, <<CODE
   Vector departments = new Vector();
   Vector employees = new Vector();
   CODE\}*/


   //***
   //*** Show the problem string for any lab n where 8<=n<9, typically both
   //*** lab 8 and lab 8.5 (the lab8 solution file). It is automatically
   //*** commented. The optional second string is considered the solution
   //*** and will be shown when n>8.

   //\#{problem 8, "Add the locking property", <<CODE
   Properties props = new Properties();
   props.put("database", database);
   props.put("options", (Integer.toString(Constants.OPT_LK)));
   //CODE\}


   //***
   //*** This is equivalent to the 'problem' syntax shown above. It allows
   //*** greater control at the expense of readability.

   //\#{comment 8..9.slightlyLess, "Add the locking property"}
   //\#{show 8.solution.andUp, <<CODE
   Properties props = new Properties();
   props.put("database", database);
   props.put("options", (Integer.toString(Constants.OPT_LK)));
   //CODE\}

EXAMPLE_STRING

tailString = <<TAILSTRING
Report bugs to <pschaaf@versant.com>.
TAILSTRING

options = ExtOptions.new(headerString, switchString, exampleString + tailString)
options.exitShowUsage if options.empty?

files = if ARGV.empty?
          Dir["*"]
        else
          ARGV.collect {|filespec| Dir[filespec]}.flatten
        end

if files.empty?
  $stderr.puts "No files specified!"
  exit 1
end

files.each { |file|
  begin
    prepper = PrepForLab.for(file)
    $stderr.puts "Processing #{file}."
    if options[:debug]
      puts prepper.debug
    else
      answer = prepper.prepFor(options[:labNum].to_f)

      # Overwrite only marked-up files
      if options[:overwrite]
        File.open(file, 'w').write(answer) if prepper.markedUp?
      else
        puts answer
      end
    end
  rescue
    $stderr.puts "Error processing #{file}."
    raise
  end
}

end # //////////////////////////////////////////////////////////
