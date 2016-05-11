#!/bin/sh
exec scala "$0" "$@"
!#

object FileRenamer {
  def showHelp { showHelp("") }
  def showHelp(msg: String = "") {
  	val usage = msg + """
Usage: rren (--reverseSort | <regexp> <replacement>) [-x] [files]...
Renames a set of files, replacing matches of <regexp> with <replacement>. By
default, it will display the list of renames. Use -x to make them happen.
Special characters can be used in <replacement>:

   \1, \2, ... \9         The value matched by the nth grouped subexpression
   \`                     The part of the string before the match
   \'                     The part of the string after the match
   \&                     The last match
   \+                     The highest-numbered group matched

For example, the command:

   rren '^([0-9][^0-9]+)' '0\1' *

will prepend a zero to every filename that begins with exactly one digit.

NOTE: The --reverseSort switch assumes all other arguments are filename globs.
      Using this switch with a regex will not give the results you want!

Report bugs to <paul.schaaf@gmail.com>.
    -d, --debug                      Display debugging information
    -c, --capitalize                 Change the first letter to uppercase
    -l, --lowercase                  Change the filename to use all lowercase
    -u, --uppercase                  Change the filename to use all uppercase
    -h, --help                       Show this help text
    -f, --showFrom                   Show the old name of the affected files
    -t, --showTo                     Show the new name of the affected files
    -x, --execute                    Performs the renaming
"""
    println(usage)
  }

// optparse.warn() if optparse.getopts.size < 3
/*
#!/usr/bin/env ruby

$ON_WINDOWS = (/(cyg)?win/ =~ RUBY_PLATFORM)

# //////////////////////////////////////////////////////////////
class Renamer

  def self.intermediary
    @@intermediary ||= $ON_WINDOWS ? "___.#$$" : ''
  end

  def intermediary
    @changeCaseFlag ? self.class.intermediary : ''
  end

  attr_accessor :execute, :files, :mapping, :showFrom, :lowercaseNameTerminationMarker
  attr_writer   :debugOn, :showTo

  # Answer the value of showTo. If unset, return true unless showFrom has been specified
  def showTo; @showTo == nil ? (!showFrom) : @showTo; end

  def initialize(anArray=[])
    @files, @mapping = anArray.sort, {}
  end

  def [](key); @mapping[key]; end

  def []=(key, value)
    @mapping[key] = value if value and value != key
  end

  def debugPrint
    p yield if @debugOn
  end

  def lowercase
    @changeCaseFlag = true if $ON_WINDOWS
    self.renameAll(self.files) {|each|
      result = each.downcase
      result << self.intermediary if $ON_WINDOWS && (result != each)
    }
  end

  def capitalize
    @changeCaseFlag = true if $ON_WINDOWS
    self.renameAll(self.files) {|each|
      result = each.upcase
      result << self.intermediary if $ON_WINDOWS && (result != each)
      result
    }
  end

  def uppercase
    @changeCaseFlag = true if $ON_WINDOWS
    self.renameAll(self.files) {|each|
      result = each.upcase
      result << self.intermediary if $ON_WINDOWS && (result != each)
    }
  end

  def reverseSort(separator)
    fieldWidth = Math.log10(files.size + 1).ceil
    index = -1
    self.files.reverse_each {|each|
      self.rename(each, format("%0#{fieldWidth}d%s%s", index+=1, separator, each))
    }
  end

  def rename(from, to=nil)
    self[from] = block_given? ? (yield from) : to
  end

  def renameAll(froms)
    froms.each {|from| self[from] = yield from}
  end

  def renameFiles(from, to)
    from = Regexp.new(from) if from.kind_of?(String)
    targets = self.files.select {|each|
      self.rename(each, each.gsub(from,to)) if each =~ from
    }
    $stderr.puts "No matches for '#{from.source}'." if targets.empty?
    targets
  end

  def evaluate
    announcement = "Renaming files in '#{Dir.pwd}'#{' FROM' if showFrom}#{' TO' if showTo}\n"

    answer = []
    mapping.each_pair {|key, value|
      operation = showFrom ? ($/ + key.to_s) : ''
      operation << $/ << value[0..-(self.intermediary.length+1)] if showTo
      (operation << $/) if showFrom && showTo
      answer << operation
      if execute
        # p 'key = ' + key, 'value = ' + value
        File.rename(key, value)
        # p 'done with initial renaming'
        if $ON_WINDOWS && @changeCaseFlag
          File.rename(value, value[0..-(self.intermediary.length+1)])
        end
      end
    }

    puts announcement, answer.sort.join unless answer.empty?
    puts 'Renaming complete.' if execute
  end
end

*/

//    --debug                      Display debugging information
//    --capitalize                 Change the first letter to uppercase
//    --lowercase                  Change the filename to use all lowercase
//    --uppercase                  Change the filename to use all uppercase
//    --help                       Show this help text
//    --showFrom                   Show the old name of the affected files
//    --showTo                     Show the new name of the affected files
//    --execute                    Performs the renaming

//  trait Switch {
//    def applyTo(str: String): String
//  }
//  trait Capitalize extends Switch {
//    def applyTo(str: String) = {
//      super.applyTo(str)
//    }
//  }
//  case class Debug extends Switch {
//    def applyTo(str: String) = {
//      str
//    }
//  }
//  case class Execute extends Switch {
//    def applyTo(str: String) = {
//      str
//    }
//  }
//  case class Help extends Switch {
//    def applyTo(str: String) = {
//      str
//    }
//  }
//  case class Lowercase extends Switch {
//    def applyTo(str: String) = {
//      str
//    }
//  }
//  case class Showfrom extends Switch {
//    def applyTo(str: String) = {
//      str
//    }
//  }
//  case class Showto extends Switch {
//    def applyTo(str: String) = {
//      str
//    }
//  }
//  case class Uppercase extends Switch {
//    def applyTo(str: String) = {
//      str
//    }
//  }
//
//  object Switch {
////    def parse(argStr: String): (String) => String = argStr match {
////	  //case "--debug" => str => str
////	  case "--capitalize" => str => str capitalize
////	  case "--lowercase" => str => str toLowerCase
////	  case "--uppercase" => str => str toUpperCase
////	  case "--help" => new
////	  case "--showFrom" => new
////	  case "--showTo" => new
////	  case "--execute" => new
////	  case _ => str => str
////    }
//  }
  class Renamer(modifiers: Array[String => String]) {
    val debug: Boolean
  }

  def main(rawArgs: Array[String]) {
    val args = for (rawArg <- rawArgs; arg = rawArg.trim; if arg.nonEmpty) yield arg
    val argMap = Map[String, String]()
    
    if (args.isEmpty) showHelp("Missing arguments!\n")
    else {
      var fromPattern = ""
      var toPattern = ""
      
      val filters = for (arg <- args) yield {
        printf("arg == \"%s\"\n", arg)
        arg match {
	    	  //case "--debug" => str: String => str
          case "--capitalize" => str: String => str capitalize
		  case "--lowercase" => str: String => str toLowerCase
		  case "--uppercase" => str: String => str toUpperCase
	//	    case "--help" => str: String => str
	//	  case "--showFrom" => str: String => str
	//	 case "--showTo" => str: String => str
	//		  case "--execute" => str: String => str
		  case _ => str: String => str
	    }
      }
      val rren = new Renamer(filters)
    }
  }
  
}