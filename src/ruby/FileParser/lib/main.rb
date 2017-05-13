#!/usr/bin/env ruby

# module StreamParseNode

QUIZ_FILE = 'c:\\Users\\pschaaf\\lib\\ruby\\FileParser\\lib\\quiz.rb'

require 'logger'

<<EXAMPLE
Question ID 82 : ClaimCenter Business Concepts (wrong)
Which 2 of the following statements are true about the New Claim Wizard in a base (unconfigured) installation of ClaimCenter?

A. You cannot create a Claim against an Unverified Policy
B. A temporary Claim number is generated after completing the first step of either the full or Quick wizards.
C. You can temporarily leave the Wizard to create a different Claim without losing any of your work
D. You can complete the wizard without navigating to any of the independent steps
E. You cannot complete the wizard without navigating through every ordered step


>> Correct answer is: B D
>> Student chose: B C
EXAMPLE

$log = Logger.new(STDERR)
$log.sev_threshold = Logger::INFO # DEBUG
$log.progname = 'QuizParser'


class QuizParser
  attr_reader :quiz_code
  attr_accessor :open_str, :close_str, :pad_size, :indent_char, :indent_level

  def initialize(open_str = "'", close_str = open_str)
    # open_str_map = Hash.new {|hash, miss| hash[miss]= ("%q#{miss}")}
    # open_str_map.merge!({ :')' => :'%q(', :'}' => :'%q{', :']' => :'%q[', :>   => :'%q<', :'"' => '"', :"'" => "'"})
    @indent_char  = ' '
    @indent_level = 0

    @open_str, @close_str = open_str.to_sym, close_str.to_sym
    @quiz_code    = ''
    @pad_size     = 16
  end

  def quiz;    @quiz ||= compile; end
  def compile; eval @quiz_code;   end

  def indent(str_or_arr)
    indenter = lambda {|str| @indent_char * @indent_level + str}
    if str_or_arr.kind_of? Enumerable
      str_or_arr.collect &indenter
    else
      indenter.call(str_or_arr)
    end
  end

  def append_code(*code)
    if code
      $log.debug "PARSED INTO: #{code}"
      @quiz_code << indent(code.join "\n")
    end
    code
  end

  def pad(str)
    str.ljust(@pad_size, ' ')
  end

  def readlines(line_arr)
    append_code <<FILE_HEADER
#!/usr/bin/env ruby

require #{QUIZ_FILE.inspect}
require 'date'

aQuiz = Quiz.new do |q|
FILE_HEADER

    line_arr.each {|line| append_code self.readline line}
    append_code "\naQuiz"
  end

  # Given a line of the file, answer an array of lines of ruby code
  def readline(line)
    line.strip!
    line.gsub! @close_str.to_s, '\\\\' + @close_str.to_s
    $log.debug "PARSING Line>> #{line.inspect}"

    case line
      # Header
    when /^(Course|Title): +(.*)/
      $log.info "FOUND #$1"
      "  q.#{pad($1.downcase)} = #{$2.to_sym.inspect}\n"

      # Student ID and email are symbols
    when /^Student (ID|email): +(.*)/
      $log.info "FOUND Student #{$1}"
      "  q.#{pad('student_'+ $1.downcase)} = #{$2.to_sym.inspect}\n"

      # Name and company
    when /^(Student name|Student company): *(.*)$/
      # save values first because sub() instantly changes $1 and $2!
      prop_name, value = $1.downcase!, $2
      prop_name.sub!(' ', '_')
      $log.info "FOUND #{prop_name}"
      "  q.#{pad prop_name} = #@open_str#{value}#@close_str\n"

      # Exam Date
    when /^Exam Date: +(.*)/
      # The library needs the year to be first
      aDate = $1.split('-').rotate(-1).join('-')
      $log.info "FOUND Date: #{aDate}"
      "  q.#{pad 'exam_date'} = Date.parse('#{aDate}')\n"

      # Score
    when /^Score: +(.*)/
      $log.info 'FOUND Score'
      "  q.#{pad 'score'} = #$1\n"

      # New question
    when /^Question ID (\d+) : (.*) \(.*\)$/
      $log.info "FOUND Question # #$1"
      ["end\n",
       "\n# =========",
       'aQuiz.new_question do |q|',
       "  q.id         = #$1",
       "  q.topic      = #@open_str#$2#@close_str",
       "  q.question   = #@open_str"]

      # End of the question, and first of the selectable choices
    when /^A\. (.*)$/
      $log.info 'FOUND QEnd and First Choice'
      [@close_str,
       "  q.add_choice   #@open_str#$1"]

      # Another selectable choice
    when /^[B-Z]\. (.*)$/
      $log.info 'FOUND Choice'
      [@close_str,
       "  q.add_choice   #@open_str#$1"]

      # The correct answer(s)
    when /^>> Correct answer is: (.*)/
      $log.info 'FOUND Correct Answers'
      [@close_str,
       "  q.answers    = [:#{$1.split.join ', :'}]\n"]

      # The student's answer
    when /^>> Student chose: (.*)/
      $log.info 'FOUND Responses'
      "  q.responses  = [:#{$1.split.join ', :'}]\n"

      # ignore empty and worthless lines
    when /^(|-+ Questions Missed.*|Result:.*|\d+ correct out of .*)$/
      $log.info "IGNORING Line >> #{line.inspect}" unless line.empty?
      nil

      # remove answer count hint
    when /(.*) +\[[0-9]+ answers?\](.*)/
      $log.info 'REMOVING Answer Count'
      "#$1#$2"

      # otherwise keep the line intact
    else line
      $log.info 'KEEPING Intact >> ' + line.inspect
      line + "\n"
    end
  end
end

$parser  = QuizParser.new

# If this script is run as stand-alone script
textfile = if (__FILE__ == $0 and ARGV.first)
  ARGV.first
else
  'c:\\Users\\pschaaf\\lib\\ruby\\FileParser\\lib\\CertQuestions.txt'
end

File.open(textfile, 'r') {|file| $parser.readlines(file.readlines)}

puts $parser.quiz_code if __FILE__ == $0 and ARGV.first


<<EXPERIMENTAL
===============================================================================
class Deserializer
  attr_reader :source
  def initialize(source)
    @source = source
  end

  def discard(match=/.*$/)
    string.slice! match
    this
  end

  def int(field_name)
    int_str = string.slice! /\s*([0-9])+\s*/
    instance_variable_set field_name, int_str.to_i
    this
  end
end

class QuestionDeserializer < Deserializer
end

discard 'QUESTION ID '
int :id
discard %r{ *: *}

string  :topic,    :until => %r{ \(.*}; discard  # strip it
string  :question, :until => %r{\n\n+}; discard %r{\n+}
strings :choices,  :matching => %r{^[A-Z]\. }    # assumes no wrap

topic = string.slice! / \(.*/

discard '>> CORRECT ANSWER IS: '
strings :answers, :delimiter => ' '

discard '>> YOUR ANSWER IS: '
strings :responses, :delimiter => ' '

string :reference
EXPERIMENTAL
