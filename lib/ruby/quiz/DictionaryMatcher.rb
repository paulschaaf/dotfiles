#!/bin/env ruby
# http://rubyquiz.com/quiz103.html
#
# Paul Schaaf
# paul.schaaf@gmail.com
#
# use this to experiment in irb:
#    eval File.read('lib/ruby/quiz/DictionaryMatcher.rb')

module Enumerable
  def orExtractSoleMember
    # use to have a method work on a collection or a single object, e.g.
    # 
    self.size == 1 ? self.detect {|e| e} : self
  end
end

class DictionaryMatcher
  include Enumerable

  def self.[](*args)
    (self.new).push(*args)
  end

  def <<(arg)
    self.push(arg)
  end

  def =~(string)
    (mtch = self.match(compile(string))) ? mtch.begin(0) : nil
  end

  alias_method :===, :=~

  def casefold?
    @casefold
  end

  def each(*args, &proc)
    @arr.each(*args, &proc)
  end

  def include?(obj)
    super(compile(obj))
  end

  def initialize(casefold=false)
    @arr, @casefold = [], casefold
  end

  def match(string)
    self.each {|e|
      answer = string.match(e)
      return answer if answer
    }
    nil
  end

  def push(*args)
    @arr.push(*compile(*args))
    self
  end

  def compile(*objs)
    (@casefold ? objs.collect {|e| e.downcase} : objs).orExtractSoleMember
  end
end

# //////////////////////////////////////////////////////////////
if $0 == "irb"
  def freshen
    eval File.read('lib/ruby/quiz/DictionaryMatcher.rb')
  end
end

# creates a new empty matcher
dm = DictionaryMatcher["string", "Ruby"]

dm.include?("Ruby")                # => true
dm.include?("rUby")                # => false
dm.include?("missing")             # => false
dm.include?("stringing you along") # => false

dm =~ "rub you the wrong way"      # => nil
dm =~ "long string"                # => 5
"long string" =~ dm                # => 5

idm = DictionaryMatcher.new(true)
idm << 'string' << 'Ruby'

idm.include?("Ruby")                # => true
idm.include?("rUby")                # => true
idm.include?("missing")             # => false
idm.include?("stringing you along") # => false

idm =~ "rub yOu the wrong way"      # => nil
idm =~ "long sTring"                # => 5
 "long sTring" =~ idm               # => 5
