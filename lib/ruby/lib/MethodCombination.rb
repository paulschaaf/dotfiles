#!/usr/bin/env ruby

require 'set'
require 'singleton'
require 'debugMethod'

$no_op = lambda {} # NullProc.new

def show(*args)
  $stderr.puts *args
end

module GenericMethods

  def def_before(name, code=nil, &block)
    def_generic(name)
    before_methods[name.to_sym] = if block_given?
                                    block
                                  else
                                    eval('{' + code + '}')
                                  end
  end

  def def_after(name, code=nil)
    def_generic(name)
    @after_methods[name.to_sym]  = (block_given? ? lambda : eval('{' + code + '}'))
  end

  def undef_before(name); before_methods.delete(name); end
  def undef_after(name);  after_methods.delete(name);  end

  def generic_method?(name)
    @generic_methods && generic_method[name.to_sym]
  end

  def def_generic(name)
    return if generic_method?(name)
    next_method_name = "#{name}__next".to_sym
    #alias next_method_name, name
    generic_methods[name] = next_method_name
    show self
    code = "
      def self.#{name}(*args, &block)
        #show 'before before'
        before_methods[:#{name}].call
        #show 'after before'

        answer = super
#self.#{next_method_name}(*args, &block)
        #show 'before after'
        after_methods[:#{name}].call
        #show 'after after'
        answer
      end"
    #show code
    eval code
  end

  def undef_generic(name)
    next_method_name = generic_method?(name)
    if next_method_name
      alias_method name, next_method_name
      undef next_method_name
    end
    undef_before(name)
    undef_after(name)
  end

  protected

  def generic_methods
    @generic_methods ||= Hash.new
  end

  def before_methods
    @before_methods ||= Hash.new($no_op)
  end

  def after_methods
    @after_methods ||= Hash.new($no_op)
  end

#  debug
end

# ================================================
# ======= If this script is run as stand-alone script
if __FILE__ == $0

class String
end

a='hello'
a.extend GenericMethods
a.def_before(:length) {$stderr.puts 'in length\'s before method'; $stderr.flush}

puts a.length

end

# class Proc
#   def call_unless_nil(*args)
#     call(*args)
#   end
# end

# class NilClass
#   def call_unless_nil(*args)
#   end
# end

# class NullProc
#   require Singleton
#   def call(*args); nil; end
# end

#   def initialize
#     no_op = lambda {} # NullProc.new
#     @before_methods  = Hash.new(no_op)
#     @generic_methods = Hash.new
#     @after_methods   = Hash.new(no_op)
#   end

#p String.before_methods
#p String.instance_variable_get("@before_methods")
#('before_methods')

  #String.before(:puts) {|s|

#   def method_added
#     super
#     if generic_method?(name)
#       #redefine generic
#     end
#   end
