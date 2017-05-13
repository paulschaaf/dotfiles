#!/usr/bin/env ruby

class Module
  alias_method :def_method, :define_method
  public       :def_method
end

class Object

  # give the singleton class associated with the receiver
  def_method(:singleton_class) {
    class << self; self end
  }

#   def_method(:def_method, *args, &block) {
#     singleton_class.def_method(*args, &block)
#   end

  # define a method for the receiver only (and not, for example, its class)
  def def_method(*args, &block)
    singleton_class.def_method(*args, &block)
  end

end

class B
  def initialize(keys, obj)
    obj.def_method(:cassette) { keys }
  end
end

a = Object.new
b = B.new([1, 2, 3], a)
puts a.cassette

