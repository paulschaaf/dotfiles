#!/usr/bin/env ruby

# //////////////////////////////////////////////////////////////
require 'extn/Numeric'

class Object
  def range?
    false
  end
end

class Range
  def range?
    true
  end

  def toRange
    self
  end

  def exclude_begin
    ExcludeBeginRange.new(self.begin, self.end)
  end
end

class ExcludeBeginRange<Range
  def ===(aValue)
    !(aValue === self.begin) and super(aValue)
  end

  def length
    super - 1
  end

  def to_s
    '(' + super + ').exclude_begin'
  end

  def exclude_begin?
    true
  end
end

# //////////////////////////////////////////////////////////////
# If this script is run as stand-alone script
if __FILE__ == $0 # ////////////////////////////////////////////

puts a = (1..3).exclude_begin
puts a === 1

end # //////////////////////////////////////////////////////////
