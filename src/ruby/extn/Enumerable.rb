#!/usr/bin/env ruby

require 'extn/Object'
require 'extn/Numeric'

# //////////////////////////////////////////////////////////////
# class Object
#   def empty?; false; end

#   def not_empty?; ! empty?; end
# end

# //////////////////////////////////////////////////////////////
class NilClass
  def empty?; true; end
end

# //////////////////////////////////////////////////////////////
module Enumerable
  def each_separatedBy(sepBlock)
    # Pass in false initially to identify the first pass, pass
    # true for subsequent executions. This allows the use of 
    # blocks that legitimately return nil or false.
    # Example:
    #    def myjoin(sep)
    #      self.each_separatedBy(lambda {print sep}) {|e| print e.to_s}
    #      puts
    #    end
    #
    #    (1..4).myjoin(', ')   # => 1, 2, 3, 4

    self.inject(lambda {}) {|block, ea|
      block.call
      yield ea
      sepBlock
    }
    self
  end

  def detect_if_none(noneBlock, &block)
    self.detect(&block) || noneBlock.value
  end

  def or_if_empty(value=nil)
    return self unless self.empty?
    value || yield
  end

  def or_nil_if_empty
    self.or_if_empty
  end

  def with(other, &action)
    (0...([self.size, other.size].max)).each {|index|
      action.call(self[index], other[index])
    }
  end

  def combined_with(*args)
    CompositeCollection[self].concat(args)
  end
end

# //////////////////////////////////////////////////////////////
class Array
   def each_separatedBy(sepBlock)
     yield self[0] unless self.empty?
     (1...self.size).each {|idx|
       sepBlock.value
       yield self[idx]
     }
     self
   end

   # Deprecated; use require 'enumerator'; anArray.each_slice(count) {}
   #
   # def each_by(count)
   #   # [0, 1, 2, 3, 4, 5, 6, 7, 8, 9].each_by(3) {|x, y, z| puts z}
   #   start = 0
   #   (self.size/count).times {yield(*self.slice(start, start += count))}
   #   self[start..-1]
   # end
end

# //////////////////////////////////////////////////////////////
class CompositeCollection < Array
  alias :each_component :each

  def each(&block)
    self.each_component {|coll| coll.each(&block)}
  end

  def size
    size = 0
    self.each_component {|coll| size += coll.size}
  end

  def empty?
    self.detect {|e| true}
  end
end

# //////////////////////////////////////////////////////////////
# If this script is run as stand-alone script
if __FILE__ == $0 # ////////////////////////////////////////////

puts 'Testing orIfEmpty...'
puts [].or_if_empty('empty')
puts [].or_if_empty {'empty'}
puts ''.or_if_empty('empty')
puts 'not empty'.or_if_empty('empty')
puts 'not empty'.or_if_empty {'empty'}
puts
puts 'Testing with...'
puts %w(a b c d e).with(10..20) {|first, second| puts "#{first} #{second}"}
puts
cc = CompositeCollection.new
cc << [1, 2, 3, 4, 5] << [1, 2, 3, 4, 5] << [1, 2, 3, 4, 5]
p 'cc = ', cc
p cc.select {|e| e == 2}

puts [1, 2, 3, 4, 5].detect_if_none(lambda {'none found'}) {|e| e == 4}
puts [1, 2, 3, 4, 5].detect_if_none(lambda {'none found'}) {|e| e == 7}

[1, 2, 3, 4, 5].each_separatedBy(lambda {print ', '}) {|e| print e}; puts
#(1..10000).each_separatedBy(lambda {print ', '}) {|e| print e}; puts
end # //////////////////////////////////////////////////////////
