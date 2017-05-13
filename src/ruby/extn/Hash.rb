#!/usr/bin/env ruby

# $= = true	#use case-insensitive string comparisons

class Object
  def inOrderDo(depth=0, &visitor)
    visitor.call(depth, self)
  end
end

class Array
  def inOrderDo(depth=0, &visitor)
    self.each do |each|
      visitor.call(depth, each)
    end
  end
end

class Hash
  def inOrderDo(depth=0, &visitor)
    unless self.empty?
      myKeys = keys.sort {|a, b| a.downcase <=> b.downcase}
      myKeys.inOrderDo(depth) {|depth, aKey|
	aKey.inOrderDo(depth, &visitor)
	self[aKey].inOrderDo(depth+1, &visitor)
      }
    end
  end

  def atIfAbsent(key)
    return self[key] if self.has_key?(key)
    yield if block_given?
  end

  def printTree(indent=3)
    self.inOrderDo {|depth, obj|
      puts ' '*indent*depth + obj.to_s
    }
  end
end

# //////////////////////////////////////////////////////////////
# //////////////////////////////////////////////////////////////
# If this is run as stand-alone script
if __FILE__ == $0

{"Versant"=>{"Foo"=>"bar", "alpha"=>"beta"}, "ODB"=>"yes"}.printTree

end
