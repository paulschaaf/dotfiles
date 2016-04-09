require 'extn/Object'

# //////////////////////////////////////////////////////////////
class Object
  def boolean?
    false
  end

  def to_b
    self ? true : false
  end
end

# //////////////////////////////////////////////////////////////
class TrueClass
  def boolean?
    true
  end
end

# //////////////////////////////////////////////////////////////
class FalseClass
  def boolean?
    true
  end
end

# //////////////////////////////////////////////////////////////
class String
  def to_b
    (self =~ /^y(|es)|t(|rue)$/i).to_b
  end
end

# //////////////////////////////////////////////////////////////
# If this script is run as stand-alone script
if __FILE__ == $0 # ////////////////////////////////////////////

puts assert {! 1.boolean?}
puts assert {true.boolean?}
puts assert {false.boolean?}
puts assert {! nil.boolean?}
puts

puts assert {1.to_b}
puts assert {true.to_b}
puts assert {! false.to_b}
puts assert {! nil.to_b}
puts

puts assert {! 'hello'.to_b}
puts assert {'true'.to_b}
puts assert {'Y'.to_b}
puts assert {'yEs'.to_b}
puts

puts assert {! 'no'.to_b}
puts assert {! 'N'.to_b}
puts assert {! 'false'.to_b}
puts assert {! 'fAlse'.to_b}

end # //////////////////////////////////////////////////////////
