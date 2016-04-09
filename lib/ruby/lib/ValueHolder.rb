require 'extn/Object'

# //////////////////////////////////////////////////////////////
class ValueHolder
  attr_accessor :value

  def finalValue
    self.value.finalValue
  end

end
