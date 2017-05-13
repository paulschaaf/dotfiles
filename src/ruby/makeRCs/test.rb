# $Source$
# $Revision$
# $Date$
# $Author$
# $State$
# $Name$

#!/usr/bin/ruby

require 'lib/ValueHolder'

class Hash
  def value
    self[true]
  end
end

class Array
  def value
    self.reverse.detect {|e| e.value}
  end
end

def function(name, *args)
  p args
  name.value #+ lastValue(*args)
end

function(:any_alias,
  false => "local label=$1; shift; alias $label=\"#{args}\"",
  true  => "alias #{args}"
)

__END__

function(:any_alias) {
  sh  "local label=$1; shift; alias $label=\"#{args}\""
  csh "alias #{args}"
}

def test(*args)
  #p arg1
  p args
end

test('hello', :alpha => true, :beta => false)

