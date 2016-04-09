#!/usr/bin/env ruby

# //////////////////////////////////////////////////////////////
class Array
  def self.join(aSepString=$,, arr=self.new)
    yield(arr)
    arr.join(aSepString)
  end

  def middle
    self[self.mid_index]
  end

  def mid_index
    self.size / 2
  end

  # swaps the contents of [a] and [b]
  def swap(a, b); self.clone.swap!(a,b); end
  def swap!(a, b)
    self[a], self[b] = self[b], self[a]
    self
  end
end

# //////////////////////////////////////////////////////////////
class Range
  def median
    self.begin + self.mid_point
  end
end

