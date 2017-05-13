#!/usr/bin/env ruby

# //////////////////////////////////////////////////////////////

class Comparable
  def max(*args, &block)
    args.push(self)max(&block)
  end

  def median(*args, &sortBlock)
    sorted = args.push(self).sort(&sortBlock)
    sorted[sorted.size / 2]
  end

  def min(*args, &block)
    args.push(self).min(&block)
  end
end

# //////////////////////////////////////////////////////////////
# If this script is run as stand-alone script
if __FILE__ == $0 # ////////////////////////////////////////////

end # //////////////////////////////////////////////////////////
