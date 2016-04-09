require 'extn/Enumerable'
require 'extn/Array'
require 'extn/Numeric'
# require 'GSL'
require 'extn/Module'

# //////////////////////////////////////////////////////////////
class Object
  def medThree(b, c)
    [self, b, c].sort3![1]
  end
end

# //////////////////////////////////////////////////////////////
module Number
  def average(*args)
    self.sum(*args) / (args.size + 1)
  end

  alias :mean :average
  alias :arithmetic_mean :average

  def sum(*args)
    self + args.sum
  end

  def product(*args)
    self * args.product
  end
end

# //////////////////////////////////////////////////////////////
module Enumerable
  def average
    (1.0 * self.sum) / self.size
  end

  def geometric_mean
    self.product ** (self.size.reciprocal)
  end

  def harmonic_mean
    prod_reciprocals = self.inject(1) {|prod, e| prod * e.reciprocal}
    (prod_reciprocals ** self.size.reciprocal).reciprocal
  end

  def combinations(collection, *others)
    return self.flatten if collection.empty?
    answer = []
    self.with(collection) {|a, b| answer << (a + b)}
    answer.combinations(others)
  end

  def median
    self.sort[self.size / 2]
  end

  def product(initial=1)
    self.inject(initial) {|n, value| n * value}
  end

  def mean_and_standard_deviation
    x_bar = self.mean
    dividend = self.inject(0) {|sum, ea| sum + ((ea - x_bar)**2)}
    [x_bar, Math.sqrt(dividend/(self.size-1))]
  end

  def standard_deviation
    self.mean_and_standard_deviation.last
  end

  #alias_method :sd, :sd1, GSL::Stats, 0
  #alias_method :sd_m, :sd1_m, GSL::Stats, 0
  alias_method :mean, :mean1, GSL::Stats, 0

  def sd
    GSL::Stats::sd1 self
  end
  def sd_m(m)
    GSL::Stats::sd1_m self, m
  end

  def absdev
    GSL::Stats::absdev self, 1
  end

  def kurtosis_m_sd(m, sd)
    GSL::Stats::kurtosis_m_sd self, 1, m, sd
  end

  def sum(initial=0)
    self.inject(initial) {|n, value| n + value}
  end

  def permutations(collection)
    (self.combinations(collection) << collection.combinations(self)).flatten
  end

  #alias :mean :average
  alias :arithmetic_mean :mean
end

# //////////////////////////////////////////////////////////////
class Array
  def sort2!
    swap!(0,1) if (self[0]>self[1])
    self
  end

  def sort3!
    swap!(0,1) if (self[0]>self[1])
    swap!(0,2) if (self[0]>self[2])
    swap!(1,2) if (self[1]>self[2])
    self
  end
end

# //////////////////////////////////////////////////////////////
=begin
class Statistician
  def medThree(first=b=, c)
    arr = [self, b, c]
    arr.swap!(0,1) if (arr[0]>arr[1])
    arr.swap!(0,2) if (arr[0]>arr[2])
    arr.swap!(1,2) if (arr[1]>arr[2])
    arr[1]
  end
end
=end

# //////////////////////////////////////////////////////////////
class Range
  alias :first :begin
  alias :last  :end
  def median
    self.begin + (self.size / 2)
  end

  def sum
    self.size * ((2 * self.begin) + self.size - 1) / 2
  end
end

# //////////////////////////////////////////////////////////////
# If this script is run as stand-alone script
if __FILE__ == $0 # ////////////////////////////////////////////

puts 'Testing sum...'
puts [1, 2, 3, 4, 5].sum 		#» 15
puts ((1..5).sum)			#» 15
#puts (('a'..'m').sum("Letters: ")) 	#» "Letters: abcdefghijklm"
puts

puts 'Testing product...'
puts [1, 2, 3, 4, 5].product 		#» 120
puts ((1..5).product) 			#» 120
puts

puts 'Testing combinations...'
puts ['1', '2', '3', '4', '5'].combinations(('a'..'d')).join(', ')
puts
#puts (('a'..'d').combinations(('1'..'5'), ('w'..'z')).join(', '))
puts
puts 'Testing permutations...'
puts ['1', '2', '3', '4', '5'].permutations(('a'..'d')).join(', ')
puts

perms= ['abstract ', ''] 				\
  .permutations(['public ', 'protected ', 'private ']) 	\
  .combinations(['class ', 'interface '])
perms.uniq!
perms.sort!{|a, b| a.length <=> b.length}
puts perms

puts
arr = [6, 87, 3, 8, 4, 1]
puts "TESTING           [#{arr.join(', ')}]"
puts "arithmetic_mean = #{arr.arithmetic_mean}"
puts "mean            = #{arr.mean}"
puts "geometric_mean  = #{arr.geometric_mean}"
puts "harmonic_mean   = #{arr.harmonic_mean}"
puts "sorted          = [#{arr.sort.join(', ')}]"
puts "median          = #{arr.median}"
puts "standard_deviation = #{arr.standard_deviation}"
puts "sd              = #{arr.sd}"
puts "absdev          = #{arr.absdev}"
puts

end
