#!/usr/bin/env ruby

# //////////////////////////////////////////////////////////////

Infinity      = 1.0/0.0 unless defined?(Infinity)
Infinitesimal = 1e-15   unless defined?(Infinitesimal)

class Numeric
  def andUp
      self..Infinity
  end

  def between?(smaller, larger)
    (smaller <= self) && (self <= larger)
  end

  def median(*args, &sortBlock)
    sorted = args.push(self).sort(&sortBlock)
    sorted[sorted.size / 2]
  end

  def reciprocal
    1.0/self
  end

  def slightlyMore
    self + Infinitesimal
  end

  def slightlyLess
    self - Infinitesimal
  end

  def max(*others, &block)
    (others << self).max(&block)
  end

  def min(*others, &block)
    (others << self).min(&block)
  end
end

class Integer
  def pred
    self - 1
  end

  def even?
    (self.modulo 2) == 0
  end

  def odd?
    not self.even?
  end

  def fibonacci
    curr_fib, next_fib = 0, 1
    self.times {
      curr_fib, next_fib, = next_fib, curr_fib + next_fib
    }
    curr_fib
  end

end

# //////////////////////////////////////////////////////////////
# If this script is run as stand-alone script
if __FILE__ == $0 # ////////////////////////////////////////////

require 'extn/Proc'

  def reportOn(code, repeat=1)
    answer, time, values, times = *Time.to_do(repeat) {eval code}
    puts ; puts code
    puts "# (= #{answer.to_s.size} digit number) found in #{times.median} sec#{" (median from #{repeat} reps)" if repeat > 1}\n"
  end

puts Infinity
puts Infinitesimal
puts(1.slightlyMore > 1)

reportOn('5000.fibonacci', 50)
#puts reportOn('27.fib2')
#puts reportOn('2700.fib2')

def bar
  puts 'in bar'
  yield
  return 'bar'
end

def foo
  p = lambda {puts 'in lambda'; return 'lambda'}
  puts "\nin foo"
  bar(&p)
  puts 'back in foo'
  return 'in foo'
end

puts foo

=begin
def Integer
  def fib
    if self<2
      self
    else
      (self-2).fib + (self-1).fib
    end
  end

  def fib2
    _fib = lambda {|n, curr_fib, next_fib|
      if n == self
	curr_fib
      else
	_fib.call(n+1, next_fib, curr_fib+next_fib)
      end
    }
    _fib.call(0, 0, 1)
  end
end
=end

end # //////////////////////////////////////////////////////////
