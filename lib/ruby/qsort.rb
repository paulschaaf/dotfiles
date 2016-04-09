#!/usr/bin/env ruby
#http://yagni.com/combsort/index.php#ruby-inplace-quicksort

require 'debugMethod'
require 'extn/Proc'
require 'lib/statistics'

def listOfIntegers(size)
  (0...size).to_a
end

def scramble(a)
  a.each_index { |i| a.swap!(i, rand(a.size))}
end

def scrambledListOfIntegers(size)
  scramble(listOfIntegers(size))
end

def setRandomNumberSeed
  srand(1)
end

#BEGIN-ALGORITHM
def quicksort(a)
  return a if a.size <= 1
  pivot = a[0]
  quicksort(a.select {|value| value < pivot}) +
    a.select {|value| value == pivot} +
    quicksort(a.select {|value| value > pivot})
end
#END-ALGORITHM

class Array
  def partition()
    #this array holds elements that are
    #      [ [equalTo], [greaterThan], [lessThan] ]
    #the partition value
    median = self.first.medThree(self.middle, self.last)
    
    answer= [[], [], []]
    self.each {|each| answer[each <=> median] << each}
    
    #put back in sensible order: [ [lessThan], [equalTo], [greaterThan] ]
    [answer[2], answer[0], answer[1]]
  end

  def qsort
    return self if length <= 1
    part = self.partition
    part[0].qsort + part[1] + part[2].qsort
  end

  def partition2()
    #this array holds elements that are
    #      [ [equalTo], [greaterThan], [lessThan] ]
    #the partition value
    median = self.first.medThree(self.middle, self.last)
    
    answer= [[], [], []]
    self.each {|each| answer[each <=> median] << each}
    
    #put back in sensible order: [ [lessThan], [equalTo], [greaterThan] ]
    #[answer[2], answer[0], answer[1]]
    answer.unshift(answer.pop)
  end

  def q2sort!
    case length
      when 0, 1 then self
      when 2    then self.sort2!
      when 3    then self.sort3!
      else
        part = self.partition2
        part[0].q2sort! << part[1] << part[2].q2sort!
    end
  end

  #debug
end

setRandomNumberSeed

a = scrambledListOfIntegers(4000)
#puts Time.to_do {quicksort(a)}[1]
#answer =nil
times= Time.to_repeat(20) {a.qsort}[3]
puts "mean=#{times.mean}, std_dev=#{times.standard_deviation}"
puts Time.to_repeat(20) {a.q2sort!}[3].median
puts a.first
