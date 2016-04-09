require 'extn/Enumerable'

# //////////////////////////////////////////////////////////////
class Object
  def ifValueYield
    self.value && block_given? && yield
  end

  def yieldOrValueIfNoBlock
    block_given? ? yield : self.value
  end

  def detectValue(*args, &block)
    self.value || args.detectValue(&block)
  end

  def valueAnd(*args)
    answer = (self.value) && (args.detect {|e| e.value})
    (answer && block_given?) ? yield : answer
  end

  def time_to_do(*args, &block)
    Time.to_do {self.value(*args, &block)}
  end
end

# //////////////////////////////////////////////////////////////
class Time
  def Time.to_do()
    start=  self.times.sum
    answer= yield if block_given?
    self.times.sum-start
  end

  def Time.to_do(repeat=1, answers=nil, times=nil)
    last_answer, total_time = nil, 0
    repeat.times {
      start_time  =   self.times.sum
      answers     <<  yield if answers
      duration    =  (self.times.sum - start_time)
      times       <<  duration if times
      total_time  +=  duration
    }
    #[answers.last, 1.0*total_time/repeat, answers, times]
    times.mean_and_standard_deviation
  end
end

# //////////////////////////////////////////////////////////////
module Enumerable
  def detectValue(*args)
    self.combined_with(args).detect {|e| e.detectValue} \
    || (lambda.detectValue if block_given?)
  end
end

# //////////////////////////////////////////////////////////////
# If this script is run as stand-alone script
if __FILE__ == $0 # ////////////////////////////////////////////

puts 1.ifValueYield
puts 1.ifValueYield {"Block has been run!"}
puts nil.ifValueYield
puts nil.ifValueYield {"Block has been run!"}
puts

puts 1.detectValue{"Block has been run!"}
puts 1.detectValue(2) {"Block has been run!"}
puts 1.detectValue(nil) {"Block has been run!"}
puts 1.detectValue(nil, 3) {"Block has been run!"}
puts

puts false.detectValue{"Block has been run!"}
puts false.detectValue(2){"Block has been run!"}
puts false.detectValue(nil){"Block has been run!"}
puts

puts [1, nil].detectValue{"Block has been run!"}
puts [1, nil].detectValue(2) {"Block has been run!"}
puts [1, 2].detectValue(nil) {"Block has been run!"}
puts

puts [false].detectValue{"Block has been run!"}
puts [false].detectValue(2){"Block has been run!"}
puts [false].detectValue(nil){"Block has been run!"}

p Time.to_do {puts 'foo'}

end # //////////////////////////////////////////////////////////
