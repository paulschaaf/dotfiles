# //////////////////////////////////////////////////////////////
class AssertionFailure < Exception
end

# //////////////////////////////////////////////////////////////
module Kernel
  def assert
    yield || raise(AssertionFailure, '', caller)
  end

#   $pager_bin = if rocket
#                  '/gnu/bin/less'
#                elsif owned
#                  nil
#                elsif sunos
#                  '/apps/gnu/bin/less'
#                elsif package?(:less)
#                  '/usr/bin/less'
#                else
#                  '/bin/more'
#                end

#   $pager_bin = {
#     any             => '/bin/more',
#     package?(:less) => '/usr/bin/less',
#     owned           => nil,
#     sunos           => '/apps/gnu/bin/less',
#     rocket          => '/gnu/bin/less',
#   }[true]

#   # last one wins
#   # -- caller must eval every key before calling
#   #    -- no delayed execution or side effects
#   # -- interpreter must build the hash using all args moving left to right
#   $pager_bin = cond(any             => '/bin/more',
#                     package?(:less) => '/usr/bin/less',
#                     owned           => nil,
#                     sunos           => '/apps/gnu/bin/less',
#                     rocket          => '/gnu/bin/less')

#   # ++ first one wins (short-circuiting)
#   # ++ keys can be continuations
#   # -- argument pairing is not clearly visible
#   $pager_bin = cond(rocket,          '/gnu/bin/less',
#                     owned,           nil,
#                     sunos,           '/apps/gnu/bin/less',
#                     package?(:less), '/usr/bin/less',
#                     true,            '/bin/more')

#   # ++ can be defined as either first or last one wins
#   # -- keys cannot be continuations (since we can't redefine Proc.&& or Proc.and)
#   #    -- no delayed execution or side effects
#   $pager_bin = cond(rocket          && '/gnu/bin/less',
#                     owned           && nil,
#                     sunos           && '/apps/gnu/bin/less',
#                     package?(:less) && '/usr/bin/less',
#                     true            && '/bin/more')

#   # -- nil is illegal return value except as default
#   # -- conditions specified last makes for difficult reading, alignment
#   $pager_bin = cond('/gnu/bin/less'      if rocket,
#                     ''                   if owned,
#                     '/apps/gnu/bin/less' if sunos,
#                     '/usr/bin/less'      if package?(:less),
#                     '/bin/more')


#   $pager_bin = rocket             '/gnu/bin/less' \
#                or owned            nil \
#                or sunos           '/apps/gnu/bin/less' \
#                or package?(:less) '/usr/bin/less' \
#                or any             '/bin/more'

  def cond(*args)
    # cond                                    #=> nil
    # cond(      1)                           #=> 1
    # cond(false,1)                           #=> nil
    # cond(false,1,       2)                  #=> 2
    # cond(true, 1,       2)                  #=> 1
    # cond(false,1,  true,2)                  #=> 2
    # cond(false,1,  true,nil)                #=> nil
    # cond(false,1,  nil,2) {3}               #=> 3
    # a=10; cond(proc {a*=2}, a*3, true, a*4) #=> 30, a == 20

    remaining = args.each_slice(2) {|condition, consequence|
      return consequence if condition.value
    }
    remaining[0] || block_given? && yield
  end
end

# //////////////////////////////////////////////////////////////
class Object
  def yourself
    self
  end

  def value(*args)
    self
  end

  def is?(*args, &block)
    args.each {|e| return false unless e.value(self)}
    block_given? ? block.value(self) : true
  end

  alias :finalValue :value
end

# //////////////////////////////////////////////////////////////
class Continuation
  alias :call :value

  def finalValue(*args)
    self.value.finalValue(*args)
  end
end

# //////////////////////////////////////////////////////////////
class Method
  alias :value :call

  def finalValue(*args)
    self.value.finalValue(*args)
  end
end

# //////////////////////////////////////////////////////////////
class Proc
  alias :value :call

  def finalValue(*args)
    self.value.finalValue(*args)
  end
end

# //////////////////////////////////////////////////////////////
class Symbol
  def value(receiver=caller, *args, &block)
    # the following should be equivalent, but send seems to
    # ignore the block
    #   puts %w(h e l l o).select {|e| 'aeiou'.include?(e)}
    #   :select.value(%w(h e l l o)) {|e| 'aeiou'.include?(e)}
    (receiver == caller) ? self.to_s.value : receiver.send(self, *args, &block)
  end

  def finalValue(*args)
    self.value.finalValue(*args)
  end
end

# //////////////////////////////////////////////////////////////
# If this script is run as stand-alone script
if __FILE__ == $0 # ////////////////////////////////////////////

puts assert {:size.value('hello') == 5}

puts assert {
    File.open("/dev/null") {|f|
      f.stat.is?(:readable?, lambda {|e| e.size == 0}) {|e| e.writable?}
    }
}

p assert {
    File.open("/dev/null") {|f|
      ! [:readable?, lambda {|e| e.size == 0}].detect {|e| ! e.value(f.stat)}
    }
}

assert {1}

def a
  assert {false}
end

def b
  a()
end

begin
  b()
rescue AssertionFailure => detail
  puts true
  #print detail.backtrace.join("\n")
end


end # //////////////////////////////////////////////////////////
