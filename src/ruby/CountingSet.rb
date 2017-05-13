#!/usr/bin/env ruby

require 'set'

class CountingSet < Set
  def [](o)
    @hash[o] || 0
  end
  alias_method :occurrences_of, :[]

  def add(o)
    @hash[o] = self[o] + 1
    self
  end

  def <<(o)
    @hash[o] = self[o] + 1
    self
  end

  def delete(o)
    case (count = self[0])
    when 0   # nothing to do as it is absent
    when 1
      super  # remove the entry entirely
    else
      @hash[o] = count - 1
    end
  end

end

if $0 == __FILE__
  eval DATA.read, nil, $0, __LINE__+4
end

__END__

require 'test/unit'

CLASS_TO_TEST = CountingSet

class TC_CountingSet < Test::Unit::TestCase
  def test_aref
    assert_nothing_raised {
      CLASS_TO_TEST[]
      CLASS_TO_TEST[nil]
      CLASS_TO_TEST[1,2,3]
    }

    assert_equal(0, CLASS_TO_TEST[].size)
    assert_equal(1, CLASS_TO_TEST[nil].size)
    assert_equal(1, CLASS_TO_TEST[[]].size)
    assert_equal(1, CLASS_TO_TEST[[nil]].size)

    set = CLASS_TO_TEST[2,4,6,4]
    assert_equal(CLASS_TO_TEST.new([2,4,6]), set)
  end

  def test_add
    set = CLASS_TO_TEST[1,2,3]

    ret = set.add(2)
    assert_same(set, ret)
    assert_equal(CLASS_TO_TEST[1,2,3], set)

    ret = set.add?(2)
    assert_nil(ret)
    assert_equal(CLASS_TO_TEST[1,2,3], set)

    ret = set.add(4)
    assert_same(set, ret)
    assert_equal(CLASS_TO_TEST[1,2,3,4], set)

    ret = set.add?(5)
    assert_same(set, ret)
    assert_equal(CLASS_TO_TEST[1,2,3,4,5], set)
  end

  def test_delete
    set = CLASS_TO_TEST[1,2,3]

    ret = set.delete(4)
    assert_same(set, ret)
    assert_equal(CLASS_TO_TEST[1,2,3], set)

    ret = set.delete?(4)
    assert_nil(ret)
    assert_equal(CLASS_TO_TEST[1,2,3], set)

    ret = set.delete(2)
    assert_equal(set, ret)
    assert_equal(CLASS_TO_TEST[1,3], set)

    ret = set.delete?(1)
    assert_equal(set, ret)
    assert_equal(CLASS_TO_TEST[3], set)
  end

  def test_s_new
    assert_nothing_raised {
      CLASS_TO_TEST.new()
      CLASS_TO_TEST.new(nil)
      CLASS_TO_TEST.new([])
      CLASS_TO_TEST.new([1,2])
      CLASS_TO_TEST.new('a'..'c')
      CLASS_TO_TEST.new('XYZ')
    }
    assert_raises(ArgumentError) {
      CLASS_TO_TEST.new(false)
    }
    assert_raises(ArgumentError) {
      CLASS_TO_TEST.new(1)
    }
    assert_raises(ArgumentError) {
      CLASS_TO_TEST.new(1,2)
    }

    assert_equal(0, CLASS_TO_TEST.new().size)
    assert_equal(0, CLASS_TO_TEST.new(nil).size)
    assert_equal(0, CLASS_TO_TEST.new([]).size)
    assert_equal(1, CLASS_TO_TEST.new([nil]).size)

    ary = [2,4,6,4]
    set = CLASS_TO_TEST.new(ary)
    ary.clear
    assert_equal(false, set.empty?)
    assert_equal(3, set.size)

    ary = [1,2,3]

    s = CLASS_TO_TEST.new(ary) { |o| o * 2 }
    assert_equal([2,4,6], s.sort)
  end

  def test_clone
    set1 = CLASS_TO_TEST.new
    set2 = set1.clone
    set1 << 'abc'
    assert_equal(CLASS_TO_TEST.new, set2)
  end
end
