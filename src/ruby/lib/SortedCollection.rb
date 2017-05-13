require 'extn/Object'

# //////////////////////////////////////////////////////////////
class SortedCollection
  include Enumerable

  attr_accessor :sort_block

  def initialize(*items, &block)
    super()
    @items, @sort_block = items, block
  end

#   def <=>(a, b)
#     @sort_block ? @sort_block.call(a, b) : super
#   end

  def each(&block)
    sorted_items.each(&block)
    self
  end

  def sort!(&block)
    @sort_block = block if block_given?
    self
  end

  def sort(&block)
    self.dup.sort!(&block)
  end

  def join(*args)
    sorted_items.join(*args)
  end

  def to_s
    self.join('')
  end

  def sorted_items
    if @sort_block
      @items.sort!(&@sort_block)
    else
      @items.sort!
    end
    @items
  end

  def <<(obj)
    @items << obj
    self
  end

  def add(*args)
    args.each {|obj| self << obj}
    (args.size == 1) ? args[0] : args
  end

  def delete(*args)
    objs_deleted = args.select {|obj| @items.delete(obj)}
    (args.size == 1) ? objs_deleted[0] : objs_deleted
  end
end

# If this script is run as stand-alone script
if __FILE__ == $0 # ////////////////////////////////////////////

  sc = SortedCollection.new(5, 4, 3, 2, 1)
  sc.instance_eval {
    p @items
  }
  p sc.to_s
  sc.add(1, 7, 3, 2)
  p sc.to_s
end
