class DebugIO
  attr_accessor :out
  attr_reader   :header, :level

  def initialize(aLevel=0, anIO=$stderr)
    @level, @out = aLevel, anIO
  end

  def header(level=@level)
    @header ||= Kernel.sprintf('#debug%s ', '+'*level)
  end

  def level=(value)
    @level, @header = value, nil
  end

  def at_level?(minimum=0)
    # minimum = true, 0, or nil means always true
    if minimum and (minimum == true or minimum <= @level)
      block_given? ? yield(minimum) : @level
    else
      false
    end
  end

  def puts(level, *args, &block)
    at_level?(level) {
      self.print_header(level)
      @out.puts(*args, &block)
    }
  end
  def print(level, *args, &block)
    at_level?(level) {
      self.print_header(level)
      @out.print(*args, &block)
    }
  end
  def printf(level, *args, &block)
    at_level?(level) {
      self.print_header(level)
      @out.printf(*args, &block)
    }
  end
  def sprintf(level, *args, &block)
    at_level?(level) {
      self.print_header(level)
      @out.sprintf(*args, &block)
    }
  end
  def print_header(level=0)
    @out.printf('# debug %d: ', level) if level > 0
  end
end

class NullIO
  def puts(*args, &block)
  end
  alias_method :print,   :puts
  alias_method :printf,  :puts
  alias_method :sprintf, :puts
end

