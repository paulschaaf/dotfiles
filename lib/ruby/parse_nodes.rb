class NilClass
  def emit(aStream)
    false
  end
end

class Numeric
  def emit(aStream=[])
    aStream << self.to_s
    true
  end
end

class Proc
  def emit(aStream=[])
    call.emit(aStream)
  end
end

class String
  def emit(aStream=[])
    aStream << self
    true
  end
end

class ParseNode
  def initialize(aValue)
    @myValue=aValue
  end
  def emit(aStream=[])
    raise('Subclasses must implement this method!')
    false
  end
end

class CompositionNode < ParseNode
  def emit(aStream=[])
    answer = false
    myValue.each {|ea|
      answer = (ea.emit(aStream) || answer)
    }
    answer
  end
end

class DetectFirstNode < ParseNode
  def emit(aStream=[])
    myValue.each {|ea|
      return true if ea.emit(aStream)
    }
    false
  end
end

class DetectLastNode < DetectFirstNode
  def initialize(aValue)
    super(aValue.reverse)
  end
end

