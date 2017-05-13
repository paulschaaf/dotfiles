# //////////////////////////////////////////////////////////////
class Module
  def self.allInstances
    raise NotImplementedError unless self == Module || self == Class
    self.constants \
      .collect {|e| self.const_get(e)} \
      .select {|e| e.class == self} \
      .sort {|a, b| a.to_s <=> b.to_s}
  end

#   def Module.const_mapping
#     map = {}
#     self.constants.each {|name|
#       value = self.const_get(name)
#       value_class = value.class
#       new_value = if value_class == Module || value_class == Class
#                     value
#                   else
#                     name
#                   end
#       (map[value_class] ||= Array.new) << new_value
#     }
#     map
#   end

  def ignoreMethod(*names)
    names.each {|ea| self.module_eval("alias :#{ea} :no_op")}
  end

  def subclassResponsibility(*methodNames)
    methodNames.each {|aName|
      self.module_eval(
	"def #{aName}
	  raise 'Error: Implementation of #{aName.to_s} must be defined by any subclasses!'
	end")
    }
  end

  alias :pgs_alias_method :alias_method

  def alias_method(newId, oldId, to=nil, self_is_arg=nil)
    return pgs_alias_method(newId, oldId) unless to
    meth  = "def #{newId}(*args, &block)\n#{to}.#{oldId}("
    meth += case self_is_arg
	    when nil
	      '*args'
	    when 0
	      'self, *args'
	    else
	      "*args[0..#{self_is_arg-1}], self, *args[#{self_is_arg}..-1]"
	    end
    self.module_eval(meth + ", &block)\nend")
  end

  def notYetImplemented(*methods)
    methods.each {|each| self.alias_method(each, :__notYetImplemented)}
    #  self.module_eval("def #{each}(*args); self.__notYetImplemented; end")
    #}
  end
end

# //////////////////////////////////////////////////////////////
class Object
  def addMethod(aString)
    self.class.module_eval(aString)
  end

  def ignoreMethod(*args)
    self.class.ignoreMethod(args)
  end

  def no_op(*args)
    nil
  end

end

# //////////////////////////////////////////////////////////////
module Kernel
  def __notYetImplemented
    call_stack=caller.clone # caller is immutable

    # ignore stack entries for redefinitions of *this* method
    #call_stack.shift while (call_stack[0] =~ /notYetImplemented/)

    #method_name = call_stack.shift.slice(/\`(.*)\'/)[1..-2]
    #message = "#{self}:#{self.class} does not yet implement " #{method_name}"

    raise NotImplementedError #, message #, call_stack
  rescue NotImplementedError => ex
    puts; p ex.backtrace
    message = "#{self}:#{self.class} does not yet implement " #{method_name}"
    ex.raise(message)
  end

  protected :__notYetImplemented
end

# //////////////////////////////////////////////////////////////
# If this script is run as stand-alone script
if __FILE__ == $0 # ////////////////////////////////////////////

class SampleClass
  notYetImplemented :foo

  def bar
    self.foo
  end

  def to_s; 'samplename'; end
end

begin
  SampleClass.new.bar
rescue NotImplementedError => ex
end

SampleClass.new.bar

end # //////////////////////////////////////////////////////////
