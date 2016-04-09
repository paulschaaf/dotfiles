#!/usr/bin/env ruby
# Adds automatic debugging information to a class (writes argument and
# return values of each instance method call).
#
# Write
#
#    debug :first_method, :second_method
#
# to debug a selected set of instance methods. Write
#
#    debug
#
# to debug all instance methods.

$debug_indent = ''
$debug_no = 0

class String
	def debug_displayString()
		self
	end
end
class Object
	def debug_displayString()
		#re-define this in subclasses
		self.inspect
	end
	def debug_display(pre="", post="")
		$stderr.puts $debug_indent + pre + self.debug_displayString + post
	end
end
class Module
	def debug(*args)
		if args.length == 0
			args = module_eval 'instance_methods'
			args = args.collect {|x| x.to_sym}
		end
		args.each do |m|
			rename = '__debug_' + $debug_no.to_s
			$debug_no += 1
			module_eval <<-DEBUG_WRAPPER
				alias_method :#{rename}, :#{m.id2name}

				def #{m.id2name}(*args, &block)
					$debug_indent += '    '
					arglist = args.collect{|x| x.inspect}.join ', '
					''.debug_display
					('*#{m.id2name}(' + arglist + ')').debug_display
					res = #{rename}(*args, &block)
					res.debug_display('=> ')
					''.debug_display
					$debug_indent['    '] = ''
					return res
				end
			DEBUG_WRAPPER
		end
		#alias_method ':puts
		#def puts(*args, &block)
	end
end

# Sample usage

=begin
class Test
    def fact(n)
        n > 0 ? n * fact(n-1) : 1
    end
    def bin(n, k)
        fact(n) / fact(n-k) / fact(k)
    end
    debug
end

Test.new.bin(4,2)
=end
