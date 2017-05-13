class Object
  def delegate_to(accessor, *methods)
    methods.each {|method|
      eval <<-CODE
        def #{method}(*args, &block)
          #{accessor}.#{method}(*args, &block)
        end
      CODE
    }
  end
end

class Foo
  attr_accessor :component

  delegate_to :component, :upcase
end

__END__

a = Foo.new
a.component = 'hello'
a.to_s
a.inspect
a.upcase

a.delegate_to('@component', :reverse)
