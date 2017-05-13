class Object
  def to_full_s
    self.to_s
  end
end

# ================================================
class Makefile
  attr_accessor :entries, :env, :macros

  def initialize
    @entries = Array.new
    @env     = Hash.new {|hash, key| hash[key] = MakefileMacro.new_in(self, key)}
  end

  def endif
    push('endif')
  end

  def each(&block)
    entries.each(&block)
  end

  def if_defined(var, &block)
    code_block("ifdef #{var}", &block)
  end

  def if_not_defined(var, &block)
    code_block("ifndef #{var}", &block)
  end

  def else(&block)
    code_block('else', &block)
  end

  def code_block(begin_code, end_code='endif', &block)
    push(begin_code, &block)
    push(end_code) if block_given?
  end

  def set_default(var, value)
    if_not_defined(var) {push "#{var}=\"#{value}\""}
  end

  def new_rule(*args)
    rule = MakefileRule.new_in(self, *args)
    yield rule if block_given?
    rule
  end

  def newline(count=1)
    push(Array.new(count, ''))
  end

  def comment(*strings)
    push(*strings.collect {|aComment| "# #{aComment}"})
  end

  def suspend_while
    return unless block_given?
    saved_buffer, @entries = @entries, Array.new
    yield
    @entries
  ensure
    @entries = saved_buffer
  end

  protected :suspend_while

  def compiled_entries
    # todo: merge dependency lines if prereqs and commands are identical
    entries
  end

  def [](var)
    env[var]
  end

  def []=(var, *values)
    env[var].value = values
  end

  def push(*arr, &block)
    #new_entries = arr - @entries
    @entries.push(*arr) #unless arr == [nil]
    @entries.push(*self.suspend_while(&block)) if block_given?
  end

  def pushIfHasTargets(*arr)
    self.push(*arr.reject {|e| e.targets.empty?})
  end

  def <<(anObject)
    push(anObject); self
  end

  def to_s
    has_rebuild = false
    answer = self.compiled_entries.collect {|e|
      this = e.to_full_s
      has_rebuild = true if /\brebuild\b/ =~ this
      this
    }
    answer << "rebuild:\tclean all" unless has_rebuild
    answer.join($/)
  end

  def phony(*args)
    # todo: make this handle multiple calls
    self.new_rule('.PHONY', *args)
  end

  def suffixes(*args)
    # todo: make this handle multiple calls
    self.new_rule('.SUFFIXES', *args)
  end
end

# ================================================
class MakefileComponent
  def self.new_in(makefile, *args)
    answer = self.new(*args)
    answer.makefile = makefile
    answer
  end

  def makefile=(value)
    value << self
  end

  def initialize(*args); end
end

# ================================================
class MakefileRule < MakefileComponent
  attr_accessor :targets, :dependencies, :commands #:makefile, 

  def initialize(target=nil, deps=[], *commands)
    super
    @targets = []
    self.addTarget(*target) if target
    @dependencies, @commands = deps.to_a, commands
  end

  def addTarget(*strings)
    @targets += strings
    strings
  end

  def addDep(*strings)
    @dependencies += strings
    strings
  end

  def addCmd(*strings)
    @commands += strings
    strings
  end

  def to_s
    deps_s = if @dependencies.empty?
               ''
             else
               "\t" + @dependencies.join(' ')
             end
    cmd_s  = if @commands.empty?
               ''
             else
               "\t" + @commands.join("#$/\t") + $/
             end
    "#{@targets.join(' ')}:#{deps_s}#$/#{cmd_s}"
  end
end

# ================================================
class MakefileMacro < MakefileComponent
  attr_accessor :name, :value

  def makefile=(file)
    super
    file.env[self.name] = self
    file
  end

  def initialize(name, *value)
    super
    @name = name
    @value = value
  end

  def to_full_s
    "#{@name}=#{@value.join(' ')}"
  end

  def to_s
    "${#{name}}"
  end

  def push(*arr)
    value.push(*arr)
  end

  def <<(anObject)
    value << anObject; self
  end

end
