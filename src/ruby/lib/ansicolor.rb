#!/usr/bin/env ruby

# $Source: e:/MyDocuments/cvsroot/lib/ruby/lib/ansicolor.rb,v $
# $Revision: 1.15 $
# $Date: 2003/08/27 22:34:50 $
# $Author: pschaaf $
# $State: Exp $
# $Name:  $

# downloaded from Ruby application archive
# PGS cleaned up code

#require 'extn/String'

unless :to_sym.respond_to?(:to_sym)
  class String
    alias_method(:to_sym, :intern)
  end

  class Symbol
    def to_sym
      self
    end
  end
end

# ================================================
# ======= Hash
class Hash
  def keysSortedByValue
    self.sort.collect {|key, value| key}
  end

  def valuesSortedByKey
    self.sort.collect {|key, value| value}
  end
end

# ================================================
# ======= Term
module Term
  module ANSIColor
    @@attributes = []

    @@foregrounds = {
      30 => :black,
      31 => :red,
      32 => :green,
      33 => :yellow,
      34 => :blue,
      35 => :magenta,
      36 => :cyan,
      37 => :white,
    }

    @@backgrounds= {}
    @@foregrounds.each_pair {|number, name| @@backgrounds[number+10] = "on_#{name}".to_sym}

    @@colors=@@foregrounds.valuesSortedByKey

    # 'reverse' is not used to avoid confusion with String.reverse
    @@common_effects = {
      1 => :bold,
      2 => :dim,
      4 => :uline,
      5 => :blink,
      7 => :inverse,
    }

    # these are not widely implemented
    @@uncommon_effects = {
      3 => :italic,
      6 => :rapid_blink,
      8 => :concealed,
      9 => :strikeout,
    }

    [@@foregrounds, @@backgrounds, @@common_effects, @@uncommon_effects, {0 => :clear}].each {|arr|
      arr.each {|number, name|
          # cases for the receiving context:
          #    self.class  block_given?  result=
          # 1) Object      false         <color_code>
          # 2) String      false         <color_code>#{self}</color_code>
          # 3) Object      true          <color_code>#{yield}</color_code>
          # 4) String      true          <color_code>#{self}#{yield}</color_code>
          code=("def #{name}(arg=nil)
                result = self.new(\"\e[#{number}m\")
                use_close_tag=false
                if arg
                   result << arg.to_s
                   use_close_tag=true
                end
                if block_given?
                   result << yield.to_s
                   use_close_tag=true
                end
                #result << self.clear if use_close_tag
                result.simplify_colors!
                result
        end")
        eval code
        @@attributes << name
      }
    }

    def attributes;       @@attributes;       end
    def colors;           @@colors;           end
    def backgrounds
      @@colors.collect {|e| "on_#{e}".to_sym}
    end
    def common_effects
      @@common_effects.valuesSortedByKey << :none
    end
    def uncommon_effects; @@uncommon_effects.valuesSortedByKey; end

    def none(*args)
      result = self.new(args.to_s)
      result << yield.to_s if block_given?
      result
    end

    alias :foregrounds    :colors
    alias :negative       :inverse
    alias :no_color       :clear
    alias :reset          :clear
    alias :strikethrough  :strikeout
    alias :underline      :uline
    alias :underscore     :uline
  end
end

# ================================================
# ======= Use this trick to work around namespace cluttering that
# ======= happens if you just include Term::ANSIColor:

class Color < String
  class << self
    include Term::ANSIColor
  end

  self.attributes.each {|attrib|
    eval "def #{attrib}
	self.class.#{attrib}(self)
end"
  }

  def <<(*args)
    (super).simplify_colors!
  end

  def +(*args)
    (super).simplify_colors!
  end
end

# ================================================
# ======= String
class String
  def bookend(open, close='')
    (open + self) << close
  end

  def highlight!(regex, open=Color.yellow + Color.bold, close=Color.clear)
    regex = if regex.class == String
              return self if regex.empty?
              Regexp.new(regex)
            elsif regex.class == Array
              return self if regex.empty?
              Regexp.new(regex.join('|'))
            else
              regex
            end
    self.gsub!(regex) {|match| match.bookend(open, close)}
    self
  end

  def highlight(*args); self.dup.highlight!(*args); end

  def simplify_colors!
    self.gsub!(/m.\[/, ';')
    self.gsub!(/\[(0;)+0m/, '[0m')
    self
  end

  def simplify_colors
    (answer = self.dup).simplify_colors!
    answer
  end
end

# ================================================
# ======= If this script is run as stand-alone script
if __FILE__ == $0

print Color.red, Color.bold, "No Namespace cluttering:", Color.clear, "\n"
print Color.green + "green" + Color.clear, "\n"
print Color.yellow { Color.on_black { "yellow on_black" } }, "\n\n"

# Anyway, I don't define any of Term::ANSIColor's methods in this example
# and I want to keep it short:
include Term::ANSIColor

print red, bold, "Usage as constants:", reset, "\n"
print clear, "clear", reset, reset, "reset", reset,
  bold, "bold", reset, dark, "dark", reset,
  underscore, "underscore", reset, blink, "blink", reset,
  inverse, "inverse", reset, concealed, "concealed", reset, "|\n",
  black, "black", reset, red, "red", reset, green, "green", reset,
  yellow, "yellow", reset, blue, "blue", reset, magenta, "magenta", reset,
  cyan, "cyan", reset, white, "white", reset, "|\n",
  on_black, "on_black", reset, on_red, "on_red", reset,
  on_green, "on_green", reset, on_yellow, "on_yellow", reset,
  on_blue, "on_blue", reset, on_magenta, "on_magenta", reset,
  on_cyan, "on_cyan", reset, on_white, "on_white", reset, "|\n\n"

print red { bold { "Usage as block forms:" } }, "\n"
print clear { "clear" }, reset { "reset" }, bold { "bold" },
  dark { "dark" }, underscore { "underscore" }, blink { "blink" },
  inverse { "inverse" }, concealed { "concealed" }, "|\n",
  black { "black" }, red { "red" }, green { "green" },
  yellow { "yellow" }, blue { "blue" }, magenta { "magenta" },
  cyan { "cyan" }, white { "white" }, "|\n",
  on_black { "on_black" }, on_red { "on_red" }, on_green { "on_green" },
  on_yellow { "on_yellow" }, on_blue { "on_blue" },
  on_magenta { "on_magenta" }, on_cyan { "on_cyan" },
  on_white { "on_white" }, "|\n\n"

# Usage as Mixin into String or its Subclasses
class String
  include Term::ANSIColor
end

print "Usage as String Mixins:".red.bold, "\n"
print "clear".clear, "reset".reset, "bold".bold, "dark".dark,
  "underscore".underscore, "blink".blink, "inverse".inverse,
  "concealed".concealed, "|\n",
  "black".black, "red".red, "green".green, "yellow".yellow,
  "blue".blue, "magenta".magenta, "cyan".cyan, "white".white, "|\n",
  "on_black".on_black, "on_red".on_red, "on_green".on_green,
  "on_yellow".on_yellow, "on_blue".on_blue, "on_magenta".on_magenta,
  "on_cyan".on_cyan, "on_white".on_white, "|\n\n"

symbols = Term::ANSIColor::attributes
print red { bold { "All supported attributes = " } },
  reset { symbols.inspect }, "\n\n"

print "Send symbols to strings:".send(:red).send(:bold), "\n"
print symbols[12, 8].map { |c| c.to_s.send(c) }, "\n"
# vim: set cin sw=4 ts=4:

end
