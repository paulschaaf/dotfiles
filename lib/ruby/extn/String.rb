# $Source:  $
# $Revision:  $
# $Date: 2003/10/23 01:28:06 $
# $Author: pschaaf $
# $State: Exp $
# $Name:  $

# //////////////////////////////////////////////////////////////
class String
  def bookend(open, close='')
    (open + self) << close
  end

  def wordwrap(args={})
    separator = args[:separator] ||= ' '
    self.split(separator).wordwrap(args).join($/)
  end

  def permutation(sep='')
    s = Set.new
    self.split(//).permutation.each {|perm| s << perm.join(sep)}
    s.sort!
  end

end

class Array
  def wordwrap(args={})
    l_margin  = args[:l_margin] || args[:indent] || 0
    row_label = (' ' * l_margin) + args[:row_label].to_s

    separator = args[:separator] || ', '
    r_margin  = args[:r_margin]  || ENV["COLUMNS"].to_i || 80

    col_width = r_margin - row_label.length

    output=[]

    eval "def output.<< (aLine)
      super('#{row_label}' + aLine.to_s.strip)
    end"

    last_line = self.inject('') {|line, word|
      if line.length + word.length + separator.length <= col_width
        line + word + separator
      else
        output << line
        word
      end
    }
    output << last_line[0...-separator.length] unless last_line.empty?
  end
end

# //////////////////////////////////////////////////////////////
# If this script is run as stand-alone script
if __FILE__ == $0 # ////////////////////////////////////////////

def test(string=nil, &block)
  return true unless block_given?
  puts "Testing #{string}" if string
  if block.call
    puts "\nTest passed."
    true
  else
    puts "\nTest failed! #{block.binding.instance_variables}"
    false
  end
end

methods = Integer.instance_methods(true).sort
output = methods.join(' ').wordwrap(:separator=>', ', :indent=>3, :r_margin=>35)
puts output

p ''.wordwrap

new_methods = output.join(' ').gsub(/, /, ' ').split(' ')
test {methods.size == new_methods.size}

end # //////////////////////////////////////////////////////////

