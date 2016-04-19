#!/usr/bin/env ruby

def getColorString(code, text=nil, pad=4)
  text = code.to_s '' if (text == nil)
  pad = 2 if code == 8
  answer = sprintf "[%sm%#{pad}s[0m", code, text
  answer += '* ' if code == 8 # do not format the footnote
  return answer
end

def printColorString(code, text=code, pad=4)
  print(getColorString code, text, pad)
end

GridSize = 6
def printColorsForPage(pageName, ground)
  puts "\n", pageName
  line = ''
  inverseLine = line.dup
  (16..255).each {|color|
    colorStr = color < 0 ? '' : color.to_s
    line        += getColorString("40;#{ground};5;#{color}", colorStr)
    inverseLine += getColorString("97;107;#{ground};5;#{color}", colorStr)
    if (color == 255) or ((color-15) % 6 == 0)
      print '  ', line, '  ', inverseLine, "\n"
      line, inverseLine = '', ''
    end
  }
end

def section(header)
  puts
  printColorString 4, header
  puts
  yield
  puts "\n"
end

def printSample(code, desc)
  print "  echo \"\\e[#{code}m"
  printColorString code, desc
  puts '"'
end

section("EFFECTS") {
  effects = %w{-unset\ all- Bold [Dim] [Italic] Underline Blink [Fast\ blink] Inverse [Concealed] [Strikeout]}
  (0...effects.length).each {|effect|
    puts if effect % 4 == 0
    print "  ", (effect).to_s, ": "
    printColorString effect, effects[effect], -13
  }
  puts <<STRING


  * Bracketed items are not widely supported. The text in code #8 may not
    be visible because it is "concealed".

  Usage:  echo "\\e[<number>mHello world"
    To unset an individual effect add 20 to the effect number.
STRING
}

section("8-COLOR PALETTE") {
  { 0 => 'Foreground', 10 => 'Background' }.each {|ground, groundName|
    print "\n  ", groundName
    { 0 => 'Dim', 60 => 'Bright' }.each {|intensity, intensityName|
      printf "\n    %-6s  ", intensityName
      (30..37).each {|color| printColorString ground+color+intensity}
    }
    puts
  }
  puts "\n  Usage:         echo \"\\e[<number>mHello world\"' "
}

section("256-COLOR PALETTE (16..231)") {
  (0...GridSize).each {|section|
    (0...GridSize).each {|row|
      { 0 => 'Foreground', 10 => 'Background' }.each {|ground, groundName|
        print '  '
        (16..21).each {|cell|
          color = cell + (row + (section*GridSize))*GridSize
          printColorString "#{38+ground};5;#{color}", color
        }
      }
      puts
    }
  }
  puts <<STRING

  Foreground (use 'echo "\\e[38;5;<number>mHello world"')
  Background (use 'echo "\\e[48;5;<number>mHello world"')
STRING
}

section("GRAYSCALE") {
  (0..3).each {|row|
    { 0 => 'Foreground', 10 => 'Background' }.each {|ground, groundName|
      print '  '
      (232..237).each {|cell|
        color = cell + (row*GridSize)
        printColorString "#{38+ground};5;#{color}", color
      }
    }
    puts
  }
  puts "\n  Usage:         echo \"\\e[<number>mHello world\"' "
}

section("USAGE") {
  puts "\n  echo \"\\e[<effect*>;<foreground_color>;<background_color>mHello World\\\!\e[0m\""

  printSample '31', 'Red foreground'
#  printSample '38;5;1', 'Red foreground'
  printSample '48;5;12', 'Light blue background'
  printSample '38;5;94', 'Brown foreground'
  printSample '1;38;5;11', 'Boldface gold foreground'
  printSample '1;4;38;5;55', 'Boldface underlined purple foreground'
  printSample '38;5;171;48;5;11', 'Pink foreground, gold background'
}
