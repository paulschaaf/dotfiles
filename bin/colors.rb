#!/usr/bin/env ruby

def printColorCode(code, text=code, pad=4)
  text = code.to_s + ' ' if (text === code)
  pad = 2 if code == 8
  printf "[%sm%#{pad}s[0m", code, text
  if code == 8
    print '* '  # do not format the footnote
  end
end

GridSize = 6
def printColorsForPage(pageName, ground)
  print "\n", pageName, "\n  "
  # [16, 124].each {|page|
  #   (0...GridSize).each {|row|
  #     (0..2).each {|column|
  #       (0...GridSize).each {|cellNumber|
  #         color = page + cellNumber + (GridSize*row) + (column*36)
  #         printColorCode "#{ground};5;#{color}", color > 0 ? color : ''
  #       }
  #       print ' '
  #     }
  #     print "\n  "
  #   }
  # }
  # print "\n~~\n"
  line = ''
  (16..255).each {|color|
    print "\n  " if (color-16) % (6) == 0
    printColorCode "#{ground};5;#{color}", color > 0 ? color : ''
  }
end

def section(header)
  print "\n"
  printColorCode 4, header
  print "\n"
  yield
  print "\n"
end

def printSample(code, desc)
  print "  echo \"\\e[#{code}m"
  printColorCode code, desc
  print "\"\n"
end

section("EFFECTS") {
  effects = %w{-none- Bold Dim [Italic] Underline Blink [Fast\ blink] Inverse [Concealed] [Strikeout]}
  (0...effects.length).each {|effect|
    print "\n" if effect % 4 == 0
    print "  ", effect.to_s, ": "
    printColorCode effect, effects[effect], -13
  }
  print "\n\n  Usage:         echo \"\\e[<number>mHello world\"' "
  print "\n\n  * The text in code #8 may not be visible because it is \"concealed\"."
}

section("8-COLOR PALETTE #30-37") {
  print "\n  Foregrounds:  "
  (30..37).each {|color| printColorCode color}
  print "\n  Backgrounds:  "
  (40..47).each {|color| printColorCode color}
  print "\n  Usage:         echo \"\\e[<number>mHello world\"' "
}

section("256-COLOR PALETTE") {
  printColorsForPage "  Foregrounds (use 'echo \"\\e[38;5;<number>mHello world\"')\n", '38'
  print "\n"
  printColorsForPage "  Backgrounds (use 'echo \"\\e[48;5;<number>mHello world\"')\n", '48'
}

section("USAGE") {
  print "\n  echo \"\\e[<effect*>;<foreground_color>;<background_color>mHello World\\\!\e[0m\"\n"

  printSample '31', 'Red foreground'
  printSample '38;5;1', 'Red foreground'
  printSample '48;5;12', 'Light blue background'
  printSample '38;5;94', 'Brown foreground'
  printSample '1;38;5;11', 'Boldface gold foreground'
  printSample '1;4;38;5;55', 'Boldface underlined purple foreground'
  printSample '38;5;171;48;5;11', 'Pink foreground, gold background'
}
