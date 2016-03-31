#!/usr/bin/env ruby

def printColorCode(code, text=code, pad=4)
  if code == 8
    print '*'  # do not format the footnote
    pad += 1
  end
  text = code.to_s + ' ' if (text === code)
  printf "[%sm%#{pad}s[0m", code, text
end

def printColorsForPage(pageName, ground)
  print "\n", pageName
  (-2..255).each {|color|
    print "\n  " if (color-16) % 6 == 0
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
  (0...effects.length).each {|color|
    print "\n" if color % 4 == 0
    print "  ", color.to_s, ": "
    printColorCode color, effects[color], -13
  }
  print "\n\n  Usage:         echo \"\\e[<number>mHello world\"' "
  print "\n\n  * Code #8 may not be visible because it is \"concealed\"."
}

section("8-COLOR PALETTE #30-37") {
  print "\n  Foregrounds:  "
  (30..37).each {|color| printColorCode color}
  print "\n  Backgrounds:  "
  (40..47).each {|color| printColorCode color}
  print "\n  Usage:         echo \"\\e[<number>mHello world\"' "
}

section("256-COLOR PALETTE") {
  printColorsForPage "  Foregrounds (use 'echo \"\\e[38;5;<number>mHello world\"')\n", 38
  print "\n"
  printColorsForPage "  Backgrounds (use 'echo \"\\e[48;5;<number>mHello world\"')\n", 48
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
