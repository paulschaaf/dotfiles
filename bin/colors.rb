#!/usr/bin/env ruby

LOOKUP = ARGV[0] ? ARGV[0].to_i : nil

abort "ERROR: Color #{LOOKUP} is out of range. It must be between 0 and 255!" if LOOKUP and !(0..255).member? LOOKUP

PAD = 4
GRIDSIZE = 6

def effect(n)
  "\e[#{n}m"
end

BLINK = effect 5
CLR = effect 0
DEFAULT_BACKGROUND = 16
INVERSE = effect 7
UNDERLINE = effect 4
#noinspection RubyConstantNamingConvention
FG, BG = 38, 48

def fg(c)
  effect "#{FG};5;#{c}"
end

def bg(c)
  effect "#{BG};5;#{c}"
end

if ARGV[0] == "--background"
  DEFAULT_BACKGROUND = ARGV[1]
  ARGV.shift(2)
end


RESETBACKGROUND = CLR + bg(DEFAULT_BACKGROUND)

def printColorString(code, text = code, pad = PAD)
  open, close = '', ''
  if text[0] == '[' # if the text is in brackets
    text = text[1..-2] # extract them so they don't get formatted
    open, close = '[', ']'
    pad += 2
  elsif text == LOOKUP # if this is the color the user asked about
    pad -= 1 # back up one
    open = "#{BLINK}>#{RESETBACKGROUND}" # and insert a blinking carat instead of the space
  end
  text = sprintf "%#{pad}s", text
  print open, effect(code), text, RESETBACKGROUND, close
end

def section(header, subHeader = '')
  puts "#{RESETBACKGROUND}"
  printColorString 4, header
  printColorString 0, " #{subHeader}\n"
  yield
  puts
end

# An Easter Egg!
if ARGV[0] == "--rainbows"

  yel = fg(227)
  blu = fg(33)
  ora = fg(202)
  gre = fg(034)
  btr = fg(214)
  red = fg(196)
  lBl = fg(81)
  blk = "#{RESETBACKGROUND}"
  link = "https://tinyurl.com/RadioheadInRainbows"

  print <<STRING

     ┌────────────┐
     │#{yel}IN/ #{bg 27}R#{bg 16}AINB#{bg 208}O#{bg 16}WS#{blk}│
     │#{blu}IN#{bg 27} #{bg 16}RAIN/#{bg 208}BOW#{bg 16}S#{blk}│
     │#{ora}I#{bg 27}N#{bg 16} RA#{bg 229}I#{bg 16}N#{bg 166}BOW#{bg 16}/S#{blk}│
     │#{gre}I#{bg 27}N#{bg 16} RA#{bg 172}I#{bg 208}NB#{bg 166}O#{bg 208}W#{bg 16}S/#{blk}│#{CLR}   #{fg "33;4"}#{link}#{CLR}
     │#{btr}I#{bg 27}N#{bg 16} RAI#{bg 172}N#{bg 208}_B#{bg 16}OWS#{blk}│
     │#{red}R#{bg 16}A#{bg 27} #{bg 16}D I#{bg 172}O#{bg 16}HEA_D#{blk}│
     │#{lBl}_R#{bg 27}A#{bg 16}DIO HEA D#{blk}│
     └────────────┘

STRING

elsif ARGV[0] == "--24"
  # truecolor
  [BG].each { |ground|
    (0..23).step(2).each { |redSet|
      (0..23).each { |green|
        puts
        [redSet, redSet + 1].each {|red|
          printf("%s%02i%s%02i#{CLR}", effect(91), red, effect(92), green)
          (0..23).each { |blue|
            printf " \e[#{ground};2;#{red}0;#{green}0;#{blue}0m%02i", blue
          }
          print(CLR, ' ')
        }
      }
    }
    puts
  }

  [FG, BG].each { |ground|
    printf(ground == FG ? '  Foreground: ' : '  Background: ')
    puts "echo $'\\e[#{ground};2;#{UNDERLINE}rr#{CLR}0;#{UNDERLINE}gg#{CLR}0;#{UNDERLINE}bb#{CLR}0mSome Text'"
  }

elsif ARGV[0] == "--sort"
  cmd = ''
  ([%w(%F %f), %w(%K %k)]).each { |ground|
    (0x0..0xf).each { |page|
      cmd = 'print -nP "  '
      (0x0..0xf).each { |shade|
        color = page * 16 + shade
        cmd += sprintf("%s{%3s}%4s%s", ground[0], color, color, ground[1])
      }
      cmd += '"'
      output = `zsh -c '#{cmd}'`
      puts output
    }
    puts
  }

elsif ARGV[0] == "--help"
  puts <<STRING
Usage: colors.rb [--24] [--sort] [number]
  When passed a number without a switch, highlight that number in the 256 color table

  --24 Show 24-bit color display
  --sort Show the 256 color codes in numeric order
STRING

else
  section('EFFECTS', '(0..9, add 20 to unset effect)') {
    # print effect names
    effects = {0 => '-unset all-', 1 => 'Bold', 2 => '[Dim]', 3 => '[Italic]', 4 => 'Underline', 5 => 'Blink', 6 => '[Fast blink]', 7 => 'Inverse', 8 => '[Concealed]', 9 => '[Strikeout]'}
    pad = effects.values.map { |e| e.strip.length }.max
    effects.each { |effectNum, effectName|
      printf '%3d: ', effectNum
      printColorString effectNum, effectName, -pad
      puts if (effectNum + 1) % 4 == 0
    }
    puts <<STRING


  2 is "Dim" and 8 is "concealed". Bracketed effects are not widely supported.

  Usage:  echo "\\e[#{INVERSE}color#{CLR}mHello world"
STRING
  }

  section('8-COLOR PALETTE', '(30..37, 40..47, 90..97, 100..107)') {
    [0, 10].each { |ground|
      {0 => 'Reg', 60 => 'Bright'}.each { |intensity, intensityName|
        printf '  %-7s ', intensityName
        (30..37).each { |cell|
          color = ground + intensity + cell
          printColorString color
        }
        puts
      }
    }
    puts "\n  Usage:  echo \"\\e[#{INVERSE}color#{CLR}mHello world\""
  }

  section('256-COLOR PALETTE', '(0..255)') {
    [FG, BG].each { |ground|
      print("\n  #{UNDERLINE}Page #{ground}: #{ground == FG ? 'Fore' : 'Back'}grounds#{RESETBACKGROUND}\n\n")
      [0, 8].each { |page|
        print '  '
        (0..7).each { |col|
          color = col + page
          colorCode = "#{ground};5;#{color}"
          printColorString colorCode, color
        }
        puts
      }
      rowCounts = (0...GRIDSIZE).to_a
      (0..2).each { |section|
        rowCounts.each { |row|
          print '  '
          [16, 22, 28, 34, 40, 46, 82, 76, 70, 64, 58, 52, 16].each { |cell|
            color = cell + row + (72 * section)
            colorCode = "#{ground};5;#{color}"
            colorCode = '30;' + colorCode if ground == BG and [40, 46, 82, 76].include?(cell) # set foreground color to black for readability
            printColorString colorCode, color
          }
          puts
        }
        rowCounts = rowCounts.reverse
      }

      # now do the grayscales
      print '  '
      shades = (232..237).to_a
      [0, 1, 3, 2].each { |row|
        shades = shades.reverse if row == 2
        shades.each { |cell|
          color = cell + (row * GRIDSIZE)
          colorCode = (ground == BG and color >= 250) \
            ? "#{color};30" # set fg to black when bg nears solid white
                          : color
          printColorString "#{ground};5;#{colorCode}", color
        }
        print "\n  " if row == 1
      }
      puts <<STRING


  echo "\\e[#{ground};5;#{INVERSE}color#{CLR}mHello world";
  print -P "%#{ground == FG ? 'F' : 'K'}{#{INVERSE}color#{CLR}}Hello world%#{ground == FG ? 'f' : 'k'}"
STRING
    }
  }

  section('APPLICATION') {
    puts '  Begin your text with "\e[<effect>;<foreground>;<background>m"', ''

    {
     '1;93' => 'Bold light yellow foreground (short 8-color notation)',
       '93' => '       Light yellow foreground (short 8-color notation)',
     '2;93' => 'Dim  light yellow foreground (short 8-color notation)',
     '1;33' => 'Bold dark  yellow foreground (short 8-color notation)',
       '33' => '       dark  yellow foreground (short 8-color notation)',
     '2;33' => 'Dim  dark  yellow foreground (short 8-color notation)',
     "#{FG};5;94;#{BG};5;33" => 'Brown foreground, light blue background',
     "1;4;#{FG};5;57" => 'Bold underlined, purple foreground',
    }.each { |code, desc|
      print '  echo $\'\e[' + code + 'm'
      printColorString code, desc
      puts "'"
    }
  }
end
