#!/usr/bin/env ruby

Lookup = (ARGV[0] || 0).to_i
$success = Lookup == 0
Pad = 4
GridSize = 6
BlackBackground = "\e[0;48;5;16m"

def printColorString(code, text = code, pad = Pad)
  open, close = '', ''
  if text == Lookup # if we're about to draw the one the user asked about
    $success = true
    pad -= 1 # back up one
    print "\e[5m>#{BlackBackground}" # add a blinking carat
  elsif text[0] == '[' # if the text is in brackets
    text = text[1..-2] # remove them
    pad += 2
    open, close = '[', ']'
  end
  printf "#{open}\e[%sm%#{pad}s#{BlackBackground}#{close}", code, text
end

def section(header)
  puts "#{BlackBackground}"
  printColorString 4, header + "\n"
  yield
  puts
end

# An Easter Egg!
if ARGV[0] == "--rainbows"
  clr = "\e[0m"

  def fg(c)
    "\e[38;5;#{c}m"
  end

  def bg(c)
    "\e[48;5;#{c}m"
  end

  yel = fg(227)
  blu = fg(33)
  ora = fg(202)
  gre = fg(034)
  btr = fg(214)
  red = fg(196)
  lBl = fg(81)
  blk = "#{BlackBackground}"
  link = "https://tinyurl.com/RadioheadInRainbows"

  cover = [
      '',
      "┌────────────┐",
      "│#{yel}IN/ #{bg 27}R#{bg 16}AINB#{bg 208}O#{bg 16}WS#{blk}│",
      "│#{blu}IN#{bg 27} #{bg 16}RAIN/#{bg 208}BOW#{bg 16}S#{blk}│",
      "│#{ora}I#{bg 27}N#{bg 16} RA#{bg 229}I#{bg 16}N#{bg 166}BOW#{bg 16}/S#{blk}│",
      "│#{gre}I#{bg 27}N#{bg 16} RA#{bg 172}I#{bg 208}NB#{bg 166}O#{bg 208}W#{bg 16}S/#{blk}│#{clr}   #{fg "33;4"}#{link}\e[0m",
      "│#{btr}I#{bg 27}N#{bg 16} RAI#{bg 172}N#{bg 208}_B#{bg 16}OWS#{blk}│",
      "│#{red}R#{bg 16}A#{bg 27} #{bg 16}D I#{bg 172}O#{bg 16}HEA_D#{blk}│",
      "│#{lBl}_R#{bg 27}A#{bg 16}DIO HEA D#{blk}│",
      "└────────────┘\n",
  ]
  cover = cover.join("#{clr}\n     #{bg 16}")
  print cover

elsif ARGV[0] == "--24"
  # truecolor
  [48].each { |ground|
    (0x0..0xf).each { |red|
      printf "\n\nR-%x0", red
      (0x0..0xf).each { |green|
        printf "\nG-%x0 ", green
        (0x0..0xf).each { |blue|
          code = "#{ground};2;#{red}0;#{green}0;#{blue}"
          printf "\e[%sm%.x\e[0m", code, blue
        }
      }
    }
    puts
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
  section('EFFECTS (0..9, add 20 to unset effect)') {
    # print effect names
    effects = {0 => '-unset all-', 1 => 'Bold', 2 => '[Dim]', 3 => '[Italic]', 4 => 'Underline', 5 => 'Blink', 6 => '[Fast blink]', 7 => 'Inverse', 8 => '[Concealed]', 9 => '[Strikeout]'}
    pad = effects.values.map { |e| e.strip.length }.max + 1
    effects.each { |effectNum, effectName|
      printf '%4d: ', effectNum
      printColorString effectNum, effectName, -pad
      puts if (effectNum + 1) % 4 == 0
    }
    puts <<STRING


  Bracketed effects are not widely supported. #2 is "Dim" and #8 is "concealed".

  Usage:  echo "\\e[<number>mHello world"
STRING
  }

  section('8-COLOR PALETTE (30..37, 90..97, 40..47, 100..107)') {
    [0, 10].each { |ground|
      {0 => 'Dim', 60 => 'Bright'}.each { |intensity, intensityName|
        printf '  %-7s ', intensityName
        (30..37).each { |cell|
          color = ground + intensity + cell
          printColorString color
        }
        puts
      }
    }
    puts "\n  Usage:  echo \"\\e[<color>mHello world\"' "
  }

  section('256-COLOR PALETTE (16..255)') {
    foreground, background = 38, 48
    [foreground, background].each { |ground|
      puts if ground == background
      rowCounts = (0...GridSize).to_a
      (0..2).each { |section|
        rowCounts.each { |row|
          print '  '
          [16, 22, 28, 34, 40, 46, 82, 76, 70, 64, 58, 52, 16].each { |cell|
            color = cell + row + (72 * section)
            colorCode = "#{ground};5;#{color}"
            colorCode = '30;' + colorCode if ground == background and [40, 46, 82, 76].include?(cell) # set foreground color to black for readability
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
          color = cell + (row * GridSize)
          colorCode = (ground == background and color >= 250) \
            ? "#{color};30" # set fg to black when bg nears solid white
                          : color
          printColorString "#{ground};5;#{colorCode}", color
        }
        print "\n  " if row == 1
      }
      puts <<STRING


  echo "\\e[#{ground};5;\e[4mcolor\e[0mmHello world";
  print -P "%#{ground == foreground ? 'F' : 'K'}{\e[4mcolor\e[0m}Hello world%#{ground == foreground ? 'f' : 'k'}"
STRING
    }
  }

  section('APPLICATION') {
    puts '  Begin your text with "\e[<effect*>;<foreground>;<background>m"', ''

    {'91' => 'Bright red foreground (short 8-color notation)',
     '38;5;94;48;5;33' => 'Brown foreground, light blue background',
     '1;4;38;5;11' => 'Bold underlined, gold foreground',
    }.each { |code, desc|
      print '  echo $\'\e[' + code + 'm'
      printColorString code, desc
      puts "'"
    }
  }

  abort "\e[101mERROR:\e[0m No such color #{Lookup}!" unless $success
end
