#!/usr/bin/env ruby
# coding: utf-8

#require 'matrix'
Lookup = (ARGV[0] || 0).to_i
$success = Lookup == 0
Pad = 4
GridSize = 6

def getColorString(code, text=code, pad=Pad)
  if text == Lookup
    $success = true
    pad -= 1
    print "[5m>[0m"
  end
  return sprintf "[%sm%#{pad}s[0m", code, text
end

def printColorString(code, text=code, pad=Pad)
  printf getColorString code, text, pad
end

def section(header)
  puts
  printColorString 4, header + "\n"
  yield
  puts
end

# An Easter Egg!
if ARGV[0] == "--rainbow"
  cover = "
   ┌──────────────┐
   │ \e[38;5;227mIN/ RAINBOWS\e[0m │
   │ \e[38;5;033mIN RAIN/BOWS\e[0m │
   │ \e[38;5;202mIN RAINBOW/S\e[0m │
   │ \e[38;5;034mIN RAINBOWS/\e[0m │
   │ \e[38;5;215mIN RAIN_BOWS\e[0m │
   │ \e[38;5;196mRA D IOHEA_D\e[0m │
   │ \e[38;5;153m_RAD IOHEA D\e[0m │
   └──────────────┘
https://www.youtube.com/watch?v=HeHqt9YcVDs&t=29s
"
  print cover
#  print "\necho \"", cover.gsub(%r(\e), '\e'), "\"\n"
  exit
end


section('EFFECTS (0..9, 21..29)') {
  effects = {0=>'-unset all-', 1=>'Bold', 2=>'[Dim]', 3=>'[Italic]', 4=>'Underline', 5=>'Blink', 6=>'[Fast blink]', 7=>'Inverse', 8=>'[Concealed]', 9=>'[Strikeout]'}
  pad = effects.values.map {|e| e.strip.length}.max + 1
  effects.each {|effectNum, effectName|
    printf '%4d: ', effectNum
    printColorString effectNum, effectName, -pad
    puts if (effectNum+1) % 4 == 0
  }
  puts <<STRING
21-9: [Unset effect]

  Bracketed effects are not widely supported. Effect #8 may be invisible as
  it is "concealed".

  Usage:  echo "\\e[<number>mHello world"
STRING
}

section('8-COLOR PALETTE (30..37, 90..97, 40..47, 100..107)') {
  [0, 10].each {|ground|
    { 0 => 'Dim', 60 => 'Bright' }.each {|intensity, intensityName|
      printf '  %-7s ', intensityName
      (30..37).each {|cell|
        color = ground + intensity + cell
        printColorString color
      }
      puts
    }
  }
  puts "\n  Usage:  echo \"\\e[<color>mHello world\"' "
}

section('256-COLOR PALETTE (16..255)') {
  [38, 48].each {|ground|
    rowCounts = (0...GridSize).to_a
    (0..2).each {|section|
      rowCounts.each {|row|
        print '  '
       [16, 22, 28, 34, 40, 46, 82, 76, 70, 64, 58, 52, 16].each {|cell|
          color = cell + row + (72*section)
          colorCode = "#{ground};5;#{color}"
          colorCode = '30;' + colorCode if [40, 46, 82, 76].include?(cell)
          printColorString colorCode, color
        }
       puts
      }
      rowCounts = rowCounts.reverse if section == 0
    }

    # now do the grayscales
    print '  '
    (0..3).each {|row|
      (232..237).each {|cell|
        color = cell + (row*GridSize)
        printColorString "#{ground};5;#{color}", color
      }
      print "\n  " if row == 1
    }
    puts
  }

  puts <<STRING

  Usage:
    Foreground 'echo "\\e[38;5;<color>mHello world"'
    Background 'echo "\\e[48;5;<color>mHello world"'
STRING

  # truecolor
  # [48].each {|ground|
  #   (0x0..0xf).each {|red|
  #     printf "\n\nR-%x0", red
  #     (0x0..0xf).each {|green|
  #       printf "\nG-%x0 ", green
  #       (0x0..0x1f).each {|blue|
  #         code="#{ground};2;#{red}0;#{green}0;#{blue}"
  #         printf "[%sm%.x[0m", code, blue
  #       }
  #     }
  #   }
  #   puts
  # }
}

def printSamples(codeToDesc)
  codeToDesc.each {|code, desc|
    print '  echo "\e[' + code + 'm'
    printColorString code, desc
    puts '"'
  }
end

section('APPLICATION') {
  puts '  Begin your text with "\e[<effect*>;<foreground>;<background>m"', ''

  printSamples({
    '91'               => 'Bright red foreground (short 8-color notation)',
    '38;5;94'          => 'Brown foreground',
    '38;5;94;48;5;33'  => 'Brown foreground, light blue background',
    '1;4;38;5;11'      => 'Gold foreground, boldface underlined',
    '38;5;171;48;5;11' => 'Pink foreground, gold background'
  })
}

abort "[101mERROR:[0m No such color #{Lookup}!" unless $success
