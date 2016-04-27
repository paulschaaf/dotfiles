#!/usr/bin/env ruby

require 'matrix'

GridSize = 6

def getColorString(code, text=code, pad=4)
  return sprintf "[%sm%#{pad}s[0m", code, text
end

def printColorString(code, text=code, pad=4)
  printf getColorString code, text, pad
end

def section(header)
  puts
  printColorString 4, header
  puts "\n"
  yield
  puts
end

section('EFFECTS (0..9, 21..29)') {
  effects = %w{-unset\ all- Bold [Dim] [Italic] Underline Blink [Fast\ blink] Inverse [Concealed[0m]*  [Strikeout]}
  pad = effects.max {|e| e.length}.length + 1
  (0...effects.length).each {|effectNum|
    print "  #{effectNum}: "
    printColorString effectNum, effects[effectNum], -pad
    puts if (effectNum+1) % 4 == 0
  }
  print "  2X: Unset effect number X"
  puts <<STRING

     * Effect #8 may be invisible as it is "concealed".

  Bracketed effects are not widely supported.

  Usage:  echo "\\e[<number>mHello world"
STRING
}

section('8-COLOR PALETTE (30..37, 90..97, 40..47, 100..107)') {
  { 0 => 'Foreground', 10 => 'Background' }.each {|ground, groundName|
    { 0 => 'Dim', 60 => 'Bright' }.each {|intensity, intensityName|
      printf '  %-7s ', intensityName
      (30..37).each {|cell|
        color = ground + intensity + cell
        printColorString color
      }
      puts
    }
    # puts
  }
  puts "\n  Usage:  echo \"\\e[<number>mHello world\"' "
}

section('256-COLOR PALETTE (16..255)') {
  [38, 48].each {|ground|
    rowCounts = (0...GridSize).to_a
    (0..2).each {|section|
      rowCounts.each {|row|
        print '  '
       [16, 22, 28, 34, 40, 46, 82, 76, 70, 64, 58, 52, 16].each {|cell|
       # [16, 22, 28, 34, 40, 46, 82, 76, 70, 64, 58, 52].each {|cell|
          color = cell + row + (72*section)
          colorCode = "#{ground};5;#{color}"
          colorCode = '30;' + colorCode if [40, 46, 82, 76].include?(cell)
          printColorString colorCode, color
        }
       puts
      }
      rowCounts = rowCounts.reverse if section == 0
    }
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
    Foreground 'echo "\\e[38;5;<number>mHello world"'
    Background 'echo "\\e[48;5;<number>mHello world"'
STRING
}

def printSamples(codeToDesc)
  codeToDesc.each {|code, desc|
    print "  echo \"\\e[#{code}m"
    printColorString code, desc
    puts '"'
  }
end

section('USAGE') {
  puts '  Begin your text with "\e[<effect*>;<foreground>;<background>m"', ''

  printSamples( {
    '91'               => 'Bright red foreground',
    '48;5;12'          => 'Light blue background',
    '38;5;94'          => 'Brown foreground',
    '1;4;38;5;11'      => 'Boldface underlined gold foreground',
    '38;5;171;48;5;11' => 'Pink foreground, gold background'
  })
}

showColor = ARGV[0] != '-n'

#
# CONSTRUCT THE VALUES
#
# sections = []
# [38, 48].each {|ground|
#   (0...GridSize).each {|sectionNum|
#     section = []
#     (0...GridSize).each {|rowNum|
#       row = []
#       (0...GridSize).each {|cellNum|
#         color = 16 + cellNum + (GridSize * (rowNum + (sectionNum*GridSize)))
#         colorCode = "#{ground};5;#{color}"
#         colorCode += ';0' if not showColor
#         row.push(getColorString colorCode, color)
#       }
#       # if sectionNum % 2 == 1 then
#       #   section.unshift(row)
#       # else
#         section.push(row)
#       # end
#     }
#     sections.push((section))
#   }
# }
#sections = Matrix[*sections]

#
# DISPLAY THE VALUES
#

# Naive linear
# sections.each_with_index {|section, sectionIndex|
#   section.each_with_index {|cell, cellIndex|
#     print cell.join
#     puts if cellIndex+1 == GridSize/2
#   }
#   puts
# }
