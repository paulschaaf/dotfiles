#!/usr/bin/env ruby

# program Cradle

<<PORTPASCAL
; Port selected code to Ruby
(progn
  (defun port-pascal ()
    (interactive)
    (ruby-mode)

    ; fix individual characters
    (replace-all-regex "’"                       "'")
    (replace-all-regex ""                      "*")

    ; minus sign is 􀀀, or \364\200\200\200
    (replace-all-regex "[^A-Za-z0-9~!@#$\%^&*(){}\|;:',./<>?`]+"
                       "-")

    ; comments
    (replace-all-regex "{\\(.*\\)}"       "#\\1")
    (replace-all-regex "^#"               "\n#")

    (replace-all-regex "[ ;]+$"           "") ; no trailing space or semicolon

    (replace-all-regex "\\(procedure\\|function\\) +\\(.*\\)\nbegin"
                       "def \\2")


    ; statements
    (replace-all-regex "while \\(.*\\) in \\(.*\\) do begin"
                       "if \\2.include?(\\1)"        t)

    (replace-all-regex "case \\(.*\\) of" "case \\1")
    (replace-all-regex "\\('[^']*'\\) : \\(.*\\)" "when \\1 then \\2")

    ; functions
    (replace-all-regex "Add"              "add"      t)
    (replace-all-regex "Divide"           "divide"   t)
    (replace-all-regex "EmitLn"           "emitLn"   t)
    (replace-all-regex "Expected"         "expected" t)
    (replace-all-regex "Factor"           "factor"   t)
    (replace-all-regex "GetNum"           "getNum"   t)
    (replace-all-regex "Look"             "$look"    t)
    (replace-all-regex "Match"            "match"    t)
    (replace-all-regex "M ?u ?l ?t ?i ?p ?l ?y ?"     "multiply" t)
    (replace-all-regex "Subt ?ract"       "subtract" t)
    (replace-all-regex "Term"             "term"     t)

    ; trim whitespace
    (replace-all-regex " *\\((\\|)\\) *"         "\\1")   ;-parens & brackets
    (replace-all-regex " ?\\(*\\|/\\|+\\|-\\) ?" "\\1")   ;-arithmetic ops
    (replace-all-regex "\\([^ ]\\){"
                       "   \\1 {")                        ;+before block arg
    (replace-all-regex "(' "                     "('")    ;-start of string
    (replace-all-regex " ')"                     "')")    ;-end of string
    (replace-all-regex ",\\([^ ]\\)"             ", \\1") ;+after comma
    (replace-all-regex "( +("                    "((")    ;-between parens
    (replace-all-regex ") +)"                    "))")    ;-between parens

    (indent-buffer))

  (global-set-key (kbd "C-c q") 'port-pascal))
'
PORTPASCAL


###############################
# Variable Declarations
###############################

$look = ''  # lookahead Character

def getChar
  $look = $stdin.readchar.chr
end

def error(s); $stderr.puts("\n### Error: " + s + '.'); end

def abort(s)
  error(s)
  exit(1)
end

def expected(s); abort(s + ' Expected'); end

def expect(type)
  test = case type
         when :Name    then $look.alpha?
         when :Integer then $look.digit?
         end
  expected(type) unless test
end

def match(x)
  ($look == x) ? getChar : (expected('"' + x + '"'))
end


###############################
# Recognize a character
###############################

class Fixnum
  def alpha?; return (?A..?Z).include?(self.upcase); end
  def digit?; return (?0..?9).include?(self);        end
end


###############################
# Recognize a word
###############################

# Get an Identifier
def getName
  # expect(:Name)
  if not $look.alpha?
    expected('Name')
  end
  answer = $look.upcase
  getChar
  return answer
end

def getNum
  # expect(:Integer)
  if not $look.digit?
    expected('Integer')
  end
  answer = $look
  getChar
  return answer
end

# Output a String with Tab
def emit(s); puts("\t", s); end

alias :emitLn :puts


###############################
# phrases
###############################

def factor
  if $look == '('
    match('(')
    expression
    match(')')
  else
    emitLn('MOVE #' + getNum + ' , D0')
  end
end

def term
  factor
  if [ '*' , '/' ].include?($look)
    emitLn('MOVE D0, -(SP)')
    case $look
    when '*' then multiply
    when '/' then divide
    else expected('Mulop')
    end
  end
end

def expression
  term
  if [ '+' , '-' ].include?($look)
    emitLn('MOVE D0, -(SP)')
    case $look
    when '+' then add
    when '-' then subtract
    else expected('addop')
    end
  end
end


###############################
# Arithmetic
###############################

def add
  match('+')
  term  emitLn('ADD(SP)+, D0')
end

def subtract
  match('-')
  term
  emitLn('SUB(SP)+, D0')
  emitLn('NEG D0')
end

def multiply
  match('*')
  factor
  emitLn('MULS(SP)+, D0')
end

def divide
  match('/')
  factor
  emitLn('MOVE(SP)+, D1')
  emitLn('DIVS D1, D0')
end


###############################
# Main Program
###############################

def init
  getChar
end

init
expression
