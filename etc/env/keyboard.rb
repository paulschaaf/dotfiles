# ================================================
# ======= Key Definitions

xterm, rxvt = linux, false

# ======= Escape

$ESC = {
  any  => 033.chr,
  zsh  => '\033',
}[true]

(Keymap = Hash.new)[:esc]=$ESC

# ======= Modifiers

# to require modifier keys, usually insert the code before the last char
# CODE  MODIFIER KEY(S)
# ;2             shift
# ;3         alt
# ;4         alt shift
# ;5    ctrl
# ;6    ctrl     shift
# ;7    ctrl alt
# ;8    ctrl alt shift

Keymap[:SHIFT] = ';2'
Keymap.add_successors_of_value_at(:SHIFT, :ALT,        :ALT_SHIFT,
                                  :CTRL,  :CTRL_SHIFT, :ALT_CTRL, :ALT_CTRL_SHIFT)

def modify(keyname, with)
  keystroke, mod = Keymap[keyname], Keymap[with]
  case keyname
  when :up, :down, :right, :left
    mod = "1#{mod}"
  else
  end
  keystroke.dup.insert(-2, mod)
end

def alt(key);   modify(key, :ALT);   end
def ctrl(key);  modify(key, :CTRL);  end
def shift(key); modify(key, :SHIFT); end

# ======= Function Keys

FunKeys = Array.new

def FunKeys.[]=(num, val)
  Keymap["F#{num}".to_sym] = val
  super
end

if screen_term
  %w(k1 k2 k3 k4 k5 k6 k7 k8 k9 k; F1 F2).inject(1) {|num, keyname|
    FunKeys[num] = "-k #{keyname}"
    num + 1
  }
else
  #codes = %w([[A [[B [[C [[D [[E) # if term == :cygwin
  codes = %w( OP  OQ  OR  OS [15) # if term == :xterm under cygwin

  codes += %w([17 [18 [19 [20 [21 [23 [24)

  codes.inject(1) {|num, code|
#   %w(A B C D E 17 18 19 20 21 23 24).inject(1) {|num, code|
    FunKeys[num] = "#{$ESC}#{code}"
    FunKeys[num] += '~' #unless cygwin
    num + 1
  }
end

# ======= Arrow Keys

Keymap[:up]      = "#{$ESC}[A"
Keymap[:alt_up]  = alt( :up)
Keymap[:ctrl_up] = ctrl(:up)

Keymap.add_successors_of_value_at(     :up,      :down,      :right,      :left)
Keymap.add_successors_of_value_at( :alt_up,  :alt_down,  :alt_right,  :alt_left)
Keymap.add_successors_of_value_at(:ctrl_up, :ctrl_down, :ctrl_right, :ctrl_left)


# ======= Command Keys

Keymap[:home]    = "#{$ESC}[1~"
Keymap.add_successors_of_value_at(:home, :insert, :delete, :end, :page_up, :page_down)

# if cygwin and not screen_term
#   Keymap[:home]  = "#{$ESC}[H"
#   Keymap[:end]   = "#{$ESC}[F"
# end

Keymap[:ctrl_delete] = {
  any    => ctrl(:delete),
  rxvt   => "#{$ESC}[3^",
  linux  => nil,
  cygwin => "#{$ESC}[3;5~",
}[true]

Keymap[:alt_backspace]  = {
  any    => nil,
  cygwin => "#{$ESC}^È",
}[true]

Keymap[:ctrl_backspace] = 0177.chr

# Keymap[:alt_tab] = {
#   any    => "#{$ESC}\t",
#   cygwin => nil,
# }[true]

Keymap[:shift_tab] = {
  any    => '-k kB',
  cygwin => "#{$ESC}[Z", # "#{$ESC}OI",
}[true]

Keymap[:exit] = 'exit'


# ======= Windows Keys

Keymap[:windows] = {
  any    => "#{$ESC}[25~",
  cygwin => nil,
}[true]

Keymap[:menu] = {
  any    => "#{$ESC}[29~",
  cygwin => nil # "#{$ESC}[21;2~",
}[true]
