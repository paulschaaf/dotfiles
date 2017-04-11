#!/bin/bash

# these get installed first, in the order defined
ordered_packages=(git zsh)

# These are installed after the ordered_packages. Keep them sorted for convenience
unordered_packages=(
  ascii
  emacs
  lynx
  screen
  silversearcher-ag
)

all_packages=(${ordered_packages[@]} ${unordered_packages[@]})

for pkg in ${all_packages[@]}; do
    echo apt-get install $pkg
done

# I only need 2 ttys (CTRL-ALT-F1 and CTRL-ALT-F2)
sudo perl -pi -e "s/(ACTIVE_CONSOLES=\"\/dev\/tty\[1)-6/\1-2/g" /etc/default/console-setup

