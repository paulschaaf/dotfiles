#!/bin/bash

cd ~pschaaf

# these get installed first, in the order defined
ordered_packages=(
    git
    zsh
    google-chrome-stable
    openssh-client
    openssh-server
)

git clone git@github.com:paulschaaf/dotfiles.git

# These are installed after the ordered_packages. Keep them sorted for convenience
unordered_packages=(
    amarok
    ascii
    cairo-dock
    calibre
    emacs
    k4dirstat
    keychain
    lynx
    meld
    playonlinux
    ruby
    screen
    silversearcher-ag
    slack
    virtualbox
    wine
)

all_packages=(${ordered_packages[@]} ${unordered_packages[@]})

for pkg in ${all_packages[@]}; do
    echo apt-get install $pkg
done

# I only need 2 ttys (CTRL-ALT-F1 and CTRL-ALT-F2)
perl -pi -e "s/(ACTIVE_CONSOLES=\"\/dev\/tty\[1)-6/\1-2/g" /etc/default/console-setup

