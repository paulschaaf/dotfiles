#!/bin/bash
cd ~pschaaf

# these get installed first, in the order defined
ordered_packages=(
    git                  # version control
    zsh                  # the best shell
    google-chrome-stable # the best browser
    openssh-client       # make secure remote connections
    openssh-server       # host secure remote connections
    wine                 # run windows programs
)

# These are installed after the ordered_packages. Keep them sorted for convenience
unordered_packages=(
    amarok            # music
    ascii             # tree of ascii codes
    cairo-dock        # Mac-like icon dock
    calibre           # e-book manager
    davfs2            # mount box.com into filesystem
    emacs             # the king of editors
    k4dirstat         # disk usage report
    keychain          # ssh key organizer
    lynx              # command line browser
    meld              # merge tool
    playonlinux       # addons for wine
    ruby              # programming language
    screen            # terminal multiplexer
    silversearcher-ag # quicker grep
    slack             # IM client
    virtualbox        # VM manager
)

unofficial_ppas=(
    webupd8team/java
)

unofficial_packages=(
    oracle-java7-installer
    oracle-java8-installer
    oracle-java9-installer
)

git_repos=(
    git@github.com:paulschaaf/dotfiles.git
)

## FUNCTIONS ===========================================
function @#() {
    printf "\e[48;5;21m\n--- $*\e[0m\n"
}

function apt-get-all() {
    eval local packages=\${${*}[@]}
    @# Install $*: $packages
    for package in ${packages[@]}; do
        echo sudo apt-get install $package
    done
}

## ====================================================

@# PACKAGE INSTALLATION ================================
apt-get-all ordered_packages
apt-get-all unordered_packages

for ppa in ${unofficial_ppas[@]}; do
    @# Add PPA $ppa
    sudo add-apt-repository ppa:${ppa}
done

@# Update Package Cache
sudo apt-get update

apt-get-all unofficial_packages


@# SYSTEM SETUP ========================================
@# Add DavFS to fstab
cp /etc/fstab ~/fstab.bak
echo 'https://dav.box.com/dav/ /home/pschaaf/box  davfs  rw,user,noauto 0 0' | sudo tee --append /etc/fstab > /dev/null

@# Remove ttys beyond CTRL-ALT-F1 and CTRL-ALT-F2
#sudo perl -pi -e "s/(ACTIVE_CONSOLES=\"\/dev\/tty\[1)-6/\1-2/g" /etc/default/console-setup

@# USER SETUP ==========================================

@# Downloading Git Dotfiles repository
for repo in ${git_repos[@]}; do
    git clone $repo
done

if [ -d .davfs2 ]; then
    @# Setup DavFS2 for Box.com access
    cp -r /etc/davfs2 .davfs2
    sudo adduser pschaaf davfs2
    ( cd .davfs2;
      echo -n 'Please type your box.com password:'
      read -s password; echo
      umask 077;
      echo "https://dav.box.com/dav paul666survey@gmail.com \"$password\"" >> secrets
    )
fi
