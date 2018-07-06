#!/bin/bash
# -*- eval: (progn (highlight-regexp "\\bheader[1-9]+\\b" 'hi-green-b) (highlight-regexp "\\bh1\\b.*" 'header1) (highlight-regexp "\\bh2\\b.*" 'header2) ) -*-
cd $HOME
backup=~/backup
shell=/bin/zsh

if [ "$host" = "copernicus" ]; then
    yay -S broadcom-wl
fi

## LIBRARIES AND PACKAGES ===========================================
libraries=(
#    libimage-exiftool-perl # edit EXIF data in image
#    libncurses
#    libncurses-dev
#    libevent
)

# these get installed first, in the order defined
ordered_packages=(
    mlocate
    the_silver_searcher  # quicker grep
    chromium             # the best browser
    git                  # version control
    tmux                 # terminal multiplexor
    byobu                # tmux/screen enhancements
    cairo-dock           # Mac-like icon dock
    cairo-dock-plug-ins  # Mac-like icon dock
)

# These are installed after the ordered_packages. Keep them sorted for convenience
unordered_packages=(
    ascii                # tree of ascii codes
    atom
    bcompare
#    davfs2                # mount box.com into filesystem
#    devede                # make video DVDs
    dolphin-plugins       # git integration into KDE file manager
    emacs                 # the king of editors
    enscript              # convert txt to ps, html, rtf, etc.
    inxi                  # system info tool
    jdk-dcevm             # Java Dynamic Code Evolution VM
    kdiff3
    keychain              # ssh key organizer
    lsof
    lynx                  # command line browser
    manjaro-i3-solarized-settings
#    mp3fs                 # MP3 virtualf ilesystem
    nerd-fonts-fira-code # programming font with ligatures
    nmap
    oracle~jdk7           # Java 1.7
    oracle~jdk8           # Java 1.8
    sqlops                # MS SQL Server admin tool
    tree
    units                 # unit conversions
    vlc                   # video player
    xorg-xkill
)

applications=(
    amarok                # music
    calibre               # e-book manager
    filezilla             # file transfers
    freecad
    frostwire             # bittorrent client
    gitkraken
    masterpdfeditor4
    slack-desktop         # IM client
    sweethome3d           # architectural modelling
    virtualbox            # VM manager
    virtualbox-ext-oracle
    virtualbox-guest-iso
)

# Use URL query-style syntax: e.g. double-quote any phrases and replace the spaces with plusses
manually_install=(
)

git_repos=(
    git@github.com:paulschaaf/dotfiles.git
    git@github.com:paulschaaf/launch-url-from-string.git
)

## FUNCTIONS ===========================================
dash='----------------------------------------'

function printFgBg() {
    fg=$1; shift
    bg=$1; shift
    echo "[38;5;${fg};48;5;${bg}m${*}[0m"
}

function setScreenTitle() {
    echo -n "k$*\\"
}

function h1() {
    echo
    printFgBg 255 21 === $*
}

function h2() {
    setScreenTitle $*
    echo
    printFgBg 250 27 $*
}

#function error() {
#    printFgBg 255 196 $*
#}

function backup() {
    cp $* $backup
}

function install-all() {
    eval local packages=\${${*}[@]}
    h1 Install $* #: $packages
    skipped=''
#    set -x
    for pkg in ${packages[@]}; do
        package=${pkg//\~/ }
        # is it already installed?
        if yay --query $package 2> /dev/null; then
            skipped=${skipped}\'${package}\'' '
        else
            h2 Installing \'$package\'
            LESS+=-"P $package" yay --noconfirm $package
            [ "$?" -ne 0 ] && error Could not install \'$package\'
        fi
    done
    h2 Skipped already installed $*: $skipped
    set +x
}

function ln-all() {
    for src in $*; do
        target=.${src##*/}
        # echo src=$src
        # readlink $target
        if [ "$(readlink $target)" == "$src" ]; then
            echo The link $target is already correct
        else
            ln -s $src ${target}
        fi
    done
}

## ====================================================
mkdir $backup

h1 SYSTEM SETUP
h2 Remove ttys beyond CTRL-ALT-F1 and CTRL-ALT-F2
file=/etc/systemd/logind.conf
if grep -q '^#NAutoVTs=6$' $file; then
    backup $file
    sudo perl -pi -e "s/^#(NAutoVTs)=6$/\1=3/g" $file
else
    echo Already done!
fi

h1 PACKAGE INSTALLATION
install-all libraries
install-all ordered_packages
install-all unordered_packages
install-all applications

h1 USER SETUP
#for repo in ${git_repos[@]}; do
#    h1 Downloading Git Repository: $repo
#    git clone $repo
#done
#
#if grep --quiet $USER:$shell /etc/passwd; then
#    h2 Default shell already set to ${shell##*/}
#else
#    h1 Changing default shell to ${shell##*/}
#    sudo chsh --shell $shell $USER
#fi


h1 Setup Symlinks to RC files
ln-all ~/etc/home/*[^~]

#h1 Setup Symlinks to ssh files
#[ -d .ssh ] || mkdir .ssh
#chmod 700 .ssh
#cd .ssh
#ln-all ~/etc/ssh/*[^~]
#cd

# Clean Up
if rmdir $backup 2>/dev/null; then
    h1 No backups were necessary
else
    h1 Created backups in $backup
fi

if [ -n "$manually_install" ]; then
    h1 Manually install the following packages:
    echo "$manually_install"
    for pkg in ${manually_install[@]}; do
        echo google-chrome https://www.google.com/search?q="${pkg// //+}"+ubuntu+download
    done
fi

setScreenTitle