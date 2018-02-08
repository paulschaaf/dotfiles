#!/bin/bash
# -*- eval: (progn (highlight-regexp "\\bheader[1-9]+\\b" 'hi-green-b) (highlight-regexp "\\bh1\\b.*" 'header1) (highlight-regexp "\\bh2\\b.*" 'header2) ) -*-
cd $HOME
backup=~/backup
shell=/usr/bin/zsh

libraries=(
    libimage-exiftool-perl # edit EXIF data in image
    libncurses
    libncurses-dev
    libevent
)

# these get installed first, in the order defined
ordered_packages=(
    git                  # version control
    zsh                  # the best shell
    tmux                 # terminal multiplexor
    byobu                # tmux/screen enhancements
    google-chrome-stable # the best browser
    openssh-client       # make secure remote connections
    openssh-server       # host secure remote connections
    wine                 # run windows programs
)

# These are installed after the ordered_packages. Keep them sorted for convenience
unordered_packages=(
    adb                # android connectivity
    amarok             # music
    ascii              # tree of ascii codes
    byobu              # tmux/screen enhancements
    cairo-dock         # Mac-like icon dock
    calibre            # e-book manager
    cmake              # multi-platform install system
    davfs2             # mount box.com into filesystem
    devede             # make video DVDs
    dolphin-plugins    # git integration into KDE file manager
    emacs              # the king of editors
    enscript           # convert txt to ps, html, rtf, etc.
    filezilla          # file transfers
    frostwire          # bittorrent client
    g++                # compiler
    gitk               # git tk browser
    k4dirstat          # disk usage report
    keychain           # ssh key organizer
#    latte-dock         # dock app
    lynx               # command line browser
    meld               # merge tool
    mp3fs              # MP3 virtualf ilesystem
    mtpfs              # android file transfer
    partitionmanager   # manage disk partitions
    playonlinux        # addons for wine
    ruby               # programming language
    silversearcher-ag  # quicker grep
    slack              # IM client
    sublime-text       # code editor
    sweethome3d        # architectural modelling
    units              # unit conversions
    virtualbox         # VM manager
    virtualbox-ext-pack
    virtualbox-guest-additions-iso
    virtualbox-qt
    vlc                # video player
    vpnc               # vpn to Guidewire
    winetricks         # wine extensions
    xterm              # the basics
)

unofficial_ppas=(
    freecad-maintainers/freecad-stable
    webupd8team/java
)

unofficial_packages=(
    oracle-java6-installer
    oracle-java7-installer
    oracle-java8-installer
    # oracle-java9-installer
    freecad
)

# Use URL query-style syntax: e.g. double-quote any phrases and replace the spaces with plusses
manually_install=(
    Gitkraken
    Frostwire
    "Master+PDF+Editor"
    Solarized
)

git_repos=(
    git@github.com:paulschaaf/dotfiles.git
)

## FUNCTIONS ===========================================
pre=''
dash='----------------------------------------'

function header() {
    bg=21  # no color
    fg=248  # dim white
    leadingDashes=1
    trailingDashes=10

    case $1 in
        1)
            bg=18
            shift
            leadingDashes=3
            trailingDashes=3
            ;;
        2)
            bg=27
            shift
            ;;
        3)
            bg=24
            shift
            ;;
        4|5|6|7|8|9)
            bg=0 # no color
            shift
            ;;
        0) # 0 is the same as passing no number at all
            fg=255
            shift
            ;;
        *)
            fg=255
            ;;
    esac
    printf "\e[38;5;${fg};48;5;${bg}m\n%.${leadingDashes}s ${*}%.${trailingDashes}s\e[0m\n" $dash $dash
}

function printFgBg() {
    fg=$1; shift
    bg=$1; shift
    echo "[38;5;${fg};48;5;${bg}m${*}[0m"
}

function h1() {
    echo
    printFgBg 255 21 ========== $*
}

function h2() {
    printFgBg 255 27 $*
}

function backup() {
    cp $* $backup
}

function apt-get-all() {
    eval local packages=\${${*}[@]}
    h1 Install $* #: $packages
    skipped=''
    for package in ${packages[@]}; do
        # if it's not already installed
        if apt-cache policy $package | grep -q 'Installed: [^(]'; then
            skipped=${skipped}${package}' '
        else
            h2 Install $package
            sudo apt-get -y install $package
        fi
    done
    h2 Skipped already installed $*: $skipped
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

h1 PACKAGE INSTALLATION
apt autoremove
apt-get-all libraries
apt-get-all ordered_packages
apt-get-all unordered_packages

added=no
for ppa in ${unofficial_ppas[@]}; do
    if apt-cache policy | grep $ppa ``; then
        h2 PPA $ppa already present!
    else
        h1 Add PPA $ppa
        sudo apt-add-repository ppa:${ppa}
        added=yes
    fi
done

if [ "$added" == "yes" ]; then
    h1 Update Package Cache
    sudo apt-get update
else
    h2 No PPAs were added--no need to update the package cache
fi

apt-get-all unofficial_packages


h1 SYSTEM SETUP
if grep -q /home/pschaaf/box /etc/fstab; then
    h2 DavFS already listed in fstab
else
    h2 Adding DavFS to fstab
    backup /etc/fstab
    echo 'https://dav.box.com/dav/ /home/pschaaf/box  davfs  rw,user,noauto 0 0' | sudo tee --append /etc/fstab > /dev/null
fi

if grep '/dev/tty\[1-[^2]' /etc/default/console-setup; then
    h1 Remove ttys beyond CTRL-ALT-F1 and CTRL-ALT-F2
    backup /etc/default/console-setup
    sudo perl -pi -e "s/(ACTIVE_CONSOLES=\"\/dev\/tty\[1)-[^2][0-9]*/\1-2/g" /etc/default/console-setup
else
    h2 ttys beyond CTRL-ALT-F1 and CTRL-ALT-F2 already removed
fi


h1 USER SETUP
for repo in ${git_repos[@]}; do
    h1 Downloading Git Repository: $repo
    git clone $repo
done

if grep --quiet $USER:$shell /etc/passwd; then
    h2 Default shell already set to ${shell##*/}
else
    h1 Changing default shell to ${shell##*/}
    sudo chsh --shell $shell $USER
fi

if [ -d .davfs2 ]; then
    h2 DavFS2 already set up for Box.com access
else
    h1 Setup DavFS2 for Box.com access
    cp -r /etc/davfs2 .davfs2

    sudo adduser $USER davfs2
    ( cd .davfs2;
      echo -n 'Please type your box.com password:'
      read -s password; echo
      umask 077;
      echo "https://dav.box.com/dav paul666survey@gmail.com \"$password\"" >> secrets
    )
fi

h1 Setup Symlinks to RC files
ln-all ~/etc/home/*[^~]

h1 Setup Symlinks to ssh files
[ -d .ssh ] || mkdir .ssh
chmod 700 .ssh
cd .ssh
ln-all ~/etc/ssh/*[^~]
cd

# Clean Up
if rmdir $backup 2>/dev/null; then
    h1 No backups were necessary
else
    h1 Created backups in $backup
fi

[ -d box ] || mkdir box
if mount box 2> /dev/null; then
    h2 Mounted box.com
else
    h2 Box.com already mounted!
fi

h1 Manually install the following packages:
echo "$manually_install"
for pkg in ${manually_install[@]}; do
    echo google-chrome https://www.google.com/search?q="${pkg// //+}"+ubuntu+download
done