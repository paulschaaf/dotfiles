#!/bin/bash
cd $HOME
backup=~/backup
shell=/usr/bin/zsh

# these get installed first, in the order defined
ordered_packages=(
    git                  # version control
    zsh                  # the best shell
    google-chrome-stable # the best browser
    openssh-client       # make secure remote connections
    openssh-server       # host secure remote connections
    tmux                 # terminal multiplexor
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
    dolphin-plugins    # git integration into KDE file manager
    emacs              # the king of editors
    filezilla          # file transfers
    frostwire          # bittorrent client
    g++                # compiler
    gitk               # git tk browser
    k4dirstat          # disk usage report
    keychain           # ssh key organizer
    lynx               # command line browser
    meld               # merge tool
    mtpfs              # android file transfer
    partitionmanager   # manage disk partitions
    playonlinux        # addons for wine
    ruby               # programming language
    screen             # terminal multiplexer
    silversearcher-ag  # quicker grep
    slack              # IM client
    sublime-text       # code editor
    sweethome3d        # architectural modelling
    virtualbox         # VM manager
    virtualbox-ext-pack
    virtualbox-guest-additions-iso
    virtualbox-qt
    vlc                # video player
    vpnc               # vpn to Guidewire
    xterm              # the basics
)

unofficial_ppas=(
    webupd8team/java
)

unofficial_packages=(
    oracle-java6-installer
    oracle-java7-installer
    oracle-java8-installer
    oracle-java9-installer
)

git_repos=(
    git@github.com:paulschaaf/dotfiles.git
)

## FUNCTIONS ===========================================
pre=''
dash='----------------------------------------'

function @#() {
    bg=21  # no color
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
            shift
            ;;
        *)
            ;;
    esac

    printf "\e[48;5;${bg}m\n%.${leadingDashes}s ${*}%.${trailingDashes}s\e[0m\n" $dash $dash
    set +x
}

function backup() {
    cp $* $backup
}

function apt-get-all() {
    eval local packages=\${${*}[@]}
    @# Install $*: $packages
    for package in ${packages[@]}; do
        @# Install $package
        sudo apt-get -y install $package
    done
}

## ====================================================
mkdir $backup

@# PACKAGE INSTALLATION ================================
apt-get-all ordered_packages
apt-get-all unordered_packages

added=no
for ppa in ${unofficial_ppas[@]}; do
    @# Add PPA $ppa
    if apt-cache policy | grep $ppa ``; then
        echo Already present!
    else
        sudo apt-add-repository ppa:${ppa}
        added=yes
    fi
done

    @# Update Package Cache
    sudo apt-get update
    apt-get-all unofficial_packages


@# SYSTEM SETUP ========================================
@# 1 Add DavFS to fstab
if ! grep -q /home/pschaaf/box /etc/fstab; then
    backup /etc/fstab
    echo 'https://dav.box.com/dav/ /home/pschaaf/box  davfs  rw,user,noauto 0 0' | sudo tee --append /etc/fstab > /dev/null
else
    @# 3 Already done!
fi

@# Remove ttys beyond CTRL-ALT-F1 and CTRL-ALT-F2
if grep '/dev/tty\[1-[^2]' /etc/default/console-setup; then
    backup /etc/default/console-setup
    sudo perl -pi -e "s/(ACTIVE_CONSOLES=\"\/dev\/tty\[1)-[^2][0-9]*/\1-2/g" /etc/default/console-setup
else
    echo Already done!
fi


@# USER SETUP ==========================================
for repo in ${git_repos[@]}; do
    @# Downloading Git Repository: $repo
    git clone $repo
done

@# Changing default shell to ${shell##*/}
if grep --quiet $USER:$shell /etc/passwd; then
    echo Already set!
else
    sudo chsh --shell $shell $USER
fi

@# Setup DavFS2 for Box.com access
if [ ! -d .davfs2 ]; then
    cp -r /etc/davfs2 .davfs2

    sudo adduser $USER davfs2
    ( cd .davfs2;
      echo -n 'Please type your box.com password:'
      read -s password; echo
      umask 077;
      echo "https://dav.box.com/dav paul666survey@gmail.com \"$password\"" >> secrets
    )
else
    echo Already done!
fi

@# Setup Symlinks to RC files
for file in ~/etc/home/*[^~]; do  # ignore files ending in ~
    ln -s $file .${file##*/}
done

@# Setup Symlinks to ssh files
mkdir -m700 .ssh
ln -s ~/etc/ssh/*[^~] ~/.ssh

# Clean Up
if rmdir $backup 2>/dev/null; then
    @# No backups were necessary
else
    @# Created backups in $backup
fi

[ -d box ] || mkdir box
@# Mounting box
if mount box 2> /dev/null; then
    echo Done!
else
    echo Already mounted!
fi

@# Manually install the following packages: \
<<EOF
Master PDF Editor
Frostwire

EOF
