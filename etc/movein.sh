#!/bin/bash
cd ~pschaaf
backup=~/backup

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
    amarok             # music
    ascii              # tree of ascii codes
    cairo-dock         # Mac-like icon dock
    calibre            # e-book manager
    davfs2             # mount box.com into filesystem
    emacs              # the king of editors
    frostwire          # bittorrent client
    k4dirstat          # disk usage report
    keychain           # ssh key organizer
    lynx               # command line browser
    meld               # merge tool
    partitionmanager   # manage disk partitions
    playonlinux        # addons for wine
    ruby               # programming language
    screen             # terminal multiplexer
    silversearcher-ag  # quicker grep
    slack              # IM client
    sweethome3d        # architectural modelling
    virtualbox         # VM manager
    virtualbox-qt      # VM manager GUI
    vlc                # video player
    vpnc               # vpn to Guidewire
    xterm              # the basics
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

if [ $added = 'yes' ]; then
    @# Update Package Cache
    sudo apt-get update
    apt-get-all unofficial_packages
fi


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

chsh --shell /usr/bin/zsh

if [ ! -d .davfs2 ]; then
    @# Setup DavFS2 for Box.com access
    cp -r /etc/davfs2 .davfs2

    sudo adduser pschaaf davfs2
    ( cd .davfs2;
      echo -n 'Please type your box.com password:'
      read -s password; echo
      umask 077;
      echo "https://dav.box.com/dav paul666survey@gmail.com \"$password\"" >> secrets
    )
else
    echo Already done!
fi

# Clean Up
if rmdir $backup 2>/dev/null; then
    @# No backups were necessary
else
    @# Created backups in $backup
fi

[ -d box ] || mkdir box
mount box
