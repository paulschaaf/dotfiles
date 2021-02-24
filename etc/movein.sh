#!/usr/bin/zsh
# -*- eval: (progn (highlight-regexp "\\bheader[1-9]+\\b" 'hi-green-b) (highlight-regexp "\\bh1\\b.*" 'header1) (highlight-regexp "\\bh2\\b.*" 'header2) ) -*-
# capture packages:
# pacman -Qqen >| pkglist.txt && pacman -Qqem >| pkglist_aur.txt
cd $HOME
backup=~/backup
shell=/bin/zsh
if [ "$host" = "copernicus" ]; then
   yaourt  -S broadcom-wl
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
   meta-group-base-devel   # all base devel tools
   ripgrep                 # quicker grep
   google-chrome           # the best browser
   git                     # version control
   intellij-idea-ultimate  # ide
   tmux                    # terminal multiplexer
   byobu                   # tmux/screen enhancements
   #    cairo-dock             # Mac-like icon dock
   #    cairo-dock-plug-ins    # Mac-like icon dock
   #    nvm                    # nodejs version manager
)

# These are installed after the ordered_packages. Keep them sorted for convenience
unordered_packages=(
   ascii                  # tree of ascii codes
   #    atom
   bcompare
   bcompare-kde5
   #    davfs2                # mount box.com into filesystem
   #    devede                # make video DVDs
   dolphin-plugins        # git integration into KDE file manager
   emacs                  # the king of editors
   enscript               # convert txt to ps, html, rtf, etc.
   inxi                   # system info tool
   jdk-dcevm              # Java Dynamic Code Evolution VM
   keychain               # ssh key organizer
   lsof
   #    lynx                  # command line browser
   #    manjaro-i3-solarized-settings
   #    mp3fs                 # MP3 virtualf ilesystem
   nerd-fonts-fira-code   # programming font with ligatures
   nmap
   #    nodejs
   #    npm
   #    oracle~jdk7            # Java 1.7
   #    oracle~jdk8            # Java 1.8
   ruby-irb               # Ruby REPL
   #    sqlops                # MS SQL Server admin tool
   tree
   units                  # unit conversions
   vlc                    # video player
   xorg-xkill
)

applications=(
   #    amarok                # music
   calibre                # e-book manager
   #    filezilla              # file transfers
   #    freecad
   frostwire              # bittorrent client
   #    gitkraken
   masterpdfeditor4
   #    slack-desktop          # IM client
   #    sweethome3d            # architectural modelling
   #    virtualbox            # VM manager
   #    virtualbox-ext-oracle
   #    virtualbox-guest-iso
   #    visual-studio-code
   xeyes                  # simple way to test X display
)

# Use URL query-style syntax: e.g. double-quote any phrases and replace the spaces with plusses
manually_install=(
)

## FUNCTIONS ===========================================
dash='----------------------------------------'

function printFgBg() {
   fg=$1
   shift
   bg=$1
   shift
   echo  "[38;5;${fg};48;5;${bg}m${*}[0m"
}

function setScreenTitle() {
   echo  -n "k$*\\"
}

function h1() {
   echo
   printFgBg  255 21 "=== $*"
}

function h2() {
   setScreenTitle  $*
   echo
   echo  -n '   '
   printFgBg  250 27 $*
}

function error() {
   printFgBg  255 196 $*
}

function backup() {
   cp  $* $backup
}

function install-all() {
   set -x
   if  [ $(uname) = "Linux" ]; then
#      eval   local packages=\${${*}[@]}
      h1   Install $* #: $packages
      skipped=''
      #    set -x
      for pkg in   "${@}"; do
         package=${pkg//\~/ }
         # is it already installed?
         if    pacman -Q $package 2> /dev/null; then
            skipped=${skipped}\'${package}\'' '
         else
            h2     Installing \'$package\'
            LESS+=-"P $package"     yay --useask $package
            [     "$?" -ne 0 ] && error Could not install \'$package\'
         fi
      done
      [ -n "$skipped" ] && h2   Skipped already installed: $skipped
      set   +x
   fi
   set  +x
}

function ln-all() {
   cd
   pwd

   for src in "$@"; do
      target=$(readlink ${src##*/})
      # echo src=$src
      # readlink $target
      if [ "$target" = "$src" ]; then
         echo '   '$target is already correct
      else
         ln -s $src ${target}
      fi
   done
}

## ====================================================
[[ -d $backup ]] || mkdir $backup

#h1 SYSTEM SETUP
#h2 Remove ttys beyond CTRL-ALT-F1 and CTRL-ALT-F2
#file=/etc/systemd/logind.conf
#if grep -q '^#NAutoVTs=6$' $file; then
#    backup $file
#    sudo perl -pi -e "s/^#(NAutoVTs)=6$/\1=3/g" $file
#else
#    echo Already done!
#fi

h1 PACKAGE INSTALLATION
#install-all libraries
install-all $ordered_packages
install-all $unordered_packages
install-all $applications

h1 USER SETUP

git_repos=(
   git@github.com:paulschaaf/dotfiles.git
   git@github.com:paulschaaf/launch-url-from-string.git
)
for repo in ${git_repos[@]}; do
    h1 Downloading Git Repository: $repo
    git clone $repo
done
PATH=~/bin;${PATH}
rehash
mkdir ~/bin/macos ~/bin/linux ~/bin/win 2>/dev/null

ln -fs /usr/bin/false ~/bin/inCygwin
ln -fs /usr/bin/true  ~/bin/win/inCygwin

ln -fs /usr/bin/false ~/bin/inMacOS
ln -fs /usr/bin/true  ~/bin/macos/inMacOS

ln -fs /usr/bin/false ~/bin/inLinux
ln -fs /usr/bin/true  ~/bin/linux/inLinux

#if grep --quiet $USER:$shell /etc/passwd; then
#    h2 Default shell already set to ${shell##*/}
#else
#    h1 Changing default shell to ${shell##*/}
#    sudo chsh --shell $shell $USER
#fi

#h1 Setup Symlinks to RC files
ln-all ~/etc/home/*[^~]

#h1 Setup Symlinks to ssh files
#[ -d .ssh ] || mkdir .ssh
#chmod 700 .ssh
#cd .ssh
#ln-all ~/etc/ssh/*[^~]
#cd

# Clean Up
if rmdir $backup 2> /dev/null; then
   h1  No backups were necessary
else
   h1  Created backups in $backup
fi

if [ -n "$manually_install" ]; then
   h1  Manually install the following packages:
   echo  "$manually_install"
   for pkg in  ${manually_install[@]}; do
      echo   google-chrome https://www.google.com/search?q="${pkg// //+}"+ubuntu+download
   done
fi

h1 Other packages
rehash
#h2 npm yarn
# sudo npm install -g yarn

setScreenTitle
