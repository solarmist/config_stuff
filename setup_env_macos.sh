#!/usr/bin/env bash

link_emacs='tell application "Finder" to make alias file to POSIX file "/usr/local/opt/emacs-mac/Emacs.app" at POSIX file "/Applications"'

# Install homebrew if it isn't installed
if ! which brew >/dev/null; then
    # Ensure that brew is installed
    $(which ruby) -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    # brew options {package_name}
    # https://github.com/mrowa44/emojify
    brew install aspell bash direnv emojify imagemagick
    brew install git --with-pcre2 --with-persistent-https --with-gettext
    brew install zsh --with-unicode9
    # https://www.reddit.com/r/emacs/comments/6ig02i/osx_if_youre_not_already_using_the_railwaycat/
    # https://github.com/railwaycat/homebrew-emacsmacport
    brew tap railwaycat/emacsmacport
    brew install emacs-mac --with-imagemagick --with-modern-icon --with-natural-title-bar --with-xml2
    # Link Emacs in place so that finder can index it
    ! [[ -d "/Application/Emacs.app" ]]; && osascript -e "$link_emacs"

fi
