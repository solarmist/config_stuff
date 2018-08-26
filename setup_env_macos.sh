#!/usr/bin/bash

# Install homebrew if it isn't installed
if ! which brew >/dev/null; then
    # Ensure that brew is installed
    $(which ruby) -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    # https://github.com/mrowa44/emojify
    brew install direnv emojify imagemagick
    # brew options zsh
    brew install zsh --with-unicode9
    # https://www.reddit.com/r/emacs/comments/6ig02i/osx_if_youre_not_already_using_the_railwaycat/
    # https://github.com/railwaycat/homebrew-emacsmacport
    brew tap railwaycat/emacsmacport
    brew install emacs-mac --with-imagemagick
    if [[ -d "/Application/Emacs.app" ]]; then
	ln -s /usr/local/opt/emacs-mac/Emacs.app /Applications
    fi
fi
