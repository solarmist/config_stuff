#!/usr/bin/bash

# Setup universal stuff
mkdir -p ~/bin
git clone https://github.com/olivierverdier/zsh-git-prompt.git ~/bin/zsh-git-prompt

# Install MacOS stuff
if [ "$(uname -s)" = "Darwin" ]; then
    # Ensure that brew is installed
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    # Install homebrew if it isn't installed
    if ! which brew >/dev/null; then
	# https://github.com/mrowa44/emojify
	brew install direnv emojify imagemagick
	# brew options zsh
	brew install zsh --with-unicode9
	# https://www.reddit.com/r/emacs/comments/6ig02i/osx_if_youre_not_already_using_the_railwaycat/
	# https://github.com/railwaycat/homebrew-emacsmacport
	brew tap railwaycat/emacsmacport
	brew install emacs-mac --with-imagemagick
    fi
fi



# Install Linux stuff
if [ "$(uname -s)" = "Linux" ]; then
    # https://www.cyberciti.biz/faq/find-linux-distribution-name-version-number/
    "cat /etc/*-release"  # Linux Distro info
fi
