#!/usr/bin/env bash

brew_prefix=$(brew --prefix 2>/dev/null || echo "/opt/homebrew")
link_emacs="tell application \"Finder\" to make alias file to POSIX file \"${brew_prefix}/opt/emacs-mac/Emacs.app\" at (path to applications folder)"

# Install homebrew if it isn't installed
if ! which brew >/dev/null; then
    # Ensure that brew is installed
    echo "Installing homebrew"
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi

# Install iterm2 integrations
echo "Setting up iterm2 integrations"
curl -L https://iterm2.com/shell_integration/install_shell_integration_and_utilities.sh | zsh

# brew options {package_name}
# https://github.com/mrowa44/emojify
brew install exiftool stow aspell bash direnv pipenv emojify httpie imagemagick git wget zsh
brew install --cask font-fira-code font-hasklig
# https://www.reddit.com/r/emacs/comments/6ig02i/osx_if_youre_not_already_using_the_railwaycat/
# https://github.com/railwaycat/homebrew-emacsmacport
brew tap railwaycat/emacsmacport
brew install emacs-mac --with-imagemagick --with-emacs-sexy-icon --with-natural-title-bar --with-mac-metal

# Link Emacs in place so that finder can index it.
# The Finder alias is a *file* named "Emacs" (not a dir "Emacs.app"), so guard
# on -e /Applications/Emacs to stay idempotent and avoid duplicate aliases.
! [[ -e "/Applications/Emacs" ]] && osascript -e "$link_emacs"
