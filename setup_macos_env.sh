#!/usr/bin/env bash

link_emacs='tell application "Finder" to make alias file to POSIX file "/usr/local/opt/emacs-mac/Emacs.app" at POSIX file "/Applications"'

# Install homebrew if it isn't installed
if ! which brew >/dev/null; then
    # Ensure that brew is installed
    echo "Installing homebrew"
    $(which ruby) -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

# Install iterm2 integrations
echo "Setting up iterm2 integrations"
curl -L https://iterm2.com/shell_integration/install_shell_integration_and_utilities.sh | zsh

# brew options {package_name}
# https://github.com/mrowa44/emojify
brew install exiftool stow aspell bash direnv pipenv emojify httpie imagemagick git wget zsh
# brew install --with-debug --with-gpgme --with-libmetalink --with-pcre wget
brew tap homebrew/cask-fonts
brew cask install font-fira-code font-hasklig
# TODO: Auto detect version of bash this is being run with
echo "Please re-run setup_env.sh because the bash version has been upgraded"
# brew install zsh --with-unicode9
# https://www.reddit.com/r/emacs/comments/6ig02i/osx_if_youre_not_already_using_the_railwaycat/
# https://github.com/railwaycat/homebrew-emacsmacport
brew tap railwaycat/emacsmacport
brew install emacs-mac --with-imagemagick --with-emacs-sexy-icon --with-natural-title-bar --with-mac-metal --with-rsvg

# azuredevspaces.azds
# bajdzis.vscode-database
# DotJoshJohnson.xml
# gerane.Theme-Zenburn
# lfs.vscode-emacs-friendly
# ms-azuretools.vscode-azurefunctions
# ms-azuretools.vscode-docker
# ms-kubernetes-tools.vscode-aks-tools
# ms-kubernetes-tools.vscode-kubernetes-tools
# ms-python.python
# ms-vscode.azure-account
# ms-vscode.azurecli
# octref.vetur
# publicus.org-checkbox
# redhat.vscode-yaml
# Rubymaniac.vscode-direnv
# tootone.org-mode

# Link Emacs in place so that finder can index it
! [[ -d "/Application/Emacs.app" ]] && osascript -e "$link_emacs"
