#!/usr/bin/bash

# Setup universal stuff
mkdir -p ~/bin
git clone https://github.com/olivierverdier/zsh-git-prompt.git ~/bin/zsh-git-prompt

pwd="$(dirname $0)"
# Install MacOS stuff
if [ "$(uname -s)" = "Darwin" ]; then
    ${pwd}/setup_env_macos.sh
fi



# Install Linux stuff
if [ "$(uname -s)" = "Linux" ]; then
    ${pwd}/setup_env_linux.sh
fi

# Link things into place
stow --target=${HOME} ${pwd}/emacs
stow --target=${HOME} ${pwd}/git
stow --target=${HOME} ${pwd}/images
stow --target=${HOME} ${pwd}/ispell
stow --target=${HOME} ${pwd}/macos
stow --target=${HOME} ${pwd}/pip
stow --target=${HOME} ${pwd}/zsh

# Special setups
stow --target=${HOME}/.ssh/ ${pwd}/ssh
