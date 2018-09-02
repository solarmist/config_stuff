#!/usr/bin/env bash

pwd="$(dirname $0)"
source ${pwd}/utils.sh

# Setup universal stuff
mkdir -p ${HOME}/bin
# Requires git and ssh key to be setup
! [ -e ${HOME}/bin/zsh-git-prompt ] && git clone https://github.com/olivierverdier/zsh-git-prompt.git ${HOME}/bin/zsh-git-prompt

# Install stuff before depending on it with later commands
# OS Specific setups
case "$(uname -s)" in
    Darwin) # Install macOS stuff
	echo "Installing macOS specific things"
	${pwd}/setup_macos_env.sh
	;;
    Linux) # Install Linux stuff
	echo "Installing Linux specific things"
	${pwd}/setup_linux_env.sh
	;;
esac

find $pwd -name ".DS_Store" -delete

# Link all the packages
excluded="ssh vim"
link_packages `find_stow_packages $excluded`
# Special setups
echo "Linking: ssh"
stow --target=${HOME}/.ssh/ ssh
