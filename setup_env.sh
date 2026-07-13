#!/usr/bin/env bash
# set -o xtrace  # Turn on echo variable
# set -o verbose # Turn on echo debugging

pwd="$(dirname $0)"
# Install stuff before depending on it with later commands
# OS Specific setups
case "$(uname -s)" in
    Darwin) # Install macOS stuff
	echo "Installing macOS specific things"
	${pwd}/setup_macos_env.sh
	echo "Profiles for programs."
	ls -l "${pwd}/macos/macos_app_settings"
	;;
    Linux) # Install Linux stuff
	echo "Installing Linux specific things"
	${pwd}/setup_linux_env.sh
	;;
esac

find ${pwd} -name ".DS_Store" -delete
source ${pwd}/utils.sh

# Setup universal stuff
mkdir -p ${HOME}/bin
# Requires git to be setup
! [ -e ${HOME}/bin/zsh-git-prompt ] && git clone https://github.com/olivierverdier/zsh-git-prompt.git ${HOME}/bin/zsh-git-prompt

# Link all the packages
excluded=(vim examples)
link_packages `find_stow_packages ${excluded[@]}`

cat README
