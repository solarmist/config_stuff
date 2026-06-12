#!/usr/bin/env bash

# Install Linux stuff
# https://www.cyberciti.biz/faq/find-linux-distribution-name-version-number/
# "cat /etc/*-release"  # Linux Distro info
# hostnamectl | grep "Operating System"
sudo yum -y install stow emacs tmux
# Installs the latest direnv release for this architecture
curl -sfL https://direnv.net/install.sh | sudo bin_path=/usr/local/bin bash
