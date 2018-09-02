#!/usr/bin/bash

# Install Linux stuff
# https://www.cyberciti.biz/faq/find-linux-distribution-name-version-number/
# "cat /etc/*-release"  # Linux Distro info
# hostnamectl | grep "Operating System"
sudo yum -y install stow emacs tmux
echo "Check that 2.17 is the version of direnv you want"
pushd $HOME
wget https://github.com/direnv/direnv/releases/download/v2.17.0/direnv.linux-amd64
chmod +x direnv.linux-amd64
sudo mv direnv.linux-amd64 /usr/local/bin/direnv
popd
