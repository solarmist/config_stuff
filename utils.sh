#!/usr/bin/env bash

function find_stow_packages {
    # Always exclude .git path
    local exclude_paths=(-not -name ".git")
    for exclude in "$@"; do
	exclude_paths+=(-not -name "${exclude}")
    done
    find_cmd="$(find "${pwd}" -mindepth 1 -maxdepth 1 -type d "${exclude_paths[@]}" | sort)"
    local dirs=""
    while read -r line; do
	dirs="$dirs $(basename "$line")"
    done <<<"$find_cmd"
    echo $dirs
}

function backup_conflicts {
    # Apps sometimes drop plain files where stow wants a link (e.g. iTerm2
    # creating ~/.zshrc). Move those aside so stow can proceed: delete them
    # if identical to the package copy, otherwise keep a .bak of them.
    local package="$1"
    local conflicts
    conflicts="$(stow -n --target=${HOME} ${package} 2>&1 |
	sed -n 's/.*existing target \(.*\) since neither a link nor a directory.*/\1/p')"
    [[ -z "${conflicts}" ]] && return
    while read -r target; do
	if cmp -s "${package}/${target}" "${HOME}/${target}"; then
	    echo "Removing ${HOME}/${target}: identical to ${package}/${target}"
	    rm "${HOME}/${target}"
	else
	    echo "Backing up ${HOME}/${target} to ${HOME}/${target}.bak"
	    mv "${HOME}/${target}" "${HOME}/${target}.bak"
	fi
    done <<<"${conflicts}"
}

function link_packages {
    echo "Linking files in place"
    for package in $@; do
	case "$(uname -s) ${package}" in
	    "Darwin linux" | "Linux macos")
		 echo "On the $(uname -s) OS we don't link the package: ${package}"
		 continue;;
	     *)
		 echo "Linking package: ${package}"
		 backup_conflicts ${package}
		 stow --target=${HOME} ${package}
	esac
    done
}
