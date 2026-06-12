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

function link_packages {
    echo "Linking files in place"
    for package in $@; do
	case "$(uname -s) ${package}" in
	    "Darwin linux" | "Linux macos")
		 echo "On the $(uname -s) OS we don't link the package: ${package}"
		 continue;;
	     *)
		 echo "Linking package: ${package}"
		 stow --target=${HOME} ${package}
	esac
    done
}
