#!/usr/bin/env bash

function find_stow_packages {
    # Always exclude .git path
    local exclude_paths="-not -path *.git"
    for exclude in $@; do
	exclude_paths="$exclude_paths -not -path */${exclude}"
    done
    find_cmd="$(find ${pwd} -type d -depth 1 ${exclude_paths} | sort)"
    local dirs=""
    while read -r line; do
	dirs="$dirs $(basename $line)"
    done <<<"$find_cmd"
    echo $dirs
}

function link_packages {
    echo "Linking files in place"
    for package in $@; do
	case "$(uname -s) ${package}" in
	    "Darwin linux")
		;&  # Fall through (Bash 4+)
	    "Linux macos")
		 echo "On the $(uname -s) OS we don't link the package: ${package}"
		 continue;;
	     *)
		 echo "Linking package: ${package}"
		 stow --target=${HOME} ${package}
	esac
    done
}
