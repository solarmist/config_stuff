# -*- shell-script -*-
#
# solarmist's .zshrc based on
#     anrxc's init file for Z-SHELL 4.3.10 on Arch GNU/Linux.
#     http://sysphere.org/~anrxc/
#     modified by Danny Navarro

# Import OS specific setting
case "$(uname -s)" in
    Darwin)
	source ~/.zshrc_macos;;
    Linux)
	source ~/.zshrc_linux;;
esac

# zsh settings
source ~/.zsh_settings

# Prompt init
source ~/.zsh_prompt
chpwd
setprompt

#Add direnv zsh hook
eval "$(direnv hook zsh)"

# {{{ User settings
export PIP_DOWNLOAD_CACHE=${HOME}/.pip/cache

# LI Specific stuff
if [[ -e ${HOME}/.zshrc_li ]]; then
    source ${HOME}/.zshrc_li
fi

# {{{ Environment
export PATH="${PATH}:${HOME}/bin:/usr/local/sbin:/opt/local/bin:/opt/local/sbin"
export TIME_STYLE=long-iso
export LESSHISTFILE="-"
export EDITOR="emacs"
export BROWSER="chrome"

export MANPATH="/opt/local/share/man:${MANPATH}"
export GREP_OPTIONS="--color=auto"
# By default: export WORDCHARS='*?_-.[]~=/&;!#$%^(){}<>'
# We take out the slash, period, angle brackets, dash here.
# I like killing/moving whole paths, so comment this out
# export WORDCHARS='*?_[]~=&;!#$%^(){}'

# }}}

# {{{ Manual pages
#     - colorize, since man-db fails to do so
export LESS_TERMCAP_mb=$'\E[01;31m'   # begin blinking
export LESS_TERMCAP_md=$'\E[01;31m'   # begin bold
export LESS_TERMCAP_me=$'\E[0m'       # end mode
export LESS_TERMCAP_se=$'\E[0m'       # end standout-mode
export LESS_TERMCAP_so=$'\E[1;33;40m' # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'       # end underline
export LESS_TERMCAP_us=$'\E[1;32m'    # begin underline
# }}}

# {{{ Aliases

# Git Aliases
alias ga='git add '
alias gb='git branch '
alias gba="git branch -a "
alias gc="git commit "
alias gd='git diff'
alias get='git '
alias gg="git grep -n "
alias git_line='git log --format=format:%H $1 | xargs -L 1 git blame $1 -L $2,$2'
alias gk='gitk --all&'
alias gl="fact; stashed=false; if ! git diff --quiet; then git stash --include-untracked; stashed=true; echo 'Stashing'; fi; git pull --rebase; if $stashed; then git stash pop --index; echo 'Popping'; fi"
alias glg="git log --graph --decorate"
alias go='git checkout '
alias got='git '
alias gp="git push "
alias gs='git status '
alias gst="git status "
alias gx='gitx --all'

# Aliases I don't know what they do.
alias free="free -m"
alias psg="ps aux | grep --color=always -i "
alias psptree="ps auxwww -f"
alias du="du -hc"
alias dus="du -S | sort -n"

# CD & LS aliases & common options for frequently used commands
alias ...="cd ../.."
alias ..="cd .."
alias cd...="cd ../.."
alias cd..="cd .."

alias cp="cp -a"
alias su="su - "
alias vi="vim"
alias rm="rm -rv"
alias pjson='python -mjson.tool'

# Other Aliases
alias calc="bc -l <<<"
alias cls="clear"
alias emacs="emacs --no-desktop"
alias fact="elinks -dump randomfunfacts.com | sed -n '/^| /p' | tr -d \|"
alias g="gvim"
alias html2ascii='lynx -force_html -stdin -dump -nolist'
# alias less=$PAGER
alias passgen="< /dev/urandom tr -cd \[:graph:\] | fold -w 32 | head -n 5"
alias ping="ping -c 5"
alias spell="aspell -a <<< "
# alias zless=$PAGER
# }}}


# {{{ Functions

function make_envrc {
    local preferred_app="${1}"

    local repo_root="$(git worktree list | grep master | cut -d' ' -f1)"
    local mp_name="$(get_product_spec_value name)"
    # Find venv within the build dir
    local apps="$(find ${repo_root}/build -type d -depth 2 -name venv | sed "s|${repo_root}/build/||g" | sed "s|/venv||g")"
    local num_apps="$(echo $apps | wc -w)"

    local mp_var="export MP_NAME='${mp_name}'\n"
    local preferred_app=""
    if [ -n "${preferred_app}" ]; then
	local preferred_app="export APP_NAME='${app}'\n"
    fi

    # If there are more than 1 app then make envrcs for all of the applications
    if [ "${num_apps}" -gt 1 ]; then
	for app in $apps; do
	    if [ -d "${repo_root}/${app}" ]; then
		local app_var="export APP_NAME='${app}'\n"
		local path_var="PATH_add 'build/${app}/venv/bin'\n"
		local envrc="${repo_root}/${app}/.envrc"

		rm -f ${repo_root}/${app}/.envrc
		echo "${mp_var}${app_var}${path_var}" > ${envrc}
	    fi
	done
    fi
    # Create an MP level .envrc
    local envrc="${repo_root}/.envrc"
    local path_var="PATH_add 'build/${mp_name}/venv/bin'\n"

    rm -f ${envrc}
    # If we only have one app then we don't need to include a preferred app
    if [ "${num_apps}" -ne 1 ]; then
	echo "${mp_var}${path_var}" > ${envrc}
    else
	echo "${mp_var}${preferred_app}${path_var}" > ${envrc}
    fi
    direnv allow
}
