# -*- shell-script -*-
#
# anrxc's init file for Z-SHELL 4.3.10 on Arch GNU/Linux.
# http://sysphere.org/~anrxc/
# modified by Danny Navarro

#Add direnv zsh hook
eval "$(direnv hook zsh)"

# {{{ User settings
export PEP8_IGNORE="E501\|E111\|E114"
export PIP_DOWNLOAD_CACHE=${HOME}/.pip/cache

# {{{ Environment
export PATH="${PATH}:/usr/local/sbin:${HOME}/bin:/usr/local/opt/icu4c/bin:/usr/local/opt/icu4c/sbin:${HOME}/bin/scripts:${HOME}/bin:/opt/local/bin:/opt/local/sbin"
export TIME_STYLE=long-iso
export HISTFILE="${HOME}/.zsh_history"
export HISTSIZE=100000000
export SAVEHIST=100000000
export LESSHISTFILE="-"
export EDITOR="emacs"
export BROWSER="chrome"
export XTERM="urxvtc"
export RSYNC_PROXY="localhost:8118"

export MANPATH="/opt/local/share/man:${MANPATH}"
export CFLAGS="-I/usr/local/opt/openssl101/include"
export CLICOLOR=1 # Colorize Mac OS
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
# If this is linux then add --color=always to ls
if ! [ -f /usr/bin/sw_vers ]; then
    c="--color=auto"
    l="-l --time-style=long-iso"
else
    c=""
    l="-l -T"
fi

alias liversion="cat /export/content/linkedin/DISTRIBUTION | python -c \"import json, sys; obj=json.load(sys.stdin); print obj['name'], 'version:', obj['version']; print; print '\n'.join(sorted([('%21s' % unit['name']) + '\tversion: ' + unit['version'] for unit in obj['units']], cmp=lambda x,y: cmp(x.strip(), y.strip())))\""
alias fact="elinks -dump randomfunfacts.com | sed -n '/^| /p' | tr -d \|"
alias git_line='git log --format=format:%H $1 | xargs -L 1 git blame $1 -L $2,$2'
alias ga='git add '
alias gb='git branch '
alias gba="git branch -a "
alias gc="git commit "
alias gd='git diff'
alias get='git '
alias gg="git grep -n "
alias gk='gitk --all&'
alias gl="fact; stashed=false; if ! git diff --quiet; then git stash --include-untracked; stashed=true; echo 'Stashing'; fi; git pull --rebase; if $stashed; then git stash pop --index; echo 'Popping'; fi"
alias glg="git log --graph --decorate"
alias go='git checkout '
alias got='git '
alias gp="git push "
alias gst="git status "
alias gs='git status '
alias gx='gitx --all'

alias testify='testify -v'
alias html2ascii='lynx -force_html -stdin -dump -nolist'
alias cd..="cd .."
alias cd...="cd ../.."
alias ..="cd .."
alias ...="cd ../.."
alias ls="ls -Fh $c"
alias ll="ls -hF $c $l"
alias la="ls -AhF $c"
alias lla="ls -AhF $c $l"
alias lfi="ls -Fh $c $l| egrep -v '^d'"
alias ldi="ls -hF $c $l| egrep  '^d'"
alias lst="ls -hthF $c $l| grep `date +%Y-%m-%d`"
alias cp="cp -a"
alias rm="rm -rv"
alias cls="clear"
alias g="gvim"
alias vi="vim"
alias psg="ps aux | grep --color=always -i "
alias psptree="ps auxwww -f"
alias df="df -hT"
alias du="du -hc"
alias dus="du -S | sort -n"
alias free="free -m"
alias su="su - "
alias eject="eject -v "
alias retract="eject -t -v "
alias ping="ping -c 5"
alias sat="date +%R"
alias calc="bc -l <<<"
alias spell="ispell -a <<< "
alias passgen="< /dev/urandom tr -cd \[:graph:\] | fold -w 32 | head -n 5"
alias pjson='python -mjson.tool'
#alias less=$PAGER
#alias zless=$PAGER
# }}}

# {{{ Completion
compctl -k "(add delete draft edit list import preview publish update)" nb
# }}}



# {{{ Functions

function make_envrc {
    local preferred_app="${1}"

    local repo_root="$(git worktree list | grep master | cut -d' ' -f1)"
    local mp_name="$(get_product_spec_value name)"
    local apps="$(find ${repo_root} -type d -depth 1 -not -path ./.git -not -path ./acl -not -path ./build -not -path ./docs -not -path ./.gradle -not -path ./config -not -path ./bin -not -path ./ligradle -not -path ./test-site -not -path ./.svn  | sed 's|./||g')"
    local num_apps="$(echo $apps | wc -l)"

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

function vpn {
    case $1 in
	"disconnect")
	    /opt/cisco/anyconnect/bin/vpn disconnect
	    ;;
	*)
	    local yubikey=$1;
	    local password="$(security -q find-generic-password -l "Enterprise Connect" -w)"
	    local command="\n${password}${yubikey}\ny\n"
	    # Clear any dialogs from a previous disconnect
	    # osascript -e "tell application 'System Events'"
	    # osascript -e "tell process 'Finder'"
	    # osascript -e "tell its window"
	    # osascript -e "click button 'OK'"
	    # osascript -e "end tell"
	    # osascript -e "tell its window"
	    # osascript -e "click button 'OK'"
	    # osascript -e "end tell"
	    # osascript -e "end tell"
	    # osascript -e "end tell"
	    echo "${command}" | /opt/cisco/anyconnect/bin/vpn -s connect 1
	    ;;
    esac
}

function chpwd {
    # -P prompt expansion
    print -Pn "\e]2;%n@%m: %c\a"
}


source ~/.zshrc_macos
source ~/.zsh_settings

# Prompt init
source ~/.zsh_prompt
chpwd
setprompt
