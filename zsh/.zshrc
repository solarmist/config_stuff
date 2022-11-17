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

# {{{ Environment
export PATH="${PATH}:${HOME}/.poetry/bin:${HOME}/bin:/usr/local/sbin:/opt/local/bin:/opt/local/sbin"
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
alias rm="rm -r"
alias pjson='python -mjson.tool'

# Other Aliases
alias calc="bc -l <<<"
alias cls="clear"
alias emacs="emacs --no-window-system --no-desktop"
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
function make_sub_envrcs {
    local repo_root="$1"
    local mp_name="$2"
    local apps="$3"
    for app in $apps; do
        if [[ ! -d "${repo_root}/${app}" ]]; then
            local app_name="export APP_NAME='${app}'"
            local path_var="PATH_add 'build/${app}/venv/bin'"

            local envrc="${repo_root}/${app}/.envrc"
            rm -f ${envrc}
            echo "${mp_name}\n${app_name}\n${path_var}\n" > ${envrc}
        fi
    done
}

function make_envrc_mp {
    local repo_root="$1"

    local mp_name="$(get_product_spec_value name)"
    # Find directories containing venvs inside the build dir
    local app_dirs="$(find ${repo_root}/build -type d -depth 2 -name venv)"
    # clean them up to be only the directory name
    local apps="$(echo ${app_dirs} | sed "s|${repo_root}/build/||g" | sed "s|/venv||g")"
    local default_app="$(echo $apps| cut -d' ' -f1)"

    local mp_name="export MP_NAME='${mp_name}'"
    local path_var="PATH_add 'build/${mp_name}/venv/bin'"

    if [[ "$(echo $apps | wc -w)" -gt 1 ]]; then
        # If there are more than 1 app then make .envrcs for all of the applications
        make_sub_envrcs "$repo_root" "$mp_name" "$apps"
        preferred_app="${2:-$default_app}"
    fi

    if [[ -n "$preferred_app" ]]; then
        local app_name="export APP_NAME='$preferred_app'"
        echo "${mp_name}\n${app_name}\n${path_var}\n"
    else
        # If we only have one app then we don't need to include a preferred app
        echo "${mp_name}\n${path_var}\n"
    fi
}

function make_envrc_pipenv {
    local repo_root="$1"
    local app_name="$(basename $repo_root)"
    local venv="$(pipenv --venv)"

    # .envrc contents
    local venv_prompt="export VIRTUAL_ENV_DISABLE_PROMPT='1'"
    local venv_var="export VIRTUAL_ENV='$venv'"
    local app_name="export APP_NAME='$app_name'"
    local path_var="PATH_add '$venv/bin'"
    echo "${venv_prompt}\n${venv_var}\n${app_name}\n${path_var}\n"
}

function make_envrc {
    local preferred_app="$1"
    local repo_root="$(git worktree list | grep master | cut -d' ' -f1)"
    local envrc="$repo_root/.envrc"
    # If we have an existing env disable and remove it while we're making changes
    if [[ -e "$envrc" ]]; then
        rm -f ${envrc} 2>/dev/null # Remove existing .envrc file
        direnv reload 2>/dev/null
        unset VIRTUAL_ENV  # Clear any VIRTUAL_ENV settings that are loaded by direnv in the script env
    fi
    if [[ -e "$repo_root/product-spec.json}" ]]; then
        make_envrc_mp "$repo_root" "$preferred_app" > ${envrc}
    elif [[ -n "$(pipenv --venv)" ]]; then
        make_envrc_pipenv "$repo_root" > ${envrc}
    fi
    direnv allow
}

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"


autoload -Uz compinit
zstyle ':completion:*' menu select
fpath+=~/.zfunc
