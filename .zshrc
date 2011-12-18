# -*- shell-script -*-
#
# anrxc's init file for Z-SHELL 4.3.10 on Arch GNU/Linux.
# http://sysphere.org/~anrxc/
# modified by Danny Navarro

# {{{ User settings

# {{{ Environment
export HISTFILE="${HOME}/.zsh_history"
export HISTSIZE=1000000
export SAVEHIST=1000000
export LESSHISTFILE="-"
export PAGER="${HOME}/bin/vimpager"
export READNULLCMD="${PAGER}"
export EDITOR="emacs"
export BROWSER="firefox"
export XTERM="urxvtc"
export RSYNC_PROXY="localhost:8118"
export CLASSPATH="${CLASSPATH}:/Applications/Development/weka-3-6-4/weka.jar"
export PATH="${HOME}/bin/scripts:${HOME}/bin:/opt/local/bin:/opt/local/sbin:${PATH}"
export MANPATH="/opt/local/share/man:${MANPATH}"
export CLICOLOR=1 # Colorize Mac OS
export GREP_OPTIONS="--color=always"
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
    c="--color=always"
fi
alias port="sudo /opt/local/bin/port"
alias yum="sudo yum"
alias easy_install="sudo easy_install"
alias pip="sudo pip"
alias ..="cd .."
alias ...="cd ../.."
alias ls="ls -F $c"
alias ll="ls -lF $c"
alias la="ls -AF $c"
alias lla="ls -AlF $c"
alias lfi="ls -lF $c| egrep -v '^d'"
alias ldi="ls -lF $c| egrep  '^d'"
alias lst="ls -htlF $c| grep `date +%Y-%m-%d`"
alias grep="grep"
alias cp="cp -a"
alias mv="mv"
alias rm="rm -rv"
alias cls="clear"
alias upmem="ps aux | sort -k 6"
alias g="gvim"
alias vi="vim"
alias top="htop"
alias psg="ps auxw | grep --color=always -i "
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

# {{{ Virtualenv wrapper
if $(which 'virtualenvwrapper.sh') ; then
    export WORKON_HOME=$HOME/sandbox/virtualenvs
    source $(which 'virtualenvwrapper.sh')
fi
# }}}
# }}}

# {{{ ZSH settings
setopt nohup
setopt autocd
setopt cdablevars
setopt nobgnice
setopt noclobber
setopt shwordsplit
setopt interactivecomments
setopt autopushd pushdminus pushdsilent pushdtohome
setopt histreduceblanks histignorespace inc_append_history
setopt nobeep

# keybindings
bindkey -e # emacs
bindkey "\e[A" up-line-or-search
bindkey "\e[B" down-line-or-search
bindkey '^xx' backward-kill-word
# Allow killing of part of a word/path
zle -N backward-kill-partial-word
bindkey '^x/' backward-kill-partial-word

# Prompt requirements
setopt extended_glob prompt_subst
autoload colors zsh/terminfo

# New style completion system
autoload -U compinit; compinit
#  * List of completers to use
zstyle ":completion:*" completer _complete _match _approximate
#  * Allow approximate
zstyle ":completion:*:match:*" original only
zstyle ":completion:*:approximate:*" max-errors 1 numeric
#  * Selection prompt as menu
zstyle ":completion:*" menu select=1
#  * Menu selection for PID completion
zstyle ":completion:*:*:kill:*" menu yes select
zstyle ":completion:*:kill:*" force-list always
zstyle ":completion:*:processes" command "ps -au$USER"
zstyle ":completion:*:*:kill:*:processes" list-colors "=(#b) #([0-9]#)*=0=01;32"
#  * Don't select parent dir on cd
zstyle ":completion:*:cd:*" ignore-parents parent pwd
#  * Complete with colors
zstyle ":completion:*" list-colors ""

# vcs_info
autoload -Uz vcs_info
zstyle ':vcs_info:*' stagedstr '%F{28}●'
zstyle ':vcs_info:*' unstagedstr '%F{11}●'
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:[svn]' formats '[%b%c%u]'
zstyle ':vcs_info:*' enable hg git

# }}}

# {{{ Functions

function chpwd() {
    print -Pn "\e]2;%n@%m: %c\a"
}

function cl () { cd $1 && ls }
function covtest () {
    nosetests --cover-package=$1 --cover-erase --with-coverage
}
function backward-kill-partial-word {
    local WORDCHARS="${WORDCHARS//[\/.]/}"
	zle backward-kill-word "$@"
}
function eave () { diff <(lsof -p $1) <(sleep 10; lsof -p $1) }

# {{{ Terminal and prompt


function precmd {
    # Terminal width = width - 1 (for lineup)
    local TERMWIDTH
    ((TERMWIDTH=${COLUMNS} - 1))

    # Truncate long paths
    PR_FILLBAR=""
    PR_PWDLEN=""
    local PROMPTSIZE="${#${(%):---(%n@%m:%l)---()--}}"
    local PWDSIZE="${#${(%):-%~}}"
    if [[ "${PROMPTSIZE} + ${PWDSIZE}" -gt ${TERMWIDTH} ]]; then
	((PR_PWDLEN=${TERMWIDTH} - ${PROMPTSIZE}))
    else
        PR_FILLBAR="\${(l.((${TERMWIDTH} - (${PROMPTSIZE} + ${PWDSIZE})))..${PR_HBAR}.)}"
    fi

    # VCS
    if [[ -z $(git ls-files --other --exclude-standard 2> /dev/null)
            && -z $(bzr ls -R --unknown 2> /dev/null) ]]; then
        if [[ -z $(bzr st -V 2> /dev/null) ]]; then
            zstyle ':vcs_info:*' formats ' %F{white}[%b%c%u%F{white}]'
        else
            zstyle ':vcs_info:*' formats ' %F{white}[%b%c%u%F{28}●%F{white}]'
        fi
    else
        if [[ -z $(bzr st -V 2> /dev/null) ]]; then
            zstyle ':vcs_info:*' formats ' %F{white}[%b%c%u%F{red}●%F{white}]'
        else
            zstyle ':vcs_info:*' formats ' %F{white}[%b%c%u%F{red}●%F{28}●%F{white}]'
        fi
    fi
    vcs_info
}

function preexec () {
    # Screen window titles as currently running programs
    if [[ "${TERM}" == "screen-256color" ]]; then
        local CMD="${1[(wr)^(*=*|sudo|-*)]}"
        echo -n "\ek$CMD\e\\"
    fi
}

function setprompt () {
    if [[ "${terminfo[colors]}" -ge 8 ]]; then
        colors
    fi
    for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
	eval PR_"${color}"="%{${terminfo[bold]}$fg[${(L)color}]%}"
	eval PR_LIGHT_"${color}"="%{$fg[${(L)color}]%}"
    done
    PR_NO_COLOUR="%{${terminfo[sgr0]}%}"

    # Try to use extended characters to look nicer
    typeset -A altchar
    set -A altchar ${(s..)terminfo[acsc]}
    PR_SET_CHARSET="%{${terminfo[enacs]}%}"
    PR_SHIFT_IN="%{${terminfo[smacs]}%}"
    PR_SHIFT_OUT="%{${terminfo[rmacs]}%}"
    PR_HBAR="${altchar[q]:--}"
    PR_ULCORNER="${altchar[l]:--}"
    PR_LLCORNER="${altchar[m]:--}"
    PR_LRCORNER="${altchar[j]:--}"
    PR_URCORNER="${altchar[k]:--}"

    # Terminal prompt settings
    case "${TERM}" in
        dumb) # Simple prompt for dumb terminals
            unsetopt zle
            PROMPT='%n@%m:%~%% '
            ;;
        linux) # Simple prompt with Zenburn colors for the console
            echo -en "\e]P01e2320" # zenburn black (normal black)
            echo -en "\e]P8709080" # bright-black  (darkgrey)
            echo -en "\e]P1705050" # red           (darkred)
            echo -en "\e]P9dca3a3" # bright-red    (red)
            echo -en "\e]P260b48a" # green         (darkgreen)
            echo -en "\e]PAc3bf9f" # bright-green  (green)
            echo -en "\e]P3dfaf8f" # yellow        (brown)
            echo -en "\e]PBf0dfaf" # bright-yellow (yellow)
            echo -en "\e]P4506070" # blue          (darkblue)
            echo -en "\e]PC94bff3" # bright-blue   (blue)
            echo -en "\e]P5dc8cc3" # purple        (darkmagenta)
            echo -en "\e]PDec93d3" # bright-purple (magenta)
            echo -en "\e]P68cd0d3" # cyan          (darkcyan)
            echo -en "\e]PE93e0e3" # bright-cyan   (cyan)
            echo -en "\e]P7dcdccc" # white         (lightgrey)
            echo -en "\e]PFffffff" # bright-white  (white)
            PROMPT='$PR_GREEN%n@%m$PR_WHITE:$PR_YELLOW%l$PR_WHITE:$PR_RED%~$PR_YELLOW%%$PR_NO_COLOUR '
            ;;
        *)  # Main prompt
            PROMPT='$PR_SET_CHARSET$PR_GREEN$PR_SHIFT_IN$PR_ULCORNER$PR_GREEN$PR_HBAR\
$PR_SHIFT_OUT($PR_GREEN%(!.%SROOT%s.%n)$PR_GREEN@%m$PR_WHITE:$PR_YELLOW%l$PR_GREEN)\
$PR_SHIFT_IN$PR_HBAR$PR_GREEN$PR_HBAR${(e)PR_FILLBAR}$PR_GREEN$PR_HBAR$PR_SHIFT_OUT(\
$PR_RED%$PR_PWDLEN<...<%~%<<$PR_GREEN)$PR_SHIFT_IN$PR_HBAR$PR_GREEN$PR_URCORNER$PR_SHIFT_OUT\

$PR_GREEN$PR_SHIFT_IN$PR_LLCORNER$PR_GREEN$PR_HBAR$PR_SHIFT_OUT(\
%(?..$PR_RED%?$PR_WHITE:)%(!.$PR_RED.$PR_YELLOW)%#$PR_GREEN)$PR_NO_COLOUR '

            RPROMPT='${vcs_info_msg_0_}$PR_GREEN$PR_SHIFT_IN$PR_HBAR$PR_GREEN$PR_LRCORNER$PR_SHIFT_OUT$PR_NO_COLOUR'
            ;;
    esac
}

# Prompt init
chpwd
setprompt

#Setup ssh agent environment
SSH_ENV="$HOME/.ssh/environment"

# start the ssh-agent
function start_agent {
    echo "Initializing new SSH agent..."
    # spawn ssh-agent
    ssh-agent | sed 's/^echo/#echo/' > "$SSH_ENV"
    echo succeeded
    chmod 600 "$SSH_ENV"
    . "$SSH_ENV" > /dev/null
    ssh-add
}

# test for identities
function test_identities {
    # test whether standard identities have been added to the agent already
    ssh-add -l | grep "The agent has no identities" > /dev/null
    if [ $? -eq 0 ]; then
        ssh-add
        # $SSH_AUTH_SOCK broken so we start a new proper agent
        if [ $? -eq 2 ];then
            start_agent
        fi
    fi
}

# check for running ssh-agent with proper $SSH_AGENT_PID
if [ -n "$SSH_AGENT_PID" ]; then
    ps -ef | grep "$SSH_AGENT_PID" | grep ssh-agent > /dev/null
    if [ $? -eq 0 ]; then
	test_identities
    fi
# if $SSH_AGENT_PID is not properly set, we might be able to load one from
# $SSH_ENV
else
    if [ -f "$SSH_ENV" ]; then
	. "$SSH_ENV" > /dev/null
    fi
    ps -ef | grep "$SSH_AGENT_PID" | grep -v grep | grep ssh-agent > /dev/null
    if [ $? -eq 0 ]; then
        test_identities
    else
        start_agent
    fi
fi

# }}}
# }}}
