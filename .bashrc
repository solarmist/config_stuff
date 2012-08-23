# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

export PATH="${HOME}/bin/scripts:${HOME}/bin:/opt/local/bin:/opt/local/sbin:${PATH}"
export PERL5LIB="$PERL5LIB:/site/lib"

alias fact="elinks -dump randomfunfacts.com | sed -n '/^| /p' | tr -d \|"
alias ga='git add '
alias gb='git branch '
alias gba="git branch -a "
alias gc="git commit "
alias gd='git diff'
alias get='git '
alias gg="git grep -n "
alias gk='gitk --all&'
alias gl="fact; git pull --rebase "
alias glg="git log --graph "
alias go='git checkout '
alias got='git '
alias gp="git push "
alias gst="git status "
alias gs='git status '
alias gx='gitx --all'
alias cd..="cd .."
alias cd...="cd ../.."
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
alias rm="rm -r"
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


PS1="\[\e]0;\u@\$(simplehost \h) working in \w\$(parse_git_branch)\007\][\[\e[32m\]\u@\$(simplehost \h) \[\e[1;34m\](\t)\[\e[1;33m\]\$(parse_git_branch) \[\e[1;31m\]\w]\[\e[0m\] "


function parse_git_branch {
  # We just want the success status of the command
  git rev-parse 2> /dev/null
  if [[ $? = 0 ]]; then
      ref=$(git symbolic-ref HEAD|awk -F/ '{print $3}') || return
      echo " ("${ref#refs/heads/}")"
  fi
}


function simplehost(){
    name=$1
    case $name in
	ii52-25) echo -n "DEV7:$name";;
	ii52-26) echo -n "DEV8:$name";;
	ii52-27) echo -n "DEV9:$name";;
	ii50-9) echo -n "DEV12:$name";;
	ii50-10) echo -n "DEV13:$name";;
	ii50-11) echo -n "DEV14:$name";;
	ii50-15) echo -n "DEV15:$name";;
	bi*) echo -n "DEVLAB=DEVLAB=DEVLAB:$name";;
	*) echo -n "$name";;
    esac
}
