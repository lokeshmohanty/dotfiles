#!/bin/bash

# Enable bash programmable completion features in interactive shells
 [[ $PS1 && -f /usr/share/bash-completion/bash_completion ]] && \
    . /usr/share/bash-completion/bash_completion

#######################################################
# EXPORTS
#######################################################

# Expand the history size
export HISTFILESIZE=10000
export HISTSIZE=500

# Don't put duplicate lines in the history and do not add lines that start with a space
export HISTCONTROL=erasedups:ignoredups:ignorespace

# Check the window size after each command and, if necessary, update the values of LINES and COLUMNS
shopt -s checkwinsize

# Causes bash to append to history instead of overwriting it so if you start a new terminal, you have old session history
shopt -s histappend
# PROMPT_COMMAND='history -a'

# Allow ctrl-S for history navigation (with ctrl-R)
# stty -ixon

# Ignore case on auto-completion
# Note: bind used instead of sticking these in .inputrc
 bind "set completion-ignore-case on"

# Show auto-completion list automatically, without double tab
 bind "set show-all-if-ambiguous On"

# Set the default editor
export EDITOR=emacsclient
export VISUAL=emacsclient

# # To have colors for ls and all grep commands such as grep, egrep and zgrep
# export CLICOLOR=1
# export LS_COLORS='no=00:fi=00:di=00;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.avi=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.ogg=01;35:*.mp3=01;35:*.wav=01;35:*.xml=00;31:'
# #export GREP_OPTIONS='--color=auto' #deprecated
# alias grep="/bin/grep $GREP_OPTIONS"
# unset GREP_OPTIONS

# # Color for manpages in less makes manpages a little easier to read
# export LESS_TERMCAP_mb=$'\E[01;31m'
# export LESS_TERMCAP_md=$'\E[01;31m'
# export LESS_TERMCAP_me=$'\E[0m'
# export LESS_TERMCAP_se=$'\E[0m'
# export LESS_TERMCAP_so=$'\E[01;44;33m'
# export LESS_TERMCAP_ue=$'\E[0m'
# export LESS_TERMCAP_us=$'\E[01;32m'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# cd into the old directory
# alias bd='cd "$OLDPWD"'

# Remove a directory and all files
# alias rmd='/bin/rm  --recursive --force --verbose '

# Count all files (recursively) in the current folder
alias countfiles="for t in files links directories; do echo \`find . -type \${t:0:1} | wc -l\` \$t; done 2> /dev/null"

# Show all logs in /var/log
alias logs="sudo find /var/log -type f -exec file {} \; | grep 'text' | cut -d' ' -f1 | sed -e's/:$//g' | grep -v '[0-9]$' | xargs tail -f"

# Add all shell aliases
[[ -f ~/.shell_aliases ]] && . ~/.shell_aliases

# Add flutter/bin to path
export PATH="$PATH:$HOME/Applications/flutter/bin"

# Add .local/bin and .bin to path
export PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:$HOME/.bin"

# Add GOPATH/bin to path
export PATH="$PATH:$HOME/go/bin"

# Add android/sdk/tools/bin to path
export PATH="$PATH:$HOME/Android/Sdk/tools/bin"

# Add dynamic library path (added due to libvterm)
# export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/usr/lib/x86_64-linux-gnu"

# Load guix paths
export GUIX_PROFILE="/home/lokesh/.guix-profile"
# . "$GUIX_PROFILE/etc/profile"
source "$HOME/.guix-profile/etc/profile"
source "$HOME/.config/guix/current/etc/profile"


# Load nvm paths
export NVM_DIR="/home/lokesh/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion


# Start commands
## Unset previous key options
# setxkbmap -option

## Swap ctrl and capslock, being set in ~/.xprofile
# setxkbmap -option ctrl:swapcaps

# Use starship to set prompt
eval "$(starship init bash)"

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
# export SDKMAN_DIR="$HOME/.sdkman"
# [[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

PATH="/home/lokesh/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/home/lokesh/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/home/lokesh/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/home/lokesh/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/home/lokesh/perl5"; export PERL_MM_OPT;

[ -f "/home/lokesh/.ghcup/env" ] && source "/home/lokesh/.ghcup/env" # ghcup-env