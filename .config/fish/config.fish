fish_vi_key_bindings

# set fish greeting to none
set fish_greeting

set PATH ~/Applications/flutter/bin $PATH
set PATH ~/.bin $PATH

# Add GOPATH/bin to path
set PATH ~/go/bin $PATH

# Add android/sdk/tools/bin to path
set PATH ~/Android/Sdk/tools/bin $PATH

# Add .local/bin and .bin to path
set PATH ~/.local/bin $PATH

# Add cabal and ghcup binaries to path
set PATH ~/.cabal/bin $PATH
set PATH ~/.ghcup/bin $PATH

set -x EDITOR "emacsclient -s term"
set -x VISUAL "emacsclient -s term"
alias ec="emacsclient -s term"

# nvm use v14.18.2

# if test -e ~/.config/shell/aliasrc
#     source ~/.config/shell/aliasrc
# end
