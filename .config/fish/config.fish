fish_vi_key_bindings

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

set -x EDITOR emacsclient
set -x VISUAL emacsclient

# nvm use v14.18.2

source ~/.shell_aliases
# if test -e ~/.shell_aliases
#     source ~/.shell_aliases
# end
