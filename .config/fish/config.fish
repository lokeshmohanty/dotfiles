fish_vi_key_bindings

set PATH ~/.local/bin $PATH
set -x EDITOR emacsclient
set -x VISUAL emacsclient

nvm use v14.16.0

source ~/.shell_aliases
# if test -e ~/.shell_aliases
#     source ~/.shell_aliases
# end
