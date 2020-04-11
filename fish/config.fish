fish_vi_key_bindings

set PATH ~/.local/bin $PATH
set -x EDITOR nvim
set -x VISUAL nvim


if test -S ~/.ssh/ssh_auth_sock 
else
  eval (ssh-agent -c)
  ln -sf "$SSH_AUTH_SOCK" ~/.ssh/ssh_auth_sock
end
set -x SSH_AUTH_SOCK ~/.ssh/ssh_auth_sock
ssh-add -l > /dev/null || ssh-add
