#!/usr/bin/env bash

if [ "$1" = "http" ];
then
  git clone --bare https://github.com/lokesh1197/dotfiles.git "$HOME/.cfg"
else
  git clone --bare git@github.com:lokesh1197/dotfiles.git "$HOME/.cfg"
fi

function config {
   /usr/bin/git --git-dir="$HOME/.cfg/" --work-tree="$HOME" "$@"
}
mkdir -p .config-backup
config checkout
if [ "$?" = 0 ]; then
  echo "Checked out config.";
  else
    echo "Backing up pre-existing dot files.";
    config checkout 2>&1 | grep "\s+\." | awk '{print $1}' | xargs -I{} mv {} .config-backup/{}
fi;
config checkout
config config status.showUntrackedFiles no
