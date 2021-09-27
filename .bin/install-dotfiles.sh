#!/usr/bin/env bash

GIT_CLONE_URL="git@github.com:lokesh1197/dotfiles.git"
TARGET_DIR="$HOME/.cfg"

if [ "$1" = "http" ];
then
    GIT_CLONE_URL="https://github.com/lokesh1197/dotfiles.git"
fi

git clone --bare --recurse-submodules $GIT_CLONE_URL $TARGET_DIR

function config {
   /usr/bin/git --git-dir="$HOME/.cfg/" --work-tree="$HOME" "$@"
}
mkdir -p .config-backup
config checkout
if [ "$?" = 0 ]; then
  echo "Checked out config.";
  else
    echo "Backing up pre-existing dot files.";
    config checkout 2>&1 | egrep "\s+\." | awk '{print $1}' | xargs -I{} mv {} .config-backup/{}
fi;
config checkout
config config status.showUntrackedFiles no
