#!/usr/bin/env bash

# Update this list when adding new config files
# Currently not using zsh
dotfiles=("bashrc" "shrc" "dir_colors" "bash_profile" "gitconfig")
# dotfiles=("zshenv" "zshrc" "zprezto" "bashrc" "shrc" "dir_colors" "bash_profile" "tmux.conf" "gitconfig")
DOTFILES_DIR="${HOME}/.dotfiles"
BACKUP_DIR="${HOME}/.dotfiles_backup"

echo "OS_NAME = $(uname)"

echo "This will create symbolic links of this directory's files in the home directory"

echo "The conflicting dotfiles of home directory will be backed up to ~/.dotfiles_backup"

if [ ! -d "${BACKUP_DIR}" ] ; then
    echo "Creating ~/.dotfiles_backup to backup existing dotfiles"
    mkdir -p ${HOME}/.dotfiles_backup
fi

echo "Installing dotfiles..."
# Add functionality later
# for file in $( ls -1A | grep -vE '\.git|\.gitignore|.*.md|*/|*.sh' ) ; do
#     if [ -f "${HOME}/${file}" ] ; then
#         mv -f ${HOME}/${file} ${BACKUP_DIR}
#     fi
#     ln -sv "${DOTFILES_DIR}/${file}" "${HOME}"
# done

for file in ${dotfiles[@]} ; do
    if [ -f ${HOME}/.${file} ] ; then
        mv -f ${HOME}/.${file} ${BACKUP_DIR}
    fi
    ln -sv ${DOTFILES_DIR}/*/${file} ${HOME}/.${file}
done

# Neovim config files
if [ -d ${HOME}/.config/nvim ] ; then
    mv -f ${HOME}/.config/nvim ${BACKUP_DIR}
fi
mkdir -p ${HOME}/.config/nvim
ln -sv ${DOTFILES_DIR}/nvim/init.vim ${HOME}/.config/nvim/init.vim
ln -sv ${DOTFILES_DIR}/nvim/plugin ${HOME}/.config/nvim/

# Install neovim
echo "Installing neovim..."
curl https://raw.githubusercontent.com/Shougo/dein.vim/master/bin/installer.sh > installer.sh
sh ./installer.sh ~/.local/share/dein > /dev/null 2>&1
rm installer.sh
if hash pip3 2>/dev/null; then
    pip3 install --user neovim
else
    echo "Install pip for python3"
fi
# nvim -c ":call dein#install()"
# nvim -c ":UpdateRemotePlugins"
# nvim -c ":wq"
nvim -E -c ":call dein#install()" -c ":UpdateRemotePlugins" -c q

# echo "Installing submodule (s)..."
# git submodule update --init --recursive

echo "Done. Reload your terminal."
