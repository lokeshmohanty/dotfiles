#!/bin/bash

set_dotfiles_remote() {
    echo "Setting git clone url"
    read -n 1 -p "Type 1 for git and 2 for https: " url_type
    case $url_type in
        1) GIT_CLONE_URL="git@github.com:lokesh1197/dotfiles.git";;
        2) GIT_CLONE_URL="https://github.com/lokesh1197/dotfiles.git";;
        *) echo "Invalid input";;
    esac
}

symlink_directories() {
    CONFIG_DIRECTORIES=("emacs" "fish" "qutebrowser" "xmonad" "shell" "zathura" "nvim" "starship")
    OTHER_DIRECTORIES=(".local/bin/custom" ".local/share/applications")

    echo "Symlinking directories..."
    for dir in "${CONFIG_DIRECTORIES[@]}" ;
    do
        if [ -d ${HOME}/.config/${dir} ] ; then
            mv -f ${HOME}/.config/${dir} ${BACKUP_DIR}/.config/
        fi
        ln -sv ${DOTFILES_DIR}/.config/${dir} ${HOME}/.config/${dir}
    done

    for dir in "${OTHER_DIRECTORIES[@]}" ;
    do
        if [ -d ${HOME}/${dir} ] ; then
            mv -f ${HOME}/${dir} ${BACKUP_DIR}/
        fi
        ln -sv ${DOTFILES_DIR}/${dir} ${HOME}/${dir}
    done
}

symlink_files() {
    FILES=(
        "x11/xinitrc:.xinitrc"
        "x11/xprofile:.xprofile"
        "shell/profile:.profile"
        "shell/bashrc:.bashrc"
        "mimeapps.list:.config/mimeapps.list"
        "user-dirs.dirs:.config/user-dirs.dirs"
        ".gitconfig:.gitconfig"
    )

    echo "Symlinking files..."

    for item in "${FILES[@]}" ;
    do
        IFS=":" read -r source dest <<< $item
        if [ -f ${HOME}/${dest} ] ; then
            mv -f ${HOME}/${dest} ${BACKUP_DIR}/
        fi
        ln -sv ${DOTFILES_DIR}/${source} ${HOME}/${dest}
    done
}

vim_plugin_install() {
	# Installs vim plugins.
	echo "Installing neovim plugins..."
	mkdir -p "$HOME/.config/nvim/autoload"
	curl -Ls "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim" >  "$HOME/.config/nvim/autoload/plug.vim"
	nvim -c "PlugInstall|q|q"
}

install_xmonad() {
    cd $HOME/.config/xmonad
    git clone https://github.com/xmonad/xmonad
    git clone https://github.com/xmonad/xmonad-contrib
    git clone https://codeberg.org/xmobar/xmobar

    cd xmobar
    git fetch --tags
    latestTag=$(git describe --tags `git rev-list --tags --max-count=1`)
    git checkout $latestTag
    cd ../

    stack install
}

install_packages() {
    echo "Installing void linux packages"
    DOTFILES_DIR="${HOME}/dotfiles"
    filename=$DOTFILES_DIR/"packages.csv"
    # declare -a packages
    while IFS=, read -r pkg comment; do
        packages="$packages $pkg"
    done < $filename
    xi $packages
}

setup_gh() {
    gh auth login
}

install_source_packages() {
    mkdir -p $HOME/.local/src
    gh repo clone lokesh1197/void-packages
    cd void-packages

    packages="cronie any-desk teams-bin yt-dlp"
    $HOME/.local/src/void-packages/xbps-src binary-bootstrap
    $HOME/.local/src/void-packages/xbps-src pkg $packages
    xi $packages
}

install_dotfiles() {
    echo "Installing using the dotfiles method (i.e., by creating symlinks)"

    DOTFILES_DIR="${HOME}/.dotfiles"
    BACKUP_DIR="${HOME}/.dotfiles/backup"
    BRANCH="void"

    echo "Cloning dotfiles repository to $DOTFILES_DIR"
    # git clone -b $BRANCH --recurse-submodules $GIT_CLONE_URL $DOTFILES_DIR

    echo "OS_NAME = $(uname)"
    echo "This will create symbolic links of this directory's files in the home directory"
    echo "The conflicting dotfiles of home directory will be backed up to $BACKUP_DIR"
    mkdir -p $BACKUP_DIR

    echo "Installing dotfiles..."
    symlink_directories
    symlink_files
    install_packages
    install_xmonad

    # get wallpapers
    git clone https://gitlab.com/lokesh1197/wallpapers.git $HOME/.local/share/wallpapers

    setup_gh
    install_source_packages
    

    # Install vim plugins if not alread present.
    [ ! -f "/home/$name/.config/nvim/autoload/plug.vim" ] && vim_plugin_install


    # if hash python3 2>/dev/null; then
    #     python3 -m pip install --user neovim
    #     python3 -m pip install --user pynvim
    #     # pip3 install --user pynvim
    # else
    #     echo "Install pip for python3"
    # fi

    # nvim -E -c ":call dein#install()" -c ":UpdateRemotePlugins" -c q

    # # echo "Installing submodule (s)..."
    # # git submodule update --init --recursive

    # echo "Done. Reload your terminal."
}

install_bare_repo() {
    echo "Installing using the bare repo method"

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
}


cron_commands() {
    [[ $EUID -ne 0 ]] && echo "This script must be run as root." && exit 1

    # Periodic trim for ssd
    printf "#!/bin/sh\nfstrim /\nfstrim /home" > /etc/cron.weekly/fstrim
    chmod u+x /etc/cron.weekly/fstrim
}

runit_commands() {
    services=("NetworkManager dbus iwd nanoklogd socklog-unix sshd uuidd bluetoothd udevd")
    for service in "${services[@]}" ;
    do
        if [ -d /var/service/${service} ] ; then
            echo "$service is already enabled"
        else
            ln -sv /etc/sv/${service} /var/service/${service}
        fi
    done
}

superuser_commands() {
    [[ $EUID -ne 0 ]] && echo "This script must be run as root." && exit 1
    # Most important command! Get rid of the beep!
    rmmod pcspkr
    echo "blacklist pcspkr" > /etc/modprobe.d/nobeep.conf

    # # dbus UUID must be generated for Artix runit.
    # dbus-uuidgen >/var/lib/dbus/machine-id

    # # Use system notifications for Brave on Artix
    # echo "export \$(dbus-launch)" >/etc/profile.d/dbus.sh

    # Enable tap to click
    [ ! -f /etc/X11/xorg.conf.d/40-libinput.conf ] && printf 'Section "InputClass"
            Identifier "libinput touchpad catchall"
            MatchIsTouchpad "on"
            MatchDevicePath "/dev/input/event*"
            Driver "libinput"
    # Enable left mouse button by tapping
    Option "Tapping" "on"
    EndSection' >/etc/X11/xorg.conf.d/40-libinput.conf

    # Force modesetting driver
#     [ ! -f /etc/X11/xorg.conf.d/10-modesetting.conf ] && printf 'Section "Device"
#     Identifier "GPU0"
#     Driver "modesetting"
# EndSection' > /etc/X11/xorg.conf.d/10-modesetting.conf

    runit_commands
    cron_commands
}

case $1 in
    1) superuser_commands;;
    2) install_packages;;
    *)
        set_dotfiles_remote
        printf "\nChoose an installation method: \n1) Dotfiles (Create a dotfiles folder and symlink the files)\n2) Bare Repository (Create a bare repository and add all files directly at the home folder)\n(1, 2): " 
        read -n 1 installation_method
        case $installation_method in
            1) install_dotfiles;;
            2) install_bare_repo;;
            *) echo "Invalid input";;
        esac
    ;;
esac
