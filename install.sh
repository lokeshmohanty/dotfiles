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
    CONFIG_DIRECTORIES=("emacs" "fish" "qutebrowser" "xmonad" "shell" "zathura" "nvim" "starship" "sxiv")
    OTHER_DIRECTORIES=(".local/bin/custom" ".local/share/applications")

    echo "Symlinking directories..."
    mkdir -p ${BACKUP_DIR}/.config
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

    stack upgrade
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

    # python packages
    # pip install pyright matplotlib 

    # npm packages
    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.5/install.sh | bash
    nvm install --lts
    nvm use --lts
    # npm i -g surge

    # install/update nnn plugins
    # curl -Ls https://raw.githubusercontent.com/jarun/nnn/master/plugins/getplugs | sh

    # gpg enable pinentry
    mkdir -p $HOME/.local/share/gnupg
    echo "enable-ssh-support" > "$HOME/.local/share/gnupg/gpg-agent.conf"
    echo "pinentry-program /bin/pinentry-gtk-2" >> "$HOME/.local/share/gnupg/gpg-agent.conf"
}

setup_gh() {
    gh auth login
}

setup_imap() {
    go install gitlab.com/shackra/goimapnotify@latest
    # goimapnotify -conf ~/.dotfiles/.config/imap/go-gmail.json
    # goimapnotify -conf ~/.dotfiles/.config/imap/go-yahoo.json
    # goimapnotify -conf ~/.dotfiles/.config/imap/go-outlook.json
}

install_source_packages() {
    mkdir -p $HOME/.local/src
    gh repo clone lokesh1197/void-packages
    cd void-packages

    $HOME/.local/src/void-packages/xbps-src binary-bootstrap
    echo XBPS_ALLOW_RESTRICTED=yes >> etc/conf

    packages="cronie any-desk teams-bin"
    $HOME/.local/src/void-packages/xbps-src pkg $packages
    xi $packages
}

install_extra_packages() {
    mkdir -p $HOME/.local/src
    cd $HOME/.local/src
    repositories=(
        "lokesh1197/mutt-wizard"
        "pystardust/ytfzf"
        "djcb/mu"
    )
    for repo in $repositories ;
    do
        gh repo clone $repo
    done

    tars=("https://github.com/ventoy/Ventoy/releases")
}

install_emacs() {
    # https://git.savannah.gnu.org/cgit/emacs.git/tree/INSTALL
		mkdir -p ~/.local/src
		cd ~/.local/src
    git clone git://git.sv.gnu.org/emacs.git
    cd emacs
    ./autogen.sh
    ./configure --with-mailutils --with-native-compilation --with-json --with-imagemagick
		--with-cairo --with-tree-sitter --with-gif --with-png --with-jpeg --with-rsvg
		--with-tiff
}

package_extra_steps() {
    flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo

    # wine extra packages
    xi vkd3d vkd3d-32bit mesa-vulkan-radeon mesa-vulkan-radeon-32bit Vulkan-Tools amdvlk amdvlk-32bit

    # then using winetricks install dxvk in 32bit

}


install_dotfiles() {
    echo "Installing using the dotfiles method (i.e., by creating symlinks)"

    DOTFILES_DIR="${HOME}/.dotfiles"
    BACKUP_DIR="${HOME}/.dotfiles/backup"
    BRANCH="voidlinux"
		OS=$(grep "ID" /etc/os-release | cut -d= -f2 | head -n1)

		if [[ $(grep "ID" /etc/os-release | head -n1 | cut -d= -f2) == "void" ]]; then
				xbps-install -Sy
				xbps-install -u xbps
				xbps-install -Syu
				install_packages
		fi

    echo "Cloning dotfiles repository to $DOTFILES_DIR"
    # git clone -b $BRANCH --recurse-submodules $GIT_CLONE_URL $DOTFILES_DIR

    echo "OS_NAME = $(uname)"
    echo "This will create symbolic links of this directory's files in the home directory"
    echo "The conflicting dotfiles of home directory will be backed up to $BACKUP_DIR"
    mkdir -p $BACKUP_DIR

    echo "Installing dotfiles..."
    mkdir -p $HOME/.config
    mkdir -p $HOME/.local/share
    mkdir -p $HOME/.local/bin
    symlink_directories
    symlink_files

    install_xmonad

    # get wallpapers
    # git clone https://gitlab.com/lokesh1197/wallpapers.git $HOME/.local/share/wallpapers

    # setup_gh
    # install_source_packages


    # Install vim plugins if not alread present.
    # [ ! -f "/home/$name/.config/nvim/autoload/plug.vim" ] && vim_plugin_install

    # if hash python3 2>/dev/null; then
    #     python3 -m pip install --user neovim
    #     python3 -m pip install --user pynvim
    #     # pip3 install --user pynvim
    # else
    #     echo "Install pip for python3"
    # fi

    # nvim -E -c ":call dein#install()" -c ":UpdateRemotePlugins" -c q

    # echo "Installing submodule (s)..."
    # git submodule update --init --recursive

    echo "Done. Reload your terminal."
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

    mkdir -p /etc/cron.weekly

    # Periodic trim for ssd
    printf "#!/bin/sh\nfstrim /\nfstrim /home" > /etc/cron.weekly/fstrim
    chmod u+x /etc/cron.weekly/fstrim
}

runit_commands() {
    services=("NetworkManager" "dbus" "iwd" "nanoklogd" "socklog-unix" "sshd" "uuidd" "bluetoothd" "udevd" "tlp" "cronie")
    for service in "${services[@]}" ;
    do
        # if [ -d "/var/service/${service}" ] ; then
        #     echo "$service is already enabled"
        # else
            ln -sv "/etc/sv/${service}" "/var/service/${service}"
#         fi
    done
    # remove dhcpcd as it conflicts with NetworkManager
    rm -rf /var/service/dhcpcd
}

voidlinux_fixes() {
    printf "\n[General]\nUseDefaultInterface=true" >> /etc/iwd/main.conf
    printf "\n[Device]\nwifi.backend=iwd" >> /etc/NetworkManager/NetworkManager.conf
}

superuser_commands() {
    [[ $EUID -ne 0 ]] && echo "This script must be run as root." && exit 1
    # Most important command! Get rid of the beep!
    mkdir -p /etc/modprobe.d
    rmmod pcspkr
    echo "blacklist pcspkr" > /etc/modprobe.d/nobeep.conf

    # # dbus UUID must be generated for Artix runit.
    # dbus-uuidgen >/var/lib/dbus/machine-id

    # # Use system notifications for Brave on Artix
    # echo "export \$(dbus-launch)" >/etc/profile.d/dbus.sh

    # Enable tap to click and natural scrolling
    mkdir -p /etc/X11/xorg.conf.d
    [ ! -f /etc/X11/xorg.conf.d/40-libinput.conf ] && printf 'Section "InputClass"
            Identifier "libinput touchpad catchall"
            MatchIsTouchpad "on"
            MatchDevicePath "/dev/input/event*"
            Driver "libinput"
    # Enable left mouse button by tapping
    Option "Tapping" "on"
    Option "NaturalScrolling" "true"
    EndSection' >/etc/X11/xorg.conf.d/40-libinput.conf

    # Force modesetting driver
#     [ ! -f /etc/X11/xorg.conf.d/10-modesetting.conf ] && printf 'Section "Device"
#     Identifier "GPU0"
#     Driver "modesetting"
# EndSection' > /etc/X11/xorg.conf.d/10-modesetting.conf

    runit_commands
    cron_commands

    # add user to bluetooth group
    usermod -aG bluetooth "$(id -u -n)"

    # set default grub timeout to 1
    sed -i s/^(GRUB_TIMEOUT=)[0-9]*$/11/ /etc/default/grub
    update-grub

    # replace add with change to give write access to brightness in 90-backlight.rules file
    # give user access to /sys/class/leds for xbacklight
    mkdir -p /etc/udev/rules.d/
    touch /etc/udev/rules.d/90-leds.rules
    echo 'SUBSYSTEM=="leds", ACTION=="add"' >> /etc/udev/rules.d/90-leds.rules
    echo 'RUN+="/bin/chgrp video /sys/class/leds/%k/brightness"' >> /etc/udev/rules.d/90-leds.rules
    echo 'RUN+="/bin/chmod g+w /sys/class/leds/%k/brightness"' >> /etc/udev/rules.d/90-leds.rules

    # # fix Boot Error: Unknown key identifier 'zoom'
    # mkdir -p /etc/udev/hwdb.d
    # cp /usr/lib/udev/hwdb.d/60-keyboard.hwdb /etc/udev/hwdb.d/
    # remove the lines with zoom under Lenovo(13=zoom) and Sony(0e=zoom)
    # udevadm hwdb --update && sudo udevadm control --reload-rules && sudo udevadm trigger
}

case $1 in
    1) superuser_commands;;
		2) install_emacs
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
