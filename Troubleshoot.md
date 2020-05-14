## SSH

### Generate a new SSH key

  ```
    ssh-keygen -t rsa -b 4096 -C "your_email@example.com"
  ```

### Add key to ssh-agent

  - Start ssh-agent in background(bash)
  ```
    eval "$(ssh-agent -s)"
  ```

  - Start ssh-agent in background(fish)
  ```
    eval (ssh-agent -c)
  ```

  - Add key to the agent(private)
  ```
    ssh-add ~/.ssh/id_rsa
  ```

  - List added keys to ssh-agent
  ```
    ssh-add -l
  ```

## Font

  - Download a [font](http://nerdfonts.com/)
  - Unzip and copy to `~/fonts/`
  - Run `fc-cache -fv` to manually rebuild the font cache

  - If icons appear as rectangles then install Font-awesome and restart


## Git

  - To set upstream branch
  ```
    git branch --set-upstream-to <remote/branch-name>
  ```

  - To force pull a remote in git
  ```
    git fetch origin master
    git reset --hard origin/master
  ```
  - To force a pull from remote while maintaining local commits(local commits stored in new-branch)
  ```
    git checkout master
    git branch new-branch
    git fetch --all
    git reset --hard origin/master
  ```

  - To stash uncommitted changes
  ```
    git stash
  ```
  - To reapply stashed changes
  ```
    git stash pop
  ```
  - To send a pull request to upstream (start is the commit from where you last pulled)
  ```
    git request-pull <start> <url> <end, default: HEAD>
  ```

## Haskell
  - Install haskell
  ```
    wget -qO- https://get.haskellstack.org/ | sh
  ```

# Linux

## Process
  - check process
  ```
    sudo netstat -tnpl
  ```

  - Application exists when it tries to open a file window: Reinstalling `shared-mime-info` fixes it

## neovim

  - Build from source
  ```
    make CMAKE_BUILD_TYPE=RelWithDebInfo
    sudo make install
  ```

  - Dependencies
  ```
    sudo apt install cmake pkg-config libtool libtool-bin m4 automake gettext
  ```
## dpkg

  - Resolve `trying to overwrite error`
  ```
    sudo dpkg -i --force-overwrite <pkg-name>
    sudo apt -f install
  ```

## Firewall

  - Check allowed ports
  ```
  sudo ufw status verbose
  ```

  - Allow a port
  ```
  sudo ufw allow 3000/tcp
  ```

## Youtube-dl

  - Download audio of a video from youtube
  ```
    youtube-dl --extract-audio --audio-format mp3 <url>
  ```

## Stack (haskell)

  - Uninstall stack
  ```
    rm -rf ~/.stack
    rm -rf /usr/local/bin/stack
  ```
  - Install stack

## Xmonad

  - Get WM_CLASS for manageHook
  ```
    xprop | grep WM_CLASS
  ```
## Npm errors

  - For Error: EACCESS: permission denied, use the following argument
  ```sh
    --unsafe-perm=true --allow-root
    ```
    
## locate command

  - Update the database
  ```sh
    sudo updatedb
  ```

  - locate with permissions
  ```sh
    ls -l $(locate -e <fileName>)
  ```

# Tips

## Miscellaneous

  - Hardware Info
  ```
    inxi -Fxz
  ```

  - Find processes
  ```
    sudo netstat -tnlp
  ```

  - Kill a process with pid
  ```
    sudo kill -9 <pid>
  ```

  - View running process
  ```
    ps -aux
  ```

  - Get window information
  ```
    xwininfo
  ```

  - Find graphics card model
  ```
    lspci | grep -i vga
  ```

  - Find hardware info (display)
  ```
    lshw -class display
  ```

  - View installed locale: `locale -a`
  - Setup locale: `echo "LANG=en_US.UTF-8" > /etc/locale.conf`

  - Splitting/Compression
  ```
    # create archives
    $ tar cz my_large_file_1 my_large_file_2 | split -b 1024MiB - myfiles_split.tgz_
    # uncompress
    $ cat myfiles_split.tgz_* | tar xz
  ```

# Handy Commands

## Ubuntu
  - Lock user
  ```
    gnome-screensaver-command -l
  ```

  - Unlock user
  ```
    loginctl unlock-session <session-id>
  ```

  - Get all sessions
  ```
    loginctl list-sessions --no-legend | while read id rest; do echo; loginctl show-session $id; done
  ```

## Scrot
  - Generate thumbnail, <num> is percentage of original size
  ```
    scrot --thumb <num>
  ``` 

  - Execute operations on saved images
  ```
    scrot -e 'mv $f ~/Pictures/'
  ```

  - Adjust quality of screenshot, <num> is in the scale of 1-100
  ```
    scrot --quality <num>
  ```

