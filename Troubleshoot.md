## SSH

### Generate a new SSH key

  ```
    ssh-keygen -t rsa -b 4096 -C "your_email@example.com"
  ```

### Add key to ssh-agent

  - Start ssh-agent in background
  ```
    eval "$(ssh-agent -s)"
  ```

  - Add key to the agent(private)
  ```
    ssh-add ~/.ssh/id_rsa
  ```

## Font

  - Download a [font](http://nerdfonts.com/)
  - Unzip and copy to `~/fonts/`
  - Run `fc-cache -fv` to manually rebuild the font cache


## Git

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

# Ubuntu

## Process
  - check process
  ```
    sudo netstat -tnpl
  ```

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

# Handy Commands

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
