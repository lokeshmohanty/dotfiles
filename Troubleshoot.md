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

  - Add key to the agent
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

# Ubuntu

## dpkg

  - Resolve `trying to overwrite error`
  ```
    sudo dpkg -i --force-overwrite <pkg-name>
    sudo apt -f install
  ```
