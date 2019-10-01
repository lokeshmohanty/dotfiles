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
