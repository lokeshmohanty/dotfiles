# Xmonad dotfiles

## Location
 - xmonad: `~/.xmonad/`
 - xinitrc : `~/.xinitrc`

## Commands

  - Install GHC with stack
  ```
    stack setup
  ```
  
  - Create `stack.yaml` file if it doesn't exist
  ```
    stack init
  ```

  - To build and install or update
  ```
    stack install
  ```

  - Recompile `xmonad.hs`
  ```
    xmonad --recompile
  ```

  - Restart `xmonad.hs`
  ```
    xmonad --restart
  ```

## Troubleshooting

  - Make the build executable by running `chmod a+x build`
  - Run `stack clean` when you add new flag or extra dependencies

## Dependencies (Voidlinux)

### For xmonad
  - stack
  - libXft-devel
  - libXinerama-devel, libXrandr-devel, libXScrnSaver-devel
  - pkg-config

### For xmobar
  - alsa-lib-devel
  - wireless_tools-devel
  - libXpm-devel
