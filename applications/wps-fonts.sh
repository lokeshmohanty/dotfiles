#!/usr/bin/env bash

SOURCE="https://github.com/IamDH4/ttf-wps-fonts.git"

cd /tmp
# For POSIX 
# command -v foo >/dev/null 2>&1 || { echo >&2 "I require foo but it's not installed.  Aborting."; exit 1; }
if hash git 2>/dev/null; then
  git clone ${SOURCE}
else
  exit 0
fi
cd "ttf-wps-fonts"

if [[ $(id -u) -ne 0 ]] ; then
  echo "Sudo privileges required!"
  exit 1
fi

HOME_FONT="$HOME/.fonts"
FONT_DIR="/usr/share/fonts"

if test -e $FONT_DIR ; then
        FONT_PATH=$FONT_DIR
else
        FONT_PATH=$HOME_FONT
fi

FONT_PATH=$FONT_PATH"/wps-fonts"

if [ -d "$FONT_PATH" ]; then
  # flush stdin
  while read -r -t 0; do read -r; done 
  read -p "Font Directory already exists, continue? [y/N] " -n 1 -r 

  if [[ $REPLY == "" ]]; then
    exit 0
  elif [[ $REPLY =~ ^[Nn]$ ]]; then
    exit 0
  fi
fi

echo -e "\nFonts will be installed in: "$FONT_PATH
read -p "Continue with installation? [Y/n] " -n 1 -r

if [[ $REPLY =~ ^[Nn]$ ]]; then
  exit 0
fi

if [ ! -d "$FONT_PATH" ]; then
  echo "Creating Font Directory..."
  mkdir $FONT_PATH
fi

echo "Installing Fonts..."
cp *.ttf $FONT_PATH
cp *.TTF $FONT_PATH
echo "Fixing Permissions..."
chmod 644 $FONT_PATH/*
echo "Rebuilding Font Cache..."
fc-cache -vfs
echo "Installation Finished."

exit 0
