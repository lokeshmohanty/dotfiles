# to fix rsync, sftp errors
if status --is-interactive

	fish_vi_key_bindings

	# set fish greeting to none
	set fish_greeting

	# set PATH ~/Applications/flutter/bin $PATH
	# set PATH ~/.bin $PATH

	# Add GOBIN to path
	set PATH $GOBIN $PATH

	# Add android/sdk/tools/bin to path
	# set PATH ~/Android/Sdk/tools/bin $PATH

	# Add .local/bin and .bin to path
	set PATH ~/.local/bin $PATH

	# Add cabal and ghcup binaries to path
	# set PATH ~/.cabal/bin $PATH
	# set PATH ~/.ghcup/bin $PATH

	# nvm use v14.18.2

	if test -e ~/.config/shell/aliasrc
			bass source ~/.config/shell/aliasrc
	end

  conda activate rl

  set -x NIX_CONFIG "experimental-features = nix-command flakes"
end