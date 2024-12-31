#!/bin/zsh
# ==============================================================================
# Title: install.sh
#  (heavily inspired by/borrowed from Daniel Vier)
#  Automates the setup and configuration of macOS,
#  including installation of essential applications and
# system preferences.
# Author: Tae Eun Kim
# Last Updated: December 30, 2024
# ==============================================================================

source ./macos-config

# COLOR
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

#########
# Start #
#########

clear
echo " _           _        _ _       _     "
echo "(_)         | |      | | |     | |    "
echo " _ _ __  ___| |_ __ _| | |  ___| |__  "
echo "| | |_ \/ __| __/ _  | | | / __| |_ \ "
echo "| | | | \__ \ || (_| | | |_\__ \ | | |"
echo "|_|_| |_|___/\__\__,_|_|_(_)___/_| |_|"
echo
echo
echo Enter root password

# Ask for the administrator password upfront.
sudo -v

# Keep Sudo until script is finished
while true; do
  sudo -n true
  sleep 60
  kill -0 "$$" || exit
done 2>/dev/null &

# Update macOS
echo
echo "${GREEN}Looking for updates.."
echo
sudo softwareupdate -i -a

# Install Rosetta
sudo softwareupdate --install-rosetta --agree-to-license

# Install Xcode
xcode-select -p &> /dev/null
if [ $? -ne 0 ]; then
    echo "Xcode CLI tools not found. Installing them..."
    touch /tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress;
    PROD=$(softwareupdate -l |
               grep "\*.*Command Line" |
               head -n 1 | awk -F"*" '{print $2}' |
               sed -e 's/^ *//' |
               tr -d '\n')
    softwareupdate -i "$PROD" -v;
else
    echo "'xcode command line tools' is already installed, you're set."
fi

# Install Homebrew
echo
echo "${GREEN}Installing Homebrew"
echo
NONINTERACTIVE=1 /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# # Append Homebrew initialization to .zprofile --> .zprofile will be pulled from dotfiles repo
# echo 'eval "$(/opt/homebrew/bin/brew shellenv)"' >>${HOME}/.zprofile

# Immediately evaluate the Homebrew environment settings for the current session
eval "$(/opt/homebrew/bin/brew shellenv)"

# Check installation and update
echo
echo "${GREEN}Checking installation.."
echo
brew update && brew doctor
export HOMEBREW_NO_INSTALL_CLEANUP=1

# Check for Brewfile in the current directory and use it if present
if [ -f "./Brewfile" ]; then
  echo
  echo "${GREEN}Brewfile found. Using it to install packages..."
  brew bundle
  echo "${GREEN}Installation from Brewfile complete."
else
  # If no Brewfile is present, continue with the default installation

  # Install Casks and Formulae
  echo
  echo "${GREEN}Installing formulae..."
  for formula in "${FORMULAE[@]}"; do
    # brew install "$formula"
    # if [ $? -ne 0 ]; then      
    if ! brew install "$formula"; then
      echo "${RED}Failed to install $formula. Continuing...${NC}"
    fi
  done  

  echo "${GREEN}Installing casks..."
  for cask in "${CASKS[@]}"; do
    # brew install --cask "$cask"
    # if [ $? -ne 0 ]; then
    if ! brew install --cask "$cask"; then	
      echo "${RED}Failed to install $cask. Continuing...${NC}"
    fi
  done

  # App Store
  echo
  echo -n "${RED}Install apps from App Store? ${NC}[y/N]"
  read REPLY
  if [[ $REPLY =~ ^[Yy]$ ]]; then
    brew install mas
    for app in "${APPSTORE[@]}"; do
      eval "mas install $app"
    done
  fi
fi

# MacTeX
echo
echo -n "${RED}Install MacTeX ${NC}[y/N]"
read REPLY
if [[ $REPLY =~ ^[Yy]$ ]]; then
  brew install --cask mactex-no-gui
  # to add tex executables to path
  eval "$(/usr/libexec/path_helper)"
  
  # Set up `texmf' tree
  mkdir -p $HOME/Library/texmf/tex/latex

  # TODO: Move custom latex class and style files to the dotfiles so
  # that they can be stowed to the texmf directory created above.
fi

# CPDF
echo
echo -n "${RED}Install CPDF ${NC}[y/N]"
read REPLY
if [[ $REPLY =~ ^[Yy]$ ]]; then
  brew tap oncletom/cpdf
  brew install cpdf
fi

# Emacs-Plus
echo
echo -n "${RED}Install Emacs-Plus ${NC}[y/N]"
read REPLY
if [[ $REPLY =~ ^[Yy]$ ]]; then
  brew tap d12frosted/emacs-plus
  brew install emacs-plus
fi
echo -n "${GREEN}Then either copy or symlink /opt/homebrew/opt/emacs-plus/Emacs.app to /Applications/Emacs.app.${NC}"

# Fonts and Nerd Fonts
echo
echo -n "${RED}Install Nerd-Fonts ${NC}[y/N]"
read REPLY
if [[ $REPLY =~ ^[Yy]$ ]]; then
  brew tap homebrew/cask-fonts         # You only need to do this once!
  brew install --cask font-fira-code-nerd-font
  brew install --cask font-hack-nerd-font
  brew install --cask font-roboto-mono-nerd-font
  brew install --cask font-roboto-mono
  brew install --cask font-jetbrains-mono-nerd-font
  brew install --cask font-iosevka-nerd-font
  brew install --cask font-iosevka-comfy
fi

# TODO: zsh plugins
# git clone https://github.com/zsh-users/zsh-autosuggestions ~/.zsh/zsh-autosuggestions
# git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ~/.zsh/zsh-syntax-highlighting

# Cleanup
echo
echo "${GREEN}Cleaning up..."
brew update && brew upgrade && brew cleanup && brew doctor
mkdir -p ~/Library/LaunchAgents
brew tap homebrew/autoupdate
brew autoupdate start $HOMEBREW_UPDATE_FREQUENCY --upgrade --cleanup --immediate --sudo

# Settings
echo
echo -n "${RED}Configure default system settings? ${NC}[Y/n]"
read REPLY
if [[ -z $REPLY || $REPLY =~ ^[Yy]$ ]]; then
  echo "${GREEN}Configuring default settings..."
  for setting in "${SETTINGS[@]}"; do
    eval $setting
  done
fi

# Dock settings
echo
echo -n "${RED}Apply Dock settings?? ${NC}[y/N]"
read REPLY
if [[ $REPLY =~ ^[Yy]$ ]]; then
  brew install dockutil
  # Handle replacements
  for item in "${DOCK_REPLACE[@]}"; do
    IFS="|" read -r add_app replace_app <<<"$item"
    dockutil --add "$add_app" --replacing "$replace_app" &>/dev/null
  done
  # Handle additions
  for app in "${DOCK_ADD[@]}"; do
    dockutil --add "$app" &>/dev/null
  done
  # # Handle removals
  # for app in "${DOCK_REMOVE[@]}"; do
  #   dockutil --remove "$app" &>/dev/null
  # done
fi

# Git Login
echo
echo "${GREEN}SET UP GIT"
echo

echo "${RED}Please enter your git username:${NC}"
read name
echo "${RED}Please enter your git email:${NC}"
read email

git config --global user.name "$name"
git config --global user.email "$email"
git config --global color.ui true

echo
echo "${GREEN}GITTY UP!"

# HUSH LOGIN: Remove "Last Login" message when opening a terminal
echo
echo "${GREEN}HUSH LOGIN"
echo

echo
touch ~/.hushlogin

# # ohmyzsh
# echo
# echo "${GREEN}Installing ohmyzsh!"
# echo
# sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended

clear
echo "${GREEN}______ _____ _   _  _____ "
echo "${GREEN}|  _  \  _  | \ | ||  ___|"
echo "${GREEN}| | | | | | |  \| || |__  "
echo "${GREEN}| | | | | | | .   ||  __| "
echo "${GREEN}| |/ /\ \_/ / |\  || |___ "
echo "${GREEN}|___/  \___/\_| \_/\____/ "

echo
echo
printf "${RED}"
read -s -k $'?Press ANY KEY to REBOOT\n'
sudo reboot
exit
