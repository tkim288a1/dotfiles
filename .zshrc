# Sensible, short .zshrc
# Gist page: git.io/vSBRk
# Raw file:  curl -L git.io/sensible-zshrc

# GNU and BSD (macOS) ls flags aren't compatible
ls --version &>/dev/null
if [ $? -eq 0 ]; then
    lsflags="--color --group-directories-first -F"
else
    lsflags="-GF"
    export CLICOLOR=1
fi

# Aliases
alias ls="ls ${lsflags}"
alias ll="ls ${lsflags} -l"
alias la="ls ${lsflags} -la"
alias l="ls ${lsflags} -lah"
alias h="history"
alias hg="history -1000 | grep -i"
alias ,="cd .."
alias m="less"

# GIT
# Do this: git config --global url.ssh://git@github.com/.insteadOf https://github.com
alias gd="git diff"
alias gs="git status 2>/dev/null"
alias gl="git log --oneline"
function gc() { git clone ssh://git@github.com/"$*" }
function gg() { git commit -m "$*" }

# PROMPTS
# Enable colors and change prompt: This is from Luke Smith
autoload -U colors && colors    # Load colors

# Show git branch
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git:*' formats '%F{cyan}(%b)%f'
precmd_vcs_info() { vcs_info }
precmd_functions+=( precmd_vcs_info )
setopt prompt_subst
PROMPT='%(?.%F{green}√.%F{red}?%?)%f %B%F{yellow}%~%f%b ${vcs_info_msg_0_}%(!.#.>) '
RPROMPT='< %F{magenta}%*%f'

# History settings
HISTFILE=~/.history-zsh
HISTSIZE=10000
SAVEHIST=10000
setopt append_history           # allow multiple sessions to append to one history
setopt bang_hist                # treat ! special during command expansion
setopt extended_history         # Write history in :start:elasped;command format
setopt hist_expire_dups_first   # expire duplicates first when trimming history
setopt hist_find_no_dups        # When searching history, don't repeat
setopt hist_ignore_dups         # ignore duplicate entries of previous events
setopt hist_ignore_space        # prefix command with a space to skip it's recording
setopt hist_reduce_blanks       # Remove extra blanks from each command added to history
setopt hist_verify              # Don't execute immediately upon history expansion
setopt inc_append_history       # Write to history file immediately, not when shell quits
setopt share_history            # Share history among all sessions

# Tab completion
autoload -Uz compinit && compinit
setopt complete_in_word         # cd /ho/sco/tm<TAB> expands to /home/scott/tmp
setopt auto_menu                # show completion menu on succesive tab presses
setopt autocd                   # cd to a folder just by typing it's name
ZLE_REMOVE_SUFFIX_CHARS=$' \t\n;&' # These "eat" the auto prior space after a tab complete

# MISC
setopt interactive_comments     # allow # comments in shell; good for copy/paste
unsetopt correct_all            # I don't care for 'suggestions' from ZSH
export BLOCK_SIZE="'1"          # Add commas to file sizes

# PATH
typeset -U path                 # keep duplicates out of the path
path+=(.)                       # append current directory to path (controversial)

# BINDKEY
bindkey -e
bindkey '\e[3~' delete-char
bindkey '^p' history-search-backward
bindkey '^n' history-search-forward
bindkey ' '  magic-space

# COMPLETION
unsetopt menu_complete   # do not autoselect the first completion entry
unsetopt flowcontrol
setopt auto_menu         # show completion menu on succesive tab press
setopt complete_in_word
setopt always_to_end

WORDCHARS=''

zmodload -i zsh/complist

## case-insensitive (all),partial-word and then substring completion
if [ "x$CASE_SENSITIVE" = "xtrue" ]; then
    zstyle ':completion:*' matcher-list 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
    unset CASE_SENSITIVE
else
    zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
fi

zstyle ':completion:*' list-colors ''

# should this be in keybindings?
bindkey -M menuselect '^o' accept-and-infer-next-history

zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*:*:*:*:processes' command "ps -u `whoami` -o pid,user,comm -w -w"

# disable named-directories autocompletion
zstyle ':completion:*:cd:*' tag-order local-directories directory-stack path-directories

zstyle ':completion:*' users off

# Use caching so that commands like apt and dpkg complete are useable
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion::complete:*' cache-path $ZSH/cache/
# Don't complete uninteresting users
zstyle ':completion:*:*:*:users' ignored-patterns \
       adm amanda apache avahi beaglidx bin cacti canna clamav daemon \
       dbus distcache dovecot fax ftp games gdm gkrellmd gopher \
       hacluster haldaemon halt hsqldb ident junkbust ldap lp mail \
       mailman mailnull mldonkey mysql nagios \
       named netdump news nfsnobody nobody nscd ntp nut nx openvpn \
       operator pcap postfix postgres privoxy pulse pvm quagga radvd \
       rpc rpcuser rpm shutdown squid sshd sync uucp vcsa xfs

# ... unless we really want to.
zstyle '*' single-ignored show

if [ "x$COMPLETION_WAITING_DOTS" = "xtrue" ]; then
    expand-or-complete-with-dots() {
        echo -n "\e[31m......\e[0m"
        zle expand-or-complete
        zle redisplay
    }
    zle -N expand-or-complete-with-dots
    bindkey "^I" expand-or-complete-with-dots
fi

zstyle -e ':completion:*:(ssh|scp|sftp|rsh|rsync):hosts' hosts 'reply=(${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) /dev/null)"}%%[# ]*}//,/ })'

eval "$(starship init zsh)"
