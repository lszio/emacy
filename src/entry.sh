EMACY_HOME=${EMACY_HOME:-~/.emacy}
export EMACY_HOME

export PATH=$EMACY_HOME/bin:$EMACY_HOME/config/doomemacs/bin:$PATH

alias doomemacs="emacs --with-profile doomemacs"
alias spacemacs="emacs --with-profile spacemacs"