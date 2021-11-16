function emacy() {
    if( [string]::IsNullOrEmpty($env:EMACY_HOME) ) {
        ros "$env:HOME\Emacy\bin\emacy"
    }else{
        ros "$env:EMACY_HOME\bin\emacy"
    }
}

function doomemacs() { 
    emacs --with-profile doomemacs
}

function spacemacs() { 
    emacs --with-profile spacemacs
}