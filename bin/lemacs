#!/bin/bash

if [ "$LEMACS_HOME" ];then
    INSTALLED=true
else
    LEMACS_HOME=~/Apps/lemacs
fi
if [ "$WSL_DISTRO_NAME" ];then
    IN_WSL=true
fi
if [ "$USER" = 'liszt' ];then
    IS_ME=true
fi

install() {
    echo "Installing"
    if [ ! -d $LEMACS_HOME ];then
        if [ $IN_WSL ] && [ $IS_ME ] && [ -d /mnt/c/Liszt/Projects/Lemacs ];then
            ln -s /mnt/c/Liszt/Projects/Lemacs $LEMACS_HOME
        else
            git clone --recursive https://github.com/Liszt21/lemacs $LEMACS_HOME
        fi
    fi
    if [ -f ~/.emacs ];then
        mv ~/.emacs ~/.emacs.bak
    fi
    if [ ! -L ~/.emacs ];then
        ln -s $LEMACS_HOME/config/spacemacs ~/spacemacs
        ln -s $LEMACS_HOME/config/doomemacs ~/doomemacs
        ln -s $LEMACS_HOME/config/.doom.d ~/.doom.d
        ln -s $LEMACS_HOME/config/.spacemacs.d ~/.spacemacs.d
        ln -s $LEMACS_HOME/config/chemacs2 ~/.emacs.d
        ln -s $LEMACS_HOME/src/profile.el ~/.emacs-profiles.el
    fi
    # if [ ! -f ~/.zshrc ];then
    #     PROFILE=~/.bashrc
    # else
    #     PROFILE=~/.zshrc
    # fi
    # echo "source $LEMACS_HOME/src/entry" >> $PROFILE
}

clean() {
    echo "Clean"
    
    rm ~/.emacs.d
    rm ~/.emacs-profiles.el
    rm ~/spacemacs
    rm ~/doomemacs
    rm ~/.doom.d
    rm ~/.spacemacs.d
    # rm -rf $LEMACS_HOME
}

check() {
    if [ $INSTALLED ];then
        return
    fi
    install
}

update() {
    echo "Updating"
    # git pull origin master
    # git submodule init
    # git submodule update
    git submodule foreach git pull --ff-only
}

check
cd $LEMACS_HOME || exit

case $1 in
    C|clean)
        echo "clean"
        clean
        ;;
    I|install)
        echo "Install"
        install
        ;;
    D|doom)
        echo "Doom"
        $LEMACS_HOME/config/doomemacs/bin/doom "$2"
        ;;
    U|update)
        echo "Update"
        update
        ;;
    R|run)
        echo "Run "
        case $2 in
            [sS]pacemacs)
                echo "Spacemacs"
                emacs --with-profile spacemacs
                ;;
            [dD]oom)
                echo "Doom"
                emacs --with-profile doomemacs
                ;;
        esac
        ;;
    *)
        echo "Help"
        # update
        ;;
esac

cd "$OLDPWD" || exit
 
