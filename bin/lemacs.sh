#!/bin/sh

OLD_PWD=(pwd)
if [ $LEMACS ];then
    INSTALLED=true
else
    LEMACS=~/Apps/Lemacs
fi
if [ $WSL_DISTRO_NAME ];then
    IN_WSL=true
fi
if [[ $USER == 'liszt' ]];then
    IS_ME=true
fi

install() {
    echo "Installing"
    if [ ! -d $LEMACS ];then
        git clone https://github.com/Liszt21/lemacs $LEMACS
    fi
    if [ ! -f ~/.emacs ];then
        cd $LEMACS
        ln -s "./src/lemacs.el" ~/.emacs
        ln -s "./config/spacemacs" ~/spacemacs
        ln -s "./config/doomemacs" ~/doomemacs
        ln -s "./config/.doom.d" ~/.doom.d
        ln -s "./config/.spacemacs.d" ~/.spacemacs.d
        cd $OLDPWD
    fi
}

uninstall() {
    echo "Uninstalling"
    
    rm  ~/.emacs
    rm  ~/spacemacs
    rm  ~/doomemacs
    rm  ~/.doom.d
    rm  ~/.spacemacs.d
    rm -rf $LEMACS
}

check() {
    if [ $INSTALLED ];then
        return
    fi
    install
}

update() {
    echo "Updating"
    git pull origin master
    git submodule init
    git submodule update
}

check
cd $LEMACS
update

if [ ! $1 ];then
    echo "$1"
fi
cd $OLDPWD
