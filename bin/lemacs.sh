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
}

check() {
    if [ $INSTALLED ];then
        return
    fi
    install
}

update() {
    echo "Updating"
    
}

check
cd $LEMACS


cd $OLDPWD