
#  ██▓    ▓█████  ███▄ ▄███▓ ▄▄▄       ▄████▄    ██████ 
# ▓██▒    ▓█   ▀ ▓██▒▀█▀ ██▒▒████▄    ▒██▀ ▀█  ▒██    ▒ 
# ▒██░    ▒███   ▓██    ▓██░▒██  ▀█▄  ▒▓█    ▄ ░ ▓██▄   
# ▒██░    ▒▓█  ▄ ▒██    ▒██ ░██▄▄▄▄██ ▒▓▓▄ ▄██▒  ▒   ██▒
# ░██████▒░▒████▒▒██▒   ░██▒ ▓█   ▓██▒▒ ▓███▀ ░▒██████▒▒
# ░ ▒░▓  ░░░ ▒░ ░░ ▒░   ░  ░ ▒▒   ▓▒█░░ ░▒ ▒  ░▒ ▒▓▒ ▒ ░
# ░ ░ ▒  ░ ░ ░  ░░  ░      ░  ▒   ▒▒ ░  ░  ▒   ░ ░▒  ░ ░
#   ░ ░      ░   ░      ░     ░   ▒   ░        ░  ░  ░  
#     ░  ░   ░  ░       ░         ░  ░░ ░            ░  
#                                     ░                 

if ($ENV:USERNAME -eq 'liszt'){
    $ISME = $true
}else{
    $ISME = $false
}

function Check {
    if (!$ENV:LEMACS){
        if ($LEMACS_DEBUG){
            $LEMACS = "."
        }
        Write-Output "Lemacs isn't installed!"

        if(!$(Get-Command emacs)){
            Write-Output "Emacs isn't installed!"

            if(!$(Get-Command scoop)){
                Write-Output "Scoop isn't installed! Installing..."
                Invoke-WebRequest -useb https://gitee.com/liszt21/AuTools/raw/master/script/system/init/windows.ps1 | Invoke-Expression
                scoop install emacs
            }
            scoop install git
            scoop bucket add extras
            scoop install emacs
        }
        scoop bucket add dragon https://github.com/Liszt21/Dragon
        scoop install lemacs
        $ENV:LEMACS = ($ENV:SCOOP + "/apps/lemacs/current")
        [environment]::setEnvironmentVariable('LEMACS',$ENV:LEMACS,'User')
        Set-Location ($ENV:LEMACS + "\..")
        Remove-Item -r master
        git clone https://github.com/Liszt21/Lemacs master
        Set-Location master
        git submodule init
        git submodule update
        sudo ln -s ($ENV:LEMACS + "/src/lemacs.el") ("C:/Users/$ENV:USERNAME/AppData/Roaming/.emacs")
        if ($ISME) {
            sudo ln -s $ENV:LEMACS C:/Liszt/Projects/Lemacs
        }
    }else {
        $LEMACS = $ENV:LEMACS
    }
    Set-Location $LEMACS
}

function Help {
    Write-Output "Lemacs --Help"
}

function Update {
    Write-Output "Update"
    git fetch origin master
    git submodule foreach git fetch 
    git submodule update
}

Write-Output "Lemacs"
Check

if (!$args){
    Update
} else {
    switch ($args[0]){
        'doom' {
            Write-Output "Doom"
            .\config\doomemacs\bin\doom.cmd $args[1..$args.Count]
        }
    }
}
