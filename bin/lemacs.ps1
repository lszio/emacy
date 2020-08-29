
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
    if (!$(Get-Command lemacs)){
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
    }
    if ($LEMACS_DEBUG){
        $LEMACS = "."
    }else {
        $LEMACS = $ENV:LEMACS
    }

    cd $LEMACS
}

function Help {
    Write-Output "Lemacs --Help"
}

function Update {
    Write-Output "Update"

}

Write-Output "Lemacs"
Check

if (!$args){
    Update
    cd $env:scoop
} else {
    Write-Output "$args"
    Write-Output $args.Count
    Write-Output $args[1..$args.Count]
    switch ($args[0]){
        'doom' {
            Write-Output "Doom"
            .\config\doomemacs\bin\doom.cmd $args[1..$args.Count]
        }
    }
}
