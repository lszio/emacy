if ($ENV:USERNAME -eq 'liszt'){
    $ISME = $true
}else{
    $ISME = $false
}

if ($ENV:LEMACS) {
    $LEMACS = $ENV:LEMACS
}else{
    $LEMACS = "."
}

$Origin_Location = Get-Location

function Debug-Lemacs($message) {
    if ($DEBUG){
        Write-Output "$message"
    }
}

function Update-Git {
    if( !(Test-Path bin/lemacs.ps1)){
        Write-Output "Not in Lemacs"
        return 
    }
    $current = Get-Location
    $folder = ($current -split "\\")[-1]
    if ($folder -eq 'current') {
        $folder = 'master'
    }
    Set-Location ..
    Remove-Item -r $folder
    git clone https://github.com/Liszt21/Lemacs $folder
    Set-Location $folder
}

function Check {
    if (!$ENV:LEMACS){
        if (!$(Get-Command lemacs)) {
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
        }
        $ENV:LEMACS = ($ENV:SCOOP + "/apps/lemacs/current")
        
        if ($ISME) {
            Set-Location ..
            if(Test-Path C:/Liszt/Projects/Lemacs){
                Remove-Item -r master
            }else{
                Move-Item master C:/Liszt/Projects/Lemacs
            }
            sudo ln -s C:/Liszt/Projects/Lemacs ./master
        }
        [environment]::setEnvironmentVariable('LEMACS',$ENV:LEMACS,'User')
    }
    Set-Location $LEMACS
    if (!(Test-Path .git)){
        Write-Debug "lemacs isn't a git folder"
        Update-Git
    }
}

function Help {
    Write-Output "Lemacs --help"
}

function Update {
    Write-Output "Updating"
    git pull origin master
    git submodule init
    git submodule update
}

function Install {
    Write-Output "Installing"
    sudo ln -s ($LEMACS + "/src/lemacs.el") ("C:/Users/$ENV:USERNAME/AppData/Roaming/.emacs")
    sudo ln -s ($LEMACS + "/config/spacemacs") ("C:/Users/$ENV:USERNAME/AppData/Roaming/spacemacs")
    sudo ln -s ($LEMACS + "/config/doomemacs") ("C:/Users/$ENV:USERNAME/AppData/Roaming/doomemacs")
    sudo ln -s ($LEMACS + "/config/.doom.d") ("C:/Users/$ENV:USERNAME/AppData/Roaming/.doom.d")
    sudo ln -s ($LEMACS + "/config/.spacemacs.d") ("C:/Users/$ENV:USERNAME/AppData/Roaming/.spacemacs.d")
}

function Uninstall {
    Write-Output "Uninstalling"
    Remove-Item ("C:/Users/$ENV:USERNAME/AppData/Roaming/.emacs")
    Remove-Item ("C:/Users/$ENV:USERNAME/AppData/Roaming/.doom.d")
    Remove-Item ("C:/Users/$ENV:USERNAME/AppData/Roaming/.spacemacs.d")
    Remove-Item ("C:/Users/$ENV:USERNAME/AppData/Roaming/spacemacs")
    Remove-Item ("C:/Users/$ENV:USERNAME/AppData/Roaming/doomemacs")
}

function Run($command) {
    if(!$command){
        $command = "Default"
    }
    switch -regex ($command){
        "Default" {
            Write-Output "Default"
            Start-Job {emacs}
            return
        }
        "^d" {
            Write-Output "Doomemacs"
            Start-Job {emacs --with-profile doomemacs}
        }
        "^s" {
            Write-Output "Spacemacs"
            Start-Job {emacs --with-profile spacemacs}
        }
    }
}

Check

if (!$args -or !$args[0] -eq "update"){
    Run
} else {
    switch ($args[0]){
        'doom' {
            Write-Output "Doom"
            .\config\doomemacs\bin\doom.cmd $args[1..$args.Count]
        }
        'spacemacs' {
            Write-Output "Spacemacs"
        }
        'run' {
            Run $args[1]
        }
        'install' {
            Install
        }
        'uninstall' {
            Uninstall
        }
        'update' {
            Update
        }
    }
}

Set-Location $Origin_Location