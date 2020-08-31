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
        [environment]::setEnvironmentVariable('LEMACS',$ENV:LEMACS,'User')
        Set-Location $ENV:LEMACS
        if (!(Test-Path .git)){
            Set-Location ..
            Remove-Item -r master
            git clone https://github.com/Liszt21/Lemacs master
            Set-Location master
        }
        git submodule init
        git submodule update
        sudo ln -s ($ENV:LEMACS + "/src/lemacs.el") ("C:/Users/$ENV:USERNAME/AppData/Roaming/.emacs")
        if ($ISME) {
            sudo ln -s $ENV:LEMACS C:/Liszt/Projects/Lemacs
        }
    }
    Write-Debug "Lemacs is already installed"
    Set-Location $ENV:LEMACS
}

function Help {
    Write-Output "Lemacs --help"
}

function Update {
    Write-Output "Update"
    git fetch origin master
    git submodule foreach git fetch 
    git submodule update
}

Check

if (!$args -or !$args[0] -eq "update"){
    try {
        Update
    }catch {
        Write-Output "Error occured while updating"
    }
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
            if (!$args[1]){
                Write-Output "None specific command was given"
            }else{
                Write-Output $args[1..$args.Count]
            }
        }
        'clean' {
            Write-Output "Clean"
        }
    }
}

Set-Location $Origin_Location