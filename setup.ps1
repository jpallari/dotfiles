# Requires PowerShell 7+

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

function Initialize-BaseDirectory {
    param(
        [string]$TargetPath
    )
    $targetDir = Split-Path $TargetPath

    if (Test-Path $targetDir) {
        return
    }

    New-Item -ItemType Directory $targetDir
}

function Update-ConfigLink {
    param (
        [string]$TargetSource,
        [string]$TargetPath
    )
    $targetSourcePath = (Resolve-Path $TargetSource).Path

    if (Test-Path $TargetPath) {
        $targetItem = Get-Item $TargetPath
        if ($targetItem.Target -eq $targetSourcePath) {
            Write-Information "Already linked: $TargetPath => $TargetSource"
            return
        }

        if ($targetItem.Exists) {
            if ($targetItem.LinkType -eq "SymbolicLink") {
                Remove-Item $TargetPath
            } else {
                Rename-Item -Path $TargetPath -NewName "$TargetPath.bak"
            }
        }
    } else {
        # Create the target config directory if it doesn't exist
        Initialize-BaseDirectory $TargetPath
    }

    Write-Information "Linking: $TargetPath => $TargetSource"
    New-Item -ItemType SymbolicLink -Path $TargetPath -Target $targetSourcePath
}

function Update-Gitconfig {
    $gitConfigSourcePath = (Resolve-Path "gitconfig").Path
    $currentGitIncludePaths = git config --global --get-all include.path

    if ($null -ne $currentGitIncludePaths -and $currentGitIncludePaths.Contains($gitConfigSourcePath)) {
        Write-Information "Already linked: gitconfig"
        return
    }

    Write-Information "Linking: gitconfig"
    git config --global --add include.path $gitConfigSourcePath
    return
}

if ($env:OS -ne "Windows_NT") {
    Write-Error "This script only works in Windows" -ErrorAction Stop
}

# Links to dotfiles
Update-ConfigLink `
    -TargetSource ".\powershell\Microsoft.PowerShell_profile.ps1" `
    -TargetPath (Join-Path $env:USERPROFILE "\Documents\PowerShell\Microsoft.PowerShell_profile.ps1")
Update-ConfigLink `
    -TargetSource "nvim" `
    -TargetPath (Join-Path $env:LOCALAPPDATA "\nvim")
Update-ConfigLink `
    -TargetSource "windows-terminal.json" `
    -TargetPath (Join-Path $env:LOCALAPPDATA "\Packages\Microsoft.WindowsTerminal_8wekyb3d8bbwe\LocalState\settings.json")
    Update-ConfigLink `
    -TargetSource "windows-terminal.json" `
    -TargetPath (Join-Path $env:LOCALAPPDATA "\Packages\Microsoft.WindowsTerminalPreview_8wekyb3d8bbwe\LocalState\settings.json")

# Links to Gitconfig
Update-GitConfig

