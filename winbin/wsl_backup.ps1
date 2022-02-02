param (
    [string]$Path = $env:WSL_BACKUP_PATH,
    [string[]]$Distros = $env:WSL_BACKUP_DISTROS
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

wsl --shutdown
$Timestamp = Get-Date -Format "yyyy-MM-dd-HHmmss"
foreach ($Distro in $Distros) {
    $DistroBackupDir = Join-Path $Path $Distro
    $DistroBackupPath = Join-Path $DistroBackupDir "$Timestamp.tar"
    Write-Information "Backing up $Distro to $DistroBackupPath"
    wsl --export $Distro $DistroBackupPath
    New-Item -Type SymbolicLink -Path (Join-Path $DistroBackupDir "latest.tar") -Target $DistroBackupPath -Force
}

# To restore a distro from the backup:
#   wsl --unregister Ubuntu
#   wsl --import Ubuntu `
#       $env:LOCALAPPDATA\Packages\CanonicalGroupLimited.UbuntuonWindows_79rhkp1fndgsc\LocalState `
#       X:\Backup\WSL\Ubuntu\latest.tar
