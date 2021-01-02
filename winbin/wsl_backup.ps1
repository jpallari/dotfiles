param (
    [string]$Path = $env:WSL_BACKUP_PATH,
    [string[]]$Distros = $env:WSL_BACKUP_DISTROS
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

wsl --shutdown
$timestamp = Get-Date -Format "yyyy-MM-dd-HHmmss"
foreach ($distro in $distros) {
    $distroBackupPath = Join-Path $Path $distro "$timestamp.tar"
    Write-Information "Backing up $distro to $distroBackupPath"
    wsl --export $distro $distroBackupPath
    New-Item -Type SymbolicLink -Path (Join-Path $distroBackupDir "latest.tar") -Target $distroBackupPath -Force
}

# To restore a distro from the backup:
#   wsl --unregister Ubuntu
#   wsl --import Ubuntu `
#       $env:LOCALAPPDATA\Packages\CanonicalGroupLimited.UbuntuonWindows_79rhkp1fndgsc\LocalState `
#       X:\Backup\WSL\Ubuntu\latest.tar
