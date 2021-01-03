if ($host.Name -ne 'ConsoleHost') {
    return
}

# Load readline
Import-Module PSReadLine
Set-PSReadLineOption -EditMode Emacs
Set-PSReadLineKeyHandler -Key Ctrl+q -Function TabCompleteNext
Set-PSReadLineKeyHandler -Key Ctrl+Q -Function TabCompletePrevious
function global:prompt {
    $success = $?
    $timestamp = Get-Date -Format "HH:mm:ss"

    if (! $env:NO_LONG_PROMPT) {
        $basedir = $pwd.Path.Replace($HOME, "~")
        $dirseparator = "\"
        $topdir = ""
        
        if ($basedir -ne "~") {
            $basedir = Split-Path -Path $basedir
            $topdir = Split-Path -Path $pwd -Leaf
        }
        if ($topdir -eq "" -or $basedir -eq "" -or $basedir.EndsWith("\")) {
            $dirseparator = ""
        }
    
        Write-Host -Object ""
        Write-Host -Object "$timestamp " -NoNewLine -ForegroundColor DarkMagenta
        Write-Host -Object "${basedir}${dirseparator}" -NoNewLine -ForegroundColor DarkCyan
        Write-Host -Object "$topdir" -NoNewLine -ForegroundColor DarkYellow
        if (!$success) {
            Write-Host -Object " !" -ForegroundColor Red -NoNewLine
        }
        Write-Host -Object ""    
    }
    return "> "
}

# Load machine local settings
$localProfilePath = Join-Path $env:USERPROFILE ".local.ps1"
if (Test-Path $localProfilePath) {
    . $localProfilePath
}
