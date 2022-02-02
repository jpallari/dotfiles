if ($host.Name -ne 'ConsoleHost') {
    return
}

# Load readline
Import-Module PSReadLine
Set-PSReadLineOption -EditMode Emacs
Set-PSReadLineKeyHandler -Key Ctrl+q -Function TabCompleteNext
Set-PSReadLineKeyHandler -Key Ctrl+Q -Function TabCompletePrevious
Set-PSReadLineOption -BellStyle None
Set-PSReadlineOption -PredictionSource History
Set-PSReadlineOption -PredictionViewStyle ListView

# Unicode input and output
[Console]::OutputEncoding = [System.Text.Encoding]::UTF8
[Console]::InputEncoding = [System.Text.Encoding]::UTF8

# prompt
function global:prompt {
    $success = $?
    $timestamp = Get-Date -Format "HH:mm:ss"

    if (! $env:NO_LONG_PROMPT) {
        $basedir = $pwd.Path.Replace($HOME, "~")
        $dirseparator = [IO.Path]::DirectorySeparatorChar
        $topdir = ""

        if ($basedir -ne "~") {
            $basedir = Split-Path -Path $basedir
            $topdir = Split-Path -Path $pwd -Leaf
        }
        if ($topdir -eq "" -or $basedir -eq "" -or $basedir.EndsWith($dirseparator)) {
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
    return "PS> "
}

# Load machine local settings
$private:localProfileDir = if ($env:USERPROFILE) {
    $env:USERPROFILE
} else {
    $env:HOME
}
$private:localProfilePath = Join-Path $localProfileDir ".local.ps1"
if (Test-Path $localProfilePath) {
    . $private:localProfilePath
}
