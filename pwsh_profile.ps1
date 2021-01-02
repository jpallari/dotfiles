if ($host.Name -eq 'ConsoleHost') {
    Import-Module PSReadLine
    Set-PSReadLineOption -EditMode Emacs
    Set-PSReadLineKeyHandler -Key Ctrl+q -Function TabCompleteNext
    Set-PSReadLineKeyHandler -Key Ctrl+Q -Function TabCompletePrevious
}

function global:prompt {
    $Success = $?
    $TimeStamp = Get-Date -Format "HH:mm:ss"
    $PwdBase = Split-Path -Path $pwd
    $PwdCurDir = Split-Path -Path $pwd -Leaf
    Write-Host -Object "$TimeStamp " -NoNewLine -ForegroundColor Magenta
    Write-Host -Object "$PwdBase\" -NoNewLine -ForegroundColor Cyan
    Write-Host -Object "$PwdCurDir" -NoNewLine
    if (!$Success) {
        Write-Host -Object " !" -ForegroundColor Red -NoNewLine
    }
    Write-Host -Object ""
    return "> "
}
