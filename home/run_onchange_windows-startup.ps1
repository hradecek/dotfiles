#==============================================================================#
#                          WINDOWS STARTUP APPS                                #
#                                                                              #
# Registers the desktop apps that should launch at logon under the per-user    #
# Run key (no admin). chezmoi re-runs this when the content changes.           #
# Windows-only (gated in .chezmoiignore). Idempotent — overwrites same values. #
#                                                                              #
# Skips any app that isn't installed yet (install via winget, then re-apply):  #
#   glzr-io.glazewm  ·  Flow-Launcher.Flow-Launcher                            #
#                                                                              #
# @author: hradecek <ivohradek@gmail.com>                                      #
#==============================================================================#
$ErrorActionPreference = 'Stop'
$run = 'HKCU:\Software\Microsoft\Windows\CurrentVersion\Run'

$apps = [ordered]@{
    'GlazeWM'      = "$env:ProgramFiles\glzr.io\GlazeWM\glazewm.exe"
    'FlowLauncher' = "$env:LOCALAPPDATA\FlowLauncher\Flow.Launcher.exe"
}

foreach ($name in $apps.Keys) {
    $path = $apps[$name]
    if (Test-Path $path) {
        # Quote the path so spaces (e.g. "Program Files") are handled.
        Set-ItemProperty -Path $run -Name $name -Value "`"$path`""
        Write-Host ":: startup enabled: $name"
    } else {
        Write-Warning "skip '$name' — not installed at $path"
    }
}
