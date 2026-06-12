#==============================================================================#
#                          WINDOWS STARTUP APPS                                #
#                                                                              #
# Sets up logon autostart for the desktop apps. chezmoi re-runs this when the  #
# content changes. Windows-only (gated in .chezmoiignore). Idempotent.         #
#                                                                              #
# komorebi + whkd use komorebi's own startup shortcut (shell:startup), which   #
# fires after the shell is ready — a plain HKCU Run entry launches a tiling    #
# WM too early and it bails. Flow Launcher uses an HKCU Run entry.             #
#                                                                              #
#   winget: LGUG2Z.komorebi  ·  Flow-Launcher.Flow-Launcher                    #
#                                                                              #
# @author: hradecek <ivohradek@gmail.com>                                      #
#==============================================================================#
$ErrorActionPreference = 'Stop'
$run = 'HKCU:\Software\Microsoft\Windows\CurrentVersion\Run'

# komorebi + whkd via komorebi's blessed autostart (creates komorebi.lnk in
# shell:startup that runs `komorebic start --whkd`).
$komorebic = "$env:ProgramFiles\komorebi\bin\komorebic.exe"
if (Test-Path $komorebic) {
    & $komorebic enable-autostart --whkd
    Write-Host ":: komorebi + whkd autostart enabled"
} else {
    Write-Warning "skip komorebi autostart — komorebic not found (winget install LGUG2Z.komorebi)"
}

# Flow Launcher at logon (stable stub path, not the versioned app-* path).
$flow = "$env:LOCALAPPDATA\FlowLauncher\Flow.Launcher.exe"
if (Test-Path $flow) {
    Set-ItemProperty -Path $run -Name 'FlowLauncher' -Value "`"$flow`""
    Write-Host ":: startup enabled: FlowLauncher"
} else {
    Write-Warning "skip Flow Launcher — not installed at $flow"
}

# Clean up the old GlazeWM Run entry (we switched WMs to komorebi).
Remove-ItemProperty -Path $run -Name 'GlazeWM' -ErrorAction SilentlyContinue
