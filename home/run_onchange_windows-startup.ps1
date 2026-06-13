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

# Clean up Run entries from earlier setups. The launcher is now the native
# Windows Start menu (tap the Win key) — no third-party launcher — and komorebi
# autostarts via its shell:startup shortcut above.
Remove-ItemProperty -Path $run -Name 'GlazeWM'      -ErrorAction SilentlyContinue
Remove-ItemProperty -Path $run -Name 'FlowLauncher' -ErrorAction SilentlyContinue
Remove-ItemProperty -Path $run -Name 'Flow.Launcher' -ErrorAction SilentlyContinue
