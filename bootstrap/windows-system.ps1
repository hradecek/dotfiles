#==============================================================================#
#                          WINDOWS SYSTEM BOOTSTRAP                            #
#                                                                              #
# One-time, ADMIN-level setup that chezmoi does not manage (it writes HKLM and #
# is not meant to run elevated). The per-user Windows tweaks live in           #
# home/run_onchange_windows-tweaks.ps1 (HKCU, no admin) and run via chezmoi.   #
#                                                                              #
# Usage (elevated PowerShell):                                                 #
#   Start-Process pwsh -Verb RunAs -ArgumentList '-File C:\projects\dotfiles\bootstrap\windows-system.ps1'
#   ...then REBOOT (the scancode map is read at boot).                         #
#==============================================================================#
$ErrorActionPreference = 'Stop'

# Require elevation (HKLM write).
$admin = ([Security.Principal.WindowsPrincipal] `
    [Security.Principal.WindowsIdentity]::GetCurrent()
).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
if (-not $admin) {
    Write-Error 'Run this in an ELEVATED PowerShell (Run as Administrator).'
    exit 1
}

# Caps Lock -> Esc (nvim-friendly; mirrors the spirit of the keyd remap on Linux).
# Scancode Map layout: 8-byte header, count (mappings + null terminator) = 2,
# then map [Esc 01 00] <- [CapsLock 3A 00], then the null terminator.
$map = [byte[]](
    0x00,0x00,0x00,0x00,  0x00,0x00,0x00,0x00,
    0x02,0x00,0x00,0x00,
    0x01,0x00, 0x3A,0x00,
    0x00,0x00,0x00,0x00
)
$path = 'HKLM:\SYSTEM\CurrentControlSet\Control\Keyboard Layout'
Set-ItemProperty -Path $path -Name 'Scancode Map' -Value $map -Type Binary
Write-Host ':: Caps Lock -> Esc scancode map written. REBOOT to apply.'
Write-Host "   (To revert: Remove-ItemProperty -Path '$path' -Name 'Scancode Map'; reboot.)"
