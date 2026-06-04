# Set XDG_CONFIG_HOME on Windows so Neovim and Alacritty read from
# %USERPROFILE%\.config (matching the Linux layout). Run once by chezmoi.
# Relog after this for the variable to take effect in new shells.
$target = Join-Path $env:USERPROFILE ".config"
$current = [Environment]::GetEnvironmentVariable("XDG_CONFIG_HOME", "User")
if ($current -ne $target) {
    [Environment]::SetEnvironmentVariable("XDG_CONFIG_HOME", $target, "User")
    Write-Host "Set XDG_CONFIG_HOME=$target (User). Restart your shell/session."
} else {
    Write-Host "XDG_CONFIG_HOME already set to $target."
}
