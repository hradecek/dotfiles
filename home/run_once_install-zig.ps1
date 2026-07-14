# Install the Zig toolchain on Windows and put it on the User PATH. Zig is the
# C compiler nvim-treesitter uses to build parsers; without it, opening Neovim
# (e.g. as git's commit editor) spams "No C compiler found!". Run once by chezmoi.
#
# We download the official portable zip directly instead of using winget: the
# winget portable-package database on this machine is corrupt ("no such table:
# metadata"), so winget extracts the archive but never records it or creates a
# PATH shim. A direct download is also more reproducible on a fresh machine.
#
# Relog (or restart the shell) after this for zig to appear on PATH.
$ErrorActionPreference = "Stop"

if (Get-Command zig -ErrorAction SilentlyContinue) {
    Write-Host "zig already on PATH; skipping."
    return
}

$version = "0.16.0"
$url     = "https://ziglang.org/download/$version/zig-x86_64-windows-$version.zip"
$sha256  = "68659eb5f1e4eb1437a722f1dd889c5a322c9954607f5edcf337bc3684a75a7e"

$dest = Join-Path $env:LOCALAPPDATA "Programs\zig"
$zip  = Join-Path $env:TEMP "zig-$version.zip"

Write-Host "Downloading zig $version ..."
Invoke-WebRequest -Uri $url -OutFile $zip

$actual = (Get-FileHash $zip -Algorithm SHA256).Hash.ToLower()
if ($actual -ne $sha256) {
    Remove-Item $zip -Force
    throw "zig checksum mismatch: expected $sha256, got $actual"
}

Write-Host "Extracting to $dest ..."
if (Test-Path $dest) { Remove-Item $dest -Recurse -Force }
$tmp = Join-Path $env:TEMP "zig-extract-$version"
if (Test-Path $tmp) { Remove-Item $tmp -Recurse -Force }
Expand-Archive -Path $zip -DestinationPath $tmp -Force
# The archive contains a single versioned top-level folder; flatten it.
$inner = Get-ChildItem $tmp -Directory | Select-Object -First 1
New-Item -ItemType Directory -Path (Split-Path $dest) -Force | Out-Null
Move-Item $inner.FullName $dest
Remove-Item $tmp -Recurse -Force
Remove-Item $zip -Force

& (Join-Path $dest "zig.exe") version | ForEach-Object { Write-Host "Installed zig $_" }

$userPath = [Environment]::GetEnvironmentVariable("Path", "User")
if ($userPath -notlike "*$dest*") {
    $newPath = if ($userPath) { "$userPath;$dest" } else { $dest }
    [Environment]::SetEnvironmentVariable("Path", $newPath, "User")
    Write-Host "Added $dest to User PATH. Restart your shell/session."
} else {
    Write-Host "$dest already on User PATH."
}
