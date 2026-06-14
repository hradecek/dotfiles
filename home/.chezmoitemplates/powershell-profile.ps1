# PowerShell profile — managed by chezmoi (Windows only).
# Mirrors the git aliases from shell/aliases/git.aliases.sh so the same
# muscle memory (gst, gco, gp, ...) works in PowerShell as on Linux/zsh.
#
# gc, gcm, gp, gci shadow BUILT-IN PowerShell aliases
# (Get-Content, Get-Command, Get-ItemProperty, Get-ChildItem). PowerShell
# resolves aliases before functions, so we remove those aliases to let the
# git versions win. Comment out the loop below to keep the PS built-ins.
foreach ($a in 'gc','gcm','gp','gci') {
    if (Test-Path "Alias:$a") { Remove-Item "Alias:$a" -Force }
}

# --- git ---
function g      { git @args }
function ga     { git add @args }
function gall   { git add . @args }
function gco    { git checkout @args }
function gcp    { git cherry-pick @args }
function gst    { git status @args }
function gss    { git status -s @args }
function gp     { git push @args }
function gpo    { git push origin @args }
function gc     { git commit -v @args }
function gca    { git commit -v -a @args }
function gcm    { git commit -v -m @args }
function gci    { git commit --interactive @args }
function gb     { git branch @args }
function gdel   { git branch -D @args }
function gcount { git shortlog -sn @args }
function gll    { git log --graph --pretty=oneline --abbrev-commit @args }
function gg     { git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative @args }
function ggs    { gg --stat @args }

# --- portable listing helpers (mirror shell/aliases/general.aliases.sh) ---
# (the GNU-coreutils-only flags don't port; Get-ChildItem is always "long",
#  so the -l variants collapse onto it. -Force shows hidden = ls -A.)
function l      { Get-ChildItem @args }
function ll     { Get-ChildItem @args }
function lh     { Get-ChildItem @args }
function la     { Get-ChildItem -Force @args }
function lah    { Get-ChildItem -Force @args }
function lla    { Get-ChildItem -Force @args }

# --- navigation / process search ---
function cd..   { Set-Location .. }
function psgrep { Get-Process | Where-Object { $_.ProcessName -match $args[0] } }

# --- PSReadLine: mirror the zsh keybindings + history behaviour ---
# zsh bindkey ^P/^N (history-prefix search) + ^A/^E (line start/end), and
# history.zsh (large, de-duplicated history). Guarded for the host/older 5.1.
if (Get-Module -ListAvailable PSReadLine) {
    try {
        Import-Module PSReadLine
        Set-PSReadLineKeyHandler -Key 'Ctrl+p' -Function HistorySearchBackward
        Set-PSReadLineKeyHandler -Key 'Ctrl+n' -Function HistorySearchForward
        Set-PSReadLineKeyHandler -Key 'Ctrl+a' -Function BeginningOfLine
        Set-PSReadLineKeyHandler -Key 'Ctrl+e' -Function EndOfLine
        # Ctrl+D like bash/zsh: delete char under cursor, or exit if line empty.
        Set-PSReadLineKeyHandler -Key 'Ctrl+d' -Function DeleteCharOrExit
        Set-PSReadLineOption -HistoryNoDuplicates
        Set-PSReadLineOption -MaximumHistoryCount 1000000
        Set-PSReadLineOption -HistorySearchCursorMovesToEnd
        # Inline history prediction needs PSReadLine 2.1+ (PowerShell 7).
        if ((Get-Module PSReadLine).Version -ge [version]'2.1.0') {
            Set-PSReadLineOption -PredictionSource History
        }
    } catch { }
}

# --- file-type icons + colours in listings (≈ Linux dircolors/LS_COLORS) ---
if (Get-Module -ListAvailable Terminal-Icons) { Import-Module Terminal-Icons }

# --- prompt: Starship ---
# Same engine + shared ~/.config/starship.toml as Linux zsh, so the prompt
# matches across OSes. Guarded so the profile still loads if starship is
# missing (install: winget install Starship.Starship).
if (Get-Command starship -ErrorAction SilentlyContinue) {
    Invoke-Expression (& starship init powershell)
}
