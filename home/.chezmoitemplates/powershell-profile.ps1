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

# --- portable listing helpers ---
# (general.aliases.sh is mostly GNU-coreutils-specific and does not port;
#  these are the ones with a clean PowerShell equivalent.)
function ll     { Get-ChildItem @args }
function la     { Get-ChildItem -Force @args }

# --- prompt: Starship ---
# Same engine + shared ~/.config/starship.toml as Linux zsh, so the prompt
# matches across OSes. Guarded so the profile still loads if starship is
# missing (install: winget install Starship.Starship).
if (Get-Command starship -ErrorAction SilentlyContinue) {
    Invoke-Expression (& starship init powershell)
}
