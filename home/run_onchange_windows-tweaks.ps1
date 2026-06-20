# Windows tweaks — managed by chezmoi (Windows only; run_once_*/run_onchange_*
# .ps1 are ignored on other OSes). Idempotent HKCU registry tweaks for a
# concise, dev-friendly, tiling-WM (komorebi) desktop that mirrors the Linux/
# River side. run_onchange = re-applied whenever this file's content changes.
#
# All values are HKCU (no admin). Some may be locked by org policy on an
# AzureAD-joined machine — those are skipped with a notice, not fatal.
$ErrorActionPreference = 'Continue'

function Set-Reg($Path, $Name, $Value, $Type = 'DWord') {
    if (-not (Test-Path $Path)) { New-Item -Path $Path -Force | Out-Null }
    try {
        Set-ItemProperty -Path $Path -Name $Name -Value $Value -Type $Type -ErrorAction Stop
        Write-Host "  ok   $Name = $Value"
    } catch {
        Write-Host "  skip $Name ($($_.Exception.Message))"
    }
}

$adv    = 'HKCU:\Software\Microsoft\Windows\CurrentVersion\Explorer\Advanced'
$srch   = 'HKCU:\Software\Microsoft\Windows\CurrentVersion\Search'
$pers   = 'HKCU:\Software\Microsoft\Windows\CurrentVersion\Themes\Personalize'
$dwm    = 'HKCU:\Software\Microsoft\Windows\DWM'
$desk   = 'HKCU:\Control Panel\Desktop'
$sticky = 'HKCU:\Control Panel\Accessibility\StickyKeys'
$pol    = 'HKCU:\Software\Policies\Microsoft\Windows\Explorer'

Write-Host 'Explorer (dev-sane):'
Set-Reg $adv 'HideFileExt'        0     # show file extensions
Set-Reg $adv 'Hidden'            1     # show hidden files (not protected OS files)
Set-Reg $adv 'LaunchTo'          1     # open Explorer to "This PC"
Set-Reg $adv 'TaskbarAl'         1     # center Start / taskbar (0 = left-align)

Write-Host 'Taskbar (concise):'
Set-Reg $adv  'ShowTaskViewButton' 0
Set-Reg $adv  'TaskbarDa'          0   # widgets
Set-Reg $adv  'TaskbarMn'          0   # chat / copilot
Set-Reg $srch 'SearchboxTaskbarMode' 0 # hide search box
try {
    $sr = 'HKCU:\Software\Microsoft\Windows\CurrentVersion\Explorer\StuckRects3'
    $s = (Get-ItemProperty $sr).Settings; $s[8] = 3   # 3 = auto-hide on
    Set-ItemProperty $sr -Name Settings -Value $s
    Write-Host '  ok   auto-hide'
} catch { Write-Host "  skip auto-hide ($($_.Exception.Message))" }

Write-Host 'Declutter / privacy:'
Set-Reg $adv 'Start_IrisRecommendations' 0   # no Start "recommended" section
Set-Reg $adv 'EnableSnapAssistFlyout'    0   # no snap-layouts hover (komorebi tiles)
Set-Reg $pol 'DisableSearchBoxSuggestions' 1 # no web/Bing results in Start search

Write-Host 'Dark mode + green accent (match colors2):'
Set-Reg $pers 'AppsUseLightTheme'   0
Set-Reg $pers 'SystemUsesLightTheme' 0
Set-Reg $dwm  'ColorPrevalence'     1          # accent on title bars + taskbar
# Accent ~ colors2 green #56c92b. AccentColor is 0xAABBGGRR; Colorization 0xAARRGGBB.
Set-Reg $dwm  'AccentColor'          ([int]0xFF2BC956)
Set-Reg $dwm  'ColorizationColor'    ([int]0xC456C92B)
Set-Reg $dwm  'ColorizationAfterglow' ([int]0xC456C92B)

Write-Host 'Input:'
Set-Reg $desk   'KeyboardDelay' '0'  'String'   # shortest repeat delay (~250ms)
Set-Reg $desk   'KeyboardSpeed' '31' 'String'   # fastest repeat rate (≈ River set-repeat)
Set-Reg $sticky 'Flags'         '506' 'String'  # disable the Shift x5 Sticky Keys prompt

# Restart Explorer so taskbar/Explorer/theme changes take effect now.
Stop-Process -Name explorer -Force -ErrorAction SilentlyContinue
Write-Host 'Done. (Key-repeat + accent fully apply after a sign-out/in.)'
