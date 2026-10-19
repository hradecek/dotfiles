"""Generate the paper theme wallpaper (warm paper with an engineering grid).

Usage: python3 wallpaper.py [output]
Default: `wallpaper` next to this script. Needs Pillow (python-pillow).
"""
import os
import sys
from PIL import Image, ImageChops, ImageDraw, ImageFilter

OUT = sys.argv[1] if len(sys.argv) > 1 else os.path.join(os.path.dirname(os.path.abspath(__file__)), "wallpaper")
W, H = 3840, 2160
PAPER = (244, 241, 234)
MINOR = (232, 226, 213)
MAJOR = (219, 211, 196)
STEP, MAJOR_EVERY = 48, 5

img = Image.new("RGB", (W, H), PAPER)
d = ImageDraw.Draw(img)

# Grid centered on the screen, so the margins are equal on every side
ox, oy = (W % STEP) // 2, (H % STEP) // 2
for i, x in enumerate(range(ox, W, STEP)):
    d.line([(x, 0), (x, H)], fill=MAJOR if i % MAJOR_EVERY == 0 else MINOR, width=2)
for i, y in enumerate(range(oy, H, STEP)):
    d.line([(0, y), (W, y)], fill=MAJOR if i % MAJOR_EVERY == 0 else MINOR, width=2)

# Paper grain: sparse two-level speckle (subtle, and keeps the PNG small)
grain = Image.effect_noise((W, H), 18).filter(ImageFilter.GaussianBlur(0.8))
grain = grain.point(lambda v: 2 if v < 112 else 0)
img = ImageChops.subtract(img, Image.merge("RGB", (grain, grain, grain)))

# Faint warm vignette toward the edges
vig = Image.radial_gradient("L").resize((W, H)).point(lambda v: int(v * 0.10))
img = Image.composite(Image.new("RGB", (W, H), (205, 196, 178)), img, vig)

img.save(OUT, "PNG", optimize=True)
