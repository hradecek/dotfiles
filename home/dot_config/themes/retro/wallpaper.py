"""Generate the retro theme wallpaper (green phosphor CRT screen).

Usage: python3 wallpaper.py [font.otf] [output]
Defaults: the Departure Mono from packages.yaml, `wallpaper` next to this script.
Needs Pillow (python-pillow).
"""
import os
import sys
from PIL import Image, ImageDraw, ImageFont, ImageFilter, ImageChops

FONT = sys.argv[1] if len(sys.argv) > 1 else "/usr/share/fonts/OTF/DepartureMono-Regular.otf"
OUT = sys.argv[2] if len(sys.argv) > 2 else os.path.join(os.path.dirname(os.path.abspath(__file__)), "wallpaper")
W, H = 3840, 2160
BG = (3, 7, 4)
GREEN = (76, 207, 114)
MUTED = (63, 159, 92)


def font(px):
    return ImageFont.truetype(FONT, px)


def glow(layer, radius, strength=1.0):
    """Additive bloom of a layer's bright pixels."""
    blur = layer.filter(ImageFilter.GaussianBlur(radius))
    if strength != 1.0:
        blur = blur.point(lambda v: min(255, int(v * strength)))
    return ImageChops.add(layer, blur)


img = Image.new("RGB", (W, H), BG)

# Everything luminous is drawn on a black layer, bloomed, then added on top.
fg = Image.new("RGB", (W, H), (0, 0, 0))
d = ImageDraw.Draw(fg)

# Outer frame with open corners (lines stop short of each corner)
M, GAP, T = 150, 90, 3
d.line([(M + GAP, M), (W - M - GAP, M)], fill=MUTED, width=T)
d.line([(M + GAP, H - M), (W - M - GAP, H - M)], fill=MUTED, width=T)
d.line([(M, M + GAP), (M, H - M - GAP)], fill=MUTED, width=T)
d.line([(W - M, M + GAP), (W - M, H - M - GAP)], fill=MUTED, width=T)

# Session block (top-left)
x0, y0 = M + 60, M + 50
d.rectangle([x0, y0, x0 + 150, y0 + 210], outline=GREEN, width=4)
d.text((x0 + 30, y0 + 40), "TTY", font=font(44), fill=GREEN)
d.text((x0 + 52, y0 + 110), "1", font=font(66), fill=GREEN)
hx = x0 + 200
d.text((hx, y0), "SELECT SESSION:", font=font(44), fill=GREEN)
d.line([(hx, y0 + 66), (hx + 900, y0 + 66)], fill=GREEN, width=3)
d.text((hx, y0 + 88), "[RIVER],SWAY,HYPRLAND", font=font(44), fill=GREEN)

img = ImageChops.add(img, glow(fg, 10, 0.9))

# Big title with chromatic aberration: red/blue copies offset sideways
title = "ARCH LINUX"
tf = font(330)
tw = ImageDraw.Draw(img).textlength(title, font=tf)
tx, ty = (W - tw) // 2, H // 2 - 200


def title_layer(color, dx):
    layer = Image.new("RGB", (W, H), (0, 0, 0))
    ImageDraw.Draw(layer).text((tx + dx, ty), title, font=tf, fill=color)
    return layer


ab = ImageChops.add(title_layer((255, 30, 60), -14), title_layer((30, 90, 255), 14))
img = ImageChops.add(img, ab.point(lambda v: int(v * 0.8)))
core = glow(title_layer((215, 255, 235), 0), 18, 0.7)
img = ImageChops.lighter(img, core)

# Glitch: shift a few horizontal slices of the title
for y, h, dx in ((ty + 70, 22, 38), (ty + 190, 14, -30), (ty + 260, 8, 22)):
    band = img.crop((int(tx) - 60, y, int(tx + tw) + 60, y + h))
    img.paste(band, (int(tx) - 60 + dx, y))

# Scanlines over everything
sl = Image.new("L", (W, H), 0)
sd = ImageDraw.Draw(sl)
for y in range(0, H, 4):
    sd.line([(0, y), (W, y)], fill=70, width=2)
img = Image.composite(Image.new("RGB", (W, H), BG), img, sl.point(lambda v: v))

# Vignette: darken toward the edges
vig = Image.radial_gradient("L").resize((W, H)).point(lambda v: int(v * 0.55))
img = Image.composite(Image.new("RGB", (W, H), (0, 0, 0)), img, vig)

img.save(OUT, "PNG", optimize=True)
