#!/usr/bin/env python3
"""Generate a layered, Affinity/Canva-editable SVG of the trait workflow figure.

Groups are named for easy selection in Affinity Designer:
  - layer_background
  - layer_column1 / layer_column2 / layer_column3
  - layer_connectors
  - layer_footer
  - layer_text (section titles live with their columns)

Output:
  trait_workflow_editable.svg
"""

from __future__ import annotations

import math
from pathlib import Path

W, H = 1600, 1140
OUT = Path(__file__).resolve().parent

# Trait colors (consistent across figure)
LEAF_N = "#2E8B57"
LMA = "#E67E22"
WATER = "#3498DB"
PIGMENTS = "#E74C3C"
SP_E = "#8E44AD"

FONT = "Helvetica Neue, Helvetica, Arial, sans-serif"


def esc(s: str) -> str:
    return (
        s.replace("&", "&amp;")
        .replace("<", "&lt;")
        .replace(">", "&gt;")
        .replace('"', "&quot;")
    )


def text(
    x: float,
    y: float,
    content: str,
    *,
    size: float = 12,
    weight: str = "normal",
    anchor: str = "start",
    fill: str = "#1a1a1a",
    style: str = "normal",
    family: str = FONT,
) -> str:
    return (
        f'<text x="{x:.1f}" y="{y:.1f}" font-family="{family}" font-size="{size}" '
        f'font-weight="{weight}" font-style="{style}" text-anchor="{anchor}" '
        f'fill="{fill}">{esc(content)}</text>'
    )


def multiline(
    x: float,
    y: float,
    lines: list[str],
    *,
    size: float = 11,
    leading: float | None = None,
    weight: str = "normal",
    anchor: str = "start",
    fill: str = "#1a1a1a",
    style: str = "normal",
) -> str:
    leading = leading or size * 1.25
    parts = []
    for i, line in enumerate(lines):
        parts.append(
            text(
                x,
                y + i * leading,
                line,
                size=size,
                weight=weight,
                anchor=anchor,
                fill=fill,
                style=style,
            )
        )
    return "\n".join(parts)


def arrow_down(x: float, y0: float, y1: float, *, stroke: str = "#222", sw: float = 1.8) -> str:
    return (
        f'<line x1="{x}" y1="{y0}" x2="{x}" y2="{y1 - 6}" stroke="{stroke}" '
        f'stroke-width="{sw}" />'
        f'<polygon points="{x-5},{y1-8} {x+5},{y1-8} {x},{y1}" fill="{stroke}" />'
    )


def arrow_right(x0: float, y: float, x1: float, *, stroke: str = "#111", sw: float = 3.5) -> str:
    return (
        f'<line x1="{x0}" y1="{y}" x2="{x1 - 10}" y2="{y}" stroke="{stroke}" '
        f'stroke-width="{sw}" />'
        f'<polygon points="{x1-14},{y-8} {x1-14},{y+8} {x1},{y}" fill="{stroke}" />'
    )


def pixel_grid(
    x: float,
    y: float,
    colors: list[list[str]],
    *,
    cell: float = 14,
    gap: float = 1.5,
    stroke: str = "#555",
) -> str:
    parts = [f'<g id="pixel-grid" transform="translate({x},{y})">']
    for r, row in enumerate(colors):
        for c, col in enumerate(row):
            parts.append(
                f'<rect x="{c * (cell + gap):.1f}" y="{r * (cell + gap):.1f}" '
                f'width="{cell}" height="{cell}" fill="{col}" stroke="{stroke}" '
                f'stroke-width="0.6" />'
            )
    parts.append("</g>")
    return "\n".join(parts)


def trait_dot(cx: float, cy: float, color: str, r: float = 7) -> str:
    return f'<circle cx="{cx}" cy="{cy}" r="{r}" fill="{color}" stroke="#333" stroke-width="0.8" />'


def bell_path(
    x0: float,
    y_base: float,
    width: float,
    height: float,
    *,
    mean: float = 0.5,
    sd: float = 0.14,
    skew: float = 0.0,
    n: int = 60,
) -> str:
    """Approximate skewed normal as SVG path coordinates (absolute)."""
    pts = []
    for i in range(n + 1):
        t = i / n
        # skewed gaussian-ish
        z = (t - mean) / sd
        y = math.exp(-0.5 * z * z) * (1 + math.tanh(skew * z))
        # normalize later
        pts.append((t, y))
    ymax = max(p[1] for p in pts) or 1.0
    coords = []
    for t, y in pts:
        px = x0 + t * width
        py = y_base - (y / ymax) * height
        coords.append(f"{px:.2f},{py:.2f}")
    return "M " + " L ".join(coords)


def clipboard_icon(x: float, y: float, s: float = 1.0) -> str:
    return f'''<g id="clipboard-icon" transform="translate({x},{y}) scale({s})">
  <rect x="4" y="8" width="28" height="36" rx="3" fill="#f7f7f7" stroke="#444" stroke-width="1.5"/>
  <rect x="10" y="2" width="16" height="10" rx="2" fill="#ddd" stroke="#444" stroke-width="1.2"/>
  <line x1="10" y1="20" x2="26" y2="20" stroke="#2E8B57" stroke-width="2"/>
  <line x1="10" y1="26" x2="26" y2="26" stroke="#2E8B57" stroke-width="2"/>
  <line x1="10" y1="32" x2="22" y2="32" stroke="#2E8B57" stroke-width="2"/>
  <polyline points="10,38 13,41 20,34" fill="none" stroke="#2E8B57" stroke-width="2"/>
</g>'''


def plant_icon(x: float, y: float, s: float = 1.0, color: str = "#2E8B57") -> str:
    return f'''<g id="plant-icon" transform="translate({x},{y}) scale({s})">
  <path d="M12 40 C12 28, 4 22, 4 12 C10 14, 14 22, 14 40 Z" fill="{color}" opacity="0.85"/>
  <path d="M16 40 C16 26, 28 18, 30 8 C22 12, 18 24, 18 40 Z" fill="{color}"/>
  <line x1="14" y1="40" x2="14" y2="48" stroke="#5D4037" stroke-width="2"/>
</g>'''


def die_icon(x: float, y: float, s: float = 1.0) -> str:
    return f'''<g id="die-icon" transform="translate({x},{y}) scale({s})">
  <rect x="0" y="0" width="28" height="28" rx="4" fill="#fff" stroke="#333" stroke-width="1.5"/>
  <circle cx="8" cy="8" r="2.2" fill="#333"/>
  <circle cx="20" cy="8" r="2.2" fill="#333"/>
  <circle cx="14" cy="14" r="2.2" fill="#333"/>
  <circle cx="8" cy="20" r="2.2" fill="#333"/>
  <circle cx="20" cy="20" r="2.2" fill="#333"/>
</g>'''


def protea_icon(x: float, y: float, s: float = 1.0) -> str:
    return f'''<g id="protea-icon" transform="translate({x},{y}) scale({s})">
  <ellipse cx="16" cy="22" rx="10" ry="12" fill="#C2185B"/>
  <path d="M16 6 L18 20 L16 18 L14 20 Z" fill="#F8BBD0"/>
  <path d="M8 10 L16 20 L14 18 Z" fill="#F48FB1"/>
  <path d="M24 10 L16 20 L18 18 Z" fill="#F48FB1"/>
  <path d="M6 18 L16 22 L14 20 Z" fill="#EC407A"/>
  <path d="M26 18 L16 22 L18 20 Z" fill="#EC407A"/>
  <line x1="16" y1="32" x2="16" y2="42" stroke="#5D4037" stroke-width="2"/>
  <path d="M16 36 C10 34, 8 38, 10 40" fill="none" stroke="#2E7D32" stroke-width="1.5"/>
</g>'''


def shrub_icon(x: float, y: float, s: float = 1.0) -> str:
    return f'''<g id="shrub-icon" transform="translate({x},{y}) scale({s})">
  <ellipse cx="16" cy="24" rx="14" ry="10" fill="#558B2F"/>
  <circle cx="8" cy="18" r="5" fill="#F9A825"/>
  <circle cx="16" cy="14" r="5" fill="#FBC02D"/>
  <circle cx="24" cy="18" r="5" fill="#F9A825"/>
  <line x1="16" y1="30" x2="16" y2="42" stroke="#5D4037" stroke-width="2"/>
</g>'''


def grass_icon(x: float, y: float, s: float = 1.0) -> str:
    return f'''<g id="grass-icon" transform="translate({x},{y}) scale({s})">
  <path d="M8 42 C8 24, 2 12, 6 4" fill="none" stroke="#8D6E63" stroke-width="2"/>
  <path d="M14 42 C14 22, 12 10, 16 2" fill="none" stroke="#A1887F" stroke-width="2.2"/>
  <path d="M20 42 C20 24, 24 12, 22 4" fill="none" stroke="#8D6E63" stroke-width="2"/>
  <path d="M26 42 C26 26, 30 14, 28 8" fill="none" stroke="#A1887F" stroke-width="1.8"/>
</g>'''


def dispersal_icon(x: float, y: float, s: float = 1.0) -> str:
    return f'''<g id="dispersal-icon" transform="translate({x},{y}) scale({s})">
  <circle cx="10" cy="16" r="6" fill="none" stroke="#555" stroke-width="1.4"/>
  <circle cx="28" cy="8" r="5" fill="none" stroke="#555" stroke-width="1.4"/>
  <circle cx="30" cy="26" r="5" fill="none" stroke="#555" stroke-width="1.4"/>
  <circle cx="10" cy="16" r="2" fill="#3498DB"/>
  <path d="M14 12 L22 10" stroke="#333" stroke-width="1.3" marker-end="url(#arrowhead-sm)"/>
  <path d="M15 19 L24 24" stroke="#333" stroke-width="1.3"/>
  <polygon points="22,8 26,10 22,12" fill="#333"/>
  <polygon points="24,22 28,26 22,26" fill="#333"/>
</g>'''


def column1(x: float, y: float) -> str:
    # Column width ~430
    parts = [f'<g id="layer_column1" transform="translate({x},{y})">']
    parts.append(text(0, 0, "1. Hindcast Functional Plant Traits", size=18, weight="700"))
    parts.append(
        text(
            0,
            20,
            "with multispectral imagery and recent imaging spectroscopy",
            size=11,
            style="italic",
            fill="#444",
        )
    )

    # Top inputs
    ms = [
        ["#6B8E4E", "#8FBC6B", "#A67C52"],
        ["#7A9E5A", "#C4A574", "#6B8E4E"],
        ["#A67C52", "#6B8E4E", "#8FBC6B"],
    ]
    sp = [
        ["#3D8B40", "#C6D94E", "#7CB342"],
        ["#F0C040", "#66BB6A", "#AED581"],
        ["#8BC34A", "#D4E157", "#43A047"],
    ]

    parts.append('<g id="c1_inputs">')
    parts.append(pixel_grid(10, 45, ms, cell=12, gap=1.2))
    parts.append(multiline(10, 100, ["Multispectral imagery", "(Landsat, Sentinel)"], size=10, fill="#333"))

    parts.append(pixel_grid(150, 45, sp, cell=12, gap=1.2))
    parts.append(
        multiline(150, 100, ["Imaging spectroscopy", "(BioSCape, SHIFT, AVIRIS)"], size=10, fill="#333")
    )

    parts.append(plant_icon(300, 40, 0.85))
    parts.append(clipboard_icon(340, 42, 0.9))
    parts.append(multiline(300, 100, ["Field data", "(plots, traits)"], size=10, fill="#333"))
    parts.append("</g>")

    # Arrows down to annual maps
    parts.append('<g id="c1_flow_arrows">')
    parts.append(arrow_down(40, 125, 165))
    parts.append(arrow_down(180, 125, 165))
    parts.append(arrow_down(340, 125, 165))
    parts.append("</g>")

    # Annual maps
    parts.append('<g id="c1_annual_maps">')
    parts.append(
        text(
            215,
            160,
            "Annual maps of community mean traits (1984 – present – 2050)",
            size=11,
            weight="600",
            anchor="middle",
        )
    )

    map_colors_1984 = [
        ["#7A9E5A", "#A67C52", "#6B8E4E"],
        ["#8FBC6B", "#6B8E4E", "#C4A574"],
        ["#A67C52", "#7A9E5A", "#8FBC6B"],
    ]
    map_colors_present = [
        ["#66BB6A", "#F0C040", "#8BC34A"],
        ["#AED581", "#43A047", "#C6D94E"],
        ["#7CB342", "#D4E157", "#66BB6A"],
    ]
    map_colors_2050 = [
        ["#FFCC80", "#EF9A9A", "#FFE082"],
        ["#A5D6A7", "#FFCC80", "#EF9A9A"],
        ["#FFE082", "#A5D6A7", "#FFCC80"],
    ]

    parts.append(pixel_grid(40, 180, map_colors_1984, cell=13, gap=1.3))
    parts.append(text(60, 235, "1984", size=11, weight="600", anchor="middle"))

    parts.append(text(130, 205, "...", size=18, anchor="middle", fill="#666"))

    parts.append(pixel_grid(160, 180, map_colors_present, cell=13, gap=1.3))
    parts.append(text(185, 235, "Present", size=11, weight="600", anchor="middle"))

    parts.append(text(250, 205, "...", size=18, anchor="middle", fill="#666"))

    parts.append(pixel_grid(280, 180, map_colors_2050, cell=13, gap=1.3))
    parts.append(text(305, 235, "2050 (forecast)", size=11, weight="600", anchor="middle"))
    parts.append("</g>")

    # Trait legend
    parts.append('<g id="c1_trait_legend">')
    parts.append(
        text(215, 270, "Community mean traits (per pixel, per year)", size=11, weight="600", anchor="middle")
    )
    traits = [
        (LEAF_N, "Leaf N"),
        (LMA, "LMA"),
        (WATER, "Water content"),
        (PIGMENTS, "Pigments"),
    ]
    tx = 55
    for color, label in traits:
        parts.append(trait_dot(tx, 295, color, 8))
        parts.append(text(tx + 14, 299, label, size=11))
        tx += 85
    parts.append(text(tx + 5, 299, "...", size=14, fill="#666"))
    parts.append("</g>")

    parts.append("</g>")
    return "\n".join(parts)


def column2(x: float, y: float) -> str:
    parts = [f'<g id="layer_column2" transform="translate({x},{y})">']
    parts.append(text(0, 0, "2. Infer Species Mixtures & Trait Distributions", size=17, weight="700"))

    # Observed trait vector + species matrix
    parts.append('<g id="c2_inputs">')
    parts.append(text(0, 35, "Observed trait vector", size=10, weight="600"))
    parts.append(text(0, 48, "(at each pixel)", size=9, fill="#555"))

    vec = [(LEAF_N, "Leaf N"), (LMA, "LMA"), (WATER, "Water content"), (PIGMENTS, "Pigments")]
    for i, (color, label) in enumerate(vec):
        cy = 70 + i * 22
        parts.append(trait_dot(12, cy, color, 7))
        parts.append(text(26, cy + 4, label, size=10))

    parts.append(text(145, 100, "+", size=28, weight="700", anchor="middle"))

    # Species trait matrix
    parts.append('<g id="c2_species_matrix" transform="translate(175,40)">')
    parts.append(text(0, -8, "Species trait matrix", size=10, weight="600"))
    parts.append(text(0, 5, "(species × traits)", size=9, fill="#555"))

    headers = ["", "Leaf N", "LMA", "Water", "Pigments"]
    rows = ["Sp A", "Sp B", "Sp C", "Sp D", "Sp E"]
    # illustrative placeholder values (edit freely in Affinity)
    values = [
        ["2.1", "120", "0.62", "0.41"],
        ["1.4", "180", "0.48", "0.55"],
        ["2.8", "95", "0.71", "0.33"],
        ["1.9", "150", "0.55", "0.60"],
        ["3.2", "110", "0.66", "0.28"],
    ]
    cw, rh = 52, 16
    # header
    for c, h in enumerate(headers):
        parts.append(
            f'<rect x="{c * cw}" y="12" width="{cw}" height="{rh}" fill="#f0f0f0" '
            f'stroke="#888" stroke-width="0.7"/>'
        )
        if h:
            parts.append(text(c * cw + cw / 2, 24, h, size=8, anchor="middle", weight="600"))
    for r, sp in enumerate(rows):
        for c in range(5):
            parts.append(
                f'<rect x="{c * cw}" y="{12 + (r + 1) * rh}" width="{cw}" height="{rh}" '
                f'fill="#fff" stroke="#888" stroke-width="0.7"/>'
            )
        parts.append(text(cw / 2, 12 + (r + 1) * rh + 12, sp, size=9, anchor="middle"))
        for c, val in enumerate(values[r]):
            parts.append(
                text(cw * (c + 1) + cw / 2, 12 + (r + 1) * rh + 12, val, size=8, anchor="middle", fill="#444")
            )
    parts.append("</g>")
    parts.append("</g>")

    # Arrow into MaxEnt
    parts.append(arrow_down(230, 175, 205, sw=1.6))

    # Maximum Entropy box
    parts.append('<g id="c2_maxent">')
    parts.append(
        '<rect x="120" y="210" width="220" height="48" rx="4" fill="#ececec" '
        'stroke="#555" stroke-width="1.2"/>'
    )
    parts.append(text(230, 230, "Maximum Entropy", size=13, weight="700", anchor="middle"))
    parts.append(
        text(230, 248, "Least biased feasible community", size=10, style="italic", anchor="middle", fill="#444")
    )
    parts.append("</g>")

    parts.append(arrow_down(230, 262, 290, sw=1.6))

    # Species mixture bar chart
    parts.append('<g id="c2_species_mixture">')
    parts.append(
        text(230, 285, "Inferred species mixture (relative abundances at each pixel)", size=10, weight="600", anchor="middle")
    )
    bars = [
        ("Sp A", LEAF_N, 55),
        ("Sp B", LMA, 120),
        ("Sp C", WATER, 40),
        ("Sp D", PIGMENTS, 70),
        ("Sp E", SP_E, 35),
    ]
    bx, by = 40, 300
    for i, (lab, col, wbar) in enumerate(bars):
        yy = by + i * 18
        parts.append(f'<rect x="{bx}" y="{yy}" width="{wbar}" height="12" fill="{col}" rx="1"/>')
        parts.append(text(bx - 6, yy + 10, lab, size=9, anchor="end"))
    parts.append("</g>")

    # Community trait distributions
    parts.append('<g id="c2_trait_distributions">')
    gx, gy, gw, gh = 40, 420, 360, 100
    parts.append(
        text(
            gx + gw / 2,
            gy - 12,
            "Community trait distributions through time (at each pixel)",
            size=10,
            weight="600",
            anchor="middle",
        )
    )
    parts.append(
        f'<rect x="{gx}" y="{gy}" width="{gw}" height="{gh}" fill="#fafafa" stroke="#bbb" stroke-width="1"/>'
    )
    # axes labels
    fx, fy = gx - 14, gy + gh / 2
    parts.append(
        f'<text x="{fx:.1f}" y="{fy:.1f}" font-family="{FONT}" font-size="9" '
        f'text-anchor="middle" fill="#555" '
        f'transform="rotate(-90 {fx:.1f} {fy:.1f})">Frequency</text>'
    )

    # three curves
    past = bell_path(gx + 20, gy + gh - 10, gw - 40, 70, mean=0.42, sd=0.15, skew=0.3)
    present = bell_path(gx + 20, gy + gh - 10, gw - 40, 75, mean=0.50, sd=0.13, skew=0.1)
    future = bell_path(gx + 20, gy + gh - 10, gw - 40, 65, mean=0.62, sd=0.16, skew=-0.2)
    parts.append(f'<path d="{past}" fill="none" stroke="{WATER}" stroke-width="2"/>')
    parts.append(f'<path d="{present}" fill="none" stroke="{LEAF_N}" stroke-width="2"/>')
    parts.append(
        f'<path d="{future}" fill="none" stroke="{PIGMENTS}" stroke-width="2" stroke-dasharray="5 3"/>'
    )

    # legend
    parts.append(f'<line x1="{gx + 250}" y1="{gy + 18}" x2="{gx + 275}" y2="{gy + 18}" stroke="{WATER}" stroke-width="2"/>')
    parts.append(text(gx + 280, gy + 22, "Past", size=9))
    parts.append(f'<line x1="{gx + 250}" y1="{gy + 34}" x2="{gx + 275}" y2="{gy + 34}" stroke="{LEAF_N}" stroke-width="2"/>')
    parts.append(text(gx + 280, gy + 38, "Present", size=9))
    parts.append(
        f'<line x1="{gx + 250}" y1="{gy + 50}" x2="{gx + 275}" y2="{gy + 50}" stroke="{PIGMENTS}" '
        f'stroke-width="2" stroke-dasharray="4 2"/>'
    )
    parts.append(text(gx + 280, gy + 54, "Future", size=9))

    # stats labels under axis, then axis title below them
    stats = ["Mean", "Variance", "Skewness", "Tails", "..."]
    for i, s in enumerate(stats):
        parts.append(text(gx + 40 + i * 70, gy + gh + 16, s, size=9, fill="#666", anchor="middle"))
    parts.append(text(gx + gw / 2, gy + gh + 34, "Trait value (e.g., LMA)", size=9, anchor="middle", fill="#555"))
    parts.append("</g>")

    parts.append("</g>")
    return "\n".join(parts)


def mini_dist_panel(
    x: float,
    y: float,
    title: str,
    subtitle: str,
    curves: list[tuple[str, float, float, float, str | None]],
) -> str:
    """Small hypothesis panel with overlapping distributions."""
    parts = [f'<g transform="translate({x},{y})">']
    parts.append(text(0, 0, title, size=10, weight="700"))
    parts.append(text(0, 13, subtitle, size=8.5, fill="#555"))
    parts.append('<rect x="0" y="20" width="145" height="55" fill="#fafafa" stroke="#ccc" stroke-width="0.8"/>')
    for color, mean, sd, skew, dash in curves:
        d = bell_path(8, 68, 130, 40, mean=mean, sd=sd, skew=skew)
        dash_attr = f' stroke-dasharray="{dash}"' if dash else ""
        parts.append(f'<path d="{d}" fill="none" stroke="{color}" stroke-width="1.5"{dash_attr}/>')
    parts.append("</g>")
    return "\n".join(parts)


def tdt_process(
    x: float,
    y: float,
    title: str,
    subtitle: str,
    icon_svg: str,
    note: str,
    curve_means: tuple[float, float] = (0.4, 0.55),
) -> str:
    parts = [f'<g transform="translate({x},{y})">']
    parts.append(text(55, 0, title, size=10, weight="700", anchor="middle"))
    parts.append(text(55, 12, subtitle, size=8, fill="#555", anchor="middle"))
    # two overlapping curves
    parts.append('<rect x="5" y="18" width="100" height="40" fill="#fafafa" stroke="#ddd" stroke-width="0.7"/>')
    c1 = bell_path(10, 52, 90, 28, mean=curve_means[0], sd=0.14)
    c2 = bell_path(10, 52, 90, 28, mean=curve_means[1], sd=0.12)
    parts.append(f'<path d="{c1}" fill="none" stroke="#555" stroke-width="1.3"/>')
    parts.append(f'<path d="{c2}" fill="none" stroke="#888" stroke-width="1.3" stroke-dasharray="3 2"/>')
    parts.append(arrow_down(55, 60, 78, sw=1.2))
    parts.append(f'<g transform="translate(35,80)">{icon_svg}</g>')
    parts.append(multiline(55, 125, note.split("\n"), size=8, anchor="middle", fill="#444"))
    parts.append("</g>")
    return "\n".join(parts)


def column3(x: float, y: float) -> str:
    parts = [f'<g id="layer_column3" transform="translate({x},{y})">']
    parts.append(text(0, 0, "3. Trait Driver Theory & Ecological Change", size=16, weight="700"))
    parts.append(
        text(0, 18, "Test mechanisms and forecast ecosystem reorganization", size=11, style="italic", fill="#444")
    )

    # TDT processes
    parts.append('<g id="c3_tdt_processes">')
    parts.append(text(0, 42, "Core processes (Trait Driver Theory)", size=11, weight="600"))

    selection_icon = (
        f'<circle cx="4" cy="8" r="4" fill="{LEAF_N}"/>'
        f'<circle cx="16" cy="6" r="4" fill="{LMA}"/>'
        f'<circle cx="28" cy="10" r="4" fill="{WATER}"/>'
    )
    parts.append(
        tdt_process(
            0,
            55,
            "Selection",
            "(environmental filtering)",
            selection_icon,
            "Shifts in trait optima\nand niche breadth",
            (0.35, 0.55),
        )
    )
    parts.append(
        tdt_process(
            155,
            55,
            "Dispersal",
            "(movement, colonization)",
            dispersal_icon(0, 0, 0.85),
            "Introduces new traits\nand modes",
            (0.4, 0.65),
        )
    )
    parts.append(
        tdt_process(
            310,
            55,
            "Drift",
            "(stochastic dynamics)",
            die_icon(2, 0, 0.9),
            "Random changes in\ntrait frequencies",
            (0.45, 0.5),
        )
    )
    parts.append("</g>")

    # Hypotheses — 2×2 grid matching original layout intent
    parts.append('<g id="c3_hypotheses">')
    parts.append(text(0, 205, "Test key hypotheses", size=11, weight="600"))
    parts.append(
        mini_dist_panel(
            0,
            215,
            "Environmental filtering",
            "Trait optima shift under climate change",
            [
                (WATER, 0.38, 0.12, 0.0, None),
                (PIGMENTS, 0.62, 0.12, 0.0, "3 2"),
            ],
        )
    )
    parts.append(
        mini_dist_panel(
            230,
            215,
            "Post-fire succession",
            "Distributions broaden then narrow",
            [
                (WATER, 0.5, 0.10, 0.0, None),
                (LMA, 0.5, 0.20, 0.0, "3 2"),
                (LEAF_N, 0.5, 0.11, 0.0, None),
            ],
        )
    )
    parts.append(
        mini_dist_panel(
            0,
            310,
            "Range expansion",
            "New modes as species arrive",
            [
                (WATER, 0.35, 0.11, 0.0, None),
                (PIGMENTS, 0.72, 0.10, 0.0, "3 2"),
            ],
        )
    )
    parts.append(
        mini_dist_panel(
            230,
            310,
            "Biome reorganization",
            "Community-level shifts in trait distributions",
            [
                (WATER, 0.40, 0.14, 0.2, None),
                (LEAF_N, 0.55, 0.13, -0.1, None),
                (PIGMENTS, 0.68, 0.15, 0.0, "3 2"),
            ],
        )
    )
    parts.append("</g>")

    # Ecological change forecast
    parts.append('<g id="c3_ecological_change">')
    parts.append(arrow_down(230, 400, 425, sw=1.5))
    parts.append(text(0, 445, "Ecological change (forecast)", size=11, weight="600"))

    parts.append(protea_icon(20, 455, 1.0))
    parts.append(text(70, 480, "→", size=18, fill="#555"))
    parts.append(shrub_icon(95, 455, 1.0))
    parts.append(text(145, 480, "→", size=18, fill="#555"))
    parts.append(grass_icon(170, 455, 1.0))

    bullets = [
        "Community turnover",
        "Functional diversity",
        "Resilience",
        "Regime shifts",
        "Novel communities",
    ]
    parts.append(text(260, 465, "•  " + bullets[0], size=11))
    for i, b in enumerate(bullets[1:], start=1):
        parts.append(text(260, 465 + i * 16, "•  " + b, size=11))
    parts.append("</g>")

    parts.append("</g>")
    return "\n".join(parts)


def footer() -> str:
    parts = ['<g id="layer_footer">']
    # gradient arrow spanning bottom
    y = 1055
    parts.append(
        f'''<defs>
  <linearGradient id="footerGrad" x1="0%" y1="0%" x2="100%" y2="0%">
    <stop offset="0%" stop-color="#3498DB"/>
    <stop offset="45%" stop-color="#A67C52"/>
    <stop offset="100%" stop-color="#2E8B57"/>
  </linearGradient>
</defs>'''
    )
    # thick arrow body
    parts.append(
        f'<path d="M 40 {y} L 1520 {y} L 1520 {y - 8} L 1565 {y + 10} L 1520 {y + 28} '
        f'L 1520 {y + 20} L 40 {y + 20} Z" fill="url(#footerGrad)" stroke="#333" stroke-width="0.8"/>'
    )
    parts.append(text(220, y + 48, "Remote sensing observations", size=13, weight="600", anchor="middle", fill="#1a5276"))
    parts.append(
        text(
            800,
            y + 48,
            "Reconstructed community trait distributions",
            size=13,
            weight="600",
            anchor="middle",
            fill="#6e4b2a",
        )
    )
    parts.append(
        text(
            1300,
            y + 48,
            "Mechanistic understanding and forecasting",
            size=13,
            weight="600",
            anchor="middle",
            fill="#1e5c38",
        )
    )
    parts.append("</g>")
    return "\n".join(parts)


def connectors() -> str:
    # Large arrows between columns
    parts = ['<g id="layer_connectors">']
    parts.append(arrow_right(470, 380, 530, sw=4))
    parts.append(arrow_right(1000, 380, 1060, sw=4))
    parts.append("</g>")
    return "\n".join(parts)


def build() -> str:
    body = []
    body.append(
        f'''<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg"
     xmlns:xlink="http://www.w3.org/1999/xlink"
     width="{W}" height="{H}"
     viewBox="0 0 {W} {H}"
     role="img"
     aria-label="Trait workflow: remote sensing to ecological forecasting">
<title>Hindcast traits → Infer mixtures → Trait Driver Theory</title>
<desc>Editable layered SVG for Affinity Designer / Canva. Groups are named by layer.</desc>
<defs>
  <style type="text/css"><![CDATA[
    text {{ font-family: {FONT}; }}
  ]]></style>
  <marker id="arrowhead-sm" markerWidth="6" markerHeight="6" refX="5" refY="3" orient="auto">
    <path d="M0,0 L6,3 L0,6 Z" fill="#333"/>
  </marker>
</defs>
'''
    )
    body.append('<g id="layer_background">')
    body.append(f'<rect width="{W}" height="{H}" fill="#ffffff"/>')
    # subtle column guides (locked reference – delete in Affinity if unwanted)
    body.append(
        '<rect x="20" y="20" width="450" height="1000" fill="#fbfbfb" stroke="none"/>'
        '<rect x="540" y="20" width="450" height="1000" fill="#fafafa" stroke="none"/>'
        '<rect x="1070" y="20" width="500" height="1000" fill="#fbfbfb" stroke="none"/>'
    )
    body.append("</g>")

    body.append(column1(30, 50))
    body.append(column2(545, 50))
    body.append(column3(1075, 50))
    body.append(connectors())
    body.append(footer())

    # Editing notes as a hidden-ish layer for Affinity users
    body.append(
        '''<g id="layer_editing_notes" opacity="0">
  <text x="20" y="20" font-size="10">Affinity tips: Ungroup as needed; all text is live text;
  charts are paths/rects; trait colors are set per-shape fill.</text>
</g>'''
    )

    body.append("</svg>")
    return "\n".join(body)


def main() -> None:
    svg = build()
    out = OUT / "trait_workflow_editable.svg"
    out.write_text(svg, encoding="utf-8")
    print(f"Wrote {out} ({len(svg):,} bytes)")


if __name__ == "__main__":
    main()
