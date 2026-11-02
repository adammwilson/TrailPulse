# Editable trait-workflow figure

Layered vector recreation of the remote-sensing → MaxEnt → Trait Driver Theory schematic, for editing in **Affinity Designer** (preferred) or **Canva**.

## Files

| File | Use |
|------|-----|
| `trait_workflow_editable.svg` | Primary editable file (full figure) |
| `trait_workflow_editable.pdf` | Vector PDF (also opens cleanly in Affinity) |
| `trait_workflow_editable.png` | Raster preview only |
| `layer_column1.svg` … `layer_column3.svg`, `layer_footer.svg` | Simpler per-panel SVGs for Canva |
| `generate_figure.py` | Regenerates the SVG if you want programmatic tweaks |

## Affinity Designer (best)

1. **File → Open** `trait_workflow_editable.svg` (or the PDF).
2. In the Layers panel you should see named groups:
   - `layer_background` — column wash (delete if you want pure white)
   - `layer_column1` / `layer_column2` / `layer_column3` — main content
   - `layer_connectors` — big arrows between columns
   - `layer_footer` — gradient process arrow + labels
3. Text is live `<text>` — double-click to edit.
4. Charts are paths/rects — select and restyle fills/strokes.
5. Trait colors used throughout: Leaf N `#2E8B57`, LMA `#E67E22`, Water `#3498DB`, Pigments `#E74C3C`.

Optional: **File → Export** → PDF/PNG/SVG for journals or slides.

## Canva

Canva’s SVG import is limited on complex figures (groups may flatten; some text may convert to shapes).

Practical approach:

1. Upload `trait_workflow_editable.svg` (Canva Pro) **or** place the PNG as a locked background.
2. Prefer editing **one column at a time**: in Affinity, export each `layer_column*` as its own SVG, then upload those simpler files to Canva.
3. Rebuild labels you need to change with Canva text boxes on top of the locked graphic.

## Regenerate

```bash
python3 generate_figure.py
```

Requires only the Python standard library for SVG output. PNG/PDF preview used `cairosvg` in this environment.
