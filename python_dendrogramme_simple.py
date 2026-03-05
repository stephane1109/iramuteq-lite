#!/usr/bin/env python3
"""Génère un dendrogramme CHD simplifié en SVG (sans dépendances externes).

Entrée attendue: CSV avec au moins les colonnes `Classe` et `Terme`.
Colonnes optionnelles: `chi2`, `frequency`.

Exemple:
  python python_dendrogramme_simple.py --stats stats_par_classe.csv --out dendrogramme_simple.svg
"""

from __future__ import annotations

import argparse
import csv
import html
from collections import defaultdict
from pathlib import Path


def to_int(value: str) -> int | None:
    try:
        return int(float(str(value).strip()))
    except Exception:
        return None


def to_float(value: str, default: float = 0.0) -> float:
    try:
        f = float(str(value).strip())
        return f if f == f else default
    except Exception:
        return default


def read_stats(path: Path):
    by_class = defaultdict(list)
    counts = defaultdict(float)

    with path.open("r", encoding="utf-8-sig", newline="") as f:
        reader = csv.DictReader(f)
        if not reader.fieldnames:
            raise ValueError("CSV vide ou en-têtes absents.")

        cols = {c.lower(): c for c in reader.fieldnames}
        classe_col = cols.get("classe")
        terme_col = cols.get("terme")
        if not classe_col or not terme_col:
            raise ValueError("Colonnes requises manquantes: 'Classe' et 'Terme'.")

        chi2_col = cols.get("chi2")
        freq_col = cols.get("frequency")

        for row in reader:
            cl = to_int(row.get(classe_col, ""))
            terme = (row.get(terme_col, "") or "").strip()
            if cl is None or cl <= 0 or not terme:
                continue

            score = to_float(row.get(chi2_col, ""), 0.0) if chi2_col else 0.0
            freq = to_float(row.get(freq_col, ""), 1.0) if freq_col else 1.0
            if score <= 0:
                score = freq
            if score <= 0:
                score = 1.0

            by_class[cl].append((terme, score))
            counts[cl] += freq if freq > 0 else 1.0

    if not by_class:
        raise ValueError("Aucune donnée exploitable dans le CSV.")

    for cl in list(by_class):
        merged = {}
        for term, score in by_class[cl]:
            merged[term] = max(score, merged.get(term, 0.0))
        by_class[cl] = sorted(merged.items(), key=lambda t: t[1], reverse=True)

    return by_class, counts


def color_for_class(cl: int) -> str:
    palette = [
        "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
        "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
    ]
    return palette[(cl - 1) % len(palette)]


def make_svg(by_class, counts, top_n: int, out: Path):
    classes = sorted(by_class)
    n = len(classes)

    width = max(980, 250 * n)
    height = 720

    margin_x = 70
    trunk_y = 110
    class_y = 180
    words_y = 245

    step = (width - 2 * margin_x) / max(n, 1)
    x_for = {cl: margin_x + (i + 0.5) * step for i, cl in enumerate(classes)}

    total = sum(counts.values()) or 1.0

    parts = []
    add = parts.append
    add(f"<svg xmlns='http://www.w3.org/2000/svg' width='{width}' height='{height}' viewBox='0 0 {width} {height}'>")
    add("<rect width='100%' height='100%' fill='white'/>")
    add("<style>text{font-family:Arial,Helvetica,sans-serif;} .title{font-size:24px;font-weight:700;} .line{stroke:#2a2a2a;stroke-width:2;} .small{fill:#555;font-size:12px;}</style>")
    add(f"<text x='{width/2:.1f}' y='52' text-anchor='middle' class='title'>Dendrogramme CHD (affichage simplifié)</text>")

    min_x = min(x_for.values())
    max_x = max(x_for.values())
    add(f"<line x1='{min_x:.1f}' y1='{trunk_y}' x2='{max_x:.1f}' y2='{trunk_y}' class='line'/>" )

    for cl in classes:
        x = x_for[cl]
        col = color_for_class(cl)
        pct = counts[cl] * 100.0 / total

        add(f"<line x1='{x:.1f}' y1='{trunk_y}' x2='{x:.1f}' y2='{class_y-24}' class='line'/>" )
        add(f"<text x='{x:.1f}' y='{class_y:.1f}' text-anchor='middle' fill='{col}' font-size='17' font-weight='700'>Classe {cl} ({pct:.1f} %)</text>")

        terms = by_class[cl][:top_n]
        if not terms:
            continue

        max_score = max(score for _, score in terms) or 1.0
        offsets_x = [-95, -45, 0, 45, 95, -70, 70, -25, 25, 0]
        offsets_y = [0, 22, 38, 18, 2, 54, 54, 72, 72, 90]
        for i, (term, score) in enumerate(terms):
            rx = offsets_x[i % len(offsets_x)]
            ry = offsets_y[i % len(offsets_y)]
            size = 12 + int(12 * (score / max_score))
            safe = html.escape(term)
            add(
                f"<text x='{x + rx:.1f}' y='{words_y + ry:.1f}' text-anchor='middle' fill='{col}' "
                f"font-size='{size}'>{safe}</text>"
            )

    add("<text x='70' y='685' class='small'>Fond blanc • classes et mots horizontaux • rendu SVG sans dépendances Python externes.</text>")
    add("</svg>")

    out.write_text("\n".join(parts), encoding="utf-8")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--stats", required=True, type=Path, help="CSV des stats CHD (colonnes: Classe, Terme, chi2/frequency optionnelles)")
    ap.add_argument("--out", required=True, type=Path, help="Fichier SVG de sortie")
    ap.add_argument("--top-n", type=int, default=8, help="Nombre de mots par classe")
    args = ap.parse_args()

    top_n = max(1, int(args.top_n))
    by_class, counts = read_stats(args.stats)
    make_svg(by_class, counts, top_n=top_n, out=args.out)
    print(f"OK: SVG généré -> {args.out}")


if __name__ == "__main__":
    main()
