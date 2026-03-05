# Dendrogramme CHD simplifié (Python)

Oui, il existe une option **plus simple** via Python: le script `python_dendrogramme_simple.py`.

## Objectif
- fond blanc (pas de fond noir)
- noms de classes horizontaux
- mots affichés sous les classes, en style nuage simplifié
- sortie en **SVG**
- **sans dépendances externes** (stdlib Python uniquement)

## Entrée
CSV avec colonnes:
- `Classe` (obligatoire)
- `Terme` (obligatoire)
- `chi2` (optionnelle)
- `frequency` (optionnelle)

## Exemple
```bash
python python_dendrogramme_simple.py \
  --stats stats_par_classe.csv \
  --out dendrogramme_simple.svg \
  --top-n 8
```

## Notes
- Si `chi2` est absent ou non positif, le script retombe sur `frequency`.
- Si `frequency` est absente, une pondération unitaire est utilisée.
