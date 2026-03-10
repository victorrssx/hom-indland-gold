# Overleaf package (Amazonia + Cerrado, 2010+, treatment 2016)

## Main file
- `main.tex`

## Tables
- `tables/table_main_compact.tex` (Table 1 in paper)
- `tables/table_dd_slices.tex` (replication/supporting table)
- `tables/table_additional_checks.tex` (replication/supporting table)

## Figures
- `figures/fig_event_study_ddd.pdf` (event-study figure)

## Replication summaries (CSV/TXT)
- `summary.txt`
- `additional_checks_summary.csv`
- `dd_slices_summary.csv`
- `event_study_ddd.csv`
- `event_study_ddd_stateyear.csv`

## Build locally
From this folder:

```bash
pdflatex main.tex
bibtex main
pdflatex main.tex
pdflatex main.tex
```
