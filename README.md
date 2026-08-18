# A New Multidimensional Energy Poverty Index for Mexico

Replication package for *A New Multidimensional Energy Poverty Index for Mexico*
(Carrasco-Jiménez).

The paper constructs a time-comparable Multidimensional Energy Poverty Index (MEPI)
for Mexico over 2016–2024 from ENIGH microdata, using the Alkire–Foster framework
extended to a **dimensionally-flexible** setting in which the set of relevant
dimensions varies across individuals. Eight dimensions are used: electricity access,
cooking, water heating, thermal comfort, refrigeration, entertainment, communication
and energy affordability. Thermal comfort applies only in municipalities classified
as hot or cold, and weights are renormalized per individual through an availability
matrix.

Everything reported in the paper — every table cell, every figure, every number in
the running text — is produced by the scripts in this repository. There are no ad
hoc values. `8_extract_for_tex.R` prints all of them, grouped by the LaTeX label that
consumes them.

---

## 1. Requirements

- **R ≥ 4.2.** The reported results were produced under **R 4.3.2 (2023-10-31)**
  on macOS (darwin, arm64).

```r
install.packages(c(
  "pacman", "dplyr", "tidyr", "purrr", "stringr", "readr", "haven", "zoo",
  "data.table", "bit64", "tidyverse", "survey", "srvyr", "convey", "dineq",
  "FactoMineR", "broom", "scales", "sf", "stars", "spdep", "gstat",
  "ggplot2", "ggthemes", "paletteer", "latex2exp", "xtable", "stargazer",
  "readxl", "writexl", "rstudioapi"
))
```

Every script begins with a `pacman::p_load()` call that installs anything missing,
so in practice running them in order is sufficient.

Spatial work (`1_spatial.R`) needs a working GDAL/GEOS/PROJ stack behind `sf`. On
macOS, `brew install gdal geos proj` before installing `sf` from source.

There is **no `renv.lock` in this repository yet**. Until one is generated
(`renv::init(); renv::snapshot()`), the versions the reported results were
produced under are recorded here:

| Package | Version | Package | Version | Package | Version |
|---|---|---|---|---|---|
| pacman | 0.5.1 | dineq | 0.1.0 | ggthemes | 5.1.0 |
| dplyr | 1.1.4 | FactoMineR | 2.9 | paletteer | 1.6.0 |
| tidyr | 1.3.1 | broom | 1.0.5 | latex2exp | 0.9.6 |
| purrr | 1.0.4 | scales | 1.4.0 | xtable | 1.8.4 |
| stringr | 1.5.1 | sf | 1.0.19 | stargazer | 5.2.3 |
| readr | 2.1.5 | stars | 0.6.8 | readxl | 1.4.5 |
| haven | 2.5.4 | spdep | 1.3.13 | writexl | 1.5.4 |
| zoo | 1.8.12 | gstat | 2.1.4 | rstudioapi | 0.17.1 |
| data.table | 1.16.4 | ggplot2 | 3.5.2 | tidyverse | 2.0.0 |
| bit64 | 4.6.0.1 | survey | 4.2.1 | srvyr | 1.2.0 |
| convey | 1.0.0 | | | | |

`survey` 4.2.1 is the one that matters for reproducing the point estimates and
standard errors exactly; the rest are data-handling and plotting dependencies.

## 2. Getting the data

`inputs/` is **not** tracked (see `.gitignore`) — roughly 3.5 GB of public
microdata. Download it and reproduce the following layout:

```
inputs/
├── viviendas/viviendas{2016,2018,2020,2022,2024}.csv     ENIGH, dwellings
├── hogares/hogares{...}.csv                              ENIGH, households
├── poblacion/poblacion{...}.csv                          ENIGH, persons
├── concentrado/concentradohogar{...}.csv                  ENIGH, aggregates
├── gastos/gastoshogar{...}.csv, gastospersona{...}.csv    ENIGH, expenditure
├── pobreza/pobreza{...}.csv                              CONEVAL / INEGI poverty
├── additional/inpc.csv                                   INPC deflators
├── additional/lp.xlsx                                    CONEVAL poverty lines
└── spatial_data/
    ├── ent/ent.shp        state polygons          (INEGI geostatistical framework)
    ├── mun/mun.shp        municipal polygons      (idem)
    └── temp/              temperature isolines and weather-station records
```

Sources:

| Input | Source |
|---|---|
| ENIGH 2016–2024 (*nueva serie*) | INEGI, ENIGH microdata, `Datos abiertos` (CSV) |
| Poverty modules 2016–2022 | CONEVAL, *Medición de la pobreza*, ENIGH-linked datasets |
| Poverty module 2024 | INEGI (the mandate transferred from CONEVAL in 2025; the methodology is unchanged) |
| INPC deflators | INEGI, *Índice Nacional de Precios al Consumidor* |
| Poverty lines (LPI) | CONEVAL, *Líneas de pobreza por ingresos* |
| Municipal and state polygons | INEGI, *Marco Geoestadístico* |
| Temperature isolines, weather stations | INEGI, climatological cartography |

Two notes on the raw files:

- `temp/temp.shp` ships **without** a `.prj`. `1_spatial.R` assigns the CRS
  manually; it was produced by INEGI in the same projection as the municipal
  framework (MEXICO_ITRF_2008_LCC).
- Variable names differ across waves in two places, both handled in `2_data.R`:
  2016–2022 use `combustible` / `estufa_chi` where 2024 uses `combus` / `fogon_chi`,
  and missing television counts are coded `-1` in 2016–2020 but `"&"` in 2022–2024.

## 3. Running the pipeline

Scripts are numbered in execution order. Each reads what the previous one wrote,
so running them out of order will either fail or silently use stale data. Run from
the repository root (the paths are relative to it).

`0_legend_helpers.R` is a function library, not a stage: it is `source()`d by
`4_plots.R` and is never run on its own. Everything else runs in order.

```sh
Rscript 1_spatial.R              # climate zones          -> outputs/intermediate/spatial.RData
Rscript 2_data.R                 # achievement matrix     -> outputs/intermediate/dt.RData   (~79 MB)
Rscript 3_estimations.R          # survey estimates       -> outputs/final/*.RData            (slow)
Rscript 4_plots.R                # figures                -> outputs/plots/*.jpeg
Rscript 5_climate_sensitivity.R  # climate robustness     -> outputs/final/climate_sensitivity.RData
Rscript 6_et_drop.R              # entertainment drop     -> outputs/final/et_sensitivity.RData
Rscript 7_diagnostics.R          # indicator diagnostics  -> outputs/final/diagnostics.RData
Rscript 8_extract_for_tex.R > outputs/final/values_for_tex.txt
```

**`3_estimations.R` takes 3 h 46 min** and is ~97% of a full rebuild. That is measured, not
estimated: the subgroup block runs `svyby()` for every variable × subgroup
combination on a ~300,000-record clustered design (about six minutes per wave),
and the $k$-sensitivity block then rebuilds the survey design 100 times over
5 cutoffs × 5 waves × 4 weighting schemes. Start it when you do not need the
machine back soon.

Everything else is quick: `2_data.R` reads ~3.5 GB of raw CSV but is I/O-bound and
finishes in 1 min 40 s; `1_spatial.R` takes 26 s; `4_plots.R` 20 s;
`5_climate_sensitivity.R` 2 min; `6_et_drop.R` 18 s; `7_diagnostics.R` 1 min;
`8_extract_for_tex.R` 5 s. (Earlier versions of this file called `2_data.R` the
slow step. That has not been true since the estimation script grew its robustness
blocks.) See `PIPELINE.md` for the per-block breakdown of script 3.

Scripts 5, 6 and 7 depend only on `dt.RData` plus `outputs/final/robustness.RData`
(5 and 6, for the baseline sanity check and the main-scheme comparison columns)
and `estimations_means.RData` / `estimations_inc.RData` (7), so after a full build
any of them can be re-run on its own without redoing 1–3.

`PIPELINE.md` documents what each script does, the wave-specific quirks it handles,
and a table mapping every LaTeX label in the paper to the object that produces it.

## 4. Verifying the paper against the code

`8_extract_for_tex.R` is the audit trail. It reads every object in `outputs/final/`
and prints each numeric value that appears in `essay_I.tex`, grouped under the
LaTeX label of the table or figure that consumes it. After any change to scripts
1–7, re-run it and diff against the previous dump to see exactly which table cells
move:

```sh
Rscript 8_extract_for_tex.R > outputs/final/values_for_tex.new
diff outputs/final/values_for_tex.txt outputs/final/values_for_tex.new
```

`outputs/final/*.RData` is tracked in this repository (about 250 KB in total), so
a reader can check any number in the paper without re-running the pipeline over
the raw microdata. The dump itself, `outputs/final/values_for_tex.txt`, is
`.gitignore`d — it is a derived artefact of the tracked `.RData` files.

## 5. Compiling the paper

`essay_I.tex` is compiled with `pdflatex` + `biber` (biblatex, APA style) and
requires `references.bib`. It expects figures under `EECC/figures/`, which is the
Overleaf project layout; when compiling locally, either point `\includegraphics`
at `outputs/plots/` or symlink it.

**`references.bib` is not in this folder** — it is maintained in the Overleaf
project, and the source as deposited here will therefore not build until it is
brought in. This is the one outstanding gap in the replication package; see the
checklist at the end of `PIPELINE.md`.

## 6. Repository layout

```
.
├── 0_legend_helpers.R              map legend scales, derived from the data
├── 1_spatial.R … 7_diagnostics.R   pipeline, in execution order
├── 8_extract_for_tex.R             code-to-text audit trail
├── essay_I.tex                     the paper
├── PIPELINE.md                     what each script does; label -> object map
├── README.md                       this file
├── lit/                            background PDFs (not part of the pipeline)
├── inputs/                         raw microdata (not tracked — see §2)
└── outputs/
    ├── intermediate/               dt.RData, spatial.RData (not tracked)
    ├── final/                      estimation output (tracked)
    └── plots/                      29 figures (not tracked; 20 from 4_plots.R,
                                    9 from 1_spatial.R)
```

Not present yet, and referenced by earlier drafts of this file: `references.bib`
(lives in Overleaf, see §5), `renv.lock` (see §1) and `CITATION.cff` (see §7).

## 7. Citation

If you use this code, please cite the paper. A `CITATION.cff` will be added
alongside the archived release.

## 8. License

`[Author: choose a license before publishing — MIT or BSD-3-Clause for the code,
CC BY 4.0 for the text and figures, is the usual pairing for an economics
replication package.]`
