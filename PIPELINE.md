# Work pipeline — *A New Multidimensional Energy Poverty Index for Mexico*

How the paper is produced, end to end: raw INEGI/CONEVAL inputs → achievement matrix → index
estimates → tables and figures → `essay_I.tex`.

Scripts are numbered in execution order. Each one reads what the previous one wrote, so running
them out of order will either fail or silently use stale data.

---

## 0. Quick reference

```
inputs/                      ⟶ 1_spatial.R              ⟶ outputs/intermediate/spatial.RData
inputs/ + spatial.RData      ⟶ 2_data.R                 ⟶ outputs/intermediate/dt.RData
dt.RData                     ⟶ 3_estimations.R          ⟶ outputs/final/{estimations_*,robustness,
                                                            k_sensitivity,dimensional_decomposition,
                                                            af_circularity}.RData
outputs/final/ + dt.RData    ⟶ 4_plots.R                ⟶ outputs/plots/*.jpeg
   └── sources 0_legend_helpers.R
dt.RData                     ⟶ 5_climate_sensitivity.R  ⟶ outputs/final/climate_sensitivity.RData
dt.RData                     ⟶ 6_et_drop.R              ⟶ outputs/final/et_sensitivity.RData
dt.RData + inputs/ + final/  ⟶ 7_diagnostics.R          ⟶ outputs/final/diagnostics.RData
all of outputs/final/        ⟶ 8_extract_for_tex.R        ⟶ stdout dump, keyed by LaTeX label
```

Full rebuild from scratch: run `1` → `7` in order, then `8_extract_for_tex.R`. `dt.RData` is ~79 MB.

Measured wall-clock on the development machine (R 4.3.2, Apple silicon, August 2026):

| Script | Time |
|---|---|
| `1_spatial.R` | 26 s |
| `2_data.R` | 1 min 40 s |
| `3_estimations.R` | **3 h 46 min** |
| `4_plots.R` | 20 s |
| `5_climate_sensitivity.R` | 2 min 05 s |
| `6_et_drop.R` | 18 s |
| `7_diagnostics.R` | 1 min |
| `8_extract_for_tex.R` | ~5 s |

**`3_estimations.R` is the pipeline, time-wise** — it is ~97% of a full rebuild, and the number is
not a typo. Budget an afternoon, and do not start it expecting to iterate. Four blocks account
for it:

1. *Subgroup means* — `svyby()` for every variable × subgroup combination (20 × 8) on a
   ~300,000-record clustered design. Measured at 5.5–6.5 minutes per wave, ~30 min total.
2. *k sensitivity* — 5 cutoffs × 5 waves × 4 weighting schemes = 100 fresh `svydesign()`
   constructions, each followed by two `svymean()` calls. This is the single most expensive block.
3. *Decile/centile* — `svyby()` grouped by 100 ICTPC centiles, 5 variables, 5 waves.
4. *Totals and the dimensional decomposition*, which rebuild the design once more per wave.

Every block re-runs `svydesign()` from scratch rather than subsetting a design built once per
wave; that is where the time goes. If this ever needs to be faster, build the design once per
wave and pass subsets of it, rather than reaching for parallelism.

Earlier versions of this file called `2_data.R` the slow step. That stopped being true once the
robustness blocks were added to script 3: `2_data.R` reads ~3.5 GB of raw CSV but is I/O-bound
and finishes in under two minutes.

**Every number in `essay_I.tex` is produced by one of these scripts.** There are no ad hoc values.
`8_extract_for_tex.R` prints all of them grouped by the LaTeX label that consumes them; to capture
the dump for a replication package:

```sh
Rscript 8_extract_for_tex.R > outputs/final/values_for_tex.txt
```

---

## 1. `1_spatial.R` — climate zones

**Reads** `inputs/spatial_data/` (INEGI isoline shapefiles, weather-station records, municipal
geostatistical framework).
**Writes** `outputs/intermediate/spatial.RData` (object `spatial_char`).

What it does:

1. Loads the temperature isoline shapefile and assigns the CRS manually — `temp.shp` ships
   without a `.prj`, but was produced by INEGI in the same projection as `00mun.shp`
   (MEXICO_ITRF_2008_LCC). Filters out non-temperature polygons (`H2O` water bodies, `P`/`E`
   precipitation-evaporation zones).
2. Spatially intersects isolines with municipal polygons; municipalities that no isoline reaches
   fall through to a **nearest-isoline fallback**.
3. Separately, reads the 1,494 weather stations (1902–2012), collapses each to its long-run
   annual mean / max / min, and **IDW-interpolates** to municipal centroids
   (`nmax = 10`, `idp = 2`).
4. Assigns each municipality a climate zone. This is the definitive rule, stated as
   equation (1) in §2.1 of the paper — it precedes the AF block, so it takes the first
   number. (The AF equations that used to be labelled `\label{1}`…`\label{12}` in the
   tex were therefore rendering as (2)…(13); they now carry semantic labels
   `eq:deprivation`, `eq:availability`, `eq:weights`, `eq:score`, `eq:identification`,
   `eq:censored`, `eq:H`, `eq:A`, `eq:M0`, `eq:delta`, `eq:deltarel`, `eq:decomp`.)

   ```r
   weather = case_when(
     tmax_annual >= 30 & avg_temp >= 24 ~ 1,  # hot
     tmin_annual <= 10 & avg_temp <= 12 ~ 3,  # cold
     TRUE                               ~ 2   # temperate
   )
   ```

   Note it is **conjunctive on two variables**, not a single-threshold rule. An earlier draft of
   the paper described it as ">30 °C hot / <10 °C cold" in the Table 1 note; that was wrong and
   has been corrected.

Also produces the six temperature maps used in Figure 2.

## 2. `2_data.R` — dataset construction and weights

**Reads** `inputs/{viviendas,hogares,poblacion,concentrado,gastos,pobreza}/`, plus
`spatial.RData`.
**Writes** `outputs/intermediate/dt.RData` — `dt_list` (five wave-level data frames), the
weight vectors (`w_j`, `w_j_7dim`, and the `_mca` / `_eq` / `_freq` / `_noaf` variants), and
`mca_diagnostics`.

**Part I — achievement matrix.** Loops over the five waves, merging the ENIGH modules and
building the eight achievement indicators of Table 2. Wave-specific quirks handled here:

- 2016–2022 use `combustible` / `estufa_chi`; **2024 renames these to `combus` / `fogon_chi`**.
- 2016–2020 code missing TV counts as `-1`; 2022–2024 use `"&"`. `num_tv = num_tva + num_tvd`.
- Cooking fuel categories 6 ("other") and 7 ("doesn't cook") are **both** treated as
  achievements (`combus >= 3`). Category 7 only exists in the 2024 questionnaire, so the 6/7
  split is not comparable across waves — but since both map to the same achievement, the index
  is unaffected. Documented in the Table 2 note.
- Affordability compares `(ict − energy_exp) / tamhogesc` against CONEVAL's income poverty line
  (LPI), hard-coded in the `lineas` list, urban/rural, nominal at each wave. **This quantity
  *is* ICTPC, computed net of energy expenditures.** CONEVAL builds ICTPC as `ict/tamhogesc`
  — verified directly against the published microdata, where `ictpc ≡ ict/tamhogesc` to
  floating point — so `tamhogesc` is the correct and intended denominator, and the Af cutoff
  is exactly the line that defines income poverty. An August 2026 edit briefly asserted the
  opposite in the Table 2 note ("not ICTPC"); that was wrong and has been reverted. The
  hard-coded `lineas` values were checked against `plp` in the raw poverty modules and
  bracket the observed cut in every wave.
- The only exclusion imposed on the merged file is `filter(ictpc_l != -Inf)`, i.e. dropping
  households with zero total current income (the log transform for `ictpc_l` requires it).
  This is 115 / 157 / 290 / 156 / 160 individuals in 2016 / 2018 / 2020 / 2022 / 2024, and it
  accounts for the *entire* difference between the raw module row count and the realized
  sample in Table 1 — **no observation is dropped for missing achievement values**, contrary
  to what the Table 1 note used to say. Individuals with a missing indicator are retained and
  drop out pairwise via `svymean(na.rm = TRUE)`.

**Part II — weights.** Two schemes are derived here and two more are trivial:

- **NB (main).** Three tiers with adjacent ratio `0.20/0.13 ≈ 1.538`, taken from
  Nussbaumer et al. (2012, Table 2, p. 235). Their scheme is a **two-tier (3,3)** structure over
  six indicators — 0.20 ×3 and 0.13 ×3, summing to 0.99 — so only the *ratio* is borrowed; the
  third tier is this paper's own extension of the geometric progression. Renormalized to (3,3,2)
  over eight dimensions this gives `w₁ ≈ 0.1726`, `w₂ ≈ 0.1122`, `w₃ ≈ 0.0729`. In temperate
  municipalities Tc drops out and the (3,3,1) renormalization gives `0.1861 / 0.1210 / 0.0786`
  (the exact values are 0.186133 / 0.120987 / 0.078641 — earlier drafts rounded the first to
  0.1862, which is wrong in the fourth decimal). The NB-noAf scheme is the same (3,3,1) solve
  with Af rather than Tc removed, so it carries the identical weight triple.
- **MCA.** Weighted MCA on the pooled individual-level cross-sections, expansion factors as row
  weights. Axis 1 retained; weights are `|loading| / Σ|loading|`.
- **Equal** (`1/8`, `1/7` temperate) and **frequency** (`w ∝ 1 − p̄ⱼ`).

**Both data-driven schemes are estimated on the hot/cold subsample only.** `dt_pooled` is
filtered to `weather %in% c(1, 3)` because `dp_clim` is undefined in temperate municipalities
and MCA needs all eight indicators simultaneously defined. That pool is 21.4% of individuals pooled across the five waves (20.5-22.2% by wave),
and it is not a random 21.4% — hot and cold municipalities are disproportionately southern,
south-eastern and highland, with deprivation rates above the national ones. The same pool
supplies `p_j_w` for the frequency weights. This is a consequence of the availability
structure rather than a choice, but it is a disclosure obligation and is now stated in the
notes to `tab:mca` and `tab:robustness`.

The availability matrix logic lives here too: `dp_clim` is `NA` in temperate municipalities,
which is what drives the per-individual weight renormalization.

## 3. `3_estimations.R` — survey estimates

**Reads** `dt.RData`. **Writes** the bulk of `outputs/final/`.

All estimation uses `survey::svydesign(ids = ~upm, strata = ~est_dis, weights = ~factor)`.
Standard errors are linearization-based (ENIGH does not publish replicate weights for the new
series — now stated in the Table A3 note).

| Section | Output | Feeds (LaTeX label) |
|---|---|---|
| Means by subgroup | `estimations_means.RData` | `tab3`, `tab4`, `tab:states` |
| Delta tests 2016 vs 2024 | inside `robustness.RData` | `tab:delta` |
| Totals | `estimations_totals.RData` | `tab3_1` |
| Decile/centile | `estimations_inc.RData` | `fig:MEPIcen_plots`, centile regression |
| Robustness (4 schemes) | `robustness.RData` | `tab:robustness` |
| NB-noAf | `robustness.RData` (`af_drop`) | `tab:af_drop` |
| k sensitivity | `k_sensitivity.RData` | `fig:ksens`, `tab:ksens_headline` |
| Dimensional decomposition | `dimensional_decomposition.RData` | `tab:dim_contributions` |
| Af-circularity diagnostics | `af_circularity.RData` | `tab:af_circ` |

`SE(Δ) = √(SE²₂₀₁₆ + SE²₂₀₂₄)` throughout, assuming cross-section independence.

**Missing-value convention in `.ksens_one()`.** `mep_k` and `score_k` must be `NA` on the
same rows. An earlier version set `score_k = 0` when the raw score was `NA`, so individuals
with a missing indicator were counted as non-deprived in M₀ but excluded from H;
`svymean(na.rm = TRUE)` then computed the two over different denominators and `A = M₀/H`
inherited the mismatch. That is why `tab:ksens_headline` Panel A used to print A = 30.476 for
2024 where `tab3` printed 30.479. Fixed; Panel A now reproduces `tab3` exactly, and both are
drawn from the same estimation object. The correction moves the k = 0.15–0.30 rows and
Figure 3 in the third decimal. The full pipeline was re-run on 2026-08-17 and the tables in
`essay_I.tex` carry the corrected values, so no further action is outstanding here.

## 4. `4_plots.R` — figures

**Reads** `outputs/final/*` and `dt.RData`. **Writes** `outputs/plots/*.jpeg` — 20 files, of which
16 are written to literal paths and the four `k_sens_*.jpeg` panels are written through an
`sprintf()` loop at the end of the script (worth knowing: grepping the source for
`outputs/plots/…jpeg` will not find them). Together with the 9 maps from `1_spatial.R` that is
29 figures in `outputs/plots/`.

Uses `0_legend_helpers.R`, which generates `limits`/`breaks`/`labels` for the map colour scales
from the data rather than hand-typed values, so legends can't go stale.

Covers Figures 1–5 and A1. Note panel (d) of Figure 3 uses a coarser k grid (0.125, 0.25, …)
because under equal weights `cᵢ` can only take multiples of 1/8 — the caption now says so.

## 5. `5_climate_sensitivity.R` — climate-zone robustness

**Reads** `dt.RData` only; does **not** re-run 1–3. **Writes** `climate_sensitivity.RData`.

Recomputes `ach_clim → dp_clim` and the NB score under four zone definitions — Baseline,
Restrictive (+2 °C), Permissive (−2 °C), Monthly-extreme (peak monthly readings) — and reports
H, M₀, A plus population shares per zone. Ends with a sanity check that the Baseline variant
reproduces the main pipeline. Feeds Tables A7 and A8.

**Missing-value convention (fixed 2026-08-17).** `score_with_weather()` used to return
`mep_score = 0` where the score was `NA`, while `mep` was correctly `NA`. Under
`svymean(na.rm = TRUE)` that puts H and M₀ on different denominators, so `A = M₀/H` was biased:
the Baseline variant returned A = 30.476 for 2024 where the main pipeline returns 30.479, and
the script's own sanity check printed `dA = 0.003` without anyone acting on it. This is the same
defect that had already been fixed in `.ksens_one()` (script 3) and `.scorable()` (script 7) —
third instance of one bug. Both branches now yield `NA` on the same rows and the sanity check
closes to zero on H, M₀ *and* A. Only A was affected; the H and M₀ columns that Table A8 reports
never moved.

**Stored precision.** The results tibble now keeps 6 decimals rather than 3. Table A8 prints two,
and rounding 3 → 2 double-rounds anything sitting on a `x.xx5` boundary; Monthly-extreme 2018 H
and Permissive 2016 M₀ both did, and both were wrong in the second decimal in the paper.

## 6. `6_et_drop.R` — entertainment robustness

**Reads** `dt.RData`. **Writes** `et_sensitivity.RData`.

Drops Et and renormalizes over the remaining seven dimensions in a (3,2,2) configuration,
preserving the 1.538 tier ratio. Result: the headline M₀ reduction *strengthens* to 33.1%,
because Et is the only dimension moving the other way. Note the *level* of M₀ rises (10.456
in 2016 against 9.737), since Et carries the lowest deprivation rate of the Tier-2 group and
its weight is redistributed onto dimensions where deprivation is more common.

These estimates now feed **`tab:et_drop`**, a table of their own. Until August 2026 §3.1 cited
`tab:af_drop` for the 33.1% figure, but that table contains only the Af-drop columns — the
Et block had never been merged into it, so a headline robustness claim pointed at a table
that did not contain it.

`score_noet()` carried the same `mep_score = 0`-on-`NA` defect as script 5 and was fixed in the
same pass. It moved one published cell: A_noEt for 2024, 33.804 → 33.806.

## 7. `7_diagnostics.R` — indicator diagnostics, counterfactuals, descriptives

**Reads** `dt.RData`, `estimations_means.RData`, `estimations_inc.RData`, and the raw
`inputs/viviendas/` and `inputs/hogares/` modules.
**Writes** `outputs/final/diagnostics.RData`.

Added August 2026. This script exists because the referee-response pass introduced a batch of
numbers that were originally computed interactively and were therefore not reproducible from the
deposited code. Every one of them now lives here, in a numbered section tagged with the LaTeX
label that consumes it:

| § | Produces | Feeds |
|---|---|---|
| 1 | Realized sample sizes per wave | `tab:samples` |
| 2 | Income dispersion, bottom-quintile energy burden | §1 (Figure 1 discussion) |
| 3 | Cooking fuel category distribution | `tab2` coding note |
| 4 | Entertainment diagnostics + smartphone counterfactual | `tab:et_diag` Panel A |
| 5 | Water heating / cooking overlap, tetrachoric correlation | `tab:et_diag` Panel B |
| 6 | Two-tier counterfactual (Af on equal footing) | `tab:twotier` |
| 7 | Af conditional on the *income* line vs multidimensional poverty | `tab:af_circ` |
| 7b | Reverse conditional `P(income-poor \| Af)` and the climate-zone split of the Tier-3-only route | §4 |
| 8 | Population shares, gaps, ratios, subgroup Δ tests, eq. (12) check | `tab4`, `tab4b` |
| 9 | National Δ tests 2016–2018 | `tab:delta` Panel B |
| 10 | Dimensional change, pp vs relative | §3 |
| 11 | Convergence regression (ΔH on ICTPC centile) | §3, `fig:MEPIcen_plots` |
| 12 | Benzécri / Greenacre inertia adjustments | `tab:mca` note |
| 13 | State-level H, A, M₀ | `tab:states` |
| 14 | Water-heating specification sensitivity (dedicated heater only) | §3.1 — *runs, still not cited* |

Sections 7b and 14 were added in the August 2026 verification pass.

**§7b** produces two quantities the discussion previously asserted without computing. First,
`P(income-poor | Af-deprived)` — the reverse of the 100% column in `tab:af_circ` Panel A, and
the sharper form of the circularity objection: it runs 94.6–95.8%, so as a *population* Af is
very nearly a relabeling of income poverty. §4 now says this outright, because the defense of
the indicator is architectural (`w_Af < k`), not informational. Second, the Tier-3-only
identification share split by climate zone. The joint Af∧Tc route needs Tc to be *available*,
so it exists only where `weather ∈ {1, 3}`; in temperate municipalities `w_Af = 0.0786` and
every other available weight is ≥ 0.1210 > k, so affordability cannot move H at all there.
The script asserts `Tier3only_temperate == 0` as a check on that reasoning. The national
13–17% in Panel C is therefore an average over a population where the mechanism is
structurally absent for ~79% of individuals. Conditional on hot and cold municipalities the
share is 33.4–39.0%; §4 of the paper now quotes that range instead of saying "several times
larger", which it had asserted without ever computing.

**§14** is the water-heating counterpart of `6_et_drop.R`, and arguably the more consequential
of the two. Wt achievement is satisfied by `sh | gh | st ≥ 1`, the last being the MAEN stove
route; the paper reports a tetrachoric correlation of 0.83–0.86 with Ck and that 44–52% of Wt
achievers qualify through the stove route alone, but never showed what happens if it is
removed. §14 recomputes H, A and M₀ under `dp_wt = 1(no dedicated water heater)`, holding the
weight vector fixed. Wt carries a Tier-1 weight and Ck + Wt are ~45% of M₀ in 2024, so this is
a larger margin than the Et drop.

> **This block had never once executed successfully.** It was added on 2026-08-12 and re-read
> `viviendas` to construct `water_heat`, then `left_join()`ed it onto `dt_list`, which *already
> carries a `water_heat` column* — §5 of the same script reads it straight off the data frame.
> dplyr suffixed the pair to `water_heat.x` / `water_heat.y`, the bare name stopped resolving,
> and the script aborted at `filter(!is.na(water_heat))` with `object 'water_heat' not found` —
> *before* reaching `save()`. So from 2026-08-12 to 2026-08-17, `diagnostics.RData` on disk was
> the stale 2026-08-10 build, containing neither `wt_strict` nor `af_route`, and
> `8_extract_for_tex.R` failed on `object 'af_route' not found` whenever it was run against a
> fresh 7. Fixed 2026-08-17 by deleting the re-read and using the column that was already there.
> The lesson is the boring one: a script that ends in `save()` fails loudly, but only if someone
> runs it end to end.

Results, now that it runs (NB-main weights, k = 0.10):

| Year | Wt deprivation (strict) | H | M₀ | A |
|---|---|---|---|---|
| 2016 | 56.38 | 60.97 | 19.588 | 32.13 |
| 2018 | 55.34 | 59.85 | 19.347 | 32.33 |
| 2020 | 52.93 | 57.70 | 18.014 | 31.22 |
| 2022 | 52.60 | 57.41 | 17.395 | 30.30 |
| 2024 | 48.19 | 52.49 | 15.303 | 29.16 |

The trajectory survives — M₀ falls monotonically — but the relative reduction is **−21.9%**
against the −29.1% headline, a materially weaker result than either the Af drop (−27.6%) or the
Et drop (−33.1%). Levels roughly double, since removing the stove route makes Wt the single most
common deprivation in the index. **Still not cited in the paper.** That is now an authorial
decision rather than a missing number: the honest reading is that the headline decline is
partly carried by the MAEN stove convention, and §3.1 already concedes that Ck and Wt are
"two partially overlapping readings of the same underlying fuel-and-appliance constraint".

Two self-contained implementations worth noting, both written to avoid extra dependencies:
`wtd_quantile()` (weighted inverse-CDF quantile, replaces `Hmisc::wtd.quantile`) and
`tetrachoric()`, which solves for the correlation of the latent bivariate normal that reproduces
the observed weighted 2×2 table. Section 8 also verifies numerically that the subgroup
decomposition of equation (12) closes to the national M₀ for every partition — it does, to four
decimal places, except for the Indigenous partition where 3.2% of individuals have missing
language data (documented in the note to `tab4b`).

## 8. `8_extract_for_tex.R` — the code-to-text bridge

**Reads** every object in `outputs/final/` plus the weight vectors and MCA diagnostics in
`dt.RData`. **Writes** nothing; prints to stdout.

This is the final step and the audit trail. It prints every numeric value that appears in
`essay_I.tex`, grouped under the LaTeX label of the table or figure that consumes it. Run it after
any change to scripts 1–7 and diff the output against the previous version to see exactly which
table cells move.

## Helper scripts

- `0_legend_helpers.R` — generates `limits`, `breaks` and `labels` for `scale_*_gradientn()` from
  the data, so map legends cannot go stale. `auto_legend()`, `fmt_pp()`, `fmt_pct()`,
  `fmt_growth()` and `fmt_money()` are all in use. It is **sourced twice by `4_plots.R`**, and
  this is deliberate rather than duplication: `4_plots.R` calls `rm(list = ls())` between
  blocks, and the source before the map block is the one that matters, since every helper call
  in the file sits below it. A comment at that line now says so. `fmt_C()` and `fmt_pct_pp()`
  were dead code and were removed in the August 2026 cleanup — `1_spatial.R` still hand-codes
  its temperature legends rather than sourcing this file, which remains the one place the
  helper is not applied consistently.
- `stations.R` — **removed August 2026.** It plotted station-level warm-day (`SU25_P`) and
  frost-day (`FDO_P`) counts, but could not run standalone: it depended on `mun` and on the
  packages loaded by `1_spatial.R`, and its two figures are not cited in the paper. Its content
  now lives in `1_spatial.R` under *"Station-level warm/cold day counts"*, guarded by
  `MAKE_STATION_DAY_MAPS`, so it is reproducible as part of the pipeline. The two figures remain
  uncited — either cite them as supporting evidence for the climate thresholds or set the flag to
  `FALSE`.

---

## Where the paper's numbers come from

Table numbers shifted in the August 2026 revision (a sample-size table was added at the front of
§2.1 and four new tables elsewhere), so this maps by **LaTeX label** rather than by number.
Order of appearance is given for orientation.

| # | Label | Produced by | Object |
|---|---|---|---|
| 1 | `tab:samples` | `7_diagnostics.R` §1 | `sample_sizes` |
| 2 | `tab1` | `2_data.R` | `w_j`, `w_j_7dim` |
| 3 | `tab2` | `2_data.R` (definitions); `7_diagnostics.R` §3 (fuel note) | `fuel_distribution` |
| 4 | `tab3` | `3_estimations.R` | `results_df` |
| 5 | `tab:robustness` | `3_estimations.R` | `robustness` |
| 6 | `tab:dim_contributions` | `3_estimations.R` | `final_output` |
| 7 | `tab4` | `3_estimations.R` + `7_diagnostics.R` §8 | `results_df`, `subgroup_deltas` |
| 8 | `tab4b` | `3_estimations.R` + `7_diagnostics.R` §8 | `population_shares`, `subgroup_gaps` |
| 9 | `tab:ksens_headline` | `3_estimations.R` | `k_sensitivity` |
| 10 | `tab:et_diag` | `7_diagnostics.R` §4–5 | `et_diagnostics`, `wt_ck_overlap` |
| A1 | `tab3_1` | `3_estimations.R` | `results_df_t` |
| A2 | `tab:mca` | `2_data.R` + `7_diagnostics.R` §12 | `mca_diagnostics`, `mca_adjustment` |
| A3 | `tab:delta` | `3_estimations.R` + `7_diagnostics.R` §9 | `delta_national`, `delta_2016_2018` |
| A4 | `tab:af_drop` | `3_estimations.R` | `af_drop` |
| A5 | `tab:et_drop` | `6_et_drop.R` | `et_drop_full` |
| A6 | `tab:af_circ` | `3_estimations.R` + `7_diagnostics.R` §7 | `af_circ`, `af_conditioning` |
| A7 | `tab:climate_pop` | `5_climate_sensitivity.R` | `results` |
| A8 | `tab:climate_HM` | `5_climate_sensitivity.R` | `results` |
| A9 | `tab:twotier` | `7_diagnostics.R` §6 | `twotier` |
| A10 | `tab:states` | `7_diagnostics.R` §13 | `state_estimates` |

Not consumed by any table, but cited in the running text: `7_diagnostics.R` §7b (§4 of the
paper) and §14 (produced, not yet cited — see above).

**Appendix table numbers shifted by one from A5 onward** when `tab:et_drop` was inserted in
August 2026. This is exactly why the mapping is keyed on labels.

## Repository checklist

Before depositing the replication package:

- [x] Every value in `essay_I.tex` produced by a numbered script (closed by `7_diagnostics.R`).
- [x] No orphan scripts (`stations.R` folded into `1_spatial.R`).
- [x] Code-to-text mapping documented and machine-checkable (`8_extract_for_tex.R`).
- [x] `README.md` with run order, data sources, expected `inputs/` layout and package list.
- [x] `.gitignore`: `inputs/` and `outputs/intermediate/` excluded (`dt.RData` is 79 MB, over
      GitHub's 50 MB warning threshold, and fully reproducible from `2_data.R`);
      `outputs/final/*.RData` *is* tracked, at ~268 KB in total, so a reader can verify any
      number in the paper without re-running the pipeline. (Earlier drafts said "under 200 KB";
      the estimation objects have grown since.)
- [x] **Full re-run of `1` → `8` on 2026-08-17.** Every script executed cleanly from the raw
      inputs, and every table in `essay_I.tex` was reconciled against the resulting
      `values_for_tex.txt`. The two missing-value fixes (see §3 and §7) are in the deposited
      numbers.
- [x] Package versions recorded. `renv.lock` still does not exist; the versions the results were
      produced under are tabulated in README §1 instead. Generating a real `renv.lock`
      (`renv::init(); renv::snapshot()`) is still the better end state.
- [ ] Fill in the repository URL and Zenodo DOI placeholders in the Data and Code Availability
      statement of `essay_I.tex`, and the Funding and Acknowledgements placeholders.
- [ ] Choose a license (README §8) and add `CITATION.cff`.
- [ ] `references.bib` currently lives only in Overleaf — bring it into the repo so the paper
      source is complete. Without it `\addbibresource{references.bib}` does not resolve and the
      source as deposited will not build. **This is the one blocker left.**
- [ ] Housekeeping in the working folder (all of it already `.gitignore`d, so this is tidiness
      rather than a blocker): `.Rhistory` and 13 `.DS_Store` files. Keep `.Rproj.user/` and
      `essay_I.Rproj` locally — the first is live RStudio state, the second is useful.
      `essay_I.log` and `coarse.md`, listed here in earlier drafts, are already gone.

## House rules

- **Do not compile the PDF locally** — TinyTeX here is minimal and has mirror/checksum issues.
  Compile in Overleaf.
- After any change to scripts 1–7, regenerate the dump and diff it before touching the `.tex`:

  ```sh
  Rscript 8_extract_for_tex.R > outputs/final/values_for_tex.new
  diff outputs/final/values_for_tex.txt outputs/final/values_for_tex.new
  ```

  `values_for_tex.txt` is `.gitignore`d (it is derived from the tracked `.RData` files), but
  keeping the current copy on disk is what makes the diff possible.
- `references.bib` lives in Overleaf, not in this folder. There is no `BIB_FIXES.md`: earlier
  versions of this file referred to one as the staging place for bibliography corrections, but
  no such file was ever created here. Either create it when the first correction arises, or
  make corrections directly in Overleaf and bring `references.bib` into the repo (see the
  checklist).
- Table footnote markers use `$^a$`, `$^b$`, `$^c$`. Asterisks are reserved for statistical
  significance on Δ values.
