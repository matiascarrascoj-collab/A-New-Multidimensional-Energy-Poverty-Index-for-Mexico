#*** Code-to-text bridge: every numeric value that appears in essay_I.tex ***
#*** Matías Carrasco-Jiménez **
#
# Reads the final RData objects produced by scripts 1-7 and prints a structured
# dump, grouped by the LaTeX label of the table or figure that consumes each
# block. This is the last step of the pipeline: nothing in essay_I.tex should
# contain a number that does not appear in this dump.
#
# Run after 1-7. Writes nothing; prints to stdout. To capture:
#   Rscript 9_extract_for_tex.R > outputs/final/values_for_tex.txt

rm(list = ls())
options(scipen = 999, width = 200)
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr)
})

hdr <- function(label, what) {
  cat("\n\n", strrep("=", 78), "\n", sep = "")
  cat(sprintf("  %-22s %s\n", label, what))
  cat(strrep("=", 78), "\n", sep = "")
}

# ---------------------------------------------------------------------------
# From 2_data.R
# ---------------------------------------------------------------------------

load("outputs/intermediate/dt.RData")

hdr("tab1 / §2.2", "NB weighting scheme (main)")
cat("Hot / cold municipalities (8 dimensions):\n")
print(round(sort(w_j, decreasing = TRUE), 4))
cat("\nTemperate municipalities (7 dimensions, Tc unavailable):\n")
print(round(sort(w_j_7dim, decreasing = TRUE), 4))
cat("\nAlternative schemes (robustness):\n")
cat(" MCA:\n");       print(round(sort(w_j_mca,  decreasing = TRUE), 4))
cat(" Equal:\n");     print(round(sort(w_j_eq,   decreasing = TRUE), 4))
cat(" Frequency:\n"); print(round(sort(w_j_freq, decreasing = TRUE), 4))
cat(" NB-noAf:\n");   print(round(sort(w_j_noaf, decreasing = TRUE), 4))

hdr("tab:mca Panel A/B", "MCA eigenvalues and Axis-1 loadings")
print(mca_diagnostics$eig)
cat("\nLoadings (axes 1-2):\n")
print(mca_diagnostics$loadings)

rm(list = setdiff(ls(), c("hdr")))

# ---------------------------------------------------------------------------
# From 3_estimations.R
# ---------------------------------------------------------------------------

load("outputs/final/estimations_means.RData")

hdr("tab3", "National means: H, A, M0 and dimensional deprivation rates")
print(as.data.frame(
  results_df %>% filter(subgroup == "national") %>% arrange(year) %>%
    select(year, H, se_H, A, se_A, M0, se_M0,
           starts_with("dp_"), starts_with("se_dp_")) %>%
    mutate(across(-year, ~ round(.x * 100, 3)))
))

hdr("tab4", "Subgroups: H and A (incl. elderly panel)")
print(as.data.frame(
  results_df %>%
    filter(!(subgroup %in% c("national", "ent_name")), year %in% c(2016, 2024)) %>%
    arrange(subgroup, term, year) %>%
    select(subgroup, term, year, H, se_H, A, se_A) %>%
    mutate(across(c(H, se_H, A, se_A), ~ round(.x * 100, 3)))
))

hdr("tab4b", "Subgroups: M0 by subgroup")
print(as.data.frame(
  results_df %>%
    filter(!(subgroup %in% c("national", "ent_name")), year %in% c(2016, 2024)) %>%
    arrange(subgroup, term, year) %>%
    select(subgroup, term, year, M0, se_M0) %>%
    mutate(across(c(M0, se_M0), ~ round(.x * 100, 3)))
))

load("outputs/final/estimations_totals.RData")
hdr("tab3_1", "National totals (millions of individuals)")
print(as.data.frame(
  results_df_t %>% arrange(year) %>%
    select(year, mep, se_mep, dp_light, se_dp_light, dp_cook, se_dp_cook,
           dp_refri, se_dp_refri, dp_entr, se_dp_entr, dp_com, se_dp_com,
           dp_wt, se_dp_wt, dp_hill, se_dp_hill, dp_clim, se_dp_clim) %>%
    mutate(across(-year, ~ round(.x / 1e6, 3)))
))

load("outputs/final/robustness.RData")
hdr("tab:robustness", "Four weighting schemes: H and M0")
print(as.data.frame(robustness))
cat("\nRelative change in M0, 2016-2024, by scheme (%):\n")
rel <- function(x) round(100 * (x[5] - x[1]) / x[1], 1)
cat(sprintf("  NB %.1f | MCA %.1f | Equal %.1f | Frequency %.1f\n",
            rel(robustness$M0), rel(robustness$M0_mca),
            rel(robustness$M0_eq), rel(robustness$M0_freq)))

hdr("tab:delta Panel A/C", "Delta tests 2016-2024 (national)")
print(as.data.frame(delta_national))

hdr("tab:af_drop", "NB main vs NB-noAf")
print(as.data.frame(af_drop))

load("outputs/final/dimensional_decomposition.RData")
hdr("tab:dim_contributions", "Dimensional contributions to M0")
print(as.data.frame(final_output))

load("outputs/final/k_sensitivity.RData")
hdr("fig:ksens / tab:ksens_headline", "k sensitivity, NB weights")
print(as.data.frame(k_sensitivity %>% filter(k %in% c(0.10, 0.30))))
cat("\nFull NB grid:\n"); print(as.data.frame(k_sensitivity))

load("outputs/final/af_circularity.RData")
hdr("tab:af_circ (multidim. panel)", "Af conditional on CONEVAL multidim. poverty")
print(as.data.frame(af_circ))

# ---------------------------------------------------------------------------
# From 5_climate_sensitivity.R and 6_et_drop.R
# ---------------------------------------------------------------------------

load("outputs/final/climate_sensitivity.RData")
hdr("tab:climate_pop / tab:climate_HM", "Climate-zone sensitivity")
print(as.data.frame(results))

load("outputs/final/et_sensitivity.RData")
hdr("tab:et_drop", "NB main vs NB-noEt (entertainment dropped)")
print(as.data.frame(et_drop_full))
cat("\nRelative change in M0, 2016-2024:\n")
cat(sprintf("  NB main %.1f%% | NB-noEt %.1f%%\n",
            100 * (et_drop_full$M0[5]      / et_drop_full$M0[1]      - 1),
            100 * (et_drop_full$M0_noet[5] / et_drop_full$M0_noet[1] - 1)))

# ---------------------------------------------------------------------------
# From 7_diagnostics.R
# ---------------------------------------------------------------------------

load("outputs/final/diagnostics.RData")

hdr("tab:samples", "ENIGH realized sample by wave")
print(as.data.frame(sample_sizes))

hdr("§1 (Figure 1 discussion)", "Income dispersion and bottom-quintile energy burden")
print(as.data.frame(income_dispersion))

hdr("tab2 coding note", "Cooking fuel category distribution")
print(as.data.frame(fuel_distribution))
print(as.data.frame(fuel_6_7))

hdr("tab:et_diag Panel A", "Entertainment indicator diagnostics")
print(as.data.frame(et_diagnostics))

hdr("tab:et_diag Panel B", "Water heating / cooking overlap")
print(as.data.frame(wt_ck_overlap))

hdr("tab:twotier", "Two-tier counterfactual (Af on equal footing)")
print(as.data.frame(twotier))
cat("\nTwo-tier weights:\n")
print(round(w_twotier, 5)); print(round(w_twotier_7, 5))

hdr("tab:af_circ (income panel)", "Af conditional on the income poverty line")
print(as.data.frame(af_conditioning))

hdr("tab4b", "Population shares, gaps and ratios")
print(as.data.frame(population_shares %>% filter(year %in% c(2016, 2024))))
print(as.data.frame(subgroup_gaps))

hdr("tab4 (delta columns)", "Subgroup delta tests")
print(as.data.frame(subgroup_deltas %>%
  select(subgroup, term, delta_H, se_delta_H, z_H, sig_H,
         delta_A, se_delta_A, z_A, sig_A)))

hdr("eq:decomp check", "Subgroup decomposition closes to national M0")
print(as.data.frame(decomposition_check))

hdr("tab:delta Panel B", "National delta tests 2016-2018")
print(as.data.frame(delta_2016_2018))

hdr("§3", "Dimensional change: percentage points vs relative")
print(as.data.frame(dimensional_change))

hdr("§3 / fig:MEPIcen_plots", "Convergence regression")
print(as.data.frame(convergence_regression))

hdr("tab:mca note", "Benzecri / Greenacre inertia adjustments")
print(as.data.frame(mca_adjustment))

hdr("tab:states", "State-level H, A, M0")
print(as.data.frame(state_estimates))

hdr("\u00a74 (Discussion)", "Af: reverse conditional and climate location of the Tier-3 route")
print(as.data.frame(af_route))
cat("\n  P_incomepoor_given_Af is the sharp form of the circularity objection:\n")
cat("  the share of the Af-deprived who are also income-poor, quoted in \u00a74.\n")
cat("  Tier3only_temperate must be 0 -- Af cannot move H where Tc is unavailable.\n")

# ---------------------------------------------------------------------------
# From 8_ckwt_merge.R
# ---------------------------------------------------------------------------

load("outputs/final/ckwt_merge.RData")
hdr("tab:ckwt_merge", "NB main vs merged cooking/water-heating dimension")
print(as.data.frame(ckwt_full))
cat("\nMerged-dimension weights, hot/cold (2,3,2) and temperate (2,3,1):\n")
print(round(w_merge, 4)); print(round(w_merge_t, 4))
cat("\nFa deprivation rate and contribution to M0 (union rule):\n")
print(as.data.frame(fa_contrib))
cat("\nRelative change in M0, 2016-2024:\n")
cat(sprintf("  NB main %.1f%% | merged-union %.1f%% | merged-intersection %.1f%%\n",
            100 * (ckwt_full$M0[5]       / ckwt_full$M0[1]       - 1),
            100 * (ckwt_full$M0_merge[5] / ckwt_full$M0_merge[1] - 1),
            100 * (ckwt_full$M0_int[5]   / ckwt_full$M0_int[1]   - 1)))
cat("  H is identical to the NB main column under the union rule -- see the\n")
cat("  note to tab:ckwt_merge; both schemes share an identification set.\n")

hdr("\u00a73.1 (not yet cited)", "Water-heating sensitivity: dedicated heater only")
print(as.data.frame(wt_strict))
cat("\n  Produced by 7_diagnostics.R \u00a714. No table in essay_I.tex consumes this yet;\n")
cat("  see PIPELINE.md. Compare the relative M0 trajectory against the NB main -29.1%.\n")

cat("\n\nDone. All values in essay_I.tex should be traceable to a block above.\n")
