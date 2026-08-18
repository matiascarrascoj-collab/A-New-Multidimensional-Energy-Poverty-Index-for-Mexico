#*** Cooking / water-heating merge sensitivity (Section 3.1 / appendix) ***
#*** Matías Carrasco-Jiménez **
#
# Referee concern (August 2026 review notes, item B.2). Ck and Wt have a
# tetrachoric correlation of 0.83-0.86 and jointly carry two Tier-1 weights and
# 44.9% of M0 in 2024, yet the paper removes Et (6_et_drop.R) and Af
# (NB-noAf, in 3_estimations.R) without ever testing the pair that overlaps
# most. Section 3.1 concedes that Ck and Wt are "two partially overlapping
# readings of the same underlying fuel-and-appliance constraint" -- this script
# tests what happens if they are treated as one.
#
# Specification. Ck and Wt collapse into a single fuel-and-appliance dimension
# (Fa). Tier 1 becomes {Ea, Fa}, Tier 2 stays {Rf, Et, Cm}, Tier 3 stays
# {Tc, Af}: a (2,3,2) configuration at the same adjacent-tier ratio
# w1/w2 = w2/w3 = 0.20/0.13. Two merge rules are reported:
#
#   Union        dp_fa = 1 if deprived in Ck OR Wt   (the headline rule: the
#                household lacks the fuel-and-appliance bundle, counted once
#                instead of twice)
#   Intersection dp_fa = 1 if deprived in Ck AND Wt  (reported as a bound)
#
# Reads : outputs/intermediate/dt.RData, outputs/final/robustness.RData
# Writes: outputs/final/ckwt_merge.RData

rm(list = ls())
options(scipen = 999)
options(survey.lonely.psu = "adjust")

if (!require(pacman)) install.packages("pacman")
p_load("dplyr", "tidyr", "purrr", "stringr", "survey", "srvyr", "data.table",
       "xtable")

load("outputs/intermediate/dt.RData")
years <- c(2016, 2018, 2020, 2022, 2024)
k <- 0.10

# Weights ---------------------------------------------------------------------
# Hot / cold: 2 Tier-1 (Ea, Fa) + 3 Tier-2 (Rf, Et, Cm) + 2 Tier-3 (Tc, Af)
#   solve 2 w1 + 3 w2 + 2 w3 = 1 with w1/w2 = w2/w3 = 1.538
nb_ratio <- 0.20 / 0.13

m_w2 <- 1 / (2 * nb_ratio + 3 + 2 / nb_ratio)
m_w1 <- nb_ratio * m_w2
m_w3 <- m_w2 / nb_ratio

# Temperate (Tc unavailable): 2 + 3 + 1
t_w2 <- 1 / (2 * nb_ratio + 3 + 1 / nb_ratio)
t_w1 <- nb_ratio * t_w2
t_w3 <- t_w2 / nb_ratio

w_merge <- c(dp_light = m_w1, dp_fa = m_w1,
             dp_refri = m_w2, dp_entr = m_w2, dp_com = m_w2,
             dp_hill  = m_w3, dp_clim = m_w3)
w_merge_t <- c(dp_light = t_w1, dp_fa = t_w1,
               dp_refri = t_w2, dp_entr = t_w2, dp_com = t_w2,
               dp_hill  = t_w3)

cat("\nMerged-dimension weights, hot/cold (2,3,2):\n")
print(round(sort(w_merge, decreasing = TRUE), 4))
cat(sprintf("  sum = %.6f\n", sum(w_merge)))
cat("\nMerged-dimension weights, temperate (2,3,1):\n")
print(round(sort(w_merge_t, decreasing = TRUE), 4))
cat(sprintf("  sum = %.6f\n", sum(w_merge_t)))

# The identification logic of the main scheme must be preserved for the
# comparison to be about the merge and not about a change of cutoff: Tier 3
# stays below k and Tier 2 stays above it, in both availability classes.
stopifnot(m_w3 < k, t_w3 < k, m_w2 > k, t_w2 > k)
cat(sprintf("\nTier-3 weights below k = %.2f: hot/cold %.4f, temperate %.4f -- OK\n",
            k, m_w3, t_w3))
cat(sprintf("Tier-2 weights above k:        hot/cold %.4f, temperate %.4f -- OK\n",
            m_w2, t_w2))

# Scoring ---------------------------------------------------------------------
# Missing-value convention, as in .add_mep() (2_data.R) and the corrected
# scripts 5 and 6: an NA on any *available* dimension propagates to the score,
# so mep and mep_score are NA on the same rows and svymean(na.rm = TRUE)
# computes H and M0 over the same denominator.
score_merge <- function(dt, rule, w7, w6, k_val) {
  dp_fa <- if (rule == "union") {
    ifelse(is.na(dt$dp_cook) | is.na(dt$dp_wt), NA_real_,
           pmax(dt$dp_cook, dt$dp_wt))
  } else {
    ifelse(is.na(dt$dp_cook) | is.na(dt$dp_wt), NA_real_,
           pmin(dt$dp_cook, dt$dp_wt))
  }

  score <- case_when(
    dt$weather %in% c(1L, 3L) ~
      dt$dp_light * w7["dp_light"] + dp_fa        * w7["dp_fa"]    +
      dt$dp_refri * w7["dp_refri"] + dt$dp_entr   * w7["dp_entr"]  +
      dt$dp_com   * w7["dp_com"]   + dt$dp_hill   * w7["dp_hill"]  +
      dt$dp_clim  * w7["dp_clim"],
    dt$weather == 2L ~
      dt$dp_light * w6["dp_light"] + dp_fa        * w6["dp_fa"]    +
      dt$dp_refri * w6["dp_refri"] + dt$dp_entr   * w6["dp_entr"]  +
      dt$dp_com   * w6["dp_com"]   + dt$dp_hill   * w6["dp_hill"],
    TRUE ~ NA_real_
  )

  list(
    dp_fa     = dp_fa,
    mep       = ifelse(is.na(score), NA_real_, as.numeric(score >= k_val)),
    mep_score = ifelse(is.na(score), NA_real_,
                       ifelse(score >= k_val, score, 0))
  )
}

# Estimation ------------------------------------------------------------------

estimate_rule <- function(rule) {
  map_dfr(years, function(i) {
    dt <- dt_list[[paste0("dt", i)]]
    sc <- score_merge(dt, rule, w_merge, w_merge_t, k)

    dt2 <- dt %>%
      mutate(dp_fa_v      = sc$dp_fa,
             mep_m        = sc$mep,
             mep_score_m  = sc$mep_score,
             upm     = as.factor(upm),
             est_dis = as.factor(est_dis),
             factor  = as.numeric(factor))

    svy <- svydesign(ids = ~upm, strata = ~est_dis, weights = ~factor,
                     data = dt2, nest = TRUE)

    m_H  <- svymean(~mep_m,       svy, na.rm = TRUE)
    m_M0 <- svymean(~mep_score_m, svy, na.rm = TRUE)
    m_fa <- svymean(~dp_fa_v,     svy, na.rm = TRUE)

    H_v <- coef(m_H)[1]; M0_v <- coef(m_M0)[1]

    tibble(
      rule       = rule,
      year       = i,
      dp_fa_rate = 100 * coef(m_fa)[1],
      se_dp_fa   = 100 * as.numeric(SE(m_fa)[1]),
      H          = 100 * H_v,
      se_H       = 100 * as.numeric(SE(m_H)[1]),
      M0         = 100 * M0_v,
      se_M0      = 100 * as.numeric(SE(m_M0)[1]),
      A          = ifelse(H_v > 0, 100 * M0_v / H_v, NA_real_)
    )
  })
}

ckwt_merge <- bind_rows(estimate_rule("union"), estimate_rule("intersection"))

cat("\nMerged Ck-Wt dimension, both rules:\n")
print(as.data.frame(ckwt_merge), digits = 5)

# Contribution of the merged dimension to M0 ----------------------------------
# delta_Fa, computed the same way as the delta_j of tab:dim_contributions:
# the censored weighted deprivation, averaged over the population.

fa_contrib <- map_dfr(years, function(i) {
  dt <- dt_list[[paste0("dt", i)]]
  sc <- score_merge(dt, "union", w_merge, w_merge_t, k)
  hotcold <- dt$weather %in% c(1L, 3L)
  w_fa <- ifelse(hotcold, w_merge["dp_fa"], w_merge_t["dp_fa"])
  poor <- sc$mep

  dt2 <- dt %>%
    mutate(contrib_fa = ifelse(is.na(sc$mep_score), NA_real_,
                               ifelse(is.na(sc$dp_fa), 0, sc$dp_fa) * w_fa * poor),
           mep_score_m = sc$mep_score,
           upm = as.factor(upm), est_dis = as.factor(est_dis),
           factor = as.numeric(factor))

  svy <- svydesign(ids = ~upm, strata = ~est_dis, weights = ~factor,
                   data = dt2, nest = TRUE)
  d_fa <- coef(svymean(~contrib_fa,  svy, na.rm = TRUE))[1]
  M0_m <- coef(svymean(~mep_score_m, svy, na.rm = TRUE))[1]

  tibble(year = i,
         delta_Fa     = 100 * d_fa,
         relative_Fa  = 100 * d_fa / M0_m)
})

cat("\nContribution of the merged Fa dimension to M0 (union rule):\n")
print(as.data.frame(fa_contrib), digits = 5)

# Comparison with the main scheme ---------------------------------------------

load("outputs/final/robustness.RData")
main <- robustness %>% select(year, H, M0, A)

ckwt_full <- main %>%
  left_join(ckwt_merge %>% filter(rule == "union") %>%
              select(year, H_merge = H, se_H_merge = se_H,
                     M0_merge = M0, se_M0_merge = se_M0, A_merge = A),
            by = "year") %>%
  left_join(ckwt_merge %>% filter(rule == "intersection") %>%
              select(year, H_int = H, M0_int = M0, A_int = A),
            by = "year")

cat("\nNB main vs merged Ck-Wt:\n")
print(as.data.frame(ckwt_full), digits = 5)

rel <- function(x) 100 * (x[5] / x[1] - 1)
cat("\nRelative change in M0, 2016 -> 2024:\n")
cat(sprintf("  NB main            : %.3f -> %.3f  (%+.1f%%)\n",
            ckwt_full$M0[1], ckwt_full$M0[5], rel(ckwt_full$M0)))
cat(sprintf("  Merged (union)     : %.3f -> %.3f  (%+.1f%%)\n",
            ckwt_full$M0_merge[1], ckwt_full$M0_merge[5], rel(ckwt_full$M0_merge)))
cat(sprintf("  Merged (intersect) : %.3f -> %.3f  (%+.1f%%)\n",
            ckwt_full$M0_int[1], ckwt_full$M0_int[5], rel(ckwt_full$M0_int)))
cat(sprintf("\n  H  relative change: main %+.1f%%, merged %+.1f%%\n",
            rel(ckwt_full$H), rel(ckwt_full$H_merge)))

save(ckwt_merge, ckwt_full, fa_contrib, w_merge, w_merge_t,
     file = "outputs/final/ckwt_merge.RData")

cat("\nWritten: outputs/final/ckwt_merge.RData\n")
