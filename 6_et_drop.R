#*** Entertainment-drop sensitivity (Section 4 / appendix) ***
#*** Matías Carrasco-Jiménez **
#
# Reviewer concern: the entertainment indicator (Et) is the only deprivation
# rate that rises across 2016–2024 (3.92% → 5.59%), with its share of M0 nearly
# doubling. Possibly a coding artifact from the ENIGH 2020 durable-goods module
# redesign. This script recomputes H, M0, A under an NB-noEt scheme that drops
# Et entirely and renormalizes Tier-1/Tier-2/Tier-3 weights over the remaining
# seven dimensions, keeping the same w_1/w_2 = w_2/w_3 = 0.20/0.13 ratio.

rm(list = ls())
options(scipen = 999)
options(survey.lonely.psu = "adjust")

if (!require(pacman)) install.packages("pacman")
p_load("dplyr", "tidyr", "purrr", "stringr", "survey", "srvyr", "data.table",
       "xtable")

load("outputs/intermediate/dt.RData")
years <- c(2016, 2018, 2020, 2022, 2024)
k <- 0.10

# NB-noEt weights -------------------------------------------------------------
# Configuration: 3 Tier-1 (Ea, Ck, Wt) + 2 Tier-2 (Rf, Cm) + 2 Tier-3 (Tc, Af)
# Solve 3 w_1 + 2 w_2 + 2 w_3 = 1 with w_1/w_2 = w_2/w_3 = 1.538:
nb_ratio <- 0.20 / 0.13
nb_noet_w2 <- 1 / (3 * nb_ratio + 2 + 2 / nb_ratio)   # ≈ 0.1263
nb_noet_w1 <- nb_ratio * nb_noet_w2                    # ≈ 0.1943
nb_noet_w3 <- nb_noet_w2 / nb_ratio                    # ≈ 0.0821

w_j_noet <- c(
  dp_light = nb_noet_w1, dp_cook  = nb_noet_w1, dp_wt    = nb_noet_w1,
  dp_refri = nb_noet_w2, dp_com   = nb_noet_w2,
  dp_entr  = 0,                                  # DROPPED
  dp_hill  = nb_noet_w3, dp_clim  = nb_noet_w3
)
w_j_noet <- w_j_noet / sum(w_j_noet)

# Temperate (no Tc, no Et): 3 + 2 + 1 = 6 dims
nb_noet_t_w2 <- 1 / (3 * nb_ratio + 2 + 1 / nb_ratio)
nb_noet_t_w1 <- nb_ratio * nb_noet_t_w2
nb_noet_t_w3 <- nb_noet_t_w2 / nb_ratio
w_j_noet_7 <- c(
  dp_light = nb_noet_t_w1, dp_cook = nb_noet_t_w1, dp_wt = nb_noet_t_w1,
  dp_refri = nb_noet_t_w2, dp_com  = nb_noet_t_w2,
  dp_entr  = 0,
  dp_hill  = nb_noet_t_w3
)
w_j_noet_7 <- w_j_noet_7 / sum(w_j_noet_7)

cat("\nNB-noEt weights (Et dropped):\n")
print(round(sort(w_j_noet, decreasing = TRUE), 4))
cat("\nNB-noEt weights, temperate:\n")
print(round(sort(w_j_noet_7, decreasing = TRUE), 4))

# Recompute scores without Et -------------------------------------------------
score_noet <- function(dt, w8, w7, k_val) {
  score <- case_when(
    dt$weather %in% c(1L, 3L) ~
      dt$dp_hill  * w8["dp_hill"]  + dt$dp_light * w8["dp_light"] +
      dt$dp_cook  * w8["dp_cook"]  + dt$dp_refri * w8["dp_refri"] +
      dt$dp_com   * w8["dp_com"]   + dt$dp_wt    * w8["dp_wt"]    +
      dt$dp_clim  * w8["dp_clim"],
    dt$weather == 2L ~
      dt$dp_hill  * w7["dp_hill"]  + dt$dp_light * w7["dp_light"] +
      dt$dp_cook  * w7["dp_cook"]  + dt$dp_refri * w7["dp_refri"] +
      dt$dp_com   * w7["dp_com"]   + dt$dp_wt    * w7["dp_wt"],
    TRUE ~ NA_real_
  )
  # mep and mep_score must be NA on the same rows -- see the note in
  # 5_climate_sensitivity.R. Setting mep_score = 0 on an NA score puts H and M0
  # on different denominators under svymean(na.rm = TRUE), which biases A.
  list(
    mep       = ifelse(is.na(score), NA_real_, as.numeric(score >= k_val)),
    mep_score = ifelse(is.na(score), NA_real_,
                       ifelse(score >= k_val, score, 0))
  )
}

# Estimation per wave ---------------------------------------------------------

et_drop <- map_dfr(years, function(i) {
  yr <- paste0("dt", i)
  dt <- dt_list[[yr]]
  sc <- score_noet(dt, w_j_noet, w_j_noet_7, k)

  dt2 <- dt %>%
    mutate(
      mep_noet       = sc$mep,
      mep_score_noet = sc$mep_score,
      upm     = as.factor(upm),
      est_dis = as.factor(est_dis),
      factor  = as.numeric(factor)
    )

  svy <- svydesign(ids = ~upm, strata = ~est_dis, weights = ~factor,
                   data = dt2, nest = TRUE)

  m_H  <- svymean(~mep_noet,       svy, na.rm = TRUE)
  m_M0 <- svymean(~mep_score_noet, svy, na.rm = TRUE)

  H_v   <- coef(m_H)[1]
  M0_v  <- coef(m_M0)[1]

  tibble(
    year   = i,
    H_noet  = round(100 * H_v, 3),
    se_H_noet  = round(100 * as.numeric(SE(m_H)[1]), 3),
    M0_noet = round(100 * M0_v, 3),
    se_M0_noet = round(100 * as.numeric(SE(m_M0)[1]), 3),
    A_noet  = round(ifelse(H_v > 0, 100 * M0_v / H_v, NA_real_), 3)
  )
})

cat("\nNB-noEt sensitivity:\n")
print(as.data.frame(et_drop))

# Combine with main NB estimates for the appendix table
load("outputs/final/robustness.RData")
et_drop_full <- robustness %>%
  select(year, H, M0, A) %>%
  left_join(et_drop, by = "year")

cat("\nNB main vs NB-noEt:\n")
print(as.data.frame(et_drop_full))

# Relative reduction summary
cat("\nRelative reduction 2016 → 2024:\n")
cat(sprintf("  NB main : M0 %.3f → %.3f (%.1f%%)\n",
            et_drop_full$M0[1], et_drop_full$M0[5],
            100 * (et_drop_full$M0[5] / et_drop_full$M0[1] - 1)))
cat(sprintf("  NB-noEt : M0 %.3f → %.3f (%.1f%%)\n",
            et_drop_full$M0_noet[1], et_drop_full$M0_noet[5],
            100 * (et_drop_full$M0_noet[5] / et_drop_full$M0_noet[1] - 1)))

save(et_drop, et_drop_full, file = "outputs/final/et_sensitivity.RData")
