#*** Climate-zone sensitivity (Section 4 / appendix) **
#
# Recomputes H, M0, A under four alternative climate-zone classifications.
# Does NOT re-run 2_data.R/3_estimations.R — operates directly on dt.RData.
#
# Variants:
#   - Baseline       : tmax_annual≥30 & avg_temp≥24 (hot); tmin_annual≤10 & avg_temp≤12 (cold)
#   - Restrictive    : +2°C shift on both legs (harder to qualify; more temperate)
#   - Permissive     : −2°C shift on both legs (easier to qualify; fewer temperate)
#   - Monthly-extreme: tmax_warmest≥35 (hot); tmin_coldest≤5 (cold)
#
# For each variant × wave, computes survey-weighted H, M0, A under NB-main
# weights, plus population shares per zone.

rm(list = ls())
options(scipen = 999)
options(survey.lonely.psu = "adjust")

if (!require(pacman)) install.packages("pacman")
p_load("dplyr", "tidyr", "purrr", "stringr", "survey", "srvyr", "data.table",
       "xtable")

load("outputs/intermediate/dt.RData")
years <- c(2016, 2018, 2020, 2022, 2024)
k <- 0.10

# Climate-zone variants -------------------------------------------------------
# Each function maps temperature columns → weather ∈ {1,2,3}: 1=hot, 2=temperate, 3=cold

classify_baseline <- function(tmax_a, tmin_a, avg_t, tmax_w, tmin_c) {
  case_when(
    tmax_a >= 30 & avg_t >= 24 ~ 1L,
    tmin_a <= 10 & avg_t <= 12 ~ 3L,
    TRUE ~ 2L
  )
}
classify_restrictive <- function(tmax_a, tmin_a, avg_t, tmax_w, tmin_c) {
  case_when(
    tmax_a >= 32 & avg_t >= 26 ~ 1L,
    tmin_a <=  8 & avg_t <= 10 ~ 3L,
    TRUE ~ 2L
  )
}
classify_permissive <- function(tmax_a, tmin_a, avg_t, tmax_w, tmin_c) {
  case_when(
    tmax_a >= 28 & avg_t >= 22 ~ 1L,
    tmin_a <= 12 & avg_t <= 14 ~ 3L,
    TRUE ~ 2L
  )
}
classify_monthly <- function(tmax_a, tmin_a, avg_t, tmax_w, tmin_c) {
  case_when(
    tmax_w >= 35 ~ 1L,
    tmin_c <=  5 ~ 3L,
    TRUE ~ 2L
  )
}

variants <- list(
  Restrictive       = classify_restrictive,
  Baseline          = classify_baseline,
  Permissive        = classify_permissive,
  `Monthly-extreme` = classify_monthly
)

# Score recomputation under alternative weather --------------------------------
# Recomputes ach_clim → dp_clim under weather_alt, then the NB score.
score_with_weather <- function(dt, weather_vec, w8, w7, k_val) {
  ach_clim_alt <- case_when(
    weather_vec == 1L & (dt$aire_acond == 1 | dt$ratio_venti == 1) ~ 1,
    weather_vec == 3L &  dt$calefacc   == 1                        ~ 1,
    weather_vec == 2L                                              ~ NA_real_,
    TRUE                                                            ~ 0
  )
  dp_clim_alt <- ifelse(is.na(ach_clim_alt), NA_real_,
                        ifelse(ach_clim_alt == 1, 0, 1))

  score <- case_when(
    weather_vec %in% c(1L, 3L) ~
      dt$dp_hill  * w8["dp_hill"]  + dt$dp_light * w8["dp_light"] +
      dt$dp_cook  * w8["dp_cook"]  + dt$dp_refri * w8["dp_refri"] +
      dt$dp_entr  * w8["dp_entr"]  + dt$dp_com   * w8["dp_com"]   +
      dt$dp_wt    * w8["dp_wt"]    + dp_clim_alt * w8["dp_clim"],
    weather_vec == 2L ~
      dt$dp_hill  * w7["dp_hill"]  + dt$dp_light * w7["dp_light"] +
      dt$dp_cook  * w7["dp_cook"]  + dt$dp_refri * w7["dp_refri"] +
      dt$dp_entr  * w7["dp_entr"]  + dt$dp_com   * w7["dp_com"]   +
      dt$dp_wt    * w7["dp_wt"],
    TRUE ~ NA_real_
  )

  # Missing-value convention: mep and mep_score must be NA on the SAME rows.
  # This block used to set mep_score = 0 where the score was NA, so an individual
  # with a missing indicator was excluded from H but counted as non-deprived in
  # M0. svymean(na.rm = TRUE) then computed the two over different denominators
  # and A = M0/H inherited the mismatch -- the Baseline variant returned
  # A = 30.476 for 2024 where the main pipeline returns 30.479. Same defect that
  # was fixed in .ksens_one() (3_estimations.R) and .scorable() (7_diagnostics.R).
  list(
    weather   = weather_vec,
    mep       = ifelse(is.na(score), NA_real_, as.numeric(score >= k_val)),
    mep_score = ifelse(is.na(score), NA_real_,
                       ifelse(score >= k_val, score, 0))
  )
}

# Estimation per variant per wave ---------------------------------------------

results <- map_dfr(names(variants), function(vname) {
  classifier <- variants[[vname]]
  message(format(Sys.time(), "%H:%M:%S"), " | Variant ", vname)
  map_dfr(years, function(i) {
    yr <- paste0("dt", i)
    dt <- dt_list[[yr]]
    w_alt <- classifier(
      dt$tmax_annual, dt$tmin_annual, dt$avg_temp,
      dt$tmax_warmest, dt$tmin_coldest
    )
    sc <- score_with_weather(dt, w_alt, w_j, w_j_7dim, k)

    dt2 <- dt %>%
      mutate(
        weather_alt   = sc$weather,
        mep_alt       = sc$mep,
        mep_score_alt = sc$mep_score,
        upm     = as.factor(upm),
        est_dis = as.factor(est_dis),
        factor  = as.numeric(factor),
        is_hot  = as.numeric(weather_alt == 1L),
        is_temp = as.numeric(weather_alt == 2L),
        is_cold = as.numeric(weather_alt == 3L)
      )

    svy <- svydesign(ids = ~upm, strata = ~est_dis, weights = ~factor,
                     data = dt2, nest = TRUE)

    m_H  <- svymean(~mep_alt,       svy, na.rm = TRUE)
    m_M0 <- svymean(~mep_score_alt, svy, na.rm = TRUE)
    p_h  <- coef(svymean(~is_hot,  svy, na.rm = TRUE))[1]
    p_t  <- coef(svymean(~is_temp, svy, na.rm = TRUE))[1]
    p_c  <- coef(svymean(~is_cold, svy, na.rm = TRUE))[1]

    H_v   <- coef(m_H)[1]
    M0_v  <- coef(m_M0)[1]

    # Stored at 6 dp, not 3. Table A8 prints H and M0 to two decimals; rounding
    # to 3 here and again to 2 in the tex double-rounds anything sitting on a
    # x.xx5 boundary (Monthly-extreme 2018 H and 2024 H both do).
    tibble(
      variant       = vname,
      year          = i,
      H             = round(100 * H_v,                   6),
      se_H          = round(100 * as.numeric(SE(m_H)[1]), 6),
      M0            = round(100 * M0_v,                  6),
      se_M0         = round(100 * as.numeric(SE(m_M0)[1]), 6),
      A             = round(ifelse(H_v > 0, 100 * M0_v / H_v, NA_real_), 6),
      pop_hot       = round(100 * p_h, 2),
      pop_temperate = round(100 * p_t, 2),
      pop_cold      = round(100 * p_c, 2)
    )
  })
})

cat("\nClimate-zone sensitivity (NB-main weights, k=0.10):\n")
print(as.data.frame(results))

# Sanity check: Baseline numbers should match the existing main-pipeline output
load("outputs/final/robustness.RData")
baseline_check <- results %>%
  filter(variant == "Baseline") %>%
  select(year, H_alt = H, M0_alt = M0, A_alt = A) %>%
  left_join(robustness %>% select(year, H, M0, A), by = "year") %>%
  mutate(dH = round(H - H_alt, 3),
         dM0 = round(M0 - M0_alt, 3),
         dA = round(A - A_alt, 3))
cat("\nBaseline-vs-pipeline sanity check (should be ~0 for all rows):\n")
print(as.data.frame(baseline_check))

save(results, file = "outputs/final/climate_sensitivity.RData")
