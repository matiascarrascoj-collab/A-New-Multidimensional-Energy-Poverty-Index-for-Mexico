#*** Energy Poverty (estimations) **
#*** Matías Carrasco-Jiménez **

# Preamble --------------------------------------------------------------------

rm(list = ls())
options(scipen = 999)
options(survey.lonely.psu = "adjust") 

if (!require(pacman)) install.packages("pacman") 
p_load("dplyr", "rstudioapi","dineq", "haven", 
       "data.table", "stringr", "zoo", "data.table", 
       "tidyverse", "srvyr", "bit64", "survey", "writexl",
       "convey", "scales", "sf", "readr", "broom", "FactoMineR"
)

load("outputs/intermediate/dt.RData")
years <- c(2016, 2018, 2020, 2022, 2024)

# Estimations ----

results <- list()

for(i in years){
  message(format(Sys.time(), "%H:%M:%S"), " | Running year ", i)
  yr <- paste0("dt", i)
  
  variables <- c("mep", "mep_score", "dp_light", "dp_cook",
                 "dp_refri", "dp_entr", "dp_com", "dp_wt",
                 "dp_hill", "dp_clim", "ictpc_r", "ictpc",
                 "mep_mca",  "mep_score_mca",
                 "mep_eq",   "mep_score_eq",
                 "mep_freq", "mep_score_freq",
                 "mep_noaf", "mep_score_noaf")
  
  subgroups <- c("national", "ent_name", "pobreza", "rururb", 
                 "hli", "children", "elderly", "ic_rezedu")
  
  dt <- dt_list[[yr]] %>%
    mutate(
      ent_name = as.factor(ent_name),
      upm      = as.factor(upm),
      est_dis  = as.factor(est_dis),
      factor   = as.numeric(factor),
      national = as.factor("National"),
      across(all_of(variables), ~ as.numeric(.x)),
      across(all_of(subgroups), ~ as.factor(.x))
    ) 
  
  svy <- svydesign(
    ids     = ~upm,
    strata  = ~est_dis,
    weights = ~factor,
    data    = dt,
    nest    = TRUE
  )

  
  combos <- expand.grid(var = variables, sub = subgroups, 
                        stringsAsFactors = FALSE)
  
  results_i <- map2_dfr(combos$var, combos$sub, function(var, sub) {
    
    tryCatch({
      svyby(
        formula = as.formula(paste0("~", var)),
        by      = as.formula(paste0("~", sub)),
        design  = svy,
        FUN     = svymean,
        na.rm   = TRUE,
        vartype = "se"
      ) %>%
        as.data.frame() %>%
        rename(estimate = 2, se = 3) %>%
        mutate(
          term     = as.character(.[[1]]),
          variable = var,
          subgroup = sub,
          year     = i
        ) %>%
        select(year, subgroup, term, variable, estimate, se)
    }, error = function(e) {
      message("  Skipping ", var, " x ", sub, ": ", e$message)
      NULL
    })
  })
  
  est <- results_i %>%
    filter(variable %in% variables) %>%
    select(year, subgroup, term, variable, estimate) %>%
    pivot_wider(names_from = variable, values_from = estimate)

  se <- results_i %>%
    filter(variable %in% variables) %>%
    select(year, subgroup, term, variable, se) %>%
    pivot_wider(names_from = variable, values_from = se, names_prefix = "se_")

  results_df_i <- est %>%
    left_join(se, by = c("year", "subgroup", "term")) %>%
    mutate(
      # MAIN scheme (Nussbaumer-style, NB)
      H  = mep,          se_H  = se_mep,
      M0 = mep_score,    se_M0 = se_mep_score,
      A  = ifelse(H > 0, M0 / H, NA_real_),
      # SE for A via delta method (conservative: assumes Cov(M0,H) = 0)
      se_A = ifelse(H > 0 & M0 > 0,
                    A * sqrt((se_M0 / M0)^2 + (se_H / H)^2),
                    NA_real_),
      # Robustness: MCA, equal, frequency-based, NB-noAf
      H_mca   = mep_mca,      M0_mca   = mep_score_mca,
      A_mca   = ifelse(H_mca  > 0, M0_mca  / H_mca,  NA_real_),
      H_eq    = mep_eq,       M0_eq    = mep_score_eq,
      A_eq    = ifelse(H_eq   > 0, M0_eq   / H_eq,   NA_real_),
      H_freq  = mep_freq,     M0_freq  = mep_score_freq,
      A_freq  = ifelse(H_freq > 0, M0_freq / H_freq, NA_real_),
      H_noaf  = mep_noaf,     M0_noaf  = mep_score_noaf,
      A_noaf  = ifelse(H_noaf > 0, M0_noaf / H_noaf, NA_real_)
    )
  
  results[[yr]] <- results_df_i
  message(format(Sys.time(), "%H:%M:%S"), " | Done: ", i)
}

results_df <- bind_rows(results)

rm(list = setdiff(ls(), c("results", "results_df")))
save.image("outputs/final/estimations_means.RData")

## Delta tests: 2024 vs 2016 (national) ----
# SE(Δ) = sqrt(SE_2016² + SE_2024²) — valid under cross-section independence

p_load("stargazer", "xtable")

se_map <- c(
  H        = "se_H",        M0       = "se_M0",       A        = "se_A",
  dp_light = "se_dp_light", dp_cook  = "se_dp_cook",  dp_refri = "se_dp_refri",
  dp_entr  = "se_dp_entr",  dp_com   = "se_dp_com",   dp_wt    = "se_dp_wt",
  dp_hill  = "se_dp_hill",  dp_clim  = "se_dp_clim"
)

y16 <- results_df %>% filter(subgroup == "national", year == 2016)
y24 <- results_df %>% filter(subgroup == "national", year == 2024)

delta_national <- tibble(variable = names(se_map)) %>%
  mutate(
    est_2016 = map_dbl(variable, ~ y16[[.x]]),
    est_2024 = map_dbl(variable, ~ y24[[.x]]),
    se_2016  = map_dbl(variable, ~ y16[[se_map[.x]]]),
    se_2024  = map_dbl(variable, ~ y24[[se_map[.x]]]),
    delta    = est_2024 - est_2016,
    se_delta = sqrt(se_2016^2 + se_2024^2),
    z_stat   = delta / se_delta,
    p_value  = 2 * pnorm(-abs(z_stat)),
    sig      = case_when(p_value < 0.01 ~ "***",
                         p_value < 0.05 ~ "**",
                         p_value < 0.10 ~ "*",
                         TRUE           ~ "")
  ) %>%
  mutate(across(c(est_2016, est_2024, delta, se_2016, se_2024, se_delta),
                ~ .x * 100))

cat("\nDelta tests (2016 → 2024), national:\n")
print(delta_national)
xtable(delta_national %>%
         select(variable, est_2016, est_2024, delta, se_delta, z_stat, p_value, sig),
       digits = 3)

## Robustness: NB (main) vs MCA vs equal vs frequency weights ----

robustness <- results_df %>%
  filter(subgroup == "national") %>%
  select(year,
         H, M0, A,
         H_mca,  M0_mca,  A_mca,
         H_eq,   M0_eq,   A_eq,
         H_freq, M0_freq, A_freq) %>%
  mutate(across(-year, ~ round(.x * 100, 3))) %>%
  arrange(year)

cat("\nRobustness — NB (main) vs MCA vs equal vs frequency weights:\n")
print(robustness)
xtable(robustness, digits = 3)

## Af-circularity: NB main vs NB-noAf (Section 4 / appendix) ----

af_drop <- results_df %>%
  filter(subgroup == "national") %>%
  select(year, H, M0, A, H_noaf, M0_noaf, A_noaf) %>%
  mutate(across(-year, ~ round(.x * 100, 3))) %>%
  arrange(year)

cat("\nAf-drop sensitivity — NB main vs NB-noAf:\n")
print(af_drop)

save(delta_national, robustness, af_drop,
     file = "outputs/final/robustness.RData")

## k sensitivity analysis ----
# Compares H, A, M0 across k ∈ {0.10–0.30} for all three weight schemes (national)
# Requires *_raw continuous scores stored by .add_mep in 2_data.R

rm(list = setdiff(ls(), c("delta_national", "robustness", "results", "results_df")))
load("outputs/intermediate/dt.RData")
years  <- c(2016, 2018, 2020, 2022, 2024)
k_grid <- c(0.10, 0.15, 0.20, 0.25, 0.30)
k_grid2 <- c(0.125, 0.25, 0.375, 0.50, 0.625)

# Helper: estimate H/M0/A at a given k for one year and raw-score column
#
# NOTE (missing-value convention). Both mep_k and score_k must be NA on the same
# rows, otherwise svymean(na.rm = TRUE) computes H and M0 over different
# denominators and A = M0/H inherits the mismatch. An earlier version set
# score_k = 0 when the raw score was NA (individuals with a missing indicator
# were counted as non-deprived in M0 but excluded from H), which diluted M0
# slightly and made A here disagree with the same quantity in results_df --
# 30.476 against 30.479 for 2024. The ifelse() below keeps the two aligned.
.ksens_one <- function(i, k_val, raw_col) {
  dt <- dt_list[[paste0("dt", i)]] %>%
    mutate(
      mep_k   = ifelse(!is.na(.data[[raw_col]]),
                       as.numeric(.data[[raw_col]] >= k_val), NA_real_),
      score_k = ifelse(is.na(.data[[raw_col]]), NA_real_,
                       ifelse(.data[[raw_col]] >= k_val,
                              .data[[raw_col]], 0))
    )
  svy <- svydesign(ids = ~upm, strata = ~est_dis,
                   weights = ~factor, data = dt, nest = TRUE)
  m_H  <- svymean(~mep_k,   svy, na.rm = TRUE)
  m_M0 <- svymean(~score_k, svy, na.rm = TRUE)
  tibble(
    k     = k_val, year  = i,
    H     = coef(m_H)[1],  M0    = coef(m_M0)[1],
    se_H  = as.numeric(SE(m_H)[1]), se_M0 = as.numeric(SE(m_M0)[1])
  ) %>%
    mutate(
      A    = ifelse(H > 0, M0 / H, NA_real_),
      se_A = ifelse(H > 0 & M0 > 0,
                    (M0 / H) * sqrt((se_M0 / M0)^2 + (se_H / H)^2), NA_real_)
    )
}

k_sensitivity <- map_dfr(k_grid, function(k_val)
  map_dfr(years, function(i) .ksens_one(i, k_val, "mep_score_raw"))) %>%
  mutate(across(c(H, M0, A, se_H, se_M0, se_A), ~ round(.x * 100, 3))) %>%
  arrange(k, year)

k_sensitivity_mca <- map_dfr(k_grid, function(k_val)
  map_dfr(years, function(i) .ksens_one(i, k_val, "mep_score_mca_raw"))) %>%
  mutate(across(c(H, M0, A, se_H, se_M0, se_A), ~ round(.x * 100, 3))) %>%
  arrange(k, year)

k_sensitivity_eq <- map_dfr(k_grid2, function(k_val)
  map_dfr(years, function(i) .ksens_one(i, k_val, "mep_score_eq_raw"))) %>%
  mutate(across(c(H, M0, A, se_H, se_M0, se_A), ~ round(.x * 100, 3))) %>%
  arrange(k, year)

k_sensitivity_freq <- map_dfr(k_grid, function(k_val)
  map_dfr(years, function(i) .ksens_one(i, k_val, "mep_score_freq_raw"))) %>%
  mutate(across(c(H, M0, A, se_H, se_M0, se_A), ~ round(.x * 100, 3))) %>%
  arrange(k, year)

cat("\nk sensitivity (national, NB main weights):\n")
print(k_sensitivity)
cat("\nk sensitivity (national, MCA weights):\n")
print(k_sensitivity_mca)
cat("\nk sensitivity (national, equal weights):\n")
print(k_sensitivity_eq)
cat("\nk sensitivity (national, frequency weights):\n")
print(k_sensitivity_freq)

save(k_sensitivity, k_sensitivity_mca, k_sensitivity_eq, k_sensitivity_freq,
     file = "outputs/final/k_sensitivity.RData")

## Tables ----

national <-  results_df %>% filter(subgroup == "national") %>%
  select(-subgroup, -term, -mep, -mep_score,
         -ictpc, -ictpc_r, -year,
         -starts_with("se_"),
         -mep_mca,  -mep_score_mca,
         -mep_eq,   -mep_score_eq,
         -mep_freq, -mep_score_freq,
         -mep_noaf, -mep_score_noaf,
         -H_mca,  -M0_mca,  -A_mca,
         -H_eq,   -M0_eq,   -A_eq,
         -H_freq, -M0_freq, -A_freq,
         -H_noaf, -M0_noaf, -A_noaf)

national_t <- as.data.frame(t(national))
colnames(national_t) <- c("2016", "2018", "2020", "2022", "2024")
national_t <- national_t %>%
  mutate(across(where(is.numeric), ~.x * 100),
         Delta = `2024` - `2016`)

xtable(national_t, digits = 3)

sub_groups <-  results_df %>% filter(!(subgroup %in% c("national", "ent_name"))
                                     & year %in% c(2016, 2024)) %>%
  select(term, subgroup, year, H, A) %>%
  pivot_wider(names_from = year, 
              values_from = c("H", "A")) %>%
  mutate(across(where(is.numeric), ~.x * 100),
         Delta_H = H_2024 - H_2016,
         Delta_A = A_2024 - A_2016) 


xtable(sub_groups, digits = 3)

# Estimations: totals ----

rm(list = ls())
load("outputs/intermediate/dt.RData")
years <- c(2016, 2018, 2020, 2022, 2024)

results_t <- list()

for(i in years){
  message(format(Sys.time(), "%H:%M:%S"), " | Running year ", i)
  yr <- paste0("dt", i)
  dt <- dt_list[[yr]] %>%
    mutate(
      ent_name = as.factor(ent_name),
      upm      = as.factor(upm),
      est_dis  = as.factor(est_dis),
      factor   = as.numeric(factor),
      national = as.factor("National"))
  
  svy <- svydesign(
    ids     = ~upm,
    strata  = ~est_dis,
    weights = ~factor,
    data    = dt,
    nest    = TRUE
  )
  
  variables <- c("mep", "dp_light", "dp_cook", "dp_refri", 
                 "dp_entr", "dp_com", "dp_wt", "dp_hill", "dp_clim")
  
  subgroups <- c("national")
  
  combos <- expand.grid(var = variables, sub = subgroups, 
                        stringsAsFactors = FALSE)
  
  results_i <- map2_dfr(combos$var, combos$sub, function(var, sub) {
    
    tryCatch({
      svyby(
        formula = as.formula(paste0("~", var)),
        by      = as.formula(paste0("~", sub)),
        design  = svy,
        FUN     = svytotal,
        na.rm   = TRUE,
        vartype = "se"
      ) %>%
        as.data.frame() %>%
        rename(estimate = 2, se = 3) %>%
        mutate(
          term     = as.character(.[[1]]),
          variable = var,
          subgroup = sub,
          year     = i
        ) %>%
        select(year, subgroup, term, variable, estimate, se)
    }, error = function(e) {
      message("  Skipping ", var, " x ", sub, ": ", e$message)
      NULL
    })
  })
  
  est_t <- results_i %>%
    filter(variable %in% variables) %>%
    select(year, subgroup, term, variable, estimate) %>%
    pivot_wider(names_from = variable, values_from = estimate)

  se_t <- results_i %>%
    filter(variable %in% variables) %>%
    select(year, subgroup, term, variable, se) %>%
    pivot_wider(names_from = variable, values_from = se, names_prefix = "se_")

  results_df_i <- est_t %>%
    left_join(se_t, by = c("year", "subgroup", "term")) %>%
    mutate(H = mep, se_H = se_mep)

  results_t[[yr]] <- results_df_i
  message(format(Sys.time(), "%H:%M:%S"), " | Done: ", i)
}

results_df_t <- bind_rows(results_t)

rm(list = setdiff(ls(), c("results_df", "results_df_t")))
save.image("outputs/final/estimations_totals.RData")

p_load("stargazer", "xtable"
)

## Tables: totals ----

totals <- (results_df_t) %>%
  select(-subgroup, -term, -year)

totals <- as.data.frame(t(totals))

colnames(totals) <- c("2016", "2018", "2020", "2022", "2024")

totals <- totals %>%
  mutate(Delta = `2024` - `2016`,
         across(where(is.numeric), ~.x / 1e6)) # Convert to millions

xtable(totals, digits = 3)

# Decile-Centile estimations ----

rm(list = ls())
load("outputs/intermediate/dt.RData")
years <- c(2016, 2018, 2020, 2022, 2024)

results_inc <- list()

for(i in years){
  message(format(Sys.time(), "%H:%M:%S"), " | Running year ", i)
  yr <- paste0("dt", i)
  dt <- dt_list[[yr]] %>%
    mutate(
      ent_name = as.factor(ent_name),
      upm      = as.factor(upm),
      est_dis  = as.factor(est_dis),
      factor   = as.numeric(factor))
  
  svy <- svydesign(
    ids     = ~upm,
    strata  = ~est_dis,
    weights = ~factor,
    data    = dt,
    nest    = TRUE
  )
  
  variables <- c("mep", "mep_score", "ach_ictpc_r", "ictpc_r", "energy_exp_r")
  
  subgroups <- c("centiles")
  
  combos <- expand.grid(var = variables, sub = subgroups, 
                        stringsAsFactors = FALSE)
  
  results_i <- map2_dfr(combos$var, combos$sub, function(var, sub) {
    
    tryCatch({
      svyby(
        formula = as.formula(paste0("~", var)),
        by      = as.formula(paste0("~", sub)),
        design  = svy,
        FUN     = svymean,
        na.rm   = TRUE,
        vartype = "se"
      ) %>%
        as.data.frame() %>%
        rename(estimate = 2, se = 3) %>%
        mutate(
          term     = as.character(.[[1]]),
          variable = var,
          subgroup = sub,
          year     = i
        ) %>%
        select(year, subgroup, term, variable, estimate, se)
    }, error = function(e) {
      message("  Skipping ", var, " x ", sub, ": ", e$message)
      NULL
    })
  })
  
  results_df_i <- results_i %>%
    filter(variable %in% variables) %>%
    select(year, subgroup, term, variable, estimate) %>%
    pivot_wider(names_from = variable, values_from = estimate) %>%
    dplyr::mutate(
      H  = mep,
      M0 = mep_score,
      A  = ifelse(H > 0, M0 / H, NA_real_)
    )
  
  results_inc[[yr]] <- results_df_i
  message(format(Sys.time(), "%H:%M:%S"), " | Done: ", i)
}

results_df_inc <- bind_rows(results_inc)
rm(list = setdiff(ls(), c("results_inc", "results_df_inc")))
save.image("outputs/final/estimations_inc.RData")

# Dimensional decomposition of M0 ----

rm(list = ls())
load("outputs/intermediate/dt.RData")
years <- c(2016, 2018, 2020, 2022, 2024)
dp_cols <- c("dp_light", "dp_cook", "dp_refri", "dp_entr", 
             "dp_com", "dp_wt", "dp_hill", "dp_clim")

# Define your two weight vectors
weights_8 <- w_j

weights_7 <- w_j_7dim

# Define the dimensions (must match your w_j names)
dp_cols <- c("dp_hill", "dp_light", "dp_cook", "dp_refri", 
             "dp_entr", "dp_com", "dp_wt", "dp_clim")

for (yr in names(dt_list)) {
  dt_list[[yr]] <- dt_list[[yr]] %>%
    mutate(
      is_poor = ifelse(mep == 1, 1, 0)
    ) %>%
    mutate(across(all_of(dp_cols), .names = "contrib_{.col}", ~ {
      col_name <- cur_column()
      
      # Step 1: Determine the correct weight
      weight_val <- case_when(
        weather %in% c(1, 3) ~ w_j[col_name],
        weather == 2 & col_name != "dp_clim" ~ w_j_7dim[col_name],
        TRUE ~ 0 # This correctly sets clim weight to 0 for weather==2
      )
      
      # Step 2: Handle the NA values in dp_cols (especially dp_clim)
      # We use coalesce to treat NA deprivation as 0 only if the weight is 0
      deprivation_status <- ifelse(is.na(.x), 0, .x)
      
      deprivation_status * weight_val * is_poor
    }))
    dt_list[[yr]] <- dt_list[[yr]] %>%
    mutate(across(starts_with("contrib_"), ~ ifelse(is.na(mep_score), NA, .x)))
}

results_list <- list()
M0_se_list   <- list()   # design-based SE(M0) per wave, for the Total row

for (yr in names(dt_list)){
  dt <- dt_list[[yr]]
  yr_label <- str_extract(yr, "\\d{4}")

  design <- svydesign(ids=~upm, strata=~est_dis, weights=~factor, data=dt, nest=TRUE)
  M0_svy <- svymean(~mep_score, design, na.rm = TRUE)
  M0 <- coef(M0_svy)[1]
  M0_se_list[[yr_label]] <- as.numeric(SE(M0_svy)[1])
  delta_formula <- as.formula(paste0("~", paste(paste0("contrib_", dp_cols), collapse="+")))
  delta_j_svy <- as.data.frame(svymean(delta_formula, design, na.rm = TRUE))
  delta_j     <- delta_j_svy %>% select(-SE)
  se_delta_j  <- delta_j_svy %>% select(SE)
  delta_j_r   <- delta_j / M0

  df_yr <- data.frame(dimension = dp_cols,
                      abs = delta_j, se = se_delta_j, rel = delta_j_r) %>%
    setNames(c("dimension",
               paste0("delta_j_",    yr_label),
               paste0("se_delta_j_", yr_label),
               paste0("relative_j_", yr_label)))

  results_list[[yr]] <- df_yr
}

final_table <- results_list %>% reduce(left_join, by = "dimension")

# Total row.
#
# delta_j and relative_j DO sum across dimensions -- that is equation (11),
# sum_j delta_j = M0 -- so column-summing them is right. SEs do NOT: summing
# eight standard errors returns the perfect-correlation upper bound on
# SE(sum), not the standard error of the sum. Until August 2026 this row was
# built with a blanket summarise(across(where(is.numeric), sum)), so the
# Total SE printed 0.256 for 2016 where the design-based SE(M0) is 0.164 --
# two different standard errors in the paper for one and the same estimate.
# The design-based SE from svymean(~mep_score) is the correct quantity and is
# what tab3 already reports, so the two tables now agree by construction.
total_row <- final_table %>%
  summarise(across(starts_with("delta_j_"), sum),
            across(starts_with("relative_j_"), sum)) %>%
  mutate(dimension = "Total")

for (yl in names(M0_se_list))
  total_row[[paste0("se_delta_j_", yl)]] <- M0_se_list[[yl]]

final_output <- bind_rows(final_table, total_row) %>%
  mutate(across(where(is.numeric), ~.x * 100)) %>%
  select(dimension, starts_with("delta_j_"), starts_with("se_delta_j_"),
         starts_with("relative_j"))

xtable(final_output, digits = 3)

save(final_output, file = "outputs/final/dimensional_decomposition.RData")

# Af-circularity diagnostics ----
# Reviewer concern (May 2026): "Af is mostly a relabeling of CONEVAL monetary
# poverty." We compute three diagnostics per wave:
#   (a) P(Af deprived | NOT monetarily poor)         — informs over-classification
#   (b) P(Af deprived | monetarily poor)             — informs under-classification
#   (c) Share of energy-poor (NB-main) who have NO Tier-1 or Tier-2 deprivation
#       — i.e., who are poor solely via Tier-3 (Af + Tc) joint deprivation.
# All survey-design weighted using the standard svydesign object.

rm(list = ls())
load("outputs/intermediate/dt.RData")
years <- c(2016, 2018, 2020, 2022, 2024)

af_circ <- map_dfr(years, function(i) {
  yr <- paste0("dt", i)
  dt <- dt_list[[yr]] %>%
    mutate(
      upm     = as.factor(upm),
      est_dis = as.factor(est_dis),
      factor  = as.numeric(factor),
      # Indicator: deprived only via Tier-3 (Af + Tc), no Tier-1 / Tier-2 deprivation
      tier12_any = pmax(dp_light, dp_cook, dp_wt, dp_refri, dp_entr, dp_com,
                        na.rm = TRUE),
      tier3_only_poor = ifelse(mep == 1 & tier12_any == 0, 1, 0)
    )
  svy <- svydesign(ids = ~upm, strata = ~est_dis, weights = ~factor,
                   data = dt, nest = TRUE)

  p_af_given_notpob <- coef(svymean(~dp_hill, subset(svy, pobreza == 0), na.rm = TRUE))[1]
  p_af_given_pob    <- coef(svymean(~dp_hill, subset(svy, pobreza == 1), na.rm = TRUE))[1]
  p_tier3_only_in_poor <-
    coef(svymean(~tier3_only_poor, subset(svy, mep == 1), na.rm = TRUE))[1]

  tibble(
    year                  = i,
    P_Af_given_notpobreza = round(100 * p_af_given_notpob, 3),
    P_Af_given_pobreza    = round(100 * p_af_given_pob,    3),
    Tier3only_share_poor  = round(100 * p_tier3_only_in_poor, 3)
  )
})

cat("\nAf-circularity diagnostics by wave (in %):\n")
print(af_circ)
xtable(af_circ, digits = 3)
 
save(af_circ, file = "outputs/final/af_circularity.RData")

