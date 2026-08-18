#*** Indicator diagnostics, counterfactuals and descriptive statistics **
#
# Every number that appears in essay_I.tex but is not produced by scripts 1-6.
# Written in August 2026 in response to the referee correction list; before this
# script existed these values were computed ad hoc and were therefore not
# reproducible from the deposited code.
#
# Reads : outputs/intermediate/dt.RData
#         outputs/final/estimations_means.RData
#         outputs/final/estimations_inc.RData
#         inputs/viviendas/viviendas{year}.csv   (cooking fuel categories)
#         inputs/hogares/hogares{year}.csv       (Et component variables)
# Writes: outputs/final/diagnostics.RData
#
# Each section is tagged with the LaTeX label of the table or the section of
# essay_I.tex that consumes it, so the code-to-text mapping is explicit.

rm(list = ls())
options(scipen = 999)
options(survey.lonely.psu = "adjust")

if (!require(pacman)) install.packages("pacman")
p_load("dplyr", "tidyr", "purrr", "stringr", "survey", "srvyr", "data.table")

load("outputs/intermediate/dt.RData")
years <- c(2016, 2018, 2020, 2022, 2024)
k     <- 0.10

# Shared helpers --------------------------------------------------------------

# Weighted quantile (type 1 / inverse CDF), avoids an Hmisc dependency.
wtd_quantile <- function(x, w, probs) {
  ok <- !is.na(x) & !is.na(w)
  x <- x[ok]; w <- w[ok]
  o <- order(x); x <- x[o]; w <- w[o]
  cw <- cumsum(w) / sum(w)
  vapply(probs, function(p) x[which(cw >= p)[1]], numeric(1))
}

# Tetrachoric correlation for two weighted binary vectors. Solves for rho such
# that the bivariate-normal upper-orthant probability matches the observed
# joint cell, given the two marginal thresholds.
tetrachoric <- function(x, y, w) {
  ok <- !is.na(x) & !is.na(y) & !is.na(w)
  x <- x[ok]; y <- y[ok]; w <- w[ok]
  N   <- sum(w)
  p1  <- sum(w[x == 1]) / N              # marginal P(x = 1)
  p2  <- sum(w[y == 1]) / N              # marginal P(y = 1)
  p11 <- sum(w[x == 1 & y == 1]) / N     # joint  P(x = 1, y = 1)
  if (min(p1, p2, p11) <= 0 || max(p1, p2) >= 1) return(NA_real_)
  h <- qnorm(1 - p1)
  g <- qnorm(1 - p2)
  upper_orthant <- function(r) {
    integrate(function(z) dnorm(z) * pnorm((r * z - g) / sqrt(1 - r^2)),
              lower = h, upper = 8)$value
  }
  tryCatch(uniroot(function(r) upper_orthant(r) - p11,
                   c(-0.999, 0.999))$root,
           error = function(e) NA_real_)
}

# Two-sided z test on a difference of two independent survey means.
delta_test <- function(e0, s0, e1, s1) {
  d  <- e1 - e0
  se <- sqrt(s0^2 + s1^2)
  z  <- d / se
  p  <- 2 * pnorm(-abs(z))
  tibble(delta = d, se_delta = se, z = z, p_value = p,
         sig = ifelse(p < 0.01, "***",
               ifelse(p < 0.05, "**",
               ifelse(p < 0.10, "*", ""))))
}

# Replicates the wave-specific renaming applied in 2_data.R.
read_viviendas <- function(i) {
  v <- fread(sprintf("inputs/viviendas/viviendas%d.csv", i),
             colClasses = list(character = "folioviv")) %>% rename_all(tolower)
  if (i %in% c(2016, 2018, 2020, 2022)) v <- v %>% rename(combus = combustible)
  v %>% mutate(folioviv = ifelse(nchar(folioviv) == 9,
                                 paste0("0", folioviv), folioviv),
               combus   = as.numeric(combus))
}

read_hogares <- function(i) {
  fread(sprintf("inputs/hogares/hogares%d.csv", i),
        colClasses = list(character = c("folioviv", "foliohog"))) %>%
    rename_all(tolower) %>%
    mutate(folioviv = ifelse(nchar(folioviv) == 9,
                             paste0("0", folioviv), folioviv))
}

# =============================================================================
# 1. Realized sample sizes per wave                       -> tab:samples (§2.1)
# =============================================================================

sample_sizes <- map_dfr(years, function(i) {
  d <- dt_list[[paste0("dt", i)]]
  tibble(year        = i,
         dwellings   = n_distinct(d$folioviv),
         households  = n_distinct(paste0(d$folioviv, d$foliohog)),
         individuals = nrow(d),
         pop_millions = sum(as.numeric(d$factor), na.rm = TRUE) / 1e6)
})

cat("\n== 1. Sample sizes (tab:samples) ==\n")
print(as.data.frame(sample_sizes), digits = 6)

# =============================================================================
# 2. Income dispersion and energy burden of the bottom quintile        -> §1
#    Replaces the eyeballed claims about Figure 1 panels (a) and (b).
# =============================================================================

income_dispersion <- map_dfr(years, function(i) {
  d  <- as.data.frame(dt_list[[paste0("dt", i)]])
  f  <- as.numeric(d$factor)
  x  <- d$ictpc_r                      # real ICTPC, Aug-2024 pesos
  ee <- d$energy_exp_r                 # real energy expenditure
  ok <- !is.na(x) & !is.na(f)
  mu <- weighted.mean(x[ok], f[ok])
  lx <- log(x[ok & x > 0]); lf <- f[ok & x > 0]
  lmu <- weighted.mean(lx, lf)
  sd_log <- sqrt(sum(lf * (lx - lmu)^2) / sum(lf))
  q20 <- wtd_quantile(x[ok], f[ok], 0.20)
  eok <- !is.na(ee) & !is.na(f)
  ee_mean <- weighted.mean(ee[eok], f[eok])
  sel <- ok & eok & x <= q20
  tibble(year = i,
         mean_ictpc_real = mu,
         sd_log_ictpc    = sd_log,
         p20_ictpc       = as.numeric(q20),
         mean_energy_exp = ee_mean,
         bottom20_energy_above_mean = 100 * sum(f[sel & ee > ee_mean]) / sum(f[sel]))
})

cat("\n== 2. Income dispersion / bottom-quintile energy burden (§1) ==\n")
print(as.data.frame(income_dispersion), digits = 5)

# =============================================================================
# 3. Cooking fuel category distribution                  -> tab2 coding note
#    Documents that category 7 ("does not cook") exists only from 2024, while
#    categories 6 and 7 together are stable.
# =============================================================================

fuel_distribution <- map_dfr(years, function(i) {
  v <- read_viviendas(i) %>% select(folioviv, combus)
  d <- dt_list[[paste0("dt", i)]] %>% select(folioviv, factor)
  d %>%
    left_join(v, by = "folioviv") %>%
    mutate(f = as.numeric(factor)) %>%
    group_by(combus) %>%
    summarise(w = sum(f, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = i, share = 100 * w / sum(w)) %>%
    select(year, combus, share)
}) %>%
  pivot_wider(names_from = year, values_from = share, names_prefix = "y") %>%
  arrange(combus)

fuel_6_7 <- fuel_distribution %>%
  filter(combus %in% c(6, 7)) %>%
  summarise(across(starts_with("y"), ~ sum(.x, na.rm = TRUE))) %>%
  mutate(combus = "6+7 combined", .before = 1)

cat("\n== 3. Cooking fuel distribution, % (tab2 note) ==\n")
cat("   1=wood 2=coal 3=gas tank 4=gas pipe 5=electricity 6=other 7=does not cook\n")
print(as.data.frame(fuel_distribution), digits = 4)
cat("   Categories 6 and 7 combined (stable across waves):\n")
print(as.data.frame(fuel_6_7), digits = 4)

# =============================================================================
# 4. Entertainment indicator diagnostics          -> tab:et_diag Panel A (§3.1)
#    Tests whether the rise in Et deprivation reflects device substitution
#    (smartphone + internet displacing TV / radio / computer) rather than a
#    deterioration in access.
# =============================================================================

et_diagnostics <- map_dfr(years, function(i) {
  h <- read_hogares(i) %>%
    select(folioviv, foliohog, num_radio, num_compu, conex_inte, celular)
  d <- dt_list[[paste0("dt", i)]] %>%
    select(folioviv, foliohog, numren, factor, dp_entr, num_tv)

  m <- d %>%
    left_join(h, by = c("folioviv", "foliohog")) %>%
    mutate(f         = as.numeric(factor),
           has_tv    = as.numeric(num_tv)     >= 1,
           has_radio = as.numeric(num_radio)  >= 1,
           has_comp  = as.numeric(num_compu)  >= 1,
           has_int   = as.numeric(conex_inte) == 1,
           has_cel   = as.numeric(celular)    == 1,
           # Alternative achievement rule: add the (cellphone AND internet) route
           ach_alt   = as.numeric(has_tv | has_radio |
                                  (has_comp & has_int) | (has_cel & has_int)))

  tot  <- sum(m$f, na.rm = TRUE)
  dep  <- m %>% filter(dp_entr == 1)
  fdep <- sum(dep$f, na.rm = TRUE)
  sh   <- function(v, w, den) 100 * sum(w[v], na.rm = TRUE) / den

  tibble(
    year                = i,
    Et_dep_baseline     = sh(m$dp_entr == 1, m$f, tot),
    Et_dep_altrule      = sh(m$ach_alt == 0, m$f, tot),
    has_tv_all          = sh(m$has_tv, m$f, tot),
    has_radio_all       = sh(m$has_radio, m$f, tot),
    has_comp_int_all    = sh(m$has_comp & m$has_int, m$f, tot),
    has_internet_all    = sh(m$has_int, m$f, tot),
    has_cel_all         = sh(m$has_cel, m$f, tot),
    Etdep_with_cel      = sh(dep$has_cel, dep$f, fdep),
    Etdep_with_int      = sh(dep$has_int, dep$f, fdep),
    Etdep_with_cel_int  = sh(dep$has_cel & dep$has_int, dep$f, fdep)
  )
})

cat("\n== 4. Entertainment diagnostics, % (tab:et_diag Panel A) ==\n")
print(as.data.frame(et_diagnostics), digits = 4)

# =============================================================================
# 5. Water heating / cooking overlap              -> tab:et_diag Panel B (§3.1)
#    Wt achievement is satisfied by stove ownership alone (the MAEN convention),
#    which induces overlap with the cooking dimension.
# =============================================================================

wt_ck_overlap <- map_dfr(years, function(i) {
  d    <- as.data.frame(dt_list[[paste0("dt", i)]]) %>%
    mutate(f = as.numeric(factor))
  tot  <- sum(d$f, na.rm = TRUE)
  ach  <- d %>% filter(dp_wt == 0)
  fach <- sum(ach$f, na.rm = TRUE)

  tibble(
    year                  = i,
    Wt_dep                = 100 * sum(d$f[d$dp_wt   == 1], na.rm = TRUE) / tot,
    Ck_dep                = 100 * sum(d$f[d$dp_cook == 1], na.rm = TRUE) / tot,
    both_dep              = 100 * sum(d$f[d$dp_wt == 1 & d$dp_cook == 1],
                                      na.rm = TRUE) / tot,
    Wt_ach_via_stove_only = 100 * sum(ach$f[ach$estufa == 1 & ach$water_heat == 0],
                                      na.rm = TRUE) / fach,
    Wt_ach_via_heater     = 100 * sum(ach$f[ach$water_heat == 1],
                                      na.rm = TRUE) / fach,
    tetrachoric_Wt_Ck     = tetrachoric(d$dp_wt, d$dp_cook, d$f)
  )
})

cat("\n== 5. Water heating / cooking overlap (tab:et_diag Panel B) ==\n")
print(as.data.frame(wt_ck_overlap), digits = 4)

# =============================================================================
# 6. Two-tier counterfactual                              -> tab:twotier (§4)
#    Collapses Tiers 2 and 3, putting affordability on the same footing as the
#    appliance dimensions, and reports Af's resulting share of M0.
# =============================================================================

nb_ratio <- 0.20 / 0.13
dp_cols  <- c("dp_light", "dp_cook", "dp_wt", "dp_refri",
              "dp_entr", "dp_com", "dp_clim", "dp_hill")

# Rows the main pipeline is able to score: every dimension *available* to the
# individual must be non-missing. dp_clim is NA by construction in temperate
# municipalities (weather == 2), where it is unavailable rather than missing,
# so it is exempted there. Mirrors the NA behaviour of .add_mep() in 2_data.R.
.scorable <- function(d, cols) {
  need <- setdiff(cols, "dp_clim")
  ok   <- stats::complete.cases(d[, need, drop = FALSE])
  ok & (d$weather == 2L | !is.na(d$dp_clim))
}

# Hot / cold: 3 Tier-1 + 5 Tier-2  =>  3 w1 + 5 w2 = 1, w1 = ratio * w2
tt_w2 <- 1 / (3 * nb_ratio + 5); tt_w1 <- nb_ratio * tt_w2
# Temperate (Tc unavailable): 3 Tier-1 + 4 Tier-2
tt_w2_7 <- 1 / (3 * nb_ratio + 4); tt_w1_7 <- nb_ratio * tt_w2_7

w_twotier <- c(dp_light = tt_w1, dp_cook = tt_w1, dp_wt = tt_w1,
               dp_refri = tt_w2, dp_entr = tt_w2, dp_com = tt_w2,
               dp_clim  = tt_w2, dp_hill = tt_w2)
w_twotier_7 <- c(dp_light = tt_w1_7, dp_cook = tt_w1_7, dp_wt = tt_w1_7,
                 dp_refri = tt_w2_7, dp_entr = tt_w2_7, dp_com = tt_w2_7,
                 dp_clim  = 0,       dp_hill = tt_w2_7)

cat("\n== 6. Two-tier weights (tab:twotier) ==\n")
cat(sprintf("   hot/cold : w1 = %.5f, w2 = %.5f   (3w1 + 5w2 = %.5f)\n",
            tt_w1, tt_w2, 3 * tt_w1 + 5 * tt_w2))
cat(sprintf("   temperate: w1 = %.5f, w2 = %.5f   (3w1 + 4w2 = %.5f)\n",
            tt_w1_7, tt_w2_7, 3 * tt_w1_7 + 4 * tt_w2_7))

twotier <- map_dfr(years, function(i) {
  d <- as.data.frame(dt_list[[paste0("dt", i)]]) %>%
    mutate(f = as.numeric(factor))

  # Missing-value convention. The main pipeline (.add_mep in 2_data.R) lets an
  # NA on any *available* indicator propagate to the score, so the individual
  # drops out of svymean(na.rm = TRUE) entirely. Imputing G[is.na(G)] <- 0 here
  # instead would treat a missing indicator as an achievement and retain the
  # individual, which is why the two-tier H used to sit 0.001-0.004pp away from
  # the equal-weights column of tab:robustness even though the two schemes share
  # an identification set by construction. Drop the same rows the main pipeline
  # drops -- note dp_clim is NA *by design* in temperate municipalities and must
  # not count as missing there.
  d       <- d[.scorable(d, dp_cols), , drop = FALSE]
  G       <- as.matrix(d[, dp_cols]); G[is.na(G)] <- 0
  hotcold <- d$weather %in% c(1L, 3L)
  W <- matrix(rep(w_twotier_7, each = nrow(d)), nrow = nrow(d),
              dimnames = list(NULL, dp_cols))
  W[hotcold, ] <- matrix(rep(w_twotier, each = sum(hotcold)),
                         nrow = sum(hotcold))

  cscore <- rowSums(W * G)
  poor   <- as.numeric(cscore >= k)
  cens   <- cscore * poor
  tot    <- sum(d$f)

  # Keep the fractional M0 in its own name: delta_j is also a fraction, so the
  # dimensional shares must be formed before either is rescaled to percent.
  M0_frac <- sum(d$f * cens) / tot
  delta   <- vapply(seq_along(dp_cols),
                    function(j) sum(d$f * W[, j] * G[, j] * poor) / tot,
                    numeric(1))
  names(delta) <- dp_cols

  tibble(year = i,
         H  = 100 * sum(d$f * poor) / tot,
         M0 = 100 * M0_frac,
         A  = 100 * sum(d$f * cens) / sum(d$f * poor),
         Af_share_M0 = 100 * delta[["dp_hill"]] / M0_frac,
         Tc_share_M0 = 100 * delta[["dp_clim"]] / M0_frac)
})

cat("\n== 6. Two-tier counterfactual (tab:twotier) ==\n")
print(as.data.frame(twotier), digits = 5)

# =============================================================================
# 7. Affordability vs income poverty                     -> tab:af_circ (§4)
#    af_circularity.RData conditions on `pobreza` (CONEVAL MULTIDIMENSIONAL
#    poverty). The circularity question requires conditioning on `plp`, the
#    INCOME poverty line that actually defines the Af cutoff. Both are computed
#    here so the table can name each conditioning event explicitly.
# =============================================================================

af_conditioning <- map_dfr(years, function(i) {
  d <- as.data.frame(dt_list[[paste0("dt", i)]]) %>%
    mutate(f = as.numeric(factor))
  cond <- function(mask) 100 * weighted.mean(d$dp_hill[mask], d$f[mask],
                                             na.rm = TRUE)
  tibble(year                   = i,
         P_Af_given_income_nonpoor = cond(d$plp     == 0),
         P_Af_given_income_poor    = cond(d$plp     == 1),
         P_Af_given_multidim_nonpoor = cond(d$pobreza == 0),
         P_Af_given_multidim_poor    = cond(d$pobreza == 1),
         income_poverty_rate       = 100 * weighted.mean(d$plp, d$f, na.rm = TRUE),
         multidim_poverty_rate     = 100 * weighted.mean(d$pobreza, d$f,
                                                         na.rm = TRUE))
})

cat("\n== 7. Af conditional probabilities (tab:af_circ) ==\n")
print(as.data.frame(af_conditioning), digits = 5)

# -----------------------------------------------------------------------------
# 7b. The reverse conditional, and the climate-zone location of the Tier-3 route
#     -> §4 (Discussion)
#
#     Two quantities the earlier draft asserted without producing.
#
#     (i)  P(income-poor | Af-deprived). Panel A of tab:af_circ reports
#          P(Af | income-poor) = 100% by construction, which understates how
#          close the two populations are. Read the other way round, the share of
#          the Af-deprived who are ALSO income-poor is ~95%: as a population, Af
#          is very nearly a relabeling of income poverty. The defense of the
#          indicator is architectural (w_Af < k), not informational, and the
#          text now says so -- which requires this number.
#
#     (ii) The Tier-3-only identification route needs Tc to be AVAILABLE, so it
#          exists only where weather %in% c(1, 3). In temperate municipalities
#          w_Af = 0.0786 and every other available weight is >= 0.1210 > k, so
#          affordability cannot move H at all there. The national 13-17% in
#          Panel C therefore averages over a population where the mechanism is
#          structurally absent for ~79% of individuals. Splitting by zone shows
#          where the index is actually exposed to the Hills critique.
# -----------------------------------------------------------------------------

af_route <- map_dfr(years, function(i) {
  d <- as.data.frame(dt_list[[paste0("dt", i)]]) %>%
    mutate(
      upm     = as.factor(upm),
      est_dis = as.factor(est_dis),
      factor  = as.numeric(factor),
      tier12_any = pmax(dp_light, dp_cook, dp_wt, dp_refri, dp_entr, dp_com,
                        na.rm = TRUE),
      tier3_only_poor = ifelse(mep == 1 & tier12_any == 0, 1, 0),
      hotcold         = as.integer(weather %in% c(1L, 3L))
    )
  svy <- svydesign(ids = ~upm, strata = ~est_dis, weights = ~factor,
                   data = d, nest = TRUE)

  # (i) reverse conditional, weighted
  p_inc_given_af <- 100 * weighted.mean(d$plp[d$dp_hill == 1],
                                        d$factor[d$dp_hill == 1], na.rm = TRUE)

  # (ii) Tier-3-only share of the energy poor, nationally and by zone
  t3 <- function(sub) 100 * coef(svymean(~tier3_only_poor, sub, na.rm = TRUE))[1]

  tibble(
    year                    = i,
    P_incomepoor_given_Af   = p_inc_given_af,
    Tier3only_national      = t3(subset(svy, mep == 1)),
    Tier3only_hotcold       = t3(subset(svy, mep == 1 & hotcold == 1)),
    Tier3only_temperate     = t3(subset(svy, mep == 1 & hotcold == 0)),
    share_poor_in_hotcold   = 100 * weighted.mean(d$hotcold[d$mep == 1],
                                                  d$factor[d$mep == 1],
                                                  na.rm = TRUE)
  )
})

cat("\n== 7b. Reverse conditional and climate location of the Tier-3 route (§4) ==\n")
cat("   Tier3only_temperate must be 0 by construction: w_Af = 0.0786 and every\n")
cat("   other available weight in a temperate municipality is >= 0.1210 > k.\n")
print(as.data.frame(af_route), digits = 5)
stopifnot(all(round(af_route$Tier3only_temperate, 6) == 0))

# =============================================================================
# 8. Subgroup population shares, gaps and ratios                -> tab4 (§3)
# =============================================================================

load("outputs/final/estimations_means.RData")

subgroup_vars <- c(rururb = "rururb", hli = "hli", pobreza = "pobreza",
                   children = "children", elderly = "elderly",
                   ic_rezedu = "ic_rezedu")

population_shares <- map_dfr(years, function(i) {
  d   <- as.data.frame(dt_list[[paste0("dt", i)]]) %>%
    mutate(f = as.numeric(factor))
  tot <- sum(d$f)
  map_dfr(subgroup_vars, function(v) {
    tibble(year = i, subgroup = v,
           share_ref = 100 * sum(d$f[d[[v]] == 0], na.rm = TRUE) / tot,
           share_dis = 100 * sum(d$f[d[[v]] == 1], na.rm = TRUE) / tot)
  })
}) %>%
  mutate(share_sum = share_ref + share_dis)   # < 100 where the variable has NAs

cat("\n== 8a. Subgroup population shares, % (tab4) ==\n")
print(as.data.frame(population_shares %>% filter(year %in% c(2016, 2024))),
      digits = 5)

subgroup_gaps <- map_dfr(subgroup_vars, function(v) {
  x <- results_df %>%
    filter(subgroup == v, year %in% c(2016, 2024)) %>%
    select(year, term, H)
  g <- function(t, y) 100 * x$H[x$term == t & x$year == y]
  tibble(subgroup    = v,
         gap_pp_2016 = g(1, 2016) - g(0, 2016),
         gap_pp_2024 = g(1, 2024) - g(0, 2024),
         ratio_2016  = g(1, 2016) / g(0, 2016),
         ratio_2024  = g(1, 2024) / g(0, 2024)) %>%
    mutate(d_gap_pp = gap_pp_2024 - gap_pp_2016,
           d_ratio  = ratio_2024 - ratio_2016)
})

cat("\n== 8b. Subgroup gaps in H: percentage points vs ratios (tab4) ==\n")
print(as.data.frame(subgroup_gaps), digits = 4)

# Delta tests for every subgroup, including the restored elderly panel.
subgroup_deltas <- map_dfr(subgroup_vars, function(v) {
  x <- results_df %>%
    filter(subgroup == v, year %in% c(2016, 2024)) %>%
    arrange(term, year)
  map_dfr(c(0, 1), function(t) {
    r <- x %>% filter(term == t)
    bind_cols(
      tibble(subgroup = v, term = t),
      delta_test(100 * r$H[1], 100 * r$se_H[1],
                 100 * r$H[2], 100 * r$se_H[2]) %>%
        rename_with(~ paste0(.x, "_H")),
      delta_test(100 * r$A[1], 100 * r$se_A[1],
                 100 * r$A[2], 100 * r$se_A[2]) %>%
        rename_with(~ paste0(.x, "_A"))
    )
  })
})

cat("\n== 8c. Subgroup delta tests, 2016-2024 (tab4) ==\n")
print(as.data.frame(subgroup_deltas %>%
                      select(subgroup, term, delta_H, se_delta_H, z_H, sig_H,
                             delta_A, se_delta_A, z_A, sig_A)), digits = 4)

# Verify the M0 subgroup decomposition of equation (12) closes.
decomposition_check <- map_dfr(subgroup_vars, function(v) {
  sh <- population_shares %>% filter(year == 2024, subgroup == v)
  m  <- results_df %>% filter(subgroup == v, year == 2024) %>% arrange(term)
  tibble(subgroup = v,
         implied_M0 = (sh$share_ref * 100 * m$M0[m$term == 0] +
                       sh$share_dis * 100 * m$M0[m$term == 1]) / 100,
         national_M0 = 100 * results_df$M0[results_df$subgroup == "national" &
                                             results_df$year == 2024],
         shares_sum = sh$share_sum)
}) %>% mutate(gap = implied_M0 - national_M0)

cat("\n== 8d. Decomposition check: sum(pi_l * M0_l) vs national M0, 2024 ==\n")
print(as.data.frame(decomposition_check), digits = 5)

# =============================================================================
# 9. National delta tests, 2016-2018                 -> tab:delta Panel B (§3)
#    Formalises the claim that the small uptick is indistinguishable from zero.
# =============================================================================

nat <- results_df %>% filter(subgroup == "national") %>% arrange(year)

delta_2016_2018 <- map_dfr(c("H", "A", "M0"), function(v) {
  s <- paste0("se_", v)
  bind_cols(tibble(variable = v,
                   est_2016 = 100 * nat[[v]][nat$year == 2016],
                   est_2018 = 100 * nat[[v]][nat$year == 2018]),
            delta_test(100 * nat[[v]][nat$year == 2016],
                       100 * nat[[s]][nat$year == 2016],
                       100 * nat[[v]][nat$year == 2018],
                       100 * nat[[s]][nat$year == 2018]))
})

cat("\n== 9. National delta tests 2016-2018 (tab:delta Panel B) ==\n")
print(as.data.frame(delta_2016_2018), digits = 4)

# =============================================================================
# 10. Dimensional change: percentage points vs relative                 -> §3
#     The draft reported pp changes while calling them "relative"; both are
#     produced here so the text can state each explicitly.
# =============================================================================

dim_map <- c(Ea = "dp_light", Ck = "dp_cook", Wt = "dp_wt", Rf = "dp_refri",
             Et = "dp_entr",  Cm = "dp_com",  Tc = "dp_clim", Af = "dp_hill")

dimensional_change <- imap_dfr(dim_map, function(col, lab) {
  a <- 100 * nat[[col]][nat$year == 2016]
  b <- 100 * nat[[col]][nat$year == 2024]
  tibble(dimension = lab, y2016 = a, y2024 = b,
         change_pp = b - a, change_rel_pct = 100 * (b - a) / a)
}) %>% arrange(change_rel_pct)

cat("\n== 10. Dimensional change: pp vs relative (§3) ==\n")
print(as.data.frame(dimensional_change), digits = 4)

# =============================================================================
# 11. Convergence regression                    -> §3 and fig:MEPIcen_plots
#     Regresses the 2016-2024 change in incidence on the ICTPC centile rank.
# =============================================================================

load("outputs/final/estimations_inc.RData")

centiles <- results_df_inc %>%
  filter(subgroup == "centiles") %>%
  mutate(term = as.numeric(term)) %>%
  select(year, term, H) %>%
  pivot_wider(names_from = year, values_from = H, names_prefix = "y") %>%
  mutate(dH_pp  = 100 * (y2024 - y2016),
         dH_rel = 100 * (y2024 - y2016) / y2016)

fit_pp  <- lm(dH_pp  ~ term, data = centiles)
fit_rel <- lm(dH_rel ~ term, data = centiles)

convergence_regression <- tibble(
  spec      = c("Delta H (pp)", "Delta H (relative %)"),
  slope     = c(coef(fit_pp)[2],  coef(fit_rel)[2]),
  se        = c(summary(fit_pp)$coefficients[2, 2],
                summary(fit_rel)$coefficients[2, 2]),
  t_stat    = c(summary(fit_pp)$coefficients[2, 3],
                summary(fit_rel)$coefficients[2, 3]),
  p_value   = c(summary(fit_pp)$coefficients[2, 4],
                summary(fit_rel)$coefficients[2, 4]),
  r_squared = c(summary(fit_pp)$r.squared, summary(fit_rel)$r.squared),
  n         = c(nobs(fit_pp), nobs(fit_rel))
)

cat("\n== 11. Convergence regression (§3, fig:MEPIcen_plots caption) ==\n")
print(as.data.frame(convergence_regression), digits = 5)

# =============================================================================
# 12. MCA inertia adjustments                          -> tab:mca note (App.)
#     Raw MCA inertia percentages understate explained variance. Benzecri and
#     Greenacre corrections justify retaining a single axis.
# =============================================================================

lambda <- mca_diagnostics$eig$eigenvalue
Q <- 8L                      # number of binary variables
J <- 16L                     # total modalities
keep <- lambda > 1 / Q
benzecri <- ifelse(keep, (Q / (Q - 1))^2 * (lambda - 1 / Q)^2, 0)
greenacre_mean <- (Q / (Q - 1)) * (sum(lambda^2) - (J - Q) / Q^2)

# NB: `if`, not `ifelse` — the condition is a scalar, so ifelse() would recycle
# the first element of the true-branch across every axis.
pct_benzecri <- if (sum(benzecri) > 0) {
  100 * benzecri / sum(benzecri)
} else {
  rep(0, length(benzecri))
}

mca_adjustment <- tibble(
  axis            = seq_along(lambda),
  eigenvalue      = lambda,
  pct_raw         = 100 * lambda / sum(lambda),
  benzecri_adj    = benzecri,
  pct_benzecri    = pct_benzecri,
  pct_greenacre   = 100 * benzecri / greenacre_mean
)

cat("\n== 12. MCA inertia adjustments (tab:mca note) ==\n")
cat(sprintf("   total inertia = %.4f  (theoretical (J-Q)/Q = %.4f)\n",
            sum(lambda), (J - Q) / Q))
cat(sprintf("   Greenacre average inertia = %.5f\n", greenacre_mean))
print(as.data.frame(mca_adjustment), digits = 5)

# =============================================================================
# 13. State-level estimates                              -> tab:states (App.)
# =============================================================================

state_estimates <- results_df %>%
  filter(subgroup == "ent_name", year %in% c(2016, 2024)) %>%
  mutate(across(c(H, A, M0), ~ 100 * .x)) %>%
  select(entity = term, year, H, A, M0) %>%
  pivot_wider(names_from = year, values_from = c(H, A, M0)) %>%
  mutate(dH = H_2024 - H_2016,
         dA = A_2024 - A_2016,
         dM0 = M0_2024 - M0_2016) %>%
  arrange(desc(H_2024))

cat("\n== 13. State-level estimates (tab:states) ==\n")
print(as.data.frame(state_estimates), digits = 4)

cat("\n   Entities where incidence or intensity rose:\n")
print(as.data.frame(state_estimates %>% filter(dH > 0 | dA > 0) %>%
                      select(entity, H_2016, H_2024, dH, A_2016, A_2024, dA)),
      digits = 4)

# =============================================================================
# 14. Water-heating specification sensitivity                           -> §3.1
#
#     Section 3.1 reports that Wt achievement is satisfied by a dedicated water
#     heater (sh | gh) OR by owning a stove (st >= 1), the MAEN convention of
#     Garcia-Ochoa (2016); that the tetrachoric correlation with Ck is 0.83-0.86;
#     and that 44-52% of Wt achievers qualify through the stove route alone. It
#     does not currently report what happens if that route is removed. Wt carries
#     a Tier-1 weight and Ck + Wt together account for ~45% of M0 in 2024, so
#     this is a larger margin than the entertainment drop of 6_et_drop.R.
#
#     NB-strictWt keeps all eight dimensions and the main weight vector; only
#     the Wt achievement rule changes, to (sh | gh). Deprivation in Wt therefore
#     rises mechanically and both H and M0 rise with it; the question the
#     exercise answers is whether the 2016-2024 TRAJECTORY is preserved.
#
#     `water_heat` (= sh | gh) and `estufa` (= st >= 1) are already carried by
#     dt_list -- 2_data.R keeps both components alongside ach_wt, and section 5
#     above reads them straight off the data frame. An earlier version of this
#     block re-read viviendas and left_join()ed a second `water_heat` column on
#     top of the existing one; dplyr then suffixed the pair to water_heat.x /
#     water_heat.y, the bare name no longer resolved, and the script died here
#     with "object 'water_heat' not found" before reaching save(). That is why
#     diagnostics.RData carried neither wt_strict nor af_route. Use the column
#     that is already there.
# =============================================================================

wt_strict <- map_dfr(years, function(i) {
  d <- as.data.frame(dt_list[[paste0("dt", i)]]) %>%
    filter(!is.na(water_heat)) %>%
    mutate(
      # strict rule: dedicated water heater only, stove route removed
      dp_wt_strict = ifelse(water_heat == 1, 0, 1),
      upm = as.factor(upm), est_dis = as.factor(est_dis),
      factor = as.numeric(factor)
    )
  d <- d[.scorable(d, dp_cols), , drop = FALSE]

  G <- as.matrix(d[, dp_cols]); G[is.na(G)] <- 0
  G[, "dp_wt"] <- d$dp_wt_strict

  hotcold <- d$weather %in% c(1L, 3L)
  W <- matrix(rep(w_j_7dim[dp_cols], each = nrow(d)), nrow = nrow(d),
              dimnames = list(NULL, dp_cols))
  W[!hotcold, "dp_clim"] <- 0
  W[hotcold, ] <- matrix(rep(w_j[dp_cols], each = sum(hotcold)),
                         nrow = sum(hotcold))

  d$score_s <- rowSums(W * G, na.rm = TRUE)
  d$poor_s  <- as.numeric(d$score_s >= k)
  d$cens_s  <- d$score_s * d$poor_s

  svy <- svydesign(ids = ~upm, strata = ~est_dis, weights = ~factor,
                   data = d, nest = TRUE)
  mH  <- svymean(~poor_s, svy, na.rm = TRUE)
  mM0 <- svymean(~cens_s, svy, na.rm = TRUE)
  mWt <- svymean(~dp_wt_strict, svy, na.rm = TRUE)

  tibble(year = i,
         dp_wt_strict_rate = 100 * coef(mWt)[1],
         H_strict  = 100 * coef(mH)[1],
         se_H      = 100 * as.numeric(SE(mH)[1]),
         M0_strict = 100 * coef(mM0)[1],
         se_M0     = 100 * as.numeric(SE(mM0)[1]),
         A_strict  = 100 * coef(mM0)[1] / coef(mH)[1])
})

cat("\n== 14. Water-heating specification sensitivity (dedicated heater only) ==\n")
print(as.data.frame(wt_strict), digits = 5)
# Read the NB-main comparison off `nat` (section 9) rather than hard-coding it:
# these two numbers were literals until August 2026, which meant this line went
# stale silently whenever 3_estimations.R was re-run.
nb_M0_2016 <- 100 * nat$M0[nat$year == 2016]
nb_M0_2024 <- 100 * nat$M0[nat$year == 2024]
cat(sprintf("\n   NB main    : M0 %.3f -> %.3f (%+.1f%%)\n",
            nb_M0_2016, nb_M0_2024, 100 * (nb_M0_2024 / nb_M0_2016 - 1)))
cat(sprintf("   NB-strictWt: M0 %.3f -> %.3f (%+.1f%%)\n",
            wt_strict$M0_strict[1], wt_strict$M0_strict[5],
            100 * (wt_strict$M0_strict[5] / wt_strict$M0_strict[1] - 1)))

# =============================================================================
# Save                                                                       ==
# =============================================================================

save(sample_sizes, income_dispersion, fuel_distribution, fuel_6_7,
     et_diagnostics, wt_ck_overlap, twotier, w_twotier, w_twotier_7,
     af_conditioning, af_route, population_shares, subgroup_gaps,
     subgroup_deltas, decomposition_check, delta_2016_2018, dimensional_change,
     convergence_regression, mca_adjustment, state_estimates, wt_strict,
     file = "outputs/final/diagnostics.RData")

cat("\nWritten: outputs/final/diagnostics.RData\n")
