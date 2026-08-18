#*** Auto-legend helpers for ggplot maps **
# Generate `limits`, `breaks`, and `labels` for scale_*_gradientn() calls
# directly from the data, avoiding hand-typed labels that go stale.

auto_legend <- function(x, n = 5, formatter = function(v) sprintf("%.2f", v),
                        na_rm = TRUE) {
  v_min <- min(x, na.rm = na_rm)
  v_max <- max(x, na.rm = na_rm)
  if (!is.finite(v_min) || !is.finite(v_max) || v_min == v_max) {
    v_min <- v_min - 0.5; v_max <- v_max + 0.5
  }
  brks <- seq(v_min, v_max, length.out = n)
  list(
    limits = c(v_min, v_max),
    breaks = brks,
    labels = vapply(brks, formatter, character(1))
  )
}

# Common formatters. Each is passed to auto_legend(formatter = ) and receives a
# single break value. Note the differing input scales: fmt_pct and fmt_growth
# expect a proportion and multiply by 100; fmt_pp expects percentage points
# already (see the *100 at the call sites in 4_plots.R).
fmt_pp     <- function(v) {
  if (abs(round(v, 1)) < 0.05) "0pp" else sprintf("%+.1fpp", v)
}
fmt_pct    <- function(v) sprintf("%.0f%%", v * 100)
fmt_growth <- function(v) sprintf("%+.0f%%", v * 100)
fmt_money  <- function(v) format(round(v, -2), big.mark = ",", scientific = FALSE)

# Removed in the August 2026 cleanup: fmt_pct_pp() and fmt_C(). Neither was ever
# called -- 1_spatial.R hand-codes its temperature legends rather than sourcing
# this file. If those legends are ever moved onto auto_legend(), reinstate
#   fmt_C <- function(v) sprintf("%.0f°C", v)
# rather than hand-typing the labels again.
