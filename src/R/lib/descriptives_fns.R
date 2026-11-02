# Functions for descriptive results. Input tables: panel_quarterly.csv.
library(data.table)

main_sample <- function(dt) {
  dt <- as.data.table(dt)
  if ("in_main_sample" %in% names(dt)) {
    flag <- dt$in_main_sample
    if (!is.logical(flag)) flag <- as.character(flag) %in% c("True", "TRUE")
    dt <- dt[flag]
  }
  dt
}

# Cross-state coefficient of variation (sd with n-1, divided by mean) per period.
sigma_cv <- function(dt, var) {
  d <- main_sample(dt)
  d[, .(cv = sd(get(var)) / mean(get(var))), by = period][order(period)]
}

# Quarter index of a "YYYYQn" period string: 4 * year + (n - 1).
quarter_index <- function(period) {
  stopifnot(all(grepl("^[0-9]{4}Q[1-4]$", period)))
  4L * as.integer(substr(period, 1, 4)) + as.integer(substr(period, 6, 6)) - 1L
}

# Beta convergence: annualised growth between start and end on ln(initial level).
# Window length in years = (quarter_index(end) - quarter_index(start)) / 4.
beta_convergence <- function(dt, var, start = "2018Q4", end = "2026Q2") {
  years <- (quarter_index(end) - quarter_index(start)) / 4
  stopifnot(years > 0)
  d <- main_sample(dt)
  a <- d[period == start, .(state_id, x_start = get(var))]
  b <- d[period == end, .(state_id, x_end = get(var))]
  m <- merge(a, b, by = "state_id")
  m[, ln_start := log(x_start)]
  m[, growth := (log(x_end) - log(x_start)) / years]
  lm(growth ~ ln_start, data = m)
}
