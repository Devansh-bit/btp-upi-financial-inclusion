# Functions for the Part 1 study measures. All inputs are data.table objects.
# Periods are "YYYYQn" strings. Population is in persons.
library(data.table)

# Quarter index of a "YYYYQn" period string: 4 * year + (n - 1).
quarter_index <- function(period) {
  stopifnot(all(grepl("^[0-9]{4}Q[1-4]$", period)))
  4L * as.integer(substr(period, 1, 4)) + as.integer(substr(period, 6, 6)) - 1L
}

TYPES <- c("p2p", "retail", "utility")

# Join the interim long files and the state labels: one row per state_id and period.
load_part1 <- function(phonepe, types, population, labels) {
  inputs <- list(
    phonepe = list(d = phonepe, vars = c(count = "phonepe_count", amount = "phonepe_amount_inr",
                                         users = "phonepe_users")),
    types = list(d = types, vars = c(count_p2p = "phonepe_count_p2p",
                                     count_retail = "phonepe_count_retail",
                                     count_utility = "phonepe_count_utility")),
    population = list(d = population, vars = c(population = "population")))
  labels <- as.data.table(labels)
  grid <- CJ(state_id = labels$state_id, period = sort(unique(phonepe$period)))
  out <- copy(grid)
  for (nm in names(inputs)) {
    d <- as.data.table(inputs[[nm]]$d)
    # fread reads large whole numbers as integer64; read with fread(..., integer64 = "double")
    if (inherits(d$value, "integer64")) stop(sprintf("value in %s is integer64", nm))
    extra <- setdiff(unique(d$state_id), labels$state_id)
    if (length(extra) > 0)
      stop(sprintf("unknown state_id in %s: %s", nm, paste(extra, collapse = ", ")))
    for (col in names(inputs[[nm]]$vars)) {
      v <- inputs[[nm]]$vars[[col]]
      x <- d[variable == v, .(state_id, period, value)]
      miss <- nrow(grid[!x, on = c("state_id", "period")])
      if (miss > 0) stop(sprintf("missing %d state-periods for %s in the %s input", miss, v, nm))
      out[x, (col) := i.value, on = c("state_id", "period")]
    }
  }
  out <- merge(out, labels[, .(state_id, name, code, in_main)], by = "state_id")
  setcolorder(out, c("state_id", "period", "name", "code", "in_main", "count", "amount", "users",
                     "population", "count_p2p", "count_retail", "count_utility"))
  out[order(state_id, period)]
}

# Per-person measures, intensity, average transaction value and type shares.
add_measures <- function(dt) {
  dt <- copy(dt)
  dt[, `:=`(count_pc = count / population, amount_pc = amount / population,
            users_per_1000 = 1000 * users / population, intensity = count / users,
            atv = amount / count,
            share_p2p = count_p2p / count, share_retail = count_retail / count,
            share_utility = count_utility / count)]
  dt[]
}

pct_change <- function(x) c(NA_real_, 100 * (x[-1] / x[-length(x)] - 1))

# National totals by period, with the measures and quarter-on-quarter growth in percent.
national <- function(dt) {
  cols <- c("count", "amount", "users", "population", "count_p2p", "count_retail", "count_utility")
  nat <- dt[, lapply(.SD, sum), by = period, .SDcols = cols][order(period)]
  nat <- add_measures(nat)
  nat[, `:=`(qoq_count = pct_change(count), qoq_amount = pct_change(amount))]
  nat[]
}

# One row per calendar year from the national series. The last year may be partial.
annual <- function(nat) {
  nat <- copy(nat)
  nat[, yr := as.integer(substr(period, 1, 4))]
  a <- nat[order(period), .(n_q = .N, count = sum(count), amount = sum(amount),
                            users = users[.N], mean_pop = mean(population)), by = yr]
  a[, `:=`(count_pc = count / mean_pop, atv = amount / count)]
  full <- a$n_q == 4L
  g <- rep(NA_real_, nrow(a))
  for (i in seq_len(nrow(a))[-1])
    if (full[i] && full[i - 1] && a$yr[i] == a$yr[i - 1] + 1L)
      g[i] <- 100 * (a$count[i] / a$count[i - 1] - 1)
  a[, growth_count := g]
  a[, year := as.character(yr)]
  if (!full[nrow(a)]) {
    qs <- nat[yr == a$yr[nrow(a)], substr(period, 6, 6)]
    a[nrow(a), year := sprintf("%d (Q%s--Q%s)", yr, min(qs), max(qs))]
  }
  a[, .(year, count, amount, users, count_pc, atv, growth_count)]
}

years_between <- function(start, end) (quarter_index(end) - quarter_index(start)) / 4

annual_rate <- function(x0, x1, years) 100 * ((x1 / x0)^(1 / years) - 1)

# Median and quartiles (R type 7) of var over ids, by period.
spread <- function(dt, var, ids) {
  dt[state_id %in% ids,
     .(med = median(get(var)), q1 = quantile(get(var), 0.25, type = 7, names = FALSE),
       q3 = quantile(get(var), 0.75, type = 7, names = FALSE)), by = period][order(period)]
}

# Share (percent) of the total held by the k ids with the largest count in per.
top_k <- function(dt, per, k = 5) {
  d <- dt[period == per]
  top <- d[order(-count)][seq_len(k), state_id]
  rbindlist(lapply(c("count", "amount", "users", "population"), function(v)
    data.table(variable = v, share = 100 * sum(d[state_id %in% top][[v]]) / sum(d[[v]]))))
}

# Smallest number of units, largest first, whose sum is at least s of the total.
n_for_share <- function(x, s = 0.5) {
  cs <- cumsum(sort(x, decreasing = TRUE)) / sum(x)
  # tolerance guards against floating-point error at an exact threshold
  as.integer(which(cs >= s - 1e-12)[1])
}

spearman <- function(x, y) cor(rank(x), rank(y))

# Cross-state variance decomposition of y = ln(C/P) into reach r = ln(U/P) and intensity i = ln(C/U).
decompose <- function(dt, per, ids) {
  d <- dt[period == per & state_id %in% ids]
  y <- log(d$count / d$population)
  r <- log(d$users / d$population)
  i <- log(d$count / d$users)
  list(n = nrow(d), var_y = var(y), var_r = var(r), var_i = var(i), cov_ri = cov(r, i),
       reach = cov(r, y) / var(y), intensity = cov(i, y) / var(y))
}

decompose_series <- function(dt, ids) {
  rbindlist(lapply(sort(unique(dt$period)), function(p)
    c(list(period = p), decompose(dt, p, ids))))
}
