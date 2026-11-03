# Part 1 study: figures, tables and computed numbers (numbers.tex).
# Run from the repository root: Rscript src/R/part1_study.R
suppressPackageStartupMessages({
  library(data.table); library(ggplot2); library(jsonlite)
})
source("src/R/lib/part1_fns.R")
source("src/R/lib/latex_out.R")
source("src/R/lib/study_theme.R")

OUT <- "output/study/part1"
TAB_DIR <- file.path(OUT, "tables")
dir.create(TAB_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)

F_PHONEPE <- "data/interim/phonepe.csv"
F_TYPES <- "data/interim/phonepe_types.csv"
F_POP <- "data/interim/population.csv"
F_CHECKS <- "data/interim/phonepe_checks.csv"
F_NCP <- "data/manual/population_ncp2020.csv"
F_LABELS <- "src/R/lib/state_labels.csv"
F_XWALK <- "src/py/btpdata/state_crosswalk.csv"
F_HOVER <- "data/raw/phonepe/repo/data/map/transaction/hover/country/india/2026/2.json"
P_FIRST <- "2018Q1"; P_REF0 <- "2018Q4"; P_LAST <- "2026Q2"
SRC_ALL <- "load_part1(); data/interim/{phonepe,phonepe_types,population}.csv"

# ---- inputs ----
read_interim <- function(path) fread(path, integer64 = "double")
phonepe <- read_interim(F_PHONEPE)
types <- read_interim(F_TYPES)
pop <- read_interim(F_POP)
labels <- fread(F_LABELS)
checks <- fread(F_CHECKS)
ncp <- fread(F_NCP)
xwalk <- fread(F_XWALK)

dt <- add_measures(load_part1(phonepe, types, pop, labels))
nat <- national(dt)
main_ids <- labels[in_main == TRUE, state_id]
other_ids <- labels[in_main == FALSE, state_id]
periods <- sort(unique(dt$period))
dt[, group := factor(ifelse(in_main, "main", "other"), levels = c("main", "other"))]
at <- function(per, ids = labels$state_id) dt[period == per & state_id %in% ids][order(state_id)]

# Normalised raw name, as btpdata.states.normalise for ASCII input.
normalise <- function(x) gsub("\\s+", " ", trimws(gsub("&", "and", tolower(x), fixed = TRUE)))

# Crosswalk rows whose raw_name occurs in a source; stops on a source name without a row.
xwalk_rows <- function(raw, what) {
  n <- unique(normalise(raw))
  miss <- setdiff(n, xwalk$raw_name)
  if (length(miss) > 0) stop(sprintf("%s names without a crosswalk row: %s", what,
                                     paste(miss, collapse = ", ")))
  xwalk[raw_name %in% n]
}
hover_names <- fromJSON(F_HOVER)$data$hoverDataList$name
x_phonepe <- xwalk_rows(hover_names, "PhonePe 2026Q2 hover")
x_ncp <- xwalk_rows(ncp[normalise(raw_name) != "india", raw_name], "NCP")

# ---- numbers ----
entries <- list()
add <- function(key, value, digits = NULL, source) {
  entries[[length(entries) + 1L]] <<- nv(key, value, digits, source)
}
# "A, B, C and D"
join_and <- function(x) if (length(x) == 1L) x else
  paste(paste(x[-length(x)], collapse = ", "), "and", x[length(x)])
name_of <- function(ids) labels$name[match(ids, labels$state_id)]
# Row of d with the largest (or smallest) var; ties broken by state_id.
pick <- function(d, var, largest) {
  o <- if (largest) order(-d[[var]], d$state_id) else order(d[[var]], d$state_id)
  d[o[1]]
}

# data
add("data.n_ids", nrow(labels), 0, F_LABELS)
add("data.n_main", length(main_ids), 0, paste0("in_main; ", F_LABELS))
add("data.n_other_ut", length(other_ids), 0, paste0("not in_main; ", F_LABELS))
add("data.n_variants", nrow(xwalk), 0, paste0("rows; ", F_XWALK))
add("data.n_quarters", length(periods), 0, paste0("load_part1(); ", F_PHONEPE))
add("data.n_phonepe_names", length(unique(hover_names)), 0, paste0("raw names; ", F_HOVER))
add("data.rows.phonepe", nrow(phonepe), 0, paste0("rows; ", F_PHONEPE))
add("data.rows.types", nrow(types), 0, paste0("rows; ", F_TYPES))
add("data.rows.population", nrow(pop), 0, paste0("rows; ", F_POP))
# Union territories of India (Ladakh is in the id of Jammu and Kashmir). The other ids are states.
UT_IDS <- c("andaman_nicobar", "chandigarh", "delhi", "dnh_dd", "jammu_kashmir", "lakshadweep", "puducherry")
stopifnot(all(UT_IDS %in% labels$state_id), all(other_ids %in% UT_IDS),
          setequal(intersect(main_ids, UT_IDS), c("delhi", "jammu_kashmir")))
add("data.n_states", sum(!labels$state_id %in% UT_IDS), 0, paste0("ids not in UT_IDS; ", F_LABELS))

# checks
CHECK_KEYS <- c(type_sum = "type_sum_equals_state_total", zero_fill = "absent_type_set_to_zero",
                nat_count = "national_count_equals_state_sum",
                nat_users = "national_users_equals_state_sum")
if (!setequal(checks$check, CHECK_KEYS))
  stop("unexpected checks in ", F_CHECKS, ": ", paste(checks$check, collapse = ", "))
for (k in names(CHECK_KEYS)) {
  r <- checks[check == CHECK_KEYS[[k]]]
  add(sprintf("check.%s.n", k), r$n_items, 0, paste0(r$check, " n_items; ", F_CHECKS))
  add(sprintf("check.%s.flagged", k), r$n_flagged, 0, paste0(r$check, " n_flagged; ", F_CHECKS))
}
# The absent type entry: unit, quarter and type from the note of the check (Section 3.2 of the text).
TYPES <- c("P2P", "Retail", "Utility")
zf <- checks[check == CHECK_KEYS[["zero_fill"]]]
stopifnot(nrow(zf) == 1L, zf$n_flagged == 1L)
zf_w <- strsplit(zf$note, " ", fixed = TRUE)[[1]]
stopifnot(length(zf_w) == 3L, zf_w[1] %in% labels$state_id, zf_w[2] %in% periods, zf_w[3] %in% TYPES)
SRC_ZF <- paste0("note of ", zf$check, "; ", F_CHECKS)
add("check.zero_fill.name", name_of(zf_w[1]), NULL, SRC_ZF)
add("check.zero_fill.period", zf_w[2], NULL, SRC_ZF)
add("check.zero_fill.type", zf_w[3], NULL, SRC_ZF)
add("check.zero_fill.present_types", join_and(setdiff(TYPES, zf_w[3])), NULL, paste0("types other than the absent type, ", SRC_ZF))
# Registered users: change from the previous quarter, per state id (pairs of consecutive quarters).
users_step <- dt[order(state_id, period), .(d = diff(users)), by = state_id]
add("check.users_fall.n", nrow(users_step), 0,
    paste0("consecutive-quarter pairs, diff(users), load_part1(); ", F_PHONEPE))
add("check.users_fall.flagged", sum(users_step$d < 0), 0,
    paste0("pairs with a fall in users, diff(users), load_part1(); ", F_PHONEPE))

# population
ncp_state <- ncp[normalise(raw_name) != "india"]
ncp_india <- ncp[normalise(raw_name) == "india"]
ncp_ratio <- function(y) sum(ncp_state[year == y, population_thousands]) /
  ncp_india[year == y, population_thousands]
ncp_n_rows <- function(y) nrow(ncp_state[year == y])
# State rows minus the India row, in thousand persons (the unit of Table 8).
ncp_diff <- function(y) sum(ncp_state[year == y, population_thousands]) -
  ncp_india[year == y, population_thousands]
stopifnot(ncp_n_rows(2018L) == ncp_n_rows(2027L))
add("pop.ncp_n_rows", ncp_n_rows(2018L), 0, paste0("state rows per year, ncp_n_rows(); ", F_NCP))
for (y in c(2018L, 2027L)) {
  stopifnot(nrow(ncp_india[year == y]) == 1L)
  add(sprintf("pop.ncp_ratio.%d", y), ncp_ratio(y), 7, paste0("state rows / india row; ", F_NCP))
  add(sprintf("pop.ncp_diff.%d", y), ncp_diff(y), 0,
      paste0("state rows - india row, thousand persons, ncp_diff(); ", F_NCP))
}
# Largest |state rows - India row| from rounding each row to the nearest thousand: 0.5 per row.
ncp_bound <- 0.5 * (ncp_n_rows(2018L) + 1L)
add("pop.ncp_diff.bound", ncp_bound, if (ncp_bound == round(ncp_bound)) 0 else 1,
    paste0("0.5 * (state rows + 1), thousand persons; ", F_NCP))
pop_grid <- CJ(state_id = labels$state_id, period = periods)
pop_missing <- nrow(pop_grid) - nrow(pop[variable == "population" & !is.na(value)][pop_grid,
                                         on = c("state_id", "period"), nomatch = NULL])
pop_tot <- function(per) sum(dt[period == per, population])
add("pop.total.2018Q1", pop_tot(P_FIRST) / 1e6, 1, paste0("sum of population; ", F_POP))
add("pop.total.2026Q2", pop_tot(P_LAST) / 1e6, 1, paste0("sum of population; ", F_POP))
add("pop.growth_pct", 100 * (pop_tot(P_LAST) / pop_tot(P_FIRST) - 1), 1,
    paste0("percent change 2018Q1-2026Q2; ", F_POP))
pg <- merge(at(P_FIRST)[, .(state_id, p0 = population)], at(P_LAST)[, .(state_id, name, group,
            p1 = population)], by = "state_id")
pg[, growth := 100 * (p1 / p0 - 1)]
for (s in c("max", "min")) {
  r <- pick(pg, "growth", s == "max")
  add(sprintf("pop.growth.%s.name", s), r$name, NULL, paste0("35 ids; ", F_POP))
  add(sprintf("pop.growth.%s", s), r$growth, 1, paste0("percent change 2018Q1-2026Q2, 35 ids; ", F_POP))
}
r <- pick(pg[state_id %in% main_ids], "growth", TRUE)
add("pop.growth.main.max.name", r$name, NULL, paste0("main sample; ", F_POP))
add("pop.growth.main.max", r$growth, 1, paste0("percent change 2018Q1-2026Q2, main sample; ", F_POP))
pg_main <- pg[state_id %in% main_ids][order(-growth, state_id)]
add("pop.growth.main.top2to4.names", join_and(pg_main$name[2:4]), NULL,
    paste0("main sample, growth ranks 2 to 4 from the top; ", F_POP))
bottom2to4 <- pg_main$name[(nrow(pg_main) - 1L):(nrow(pg_main) - 3L)]
add("pop.growth.main.bottom2to4.names", join_and(bottom2to4), NULL,
    paste0("main sample, growth ranks 2 to 4 from the bottom, smallest growth first; ", F_POP))
add("pop.growth.main.min.name", pg_main$name[nrow(pg_main)], NULL, paste0("main sample; ", F_POP))
add("pop.growth.main.min", pg_main$growth[nrow(pg_main)], 1,
    paste0("percent change 2018Q1-2026Q2, main sample; ", F_POP))

# national
SRC_NAT <- paste0("national(); ", F_PHONEPE)
nat_at <- function(per) nat[period == per]
for (p in c(P_FIRST, P_REF0, P_LAST)) {
  r <- nat_at(p)
  add(sprintf("nat.count.%s", p), r$count / 1e9, 2, SRC_NAT)
  add(sprintf("nat.amount.%s", p), r$amount / 1e12, 2, SRC_NAT)
  add(sprintf("nat.users.%s", p), r$users / 1e6, 1, SRC_NAT)
}
yrs <- years_between(P_REF0, P_LAST)
for (v in c("count", "amount", "users")) {
  add(sprintf("nat.%s_ratio", v), nat_at(P_LAST)[[v]] / nat_at(P_FIRST)[[v]], 0,
      paste0("2026Q2 / 2018Q1, ", SRC_NAT))
  add(sprintf("nat.%s_rate", v), annual_rate(nat_at(P_REF0)[[v]], nat_at(P_LAST)[[v]], yrs), 1,
      paste0("annual_rate() 2018Q4-2026Q2, ", SRC_NAT))
}
SRC_NATP <- "national(); data/interim/{phonepe,population}.csv"
for (p in c(P_FIRST, P_REF0, P_LAST)) {
  add(sprintf("nat.count_pc.%s", p), nat_at(p)$count_pc, 2, SRC_NATP)
  add(sprintf("nat.users_per_1000.%s", p), nat_at(p)$users_per_1000, 0, SRC_NATP)
}
add("nat.qoq.count.2020Q2", nat_at("2020Q2")$qoq_count, 1, paste0("qoq_count, ", SRC_NAT))
add("nat.qoq.amount.2020Q2", nat_at("2020Q2")$qoq_amount, 1, paste0("qoq_amount, ", SRC_NAT))
add("nat.n_qoq_neg_count", sum(nat$qoq_count < 0, na.rm = TRUE), 0,
    paste0("quarters with qoq_count < 0, ", SRC_NAT))
last8 <- tail(periods, 8)
stopifnot(identical(last8[1], "2024Q3"), identical(last8[8], P_LAST))
add("nat.qoq.count.median_last8", median(nat[period %in% last8, qoq_count]), 1,
    paste0("median qoq_count 2024Q3-2026Q2, ", SRC_NAT))
fall <- merge(at("2020Q1")[, .(state_id, c0 = count)], at("2020Q2")[, .(state_id, c1 = count)],
              by = "state_id")
add("nat.n_states_fall.2020Q2", sum(fall$c1 < fall$c0), 0,
    paste0("ids with count 2020Q2 < 2020Q1, load_part1(); ", F_PHONEPE))
add("nat.atv.2018Q1", nat_at(P_FIRST)$atv, 0, paste0("atv, ", SRC_NAT))
atv_peak <- nat[which.max(atv)]
add("nat.atv.peak", atv_peak$atv, 0, paste0("max atv, ", SRC_NAT))
add("nat.atv.peak_period", atv_peak$period, NULL, paste0("period of max atv, ", SRC_NAT))
add("nat.atv.2026Q2", nat_at(P_LAST)$atv, 0, paste0("atv, ", SRC_NAT))
atv_after <- nat[period >= atv_peak$period, atv]
add("nat.atv.n_after_peak", length(atv_after) - 1L, 0, paste0("quarters after the max atv, ", SRC_NAT))
add("nat.atv.n_fall_after_peak", sum(diff(atv_after) < 0), 0,
    paste0("quarters after the max atv with atv below the previous, ", SRC_NAT))
SRC_NATT <- "national(); data/interim/{phonepe,phonepe_types}.csv"
for (p in c(P_FIRST, P_REF0, P_LAST)) for (k in c("p2p", "retail", "utility"))
  add(sprintf("nat.share.%s.%s", k, p), 100 * nat_at(p)[[paste0("share_", k)]], 1, SRC_NATT)
r18 <- 100 * nat[substr(period, 1, 4) == "2018", share_retail]
add("nat.share.retail.min2018", min(r18), 1, paste0("min share_retail 2018Q1-2018Q4, ", SRC_NATT))
add("nat.share.retail.max2018", max(r18), 1, paste0("max share_retail 2018Q1-2018Q4, ", SRC_NATT))
r <- nat[which.max(share_utility)]
add("nat.share.utility.max", 100 * r$share_utility, 1, paste0("max share_utility, ", SRC_NATT))
add("nat.share.utility.max_period", r$period, NULL, paste0("period of max share_utility, ", SRC_NATT))
r <- nat[which.min(share_retail)]
add("nat.share.retail.min", 100 * r$share_retail, 1, paste0("min share_retail, ", SRC_NATT))
add("nat.share.retail.min_period", r$period, NULL, paste0("period of min share_retail, ", SRC_NATT))
# First quarter from which the retail share is above the P2P share in every later quarter.
below <- which(nat$share_retail <= nat$share_p2p)
stopifnot(length(below) > 0L, max(below) < nrow(nat))
add("nat.share.retail_above_p2p_from", nat$period[max(below) + 1L], NULL,
    paste0("first period of the last run of share_retail > share_p2p, ", SRC_NATT))
# Window in which the retail and P2P shares are close: GAP_FROM to the first quarter of the last run
# of share_retail > share_p2p. GAP_FROM is the first quarter after the large 2019 gaps (2019Q1 -15.9,
# 2019Q3 +11.7 percentage points).
GAP_FROM <- "2019Q4"
# The quarters of 2019 before GAP_FROM (2019Q1-2019Q3 in the text): their largest gap.
gap_pre <- nat[substr(period, 1, 4) == "2019" & period < GAP_FROM]
stopifnot(identical(gap_pre$period, c("2019Q1", "2019Q2", "2019Q3")))
gap_pre[, gap := 100 * (share_retail - share_p2p)]
add("nat.share.gap.pre.max_abs", max(abs(gap_pre$gap)), 1,
    paste0("max |share_retail - share_p2p|, percentage points, 2019Q1-2019Q3, ", SRC_NATT))
add("nat.share.gap.pre.max_period", gap_pre[which.max(abs(gap)), period], NULL,
    paste0("period of max |share_retail - share_p2p|, 2019Q1-2019Q3, ", SRC_NATT))
gap_win <- nat[period >= GAP_FROM & period <= nat$period[max(below) + 1L]]
gap_win[, gap := 100 * (share_retail - share_p2p)]
add("nat.share.gap.from", GAP_FROM, NULL, "start of the close-shares window; part1_study.R")
add("nat.share.gap.max_abs", max(abs(gap_win$gap)), 1,
    paste0("max |share_retail - share_p2p|, percentage points, gap.from to retail_above_p2p_from, ", SRC_NATT))

# national: the 2020 fall and the quarter-on-quarter series
for (p in c("2020Q1", "2020Q2", "2020Q3"))
  add(sprintf("nat.count.%s", p), nat_at(p)$count / 1e9, 2, SRC_NAT)
add("nat.qoq.count.2020Q3", nat_at("2020Q3")$qoq_count, 1, paste0("qoq_count, ", SRC_NAT))
r <- nat[which.max(qoq_count)]
add("nat.qoq.count.max", r$qoq_count, 1, paste0("max qoq_count, ", SRC_NAT))
add("nat.qoq.count.max_period", r$period, NULL, paste0("period of max qoq_count, ", SRC_NAT))
first_q <- setdiff(periods[-1], last8)
stopifnot(identical(first_q[1], "2018Q2"), identical(first_q[length(first_q)], "2024Q2"))
add("nat.qoq.count.median_first", median(nat[period %in% first_q, qoq_count]), 1,
    paste0("median qoq_count 2018Q2-2024Q2, ", SRC_NAT))
neg_amt <- nat[!is.na(qoq_amount) & qoq_amount < 0]
add("nat.n_qoq_neg_amount", nrow(neg_amt), 0, paste0("quarters with qoq_amount < 0, ", SRC_NAT))
add("nat.qoq_neg_amount.periods", join_and(neg_amt$period), NULL,
    paste0("periods with qoq_amount < 0, ", SRC_NAT))
add("nat.qoq.amount.neg_last", neg_amt[.N, qoq_amount], 1,
    paste0("qoq_amount in the last period with qoq_amount < 0, ", SRC_NAT))

# national: calendar years (annual(), the rows of national_annual.tex)
SRC_ANN <- paste0("annual(), ", SRC_NATP)
ann_n <- annual(nat)
for (i in which(!is.na(ann_n$growth_count)))
  add(sprintf("nat.ann.growth.%s", ann_n$year[i]), ann_n$growth_count[i], 1, paste0("growth_count, ", SRC_ANN))
for (y in c("2018", "2025")) {
  r <- ann_n[year == y]
  stopifnot(nrow(r) == 1L)
  add(sprintf("nat.ann.count_pc.%s", y), r$count_pc, 1, paste0("count_pc, ", SRC_ANN))
}
r <- ann_n[year == "2025"]
add("nat.ann.count.2025", r$count / 1e9, 2, paste0("count, ", SRC_ANN))
add("nat.ann.amount.2025", r$amount / 1e12, 2, paste0("amount, ", SRC_ANN))
add("nat.ann.atv.2025", r$atv, 0, paste0("atv, ", SRC_ANN))
r <- ann_n[which.max(atv)]
add("nat.ann.atv.max", r$atv, 0, paste0("max atv, ", SRC_ANN))
add("nat.ann.atv.max_year", r$year, NULL, paste0("year of max atv, ", SRC_ANN))
# Registered users added in each full calendar year after the first (year-end minus previous year-end).
# Full years: calendar years with four quarters in the national series.
q_per_year <- nat[, .N, by = .(year = substr(period, 1, 4))]
yr_full <- ann_n[year %in% q_per_year[N == 4L, year]]
stopifnot(nrow(yr_full) >= 2L)
users_added <- diff(yr_full$users) / 1e6
add("nat.users_added.first_year", yr_full$year[2], NULL, paste0("first year of diff(users), ", SRC_ANN))
add("nat.users_added.last_year", yr_full$year[nrow(yr_full)], NULL, paste0("last full year, ", SRC_ANN))
add("nat.users_added.min", min(users_added), 1, paste0("min diff(year-end users), million, ", SRC_ANN))
add("nat.users_added.max", max(users_added), 1, paste0("max diff(year-end users), million, ", SRC_ANN))

# average transaction value across states (spread(), main sample)
SRC_ATV <- paste0("spread(atv), main sample, ", SRC_ALL)
sp_atv <- spread(dt, "atv", main_ids)
sp_atv[, iqr := q3 - q1]
for (q in c(P_REF0, P_LAST)) {
  add(sprintf("st.atv.median.%s", q), sp_atv[period == q, med], 0, SRC_ATV)
  add(sprintf("st.atv.iqr.%s", q), sp_atv[period == q, iqr], 0, paste0("q3 - q1, ", SRC_ATV))
}
r <- sp_atv[which.max(med)]
add("st.atv.median.max", r$med, 0, paste0("max median, ", SRC_ATV))
add("st.atv.median.max_period", r$period, NULL, paste0("period of max median, ", SRC_ATV))
r <- sp_atv[which.max(iqr)]
add("st.atv.iqr.max", r$iqr, 0, paste0("max q3 - q1, ", SRC_ATV))
add("st.atv.iqr.max_period", r$period, NULL, paste0("period of max q3 - q1, ", SRC_ATV))

# states (main sample)
SRC_MAIN <- paste0("main sample, ", SRC_ALL)
for (q in c(P_REF0, P_LAST)) {
  d <- at(q, main_ids)
  hi <- pick(d, "count_pc", TRUE); lo <- pick(d, "count_pc", FALSE)
  add(sprintf("st.count_pc.max.name.%s", q), hi$name, NULL, SRC_MAIN)
  add(sprintf("st.count_pc.max.%s", q), hi$count_pc, 2, SRC_MAIN)
  add(sprintf("st.count_pc.min.name.%s", q), lo$name, NULL, SRC_MAIN)
  add(sprintf("st.count_pc.min.%s", q), lo$count_pc, 2, SRC_MAIN)
  add(sprintf("st.count_pc.maxmin.%s", q), hi$count_pc / lo$count_pc, 1, paste0("max/min, ", SRC_MAIN))
}
d_last <- at(P_LAST, main_ids)
for (v in c("users_per_1000", "intensity")) for (s in c("max", "min")) {
  r <- pick(d_last, v, s == "max")
  add(sprintf("st.%s.%s.name", v, s), r$name, NULL, paste0("2026Q2, ", SRC_MAIN))
  add(sprintf("st.%s.%s", v, s), r[[v]], if (v == "intensity") 1 else 0, paste0("2026Q2, ", SRC_MAIN))
}
d_ref0 <- at(P_REF0, main_ids)
stopifnot(identical(d_ref0$state_id, d_last$state_id))
add("st.rank_cor", spearman(d_ref0$count_pc, d_last$count_pc), 3,
    paste0("spearman() count_pc 2018Q4 vs 2026Q2, ", SRC_MAIN))
add("st.reach_intensity_cor", spearman(d_last$users_per_1000, d_last$intensity), 3,
    paste0("spearman() users_per_1000 vs intensity 2026Q2, ", SRC_MAIN))
add("st.reach_intensity_cor.2018Q4", spearman(d_ref0$users_per_1000, d_ref0$intensity), 3,
    paste0("spearman() users_per_1000 vs intensity 2018Q4, ", SRC_MAIN))
# max/min ratios from unrounded values
for (v in c("users_per_1000", "intensity"))
  add(sprintf("st.%s.maxmin", v), max(d_last[[v]]) / min(d_last[[v]]), 1,
      paste0("max/min 2026Q2, ", SRC_MAIN))
for (q in c(P_REF0, P_LAST)) {
  d <- at(q, main_ids)
  add(sprintf("st.count_pc.median.%s", q), median(d$count_pc), 2, paste0("median, ", SRC_MAIN))
  add(sprintf("st.n_above_nat.%s", q), sum(d$count_pc > nat_at(q)$count_pc), 0,
      paste0("ids with count_pc above national(), ", SRC_MAIN))
}
ord_last <- d_last[order(-count_pc, state_id)]
add("st.count_pc.top5.names.2026Q2", join_and(ord_last$name[1:5]), NULL, paste0("2026Q2, ", SRC_MAIN))
add("st.count_pc.bottom5.names.2026Q2", join_and(rev(tail(ord_last$name, 5))), NULL,
    paste0("2026Q2, lowest first, ", SRC_MAIN))
# Annual rate of growth of count_pc per state, 2018Q4-2026Q2.
st_rate <- data.table(state_id = d_last$state_id, name = d_last$name,
                      rate = annual_rate(d_ref0$count_pc, d_last$count_pc, yrs))
for (s in c("max", "min")) {
  r <- pick(st_rate, "rate", s == "max")
  add(sprintf("st.count_pc_rate.%s.name", s), r$name, NULL, paste0("annual_rate() 2018Q4-2026Q2, ", SRC_MAIN))
  add(sprintf("st.count_pc_rate.%s", s), r$rate, 1, paste0("annual_rate() 2018Q4-2026Q2, ", SRC_MAIN))
}
# Rank of count_pc (1 = largest), 2018Q4 and 2026Q2; places moved up (positive) or down.
rk <- data.table(name = d_last$name, r0 = rank(-d_ref0$count_pc), r1 = rank(-d_last$count_pc))
rk[, up := r0 - r1]
stopifnot(all(rk$up == round(rk$up)))   # no tied ranks (average ranks would give fractions)
rise <- rk[up == max(up)]
stopifnot(nrow(rise) == 1L)
add("st.rank_rise.name", rise$name, NULL, paste0("largest r0 - r1, rank(-count_pc), ", SRC_MAIN))
add("st.rank_rise.r0", rise$r0, 0, paste0("rank 2018Q4, rank(-count_pc), ", SRC_MAIN))
add("st.rank_rise.r1", rise$r1, 0, paste0("rank 2026Q2, rank(-count_pc), ", SRC_MAIN))
fall_rk <- rk[up == min(up)][order(name)]
add("st.rank_fall.names", join_and(fall_rk$name), NULL, paste0("smallest r0 - r1, rank(-count_pc), ", SRC_MAIN))
add("st.rank_fall.places", -min(rk$up), 0, paste0("-(r0 - r1), rank(-count_pc), ", SRC_MAIN))
RANK_MOVE <- 3
add("st.rank_move.threshold", RANK_MOVE, 0, "places; part1_study.R")
add("st.n_rank_move_small", sum(abs(rk$up) <= RANK_MOVE), 0,
    paste0("ids with |r0 - r1| <= threshold, rank(-count_pc), ", SRC_MAIN))
# Reach rank minus intensity rank in 2026Q2 (1 = largest).
rg <- data.table(name = d_last$name, gap = rank(-d_last$users_per_1000) - rank(-d_last$intensity))
stopifnot(all(rg$gap == round(rg$gap)))   # no tied ranks
g_r <- rg[gap == min(gap)][order(name)]; g_i <- rg[gap == max(gap)][order(name)]
add("st.ri_gap.reach.names", join_and(g_r$name), NULL,
    paste0("smallest reach rank - intensity rank 2026Q2, ", SRC_MAIN))
add("st.ri_gap.reach.places", -min(rg$gap), 0, paste0("intensity rank - reach rank 2026Q2, ", SRC_MAIN))
add("st.ri_gap.intensity.names", join_and(g_i$name), NULL,
    paste0("largest reach rank - intensity rank 2026Q2, ", SRC_MAIN))
add("st.ri_gap.intensity.places", max(rg$gap), 0, paste0("reach rank - intensity rank 2026Q2, ", SRC_MAIN))
for (s in c("max", "min")) {
  r <- pick(d_last, "amount_pc", s == "max")
  add(sprintf("st.amount_pc.%s.name", s), r$name, NULL, paste0("2026Q2, ", SRC_MAIN))
  add(sprintf("st.amount_pc.%s", s), r$amount_pc, 0, paste0("amount_pc 2026Q2, ", SRC_MAIN))
}
add("st.amount_pc.maxmin", max(d_last$amount_pc) / min(d_last$amount_pc), 1,
    paste0("max/min 2026Q2, ", SRC_MAIN))
pop_top <- d_last[order(-population, state_id)][1:3]
add("st.pop.top3.names", join_and(pop_top$name), NULL, paste0("largest population 2026Q2, ", SRC_MAIN))
add("st.pop.top3.n_below_median", sum(pop_top$count_pc < median(d_last$count_pc)), 0,
    paste0("of st.pop.top3, count_pc below the median 2026Q2, ", SRC_MAIN))
for (s in c("max", "min")) {
  r <- pick(d_last, "atv", s == "max")
  add(sprintf("st.atv.%s.name", s), r$name, NULL, paste0("2026Q2, ", SRC_MAIN))
  add(sprintf("st.atv.%s", s), r$atv, 0, paste0("atv 2026Q2, ", SRC_MAIN))
}

# concentration (35 ids)
SRC_CONC <- paste0("top_k(), 35 ids, ", SRC_ALL)
conc <- list(); n_half <- list()
for (q in c(P_REF0, P_LAST)) {
  d <- at(q)
  conc[[q]] <- top_k(dt, q, 5)
  add(sprintf("conc.top5.names.%s", q), join_and(name_of(top_ids(dt, q, 5))), NULL, SRC_CONC)
  add(sprintf("conc.top5.count.%s", q), conc[[q]][variable == "count", share], 1, SRC_CONC)
  add(sprintf("conc.top5.population.%s", q), conc[[q]][variable == "population", share], 1, SRC_CONC)
  add(sprintf("conc.top5.amount.%s", q), conc[[q]][variable == "amount", share], 1, SRC_CONC)
  add(sprintf("conc.top5.users.%s", q), conc[[q]][variable == "users", share], 1, SRC_CONC)
  n_half[[q]] <- n_for_share(d$count, 0.5)
  add(sprintf("conc.n_half.%s", q), n_half[[q]], 0, paste0("n_for_share(count, 0.5), 35 ids, ", SRC_ALL))
}

# decomposition (main sample)
SRC_DEC <- paste0("decompose(), main sample, ", SRC_ALL)
dec <- decompose_series(dt, main_ids)
for (q in c(P_REF0, P_LAST)) {
  r <- dec[period == q]
  for (v in c("var_y", "var_r", "var_i", "reach", "intensity"))
    add(sprintf("dec.%s.%s", q, v), r[[v]], 3, SRC_DEC)
}
add("dec.var_y.max", max(dec$var_y), 3, paste0("max over quarters, ", SRC_DEC))
add("dec.var_y.max_period", dec[which.max(var_y), period], NULL,
    paste0("period of max var_y, ", SRC_DEC))
add("dec.2018Q1.reach", dec[period == P_FIRST, reach], 3, SRC_DEC)
add("dec.intensity_first_above", dec[intensity > reach, period][1], NULL,
    paste0("first period with intensity > reach, ", SRC_DEC))
# First quarter from which intensity > reach in every later quarter.
dec_below <- which(dec$intensity <= dec$reach)
stopifnot(length(dec_below) > 0L, max(dec_below) < nrow(dec))
dec_from <- dec$period[max(dec_below) + 1L]
add("dec.intensity_above_from", dec_from, NULL,
    paste0("first period of the last run of intensity > reach, ", SRC_DEC))
add("dec.n_var_r_rise", sum(diff(dec$var_r) > 0), 0, paste0("quarters with var_r above the previous, ", SRC_DEC))
late <- dec[substr(period, 1, 4) > "2020"]
add("dec.var_y.max_after2020", max(late$var_y), 3, paste0("max var_y 2021Q1-2026Q2, ", SRC_DEC))
add("dec.var_y.max_after2020_period", late[which.max(var_y), period], NULL,
    paste0("period of max var_y 2021Q1-2026Q2, ", SRC_DEC))
add("dec.var_i.max", max(dec$var_i), 3, paste0("max over quarters, ", SRC_DEC))
add("dec.var_i.max_period", dec[which.max(var_i), period], NULL, paste0("period of max var_i, ", SRC_DEC))

# retail share (main sample, 2026Q2)
add("retail.cor.2026Q2", spearman(d_last$share_retail, d_last$count_pc), 3,
    paste0("spearman() share_retail vs count_pc 2026Q2, ", SRC_MAIN))
for (s in c("max", "min")) {
  r <- pick(d_last, "share_retail", s == "max")
  add(sprintf("retail.%s.name", s), r$name, NULL, paste0("2026Q2, ", SRC_MAIN))
  add(sprintf("retail.%s", s), 100 * r$share_retail, 1, paste0("share_retail 2026Q2, ", SRC_MAIN))
}
add("retail.cor.2018Q4", spearman(d_ref0$share_retail, d_ref0$count_pc), 3,
    paste0("spearman() share_retail vs count_pc 2018Q4, ", SRC_MAIN))
add("retail.n_other_above", sum(at(P_LAST, other_ids)$share_retail > max(d_last$share_retail)), 0,
    paste0("other ids with share_retail 2026Q2 above the main-sample max, load_part1(); ", F_TYPES))

# ---- guards: statements in the text of part1.tex that depend on the data ----
# Each guard stops the build when a data change makes the sentence wrong. The comment names the
# section of part1.tex and the statement.
guard <- function(ok, what) if (!isTRUE(ok)) stop("text statement no longer true: ", what)
nat_full <- ann_n[year %in% yr_full$year]
g_ann <- setNames(nat_full$growth_count, nat_full$year)
# 2.1, 3.3, 6
guard(sum(users_step$d < 0) == 0L, "2.1, 3.3, 6 registered users never fall from one quarter to the next")
# 3.2, 3.3
guard(zf$n_flagged == 1L, "3.2, 3.3 one absent type entry (singular 'entry', 'is absent')")
# 3.3
guard(all(abs(c(ncp_diff(2018L), ncp_diff(2027L))) < ncp_bound) && ncp_n_rows(2027L) == ncp_n_rows(2018L),
      "3.3 NCP differences smaller than the largest difference from rounding")
# 3.4
guard(pg_main$name[1] == pick(pg[state_id %in% main_ids], "growth", TRUE)$name,
      "3.4 pop.growth.main.max is rank 1 of the main sample")
guard(!pg_main$name[nrow(pg_main)] %in% bottom2to4 && !pg_main$name[1] %in% pg_main$name[2:4],
      "3.4 ranks 2 to 4 exclude the main-sample maximum and minimum")
# 4.1
guard(nat_at(P_LAST)$users / nat_at(P_REF0)$users < nat_at(P_LAST)$count / nat_at(P_REF0)$count &
        nat_at(P_LAST)$amount / nat_at(P_REF0)$amount < nat_at(P_LAST)$count / nat_at(P_REF0)$count,
      "4.1 count grows faster than the amount and the users, 2018Q4-2026Q2")
guard(max(users_added) / min(users_added) < 1.2, "4.1 users added: almost the same size each year")
inc <- diff(nat$count)
guard(mean(inc[substr(nat$period[-1], 1, 4) <= "2020"]) < mean(inc[substr(nat$period[-1], 1, 4) >= "2021"]),
      "4.1 count rises slowly up to 2020 and more steeply after 2021")
guard(all(diff(nat_full$count) > 0), "4.1 the annual count increases in each full year")
guard(g_ann[["2020"]] < g_ann[["2019"]] && g_ann[["2020"]] < g_ann[["2021"]],
      "4.1 rate of 2020 lower than the rates of 2019 and 2021")
guard(all(diff(g_ann[as.character(2021:2025)]) < 0), "4.1 after 2021 the rate decreases in each year")
i_max <- which.max(ann_n$atv)
guard(all(diff(ann_n$atv[i_max:nrow(ann_n)]) < 0), "4.1 annual average value decreases in each later year")
# 4.2
guard(identical(nat[!is.na(qoq_count) & qoq_count < 0, period], "2020Q2"),
      "4.2 2020Q2 is the only quarter with a fall in the national count")
guard(nat_at("2020Q2")$qoq_amount > nat_at("2020Q2")$qoq_count, "4.2 amount falls less than the count in 2020Q2")
guard(nat_at("2020Q3")$count > nat_at("2020Q1")$count, "4.2 count of 2020Q3 more than the count of 2020Q1")
guard(substr(nat[which.max(qoq_count), period], 1, 4) == "2018", "4.2 growth largest in the first quarters")
guard(median(nat[period %in% last8, qoq_count]) < median(nat[period %in% first_q, qoq_count]),
      "4.2 lower growth in the last eight quarters")
guard(nrow(neg_amt) == 2L && neg_amt[1, period] == "2020Q2" && nat_at(neg_amt[2, period])$qoq_count >= 0,
      "4.2 amount falls in two quarters; the count does not fall in the later one")
# 4.3
guard(nat_at("2020Q2")$atv > nat_at("2020Q1")$atv, "4.3 average value rises in 2020Q2")
guard(nat_at(P_LAST)$atv < nat_at(P_FIRST)$atv, "4.3 average value of 2026Q2 lower than that of 2018Q1")
guard(sp_atv[period == P_LAST, iqr] < sp_atv[period == P_REF0, iqr], "4.3 state values closer in 2026Q2 than in 2018Q4")
from_rmin <- nat[period >= nat[which.min(share_retail), period]]
guard(mean(diff(from_rmin$share_retail) > 0) > 0.5 && mean(diff(from_rmin$atv) < 0) > 0.5,
      "4.3 from the retail minimum, the retail share rises and the average value falls in most quarters")
# 4.4
guard(max(abs(gap_pre$gap)) > max(abs(gap_win$gap)), "4.4 gaps of 2019Q1-2019Q3 larger than in the window")
guard(nat[which.max(share_utility), period] == nat[which.min(share_retail), period],
      "4.4 utility maximum and retail minimum in the same quarter")
guard(nat[which.max(share_utility), period] %in% gap_win$period, "4.4 utility maximum inside the close-shares window")
guard(sum(diff(sign(gap_win$gap)) != 0) >= 2L, "4.4 order of retail and P2P shares changes more than once")
guard(all(nat[period >= nat$period[max(below) + 1L], share_retail > share_utility]),
      "4.4 retail is the largest type from retail_above_p2p_from")
guard(abs(nat_at(P_LAST)$share_retail - nat_at(P_FIRST)$share_retail) < 0.01,
      "4.4 retail share of 2026Q2 close to that of 2018Q1 (within 1 percentage point)")
# 5.1
guard(all(d_last$count_pc > d_ref0$count_pc), "5.1 every main-sample state higher in 2026Q2 than in 2018Q4")
top5 <- ord_last[1:5]; bot5 <- tail(ord_last, 5)
guard(max(bot5$users_per_1000) < min(top5$users_per_1000) && max(bot5$intensity) < min(top5$intensity),
      "5.1 bottom five: lower reach and intensity than each of the top five")
# The eight states of the North Eastern Region (Ministry of Development of North Eastern Region).
NORTH_EAST <- c("arunachal_pradesh", "assam", "manipur", "meghalaya", "mizoram", "nagaland", "sikkim",
                "tripura")
guard(all(bot5$state_id %in% NORTH_EAST), "5.1 the five lowest states are all in the north-east")
guard(all(bot5$users_per_1000 < median(d_last$users_per_1000)) && all(bot5$intensity < median(d_last$intensity)),
      "5.4 north-eastern states at the lower left: reach and intensity below the median")
rr <- (max(d_last$amount_pc) / min(d_last$amount_pc)) / (max(d_last$count_pc) / min(d_last$count_pc))
guard(rr > 0.75 && rr < 1 / 0.75, "5.1 amount and count max/min ratios of similar size")
guard(sum(pop_top$count_pc < median(d_last$count_pc)) >= 1L, "5.1 a large state can have a low count per person")
# 5.2
cs <- function(q, v) conc[[q]][variable == v, share]
guard(cs(P_REF0, "count") > cs(P_REF0, "population") && cs(P_LAST, "count") > cs(P_LAST, "population"),
      "5.2 count share above population share in both quarters")
guard(cs(P_LAST, "count") - cs(P_LAST, "population") > cs(P_REF0, "count") - cs(P_REF0, "population"),
      "5.2 the difference is larger in 2026Q2")
guard(abs(cs(P_LAST, "users") - cs(P_LAST, "population")) < abs(cs(P_LAST, "count") - cs(P_LAST, "users")),
      "5.2 users share closer to the population share than to the count share")
guard(!setequal(top_ids(dt, P_REF0, 5), top_ids(dt, P_LAST, 5)), "5.2 the set of five units changes")
guard(n_half[[P_LAST]] < n_half[[P_REF0]] && cs(P_LAST, "count") > cs(P_REF0, "count"),
      "5.2 fewer units for half of the count, larger top-five share at the end")
guard(cs(P_LAST, "count") > 50, "6 five units have most of the count in 2026Q2")
# 5.3
guard(pick(d_ref0, "count_pc", TRUE)$state_id %in% top5$state_id, "5.3 2018Q4 leader still in the top five")
guard(nrow(fall_rk) > 1L, "5.3 plural verb after st.rank_fall.names")
guard(sum(abs(rk$up) <= RANK_MOVE) > nrow(rk) / 2, "5.3 most units move few places")
# 5.4
guard(nrow(g_r) > 1L && nrow(g_i) == 1L, "5.4 verbs after st.ri_gap.reach.names (plural) and .intensity.names")
guard(max(d_last$intensity) / min(d_last$intensity) > max(d_last$users_per_1000) / min(d_last$users_per_1000) &&
        dec[period == P_LAST, var_i] > dec[period == P_LAST, var_r], "5.4 states differ more in intensity than reach")
ri_cor <- spearman(d_last$users_per_1000, d_last$intensity)
guard(ri_cor > 0 && ri_cor < 0.7 && ri_cor > spearman(d_ref0$users_per_1000, d_ref0$intensity),
      "5.4 reach and intensity go together, not closely; higher correlation in 2026Q2")
# 5.5
d0 <- dec[period == P_REF0]; d1 <- dec[period == P_LAST]
guard(d0$reach > d0$intensity && d1$intensity > d1$reach, "5.5 reach larger in 2018Q4, intensity in 2026Q2")
guard(all(diff(dec$var_r) < 0), "5.5 var(r) decreases in every quarter")
guard(d1$var_i > d0$var_i && d1$cov_ri > d0$cov_ri, "5.5 var(i) and 2cov(r,i) increase")
guard(late[which.max(var_y), period] == dec[which.max(var_i), period],
      "5.5 largest var(y) after 2020 in the quarter of the largest var(i)")
# 5.6
guard(pick(d_last, "share_retail", FALSE)$state_id == pick(d_last, "count_pc", FALSE)$state_id,
      "5.6 lowest retail share is in the state with the lowest count per person")
rc0 <- spearman(d_ref0$share_retail, d_ref0$count_pc); rc1 <- spearman(d_last$share_retail, d_last$count_pc)
guard(rc1 > 0 && sign(rc0) != sign(rc1), "5.6 positive in 2026Q2, opposite sign in 2018Q4")
guard(sum(at(P_LAST, other_ids)$share_retail > max(d_last$share_retail)) > 1L,
      "5.6 plural verb after retail.n_other_above")

write_numbers(rbindlist(entries), file.path(OUT, "numbers.tex"),
              generator = "Rscript src/R/part1_study.R")

# ---- tables ----
tab <- function(name) file.path(TAB_DIR, name)
dash <- function(x, digits) ifelse(is.na(x), "--", fmt(ifelse(is.na(x), 0, x), digits))
dagger <- "$^\\dagger$"
# A full-width note row after \bottomrule.
add_note <- function(path, ncol, text) {
  l <- readLines(path)
  i <- which(l == "\\bottomrule")
  stopifnot(length(i) == 1L)
  writeLines(append(l, sprintf("\\multicolumn{%d}{@{}l}{\\footnotesize %s}\\\\", ncol, text), i), path)
}

# units.tex
xw_join <- function(x, id) {
  n <- sort(x[state_id == id, raw_name], method = "radix")
  if (length(n) == 0L) stop("no raw name for ", id)
  paste(latex_escape(n), collapse = "; ")
}
units <- labels[order(name)]
write_tabular(
  units[, .(latex_escape(name), paste0("\\texttt{", latex_escape(state_id), "}"), code,
            ifelse(in_main, "Yes", "No"),
            vapply(state_id, xw_join, "", x = x_phonepe), vapply(state_id, xw_join, "", x = x_ncp))],
  tab("units.tex"),
  align = paste0("@{}>{\\raggedright\\arraybackslash}p{2.8cm} l l l ",
                 ">{\\raggedright\\arraybackslash}p{2.8cm} >{\\raggedright\\arraybackslash}p{2.8cm}@{}"),
  header = "Name & \\texttt{state\\_id} & Code & Main & PhonePe raw names & NCP raw names \\\\",
  env = "longtable",
  caption = sprintf(paste0(
    "The %d state units of the panel: display name, \\texttt{state\\_id}, two-letter code ",
    "\\citep{iso3166_2_in}, membership of the main sample of %d, and the raw names that map to ",
    "the unit in the PhonePe Pulse hover file for %s \\citep{phonepe_pulse} and in NCP 2020, ",
    "Table 8 \\citep{ncp2020}. Source: state crosswalk (\\texttt{state\\_crosswalk.csv}) and ",
    "state labels (\\texttt{state\\_labels.csv})."), nrow(labels), length(main_ids), P_LAST),
  label = "tab:units")

# checks.tex
CHECK_TEXT <- c(
  type_sum_equals_state_total =
    "PhonePe: sum of the three type counts against the state count (state-quarters)",
  absent_type_set_to_zero =
    "PhonePe: absent type entries set to zero (state-quarter-type entries)",
  national_count_equals_state_sum =
    "PhonePe: national count file against the sum of the state counts (quarters)",
  national_users_equals_state_sum =
    "PhonePe: national user file against the sum of the state user counts (quarters)")
# "lakshadweep 2018Q1 Utility" -> ". Flagged: Lakshadweep, 2018Q1, Utility"
note_text <- function(note) {
  if (is.na(note) || note == "") return("")
  w <- strsplit(note, " ", fixed = TRUE)[[1]]
  if (w[1] %in% labels$state_id) w[1] <- name_of(w[1])
  paste0(". Flagged: ", latex_escape(paste(w, collapse = ", ")))
}
chk <- checks[match(CHECK_KEYS, check)]
chk_rows <- chk[, .(text = paste0(CHECK_TEXT[check], vapply(note, note_text, "")),
                    items = fmt(n_items, 0), flagged = fmt(n_flagged, 0), value = fmt(max_abs_diff, 0))]
chk_rows <- rbind(chk_rows, data.table(
  text = "PhonePe: registered users lower than in the previous quarter (pairs of consecutive quarters)",
  items = fmt(nrow(users_step), 0), flagged = fmt(sum(users_step$d < 0), 0), value = "--"))
pop_rows <- rbindlist(c(
  lapply(c(2018L, 2027L), function(y) data.table(
    text = sprintf("NCP 2020, %d: sum of the state rows divided by the India row (state rows)", y),
    items = fmt(ncp_n_rows(y), 0), flagged = "--", value = fmt(ncp_ratio(y), 7))),
  list(data.table(text = sprintf("Population: missing cells in the panel (%d ids $\\times$ %d quarters)",
                                 nrow(labels), length(periods)),
                  items = fmt(nrow(pop_grid), 0), flagged = fmt(pop_missing, 0), value = "--"))))
write_tabular(rbind(chk_rows, pop_rows), tab("checks.tex"),
              align = "@{}>{\\raggedright\\arraybackslash}p{9.2cm} r r r@{}",
              header = "Check & Items & Flagged & Difference or ratio \\\\")

# national_annual.tex
ann <- annual(nat)
write_tabular(
  ann[, .(year, fmt(count / 1e9, 2), fmt(amount / 1e12, 2), fmt(users / 1e6, 1), fmt(count_pc, 1),
          fmt(atv, 0), dash(growth_count, 1))],
  tab("national_annual.tex"), align = "@{}l r r r r r r@{}",
  header = c(" & Count & Amount & Users at & Count & Average & Growth \\\\",
             " & (billion) & (Rs.\\ lakh & year end & per & value & of count \\\\",
             "Year & & crore) & (million) & person & (Rs.) & (percent) \\\\"))

# state_2026q2.tex
st <- at(P_LAST)[order(-count_pc, state_id)]
write_tabular(
  st[, .(paste0(latex_escape(name), ifelse(in_main, "", dagger)), fmt(population / 1e6, 2),
         fmt(count_pc, 1), fmt(amount_pc, 0), fmt(users_per_1000, 0), fmt(intensity, 1), fmt(atv, 0),
         fmt(100 * share_retail, 1))],
  tab("state_2026q2.tex"),
  align = "@{}>{\\raggedright\\arraybackslash}p{2.95cm} r r r r r r r@{}",
  header = c(" & Population & Count & Amount & Users & Count & Average & Retail \\\\",
             " & (million) & per & per person & per 1000 & per & value & share \\\\",
             "Name & & person & (Rs.) & persons & user & (Rs.) & (percent) \\\\"))
add_note(tab("state_2026q2.tex"), 8, paste0(dagger, " Union territory outside the main sample."))

# concentration.tex
CONC_TEXT <- c(count = "Transaction count", amount = "Amount", users = "Registered users",
               population = "Population")
conc_rows <- data.table(
  text = c(paste0(CONC_TEXT, " (percent)"), "Units needed for half of all transactions"),
  a = c(fmt(conc[[P_REF0]][match(names(CONC_TEXT), variable), share], 1),
        fmt(n_half[[P_REF0]], 0)),
  b = c(fmt(conc[[P_LAST]][match(names(CONC_TEXT), variable), share], 1),
        fmt(n_half[[P_LAST]], 0)))
write_tabular(conc_rows, tab("concentration.tex"), align = "@{}l r r@{}",
              header = sprintf("Share of the top five (by count) & %s & %s \\\\", P_REF0, P_LAST))

# decomposition.tex
dq <- dec[match(c(P_REF0, P_LAST), period)]
dec_rows <- data.table(
  text = c("$n$", "$\\operatorname{var}(y)$", "$\\operatorname{var}(r)$", "$\\operatorname{var}(i)$",
           "$2\\operatorname{cov}(r,i)$", "Reach contribution", "Intensity contribution"),
  a = c(fmt(dq$n[1], 0), fmt(c(dq$var_y[1], dq$var_r[1], dq$var_i[1], 2 * dq$cov_ri[1], dq$reach[1],
                                 dq$intensity[1]), 3)),
  b = c(fmt(dq$n[2], 0), fmt(c(dq$var_y[2], dq$var_r[2], dq$var_i[2], 2 * dq$cov_ri[2], dq$reach[2],
                                 dq$intensity[2]), 3)))
write_tabular(dec_rows, tab("decomposition.tex"), align = "@{}l r r@{}",
              header = sprintf(" & %s & %s \\\\", P_REF0, P_LAST))

# ---- figures ----
nat[, t := period_num(period)]
year_breaks <- 2018:2026
x_years <- scale_x_continuous(breaks = year_breaks, labels = year_breaks)
x_years2 <- scale_x_continuous(breaks = seq(2018, 2026, 2))
X_QUARTER <- "Quarter (labelled at Q1)"
group_cols <- c(main = ink, other = col_other)

axis_size <- 0.8 * 9
# Width of each string and the height of one line, in inches, at size_pt (PDF sans font).
text_size_in <- function(x, size_pt = axis_size) {
  pdf(NULL); on.exit(dev.off())
  par(ps = size_pt, family = "sans")
  list(w = strwidth(x, units = "inches"), h = strheight("M", units = "inches"))
}
# Names as y-axis text, grey for the other union territories. ggplot2 does not support a vector of
# colours in element_text(), so the names are text layers at the left edge of the panel.
# x_edge: -Inf on a linear x scale; on a log scale, the lower limit of the scale (with no expansion).
name_axis <- function(d, x_edge = -Inf) {
  lab <- paste0(d$name, "  ")
  list(geom_text(data = transform(d, x_edge = x_edge, lab = lab),
                 aes(x = x_edge, y = name, label = lab, colour = group),
                 hjust = 1, size = axis_size / .pt, inherit.aes = FALSE),
       coord_cartesian(clip = "off"),
       theme(axis.text.y = element_blank(),
             plot.margin = margin(5.5, 5.5, 5.5, 72 * max(text_size_in(lab)$w) + 5.5, "pt")))
}

# national_totals.pdf
MEAS <- c(count = "Transactions in the quarter\n(billion)",
          amount = "Amount in the quarter\n(Rs. lakh crore)",
          users = "Registered users at quarter end\n(million)")
tot <- rbind(nat[, .(t, measure = MEAS[["count"]], v = count / 1e9)],
             nat[, .(t, measure = MEAS[["amount"]], v = amount / 1e12)],
             nat[, .(t, measure = MEAS[["users"]], v = users / 1e6)])
tot[, measure := factor(measure, levels = MEAS)]
p <- ggplot(tot, aes(t, v)) +
  geom_line(colour = col_1, linewidth = 0.8) +
  facet_wrap(~measure, nrow = 1, scales = "free_y") +
  x_years2 + scale_y_continuous(limits = c(0, NA)) +
  labs(x = X_QUARTER, y = "Value (unit in panel title)") +
  theme_study() + theme(panel.spacing = unit(1, "lines"))
save_fig(p, "national_totals.pdf", h = 2.9)

# national_growth.pdf
GROW <- c(qoq_count = "Transaction count", qoq_amount = "Amount")
gr <- rbind(nat[!is.na(qoq_count), .(t, measure = GROW[["qoq_count"]], v = qoq_count)],
            nat[!is.na(qoq_amount), .(t, measure = GROW[["qoq_amount"]], v = qoq_amount)])
gr[, measure := factor(measure, levels = GROW)]
p <- ggplot(gr, aes(t, v)) +
  geom_col(fill = col_1, width = 0.2) +
  geom_hline(yintercept = 0, colour = ink, linewidth = 0.3) +
  facet_wrap(~measure, nrow = 1) +
  x_years2 +
  labs(x = X_QUARTER, y = "Change from the previous quarter (percent)") +
  theme_study() + theme(panel.spacing = unit(1, "lines"))
save_fig(p, "national_growth.pdf", h = 3.0)

# avg_value.pdf
sp <- spread(dt, "atv", main_ids)
sp[, t := period_num(period)]
ATV_LAB <- c(nat = "India (national total)", med = "State median (main sample)",
             iqr = "Interquartile range of states (main sample)")
p <- ggplot() +
  geom_ribbon(data = sp, aes(t, ymin = q1, ymax = q3, fill = ATV_LAB[["iqr"]]), alpha = 0.2) +
  geom_line(data = sp, aes(t, med, colour = ATV_LAB[["med"]]), linewidth = 0.8) +
  geom_line(data = nat, aes(t, atv, colour = ATV_LAB[["nat"]]), linewidth = 0.8) +
  scale_colour_manual(values = setNames(c(col_2, col_1), ATV_LAB[c("nat", "med")]),
                      breaks = unname(ATV_LAB[c("nat", "med")]), name = NULL) +
  scale_fill_manual(values = setNames(col_1, ATV_LAB[["iqr"]]), name = NULL) +
  guides(colour = guide_legend(order = 1), fill = guide_legend(order = 2)) +
  x_years + scale_y_continuous(labels = scales::label_comma()) +
  labs(x = X_QUARTER, y = "Average transaction value (Rs.)") +
  theme_study() + theme(legend.position = "top")
save_fig(p, "avg_value.pdf", h = 3.2)

# type_shares.pdf
TYPE_LAB <- c(retail = "Retail (merchant payments)", p2p = "P2P (person to person)",
              utility = "Utility (recharges, bills)")
sh <- rbindlist(lapply(names(TYPE_LAB), function(k)
  nat[, .(t, type = TYPE_LAB[[k]], v = 100 * get(paste0("share_", k)))]))
sh[, type := factor(type, levels = TYPE_LAB)]
type_cols <- setNames(c(col_1, col_2, col_3), TYPE_LAB)
ends <- sh[t == max(t)]
lab_t <- max(nat$t) + 0.15
p <- ggplot(sh, aes(t, v, colour = type)) +
  geom_line(linewidth = 0.8) +
  geom_text(data = ends, aes(x = lab_t, label = type), hjust = 0, size = axis_size / .pt) +
  scale_colour_manual(values = type_cols, guide = "none") +
  x_years + scale_y_continuous(limits = c(0, 100)) +
  coord_cartesian(xlim = c(2018, 2026.25), clip = "off") +
  labs(x = X_QUARTER, y = "Share of the transaction count (percent)") +
  theme_study() + theme(plot.margin = margin(5.5, 120, 5.5, 5.5, "pt"))
save_fig(p, "type_shares.pdf", h = 3.2)

# state_count_pc.pdf
dot <- merge(at(P_REF0)[, .(state_id, x0 = count_pc)],
             at(P_LAST)[, .(state_id, name, group, x1 = count_pc)], by = "state_id")
dot[, name := factor(name, levels = name[order(x1, -rank(state_id))])]
dot_lims <- range(dot$x0, dot$x1) * c(1 / 1.3, 1.3)
dot_long <- rbind(dot[, .(name, quarter = P_REF0, x = x0)], dot[, .(name, quarter = P_LAST, x = x1)])
p <- ggplot(dot) +
  geom_segment(aes(x = x0, xend = x1, y = name, yend = name), colour = grid_col, linewidth = 0.8) +
  geom_point(data = dot_long, aes(x, name, shape = quarter), colour = col_1, fill = "white",
             size = 1.6, stroke = 0.6) +
  name_axis(dot, x_edge = dot_lims[1]) +
  scale_colour_manual(values = group_cols, guide = "none") +
  scale_shape_manual(values = setNames(c(21, 16), c(P_REF0, P_LAST)), name = NULL) +
  scale_x_log10(limits = dot_lims, expand = c(0, 0), breaks = c(0.01, 0.1, 1, 10, 100),
                labels = c("0.01", "0.1", "1", "10", "100")) +
  labs(x = "Transactions per person per quarter (log scale)", y = NULL) +
  theme_study() + theme(legend.position = "top", panel.grid.major.y = element_blank())
save_fig(p, "state_count_pc.pdf", h = 6.5)

# Label positions next to points without overlap (ggrepel is not installed). Greedy: points with
# more near neighbours first; for each, the first of eight positions around the point whose box
# overlaps no placed label, no point and stays in the panel; else the position with least overlap.
# xlim, ylim: panel limits (scale with no expansion); pw, ph: panel size in inches.
label_layout <- function(x, y, lab, xlim, ylim, pw, ph, log_x = FALSE) {
  tx <- if (log_x) log10 else identity
  sx <- pw / diff(tx(xlim)); sy <- ph / diff(ylim)
  px <- (tx(x) - tx(xlim[1])) * sx; py <- (y - ylim[1]) * sy
  ts <- text_size_in(lab)
  w <- ts$w + 0.03; h <- ts$h * 1.4; gap <- 0.04; r <- 0.035
  box <- function(cx, cy, bw, bh) c(cx - bw / 2, cx + bw / 2, cy - bh / 2, cy + bh / 2)
  ovl <- function(a, b) max(0, min(a[2], b[2]) - max(a[1], b[1])) *
    max(0, min(a[4], b[4]) - max(a[3], b[3]))
  pts <- lapply(seq_along(px), function(j) box(px[j], py[j], 2 * r, 2 * r))
  dist <- as.matrix(stats::dist(cbind(px, py)))
  ord <- order(-rowSums(dist < 0.5), lab)
  placed <- list(); cx <- cy <- numeric(length(px))
  for (i in ord) {
    dx <- gap + w[i] / 2; dy <- gap + h / 2
    cand <- rbind(c(dx, 0), c(-dx, 0), c(0, dy), c(0, -dy), c(dx, dy), c(-dx, dy), c(dx, -dy),
                  c(-dx, -dy))
    cost <- apply(cand, 1, function(o) {
      b <- box(px[i] + o[1], py[i] + o[2], w[i], h)
      out <- (b[1] < 0) + (b[2] > pw) + (b[3] < 0) + (b[4] > ph)
      sum(vapply(c(placed, pts), ovl, 0, b = b)) + 10 * out
    })
    k <- which.min(cost)   # first zero-cost candidate, or least overlap
    cx[i] <- px[i] + cand[k, 1]; cy[i] <- py[i] + cand[k, 2]
    placed[[length(placed) + 1L]] <- box(cx[i], cy[i], w[i], h)
  }
  lx <- tx(xlim[1]) + cx / sx
  list(x = if (log_x) 10^lx else lx, y = ylim[1] + cy / sy)
}
pad <- function(x, f = 0.06, log_x = FALSE) {
  if (log_x) 10^pad(log10(x), f) else range(x) + c(-1, 1) * f * diff(range(x))
}
SC_W <- 5.5; SC_H <- 3.3   # scatter panel size in inches (figure 6.3 x 4.0)
scatter <- function(d, xv, yv, log_x) {
  xlim <- pad(d[[xv]], log_x = log_x); ylim <- pad(d[[yv]])
  ll <- label_layout(d[[xv]], d[[yv]], d$code, xlim, ylim, SC_W, SC_H, log_x)
  d <- data.table(x = d[[xv]], y = d[[yv]], lx = ll$x, ly = ll$y, code = d$code, group = d$group)
  list(geom_point(data = d, aes(x, y, colour = group), size = 1.2),
       geom_text(data = d, aes(lx, ly, label = code, colour = group), size = axis_size / .pt),
       scale_colour_manual(values = group_cols, guide = "none"),
       scale_y_continuous(limits = ylim, expand = c(0, 0)),
       if (log_x) scale_x_log10(limits = xlim, expand = c(0, 0))
       else scale_x_continuous(limits = xlim, expand = c(0, 0), labels = scales::label_comma()),
       theme_study(),
       theme(panel.widths = unit(SC_W, "in"), panel.heights = unit(SC_H, "in")))
}

# reach_intensity.pdf
last <- at(P_LAST)
last[, retail_pct := 100 * share_retail]
p <- ggplot() + scatter(last, "users_per_1000", "intensity", log_x = FALSE) +
  labs(x = "Registered users per 1000 persons", y = "Transactions per user per quarter")
save_fig(p, "reach_intensity.pdf", h = 4.0)

# decomposition.pdf
# Each panel title carries the unit of its vertical axis. The legend names the variable of each line:
# its variance in the left panel, its contribution to var(y) in the right panel.
DEC_PANEL <- c("Variance across units (variance of logs)", "Contribution to var(y) (share of var(y))")
COMP <- c(y = "y: transactions per person", r = "r: reach (users per person)",
          i = "i: intensity (transactions per user)")
dec[, t := period_num(period)]
dl <- rbind(dec[, .(t, panel = DEC_PANEL[1], comp = COMP[["y"]], v = var_y)],
            dec[, .(t, panel = DEC_PANEL[1], comp = COMP[["r"]], v = var_r)],
            dec[, .(t, panel = DEC_PANEL[1], comp = COMP[["i"]], v = var_i)],
            dec[, .(t, panel = DEC_PANEL[2], comp = COMP[["r"]], v = reach)],
            dec[, .(t, panel = DEC_PANEL[2], comp = COMP[["i"]], v = intensity)])
dl[, `:=`(panel = factor(panel, levels = DEC_PANEL), comp = factor(comp, levels = COMP))]
p <- ggplot(dl, aes(t, v, colour = comp)) +
  geom_hline(yintercept = 0, colour = ink2, linewidth = 0.3) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~panel, nrow = 1, scales = "free_y") +
  scale_colour_manual(values = setNames(c(col_3, col_1, col_2), COMP), name = "Variable (in logs):") +
  x_years2 +
  labs(x = X_QUARTER, y = NULL) +
  theme_study() + theme(legend.position = "bottom", legend.title = element_text(size = axis_size),
                        panel.spacing = unit(1, "lines"))
save_fig(p, "decomposition.pdf", h = 3.2)

# retail_share.pdf
p <- ggplot() + scatter(last, "count_pc", "retail_pct", log_x = TRUE) +
  labs(x = "Transactions per person per quarter (log scale)",
       y = "Retail share of the transaction count (percent)")
save_fig(p, "retail_share.pdf", h = 4.0)

# population_growth.pdf
pg[, name := factor(name, levels = name[order(growth, -rank(state_id))])]
p <- ggplot(pg) +
  geom_col(aes(growth, name, fill = group), width = 0.7) +
  geom_vline(xintercept = 0, colour = ink, linewidth = 0.3) +
  name_axis(pg) +
  scale_fill_manual(values = c(main = col_1, other = col_other), guide = "none") +
  scale_colour_manual(values = group_cols, guide = "none") +
  labs(x = sprintf("Change in population, %s to %s (percent)", P_FIRST, P_LAST), y = NULL) +
  theme_study() + theme(panel.grid.major.y = element_blank())
save_fig(p, "population_growth.pdf", h = 6.0)

cat(sprintf("part1_study: %d numbers, %d tables, %d figures\n", length(entries),
            length(list.files(TAB_DIR, "\\.tex$")), length(list.files(FIG_DIR, "\\.pdf$"))))
