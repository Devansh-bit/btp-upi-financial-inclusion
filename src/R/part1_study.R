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
SRC_NATT <- "national(); data/interim/{phonepe,phonepe_types}.csv"
for (p in c(P_FIRST, P_REF0, P_LAST)) for (k in c("p2p", "retail", "utility"))
  add(sprintf("nat.share.%s.%s", k, p), 100 * nat_at(p)[[paste0("share_", k)]], 1, SRC_NATT)
r18 <- 100 * nat[substr(period, 1, 4) == "2018", share_retail]
add("nat.share.retail.min2018", min(r18), 1, paste0("min share_retail 2018Q1-2018Q4, ", SRC_NATT))
add("nat.share.retail.max2018", max(r18), 1, paste0("max share_retail 2018Q1-2018Q4, ", SRC_NATT))

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

# concentration (35 ids)
SRC_CONC <- paste0("top_k(), 35 ids, ", SRC_ALL)
conc <- list(); n_half <- list()
for (q in c(P_REF0, P_LAST)) {
  d <- at(q)
  conc[[q]] <- top_k(dt, q, 5)
  add(sprintf("conc.top5.names.%s", q), join_and(name_of(top_ids(dt, q, 5))), NULL, SRC_CONC)
  add(sprintf("conc.top5.count.%s", q), conc[[q]][variable == "count", share], 1, SRC_CONC)
  add(sprintf("conc.top5.population.%s", q), conc[[q]][variable == "population", share], 1, SRC_CONC)
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

# retail share (main sample, 2026Q2)
add("retail.cor.2026Q2", spearman(d_last$share_retail, d_last$count_pc), 3,
    paste0("spearman() share_retail vs count_pc 2026Q2, ", SRC_MAIN))
for (s in c("max", "min")) {
  r <- pick(d_last, "share_retail", s == "max")
  add(sprintf("retail.%s.name", s), r$name, NULL, paste0("2026Q2, ", SRC_MAIN))
  add(sprintf("retail.%s", s), 100 * r$share_retail, 1, paste0("share_retail 2026Q2, ", SRC_MAIN))
}

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
  text = c(paste0(CONC_TEXT, " (percent)"), "States needed for half of all transactions"),
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
DEC_PANEL <- c("Variance across states", "Contribution to var(y)")
COMP <- c(y = "y = ln(transactions per person)", r = "r = ln(users per person)",
          i = "i = ln(transactions per user)")
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
  scale_colour_manual(values = setNames(c(col_3, col_1, col_2), COMP), name = NULL) +
  x_years2 +
  labs(x = X_QUARTER, y = "Variance of logs; share of var(y)") +
  theme_study() + theme(legend.position = "bottom", panel.spacing = unit(1, "lines"))
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
