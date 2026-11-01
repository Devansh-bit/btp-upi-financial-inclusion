# Descriptive results: figures and tables for the report.
# Run from the repository root: Rscript src/R/01_descriptives.R
suppressPackageStartupMessages({
  library(data.table); library(ggplot2); library(sf); library(modelsummary)
})
source("src/R/lib/descriptives_fns.R")

options(modelsummary_format_numeric_latex = "plain")
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

panel <- fread("data/final/panel_quarterly.csv")
panel[, in_main_sample := as.character(in_main_sample) %in% c("True", "TRUE")]
main <- panel[in_main_sample == TRUE]
valid <- fread("data/final/validation_npci.csv")

# ---- theme (one theme for all figures; palette from the dataviz reference) ----
ink <- "#0b0b0b"; ink2 <- "#52514e"; grid_col <- "#e6e5e1"
col_main <- "#2a78d6"            # categorical slot 1 (single series)
theme_btp <- function() {
  theme_minimal(base_size = 9, base_family = "sans") +
    theme(text = element_text(colour = ink),
          axis.text = element_text(colour = ink2),
          axis.title = element_text(colour = ink2),
          panel.grid.major = element_line(colour = grid_col, linewidth = 0.3),
          panel.grid.minor = element_blank(),
          strip.text = element_text(colour = ink, hjust = 0, face = "bold"),
          plot.caption = element_text(colour = ink2, size = 7, hjust = 0),
          legend.title = element_text(colour = ink2),
          legend.text = element_text(colour = ink2))
}
save_pdf <- function(p, name, w, h)
  ggsave(file.path("output/figures", name), p, width = w, height = h, device = "pdf")

# x axis: quarters as numeric year fraction, labelled by year
period_num <- function(p) as.integer(substr(p, 1, 4)) + (as.integer(substr(p, 6, 6)) - 1) / 4
year_breaks <- 2018:2026

# ---- adoption trend ----
trend <- main[, .(med = median(ln_real_amount_pc),
                  q1 = quantile(ln_real_amount_pc, 0.25),
                  q3 = quantile(ln_real_amount_pc, 0.75)), by = period]
trend[, t := period_num(period)]
p_trend <- ggplot(trend, aes(t)) +
  geom_ribbon(aes(ymin = q1, ymax = q3), fill = col_main, alpha = 0.2) +
  geom_line(aes(y = med), colour = col_main, linewidth = 0.8) +
  annotate("text", x = 2022.3, y = 5.6, label = "Line: median across states",
           colour = ink2, size = 3, hjust = 0) +
  annotate("text", x = 2022.3, y = 5.1, label = "Band: interquartile range",
           colour = ink2, size = 3, hjust = 0) +
  scale_x_continuous(breaks = year_breaks, labels = year_breaks) +
  labs(x = "Quarter (labelled at Q1)",
       y = "ln(real value per person)\n(Rs. per person per quarter, 2012 prices)",
       caption = "Main sample: 30 states and union territories. Data: PhonePe Pulse.") +
  theme_btp()
save_pdf(p_trend, "adoption_trend_main.pdf", 6.3, 3.4)

# ---- sigma convergence (small multiples: one scale per measure) ----
vars <- c(count_pc = "Transactions per person",
          real_amount_pc = "Real value per person",
          users_per_1000 = "Registered users per 1,000")
cvs <- rbindlist(lapply(names(vars), function(v)
  sigma_cv(panel, v)[, measure := factor(vars[[v]], levels = vars)]))
cvs[, t := period_num(period)]
p_sigma <- ggplot(cvs, aes(t, cv)) +
  geom_line(colour = col_main, linewidth = 0.8) +
  facet_wrap(~measure, nrow = 1) +
  scale_x_continuous(breaks = c(2018, 2020, 2022, 2024, 2026)) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = "Year", y = "Coefficient of variation across states",
       caption = "Main sample: 30 states and union territories. CV = sd / mean, per quarter. Data: PhonePe Pulse.") +
  theme_btp() + theme(panel.spacing = unit(1, "lines"))
save_pdf(p_sigma, "sigma_convergence.pdf", 6.3, 2.9)

# ---- maps ----
geo <- st_read("data/interim/india_states.geojson", quiet = TRUE)
lims <- range(panel[period %in% c("2018Q4", "2026Q2"), real_amount_pc])
make_map <- function(per) {
  d <- panel[period == per, .(state_id, real_amount_pc)]
  g <- merge(geo, d, by = "state_id", all.x = TRUE)
  ggplot(g) +
    geom_sf(aes(fill = real_amount_pc), colour = "white", linewidth = 0.15) +
    scale_fill_gradient(low = "#dbe9fb", high = "#0b2a5b", trans = "log10",
                        limits = lims, na.value = "#cfcfcf",
                        breaks = c(100, 300, 1000, 3000, 10000, 30000),
                        labels = scales::label_comma(),
                        name = "Rs. per person per quarter\n(2012 prices, log scale)",
                        guide = guide_colourbar(title.position = "top", barwidth = 12, barheight = 0.5)) +
    labs(title = paste("Real UPI value per person,", per),
         caption = "Data: PhonePe Pulse; boundaries: geoBoundaries (CC BY 2.5 IN)") +
    theme_btp() +
    theme(axis.text = element_blank(), panel.grid.major = element_blank(),
          legend.position = "bottom", plot.title = element_text(face = "bold", colour = ink))
}
for (per in c("2018Q4", "2026Q2"))
  save_pdf(make_map(per), paste0("adoption_map_", per, ".pdf"), 5.2, 5.6)

# ---- beta convergence table ----
models <- list(`Real value per person` = beta_convergence(panel, "real_amount_pc"),
               `Transactions per person` = beta_convergence(panel, "count_pc"))
msum <- modelsummary(models, output = "latex",
                     coef_rename = c("(Intercept)" = "Intercept", "ln_start" = "ln(initial level)"),
                     gof_map = data.frame(raw = c("nobs", "r.squared"),
                                          clean = c("N", "R2"), fmt = c(0, 3)),
                     stars = FALSE, statistic = "std.error", fmt = 4)
# Remove the float wrapper so the file holds a bare tblr environment
# (the report supplies the table float and caption).
tex <- as.character(msum)
tex <- sub("^\\\\begin\\{table\\}\\s*\\\\centering\\s*", "", tex)
tex <- sub("\\s*\\\\end\\{table\\}\\s*$", "\n", tex)
writeLines(tex, "output/tables/beta_convergence.tex")

# ---- NPCI validation table ----
v <- dcast(valid[measure %in% c("share_count", "share_value")],
           period ~ measure, value.var = c("pearson", "spearman"))
setcolorder(v, c("period", "pearson_share_count", "pearson_share_value",
                 "spearman_share_count", "spearman_share_value"))
f3 <- function(x) sprintf("%.3f", x)
rows <- v[, paste(period, f3(pearson_share_count), f3(pearson_share_value),
                  f3(spearman_share_count), f3(spearman_share_value), sep = " & ")]
writeLines(c("\\begin{tabular}{lrrrr}", "\\toprule",
             " & \\multicolumn{2}{c}{Pearson} & \\multicolumn{2}{c}{Spearman} \\\\",
             "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
             "Quarter & Count share & Value share & Count share & Value share \\\\",
             "\\midrule", paste0(rows, " \\\\"), "\\bottomrule", "\\end{tabular}"),
           "output/tables/validation_npci.tex")

# ---- summary statistics ----
sv <- list(`Transactions per person (count)` = main$count_pc,
           `Real value per person (Rs., 2012 prices)` = main$real_amount_pc,
           `Registered users per 1,000` = main$users_per_1000,
           `Population (millions)` = main$population / 1e6,
           `CPI` = main$cpi)
fmt <- function(x) formatC(x, format = "f", digits = 2, big.mark = ",")
srows <- vapply(names(sv), function(n) {
  x <- sv[[n]]
  paste(n, formatC(length(x), format = "d", big.mark = ","), fmt(mean(x)), fmt(sd(x)),
        fmt(min(x)), fmt(max(x)), sep = " & ")
}, character(1))
writeLines(c("\\begin{tabular}{lrrrrr}", "\\toprule",
             "Variable & N & Mean & SD & Min & Max \\\\", "\\midrule",
             paste0(srows, " \\\\"), "\\bottomrule", "\\end{tabular}"),
           "output/tables/summary_stats.tex")

# ---- values for the report ----
for (v in c("real_amount_pc", "count_pc")) {
  m <- models[[ifelse(v == "real_amount_pc", 1, 2)]]
  cat(v, "coef:", format(coef(m), digits = 5), " se:", format(sqrt(diag(vcov(m))), digits = 5),
      " R2:", format(summary(m)$r.squared, digits = 4), " n:", nobs(m), "\n")
}
print(cvs[period %in% c("2018Q1", "2018Q4", "2026Q2"), .(measure, period, cv)])
cat("map limits:", lims, "\n")
