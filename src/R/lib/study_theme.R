# Theme and save helper for the study figures (palette of src/R/01_descriptives.R).
library(ggplot2)

ink <- "#0b0b0b"; ink2 <- "#52514e"; grid_col <- "#e6e5e1"
col_1 <- "#2a78d6"; col_2 <- "#d6762a"; col_3 <- "#3a9a5b"
col_other <- "#9a9893"   # union territories outside the main sample

theme_study <- function() {
  theme_minimal(base_size = 9, base_family = "sans") +
    theme(text = element_text(colour = ink),
          axis.text = element_text(colour = ink2),
          axis.title = element_text(colour = ink2),
          panel.grid.major = element_line(colour = grid_col, linewidth = 0.3),
          panel.grid.minor = element_blank(),
          strip.text = element_text(colour = ink, hjust = 0, face = "bold"),
          legend.title = element_text(colour = ink2),
          legend.text = element_text(colour = ink2))
}

# Quarter as a year fraction: "2020Q3" -> 2020.5.
period_num <- function(p) as.integer(substr(p, 1, 4)) + (as.integer(substr(p, 6, 6)) - 1) / 4

FIG_DIR <- "output/study/part1/figures"

save_fig <- function(p, name, w = 6.3, h) {
  dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(FIG_DIR, name), p, width = w, height = h, device = "pdf")
}
