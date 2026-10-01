###############################################################################
# 06b_oop_burden_distribution.R — format the OOP burden-ratio distribution: Figure 1 and the
# workbook of the shares above 10%, 25% and 40% of income and of the survival grid behind the figure.
# In:  $data/clean/oop_burden_dist_stata.csv
#      $data/clean/oop_burden_survival_stata.csv
# Out: $proj/output/tabs/oop_burden_distribution.xlsx
#      $proj/output/figs/oop_burden_distribution.pdf
###############################################################################

proj_env <- Sys.getenv("MEPS_PROTECTION_PROJ", unset = "")
proj <- if (nzchar(proj_env)) proj_env else getwd()
if (!file.exists(file.path(proj, "code", "00_master.do"))) {
  stop("Cannot locate project root. Set MEPS_PROTECTION_PROJ or cd to repo root.")
}

log_file <- file.path(proj, "code", "logs", "06b_oop_burden_distribution.log")
log_con  <- file(log_file, open = "wt")
sink(log_con, type = "output", split = TRUE)
sink(log_con, type = "message", append = TRUE)

cat("=== Step 6b: OOP Burden Distribution Output ===\n")
cat("Start:", format(Sys.time()), "\n\n")

library(tidyverse)
library(writexl)

# Figure device: cairo_pdf where the cairo DLL loads; base pdf() otherwise (host portability;
# the canonical figures are produced where cairo is available).
fig_device <- local({
  ok <- tryCatch({ f <- tempfile(fileext = ".pdf"); grDevices::cairo_pdf(f); grDevices::dev.off(); unlink(f); TRUE },
                 warning = function(w) FALSE, error = function(e) FALSE)
  if (!ok) cat("NOTE: cairo device unavailable on this host; figures use the base pdf device.\n")
  if (ok) grDevices::cairo_pdf else grDevices::pdf
})

ins_levels <- c("No Insurance", "Public Insurance", "Private Insurance")
dis_levels <- c("0", "1-2", "3+")
clean_dir  <- file.path(proj, "data", "clean")
tabs_dir   <- file.path(proj, "output", "tabs")
figs_dir   <- file.path(proj, "output", "figs")

# ---------------------------------------------------------------------------
# A. Shares table + survival grid
# ---------------------------------------------------------------------------
dist <- read.csv(file.path(clean_dir, "oop_burden_dist_stata.csv"),
                 stringsAsFactors = FALSE)
survival <- read.csv(file.path(clean_dir, "oop_burden_survival_stata.csv"),
                     stringsAsFactors = FALSE)

fmt_pct1 <- function(x) sprintf("%.1f", x * 100)
fmt_share_ci <- function(est, lo, hi) {
  sprintf("%.1f (%.1f-%.1f)", est * 100, lo * 100, hi * 100)
}

shares_table <- dist %>%
  transmute(
    Insurance = ins_type,
    `Household multimorbidity` = dis_cat,
    `Households, n` = n_all,
    `Households with positive income, n` = n_pos,
    `Non-positive income, %` = fmt_pct1(wtd_prop_nonpos),
    `>10% of income, % (95% CI)` = fmt_share_ci(gt10, gt10_lo, gt10_hi),
    `>25% of income, % (95% CI)` = fmt_share_ci(gt25, gt25_lo, gt25_hi),
    `>40% of income, % (95% CI)` = fmt_share_ci(gt40, gt40_lo, gt40_hi)
  )

survival_sheet <- survival %>%
  mutate(share_above_pct = share_above * 100)

stopifnot(nrow(shares_table) == 12, nrow(survival) == 228)

tab_path <- file.path(tabs_dir, "oop_burden_distribution.xlsx")
write_xlsx(
  list(
    "Shares" = shares_table,
    "Survival grid" = survival_sheet
  ),
  path = tab_path
)
cat("Workbook saved to:", tab_path, "\n")

cat("\n--- Burden-ratio shares ---\n")
as.data.frame(shares_table) %>% print()

# ---------------------------------------------------------------------------
# B. Figure 1: log-x survival curves
# ---------------------------------------------------------------------------
theme_pub <- theme_minimal(base_size = 12, base_family = "serif") +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 13),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 12))

# Multimorbidity levels are ordered, so they take one hue from light to dark; it is
# the hue of the shared palette family that no insurance type or hardship state uses.
dis_colors <- c("0" = "#B5AFA3", "1-2" = "#80796B", "3+" = "#3F3B34")
panel_levels <- c("All households", "No Insurance",
                  "Public Insurance", "Private Insurance")
x_breaks <- c(0.1, 1, 10, 100)  # sparse ticks; the 10/25/40 reference lines carry their own labels

burden_fig_data <- survival %>%
  mutate(
    dis_cat = factor(dis_cat, levels = dis_levels),
    panel = factor(
      if_else(ins_type == "Overall", "All households", ins_type),
      levels = panel_levels
    ),
    x = threshold * 100,
    y = share_above * 100
  )

ymax <- max(burden_fig_data$y, na.rm = TRUE)

burden_fig <- ggplot(burden_fig_data, aes(x = x, y = y, color = dis_cat, group = dis_cat)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.6) +
  geom_vline(xintercept = c(10, 25, 40), linetype = "dashed", color = "gray40") +
  annotate("text", x = 10 * 1.12, y = ymax * 1.02, label = "10%",
           size = 3.2, color = "gray30", hjust = 0) +
  annotate("text", x = 25 * 1.12, y = ymax * 0.93, label = "25%",
           size = 3.2, color = "gray30", hjust = 0) +
  annotate("text", x = 40 * 1.12, y = ymax * 1.02, label = "40%",
           size = 3.2, color = "gray30", hjust = 0) +
  facet_wrap(~ panel, nrow = 1) +
  scale_color_manual(values = dis_colors) +
  scale_x_log10(breaks = x_breaks,
                labels = c("0.1", "1", "10", "100")) +
  scale_y_continuous(limits = c(0, ymax * 1.05),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = "Household out-of-pocket spending as a share of pre-tax income (%, log scale)",
       y = "Households above the threshold (%)",
       color = "Household multimorbidity") +
  theme_pub

burden_fig_path <- file.path(figs_dir, "oop_burden_distribution.pdf")
ggsave(burden_fig_path, burden_fig, width = 13, height = 4.6, dpi = 600, device = fig_device)
cat("Figure 1 saved to:", burden_fig_path, "\n")

cat("\n=== Step 6b complete ===\n")
cat("End:", format(Sys.time()), "\n")

sink(type = "message")
sink(type = "output")
close(log_con)
