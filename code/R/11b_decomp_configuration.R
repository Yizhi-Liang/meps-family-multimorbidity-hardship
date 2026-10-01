###############################################################################
# 11b_decomp_configuration.R -- format the within-household distribution
# decomposition (concentrated vs distributed) for all four outcomes 11a models
# (CHE at 40%, medical debt, cost-related barriers, any hardship) and draw
# eFigure 4 (all four outcomes).
# In:  data/clean/decomp_config_desc_stata.csv,
#      data/clean/decomp_config_results_stata.csv
# Out: output/tabs/decomp_configuration.xlsx,
#      output/figs/config_distribution.pdf
###############################################################################

resolve_proj <- function() {
  env <- Sys.getenv("MEPS_PROTECTION_PROJ", unset = "")
  if (nzchar(env) && file.exists(file.path(env, "code", "00_master.do"))) return(env)
  cwd <- getwd()
  if (file.exists(file.path(cwd, "code", "00_master.do"))) return(cwd)
  stop("Cannot locate project root. Set MEPS_PROTECTION_PROJ or cd to repo root.")
}

proj <- resolve_proj()
dir.create(file.path(proj, "code", "logs"), showWarnings = FALSE, recursive = TRUE)
log_file <- file.path(proj, "code", "logs", "11b_decomp_configuration.log")
log_con <- file(log_file, open = "wt")
sink(log_con, type = "output", split = TRUE)
sink(log_con, type = "message", append = TRUE)
on.exit({
  sink(type = "message")
  sink(type = "output")
  close(log_con)
}, add = TRUE)

cat("=== Step 11b: Within-household distribution decomposition ===\n")
cat("Start:", format(Sys.time()), "\n\n")

library(tidyverse)
library(writexl)
library(patchwork)

# Figure device: cairo_pdf where the cairo DLL loads; base pdf() otherwise (host portability;
# the canonical figures are produced where cairo is available).
fig_device <- local({
  ok <- tryCatch({ f <- tempfile(fileext = ".pdf"); grDevices::cairo_pdf(f); grDevices::dev.off(); unlink(f); TRUE },
                 warning = function(w) FALSE, error = function(e) FALSE)
  if (!ok) cat("NOTE: cairo device unavailable on this host; figures use the base pdf device.\n")
  if (ok) grDevices::cairo_pdf else grDevices::pdf
})

clean_dir <- file.path(proj, "data", "clean")
tabs_dir <- file.path(proj, "output", "tabs")
figs_dir <- file.path(proj, "output", "figs")
dir.create(tabs_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figs_dir, showWarnings = FALSE, recursive = TRUE)

desc <- read.csv(file.path(clean_dir, "decomp_config_desc_stata.csv"),
                 stringsAsFactors = FALSE)
res <- read.csv(file.path(clean_dir, "decomp_config_results_stata.csv"),
                stringsAsFactors = FALSE)

res$p_holm <- NA_real_
idx <- res$type == "config_diff"
if (any(idx)) {
  res$p_holm[idx] <- ave(res$pvalue[idx], res$outcome[idx],
                         FUN = function(x) p.adjust(x, method = "holm"))
}

round_numeric <- function(x) {
  is_num <- vapply(x, is.numeric, logical(1))
  x[is_num] <- lapply(x[is_num], function(v) round(v, 6))
  x
}

# 4 outcomes x 2 configurations x 3 insurance types = 24 predicted probabilities;
# 4 outcomes x 3 insurance types x 1 contrast = 12 configuration differences.
stopifnot(sum(res$type == "pred_prob") == 24)
stopifnot(sum(res$type == "config_diff") == 12)
stopifnot("p_holm" %in% names(res))

out_xlsx <- file.path(tabs_dir, "decomp_configuration.xlsx")
write_xlsx(
  list(
    descriptives = round_numeric(desc),
    results = round_numeric(res)
  ),
  path = out_xlsx
)

cat("Workbook saved to:", out_xlsx, "\n")

# --- eFigure 4: adjusted hardship by within-household distribution -----------
# Panel A = concentrated, Panel B = distributed; facets are the four outcomes 11a models, left to
# right in the labels and order of Figure 2 (07b_main_analysis.R). The letter "a" sits on the
# concentrated bars and marks a Holm-adjusted concentrated-minus-distributed difference at P < .05
# within that insurance type and outcome (the Holm family is the 3 insurance types within an outcome).

outcome_labels <- c(
  che_40           = "CHE (40%)",
  fam_debt_any     = "Medical Debt",
  fam_barrier_any  = "Cost-Related Barriers",
  fam_any_hardship = "Any Hardship"
)
outcome_order <- c("CHE (40%)", "Medical Debt", "Cost-Related Barriers", "Any Hardship")
ins_levels <- c("No Insurance", "Public Insurance", "Private Insurance")
config_levels <- c("Concentrated", "Distributed")

palette_colors <- c("No Insurance"      = "#B24745",
                    "Public Insurance"  = "#DF8F44",
                    "Private Insurance" = "#374E55")

theme_pub <- theme_minimal(base_size = 12, base_family = "serif") +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 13),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 12))

# 11a writes the configuration label into the dis_cat field of pred_prob rows.
fig_preds <- res %>%
  filter(type == "pred_prob", outcome %in% names(outcome_labels)) %>%
  transmute(outcome,
            ins_type = factor(ins_type, levels = ins_levels),
            config   = factor(dis_cat, levels = config_levels),
            estimate, ci_lower, ci_upper,
            outcome_label = factor(outcome_labels[outcome], levels = outcome_order))

# 4 outcomes x 2 configurations x 3 insurance types.
stopifnot(nrow(fig_preds) == 24, !any(is.na(fig_preds$config)))

fig_stars <- res %>%
  filter(type == "config_diff", outcome %in% names(outcome_labels)) %>%
  transmute(outcome,
            ins_type = factor(ins_type, levels = ins_levels),
            config   = factor("Concentrated", levels = config_levels),
            mark_label = ifelse(!is.na(p_holm) & p_holm < 0.05, "a", ""))

y_axis_max <- max(fig_preds$ci_upper * 100, na.rm = TRUE)

fig_preds <- fig_preds %>%
  left_join(fig_stars, by = c("outcome", "ins_type", "config")) %>%
  mutate(mark_label = replace_na(mark_label, ""))

panel_of <- function(cfg, y_title = NULL) {
  ggplot(filter(fig_preds, config == cfg),
         aes(x = ins_type, y = estimate * 100, fill = ins_type, color = ins_type)) +
    geom_col(width = 0.6, alpha = 0.55, color = NA) +
    geom_errorbar(aes(ymin = ci_lower * 100, ymax = ci_upper * 100),
                  width = 0.15, linewidth = 0.5) +
    geom_point(size = 2.4, shape = 21, stroke = 0.8, fill = "white",
               show.legend = FALSE) +
    geom_text(aes(y = ci_upper * 100 + 0.02 * y_axis_max, label = mark_label),
              vjust = 0, size = 3.4, family = "serif", color = "black", show.legend = FALSE) +
    facet_wrap(~outcome_label, nrow = 1, scales = "fixed") +
    # Two-line tick labels: single-line labels overprint at the facet width when the host substitutes a
    # wider serif font.
    scale_x_discrete(labels = function(x) sub(" ", "\n", x)) +
    scale_fill_manual(values = palette_colors) +
    scale_color_manual(values = palette_colors, guide = "none") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.08)),
                       limits = c(0, y_axis_max * 1.12)) +
    labs(x = NULL,
         y = y_title,
         fill = NULL) +
    theme_pub
}

# The A/B tags sit in the top-right corner of each panel so they never overprint the
# y-axis title or the facet strips; each stacked panel keeps its own y-axis title, set on two
# lines so the rotated titles of the stacked panels cannot collide whatever the host font.
y_title_2l <- "Adjusted Probability of\nFinancial Hardship (%)"
panel_conc <- panel_of("Concentrated", y_title = y_title_2l)
panel_dist <- panel_of("Distributed", y_title = y_title_2l)
config_fig <- (panel_conc / panel_dist) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "bottom",
        plot.tag = element_text(face = "bold", size = 18, family = "serif"),
        plot.tag.position = "topright")

# Post-condition: the "a" letters ggplot will draw are exactly the Holm-significant rows (adjusted
# P < .05) of the config_diff family (4 outcomes x 3 insurance types, Holm within outcome), checked in
# both directions, and none is drawn on the distributed panel. A missing P, a short family, or a join
# that drops or duplicates a row stops the script before the figure is written.
diff_rows <- res %>% filter(type == "config_diff")
stopifnot("eFigure 4 check: every outcome needs a 3-member Holm family with no missing P" =
            setequal(diff_rows$outcome, names(outcome_labels)) && all(table(diff_rows$outcome) == 3) &&
            !anyNA(diff_rows$pvalue) && nrow(fig_preds) == 24)
drawn_a_cells <- function(p, cfg) {
  built <- ggplot_build(p)
  text_layer <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))
  stopifnot(length(text_layer) == 1)
  drawn <- built$data[[text_layer]]
  lay <- built$layout$layout
  drawn_outcome <- names(outcome_labels)[match(
    as.character(lay$outcome_label[match(drawn$PANEL, lay$PANEL)]), outcome_labels)]
  paste(cfg, drawn_outcome, ins_levels[round(drawn$x)], sep = "|")[drawn$label %in% "a"]
}
drawn_a <- c(drawn_a_cells(panel_conc, "Concentrated"), drawn_a_cells(panel_dist, "Distributed"))
expected_a <- with(diff_rows[diff_rows$p_holm < 0.05, ],
                   paste("Concentrated", outcome, ins_type, sep = "|"))
n_a <- length(drawn_a)
stopifnot("eFigure 4 check: a drawn letter and its Holm row disagree" =
            !anyNA(drawn_a) && identical(sort(drawn_a), sort(expected_a)) &&
            n_a == sum(diff_rows$p_holm < 0.05))
cat("eFigure 4 letters a =", n_a, "\n")

config_fig_path <- file.path(figs_dir, "config_distribution.pdf")
ggsave(config_fig_path, config_fig, width = 13, height = 10, dpi = 600, device = fig_device)
cat("eFigure 4 (concentrated vs distributed) saved to:", config_fig_path, "\n")

cat("\n--- Weighted shares among households with >=2 conditions ---\n")
stopifnot("share_of_all_eligible" %in% names(desc))
print(desc[desc$level == "config_overall",
           c("config", "n_unweighted", "share_of_all_eligible")])

cat("\n--- Concentrated minus distributed (percentage points, Holm-adjusted) ---\n")
print(res %>%
        filter(type == "config_diff", outcome %in% names(outcome_labels)) %>%
        transmute(outcome, ins_type,
                  diff_pp = round(estimate * 100, 2),
                  ci = sprintf("%.2f to %.2f", ci_lower * 100, ci_upper * 100),
                  p_holm = signif(p_holm, 3)))

cat("\nEnd:", format(Sys.time()), "\n")
