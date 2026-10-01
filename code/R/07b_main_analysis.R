###############################################################################
# 07b_main_analysis.R — format main analysis + the 4-category multimorbidity sens.
# In:  $data/clean/main_results.csv (+ sens_multimorb_4cat_stata.csv from 07a)
# Out: $proj/output/tabs/{main_results,multimorb_4cat}.xlsx
#      $proj/output/figs/main_predicted.pdf, main_heatmap.pdf,
#      main_forest.pdf
###############################################################################

proj_env <- Sys.getenv("MEPS_PROTECTION_PROJ", unset = "")
proj <- if (nzchar(proj_env)) proj_env else getwd()
if (!file.exists(file.path(proj, "code", "00_master.do"))) {
  stop("Cannot locate project root. Set MEPS_PROTECTION_PROJ or cd to repo root.")
}

log_file <- file.path(proj, "code", "logs", "07b_main_analysis.log")
log_con  <- file(log_file, open = "wt")
sink(log_con, type = "output", split = TRUE)
sink(log_con, type = "message", append = TRUE)

cat("=== Step 7b: Main Analysis Output ===\n")
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

# Figure markers: one tier, Holm-adjusted P < .05. "a" = comparison across insurance types within the
# same multimorbidity level; "b" = comparison across multimorbidity levels within the same insurance
# type; "a,b" = both.
mark_of <- function(p_a, p_b) {
  a <- !is.na(p_a) & p_a < 0.05
  b <- !is.na(p_b) & p_b < 0.05
  ifelse(a & b, "a,b", ifelse(a, "a", ifelse(b, "b", "")))
}

ins_levels <- c("No Insurance", "Public Insurance", "Private Insurance")
dis_levels <- c("0", "1-2", "3+")

# A. Load
res <- read.csv(file.path(proj, "data", "clean", "main_results.csv"),
                stringsAsFactors = FALSE)

cat("Loaded", nrow(res), "result rows\n")
cat("Types:", paste(unique(res$type), collapse = ", "), "\n")

outcome_labels <- c(che_40 = "CHE (40%)", fam_debt_any = "Medical Debt",
                    fam_barrier_any = "Cost-Related Barriers",
                    fam_any_hardship = "Any Hardship")
outcome_order <- c("CHE (40%)", "Medical Debt", "Cost-Related Barriers", "Any Hardship")

# B. Holm-Bonferroni within each outcome (separately for diffs and contrasts)
diffs <- res %>%
  filter(type %in% c("ins_diff", "morb_diff")) %>%
  group_by(outcome) %>%
  mutate(p_adj = p.adjust(pvalue, method = "holm")) %>%
  ungroup()

contrasts <- res %>%
  filter(type == "contrast") %>%
  group_by(outcome) %>%
  mutate(p_adj = p.adjust(pvalue, method = "holm")) %>%
  ungroup()

preds <- res %>% filter(type == "pred")

morb_holm <- diffs %>%
  filter(type == "morb_diff") %>%
  select(outcome, ins_type, dis_cat, morb_p = p_adj)

ins_holm <- diffs %>%
  filter(type == "ins_diff") %>%
  select(outcome, ins_type, dis_cat, ins_p = p_adj)

# Annotate predictions with the Holm-adjusted P of the differences that involve each cell;
# mark_label feeds Figure 2.
preds_annotated <- preds %>%
  left_join(morb_holm, by = c("outcome", "ins_type", "dis_cat")) %>%
  left_join(ins_holm,  by = c("outcome", "ins_type", "dis_cat")) %>%
  mutate(mark_label = mark_of(ins_p, morb_p))

# C. Print key results
cat("\n--- Predicted Probabilities (Interaction Model) ---\n")
preds_annotated %>%
  mutate(cell = sprintf("%.1f%% (%.1f, %.1f) %s",
                        estimate*100, ci_lower*100, ci_upper*100, mark_label)) %>%
  select(outcome, ins_type, dis_cat, cell) %>%
  as.data.frame() %>% print()

cat("\n--- Pairwise Differences ---\n")
diffs %>%
  mutate(cell = sprintf("%.2f pp (%.2f, %.2f) p_adj=%.4f",
                        estimate*100, ci_lower*100, ci_upper*100, p_adj)) %>%
  select(outcome, type, ins_type, dis_cat, comparison, cell) %>%
  as.data.frame() %>% print()

cat("\n--- Interaction Contrasts (0 vs 3+) ---\n")
contrasts %>%
  filter(dis_cat == "0 vs 3+") %>%
  mutate(cell = sprintf("%.2f pp (%.2f, %.2f) p_adj=%.4f",
                        estimate*100, ci_lower*100, ci_upper*100, p_adj)) %>%
  select(outcome, ins_type, cell) %>%
  as.data.frame() %>% print()

# D. Excel workbook
pp_table <- preds_annotated %>%
  mutate(
    cell = sprintf("%.1f (%.1f, %.1f)",
                   estimate*100, ci_lower*100, ci_upper*100),
    ins_type = factor(ins_type, levels = ins_levels),
    dis_cat  = factor(dis_cat, levels = dis_levels)
  ) %>%
  arrange(outcome, ins_type, dis_cat) %>%
  select(outcome, ins_type, dis_cat, cell,
         morb_p, ins_p)

pairwise_table <- diffs %>%
  mutate(cell = sprintf("%.1f pp (%.1f, %.1f)",
                        estimate*100, ci_lower*100, ci_upper*100)) %>%
  select(outcome, type, ins_type, dis_cat, comparison,
         cell, pvalue, p_adj)

# Interaction contrasts: estimates with CIs, then the Holm-adjusted P of the 9 contrasts of each
# outcome (the contrasts with P < .05 are the ones the eFigure 3 legend lists).
contrast_cells <- contrasts %>%
  mutate(cell = sprintf("%.1f pp (%.1f, %.1f)",
                        estimate*100, ci_lower*100, ci_upper*100)) %>%
  select(outcome, ins_type, dis_cat, cell) %>%
  pivot_wider(names_from = outcome, values_from = cell)

contrast_holm <- contrasts %>%
  select(outcome, ins_type, dis_cat, p_adj) %>%
  pivot_wider(names_from = outcome, values_from = p_adj,
              names_glue = "{outcome} Holm-adjusted P")

contrast_wide <- contrast_cells %>%
  left_join(contrast_holm, by = c("ins_type", "dis_cat"))

workbook_sheets <- list(
  "Predicted Probability" = pp_table,
  "Pairwise Diffs" = pairwise_table,
  "Interaction Contrasts" = contrast_wide
)
out_xlsx <- file.path(proj, "output", "tabs", "main_results.xlsx")
write_xlsx(workbook_sheets, path = out_xlsx)
cat("\nMain results saved to:", out_xlsx, "\n")

# E. Figures
palette_colors <- c("No Insurance" = "#B24745",
                 "Public Insurance" = "#DF8F44",
                 "Private Insurance" = "#374E55")
fig_dir <- file.path(proj, "output", "figs")

theme_pub <- theme_minimal(base_size = 12, base_family = "serif") +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 13),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 12))

# Figure 2: bars (levels) + dot + Holm-adjusted significance letters
main_fig_data <- preds_annotated %>%
  mutate(ins_type = factor(ins_type, levels = ins_levels),
         dis_cat  = factor(dis_cat, levels = dis_levels),
         outcome_label = factor(outcome_labels[outcome], levels = outcome_order))

y_axis_max <- max(main_fig_data$ci_upper * 100, na.rm = TRUE)

dodge <- position_dodge(width = 0.8)

main_fig <- ggplot(main_fig_data, aes(x = dis_cat, y = estimate*100,
                              fill = ins_type, color = ins_type,
                              group = ins_type)) +
  geom_col(position = dodge, width = 0.7, alpha = 0.55,
           color = NA) +
  geom_errorbar(aes(ymin = ci_lower*100, ymax = ci_upper*100),
                position = dodge, width = 0.2, linewidth = 0.5) +
  geom_point(position = dodge, size = 2.4, shape = 21,
             stroke = 0.8, fill = "white") +
  geom_text(aes(y = ci_upper*100 + 0.02 * y_axis_max, label = mark_label),
            position = dodge, vjust = 0, size = 3.4, family = "serif",
            color = "black", show.legend = FALSE) +
  facet_wrap(~outcome_label, nrow = 2, scales = "fixed") +
  scale_fill_manual(values = palette_colors) +
  scale_color_manual(values = palette_colors, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.10)),
                     limits = c(0, y_axis_max * 1.12)) +
  labs(x = "Household Multimorbidity",
       y = "Adjusted Probability of Financial Hardship (%)",
       fill = NULL) +
  theme_pub

# Post-condition: the letters ggplot will draw on Figure 2 are exactly the Holm-significant
# rows (adjusted P < .05) of the 12-member per-outcome family (6 insurance + 6 multimorbidity
# differences), recomputed here from the raw P values. "a" marks a significant insurance
# difference, "b" a significant multimorbidity difference; the check runs in both directions.
holm_rows <- res %>%
  filter(type %in% c("ins_diff", "morb_diff")) %>%
  group_by(outcome) %>%
  mutate(family_n = n(), p_holm = p.adjust(pvalue, method = "holm")) %>%
  ungroup()
stopifnot("Figure 2 check: every outcome needs a 12-member Holm family with no missing P" =
            !anyNA(holm_rows$pvalue) && all(holm_rows$family_n == 12) &&
            setequal(holm_rows$outcome, names(outcome_labels)))
sig_a <- holm_rows %>% filter(type == "ins_diff",  p_holm < 0.05) %>%
  with(paste(outcome, ins_type, dis_cat, sep = "|"))
sig_b <- holm_rows %>% filter(type == "morb_diff", p_holm < 0.05) %>%
  with(paste(outcome, ins_type, dis_cat, sep = "|"))

fig_built  <- ggplot_build(main_fig)
text_layer <- which(vapply(main_fig$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))
stopifnot(length(text_layer) == 1)
drawn <- fig_built$data[[text_layer]]
drawn_outcome <- names(outcome_labels)[match(
  as.character(fig_built$layout$layout$outcome_label[match(drawn$PANEL, fig_built$layout$layout$PANEL)]),
  outcome_labels)]
drawn_cell <- paste(drawn_outcome, ins_levels[drawn$group], dis_levels[round(drawn$x)], sep = "|")
expected_cell <- paste(main_fig_data$outcome, main_fig_data$ins_type, main_fig_data$dis_cat, sep = "|")
expected_mark <- ifelse(expected_cell %in% sig_a & expected_cell %in% sig_b, "a,b",
                 ifelse(expected_cell %in% sig_a, "a",
                 ifelse(expected_cell %in% sig_b, "b", "")))
stopifnot("Figure 2 check: a drawn letter and its Holm row disagree" =
            !anyNA(drawn_cell) && !anyDuplicated(drawn_cell) &&
            setequal(drawn_cell, expected_cell) &&
            identical(unname(setNames(drawn$label, drawn_cell)[expected_cell]), expected_mark) &&
            all(c(sig_a, sig_b) %in% expected_cell))
cat("Figure 2 letters match the Holm rows:", sum(grepl("a", drawn$label)), "insurance,",
    sum(grepl("b", drawn$label)), "multimorbidity\n")

ggsave(file.path(fig_dir, "main_predicted.pdf"), main_fig, dpi = 600, device = fig_device,
       width = 10, height = 9)
cat("Figure 2 (combo) saved\n")

# eFigure 2: heatmap of contrasts
heatmap_data <- contrasts %>%
  mutate(outcome_label = factor(outcome_labels[outcome], levels = outcome_order))

heatmap_fig <- ggplot(heatmap_data, aes(x = ins_type, y = dis_cat, fill = estimate*100)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.1f", estimate*100)), size = 3) +
  facet_wrap(~outcome_label, nrow = 2, scales = "fixed") +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                       midpoint = 0,
                       name = "Difference in Adjusted Probability of Financial Hardship (pp)") +
  labs(x = NULL, y = NULL) +
  theme_pub +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave(file.path(fig_dir, "main_heatmap.pdf"), heatmap_fig, width = 11, height = 8, dpi = 600, device = fig_device)
cat("eFigure 2 saved\n")

# eFigure 3: forest plot
forest_data <- contrasts %>%
  mutate(outcome_label = factor(outcome_labels[outcome], levels = outcome_order),
         label = paste(ins_type, dis_cat, sep = " | "))

forest_fig <- ggplot(forest_data, aes(x = estimate*100, y = reorder(label, estimate),
                                color = ins_type)) +
  geom_point(size = 2) +
  geom_errorbar(aes(xmin = ci_lower*100, xmax = ci_upper*100), height = 0.2,
                orientation = "y") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  facet_wrap(~outcome_label, nrow = 2, scales = "fixed") +
  labs(x = "Difference in Adjusted Probability of Financial Hardship (percentage points)",
       y = NULL, color = NULL) +
  theme_pub
ggsave(file.path(fig_dir, "main_forest.pdf"), forest_fig, width = 11, height = 9, dpi = 600, device = fig_device)
cat("eFigure 3 saved\n")

# F. 4-category multimorbidity sensitivity (eTable 5), including the '2-3' vs '1'
#    difference within each insurance type.
cat("\n--- Section F: Four-category multimorbidity sensitivity ---\n")
sens4_path <- file.path(proj, "data", "clean", "sens_multimorb_4cat_stata.csv")
if (!file.exists(sens4_path)) {
  stop("Step 7b: ", sens4_path, " not found. Re-run 07a to produce it.")
}

sens4 <- read.csv(sens4_path, stringsAsFactors = FALSE)
cat("Loaded", nrow(sens4), "rows from sens_multimorb_4cat_stata.csv\n")

dis4_levels <- c("0", "1", "2-3", "4+")

# eTable 5 display formats: probabilities "1.9 (1.3-2.9)", differences "1.4 (-0.6 to 3.4)", and
# P values without a leading zero ("<.001"; three decimals below .01, two decimals otherwise).
fmt_prob_ci <- function(e, lo, hi) sprintf("%.1f (%.1f-%.1f)", 100 * e, 100 * lo, 100 * hi)
fmt_diff_ci <- function(e, lo, hi) sprintf("%.1f (%.1f to %.1f)", 100 * e, 100 * lo, 100 * hi)
fmt_p <- function(p) {
  ifelse(p < 0.001, "<.001",
         ifelse(p < 0.01, sub("^0", "", sprintf("%.3f", p)),
                ifelse(sprintf("%.2f", p) == "1.00", ">.99", sub("^0", "", sprintf("%.2f", p)))))
}
etable5_outcome_labels <- c(che_40 = "CHE (40%)", fam_debt_any = "Medical debt",
                            fam_barrier_any = "Cost-related barriers",
                            fam_any_hardship = "Any hardship")

# Holm adjustment over morb_diff and eq_1v23 within each outcome (3 insurance types x
# (3 higher categories vs 0 + 2-3 vs 1) = 12 differences; preds not adjusted)
sens4_tests <- sens4 %>%
  filter(type %in% c("morb_diff", "eq_1v23")) %>%
  group_by(outcome) %>%
  mutate(family_n = n(), p_adj = p.adjust(pvalue, method = "holm")) %>%
  ungroup()
stopifnot("eTable 5: every outcome needs a 12-member Holm family" =
            all(sens4_tests$family_n == 12) &&
            setequal(sens4_tests$outcome, names(etable5_outcome_labels)))

sens4_preds <- sens4 %>%
  filter(type == "pred") %>%
  mutate(
    cell = fmt_prob_ci(estimate, ci_lower, ci_upper),
    ins_type = factor(ins_type, levels = ins_levels),
    dis_cat  = factor(dis_cat, levels = dis4_levels)
  ) %>%
  arrange(outcome, ins_type, dis_cat) %>%
  select(outcome, ins_type, dis_cat, cell, estimate, ci_lower, ci_upper)

sens4_eq <- sens4_tests %>%
  filter(type == "eq_1v23") %>%
  mutate(cell = fmt_diff_ci(estimate, ci_lower, ci_upper)) %>%
  select(outcome, ins_type, comparison, cell, pvalue, p_adj)

primary_compare <- preds %>%
  mutate(
    ins_type = factor(ins_type, levels = ins_levels),
    dis_cat = factor(dis_cat, levels = dis_levels),
    cell = fmt_prob_ci(estimate, ci_lower, ci_upper)
  ) %>%
  select(outcome, ins_type, dis_cat, cell) %>%
  pivot_wider(
    names_from = dis_cat,
    values_from = cell,
    names_prefix = "3-categorized multimorbidity: "
  )

sens4_compare <- sens4_preds %>%
  mutate(
    ins_type = factor(ins_type, levels = ins_levels),
    dis_cat = factor(dis_cat, levels = dis4_levels)
  ) %>%
  select(outcome, ins_type, dis_cat, cell) %>%
  pivot_wider(
    names_from = dis_cat,
    values_from = cell,
    names_prefix = "4-categorized multimorbidity: "
  )

sens4_compare_eq <- sens4_eq %>%
  transmute(
    outcome, ins_type,
    `1 vs 2-3 priority conditions: Difference, pp (95% CI)` = cell,
    `1 vs 2-3 priority conditions: Holm-adjusted P` = fmt_p(p_adj)
  )

sens4_comparison <- primary_compare %>%
  full_join(sens4_compare, by = c("outcome", "ins_type")) %>%
  left_join(sens4_compare_eq, by = c("outcome", "ins_type")) %>%
  arrange(factor(outcome, levels = names(etable5_outcome_labels)),
          factor(ins_type, levels = ins_levels)) %>%
  mutate(Outcome = unname(etable5_outcome_labels[outcome]),
         Insurance = as.character(ins_type)) %>%
  select(-outcome, -ins_type) %>%
  relocate(Outcome, Insurance)

cat("\n--- 4-category predicted probabilities (3 x 4 grid) ---\n")
as.data.frame(sens4_preds) %>% print()

cat("\n--- Difference, '2-3' vs '1', within each insurance type ---\n")
as.data.frame(sens4_eq) %>% print()

cat("\n--- Primary vs 4-category grouping comparison ---\n")
as.data.frame(sens4_comparison) %>% print()

sens4_xlsx <- file.path(proj, "output", "tabs", "multimorb_4cat.xlsx")
write_xlsx(list("3-level vs 4-level comparison" = sens4_comparison), path = sens4_xlsx)
cat("\n4-category sensitivity saved to:", sens4_xlsx, "\n")

cat("\n=== Step 7b complete ===\n")
cat("End:", format(Sys.time()), "\n")

sink(type = "message")
sink(type = "output")
close(log_con)
