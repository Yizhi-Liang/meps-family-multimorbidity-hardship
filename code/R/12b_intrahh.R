###############################################################################
# 12b_intrahh.R — format the intra-household OOP allocation analysis.
# In:  $data/clean/intrahh_stata.csv
# Out: $proj/output/tabs/intrahh.xlsx,
#      $proj/output/figs/intrahh.pdf
###############################################################################

proj_env <- Sys.getenv("MEPS_PROTECTION_PROJ", unset = "")
proj <- if (nzchar(proj_env)) proj_env else getwd()
if (!file.exists(file.path(proj, "code", "00_master.do"))) {
  stop("Cannot locate project root. Set MEPS_PROTECTION_PROJ or cd to repo root.")
}

log_file <- file.path(proj, "code", "logs", "12b_intrahh.log")
log_con  <- file(log_file, open = "wt")
sink(log_con, type = "output", split = TRUE)
sink(log_con, type = "message", append = TRUE)

cat("=== Step 12b: Intra-household OOP Allocation Output ===\n")
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

# Figure markers: one tier, Holm-adjusted P < .05. "a" = different from No Insurance at the same
# multimorbidity level (Holm within each member group across the 4 insurance differences); "b" = the
# paired contrast with members with priority conditions in the same households (Holm across the 6
# paired contrasts), drawn only in the members-without facet; "a,b" = both.
mark_of <- function(p_a, p_b) {
  a <- !is.na(p_a) & p_a < 0.05
  b <- !is.na(p_b) & p_b < 0.05
  ifelse(a & b, "a,b", ifelse(a, "a", ifelse(b, "b", "")))
}

ins_levels <- c("No Insurance", "Public Insurance", "Private Insurance")
dis_levels <- c("0", "1-2", "3+")

# A. Load
csv_path <- file.path(proj, "data", "clean", "intrahh_stata.csv")
res <- read.csv(csv_path, stringsAsFactors = FALSE)

cat("Loaded", nrow(res), "rows from intrahh_stata.csv\n")
cat("Outcomes:", paste(unique(res$outcome), collapse = ", "), "\n")
cat("Types:",    paste(unique(res$type),    collapse = ", "), "\n\n")

# B. Holm within outcome for the insurance differences (ins_diff: the 1-2 and 3+ levels against No
#    Insurance, 4 per member group; the 0 level is empty in mixed-composition households) and across
#    the 6 non-empty cells of the paired with-vs-without contrast
diffs <- res %>%
  filter(type == "ins_diff") %>%
  filter(!is.na(pvalue)) %>%
  group_by(outcome) %>%
  mutate(p_adj = p.adjust(pvalue, method = "holm")) %>%
  ungroup()

pt_vs_np <- res %>%
  filter(type == "pt_vs_np") %>%
  filter(!is.na(pvalue)) %>%
  mutate(p_adj = p.adjust(pvalue, method = "holm"))

preds <- res %>%
  filter(type == "pred") %>%
  filter(!is.na(estimate)) %>%
  mutate(
    ins_type = factor(ins_type, levels = ins_levels),
    dis_cat  = factor(dis_cat, levels = dis_levels)
  )

# C. Print
cat("--- Predicted per-capita normalized OOP burden (3 x 3 grid) ---\n")
preds %>%
  mutate(cell = sprintf("%.3f (%.3f, %.3f)",
                        estimate, ci_lower, ci_upper)) %>%
  select(outcome, ins_type, dis_cat, cell) %>%
  arrange(outcome, ins_type, dis_cat) %>%
  as.data.frame() %>% print()

cat("\n--- Paired test, members with vs without priority conditions (log1p-scale diff) ---\n")
pt_vs_np %>%
  mutate(cell = sprintf("%.3f (%.3f, %.3f) p=%.4f p_adj=%.4f",
                        estimate, ci_lower, ci_upper,
                        pvalue, p_adj)) %>%
  select(ins_type, dis_cat, cell) %>%
  as.data.frame() %>% print()

# D. Workbook
make_pred_sheet <- function(target_outcome) {
  preds %>%
    filter(outcome == target_outcome) %>%
    mutate(cell = sprintf("%.3f (%.3f, %.3f)",
                          estimate, ci_lower, ci_upper)) %>%
    arrange(ins_type, dis_cat) %>%
    select(ins_type, dis_cat, cell, estimate, ci_lower, ci_upper)
}

patient_sheet <- make_pred_sheet("patient_burden")
nonpat_sheet  <- make_pred_sheet("nonpat_burden")

diffs_sheet <- diffs %>%
  mutate(cell = sprintf("%.3f (%.3f, %.3f)",
                        estimate, ci_lower, ci_upper)) %>%
  select(outcome, type, ins_type, dis_cat, comparison, cell,
         estimate, ci_lower, ci_upper, pvalue, p_adj)

pt_vs_np_sheet <- pt_vs_np %>%
  mutate(cell = sprintf("%.3f (%.3f, %.3f)",
                        estimate, ci_lower, ci_upper)) %>%
  select(ins_type, dis_cat, comparison, cell,
         estimate, ci_lower, ci_upper, pvalue, p_adj)

workbook <- list(
  "patient_burden"         = patient_sheet,
  "nonpat_burden"          = nonpat_sheet,
  "With vs Without Conditions" = pt_vs_np_sheet,
  "Pairwise Diffs"         = diffs_sheet
)

out_xlsx <- file.path(proj, "output", "tabs", "intrahh.xlsx")
write_xlsx(workbook, path = out_xlsx)
cat("\nExcel workbook saved to:", out_xlsx, "\n")

# E. Side-by-side figure (Members with / without priority conditions).
#    Markers: "a" = Holm P < .05 vs No Insurance at the same cell; "b" = Holm P < .05 for the paired
#    with-vs-without contrast, drawn only in the "without" facet (the "with" facet is the reference).
#    dis_cat=="0" is absent (mixed-family sample requires >=1 patient).
palette_colors <- c("No Insurance" = "#B24745",
                 "Public Insurance" = "#DF8F44",
                 "Private Insurance" = "#374E55")

outcome_labels <- c(patient_burden = "Members with priority conditions",
                    nonpat_burden  = "Members without priority conditions")

ins_holm <- diffs %>%
  select(outcome, ins_type, dis_cat, ins_p = p_adj)

pt_holm <- pt_vs_np %>%
  select(ins_type, dis_cat, pt_p = p_adj)

fig_data <- preds %>%
  left_join(ins_holm, by = c("outcome", "ins_type", "dis_cat")) %>%
  left_join(pt_holm,  by = c("ins_type", "dis_cat")) %>%
  mutate(
    outcome_label = factor(outcome_labels[outcome],
                           levels = c("Members with priority conditions",
                                      "Members without priority conditions")),
    mark_label = mark_of(ins_p, ifelse(outcome_label == "Members without priority conditions", pt_p, NA_real_)),
    ins_type = factor(as.character(ins_type), levels = ins_levels),
    dis_cat  = factor(as.character(dis_cat),  levels = dis_levels),
    est_pct = estimate  * 100,
    # Workbook keeps raw back-transformed CIs; figure clamps LB at 0 visually
    lo_pct  = pmax(0, ci_lower) * 100,
    hi_pct  = ci_upper  * 100
  )

# Post-condition: every letter drawn in the figure marks a Holm-significant row (p_adj < .05) of its
# family, and every Holm-significant row carries its letter. "a": the ins_diff family, 4 per member
# group; "b": the pt_vs_np family, 6 cells, drawn in the members-without facet only. A disagreement,
# including a join that drops or duplicates a row, stops the script before the figure is written.
cell_key <- function(outcome, ins_type, dis_cat) {
  paste(rep_len(outcome, length(ins_type)), as.character(ins_type), as.character(dis_cat), sep = "|")
}
stopifnot(
  identical(sort(unique(diffs$outcome)), c("nonpat_burden", "patient_burden")),
  all(table(diffs$outcome) == 4),
  nrow(pt_vs_np) == 6,
  nrow(fig_data) == nrow(preds)
)
expected_a <- with(diffs[diffs$p_adj < 0.05, ], cell_key(outcome, ins_type, dis_cat))
expected_b <- with(pt_vs_np[pt_vs_np$p_adj < 0.05, ],
                   cell_key("nonpat_burden", ins_type, dis_cat))
drawn_a <- with(fig_data[fig_data$mark_label %in% c("a", "a,b"), ],
                cell_key(outcome, ins_type, dis_cat))
drawn_b <- with(fig_data[fig_data$mark_label %in% c("b", "a,b"), ],
                cell_key(outcome, ins_type, dis_cat))
stopifnot(identical(sort(drawn_a), sort(expected_a)),
          identical(sort(drawn_b), sort(expected_b)))
cat("Post-condition passed: letters a =", length(drawn_a), ", b =", length(drawn_b),
    "match the Holm-significant rows\n")

y_axis_max <- max(fig_data$hi_pct, na.rm = TRUE)

theme_pub <- theme_minimal(base_size = 12, base_family = "serif") +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 13),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 12))

dodge <- position_dodge(width = 0.8)

fig <- ggplot(fig_data, aes(x = dis_cat, y = est_pct,
                            fill = ins_type, color = ins_type,
                            group = ins_type)) +
  geom_col(position = dodge, width = 0.7, alpha = 0.55, color = NA) +
  geom_errorbar(aes(ymin = lo_pct, ymax = hi_pct),
                position = dodge, width = 0.2, linewidth = 0.5) +
  geom_point(position = dodge, size = 2.4, shape = 21,
             stroke = 0.8, fill = "white", show.legend = FALSE) +
  geom_text(aes(y = hi_pct + 0.02 * y_axis_max, label = mark_label),
            position = dodge, vjust = 0, size = 3.4, family = "serif",
            color = "black", show.legend = FALSE) +
  facet_wrap(~outcome_label, nrow = 1, scales = "fixed") +
  scale_fill_manual(values = palette_colors) +
  scale_color_manual(values = palette_colors, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08)),
                     limits = c(0, y_axis_max * 1.12)) +
  labs(x = "Household Multimorbidity",
       y = "Per-capita Normalized OOP Burden (%)",
       fill = NULL) +
  theme_pub

fig_path <- file.path(proj, "output", "figs", "intrahh.pdf")
ggsave(fig_path, fig, width = 10, height = 5, dpi = 600, device = fig_device)
cat("Figure saved to:", fig_path, "\n")

cat("\n=== Step 12b complete ===\n")
cat("End:", format(Sys.time()), "\n")

sink(type = "message")
sink(type = "output")
close(log_con)
