###############################################################################
# 08b_hardship_states.R — format the four-state hardship decomposition.
# In:  $data/clean/hardship_states.csv
# Out: $proj/output/figs/hardship_states.pdf   (Figure 3)
#      $proj/output/figs/che_conditional.pdf   (Figure 4)
#      $proj/output/tabs/hardship_states.xlsx (eTable 2)
###############################################################################

proj_env <- Sys.getenv("MEPS_PROTECTION_PROJ", unset = "")
proj <- if (nzchar(proj_env)) proj_env else getwd()
if (!file.exists(file.path(proj, "code", "00_master.do"))) {
  stop("Cannot locate project root. Set MEPS_PROTECTION_PROJ or cd to repo root.")
}

log_file <- file.path(proj, "code", "logs", "08b_hardship_states.log")
log_con  <- file(log_file, open = "wt")
sink(log_con, type = "output", split = TRUE)
sink(log_con, type = "message", append = TRUE)

cat("=== Step 8b: Four-state Hardship Decomposition Output ===\n")
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

# Figure markers: one tier, Holm-adjusted P < .05, same convention as 07b.
mark_of <- function(p_a, p_b) {
  a <- !is.na(p_a) & p_a < 0.05
  b <- !is.na(p_b) & p_b < 0.05
  ifelse(a & b, "a,b", ifelse(a, "a", ifelse(b, "b", "")))
}

ins_levels <- c("No Insurance", "Public Insurance", "Private Insurance")
dis_levels <- c("0", "1-2", "3+")

state_labels <- c(state1 = "CHE and other hardship",
                  state2 = "CHE only",
                  state3 = "Debt or barriers, no CHE",
                  state4 = "No hardship")
state_order  <- unname(state_labels)

# Quantities reported beside the states: CHE, the sum of the two CHE states,
# and the two ratios.
ratio_labels <- c(che_sum       = "CHE (CHE and other hardship plus CHE only)",
                  cond_hardship = "Debt or barriers among households with CHE",
                  detect_che    = "CHE among households with debt or barriers")

# Row order of the differences sheet within a threshold: fixed here rather than alphabetical by
# label, so relabelling a quantity does not move its rows.
diff_qty_order <- c("detect_che", "state1", "state2", "state3", "cond_hardship", "state4")

# A. Load
res <- read.csv(file.path(proj, "data", "clean", "hardship_states.csv"),
                stringsAsFactors = FALSE)
cat("Loaded", nrow(res), "result rows\n")
cat("Thresholds:", paste(sort(unique(res$threshold)), collapse = ", "), "\n")
cat("Quantities:", paste(unique(res$quantity), collapse = ", "), "\n")

stopifnot(all(c("type", "threshold", "quantity", "ins_type", "dis_cat",
                "estimate", "se", "ci_lower", "ci_upper", "pvalue") %in% names(res)))

res <- res %>%
  mutate(ins_type = factor(ins_type, levels = ins_levels),
         dis_cat  = factor(dis_cat,  levels = dis_levels))

# B. Holm within each threshold x quantity panel, across the 12 pairwise
#    differences (6 insurance, 6 multimorbidity) — the convention used by 07b.
diffs <- res %>%
  filter(type %in% c("ins_diff", "morb_diff")) %>%
  group_by(threshold, quantity) %>%
  mutate(p_adj = p.adjust(pvalue, method = "holm")) %>%
  ungroup()

cat("\nHolm applied within", n_distinct(paste(diffs$threshold, diffs$quantity)),
    "threshold x quantity panels,",
    nrow(diffs) / n_distinct(paste(diffs$threshold, diffs$quantity)),
    "comparisons each\n")

preds <- res %>% filter(type == "pred")

marks <- preds %>%
  left_join(diffs %>% filter(type == "ins_diff") %>%
              select(threshold, quantity, ins_type, dis_cat, p_a = p_adj),
            by = c("threshold", "quantity", "ins_type", "dis_cat")) %>%
  left_join(diffs %>% filter(type == "morb_diff") %>%
              select(threshold, quantity, ins_type, dis_cat, p_b = p_adj),
            by = c("threshold", "quantity", "ins_type", "dis_cat")) %>%
  mutate(mark_label = mark_of(p_a, p_b))

# C. Shared styling (same palette and theme as 07b)
palette_colors <- c("No Insurance"      = "#B24745",
                    "Public Insurance"  = "#DF8F44",
                    "Private Insurance" = "#374E55")
# The four states take the three hues of the same palette family that the
# insurance types do not use, plus a light neutral for the residual state, so
# a colour never means an insurance type in one figure and a state in another.
state_colors <- setNames(c("#00A1D5", "#6A6599", "#79AF97", "#DDE1E3"), state_order)
stopifnot(identical(names(state_colors), unname(state_labels)))

fig_dir <- file.path(proj, "output", "figs")
tab_dir <- file.path(proj, "output", "tabs")

theme_pub <- theme_minimal(base_size = 12, base_family = "serif") +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 13),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 12))

dodge <- position_dodge(width = 0.8)

# D. Figure 3 — the four states as a composition, at the 40% threshold.
#    A stacked bar is what makes the partition visible: the four probabilities
#    sum to 100% within every insurance x multimorbidity cell, so the size of
#    the "Debt or barriers, no CHE" band relative to the two CHE bands can be
#    read off directly. Cell estimates with CIs and the Holm-adjusted
#    comparisons are in eTable 2; a stacked composition cannot carry error bars.
comp <- preds %>%
  filter(threshold == 40, quantity %in% names(state_labels)) %>%
  mutate(state = factor(state_labels[quantity], levels = rev(state_order)),
         pct   = 100 * estimate)

total_check <- comp %>% group_by(ins_type, dis_cat) %>% summarise(s = sum(pct), .groups = "drop")
cat("\nComposition sums per cell (should be 100):",
    paste(sprintf("%.1f", total_check$s), collapse = ", "), "\n")
if (any(abs(total_check$s - 100) > 0.5)) {
  stop("Step 8b: the four state probabilities do not sum to 100% within a cell.")
}

comp_lab <- comp %>%
  filter(pct >= 3) %>%
  mutate(lab_col = ifelse(state == "CHE only", "white", "black"))

fig3 <- ggplot(comp, aes(x = dis_cat, y = pct, fill = state)) +
  geom_col(width = 0.7, colour = "white", linewidth = 0.3) +
  geom_text(data = comp_lab,
            aes(label = sprintf("%.1f", pct), colour = lab_col),
            position = position_stack(vjust = 0.5),
            size = 3.1, family = "serif", show.legend = FALSE) +
  facet_wrap(~ ins_type, nrow = 1) +
  scale_fill_manual(values = state_colors, breaks = state_order) +
  scale_colour_identity() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = "Household Multimorbidity",
       y = "Adjusted Share of Households (%)",
       fill = NULL) +
  theme_pub

fig3_bars <- layer_data(fig3, 1)
if (nrow(fig3_bars) != nrow(comp) || anyNA(fig3_bars$fill) || anyNA(fig3_bars$ymax)) {
  stop("Step 8b: Figure 3 would drop or leave uncoloured at least one state band.")
}

fig3_path <- file.path(fig_dir, "hardship_states.pdf")
ggsave(fig3_path, fig3, width = 13, height = 7, dpi = 600, device = fig_device)
cat("Wrote", fig3_path, "\n")

# E. Figure 4 — hardship additional to CHE: among households that already meet
#    the 40% threshold, the share also reporting medical debt or cost-related
#    barriers. This is the quantity the dichotomy hides.
cond <- marks %>%
  filter(threshold == 40, quantity == "cond_hardship") %>%
  mutate(pct = 100 * estimate, lo_pct = 100 * ci_lower, hi_pct = 100 * ci_upper)

y_max <- max(cond$hi_pct, na.rm = TRUE)

fig4 <- ggplot(cond, aes(x = dis_cat, y = pct, fill = ins_type, color = ins_type, group = ins_type)) +
  geom_col(position = dodge, width = 0.7, alpha = 0.55, color = NA) +
  geom_errorbar(aes(ymin = lo_pct, ymax = hi_pct),
                position = dodge, width = 0.2, linewidth = 0.5) +
  geom_point(position = dodge, size = 2.4, shape = 21,
             stroke = 0.8, fill = "white") +
  geom_text(aes(y = hi_pct + 0.02 * y_max, label = mark_label),
            position = dodge, vjust = 0, size = 3.4, family = "serif",
            color = "black", show.legend = FALSE) +
  scale_fill_manual(values = palette_colors) +
  scale_color_manual(values = palette_colors, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.10)),
                     limits = c(0, y_max * 1.12)) +
  labs(x = "Household Multimorbidity",
       y = "Households with Medical Debt or Cost-Related Barriers\namong Those with CHE (%)",
       fill = NULL) +
  theme_pub

# Post-condition: the letters ggplot will draw on Figure 4 are exactly the Holm-significant rows
# (adjusted P < .05) of the 12-member 40% family (6 insurance + 6 multimorbidity differences of
# the share with debt or barriers among households with CHE), recomputed here from the raw P
# values. "a" marks a significant insurance difference, "b" a significant multimorbidity
# difference; the check runs in both directions.
holm40 <- res %>%
  filter(type %in% c("ins_diff", "morb_diff"), threshold == 40, quantity == "cond_hardship") %>%
  mutate(p_holm = p.adjust(pvalue, method = "holm"))
stopifnot("Figure 4 check: the 40% family needs 12 members with no missing P" =
            nrow(holm40) == 12 && !anyNA(holm40$pvalue))
sig_a <- holm40 %>% filter(type == "ins_diff",  p_holm < 0.05) %>%
  with(paste(ins_type, dis_cat, sep = "|"))
sig_b <- holm40 %>% filter(type == "morb_diff", p_holm < 0.05) %>%
  with(paste(ins_type, dis_cat, sep = "|"))

fig4_built <- ggplot_build(fig4)
text_layer <- which(vapply(fig4$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))
stopifnot(length(text_layer) == 1)
drawn <- fig4_built$data[[text_layer]]
drawn_cell <- paste(ins_levels[drawn$group], dis_levels[round(drawn$x)], sep = "|")
expected_cell <- paste(cond$ins_type, cond$dis_cat, sep = "|")
expected_mark <- ifelse(expected_cell %in% sig_a & expected_cell %in% sig_b, "a,b",
                 ifelse(expected_cell %in% sig_a, "a",
                 ifelse(expected_cell %in% sig_b, "b", "")))
stopifnot("Figure 4 check: a drawn letter and its Holm row disagree" =
            !anyNA(drawn_cell) && !anyDuplicated(drawn_cell) &&
            setequal(drawn_cell, expected_cell) &&
            identical(unname(setNames(drawn$label, drawn_cell)[expected_cell]), expected_mark) &&
            all(c(sig_a, sig_b) %in% expected_cell))
cat("Figure 4 letters match the Holm rows:", sum(grepl("a", drawn$label)), "insurance,",
    sum(grepl("b", drawn$label)), "multimorbidity\n")

fig4_path <- file.path(fig_dir, "che_conditional.pdf")
ggsave(fig4_path, fig4, width = 10, height = 7, dpi = 600, device = fig_device)
cat("Wrote", fig4_path, "\n")

# F. eTable 2 — states, CHE and both ratios at all three thresholds, with the
#    Holm-adjusted pairwise differences on a separate sheet.
fmt_ci <- function(e, lo, hi) sprintf("%.1f (%.1f-%.1f)", 100 * e, 100 * lo, 100 * hi)

states_sheet <- preds %>%
  filter(quantity %in% names(state_labels)) %>%
  transmute(`CHE threshold, %` = threshold,
            `Insurance type`   = as.character(ins_type),
            `Household multimorbidity` = as.character(dis_cat),
            State = state_labels[quantity],
            `Adjusted share, % (95% CI)` = fmt_ci(estimate, ci_lower, ci_upper)) %>%
  arrange(`CHE threshold, %`,
          match(`Insurance type`, ins_levels),
          match(`Household multimorbidity`, dis_levels),
          match(State, state_order))

ratios_sheet <- preds %>%
  filter(quantity %in% names(ratio_labels)) %>%
  transmute(`CHE threshold, %` = threshold,
            `Insurance type`   = as.character(ins_type),
            `Household multimorbidity` = as.character(dis_cat),
            Quantity = ratio_labels[quantity],
            `Adjusted share, % (95% CI)` = fmt_ci(estimate, ci_lower, ci_upper)) %>%
  arrange(`CHE threshold, %`, Quantity,
          match(`Insurance type`, ins_levels),
          match(`Household multimorbidity`, dis_levels))

all_labels <- c(state_labels, ratio_labels)
stopifnot(!anyNA(match(diffs$quantity, diff_qty_order)))
diff_sheet <- diffs %>%
  arrange(threshold, match(quantity, diff_qty_order), comparison) %>%
  transmute(`CHE threshold, %` = threshold,
            Quantity = all_labels[quantity],
            Comparison = comparison,
            `Insurance type` = as.character(ins_type),
            `Household multimorbidity` = as.character(dis_cat),
            `Difference, pp (95% CI)` = sprintf("%.1f (%.1f to %.1f)",
                                                100 * estimate, 100 * ci_lower, 100 * ci_upper),
            `Holm-adjusted P` = ifelse(p_adj < 0.001, "<.001", sprintf("%.3f", p_adj)))

stopifnot(nrow(states_sheet) == 3 * 9 * length(state_labels),
          nrow(ratios_sheet) == 3 * 9 * length(ratio_labels))

out_xlsx <- file.path(tab_dir, "hardship_states.xlsx")
write_xlsx(list(states      = states_sheet,
                ratios      = ratios_sheet,
                differences = diff_sheet),
           path = out_xlsx)
cat("Wrote", out_xlsx, "with", nrow(states_sheet), "state rows,",
    nrow(ratios_sheet), "CHE and ratio rows,", nrow(diff_sheet), "difference rows\n")

# G. CHE and the two ratios at all three thresholds (the last three columns of eTable 2),
#    printed so the log carries them.
cat("\n--- CHE and ratio estimates, all thresholds (%) ---\n")
preds %>%
  filter(quantity %in% names(ratio_labels)) %>%
  mutate(pct = sprintf("%.1f", 100 * estimate)) %>%
  select(threshold, quantity, ins_type, dis_cat, pct) %>%
  arrange(threshold, quantity, ins_type, dis_cat) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

cat("\n=== Step 8b complete ===\n")
cat("End:", format(Sys.time()), "\n")

sink(type = "message")
sink(type = "output")
close(log_con)
