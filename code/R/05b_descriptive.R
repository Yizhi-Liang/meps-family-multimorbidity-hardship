###############################################################################
# 05b_descriptive.R — Table 1 (main), eTable 1 (full) and the observed hardship of
# non-positive-income households (summarized in eMethods 1).
#
# Three descriptive workbooks in journal format (no 95% CIs). The row order, row labels,
# column headers and footnote of the first two follow the manuscript's Table 1 and the
# supplement's eTable 1:
#   characteristics_by_multimorbidity.xlsx  — Table 1: columns = household multimorbidity
#                  (0 / 1-2 / 3+ / Overall); insurance type is a row block after
#                  calendar year.
#   characteristics_by_insurance.xlsx — eTable 1: columns = insurance (No / Public /
#                  Private / Overall) x multimorbidity (0 / 1-2 / 3+ / Sub-total); no
#                  insurance row block.
#   nonpos_income_hardship.xlsx — described below.
#
# Two reporting bases, made explicit in the row labels and the section headers:
#   * Proportion of individuals WITHIN a household (continuous): male, age<18,
#     race/ethnicity, education, employment, marital, nativity — reported as the
#     survey-weighted mean (SD) on the percent scale.
#   * Proportion of HOUSEHOLDS among all households (categorical):
#     multigenerational, insurance, region, poverty, calendar year, financial
#     hardship outcomes — reported as the unweighted No. of households (weighted %).
#   * Household size and mean age — survey-weighted mean (SD).
# Survey weighting uses the pooled MEPS weight (poolwt). Continuous values carry
# 1 decimal; household percentages are rounded to integers.
#
# nonpos_income_hardship.xlsx (summarized in eMethods 1) compares observed hardship
# (medical debt, cost-related barriers; CHE shown for the coding convention) between
# the full analytic sample and the non-positive-income subgroup, by insurance. It shares
# the row labels and column header of the two tables above.
#
# In:  $data/clean/family_analytic.csv
# Out: $proj/output/tabs/characteristics_by_multimorbidity.xlsx, $proj/output/tabs/characteristics_by_insurance.xlsx,
#      $proj/output/tabs/nonpos_income_hardship.xlsx
###############################################################################

proj_env <- Sys.getenv("MEPS_PROTECTION_PROJ", unset = "")
proj <- if (nzchar(proj_env)) proj_env else getwd()
if (!file.exists(file.path(proj, "code", "00_master.do"))) {
  stop("Cannot locate project root. Set MEPS_PROTECTION_PROJ or cd to project root.")
}

log_file <- file.path(proj, "code", "logs", "05b_descriptive.log")
log_con  <- file(log_file, open = "wt")
sink(log_con, type = "output", split = TRUE)
sink(log_con, type = "message", append = TRUE)

cat("=== Step 5b: Descriptive Table 1, eTable 1 and non-positive-income hardship ===\n")
cat("Start:", format(Sys.time()), "\n\n")

library(openxlsx)

# A. Load ------
dat <- read.csv(file.path(proj, "data", "clean", "family_analytic.csv"),
                stringsAsFactors = FALSE)

dat$fam_ins_type    <- factor(dat$fam_ins_type,
                              levels = c("Uninsured", "Public", "Private"),
                              labels = c("No Insurance", "Public Insurance",
                                         "Private Insurance"))
dat$fam_disease_cat <- factor(dat$fam_disease_cat, levels = c("0", "1-2", "3+"))

cat("Loaded", nrow(dat), "households\n")
cat("Insurance:\n");        print(table(dat$fam_ins_type, useNA = "ifany"))
cat("\nMultimorbidity:\n"); print(table(dat$fam_disease_cat, useNA = "ifany"))
# Weighted shares by multimorbidity, quoted in the Results (Table 1 prints the weighted N only)
mm_wt <- tapply(dat$poolwt, dat$fam_disease_cat, sum)
cat("\nMultimorbidity, weighted % of households:\n"); print(round(100 * mm_wt / sum(mm_wt), 1))

# Household-level 0/1 indicator columns for the categorical variables ----------
dat$ins_none <- as.integer(dat$fam_ins_type == "No Insurance")
dat$ins_pub  <- as.integer(dat$fam_ins_type == "Public Insurance")
dat$ins_priv <- as.integer(dat$fam_ins_type == "Private Insurance")
dat$reg_ne <- as.integer(dat$fam_region == "1 NORTHEAST")
dat$reg_mw <- as.integer(dat$fam_region == "2 MIDWEST")
dat$reg_s  <- as.integer(dat$fam_region == "3 SOUTH")
dat$reg_w  <- as.integer(dat$fam_region == "4 WEST")
dat$pov_poor <- as.integer(dat$fam_povcat == "1 POOR/NEGATIVE")
dat$pov_near <- as.integer(dat$fam_povcat == "2 NEAR POOR")
dat$pov_low  <- as.integer(dat$fam_povcat == "3 LOW INCOME")
dat$pov_mid  <- as.integer(dat$fam_povcat == "4 MIDDLE INCOME")
dat$pov_high <- as.integer(dat$fam_povcat == "5 HIGH INCOME")
for (y in 2018:2024) dat[[paste0("yr_", y)]] <- as.integer(dat$YEAR == y)

# B. Survey-weighted moment helpers (pooled weight; describe the sample) -------
w_mean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) return(NA_real_)
  sum(w[ok] * x[ok]) / sum(w[ok])
}
w_sd <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (sum(ok) < 2) return(NA_real_)
  m <- sum(w[ok] * x[ok]) / sum(w[ok])
  sqrt(sum(w[ok] * (x[ok] - m)^2) / sum(w[ok]))
}
f_cont  <- function(d, v) { m <- w_mean(d[[v]], d$poolwt); s <- w_sd(d[[v]], d$poolwt)
                            if (is.na(m)) "--" else sprintf("%.1f (%.1f)", m, s) }
f_pmean <- function(d, v) { m <- w_mean(d[[v]], d$poolwt); s <- w_sd(d[[v]], d$poolwt)
                            if (is.na(m)) "--" else sprintf("%.1f (%.1f)", m * 100, s * 100) }
f_bin   <- function(d, v) { p <- w_mean(d[[v]], d$poolwt); n <- sum(d[[v]] == 1, na.rm = TRUE)
                            if (is.na(p)) "--" else sprintf("%s (%.0f)", format(n, big.mark = ","), p * 100) }

cell_val <- function(sp, d) {
  switch(sp$kind,
         n     = format(nrow(d), big.mark = ","),
         N     = sprintf("%.1f", sum(d$poolwt, na.rm = TRUE) / 1e6),
         cont  = f_cont(d, sp$var),
         pmean = f_pmean(d, sp$var),
         bin   = f_bin(d, sp$var),
         "")
}

# C. Row specifications --------------------------------------------------------
sp  <- function(kind, label, var = NA) list(kind = kind, label = label, var = var)

TOP <- list(
  sp("n", "Unweighted sample size (n)"),
  sp("N", "Weighted sample size (N, [million])")
)
COMP <- list(
  sp("hdr",  "Household composition"),
  sp("cont", "Household size (mean [SD])",                "fam_size"),
  sp("cont", "Age (mean [SD])",                           "mean_age"),
  sp("pmean", "Proportion of males (mean [SD])",          "prop_male"),
  sp("pmean", "Proportion of members aged <18 y (mean [SD])", "prop_young"),
  sp("bin",  "Household with \u22652 age bands (<18, 18-64, \u226565 y) (n [%])", "is_multigenerational")
)
INS <- list(
  sp("hdr", "Household-level Insurance (n [%])"),
  sp("bin", "No insurance",      "ins_none"),
  sp("bin", "Public insurance",  "ins_pub"),
  sp("bin", "Private insurance", "ins_priv")
)
RACE <- list(
  sp("hdr",   "Race / ethnicity composition (mean [SD])"),
  sp("pmean", "Proportion of Hispanic",           "prop_race_hisp"),
  sp("pmean", "Proportion of non-Hispanic Asian", "prop_race_asian"),
  sp("pmean", "Proportion of non-Hispanic Black", "prop_race_black"),
  sp("pmean", "Proportion of non-Hispanic White", "prop_race_white"),
  sp("pmean", "Proportion of non-Hispanic Other", "prop_race_other")
)
EDU <- list(
  sp("hdr",   "Education (mean [SD])"),
  sp("pmean", "Proportion of bachelor\u2019s degree or higher", "prop_college"),
  sp("pmean", "Proportion with no degree",                       "prop_no_degree")
)
EMP <- list(
  sp("hdr",   "Employment (mean [SD])"),
  sp("pmean", "Proportion of employed", "prop_employed")
)
MAR <- list(
  sp("hdr",   "Marital and nativity (mean [SD])"),
  sp("pmean", "Proportion married among adults", "prop_married"),
  sp("pmean", "Proportion of US-born",           "prop_us_born")
)
REGION <- list(
  sp("hdr", "Geographic region (n [%])"),
  sp("bin", "Northeast", "reg_ne"), sp("bin", "Midwest", "reg_mw"),
  sp("bin", "South",     "reg_s"),  sp("bin", "West",    "reg_w")
)
POV <- list(
  sp("hdr", "Poverty category (n [%])"),
  sp("bin", "Poor", "pov_poor"), sp("bin", "Near-poor", "pov_near"),
  sp("bin", "Low income", "pov_low"), sp("bin", "Middle income", "pov_mid"),
  sp("bin", "High income", "pov_high")
)
YEAR <- c(list(sp("hdr", "Calendar year (n [%])")),
          lapply(2018:2024, function(y) sp("bin", as.character(y), paste0("yr_", y))))
OUT <- list(
  sp("hdr", "Financial hardship outcomes (n [%])"),
  sp("bin", "CHE at 40% of income",         "che_40"),
  sp("bin", "Medical debt",                 "fam_debt_any"),
  sp("bin", "Cost-related barriers",        "fam_barrier_any"),
  sp("bin", "Any hardship",                 "fam_any_hardship")
)

# Row order follows the printed tables: Table 1 carries the insurance block after
# calendar year; eTable 1 has none (insurance is its column grouping).
specs_main <- c(TOP, COMP, RACE, EDU, EMP, MAR, REGION, POV, YEAR, INS, OUT)
specs_etab <- c(TOP, COMP, RACE, EDU, EMP, MAR, REGION, POV, YEAR, OUT)

COL1_HEADER <- "Variable"

n_hh_txt <- format(nrow(dat), big.mark = ",")

# Footnotes as printed under Table 1 and eTable 1 (the household count is the analytic sample size).
FOOTNOTE <- paste0(
  "Survey-weighted characteristics of ", n_hh_txt,
  " households from the Medical Expenditure Panel Survey 2018-2024, grouped by ",
  "household-level multimorbidity (0, 1-2, or 3+ priority conditions). Continuous ",
  "variables are reported as survey-weighted mean with standard deviation (SD), and ",
  "categorical variables are reported as unweighted sample size (n) with weighted ",
  "percentage rounded to whole numbers. No degree indicates no high school diploma, ",
  "General Educational Development (GED) credential, or higher degree. Outcomes are ",
  "defined as follows. Catastrophic health expenditure (CHE) is household ",
  "out-of-pocket spending exceeding 40% of pre-tax household income; households ",
  "reporting pre-tax income of $0 or less with any out-of-pocket spending are also ",
  "classified as having CHE (eMethods 1). Medical debt is a household respondent's ",
  "report of problems paying medical bills in the prior 12 months, medical bills ",
  "currently being paid off over time, or medical bills the household is currently ",
  "unable to pay. Cost-related barriers indicate that any member delayed or forwent ",
  "medical, dental, or prescription care because of cost. Any hardship is any of the ",
  "three.")

FOOTNOTE_ETAB <- paste0(
  "Survey-weighted characteristics of ", n_hh_txt,
  " households from the Medical Expenditure Panel Survey 2018-2024, grouped first by ",
  "household insurance type (No Insurance / Public Insurance / Private Insurance) and ",
  "within each insurance type by multimorbidity (0, 1-2, or 3+ priority conditions). ",
  "Continuous variables are reported as survey-weighted mean with standard deviation ",
  "(SD), and categorical variables are reported as unweighted sample size (n) with ",
  "weighted percentage rounded to whole numbers. No degree indicates no high school ",
  "diploma, General Educational Development (GED) credential, or higher degree. ",
  "Financial hardship outcomes are defined as in Table 1; CHE indicates catastrophic ",
  "health expenditure at the 40% threshold.")

# D. Workbook writer (one sheet; openxlsx) -------------------------------------
write_table <- function(path, sheet, specs, col_subsets, sub_labels,
                        group_title = NULL, col_top = NULL, footnote = FOOTNOTE,
                        fn_height = 92) {
  # creator = "": keep the account name of whoever runs the pipeline out of the workbook metadata
  wb <- createWorkbook(creator = ""); addWorksheet(wb, sheet)
  ncol <- length(sub_labels)

  st_grp  <- createStyle(textDecoration = "bold", halign = "center", valign = "center",
                         fgFill = "#EFEFEF", border = "TopBottomLeftRight",
                         borderColour = "#999999", wrapText = TRUE)
  st_hdr  <- createStyle(textDecoration = "bold", halign = "center", valign = "center",
                         border = "TopBottomLeftRight", borderColour = "#999999", wrapText = TRUE)
  st_c1   <- createStyle(textDecoration = "bold", halign = "left", valign = "center",
                         border = "TopBottomLeftRight", borderColour = "#999999", wrapText = TRUE)
  st_sec  <- createStyle(textDecoration = "bold", halign = "left", wrapText = TRUE)
  st_data <- createStyle(halign = "left")
  st_note <- createStyle(fontSize = 9, textDecoration = "italic", valign = "top", wrapText = TRUE)

  # First-column header (merged across the two header rows)
  writeData(wb, sheet, x = COL1_HEADER, startCol = 1, startRow = 1)
  mergeCells(wb, sheet, cols = 1, rows = 1:2)
  addStyle(wb, sheet, st_c1, rows = 1:2, cols = 1, gridExpand = TRUE, stack = TRUE)

  if (!is.null(group_title)) {                     # main: one group label spanning all cols
    writeData(wb, sheet, x = group_title, startCol = 2, startRow = 1)
    mergeCells(wb, sheet, cols = 2:(1 + ncol), rows = 1)
    addStyle(wb, sheet, st_grp, rows = 1, cols = 2:(1 + ncol), gridExpand = TRUE, stack = TRUE)
  } else {                                         # eTable: insurance group labels
    col <- 2
    for (g in col_top) {
      writeData(wb, sheet, x = g$label, startCol = col, startRow = 1)
      if (g$span > 1) mergeCells(wb, sheet, cols = col:(col + g$span - 1), rows = 1)
      addStyle(wb, sheet, st_grp, rows = 1, cols = col:(col + g$span - 1),
               gridExpand = TRUE, stack = TRUE)
      col <- col + g$span
    }
  }
  for (j in seq_len(ncol)) {                        # row 2: column sub-labels
    writeData(wb, sheet, x = sub_labels[j], startCol = 1 + j, startRow = 2)
    addStyle(wb, sheet, st_hdr, rows = 2, cols = 1 + j, gridExpand = TRUE, stack = TRUE)
  }
  setRowHeights(wb, sheet, rows = 1:2, heights = c(32, 30))

  for (i in seq_along(specs)) {                     # data rows (start at row 3)
    r <- i + 2; s <- specs[[i]]
    writeData(wb, sheet, x = s$label, startCol = 1, startRow = r)
    if (s$kind == "hdr") {
      addStyle(wb, sheet, st_sec, rows = r, cols = 1, stack = TRUE); next
    }
    addStyle(wb, sheet, st_data, rows = r, cols = 1, stack = TRUE)
    for (j in seq_len(ncol)) {
      writeData(wb, sheet, x = cell_val(s, col_subsets[[j]]), startCol = 1 + j, startRow = r)
    }
  }

  fr <- length(specs) + 4                           # footnote row (one blank spacer row after data, matching the printed tables)
  writeData(wb, sheet, x = footnote, startCol = 1, startRow = fr)
  mergeCells(wb, sheet, cols = 1:(1 + ncol), rows = fr)
  addStyle(wb, sheet, st_note, rows = fr, cols = 1, gridExpand = TRUE, stack = TRUE)
  setRowHeights(wb, sheet, rows = fr, heights = fn_height)

  setColWidths(wb, sheet, cols = 1, widths = 54)
  setColWidths(wb, sheet, cols = 2:(1 + ncol), widths = if (ncol <= 4) 16 else 13)
  freezePane(wb, sheet, firstActiveRow = 3, firstActiveCol = 2)
  saveWorkbook(wb, file = path, overwrite = TRUE)
}

# E. Build the two tables ------------------------------------------------------
sub <- function(cond) dat[cond, , drop = FALSE]

# Main: 4 multimorbidity columns (insurance is a row block)
mcols <- list(sub(dat$fam_disease_cat == "0"), sub(dat$fam_disease_cat == "1-2"),
              sub(dat$fam_disease_cat == "3+"), dat)
write_table(file.path(proj, "output", "tabs", "characteristics_by_multimorbidity.xlsx"), "Table 1",
            specs_main, mcols, c("0", "1-2", "3+", "Overall"),
            group_title = "Household multimorbidity, No. of priority conditions",
            footnote = FOOTNOTE, fn_height = 108)
cat("\nTable 1 saved (", length(specs_main), "rows x 4 cols )\n")

# eTable: 16 columns = insurance (4) x multimorbidity (4)
ins_defs <- list(list(lab = "No Insurance",      val = "No Insurance"),
                 list(lab = "Public Insurance",  val = "Public Insurance"),
                 list(lab = "Private Insurance", val = "Private Insurance"),
                 list(lab = "Overall",           val = NA))
dis_defs <- c("0", "1-2", "3+", NA)
ecols <- list(); esub <- c(); col_top <- list()
for (g in ins_defs) {
  col_top <- c(col_top, list(list(label = g$lab, span = length(dis_defs))))
  for (dv in dis_defs) {
    d <- if (is.na(g$val)) dat else sub(dat$fam_ins_type == g$val)
    if (!is.na(dv)) d <- d[d$fam_disease_cat == dv, , drop = FALSE]
    ecols[[length(ecols) + 1]] <- d
    esub <- c(esub, if (is.na(dv)) "Sub-total" else dv)
  }
}
esub[length(esub)] <- "Total"
write_table(file.path(proj, "output", "tabs", "characteristics_by_insurance.xlsx"), "eTable 1",
            specs_etab, ecols, esub, col_top = col_top, footnote = FOOTNOTE_ETAB)
cat("eTable 1 saved (", length(specs_etab), "rows x", length(esub), "cols )\n")

# E2. Observed hardship: full sample vs non-positive-income --------
dat$nonpos <- as.integer(dat$fam_faminc <= 0)
nonpos_n <- sum(dat$nonpos)          # reported in the workbook footnote
stopifnot(nonpos_n > 0)

specs_s2 <- c(TOP, list(
  sp("hdr", "Financial hardship outcomes (n [%])"),
  sp("bin", "CHE at 40% of income",          "che_40"),
  sp("bin", "Medical debt",                  "fam_debt_any"),
  sp("bin", "Cost-related barriers",         "fam_barrier_any")
))

s2_cols <- list(); s2_sub <- c(); s2_top <- list()
for (g in list(list(lab = "Full analytic sample",            keep = rep(TRUE, nrow(dat))),
               list(lab = "Non-positive-income households",  keep = dat$nonpos == 1))) {
  s2_top <- c(s2_top, list(list(label = g$lab, span = 4)))
  for (iv in list("No Insurance", "Public Insurance", "Private Insurance", NA)) {
    d <- dat[g$keep, , drop = FALSE]
    if (!is.na(iv)) d <- d[d$fam_ins_type == iv, , drop = FALSE]
    s2_cols[[length(s2_cols) + 1]] <- d
    s2_sub <- c(s2_sub, if (is.na(iv)) "Overall" else as.character(iv))
  }
}

FOOTNOTE_S2 <- paste0(
  "Survey-weighted percentages with unweighted household counts, using pooled MEPS weights ",
  "(2018-2024). Non-positive-income households (n = ", format(nonpos_n, big.mark = ","),
  ") reported family income of $0 or ",
  "less (legitimate in MEPS through business or investment losses) and are retained in the ",
  "primary analysis. Because CHE is a ratio to income, any non-positive-income household with ",
  "positive out-of-pocket spending is classified as CHE by convention, so the CHE row for this ",
  "group reflects the coding convention rather than observed spending burden; medical debt and ",
  "cost-related barriers are measured identically in both groups. CHE indicates catastrophic health ",
  "expenditure.")

write_table(file.path(proj, "output", "tabs", "nonpos_income_hardship.xlsx"),
            "Nonpositive income", specs_s2, s2_cols, s2_sub, col_top = s2_top,
            footnote = FOOTNOTE_S2)
cat("Non-positive-income hardship workbook saved (", length(specs_s2), "rows x", length(s2_sub), "cols )\n")

cat("\n=== Step 5b complete ===\n")
cat("End:", format(Sys.time()), "\n")

sink(type = "message")
sink(type = "output")
close(log_con)
