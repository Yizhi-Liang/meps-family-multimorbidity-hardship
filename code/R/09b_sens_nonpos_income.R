###############################################################################
# 09b_sens_nonpos_income.R -- format non-positive-income sensitivity outputs.
# In:  data/clean/sens_nonpos_income_diag_stata.csv,
#      data/clean/sens_nonpos_income_results_stata.csv,
#      data/clean/sens_nonpos_income_states_stata.csv,
#      data/clean/hardship_overlap_stata.csv
# Out: output/tabs/sens_nonpos_income.xlsx (sheets: the diagnostics of the
#      non-positive-income CHE rule; the re-estimated logistic models; the four
#      hardship states and the two ratios among positive-income households; the
#      hardship-domain overlap)
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
log_file <- file.path(proj, "code", "logs", "09b_sens_nonpos_income.log")
log_con <- file(log_file, open = "wt")
sink(log_con, type = "output", split = TRUE)
sink(log_con, type = "message", append = TRUE)
on.exit({
  sink(type = "message")
  sink(type = "output")
  close(log_con)
}, add = TRUE)

cat("=== Step 9b: Non-positive-income sensitivity workbook ===\n")
cat("Start:", format(Sys.time()), "\n\n")

library(writexl)

clean_dir <- file.path(proj, "data", "clean")
tabs_dir <- file.path(proj, "output", "tabs")
dir.create(tabs_dir, showWarnings = FALSE, recursive = TRUE)

diag <- read.csv(file.path(clean_dir, "sens_nonpos_income_diag_stata.csv"),
                 stringsAsFactors = FALSE)
res <- read.csv(file.path(clean_dir, "sens_nonpos_income_results_stata.csv"),
                stringsAsFactors = FALSE)
overlap <- read.csv(file.path(clean_dir, "hardship_overlap_stata.csv"),
                    stringsAsFactors = FALSE)
states <- read.csv(file.path(clean_dir, "sens_nonpos_income_states_stata.csv"),
                   stringsAsFactors = FALSE)

round_numeric <- function(x) {
  is_num <- vapply(x, is.numeric, logical(1))
  x[is_num] <- lapply(x[is_num], function(v) round(v, 6))
  x
}

# Labels, column names and number format of the state and ratio sheets, as in 08b.
ins_levels <- c("No Insurance", "Public Insurance", "Private Insurance")
dis_levels <- c("0", "1-2", "3+")

state_labels <- c(state1 = "CHE and other hardship",
                  state2 = "CHE only",
                  state3 = "Debt or barriers, no CHE",
                  state4 = "No hardship")
state_order  <- unname(state_labels)

ratio_labels <- c(che_sum       = "CHE (CHE and other hardship plus CHE only)",
                  cond_hardship = "Debt or barriers among households with CHE",
                  detect_che    = "CHE among households with debt or barriers")

fmt_ci <- function(e, lo, hi) sprintf("%.1f (%.1f-%.1f)", 100 * e, 100 * lo, 100 * hi)

quantity_sheet <- function(d, labels, label_col) {
  d <- d[d$quantity %in% names(labels), , drop = FALSE]
  out <- data.frame(d$threshold, d$ins_type, d$dis_cat,
                    unname(labels[d$quantity]),
                    fmt_ci(d$estimate, d$ci_lower, d$ci_upper),
                    stringsAsFactors = FALSE)
  names(out) <- c("CHE threshold, %", "Insurance type", "Household multimorbidity",
                  label_col, "Adjusted share, % (95% CI)")
  out
}

states_sheet <- quantity_sheet(states, state_labels, "State")
states_sheet <- states_sheet[order(states_sheet[["CHE threshold, %"]],
                                   match(states_sheet[["Insurance type"]], ins_levels),
                                   match(states_sheet[["Household multimorbidity"]], dis_levels),
                                   match(states_sheet[["State"]], state_order),
                                   method = "radix"), ]

ratios_sheet <- quantity_sheet(states, ratio_labels, "Quantity")
ratios_sheet <- ratios_sheet[order(ratios_sheet[["CHE threshold, %"]],
                                   ratios_sheet[["Quantity"]],
                                   match(ratios_sheet[["Insurance type"]], ins_levels),
                                   match(ratios_sheet[["Household multimorbidity"]], dis_levels),
                                   method = "radix"), ]

stopifnot(nrow(res) == 54, nrow(overlap) == 28, nrow(states) == 189,
          nrow(states_sheet) == 3 * 9 * length(state_labels),
          nrow(ratios_sheet) == 3 * 9 * length(ratio_labels))

out_xlsx <- file.path(tabs_dir, "sens_nonpos_income.xlsx")
write_xlsx(
  list(
    diagnostics = round_numeric(diag),
    exclusion_results = round_numeric(res[, setdiff(names(res), "pvalue"), drop = FALSE]),
    states_positive_income = states_sheet,
    ratios_positive_income = ratios_sheet,
    hardship_overlap = round_numeric(overlap)
  ),
  path = out_xlsx
)

cat("Workbook saved to:", out_xlsx, "\n")
cat(nrow(states_sheet), "state rows,", nrow(ratios_sheet), "CHE and ratio rows\n")


cat("End:", format(Sys.time()), "\n")
