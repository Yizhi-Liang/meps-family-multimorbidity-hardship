###############################################################################
# 10b_sens_covid_period.R — format the COVID-period sensitivity (eTable 4).
# Holm adjustment of the per-group period differences within outcome.
# In:  $data/clean/sens_covid_period_stata.csv
# Out: $proj/output/tabs/sens_covid_period.xlsx
###############################################################################

proj_env <- Sys.getenv("MEPS_PROTECTION_PROJ", unset = "")
proj <- if (nzchar(proj_env)) proj_env else getwd()
if (!file.exists(file.path(proj, "code", "00_master.do"))) {
  stop("Cannot locate project root. Set MEPS_PROTECTION_PROJ or cd to repo root.")
}

log_file <- file.path(proj, "code", "logs", "10b_sens_covid_period.log")
log_con  <- file(log_file, open = "wt")
sink(log_con, type = "output", split = TRUE)
sink(log_con, type = "message", append = TRUE)

cat("=== Step 10b: COVID Period Sensitivity Output ===\n")
cat("Start:", format(Sys.time()), "\n\n")

library(tidyverse)
library(writexl)

# A. Load
prepost <- read.csv(file.path(proj, "data", "clean", "sens_covid_period_stata.csv"),
                    stringsAsFactors = FALSE)
cat("Loaded rows:", nrow(prepost), "\n")

# B. Holm within outcome
period_diffs <- prepost %>%
  filter(rowtype == "perioddiff") %>%
  group_by(outcome) %>%
  mutate(p_adj = p.adjust(pvalue, method = "holm")) %>%
  ungroup()

prepost_margins <- prepost %>%
  filter(rowtype == "margin") %>%
  select(-pvalue)

# C. Workbook
fmt_pct <- function(df) {
  df %>% mutate(across(c(estimate, se, ci_lower, ci_upper), ~ 100 * .x))
}

wb <- list(
  prepost_margins    = fmt_pct(prepost_margins),
  period_differences = fmt_pct(period_diffs)
)

tab_path <- file.path(proj, "output", "tabs", "sens_covid_period.xlsx")
write_xlsx(wb, tab_path)
cat("Saved:", tab_path, "\n")

cat("\nEnd:", format(Sys.time()), "\n")
sink(type = "message")
sink(type = "output")
close(log_con)
