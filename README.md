# MEPS Household Multimorbidity & Financial Hardship — Analytic Code

This repository contains the full Stata + R analytic pipeline for the analysis
"Household Multimorbidity, Insurance Type, and Financial Hardship".

The repository carries no MEPS microdata; users obtain the public-use files
directly from AHRQ.

## Requirements

- **Stata 19 SE or MP** (survey estimation with the pooled MEPS variance
  design; the pipeline sets `maxvar` to 10,000, and the command below calls
  `stata-mp`, which must be on `PATH`)
- **R 4.6.1** (output formatting). `renv.lock` records R 4.6.1 and the exact
  version of every package, with the Posit Package Manager snapshot of
  2026-09-14 as the repository (current and archived versions), so
  `renv::restore()` reproduces the environment that produced the reported
  results.
- **bash and GNU coreutils** — each R step is launched through `bash -lc` and
  `stdbuf`, so both must be on the login-shell `PATH` (on macOS, install
  coreutils and put its `gnubin` directory on `PATH`)
- **MEPS public-use files** — see "Data Sources" below

## Data Sources

MEPS Household Component Full-Year Consolidated files (`h209`, `h216`, `h224`,
`h233`, `h243`, `h251`, `h256`) and the 1996-2024 pooled variance file
(`h36u24`), in Stata format, distributed by AHRQ at
`https://meps.ahrq.gov/mepsweb/data_stats/download_data_files.jsp`. Place the
eight `.dta` files in one directory.

## Running the Pipeline

```bash
export MEPS_PROTECTION_PROJ=/path/to/this/repo
export MEPS_RAWDATA=/path/to/the/meps/files
cd $MEPS_PROTECTION_PROJ
Rscript -e 'renv::restore()'   # once: installs the locked R packages
stata-mp -b do code/00_master.do
```

`MEPS_RSCRIPT` may be set to a specific `Rscript` binary; otherwise `Rscript`
on `$PATH` is used. The pipeline creates `data/clean/` (intermediate files),
`output/tabs/`, `output/figs/` and `code/logs/`, and writes the display
items to `displays/`. Batch Stata exits 0 even when a step fails, and it does
not see the exit status of an R step (a failed R step stops the pipeline only
when one of its output files is missing): a run is complete only if
`code/logs/00_master.log` contains a line beginning `=== Pipeline Complete`
and no `r(<number>);` line, and the log of every R step
(`code/logs/??b_*.log`) contains an `End:` line.

## Display Items

Steps run in the order in which the manuscript presents their display items.
`displays/` holds each item under its manuscript label; it is written by
Step 13 and is never edited by hand.

| Display item | `displays/` file | Step | Output (under `output/`) |
|---|---|---|---|
| Table 1 | `Table1.xlsx` | 5 | `tabs/characteristics_by_multimorbidity.xlsx` |
| Figure 1 | `Figure1.pdf` | 6 | `figs/oop_burden_distribution.pdf` |
| Figure 2 | `Figure2.pdf` | 7 | `figs/main_predicted.pdf` |
| Figure 3 | `Figure3.pdf` | 8 | `figs/hardship_states.pdf` |
| Figure 4 | `Figure4.pdf` | 8 | `figs/che_conditional.pdf` |
| eTable 1 | `eTable1.xlsx` | 5 | `tabs/characteristics_by_insurance.xlsx` |
| eTable 2 | `eTable2.xlsx` | 8 | `tabs/hardship_states.xlsx`, sheets `states`, `ratios`, `differences` |
| eTable 3 | `eTable3.xlsx` | 9 | `tabs/sens_nonpos_income.xlsx`, sheets `exclusion_results`, `states_positive_income`, `ratios_positive_income` |
| eTable 4 | `eTable4.xlsx` | 10 | `tabs/sens_covid_period.xlsx`, sheets `prepost_margins`, `period_differences` |
| eTable 5 | `eTable5.xlsx` | 7 | `tabs/multimorb_4cat.xlsx`, sheet `3-level vs 4-level comparison` |
| eTable 6 | `eTable6.xlsx` | 7 | `tabs/sens_cov_concordant.xlsx`, sheets `concordant vs all households`, `insurance differences`, `interaction contrasts`, `households` |
| eFigure 1 | `eFigure1.pdf` | — | `figures_source/sample_flow_diagram.pdf` (drawn by hand from the Step 4 sample-flow counts) |
| eFigure 2 | `eFigure2.pdf` | 7 | `figs/main_heatmap.pdf` |
| eFigure 3 | `eFigure3.pdf` | 7 | `figs/main_forest.pdf` |
| eFigure 4 | `eFigure4.pdf` | 11 | `figs/config_distribution.pdf` |
| eFigure 5 | `eFigure5.pdf` | 12 | `figs/intrahh.pdf` |

Estimates quoted in the text without a display item come from the step
workbooks under `output/tabs/` or from the step logs under `code/logs/`; the
counts and shares that no workbook holds are printed to the step logs: sample
restrictions and pooled weights by Step 4 (`04_merge_pool.log`),
household-composition counts by Step 5a (`05a_descriptive.log`), the weighted
shares by multimorbidity by Step 5b (`05b_descriptive.log`), unassigned
hardship states by Step 8a (`08a_hardship_states.log`), the hardship-overlap
sample by Step 9a (`09a_sens_nonpos_income.log`), and the mixed-composition
sample and burden distribution by Step 12a (`12a_intrahh.log`).

## Directory Layout

```
.
├── code/
│   ├── 00_master.do              # Pipeline orchestrator (Stata + R)
│   ├── stata/
│   │   ├── _paths.do             # Path resolver (environment variables only)
│   │   ├── 01_load_harmonize.do
│   │   ├── 02_person_variables.do
│   │   ├── 03_family_aggregate.do
│   │   ├── 04_merge_pool.do
│   │   ├── 05a_descriptive.do
│   │   ├── 06a_oop_burden_distribution.do
│   │   ├── 07a_main_analysis.do
│   │   ├── 08a_hardship_states.do
│   │   ├── 09a_sens_nonpos_income.do
│   │   ├── 10a_sens_covid_period.do
│   │   ├── 11a_decomp_configuration.do
│   │   └── 12a_intrahh.do
│   └── R/
│       ├── 05b_descriptive.R
│       ├── 06b_oop_burden_distribution.R
│       ├── 07b_main_analysis.R
│       ├── 08b_hardship_states.R
│       ├── 09b_sens_nonpos_income.R
│       ├── 10b_sens_covid_period.R
│       ├── 11b_decomp_configuration.R
│       ├── 12b_intrahh.R
│       └── 13b_export_displays.R
├── figures_source/               # eFigure 1 source and its PDF export
│   ├── README.md
│   ├── sample_flow_diagram.drawio
│   └── sample_flow_diagram.pdf
├── displays/                     # The display items, under their manuscript labels
│   ├── Table1.xlsx
│   ├── Figure1.pdf … Figure4.pdf
│   ├── eTable1.xlsx … eTable6.xlsx
│   └── eFigure1.pdf … eFigure5.pdf
├── .Rprofile                     # Activates renv
├── renv/                         # renv bootstrap (activate.R, settings.json)
├── renv.lock
└── LICENSE
```

## Naming Convention

- Steps 1 to 4 are Stata scripts without a suffix → they build the analytic
  file
- From Step 5 on, Stata scripts end in suffix `a` → estimation and CSV export
  (Step 5a exports the analytic file)
- R scripts end in suffix `b` → output formatting (Holm adjustment, workbooks,
  figures)
- Stata passes results to R as CSV only

## License

MIT. See `LICENSE`.

## Citation

Once the article is published, please cite it in the format of the publishing
journal; the reference will be added here.
