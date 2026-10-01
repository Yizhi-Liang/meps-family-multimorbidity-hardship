/*
  00_master.do — pipeline orchestrator. Steps run in the order in which the manuscript
  presents their display items; the README maps each display item to its output file.
    1-4  data build (load and harmonize, person variables, household aggregation, pooling)
    5    descriptive tables (Table 1, eTable 1)
    6    OOP burden-ratio distribution (Figure 1)
    7    main models (Figure 2, eFigures 2-3, eTable 5)
    8    four mutually exclusive hardship states (Figures 3-4, eTable 2)
    9    re-estimation among households with positive income (eTable 3)
    10   COVID-period sensitivity (eTable 4)
    11   within-household distribution of conditions (eFigure 4)
    12   within-household OOP allocation (eFigure 5)
    13   export of the display items to displays/
  See README for setup and run instructions.
*/

version 19   // pins the Stata interpreter version; base Stata only
clear all
set more off
set maxvar 10000

timer on 99

capture confirm file "`c(pwd)'/code/stata/_paths.do"
if _rc == 0 {
    do "`c(pwd)'/code/stata/_paths.do"
}
else {
    local proj_env : env MEPS_PROTECTION_PROJ
    if "`proj_env'" != "" {
        do "`proj_env'/code/stata/_paths.do"
    }
    else {
        di as error "ERROR: cannot locate code/stata/_paths.do."
        di as error "       Set MEPS_PROTECTION_PROJ or cd to project root."
        exit 601
    }
}

capture mkdir "$data"
capture mkdir "$data/clean"
capture mkdir "$proj/output"
capture mkdir "$proj/output/tabs"
capture mkdir "$proj/output/figs"
capture mkdir "$code/logs"

capture confirm file "$rscript"
if _rc != 0 {
capture noisily shell bash -lc "command -v \"$rscript\" >/dev/null 2>&1"
    if _rc != 0 {
        di as error "ERROR: Rscript not found: $rscript"
        exit 601
    }
}

capture log close _all
log using "$code/logs/00_master.log", replace text name(master)

di "============================================"
di "=== Master Pipeline                      ==="
di "=== Start: $S_DATE $S_TIME              ==="
di "============================================"

* Steps 1-4: data build
di _n ">>> Step 1: Load and Harmonize"
capture noisily do "$code/stata/01_load_harmonize.do"
if _rc != 0 {
    local rc = _rc
    di as error "ERROR: Step 1: Load and Harmonize failed (rc=`rc')"
    exit `rc'
}

di _n ">>> Step 2: Person Variables"
capture noisily do "$code/stata/02_person_variables.do"
if _rc != 0 {
    local rc = _rc
    di as error "ERROR: Step 2: Person Variables failed (rc=`rc')"
    exit `rc'
}

di _n ">>> Step 3: Household Aggregation"
capture noisily do "$code/stata/03_family_aggregate.do"
if _rc != 0 {
    local rc = _rc
    di as error "ERROR: Step 3: Household Aggregation failed (rc=`rc')"
    exit `rc'
}

di _n ">>> Step 4: Merge and Pool"
capture noisily do "$code/stata/04_merge_pool.do"
if _rc != 0 {
    local rc = _rc
    di as error "ERROR: Step 4: Merge and Pool failed (rc=`rc')"
    exit `rc'
}

* Step 5: descriptive tables (Table 1, eTable 1)
di _n ">>> Step 5a: Export the Analytic File"
capture noisily do "$code/stata/05a_descriptive.do"
if _rc != 0 {
    local rc = _rc
    di as error "ERROR: Step 5a: Export the Analytic File failed (rc=`rc')"
    exit `rc'
}

di _n ">>> Step 5b: Descriptive Tables"
capture noisily shell bash -lc "stdbuf -oL -eL \"$rscript\" \"$code/R/05b_descriptive.R\""
if _rc != 0 {
    local rc = _rc
    di as error "ERROR: Step 5b: Descriptive Tables failed (rc=`rc')"
    di as error "       See $code/logs/05b_descriptive.log"
    exit `rc'
}
capture confirm file "$proj/output/tabs/characteristics_by_multimorbidity.xlsx"
if _rc != 0 {
    di as error "ERROR: Step 5b did not create $proj/output/tabs/characteristics_by_multimorbidity.xlsx"
    exit 603
}
capture confirm file "$proj/output/tabs/characteristics_by_insurance.xlsx"
if _rc != 0 {
    di as error "ERROR: Step 5b did not create $proj/output/tabs/characteristics_by_insurance.xlsx"
    exit 603
}
capture confirm file "$proj/output/tabs/nonpos_income_hardship.xlsx"
if _rc != 0 {
    di as error "ERROR: Step 5b did not create $proj/output/tabs/nonpos_income_hardship.xlsx"
    exit 603
}

* Step 6: OOP burden-ratio distribution (Figure 1)
di _n ">>> Step 6a: OOP Burden Distribution"
capture noisily do "$code/stata/06a_oop_burden_distribution.do"
if _rc != 0 {
    local rc = _rc
    di as error "ERROR: Step 6a: OOP Burden Distribution failed (rc=`rc')"
    exit `rc'
}

di _n ">>> Step 6b: OOP Burden Distribution Outputs"
capture noisily shell bash -lc "stdbuf -oL -eL \"$rscript\" \"$code/R/06b_oop_burden_distribution.R\""
if _rc != 0 {
    local rc = _rc
    di as error "ERROR: Step 6b: OOP Burden Distribution Outputs failed (rc=`rc')"
    di as error "       See $code/logs/06b_oop_burden_distribution.log"
    exit `rc'
}
capture confirm file "$proj/output/tabs/oop_burden_distribution.xlsx"
if _rc != 0 {
    di as error "ERROR: Step 6b did not create $proj/output/tabs/oop_burden_distribution.xlsx"
    exit 603
}
capture confirm file "$proj/output/figs/oop_burden_distribution.pdf"
if _rc != 0 {
    di as error "ERROR: Step 6b did not create $proj/output/figs/oop_burden_distribution.pdf"
    exit 603
}

* Step 7: main models (Figure 2, eFigures 2-3, eTable 5)
di _n ">>> Step 7a: Main Analysis"
capture noisily do "$code/stata/07a_main_analysis.do"
if _rc != 0 {
    local rc = _rc
    di as error "ERROR: Step 7a: Main Analysis failed (rc=`rc')"
    exit `rc'
}

di _n ">>> Step 7b: Main Analysis Outputs"
capture noisily shell bash -lc "stdbuf -oL -eL \"$rscript\" \"$code/R/07b_main_analysis.R\""
if _rc != 0 {
    local rc = _rc
    di as error "ERROR: Step 7b: Main Analysis Outputs failed (rc=`rc')"
    di as error "       See $code/logs/07b_main_analysis.log"
    exit `rc'
}
capture confirm file "$proj/output/tabs/main_results.xlsx"
if _rc != 0 {
    di as error "ERROR: Step 7b did not create $proj/output/tabs/main_results.xlsx"
    exit 603
}
capture confirm file "$proj/output/figs/main_predicted.pdf"
if _rc != 0 {
    di as error "ERROR: Step 7b did not create $proj/output/figs/main_predicted.pdf"
    exit 603
}
capture confirm file "$proj/output/figs/main_heatmap.pdf"
if _rc != 0 {
    di as error "ERROR: Step 7b did not create $proj/output/figs/main_heatmap.pdf"
    exit 603
}
capture confirm file "$proj/output/figs/main_forest.pdf"
if _rc != 0 {
    di as error "ERROR: Step 7b did not create $proj/output/figs/main_forest.pdf"
    exit 603
}
capture confirm file "$proj/output/tabs/multimorb_4cat.xlsx"
if _rc != 0 {
    di as error "ERROR: Step 7b did not create $proj/output/tabs/multimorb_4cat.xlsx"
    exit 603
}

* Step 8: four mutually exclusive hardship states (Figures 3-4, eTable 2)
di _n ">>> Step 8a: Four-state Hardship Classification"
capture noisily do "$code/stata/08a_hardship_states.do"
if _rc != 0 {
    local rc = _rc
    di as error "ERROR: Step 8a: Four-state Hardship Classification failed (rc=`rc')"
    exit `rc'
}

di _n ">>> Step 8b: Four-state Hardship Outputs"
capture noisily shell bash -lc "stdbuf -oL -eL \"$rscript\" \"$code/R/08b_hardship_states.R\""
if _rc != 0 {
    local rc = _rc
    di as error "ERROR: Step 8b: Four-state Hardship Outputs failed (rc=`rc')"
    di as error "       See $code/logs/08b_hardship_states.log"
    exit `rc'
}
capture confirm file "$proj/output/figs/hardship_states.pdf"
if _rc != 0 {
    di as error "ERROR: Step 8b did not create $proj/output/figs/hardship_states.pdf"
    exit 603
}
capture confirm file "$proj/output/figs/che_conditional.pdf"
if _rc != 0 {
    di as error "ERROR: Step 8b did not create $proj/output/figs/che_conditional.pdf"
    exit 603
}
capture confirm file "$proj/output/tabs/hardship_states.xlsx"
if _rc != 0 {
    di as error "ERROR: Step 8b did not create $proj/output/tabs/hardship_states.xlsx"
    exit 603
}

* Step 9: re-estimation among households with positive income (eTable 3)
di _n ">>> Step 9a: Positive-income Re-estimation"
capture noisily do "$code/stata/09a_sens_nonpos_income.do"
if _rc != 0 {
    local rc = _rc
    di as error "ERROR: Step 9a: Positive-income Re-estimation failed (rc=`rc')"
    exit `rc'
}

di _n ">>> Step 9b: Positive-income Re-estimation Outputs"
capture noisily shell bash -lc "stdbuf -oL -eL \"$rscript\" \"$code/R/09b_sens_nonpos_income.R\""
if _rc != 0 {
    local rc = _rc
    di as error "ERROR: Step 9b: Positive-income Re-estimation Outputs failed (rc=`rc')"
    di as error "       See $code/logs/09b_sens_nonpos_income.log"
    exit `rc'
}
capture confirm file "$proj/output/tabs/sens_nonpos_income.xlsx"
if _rc != 0 {
    di as error "ERROR: Step 9b did not create $proj/output/tabs/sens_nonpos_income.xlsx"
    exit 603
}

* Step 10: COVID-period sensitivity (eTable 4)
di _n ">>> Step 10a: COVID-period Sensitivity"
capture noisily do "$code/stata/10a_sens_covid_period.do"
if _rc != 0 {
    local rc = _rc
    di as error "ERROR: Step 10a: COVID-period Sensitivity failed (rc=`rc')"
    exit `rc'
}

di _n ">>> Step 10b: COVID-period Sensitivity Outputs"
capture noisily shell bash -lc "stdbuf -oL -eL \"$rscript\" \"$code/R/10b_sens_covid_period.R\""
if _rc != 0 {
    local rc = _rc
    di as error "ERROR: Step 10b: COVID-period Sensitivity Outputs failed (rc=`rc')"
    di as error "       See $code/logs/10b_sens_covid_period.log"
    exit `rc'
}
capture confirm file "$proj/output/tabs/sens_covid_period.xlsx"
if _rc != 0 {
    di as error "ERROR: Step 10b did not create $proj/output/tabs/sens_covid_period.xlsx"
    exit 603
}

* Step 11: within-household distribution of conditions (eFigure 4)
di _n ">>> Step 11a: Within-household Distribution of Conditions"
capture noisily do "$code/stata/11a_decomp_configuration.do"
if _rc != 0 {
    local rc = _rc
    di as error "ERROR: Step 11a: Within-household Distribution of Conditions failed (rc=`rc')"
    exit `rc'
}

di _n ">>> Step 11b: Within-household Distribution Outputs"
capture noisily shell bash -lc "stdbuf -oL -eL \"$rscript\" \"$code/R/11b_decomp_configuration.R\""
if _rc != 0 {
    local rc = _rc
    di as error "ERROR: Step 11b: Within-household Distribution Outputs failed (rc=`rc')"
    di as error "       See $code/logs/11b_decomp_configuration.log"
    exit `rc'
}
capture confirm file "$proj/output/tabs/decomp_configuration.xlsx"
if _rc != 0 {
    di as error "ERROR: Step 11b did not create $proj/output/tabs/decomp_configuration.xlsx"
    exit 603
}
capture confirm file "$proj/output/figs/config_distribution.pdf"
if _rc != 0 {
    di as error "ERROR: Step 11b did not create $proj/output/figs/config_distribution.pdf"
    exit 603
}

* Step 12: within-household OOP allocation (eFigure 5)
di _n ">>> Step 12a: Within-household OOP Allocation"
capture noisily do "$code/stata/12a_intrahh.do"
if _rc != 0 {
    local rc = _rc
    di as error "ERROR: Step 12a: Within-household OOP Allocation failed (rc=`rc')"
    exit `rc'
}

di _n ">>> Step 12b: Within-household OOP Allocation Outputs"
capture noisily shell bash -lc "stdbuf -oL -eL \"$rscript\" \"$code/R/12b_intrahh.R\""
if _rc != 0 {
    local rc = _rc
    di as error "ERROR: Step 12b: Within-household OOP Allocation Outputs failed (rc=`rc')"
    di as error "       See $code/logs/12b_intrahh.log"
    exit `rc'
}
capture confirm file "$proj/output/tabs/intrahh.xlsx"
if _rc != 0 {
    di as error "ERROR: Step 12b did not create $proj/output/tabs/intrahh.xlsx"
    exit 603
}
capture confirm file "$proj/output/figs/intrahh.pdf"
if _rc != 0 {
    di as error "ERROR: Step 12b did not create $proj/output/figs/intrahh.pdf"
    exit 603
}

* Step 13: export the display items (Figures 1-4, eFigures 1-5, Table 1, eTables 1-5)
di _n ">>> Step 13b: Export Display Items"
capture noisily shell bash -lc "stdbuf -oL -eL \"$rscript\" \"$code/R/13b_export_displays.R\""
if _rc != 0 {
    local rc = _rc
    di as error "ERROR: Step 13b: Export Display Items failed (rc=`rc')"
    di as error "       See $code/logs/13b_export_displays.log"
    exit `rc'
}
capture confirm file "$proj/displays/Figure1.pdf"
if _rc != 0 {
    di as error "ERROR: Step 13b did not create $proj/displays/Figure1.pdf"
    exit 603
}
capture confirm file "$proj/displays/eTable5.xlsx"
if _rc != 0 {
    di as error "ERROR: Step 13b did not create $proj/displays/eTable5.xlsx"
    exit 603
}

timer off 99

di _n "============================================"
di "=== Pipeline Complete                   ==="
di "=== End: $S_DATE $S_TIME                ==="
di "============================================"
timer list 99

log close master
