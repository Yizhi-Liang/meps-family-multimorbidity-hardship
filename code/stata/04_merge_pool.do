/*
  04_merge_pool.do — merge HC-036 variance file, build pooled weight, apply restrictions.
  In:  $data/clean/family_aggregate.dta, $rawdata/h36u24.dta
  Out: $data/clean/family_analytic.dta, $data/clean/sample_flow_counts.csv
*/

clear all
set more off
set maxvar 10000

if "$proj" == "" {
    local _p "`c(pwd)'/code/stata/_paths.do"
    capture confirm file "`_p'"
    if _rc != 0 {
        local _penv : env MEPS_PROTECTION_PROJ
        if "`_penv'" != "" local _p "`_penv'/code/stata/_paths.do"
        else {
            di as error "ERROR: cannot locate _paths.do."
            di as error "       Set MEPS_PROTECTION_PROJ or cd to repo root."
            exit 601
        }
    }
    do "`_p'"
}

log using "$code/logs/04_merge_pool.log", replace text

timer on 4
di "=== Step 4: Merge HC-036, Weights, Restrictions ==="
di "Start: $S_DATE $S_TIME"

* A. Prepare HC-036
use "$rawdata/h36u24.dta", clear
keep DUPERSID PANEL STRA9624 PSU9624
duplicates drop DUPERSID PANEL, force

rename DUPERSID key_person_id
rename PANEL    key_person_panel

tempfile hc036
save `hc036'

* B. Merge
use "$data/clean/family_aggregate.dta", clear
local N0 = _N
di "Starting families: `N0'"

merge m:1 key_person_id key_person_panel using `hc036'
tab _merge
keep if _merge == 3 | _merge == 1
drop _merge

* C. Pooled weight: FAMWTF / 7 (seven pooled years, 2018-2024)
gen double poolwt = FAMWTF / 7

* Counts behind the eFigure 1 exclusion box (no positive pooled weight)
qui count if missing(poolwt)
di "  Missing poolwt: " r(N)
qui count if poolwt <= 0 & !missing(poolwt)
di "  Non-positive poolwt: " r(N)

* D. Sample restrictions
di _n "=== Sample restrictions ==="
local N1 = _N
di "Pre-restriction: `N1'"

count if missing(fam_ins_type) | fam_ins_type == ""
local d1 = r(N)
drop  if missing(fam_ins_type) | fam_ins_type == ""
di "  Dropped missing ins_type: `d1'"

count if missing(che_40) & missing(fam_debt_any) & missing(fam_barrier_any)
local d2 = r(N)
drop  if missing(che_40) & missing(fam_debt_any) & missing(fam_barrier_any)
di "  Dropped all outcomes missing: `d2'"

count if missing(poolwt) | poolwt <= 0
local d3 = r(N)
drop  if missing(poolwt) | poolwt <= 0
di "  Dropped zero/missing weight: `d3'"

count if missing(STRA9624) | missing(PSU9624)
local d4 = r(N)
drop  if missing(STRA9624) | missing(PSU9624)
di "  Dropped missing STRA9624/PSU9624: `d4'"

* MEPS uses negative codes for missing
replace fam_region = . if fam_region < 0
count if missing(fam_region)
local d5 = r(N)
drop  if missing(fam_region)
di "  Dropped missing fam_region: `d5'"

di _n "Final analytic sample: " _N
di "Total dropped: " `N1' - _N

* E. Sample-flow counts (eFigure 1)
local N_final = _N

preserve
    clear
    set obs 7
    gen byte step_order = _n
    gen str36 step_label = ""
    gen long families_remaining = .
    gen long families_excluded = 0

    replace step_label = "Starting families"             if step_order == 1
    replace step_label = "Missing insurance type"        if step_order == 2
    replace step_label = "All outcomes missing"          if step_order == 3
    replace step_label = "Zero or missing pooled weight" if step_order == 4
    replace step_label = "Missing variance structure"    if step_order == 5
    replace step_label = "Missing geographic region"     if step_order == 6
    replace step_label = "Final analytic sample"         if step_order == 7

    replace families_excluded = `d1'     if step_order == 2
    replace families_excluded = `d2'     if step_order == 3
    replace families_excluded = `d3'     if step_order == 4
    replace families_excluded = `d4'     if step_order == 5
    replace families_excluded = `d5'     if step_order == 6

    replace families_remaining = `N0'                                 if step_order == 1
    replace families_remaining = `N0' - `d1'                          if step_order == 2
    replace families_remaining = `N0' - `d1' - `d2'                   if step_order == 3
    replace families_remaining = `N0' - `d1' - `d2' - `d3'            if step_order == 4
    replace families_remaining = `N0' - `d1' - `d2' - `d3' - `d4'     if step_order == 5
    replace families_remaining = `N0' - `d1' - `d2' - `d3' - `d4' - `d5' if step_order == 6
    replace families_remaining = `N_final'                            if step_order == 7

    export delimited using "$data/clean/sample_flow_counts.csv", replace
restore

compress
order DUID FAMIDYR YEAR key_person_id key_person_panel ///
      fam_ins_type fam_disease_cat fam_n_conditions fam_size ///
      STRA9624 PSU9624 poolwt

di _n "=== Analytic dataset ==="
tab YEAR, m
tab fam_disease_cat fam_ins_type, m

save "$data/clean/family_analytic.dta", replace

timer off 4
timer list 4
di "End: $S_DATE $S_TIME"

log close
