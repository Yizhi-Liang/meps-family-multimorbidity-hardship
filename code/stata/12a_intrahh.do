/*
  12a_intrahh.do — intra-household OOP allocation (secondary analysis).
  Decomposes per-capita normalized OOP burden into members with vs without priority conditions
  components within mixed-composition families. svy, subpop(): glm on log1p outcome.
  In:  $data/clean/family_analytic.csv, $data/clean/person_oop.dta
  Out: $data/clean/intrahh_stata.csv
*/

clear all
set more off
set maxvar 10000
capture set processors 5

if "$proj" == "" {
    local _p "`c(pwd)'/code/stata/_paths.do"
    capture confirm file "`_p'"
    if _rc != 0 {
        local _penv : env MEPS_PROTECTION_PROJ
        if "`_penv'" != "" local _p "`_penv'/code/stata/_paths.do"
        else {
            di as error "ERROR: cannot locate _paths.do."
            di as error "       Set MEPS_PROTECTION_PROJ or cd to project root."
            exit 601
        }
    }
    do "`_p'"
}

log using "$code/logs/12a_intrahh.log", replace text

timer on 12
di "=== Step 12a: Intra-household OOP Allocation ==="
di "Start: $S_DATE $S_TIME"

* A. Build OOP sums for members with vs without priority conditions at the family level
di _n "--- A. Building patient/non-patient OOP sums from person_oop.dta ---"

use "$data/clean/person_oop.dta", clear

gen double oop_patient = TOTSLF if has_any_condition == 1
gen double oop_nonpat  = TOTSLF if has_any_condition == 0

collapse (sum) TOTSLF_patients = oop_patient   ///
               TOTSLF_nonpat   = oop_nonpat    ///
    , by(DUID FAMIDYR YEAR)

* `collapse (sum)` returns missing only when ALL inputs are missing; recode
* the partition columns to 0 so downstream arithmetic is well-defined.
* Section D's subpopulation is mixed families, so neither group is empty there.
replace TOTSLF_patients = 0 if missing(TOTSLF_patients)
replace TOTSLF_nonpat   = 0 if missing(TOTSLF_nonpat)

* Lowercase keys to match family_analytic.csv post import-delimited
rename DUID    duid
rename FAMIDYR famidyr
rename YEAR    year

tempfile oop_split
save `oop_split'

* B. Merge to family analytic file
di _n "--- B. Loading family_analytic.csv ---"

import delimited "$data/clean/family_analytic.csv", clear
di "N (families) = " _N

merge 1:1 duid famidyr year using `oop_split', keep(match master) nogen

* C. CPI deflation to 2024 base year.
*    CPI-Medical (BLS CUUR0000SAM) annual averages applied to OOP.
*    CPI-U (BLS CUUR0000SA0) annual averages applied to family income.
*    Annual averages (period M13) retrieved 2026-09-12 from the BLS Public
*    Data API v2 (api.bls.gov).
di _n "--- C. CPI deflation to base 2024 ---"

gen double cpi_med = .
replace cpi_med = 484.707 if year == 2018
replace cpi_med = 498.413 if year == 2019
replace cpi_med = 518.876 if year == 2020
replace cpi_med = 525.276 if year == 2021
replace cpi_med = 546.554 if year == 2022
replace cpi_med = 549.084 if year == 2023
replace cpi_med = 563.841 if year == 2024
local cpi_med_2024 = 563.841

gen double cpi_u = .
replace cpi_u = 251.107 if year == 2018
replace cpi_u = 255.657 if year == 2019
replace cpi_u = 258.811 if year == 2020
replace cpi_u = 270.970 if year == 2021
replace cpi_u = 292.655 if year == 2022
replace cpi_u = 304.702 if year == 2023
replace cpi_u = 313.689 if year == 2024
local cpi_u_2024 = 313.689

assert !missing(cpi_med) & !missing(cpi_u)

gen double deflator_med = `cpi_med_2024' / cpi_med
gen double deflator_cpu = `cpi_u_2024'   / cpi_u

gen double TOTSLF_patients_real = TOTSLF_patients * deflator_med
gen double TOTSLF_nonpat_real   = TOTSLF_nonpat   * deflator_med
gen double FAMINC_real          = fam_faminc      * deflator_cpu

di "Deflator sanity check (rows by year):"
tabstat deflator_med deflator_cpu, by(year) stat(mean)

* D. Estimation subpopulation: mixed-composition families with positive real income
di _n "--- D. Mixed-family subpopulation ---"

local N0 = _N
di "Pre-restriction N: `N0'"

gen byte is_mixed = (n_sick_members >= 1 & n_sick_members < fam_size) ///
                    if !missing(n_sick_members) & !missing(fam_size)

tab is_mixed, m

* Every household stays in memory and the estimation subpopulation is flagged,
* not dropped: svy, subpop() takes the variances from the full survey design,
* which dropping observations before svy does not.
count if is_mixed == 1
di "Mixed-composition households: " r(N) " (not mixed: " `N0' - r(N) ")"

* Burden undefined when real income <= 0, so those households are outside the
* subpopulation
gen byte insub = (is_mixed == 1) & !missing(FAMINC_real) & FAMINC_real > 0
count if insub == 1
di "Mixed households with FAMINC_real > 0 (estimation subpopulation): " r(N)

* E. Per-capita normalized OOP burden
di _n "--- E. Building patient_burden and nonpat_burden ---"

gen double n_patients     = n_sick_members
gen double n_non_patients = fam_size - n_sick_members

gen double patient_burden = (TOTSLF_patients_real / n_patients) ///
                          / (FAMINC_real / fam_size)
gen double nonpat_burden  = (TOTSLF_nonpat_real / n_non_patients) ///
                          / (FAMINC_real / fam_size)

* log1p stabilizes the right-tailed share distribution
gen double log1p_patient = ln(1 + patient_burden)
gen double log1p_nonpat  = ln(1 + nonpat_burden)

* The burden is undefined outside the subpopulation (e.g. no non-patient
* member, income <= 0); set the outcomes to 0 there so svy does not drop those
* households from the design. The value is immaterial: subpop() gives them
* zero weight in estimation.
replace log1p_patient = 0 if insub == 0
replace log1p_nonpat  = 0 if insub == 0
assert !missing(log1p_patient, log1p_nonpat) if insub == 1

di "Burden summary (unit-free shares), estimation subpopulation:"
summ patient_burden nonpat_burden if insub == 1, detail

* F. Re-encode factors and rebuild dummies (same spec as 07a)
capture drop ins_num
capture drop dis_num

label define ins_lbl 1 "No Insurance" 2 "Public Insurance" 3 "Private Insurance"
gen byte ins_num = .
replace  ins_num = 1 if fam_ins_type == "Uninsured"
replace  ins_num = 2 if fam_ins_type == "Public"
replace  ins_num = 3 if fam_ins_type == "Private"
label values ins_num ins_lbl

label define dis_lbl 1 "0" 2 "1-2" 3 "3+"
gen byte dis_num = .
replace  dis_num = 1 if fam_disease_cat == "0"
replace  dis_num = 2 if fam_disease_cat == "1-2"
replace  dis_num = 3 if fam_disease_cat == "3+"
label values dis_num dis_lbl

* dis_cat=="0" is empty in mixed families by construction; report the
* cross-tab within the estimation subpopulation
di _n "--- Mixed-family cross-tab (ins x dis), estimation subpopulation ---"
tab ins_num dis_num if insub == 1, m

cap qui ds reg*
if "`r(varlist)'" != "" drop `r(varlist)'
tab fam_region, gen(reg)

cap qui ds pov*
if "`r(varlist)'" != "" drop `r(varlist)'
tab fam_povcat, gen(pov)

cap qui ds yr*
if "`r(varlist)'" != "" drop `r(varlist)'
qui tab year
local n_yr = r(r)
tab year, gen(yr)

local yr_vars ""
forvalues y = 2/`n_yr' {
    local yr_vars "`yr_vars' yr`y'"
}

cap qui ds pan*
if "`r(varlist)'" != "" drop `r(varlist)'
qui tab key_person_panel
local n_pan = r(r)
tab key_person_panel, gen(pan)
local pan_vars ""
forvalues p = 2/`n_pan' {
    local pan_vars "`pan_vars' pan`p'"
}

local covars ///
    fam_size mean_age prop_male prop_young ///
    prop_race_hisp prop_race_black prop_race_asian prop_race_other ///
    prop_college prop_no_degree prop_employed ///
    is_multigenerational ///
    prop_married prop_us_born ///
    reg2 reg3 reg4 ///
    pov2 pov3 pov4 pov5 ///
    `yr_vars' ///
    `pan_vars'

svyset psu9624, strata(stra9624) weight(poolwt) singleunit(centered)

local ins_lab1 "No Insurance"
local ins_lab2 "Public Insurance"
local ins_lab3 "Private Insurance"
local dis_lab1 "0"
local dis_lab2 "1-2"
local dis_lab3 "3+"

* G. Postfile
tempname rh10
tempfile rfile10
postfile `rh10' ///
    str20 outcome ///
    str12 type ///
    str25 ins_type ///
    str12 dis_cat ///
    str55 comparison ///
    double estimate ///
    double se ///
    double ci_lower ///
    double ci_upper ///
    double pvalue ///
    using `rfile10'

* H. Estimation: parallel models for patient and non-patient burden
local burden_outs   "log1p_patient log1p_nonpat"
local burden_labels "patient nonpat"

local b_idx = 0
foreach y of local burden_outs {
    local ++b_idx
    local ylab : word `b_idx' of `burden_labels'

    di _n "=== Outcome: `ylab'_burden (log1p scale) ==="
    di "  Estimating svy, subpop(insub): glm ...  [$S_TIME]"

    svy, subpop(insub): glm `y' i.ins_num##i.dis_num `covars', family(gaussian) link(identity)
    estimates store m_`ylab'

    * Build the CI on the log(1 + burden) scale and back-transform its
    * endpoints (exp(x) - 1). A lower bound is negative whenever the log-scale
    * lower endpoint is below 0, so it can be slightly negative for small
    * cells; the workbook keeps it and 12b truncates it at 0 in the figure.
    di "  Computing pred-cell margins on log1p scale ...  [$S_TIME]"
    margins ins_num#dis_num, subpop(insub) post

    local df   = e(df_r)
    local crit = invttail(`df', 0.025)

    forvalues i = 1/3 {
        forvalues d = 1/3 {
            * The 0 level has no household inside the subpopulation, but it
            * exists outside it, so margins would return a spurious estimate
            * for it; post the empty cell without computing anything.
            if `d' == 1 {
                post `rh10' ("`ylab'_burden") ("pred") ("`ins_lab`i''") ///
                            ("`dis_lab`d''") ("empty cell") ///
                            (.) (.) (.) (.) (.)
                continue
            }
            capture {
                local xb    = _b[`i'.ins_num#`d'.dis_num]
                local se_xb = _se[`i'.ins_num#`d'.dis_num]
                local xb_lo = `xb' - `crit' * `se_xb'
                local xb_hi = `xb' + `crit' * `se_xb'
                local b     = exp(`xb') - 1
                local lo    = exp(`xb_lo') - 1
                local hi    = exp(`xb_hi') - 1
                local se_bt = exp(`xb') * `se_xb'

                post `rh10' ("`ylab'_burden") ("pred") ("`ins_lab`i''") ///
                            ("`dis_lab`d''") ("") ///
                            (`b') (`se_bt') (`lo') (`hi') (.)
            }
            if _rc != 0 {
                * Empty cell (expected for dis_cat=="0" in mixed families)
                post `rh10' ("`ylab'_burden") ("pred") ("`ins_lab`i''") ///
                            ("`dis_lab`d''") ("empty cell") ///
                            (.) (.) (.) (.) (.)
            }
        }
    }

    estimates restore m_`ylab'
    di "  Computing back-transformed margins for the insurance differences ...  [$S_TIME]"
    margins ins_num#dis_num, expression(exp(predict(xb)) - 1) subpop(insub) post

    local df   = e(df_r)
    local crit = invttail(`df', 0.025)

    foreach i in 2 3 {
        forvalues d = 1/3 {
            * No row for the 0 level (empty inside the subpopulation)
            if `d' == 1 continue
            capture lincom _b[`i'.ins_num#`d'.dis_num] ///
                         - _b[1.ins_num#`d'.dis_num]
            if _rc == 0 {
                post `rh10' ("`ylab'_burden") ("ins_diff") ("`ins_lab`i''") ///
                            ("`dis_lab`d''") ("`ins_lab`i'' vs No Insurance") ///
                            (r(estimate)) (r(se)) (r(lb)) (r(ub)) (r(p))
            }
        }
    }

    di "  Done with `ylab'_burden  [$S_TIME]"
}

* H2. Paired test, members with vs without priority conditions (within-family).
*     Cross-model lincom would ignore within-family correlation; instead we
*     regress diff_log1p on the same ins×dis interaction and read margins.
*     Estimate/CI are on the log1p-difference scale (see eMethods 6).
gen double diff_log1p = log1p_patient - log1p_nonpat
replace diff_log1p = 0 if insub == 0

di _n "=== Paired test: members with vs without priority conditions (diff_log1p) ==="
di "  Estimating svy, subpop(insub): glm on within-family log1p difference ... [$S_TIME]"

svy, subpop(insub): glm diff_log1p i.ins_num##i.dis_num `covars', family(gaussian) link(identity)
margins ins_num#dis_num, subpop(insub) post

local df   = e(df_r)
local crit = invttail(`df', 0.025)

forvalues i = 1/3 {
    forvalues d = 1/3 {
        * The 0 level is empty inside the subpopulation: post the empty cell
        if `d' == 1 {
            post `rh10' ("diff_burden") ("pt_vs_np") ("`ins_lab`i''") ///
                        ("`dis_lab`d''") ("empty cell") ///
                        (.) (.) (.) (.) (.)
            continue
        }
        capture {
            local b  = _b[`i'.ins_num#`d'.dis_num]
            local se = _se[`i'.ins_num#`d'.dis_num]
            local lo = `b' - `crit' * `se'
            local hi = `b' + `crit' * `se'
            local t  = `b' / `se'
            local p  = 2 * ttail(`df', abs(`t'))

            post `rh10' ("diff_burden") ("pt_vs_np") ("`ins_lab`i''") ///
                        ("`dis_lab`d''") ("with vs without priority conditions") ///
                        (`b') (`se') (`lo') (`hi') (`p')
        }
        if _rc != 0 {
            post `rh10' ("diff_burden") ("pt_vs_np") ("`ins_lab`i''") ///
                        ("`dis_lab`d''") ("empty cell") ///
                        (.) (.) (.) (.) (.)
        }
    }
}

di "  Done with paired test  [$S_TIME]"

postclose `rh10'

use `rfile10', clear

di _n "=== Intra-household allocation results ==="
di "Total rows: " _N
tab outcome type

export delimited using "$data/clean/intrahh_stata.csv", replace
di "Results saved to intrahh_stata.csv"

timer off 12
timer list 12
di _n "=== Step 12a complete ==="
di "End: $S_DATE $S_TIME"

log close
