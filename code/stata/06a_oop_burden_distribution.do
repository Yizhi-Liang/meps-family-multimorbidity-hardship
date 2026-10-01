/*
  06a_oop_burden_distribution.do -- household OOP burden-ratio shares above
  10%, 25% and 40% of income and the survival grid of the ratio, by insurance type
  and multimorbidity (Figure 1 source).
  In:  $data/clean/family_analytic.csv
  Out: $data/clean/oop_burden_dist_stata.csv,
       $data/clean/oop_burden_survival_stata.csv
*/

clear all
set more off
set maxvar 10000
capture set processors 5

if "$proj" == "" {
    capture confirm file "`c(pwd)'/code/00_master.do"
    if _rc == 0 {
        global proj "`c(pwd)'"
        global data "$proj/data"
        global code "$proj/code"
        if "$rscript" == "" {
            local rscript_env : env MEPS_RSCRIPT
            if "`rscript_env'" != "" global rscript "`rscript_env'"
            else global rscript "Rscript"
        }
    }
    else {
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
}

capture mkdir "$code/logs"
log using "$code/logs/06a_oop_burden_distribution.log", replace text

timer on 6
di "=== Step 6a: OOP Burden Distribution ==="
di "Start: $S_DATE $S_TIME"

* A. Load
import delimited "$data/clean/family_analytic.csv", clear
di "N = " _N
* Sample invariant: the analytic file must carry the final sample-flow count
* written by 04_merge_pool.do (sample_flow_counts.csv, final row).
preserve
    import delimited "$data/clean/sample_flow_counts.csv", clear
    qui sum families_remaining if step_order == 7
    local n_expected = r(mean)
restore
di "  Expected analytic N (sample_flow_counts.csv): `n_expected'"
assert _N == `n_expected'
assert !missing(fam_faminc)
assert !missing(fam_oop)
assert !missing(poolwt) & poolwt > 0

* B. Burden share (positive-income households only)
gen byte pos_income = fam_faminc > 0
gen double oop_share = fam_oop / fam_faminc if pos_income == 1
gen byte gt10 = oop_share > .10 if pos_income == 1
gen byte gt25 = oop_share > .25 if pos_income == 1
gen byte gt40 = oop_share > .40 if pos_income == 1

* Re-encode factors (same convention as 07a)
capture drop ins_num
capture drop dis_num

label define ins_lbl 1 "No Insurance" 2 "Public Insurance" 3 "Private Insurance", replace
gen byte ins_num = .
replace  ins_num = 1 if fam_ins_type == "Uninsured"
replace  ins_num = 2 if fam_ins_type == "Public"
replace  ins_num = 3 if fam_ins_type == "Private"
label values ins_num ins_lbl

label define dis_lbl 1 "0" 2 "1-2" 3 "3+", replace
gen byte dis_num = .
replace  dis_num = 1 if fam_disease_cat == "0"
replace  dis_num = 2 if fam_disease_cat == "1-2"
replace  dis_num = 3 if fam_disease_cat == "3+"
label values dis_num dis_lbl

di _n "--- Factor encoding ---"
tab ins_num dis_num, m

svyset psu9624, strata(stra9624) weight(poolwt) singleunit(centered)

local ins_lab0 "Overall"
local ins_lab1 "No Insurance"
local ins_lab2 "Public Insurance"
local ins_lab3 "Private Insurance"
local dis_lab1 "0"
local dis_lab2 "1-2"
local dis_lab3 "3+"

* C. Twelve cells: insurance i in {1,2,3,0=Overall last} x multimorbidity d in {1,2,3}
tempname dh
tempfile dfile
postfile `dh' ///
    str25 ins_type ///
    str12 dis_cat ///
    long n_all ///
    long n_pos ///
    double wtd_prop_nonpos ///
    double gt10 ///
    double gt10_se ///
    double gt10_lo ///
    double gt10_hi ///
    double gt25 ///
    double gt25_se ///
    double gt25_lo ///
    double gt25_hi ///
    double gt40 ///
    double gt40_se ///
    double gt40_lo ///
    double gt40_hi ///
    using `dfile'

foreach i of numlist 1 2 3 0 {
    forvalues d = 1/3 {
        capture drop _cell
        capture drop _cellpos
        if `i' == 0 {
            gen byte _cell = (dis_num == `d')
        }
        else {
            gen byte _cell = (ins_num == `i' & dis_num == `d')
        }
        gen byte _cellpos = (_cell == 1 & pos_income == 1)

        qui count if _cell == 1
        local n_all = r(N)
        qui count if _cellpos == 1
        local n_pos = r(N)

        qui sum poolwt if _cell == 1, meanonly
        local w_den = r(sum)
        qui sum poolwt if _cell == 1 & pos_income == 0, meanonly
        local w_nonpos = r(sum)
        local wtd_prop_nonpos = cond(`w_den' > 0, `w_nonpos' / `w_den', .)

        svy, subpop(_cellpos): mean gt10 gt25 gt40
        local df = e(df_r)
        local crit = invttail(`df', .025)

        local gt10_b  = _b[gt10]
        local gt10_se = _se[gt10]
        local gt10_lo = max(0, `gt10_b' - `crit' * `gt10_se')
        local gt10_hi = min(1, `gt10_b' + `crit' * `gt10_se')

        local gt25_b  = _b[gt25]
        local gt25_se = _se[gt25]
        local gt25_lo = max(0, `gt25_b' - `crit' * `gt25_se')
        local gt25_hi = min(1, `gt25_b' + `crit' * `gt25_se')

        local gt40_b  = _b[gt40]
        local gt40_se = _se[gt40]
        local gt40_lo = max(0, `gt40_b' - `crit' * `gt40_se')
        local gt40_hi = min(1, `gt40_b' + `crit' * `gt40_se')

        post `dh' ("`ins_lab`i''") ("`dis_lab`d''") ///
            (`n_all') (`n_pos') (`wtd_prop_nonpos') ///
            (`gt10_b') (`gt10_se') (`gt10_lo') (`gt10_hi') ///
            (`gt25_b') (`gt25_se') (`gt25_lo') (`gt25_hi') ///
            (`gt40_b') (`gt40_se') (`gt40_lo') (`gt40_hi')
    }
}

postclose `dh'

preserve
    use `dfile', clear
    assert _N == 12
    assert gt40 <= gt25 & gt25 <= gt10
    assert n_pos <= n_all
    di _n "--- OOP burden shares (12 cells) ---"
    list, noobs
    export delimited using "$data/clean/oop_burden_dist_stata.csv", replace
restore

* D. Survival grid: 12 cells x 19 thresholds
tempname sh
tempfile sfile
postfile `sh' ///
    str25 ins_type ///
    str12 dis_cat ///
    double threshold ///
    double share_above ///
    using `sfile'

local thresholds ".001 .0025 .005 .01 .02 .03 .05 .075 .10 .15 .20 .25 .30 .40 .50 .75 1 1.5 2"

foreach i of numlist 1 2 3 0 {
    forvalues d = 1/3 {
        capture drop _cell
        capture drop _cellpos
        if `i' == 0 {
            gen byte _cell = (dis_num == `d')
        }
        else {
            gen byte _cell = (ins_num == `i' & dis_num == `d')
        }
        gen byte _cellpos = (_cell == 1 & pos_income == 1)

        qui sum poolwt if _cellpos == 1, meanonly
        local w_pos = r(sum)

        foreach t of local thresholds {
            qui sum poolwt if _cellpos == 1 & oop_share > `t', meanonly
            local w_above = r(sum)
            local share_above = cond(`w_pos' > 0, `w_above' / `w_pos', .)
            post `sh' ("`ins_lab`i''") ("`dis_lab`d''") ///
                (`t') (`share_above')
        }
    }
}

postclose `sh'

preserve
    use `sfile', clear
    assert _N == 228
    bysort ins_type dis_cat (threshold): assert share_above <= share_above[_n-1] if _n > 1
    export delimited using "$data/clean/oop_burden_survival_stata.csv", replace
restore

timer off 6
timer list 6
di _n "=== Step 6a complete ==="
di "End: $S_DATE $S_TIME"

log close
