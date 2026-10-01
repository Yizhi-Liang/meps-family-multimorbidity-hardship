/*
  09a_sens_nonpos_income.do -- non-positive-income CHE sensitivity
  (re-estimation of CHE and any hardship at the 10%, 25% and 40% thresholds
  among households with positive income) and hardship-domain overlap.
  The four hardship states of 08a are also re-estimated on the positive-income
  subpopulation, with CHE reported as the sum of the two CHE states.
  In:  $data/clean/family_analytic.csv
  Out: $data/clean/sens_nonpos_income_diag_stata.csv,
       $data/clean/sens_nonpos_income_results_stata.csv,
       $data/clean/sens_nonpos_income_states_stata.csv,
       $data/clean/hardship_overlap_stata.csv
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
log using "$code/logs/09a_sens_nonpos_income.log", replace text

timer on 9
di "=== Step 9a: Non-positive-income CHE Sensitivity ==="
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
gen byte pos_income = fam_faminc > 0

* Reference counts from the loaded analytic file; the posted diagnostics
* below must reproduce them exactly (internal-consistency checks).
qui count if pos_income == 0
local N_nonpos_all = r(N)
qui count if che_40 == 1
local N_che_all = r(N)
qui count if che_40 == 1 & pos_income == 0
local N_che_nonpos_all = r(N)
di "  Non-positive income: `N_nonpos_all'; CHE: `N_che_all'; CHE among non-positive income: `N_che_nonpos_all'"

* B. Re-encode factors and rebuild dummies (same convention as 07a)
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

di _n "--- Covariates ---"
di "`covars'"

svyset psu9624, strata(stra9624) weight(poolwt) singleunit(centered)

local ins_lab1 "No Insurance"
local ins_lab2 "Public Insurance"
local ins_lab3 "Private Insurance"
local dis_lab1 "0"
local dis_lab2 "1-2"
local dis_lab3 "3+"

* C. Diagnostics for the non-positive-income CHE rule
tempname dh
tempfile dfile
postfile `dh' ///
    str18 level ///
    str25 ins_type ///
    str12 dis_cat ///
    long n_households ///
    long n_nonpos ///
    long n_che ///
    long n_che_nonpos ///
    double wtd_prop_nonpos ///
    double wtd_share_che_from_rule ///
    using `dfile'

preserve
    gen double _w_nonpos = poolwt * (pos_income == 0)
    gen double _w_che = poolwt * (che_40 == 1)
    gen double _w_che_nonpos = poolwt * (che_40 == 1 & pos_income == 0)
    qui count
    local n_households = r(N)
    qui count if pos_income == 0
    local n_nonpos = r(N)
    qui count if che_40 == 1
    local n_che = r(N)
    qui count if che_40 == 1 & pos_income == 0
    local n_che_nonpos = r(N)
    qui sum poolwt, meanonly
    local w_den = r(sum)
    qui sum _w_nonpos, meanonly
    local w_nonpos = r(sum)
    qui sum _w_che, meanonly
    local w_che = r(sum)
    qui sum _w_che_nonpos, meanonly
    local w_che_nonpos = r(sum)
    local prop_nonpos = cond(`w_den' > 0, `w_nonpos' / `w_den', .)
    local share_rule = cond(`w_che' > 0, `w_che_nonpos' / `w_che', .)
    post `dh' ("overall") ("Overall") ("Overall") ///
        (`n_households') (`n_nonpos') (`n_che') (`n_che_nonpos') ///
        (`prop_nonpos') (`share_rule')
restore

postclose `dh'

preserve
    use `dfile', clear
    assert n_che == `N_che_all' if level == "overall"
    assert n_che_nonpos == `N_che_nonpos_all' if level == "overall"
    assert n_nonpos == `N_nonpos_all' if level == "overall"
    export delimited using "$data/clean/sens_nonpos_income_diag_stata.csv", replace
restore

* D. Build any hardship at the 10% and 25% CHE thresholds; missing rule mirrors
*    fam_any_hardship in 03.
gen byte fam_any_hardship_10 = .
replace  fam_any_hardship_10 = 1 if che_10 == 1 | fam_debt_any == 1 | fam_barrier_any == 1
replace  fam_any_hardship_10 = 0 if che_10 == 0 & fam_debt_any == 0 & fam_barrier_any == 0

gen byte fam_any_hardship_25 = .
replace  fam_any_hardship_25 = 1 if che_25 == 1 | fam_debt_any == 1 | fam_barrier_any == 1
replace  fam_any_hardship_25 = 0 if che_25 == 0 & fam_debt_any == 0 & fam_barrier_any == 0

* E. Exclusion re-estimation among positive-income households
tempname rh
tempfile rfile
postfile `rh' ///
    str12 type ///
    str25 outcome ///
    str25 ins_type ///
    str12 dis_cat ///
    str55 comparison ///
    double estimate ///
    double se ///
    double ci_lower ///
    double ci_upper ///
    double pvalue ///
    using `rfile'

local outcomes "che_40 che_25 che_10 fam_any_hardship fam_any_hardship_25 fam_any_hardship_10"

foreach outcome of local outcomes {
    di _n "=== Outcome: `outcome' positive-income subpopulation ==="
    capture noisily svy, subpop(pos_income): logit `outcome' i.ins_num##i.dis_num `covars'
    if _rc != 0 {
        local rc = _rc
        di as error "ERROR: model failed for `outcome' (rc=`rc')"
        exit `rc'
    }
    if e(converged) == 0 {
        di as error "ERROR: model did not converge for `outcome'"
        exit 430
    }

    capture noisily margins ins_num#dis_num, subpop(pos_income) post
    if _rc != 0 {
        local rc = _rc
        di as error "ERROR: margins failed for `outcome' (rc=`rc')"
        exit `rc'
    }

    local df   = e(df_r)
    local crit = invttail(`df', 0.025)

    forvalues i = 1/3 {
        forvalues d = 1/3 {
            local b  = _b[`i'.ins_num#`d'.dis_num]
            local se = _se[`i'.ins_num#`d'.dis_num]
            if (`b' > 0 & `b' < 1) {
                local xb    = logit(`b')
                local xb_se = `se' / (`b' * (1 - `b'))
                local lo = invlogit(`xb' - `crit' * `xb_se')
                local hi = invlogit(`xb' + `crit' * `xb_se')
            }
            else {
                local lo = max(0, `b' - `crit' * `se')
                local hi = min(1, `b' + `crit' * `se')
            }

            post `rh' ("pred_prob") ("`outcome'") ("`ins_lab`i''") ("`dis_lab`d''") ///
                ("") (`b') (`se') (`lo') (`hi') (.)
        }
    }
}

postclose `rh'

preserve
    use `rfile', clear
    assert _N == 54
    tab outcome type
    export delimited using "$data/clean/sens_nonpos_income_results_stata.csv", replace
    di "Positive-income sensitivity saved to sens_nonpos_income_results_stata.csv"
restore

* F. The four hardship states among positive-income households. States, the two
*    ratios and CHE as the sum of the two CHE states, at the three thresholds;
*    construction and estimator as in 08a, restricted by subpop(pos_income).
gen byte _hardship_any = .
replace  _hardship_any = 0 if fam_debt_any == 0 & fam_barrier_any == 0
replace  _hardship_any = 1 if fam_debt_any == 1 | fam_barrier_any == 1

label define state_lbl 1 "CHE and other hardship" 2 "CHE only" ///
                       3 "Debt or barriers, no CHE" 4 "No hardship", replace

foreach t in 40 25 10 {
    gen byte state_`t' = .
    replace  state_`t' = 1 if che_`t' == 1 & _hardship_any == 1
    replace  state_`t' = 2 if che_`t' == 1 & _hardship_any == 0
    replace  state_`t' = 3 if che_`t' == 0 & _hardship_any == 1
    replace  state_`t' = 4 if che_`t' == 0 & _hardship_any == 0
    label values state_`t' state_lbl

    di _n "--- state_`t' distribution ---"
    tab state_`t', m
    tab state_`t' ins_num, col
    tab state_`t' dis_num, col
}

drop _hardship_any

tempname sh
tempfile sfile
postfile `sh' ///
    str16 type ///
    int threshold ///
    str24 quantity ///
    str25 ins_type ///
    str12 dis_cat ///
    str55 comparison ///
    double estimate ///
    double se ///
    double ci_lower ///
    double ci_upper ///
    double pvalue ///
    using `sfile'

foreach t in 40 25 10 {

    di _n(2) "========= CHE threshold `t'%, positive-income households ========="
    di "  Estimating svy: mlogit ...  [$S_TIME]"

    svy, subpop(pos_income): mlogit state_`t' i.ins_num##i.dis_num `covars', baseoutcome(4)

    local df   = e(df_r)
    local crit = invttail(`df', 0.025)
    di "  Design df = `df', t-critical = `crit'"

    di _n "  Computing margins for all four states ...  [$S_TIME]"
    margins ins_num#dis_num, subpop(pos_income) ///
                             predict(pr outcome(1)) predict(pr outcome(2)) ///
                             predict(pr outcome(3)) predict(pr outcome(4)) post

    if colsof(e(b)) != 36 {
        di as error "ERROR: expected 36 margins columns, found " colsof(e(b))
        exit 459
    }

    * Column order is state-major, then insurance, then multimorbidity. Resolve
    * the names positionally rather than assuming Stata's base-level spelling.
    local cn : colnames e(b)
    forvalues k = 1/4 {
        forvalues i = 1/3 {
            forvalues d = 1/3 {
                local idx = (`k' - 1) * 9 + (`i' - 1) * 3 + `d'
                local n`k'`i'`d' : word `idx' of `cn'
            }
        }
    }

    * --- The four state probabilities -----------------------------------------
    forvalues k = 1/4 {

        local qname "state`k'"
        di _n "  `qname' at `t'% ...  [$S_TIME]"

        forvalues i = 1/3 {
            forvalues d = 1/3 {
                local b  = _b[`n`k'`i'`d'']
                local se = _se[`n`k'`i'`d'']
                if (`b' > 0 & `b' < 1) {
                    local xb    = logit(`b')
                    local xb_se = `se' / (`b' * (1 - `b'))
                    local lo = invlogit(`xb' - `crit' * `xb_se')
                    local hi = invlogit(`xb' + `crit' * `xb_se')
                }
                else {
                    local lo = max(0, `b' - `crit' * `se')
                    local hi = min(1, `b' + `crit' * `se')
                }
                post `sh' ("pred") (`t') ("`qname'") ("`ins_lab`i''") ("`dis_lab`d''") ///
                          ("") (`b') (`se') (`lo') (`hi') (.)
            }
        }
    }

    * --- CHE, the sum of the two CHE states -----------------------------------
    di _n "  che_sum at `t'% ...  [$S_TIME]"

    forvalues i = 1/3 {
        forvalues d = 1/3 {
            lincom _b[`n1`i'`d''] + _b[`n2`i'`d'']
            local b  = r(estimate)
            local se = r(se)
            if (`b' > 0 & `b' < 1) {
                local xb    = logit(`b')
                local xb_se = `se' / (`b' * (1 - `b'))
                local lo = invlogit(`xb' - `crit' * `xb_se')
                local hi = invlogit(`xb' + `crit' * `xb_se')
            }
            else {
                local lo = max(0, `b' - `crit' * `se')
                local hi = min(1, `b' + `crit' * `se')
            }
            post `sh' ("pred") (`t') ("che_sum") ("`ins_lab`i''") ("`dis_lab`d''") ///
                      ("") (`b') (`se') (`lo') (`hi') (.)
        }
    }

    * --- The two ratios -------------------------------------------------------
    * cond_hardship = P(debt or barriers | CHE)  = s1 / (s1 + s2)
    * detect_che    = P(CHE | debt or barriers)  = s1 / (s1 + s3)

    forvalues r = 1/2 {

        if `r' == 1 {
            local qname "cond_hardship"
            local o     2
        }
        else {
            local qname "detect_che"
            local o     3
        }

        di _n "  `qname' at `t'% ...  [$S_TIME]"

        forvalues i = 1/3 {
            forvalues d = 1/3 {
                nlcom _b[`n1`i'`d''] / (_b[`n1`i'`d''] + _b[`n`o'`i'`d''])
                matrix _nb = r(b)
                matrix _nV = r(V)
                local b  = _nb[1,1]
                local se = sqrt(_nV[1,1])
                if (`b' > 0 & `b' < 1) {
                    local xb    = logit(`b')
                    local xb_se = `se' / (`b' * (1 - `b'))
                    local lo = invlogit(`xb' - `crit' * `xb_se')
                    local hi = invlogit(`xb' + `crit' * `xb_se')
                }
                else {
                    local lo = max(0, `b' - `crit' * `se')
                    local hi = min(1, `b' + `crit' * `se')
                }
                post `sh' ("pred") (`t') ("`qname'") ("`ins_lab`i''") ("`dis_lab`d''") ///
                          ("") (`b') (`se') (`lo') (`hi') (.)
            }
        }
    }

    di _n "  Done with threshold `t'%  [$S_TIME]"
}

postclose `sh'

use `sfile', clear
di _n "=== Positive-income four-state results ==="
di "Total rows: " _N
tab quantity threshold
assert _N == 189
export delimited using "$data/clean/sens_nonpos_income_states_stata.csv", replace
di "Positive-income four-state results saved to sens_nonpos_income_states_stata.csv"

* G. Hardship-domain overlap among complete hardship domains
import delimited "$data/clean/family_analytic.csv", clear
capture drop ins_num
label define ins_lbl 1 "No Insurance" 2 "Public Insurance" 3 "Private Insurance", replace
gen byte ins_num = .
replace  ins_num = 1 if fam_ins_type == "Uninsured"
replace  ins_num = 2 if fam_ins_type == "Public"
replace  ins_num = 3 if fam_ins_type == "Private"
label values ins_num ins_lbl

local ins_lab1 "No Insurance"
local ins_lab2 "Public Insurance"
local ins_lab3 "Private Insurance"

gen byte complete_domains = !missing(che_40) & !missing(fam_debt_any) & !missing(fam_barrier_any)
qui count if complete_domains == 0
local n_overlap_dropped = r(N)
di "Hardship-domain overlap: complete-domain N dropped = `n_overlap_dropped'"

gen str20 combo = ""
replace combo = "CHE only"       if che_40 == 1 & fam_debt_any == 0 & fam_barrier_any == 0
replace combo = "Debt only"      if che_40 == 0 & fam_debt_any == 1 & fam_barrier_any == 0
replace combo = "Barrier only"   if che_40 == 0 & fam_debt_any == 0 & fam_barrier_any == 1
replace combo = "CHE + debt"     if che_40 == 1 & fam_debt_any == 1 & fam_barrier_any == 0
replace combo = "CHE + barrier"  if che_40 == 1 & fam_debt_any == 0 & fam_barrier_any == 1
replace combo = "Debt + barrier" if che_40 == 0 & fam_debt_any == 1 & fam_barrier_any == 1
replace combo = "All three"      if che_40 == 1 & fam_debt_any == 1 & fam_barrier_any == 1

* Households with any hardship but a missing domain cannot be classified by
* combination and leave the overlap sample at the next line.
qui count if complete_domains == 0 & fam_any_hardship == 1
di "Hardship-domain overlap: households with any hardship but a missing domain (excluded) = " r(N)
qui count if fam_any_hardship == 1
local n_anyh = r(N)
qui count if complete_domains == 1 & fam_any_hardship == 1
di "Hardship-domain overlap sample: " r(N) " of `n_anyh' households with any hardship (eMethods 1)"

keep if complete_domains == 1 & fam_any_hardship == 1

tempname oh
tempfile ofile
postfile `oh' ///
    str12 level ///
    str25 ins_type ///
    str20 combo ///
    long n_unweighted ///
    double wtd_share ///
    using `ofile'

local combo_levels `" "CHE only" "Debt only" "Barrier only" "CHE + debt" "CHE + barrier" "Debt + barrier" "All three" "'

forvalues g = 0/3 {
    preserve
        if `g' == 0 {
            local level_s "overall"
            local ins_s "Overall"
        }
        else {
            keep if ins_num == `g'
            local level_s "insurance"
            local ins_s "`ins_lab`g''"
        }

        qui sum poolwt, meanonly
        local w_den = r(sum)

        foreach c of local combo_levels {
            qui count if combo == "`c'"
            local n_c = r(N)
            qui sum poolwt if combo == "`c'", meanonly
            local w_c = r(sum)
            local share = cond(`w_den' > 0, `w_c' / `w_den', .)
            post `oh' ("`level_s'") ("`ins_s'") ("`c'") (`n_c') (`share')
        }
    restore
}

postclose `oh'

use `ofile', clear
* Keep the posted order (overall first, then No/Public/Private insurance;
* combinations in combo_levels order); bysort reorders rows unstably.
gen long _row = _n
bysort level ins_type: egen double _share_sum = total(wtd_share)
assert abs(_share_sum - 1) < 1e-6
drop _share_sum
sort _row
drop _row
assert _N == 28
export delimited using "$data/clean/hardship_overlap_stata.csv", replace
di "Hardship overlap saved to hardship_overlap_stata.csv"

timer off 9
timer list 9
di _n "=== Step 9a complete ==="
di "End: $S_DATE $S_TIME"

log close
