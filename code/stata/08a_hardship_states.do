/*
  08a_hardship_states.do — four mutually exclusive hardship states.
  The three hardship indicators are recoded into one partition of the population:
    1 = CHE and (debt or barriers)   2 = CHE only
    3 = debt or barriers, no CHE     4 = no hardship
  Estimated jointly by svy: mlogit, so nothing conditions on an outcome. Two ratios
  follow from the same fit: hardship additional to CHE, P(debt or barriers | CHE), and
  the CHE rule's detection rate, P(CHE | debt or barriers).
  Same RA specification, covariate list and survey design as 07a.
  Repeated at the 40%, 25% and 10% CHE thresholds.
  In:  $data/clean/family_analytic.csv
  Out: $data/clean/hardship_states.csv
  Runtime note: 3 survey margins calls (one per CHE threshold) over 75k observations.
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

log using "$code/logs/08a_hardship_states.log", replace text

timer on 8
di "=== Step 8a: Four-state Hardship Decomposition ==="
di "Start: $S_DATE $S_TIME"

* A. Load
import delimited "$data/clean/family_analytic.csv", clear
di "N = " _N

* B. Re-encode factors and rebuild dummies (same as 07a so covariates align)
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
di "Panel dummies (N=`n_pan'): `pan_vars'"

* C. Build the states. Debt or barriers is 1 if either item is 1, 0 if both
*    items are 0, and missing otherwise.
gen byte _hardship_any = .
replace  _hardship_any = 0 if fam_debt_any == 0 & fam_barrier_any == 0
replace  _hardship_any = 1 if fam_debt_any == 1 | fam_barrier_any == 1

label define state_lbl 1 "CHE and other hardship" 2 "CHE only" ///
                       3 "Debt or barriers, no CHE" 4 "No hardship"

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

* D. Setup — covariate list identical to 07a
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

* E. Postfile
tempname rh
tempfile rfile
postfile `rh' ///
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
    using `rfile'

* F. Estimation loop over thresholds
*
* One margins call per threshold returns all 36 cell x state probabilities with
* their joint VCE, so every quantity below is a delta-method transform of that
* one posted vector. The two ratios are population shares -- a ratio of weighted
* totals, E[p1]/(E[p1]+E[p2]) -- which is what nlcom on the posted margins gives.
* margins, expression() would instead average the household-level ratio, a
* different estimand, and would re-evaluate numerical derivatives over every
* observation for every parameter. Predicted probabilities and ratios post no P
* value (it would test p = 0); the difference rows carry theirs.

foreach t in 40 25 10 {

    di _n(2) "==================== CHE threshold `t'% ===================="
    di "  Estimating svy: mlogit ...  [$S_TIME]"

    svy: mlogit state_`t' i.ins_num##i.dis_num `covars', baseoutcome(4)

    local df   = e(df_r)
    local crit = invttail(`df', 0.025)
    di "  Design df = `df', t-critical = `crit'"

    di _n "  Computing margins for all four states ...  [$S_TIME]"
    margins ins_num#dis_num, predict(pr outcome(1)) predict(pr outcome(2)) ///
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

    * --- F1. The four state probabilities -------------------------------------
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
                post `rh' ("pred") (`t') ("`qname'") ("`ins_lab`i''") ("`dis_lab`d''") ///
                          ("") (`b') (`se') (`lo') (`hi') (.)
            }
        }

        foreach i in 2 3 {
            forvalues d = 1/3 {
                lincom _b[`n`k'`i'`d''] - _b[`n`k'1`d'']
                post `rh' ("ins_diff") (`t') ("`qname'") ("`ins_lab`i''") ("`dis_lab`d''") ///
                          ("`ins_lab`i'' vs No Insurance") ///
                          (r(estimate)) (r(se)) (r(lb)) (r(ub)) (r(p))
            }
        }

        forvalues i = 1/3 {
            foreach d in 2 3 {
                lincom _b[`n`k'`i'`d''] - _b[`n`k'`i'1']
                post `rh' ("morb_diff") (`t') ("`qname'") ("`ins_lab`i''") ("`dis_lab`d''") ///
                          ("`dis_lab`d'' vs 0") ///
                          (r(estimate)) (r(se)) (r(lb)) (r(ub)) (r(p))
            }
        }
    }

    * --- F2. CHE, the sum of the two CHE states -------------------------------
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
            post `rh' ("pred") (`t') ("che_sum") ("`ins_lab`i''") ("`dis_lab`d''") ///
                      ("") (`b') (`se') (`lo') (`hi') (.)
        }
    }

    * --- F3. The two ratios ---------------------------------------------------
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
                post `rh' ("pred") (`t') ("`qname'") ("`ins_lab`i''") ("`dis_lab`d''") ///
                          ("") (`b') (`se') (`lo') (`hi') (.)
            }
        }

        foreach i in 2 3 {
            forvalues d = 1/3 {
                nlcom (_b[`n1`i'`d''] / (_b[`n1`i'`d''] + _b[`n`o'`i'`d''])) ///
                    - (_b[`n11`d''] / (_b[`n11`d''] + _b[`n`o'1`d'']))
                matrix _nb = r(b)
                matrix _nV = r(V)
                local b  = _nb[1,1]
                local se = sqrt(_nV[1,1])
                local p  = 2 * ttail(`df', abs(`b' / `se'))
                local lo = `b' - `crit' * `se'
                local hi = `b' + `crit' * `se'
                post `rh' ("ins_diff") (`t') ("`qname'") ("`ins_lab`i''") ("`dis_lab`d''") ///
                          ("`ins_lab`i'' vs No Insurance") ///
                          (`b') (`se') (`lo') (`hi') (`p')
            }
        }

        forvalues i = 1/3 {
            foreach d in 2 3 {
                nlcom (_b[`n1`i'`d''] / (_b[`n1`i'`d''] + _b[`n`o'`i'`d''])) ///
                    - (_b[`n1`i'1'] / (_b[`n1`i'1'] + _b[`n`o'`i'1']))
                matrix _nb = r(b)
                matrix _nV = r(V)
                local b  = _nb[1,1]
                local se = sqrt(_nV[1,1])
                local p  = 2 * ttail(`df', abs(`b' / `se'))
                local lo = `b' - `crit' * `se'
                local hi = `b' + `crit' * `se'
                post `rh' ("morb_diff") (`t') ("`qname'") ("`ins_lab`i''") ("`dis_lab`d''") ///
                          ("`dis_lab`d'' vs 0") ///
                          (`b') (`se') (`lo') (`hi') (`p')
            }
        }
    }

    di _n "  Done with threshold `t'%  [$S_TIME]"
}

postclose `rh'

use `rfile', clear

di _n "=== Results summary ==="
di "Total rows: " _N
tab type
tab quantity threshold

* The four states and the two ratios each carry 9 predictions, 6 insurance
* differences and 6 multimorbidity differences at each of the three thresholds;
* CHE, the sum of the two CHE states, carries the 9 predictions only.
qui count if type == "pred"
if r(N) != 7 * 3 * 9 {
    di as error "ERROR: expected " 7*3*9 " prediction rows, found " r(N)
    exit 459
}
qui count if type == "ins_diff"
if r(N) != 6 * 3 * 6 {
    di as error "ERROR: expected " 6*3*6 " insurance-difference rows, found " r(N)
    exit 459
}
qui count if type == "morb_diff"
if r(N) != 6 * 3 * 6 {
    di as error "ERROR: expected " 6*3*6 " multimorbidity-difference rows, found " r(N)
    exit 459
}

export delimited using "$data/clean/hardship_states.csv", replace
di "Results saved to hardship_states.csv"

timer off 8
timer list 8
di _n "=== Step 8a complete: $S_DATE $S_TIME ==="

log close
