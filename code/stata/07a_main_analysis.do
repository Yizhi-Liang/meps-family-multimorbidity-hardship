/*
  07a_main_analysis.do — main interaction model + 4-category multimorbidity sensitivity.
  Survey-design RA via svy: logit + margins + lincom; predicted-prob CIs built on
  the logit scale (delta method) so they stay in (0, 1) for boundary cells.
  In:  $data/clean/family_analytic.csv
  Out: $data/clean/main_results.csv,
       $data/clean/sens_multimorb_4cat_stata.csv
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

log using "$code/logs/07a_main_analysis.log", replace text

timer on 7
di "=== Step 7a: Main Analysis ==="
di "Start: $S_DATE $S_TIME"

* A. Load
import delimited "$data/clean/family_analytic.csv", clear
di "N = " _N

* B. Re-encode factors and rebuild dummies (import delimited lowercases names)
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

* Drop any reg/pov/yr/pan variables that already exist before the tabulations rebuild them
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

* C. Setup
local outcomes     "che_40 fam_debt_any fam_barrier_any fam_any_hardship"
local outcome_labs `" "CHE 40%" "Medical Debt" "Cost Barrier" "Any Hardship" "'

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

* D. Postfile
tempname rh
tempfile rfile
postfile `rh' ///
    str10 type ///
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

* E. Estimation loop
local o_idx = 0
foreach outcome of local outcomes {
    local ++o_idx
    local olab : word `o_idx' of `outcome_labs'

    di _n "=== Outcome: `outcome' (`olab') ==="
    di "  Estimating svy: logit ...  [$S_TIME]"

    svy: logit `outcome' i.ins_num##i.dis_num `covars'

    di "  Computing margins ...  [$S_TIME]"
    margins ins_num#dis_num, post

    * margins,post stores _b/_se but not CI bounds; build CIs manually
    local df   = e(df_r)
    local crit = invttail(`df', 0.025)
    di "  Design df = `df', t-critical = `crit'"

    * Build CIs on the logit scale (delta method) so they stay in (0,1) at boundary cells
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

            * A predicted probability carries no P value (it would test p = 0).
            post `rh' ("pred") ("`outcome'") ("`ins_lab`i''") ("`dis_lab`d''") ///
                      ("") (`b') (`se') (`lo') (`hi') (.)
        }
    }

    * Insurance pairwise diffs vs No Insurance, within disease level
    di "  Computing pairwise diffs ...  [$S_TIME]"
    foreach i in 2 3 {
        forvalues d = 1/3 {
            lincom _b[`i'.ins_num#`d'.dis_num] - _b[1.ins_num#`d'.dis_num]

            post `rh' ("ins_diff") ("`outcome'") ("`ins_lab`i''") ("`dis_lab`d''") ///
                      ("`ins_lab`i'' vs No Insurance") ///
                      (r(estimate)) (r(se)) (r(lb)) (r(ub)) (r(p))
        }
    }

    * Morbidity pairwise diffs vs 0, within insurance type
    forvalues i = 1/3 {
        foreach d in 2 3 {
            lincom _b[`i'.ins_num#`d'.dis_num] - _b[`i'.ins_num#1.dis_num]

            post `rh' ("morb_diff") ("`outcome'") ("`ins_lab`i''") ("`dis_lab`d''") ///
                      ("`dis_lab`d'' vs 0") ///
                      (r(estimate)) (r(se)) (r(lb)) (r(ub)) (r(p))
        }
    }

    * Difference-in-slopes contrasts: 3 insurance pairs x 3 morbidity pairs
    di "  Computing interaction contrasts ...  [$S_TIME]"
    local ip_ctrl  "2 1 1"
    local ip_treat "3 3 2"
    local ip_lab   `" "Pub. Ins.->Priv. Ins." "No Ins.->Priv. Ins." "No Ins.->Pub. Ins." "'

    local mp_pre  "1 1 2"
    local mp_post "2 3 3"
    local mp_lab  `" "0 vs 1-2" "0 vs 3+" "1-2 vs 3+" "'

    local n_ip : word count `ip_ctrl'
    local n_mp : word count `mp_pre'

    forvalues ip = 1/`n_ip' {
        local ctrl : word `ip' of `ip_ctrl'
        local trt  : word `ip' of `ip_treat'
        local ilab : word `ip' of `ip_lab'

        forvalues mp = 1/`n_mp' {
            local pre  : word `mp' of `mp_pre'
            local post : word `mp' of `mp_post'
            local mlab : word `mp' of `mp_lab'

            lincom (_b[`trt'.ins_num#`post'.dis_num] - _b[`trt'.ins_num#`pre'.dis_num]) ///
                 - (_b[`ctrl'.ins_num#`post'.dis_num] - _b[`ctrl'.ins_num#`pre'.dis_num])

            post `rh' ("contrast") ("`outcome'") ("`ilab'") ("`mlab'") ///
                      ("`ilab' `mlab'") ///
                      (r(estimate)) (r(se)) (r(lb)) (r(ub)) (r(p))
        }
    }

    di "  Done with `outcome'  [$S_TIME]"
}

postclose `rh'

use `rfile', clear

di _n "=== Results summary ==="
di "Total rows: " _N
tab type

export delimited using "$data/clean/main_results.csv", replace
di "Results saved to main_results.csv"

* G. Sensitivity: 4-category multimorbidity (0 / 1 / 2-3 / 4+).
*    Also compares "2-3" with "1" within each insurance type (eTable 5).
di _n "=== G. Four-category multimorbidity sensitivity ==="
di "    Reloading family_analytic.csv ...  [$S_TIME]"

import delimited "$data/clean/family_analytic.csv", clear

* `capture drop a b c` is atomic; use one drop per variable
capture drop ins_num
capture drop dis_num
capture drop dis4_num
* `replace` on existing label avoids re-execution errors
label define ins_lbl 1 "No Insurance" 2 "Public Insurance" 3 "Private Insurance", replace
gen byte ins_num = .
replace  ins_num = 1 if fam_ins_type == "Uninsured"
replace  ins_num = 2 if fam_ins_type == "Public"
replace  ins_num = 3 if fam_ins_type == "Private"
label values ins_num ins_lbl

label define dis4_lbl 1 "0" 2 "1" 3 "2-3" 4 "4+", replace
gen byte dis4_num = .
replace  dis4_num = 1 if fam_disease_cat4 == "0"
replace  dis4_num = 2 if fam_disease_cat4 == "1"
replace  dis4_num = 3 if fam_disease_cat4 == "2-3"
replace  dis4_num = 4 if fam_disease_cat4 == "4+"
label values dis4_num dis4_lbl

di _n "--- 4-category encoding ---"
tab ins_num dis4_num, m

cap qui ds reg*
if "`r(varlist)'" != "" drop `r(varlist)'
cap qui ds pov*
if "`r(varlist)'" != "" drop `r(varlist)'
cap qui ds yr*
if "`r(varlist)'" != "" drop `r(varlist)'
cap qui ds pan*
if "`r(varlist)'" != "" drop `r(varlist)'
tab fam_region, gen(reg)
tab fam_povcat, gen(pov)
qui tab year
local n_yr = r(r)
tab year, gen(yr)

local yr_vars ""
forvalues y = 2/`n_yr' {
    local yr_vars "`yr_vars' yr`y'"
}
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

local dis4_lab1 "0"
local dis4_lab2 "1"
local dis4_lab3 "2-3"
local dis4_lab4 "4+"

tempname rh4
tempfile rfile4
postfile `rh4' ///
    str10 type ///
    str25 outcome ///
    str25 ins_type ///
    str12 dis_cat ///
    str55 comparison ///
    double estimate ///
    double se ///
    double ci_lower ///
    double ci_upper ///
    double pvalue ///
    using `rfile4'

local o_idx = 0
foreach outcome of local outcomes {
    local ++o_idx
    local olab : word `o_idx' of `outcome_labs'

    di _n "=== [4cat] Outcome: `outcome' (`olab') ==="
    di "  Estimating svy: logit with 4-category exposure ...  [$S_TIME]"

    svy: logit `outcome' i.ins_num##i.dis4_num `covars'

    margins ins_num#dis4_num, post

    local df   = e(df_r)
    local crit = invttail(`df', 0.025)

    forvalues i = 1/3 {
        forvalues d = 1/4 {
            local b  = _b[`i'.ins_num#`d'.dis4_num]
            local se = _se[`i'.ins_num#`d'.dis4_num]
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

            * A predicted probability carries no P value (it would test p = 0).
            post `rh4' ("pred") ("`outcome'") ("`ins_lab`i''") ("`dis4_lab`d''") ///
                       ("") (`b') (`se') (`lo') (`hi') (.)
        }
    }

    * Difference "2-3" (d=3) minus "1" (d=2) within each insurance type (eTable 5)
    forvalues i = 1/3 {
        lincom _b[`i'.ins_num#3.dis4_num] - _b[`i'.ins_num#2.dis4_num]

        post `rh4' ("eq_1v23") ("`outcome'") ("`ins_lab`i''") ("1 vs 2-3") ///
                   ("`ins_lab`i'': P(2-3) - P(1)") ///
                   (r(estimate)) (r(se)) (r(lb)) (r(ub)) (r(p))
    }

    forvalues i = 1/3 {
        foreach d in 2 3 4 {
            lincom _b[`i'.ins_num#`d'.dis4_num] - _b[`i'.ins_num#1.dis4_num]

            post `rh4' ("morb_diff") ("`outcome'") ("`ins_lab`i''") ("`dis4_lab`d''") ///
                       ("`dis4_lab`d'' vs 0") ///
                       (r(estimate)) (r(se)) (r(lb)) (r(ub)) (r(p))
        }
    }

    di "  Done with `outcome' (4cat)  [$S_TIME]"
}

postclose `rh4'

use `rfile4', clear
di _n "=== 4-category sensitivity summary ==="
di "Total rows: " _N
tab type

export delimited using "$data/clean/sens_multimorb_4cat_stata.csv", replace
di "4-category sensitivity saved to sens_multimorb_4cat_stata.csv"

timer off 7
timer list 7
di _n "=== Step 7a complete ==="
di "End: $S_DATE $S_TIME"

log close
