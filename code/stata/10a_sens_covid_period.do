/*
  10a_sens_covid_period.do — COVID-period sensitivity (eTable 4).
  Survey-weighted regression adjustment with period (2018-2019 vs 2020-2024) x insurance x
  multimorbidity: adjusted probabilities per period and per-group period differences.
  Outcomes: che_40 and fam_any_hardship.
  Covariates: 07a list MINUS survey-year and panel dummies — period is the estimand here;
  year dummies are collinear with it, and panel dummies are year-cohorts (Panel 29 exists
  only in 2024). Raw p-values only; Holm adjustment happens in 10b.
  In:  $data/clean/family_analytic.csv
  Out: $data/clean/sens_covid_period_stata.csv
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
            di as error "       Set MEPS_PROTECTION_PROJ or cd to repo root."
            exit 601
        }
    }
    do "`_p'"
}

log using "$code/logs/10a_sens_covid_period.log", replace text

timer on 10
di "=== Step 10a: COVID Period Sensitivity ==="
di "Start: $S_DATE $S_TIME"

* A. Load
import delimited "$data/clean/family_analytic.csv", clear
di "N = " _N

* B. Re-encode factors (same as 07a) + period split
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

gen byte period = .
replace  period = 1 if year <= 2019
replace  period = 2 if year >= 2020 & !missing(year)
label define per_lbl 1 "2018-2019" 2 "2020-2024"
label values period per_lbl

di _n "--- Period x year ---"
tab year period, m

* C. Covariate dummies (region, poverty; NO year/panel dummies — see header)
cap qui ds reg*
if "`r(varlist)'" != "" drop `r(varlist)'
tab fam_region, gen(reg)

cap qui ds pov*
if "`r(varlist)'" != "" drop `r(varlist)'
tab fam_povcat, gen(pov)

local covars ///
    fam_size mean_age prop_male prop_young ///
    prop_race_hisp prop_race_black prop_race_asian prop_race_other ///
    prop_college prop_no_degree prop_employed ///
    is_multigenerational ///
    prop_married prop_us_born ///
    reg2 reg3 reg4 ///
    pov2 pov3 pov4 pov5

di _n "--- Covariates ---"
di "`covars'"

svyset psu9624, strata(stra9624) weight(poolwt) singleunit(centered)

local ins_lab1 "No Insurance"
local ins_lab2 "Public Insurance"
local ins_lab3 "Private Insurance"
local dis_lab1 "0"
local dis_lab2 "1-2"
local dis_lab3 "3+"

local outcomes     "che_40 fam_any_hardship"
local outcome_labs `" "CHE (40%)" "Any Hardship" "'

* D. Period x insurance x multimorbidity
tempname rh1
postfile `rh1' ///
    str12 rowtype ///
    str25 outcome ///
    str10 period_s ///
    str25 ins_type ///
    str12 dis_cat ///
    double estimate ///
    double se ///
    double ci_lower ///
    double ci_upper ///
    double pvalue ///
    using "$data/clean/_covid_prepost.dta", replace

local o_idx = 0
foreach outcome of local outcomes {
    local ++o_idx
    local olab : word `o_idx' of `outcome_labs'

    di _n "=== `olab': period x ins x dis ===  [$S_TIME]"
    svy: logit `outcome' i.period##i.ins_num##i.dis_num `covars'

    di "  margins ...  [$S_TIME]"
    margins period#ins_num#dis_num, post
    local df = e(df_r)
    if `df' == . | `df' <= 0  local df = 100
    local crit = invttail(`df', 0.025)

    * Build CIs on the logit scale (delta method) so they stay in (0,1) at boundary cells
    forvalues p = 1/2 {
        local plab : label per_lbl `p'
        forvalues i = 1/3 {
            forvalues d = 1/3 {
                local b  = _b[`p'.period#`i'.ins_num#`d'.dis_num]
                local s  = _se[`p'.period#`i'.ins_num#`d'.dis_num]
                if (`b' > 0 & `b' < 1) {
                    local xb    = logit(`b')
                    local xb_se = `s' / (`b' * (1 - `b'))
                    local lo = invlogit(`xb' - `crit' * `xb_se')
                    local hi = invlogit(`xb' + `crit' * `xb_se')
                }
                else {
                    local lo = max(0, `b' - `crit' * `s')
                    local hi = min(1, `b' + `crit' * `s')
                }
                post `rh1' ("margin") ("`olab'") ("`plab'") ("`ins_lab`i''") ("`dis_lab`d''") ///
                    (`b') (`s') (`lo') (`hi') (.)
            }
        }
    }

    forvalues i = 1/3 {
        forvalues d = 1/3 {
            lincom _b[2.period#`i'.ins_num#`d'.dis_num] - _b[1.period#`i'.ins_num#`d'.dis_num]
            post `rh1' ("perioddiff") ("`olab'") ("post-pre") ("`ins_lab`i''") ("`dis_lab`d''") ///
                (r(estimate)) (r(se)) (r(lb)) (r(ub)) (r(p))
        }
    }

}
postclose `rh1'

preserve
    use "$data/clean/_covid_prepost.dta", clear
    export delimited using "$data/clean/sens_covid_period_stata.csv", replace
    di "Saved: sens_covid_period_stata.csv  (N=" _N ")"
restore
erase "$data/clean/_covid_prepost.dta"

timer off 10
timer list 10
di "End: $S_DATE $S_TIME"

log close
