/*
  05a_descriptive.do — export the analytic CSV consumed by every later step;
  then log the counts quoted in the supplement's methods sections that no
  workbook holds.
  In:  $data/clean/family_analytic.dta, $data/clean/person_variables.dta
  Out: $data/clean/family_analytic.csv
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

log using "$code/logs/05a_descriptive.log", replace text

timer on 5
di "=== Step 5a: Export analytic CSV and log supplement counts ==="
di "Start: $S_DATE $S_TIME"

use "$data/clean/family_analytic.dta", clear

* B. Composition proportions with no assessed member. Step 3 leaves prop_married and
*    prop_us_born missing when no member (no adult, for marital status) is assessed;
*    they are set to 0 here, as prop_employed is in Step 3. Section D logs all three
*    counts in the analytic sample.
foreach v in prop_male prop_young prop_race_hisp prop_race_white ///
             prop_race_black prop_race_asian prop_race_other     ///
             prop_college prop_no_degree prop_employed            ///
             prop_married prop_us_born {
    capture confirm variable `v'
    if !_rc {
        count if missing(`v')
        if r(N) > 0 {
            di "WARNING: `v' has " r(N) " missing values — setting to 0"
            replace `v' = 0 if missing(`v')
        }
    }
}

replace fam_povcat = . if fam_povcat < 0
assert !missing(fam_region)
assert !missing(fam_povcat)

di _n "--- Variable summary before export ---"
di "N = " _N
sum fam_size mean_age prop_male prop_young prop_married prop_us_born ///
    n_sick_members poolwt, sep(0)

compress
order DUID FAMIDYR YEAR key_person_id key_person_panel ///
      fam_ins_type fam_disease_cat                     ///
      fam_n_conditions fam_size                        ///
      STRA9624 PSU9624 poolwt

* CSV-only interface (R 5b and the later Stata steps read it via import delimited)
export delimited using "$data/clean/family_analytic.csv", replace
di "CSV saved: $data/clean/family_analytic.csv"

* D. Counts quoted in the supplement (log only; no file is written), computed
*    over the person records of the analytic households.
di _n "--- D. Counts quoted in the supplement ---"

use DUID FAMIDYR YEAR DUPERSID AGELAST ins_type n_conditions                 ///
    is_employed adult_marital_assess us_born_assess                         ///
    HIBPDX CHDDX ANGIDX MIDX OHRTDX STRKDX EMPHDX CHOLDX CANCERDX ARTHDX     ///
    DIABDX ASTHDX ADHDADDX JTPAIN CHBRON                                    ///
    using "$data/clean/person_variables.dta", clear
merge m:1 DUID FAMIDYR YEAR using "$data/clean/family_analytic.dta",        ///
    keep(match) nogen keepusing(fam_ins_type poolwt STRA9624 PSU9624)
di "Person records in analytic households: " _N

* D1. Households with no member assessed for a composition item (eMethods 1).
gen byte _emp_assess = !missing(is_employed)
bysort DUID FAMIDYR YEAR: egen byte _any_emp = max(_emp_assess)
bysort DUID FAMIDYR YEAR: egen byte _any_mar = max(adult_marital_assess)
bysort DUID FAMIDYR YEAR: egen byte _any_nat = max(us_born_assess)
bysort DUID FAMIDYR YEAR: gen byte _first = (_n == 1)
qui count if _first
di "Analytic households: " r(N)
qui count if _first & _any_emp == 0
di "Households with no member assessed for employment (proportion set to 0): " r(N)
qui count if _first & _any_mar == 0
di "Households with no adult assessed for marital status (proportion set to 0): " r(N)
qui count if _first & _any_nat == 0
di "Households with no member assessed for nativity (proportion set to 0): " r(N)

* D2. Priority-condition items recorded for members younger than 18 (eMethods 3).
*     An item MEPS asks only of adults is missing for every child.
gen byte _child = AGELAST < 18 if !missing(AGELAST)
foreach v in HIBPDX CHDDX ANGIDX MIDX OHRTDX STRKDX EMPHDX CHOLDX CANCERDX ///
             ARTHDX DIABDX ASTHDX ADHDADDX JTPAIN CHBRON {
    qui count if _child == 1 & !missing(`v')
    local n_c = r(N)
    qui count if _child == 0 & !missing(`v')
    local n_a = r(N)
    di "  `v': recorded for `n_c' records under 18 and `n_a' records 18 or older"
}
qui count if _child == 1
local n_child = r(N)
qui count if _child == 1 & n_conditions > 0 & !missing(n_conditions)
di "Person records under 18: `n_child'; with at least 1 priority condition: " r(N)

* D3. Members whose full-year coverage type differs from the key member's (eMethods 2).
gen byte _differs = (ins_type != fam_ins_type) if ins_type != ""
bysort DUID FAMIDYR YEAR: egen byte any_differs = max(_differs)
bysort DUID FAMIDYR YEAR: egen int _n_members = count(DUPERSID)
keep if _first
gen byte multi = (_n_members > 1)
qui count if missing(any_differs)
di "Households with no member coverage type recorded: " r(N)
qui count if any_differs == 1
di "Households with a member whose coverage type differs from the key member's (unweighted): " r(N) " of " _N
svyset PSU9624 [pw=poolwt], strata(STRA9624) singleunit(centered)
svy: mean any_differs
svy, subpop(multi): mean any_differs
encode fam_ins_type, gen(_ins_g)
svy: mean any_differs, over(_ins_g)

timer off 5
timer list 5
di "End: $S_DATE $S_TIME"

log close
