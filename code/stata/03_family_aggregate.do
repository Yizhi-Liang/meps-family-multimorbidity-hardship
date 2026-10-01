/*
  03_family_aggregate.do — collapse persons to families, build family outcomes.
  In:  $data/clean/person_variables.dta
  Out: $data/clean/family_aggregate.dta, $data/clean/person_oop.dta (Step 12a input)
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
            di as error "       Set MEPS_PROTECTION_PROJ or cd to project root."
            exit 601
        }
    }
    do "`_p'"
}

log using "$code/logs/03_family_aggregate.log", replace text

timer on 3
di "=== Step 3: Family-Level Aggregation ==="
di "Start: $S_DATE $S_TIME"

use "$data/clean/person_variables.dta", clear

* A. Key person rule: highest n_conditions (when any member has a priority condition)
*    -> oldest -> reference person (PID==REFPRS) -> lowest DUPERSID.
gen byte is_ref = (PID == REFPRS) if !missing(REFPRS)
replace  is_ref = 0 if missing(is_ref)

bysort DUID FAMIDYR YEAR: egen max_cond = max(n_conditions)

* Negate sort keys so descending order comes out under ascending bysort
gen double _sk1 = -cond(max_cond > 0 & !missing(max_cond), n_conditions, 0)
gen double _sk2 = -AGELAST
gen byte   _sk3 = -is_ref

* deterministic tie-break for reproducible re-runs
sort  DUID FAMIDYR YEAR _sk1 _sk2 _sk3 DUPERSID
by    DUID FAMIDYR YEAR: gen byte is_key = (_n == 1)

drop _sk1 _sk2 _sk3

di "Key persons:"
tab is_key

* A2. FAMWTF is not constant within every household. The household weight is the
*     key member's FAMWTF (FAMWTF_key below, renamed back to FAMWTF at the end of this
*     step). Log the number of households in which FAMWTF differs across members;
*     log only, nothing is asserted or changed.
bysort DUID FAMIDYR YEAR: egen double _fwt_min = min(FAMWTF)
bysort DUID FAMIDYR YEAR: egen double _fwt_max = max(FAMWTF)
gen byte _fwt_diff = (_fwt_min != _fwt_max) if !missing(_fwt_min) & !missing(_fwt_max)
egen byte _fwt_tag = tag(DUID FAMIDYR YEAR)
qui count if _fwt_diff == 1 & _fwt_tag == 1
local n_diff_hh = r(N)
di _n "=== FAMWTF within-household variation (log only) ==="
di "  Households in which FAMWTF differs across members: `n_diff_hh'"
drop _fwt_min _fwt_max _fwt_diff _fwt_tag

* B. Stash key-person attributes
preserve
    keep if is_key == 1
    keep DUID FAMIDYR YEAR DUPERSID PANEL ///
         ins_type REGION POVCAT FAMINC ///
         FAMWTF

    rename DUPERSID key_person_id
    rename PANEL    key_person_panel
    rename ins_type fam_ins_type
    rename REGION   fam_region
    rename POVCAT   fam_povcat
    rename FAMINC   fam_faminc
    rename FAMWTF   FAMWTF_key

    tempfile keyinfo
    save `keyinfo'
restore

* C. Helpers and pre-collapse flags
gen byte _one = 1
gen byte is_sick = (n_conditions > 0) if !missing(n_conditions)

* C1. person_oop.dta — person-level OOP for Step 12a (intra-household OOP allocation).
*     Must be written pre-collapse since TOTSLF is a person attribute.
preserve
    gen byte has_any_condition = is_sick
    keep DUID FAMIDYR YEAR TOTSLF has_any_condition
    label variable has_any_condition "1 = person has >=1 priority chronic condition"
    label variable TOTSLF "Person-level OOP (nominal dollars, year-specific)"
    order DUID FAMIDYR YEAR has_any_condition TOTSLF
    save "$data/clean/person_oop.dta", replace
    di _n "=== person_oop.dta saved (Step 12a input) ==="
    di "N (person-year records) = " _N
restore

gen byte _emp_assess = !missing(is_employed)

bysort DUID FAMIDYR YEAR age_band: gen byte _fb = (_n == 1) if !missing(age_band)
bysort DUID FAMIDYR YEAR:          egen byte n_age_bands = total(_fb)
drop _fb

collapse ///
    (sum)   fam_n_conditions     = n_conditions       ///
            n_sick_members       = is_sick             ///
            fam_oop              = TOTSLF              ///
            sum_male             = is_male             ///
            sum_child            = is_child            ///
            sum_employed         = is_employed         ///
            sum_educ_coll        = educ_college_plus   ///
            sum_educ_nodeg       = educ_no_degree      ///
            sum_race_hisp        = race_hispanic       ///
            sum_race_white       = race_white          ///
            sum_race_black       = race_black          ///
            sum_race_asian       = race_asian          ///
            sum_race_other       = race_other          ///
            sum_emp_assess       = _emp_assess         ///
            sum_married          = is_married          ///
            sum_adult_marital_assess = adult_marital_assess ///
            sum_us_born          = is_us_born          ///
            sum_us_born_assess   = us_born_assess      ///
    (count) fam_size             = _one                ///
    (mean)  mean_age             = AGELAST             ///
    (max)   fam_barrier_any      = barrier_any         ///
            fam_debt_any         = debt_any            ///
            n_age_bands          = n_age_bands         ///
    , by(DUID FAMIDYR YEAR)

merge 1:1 DUID FAMIDYR YEAR using `keyinfo', assert(match) nogen

* E. Family-level derived variables

gen str3 fam_disease_cat = ""
replace  fam_disease_cat = "0"   if fam_n_conditions == 0
replace  fam_disease_cat = "1-2" if fam_n_conditions >= 1 & fam_n_conditions <= 2
replace  fam_disease_cat = "3+"  if fam_n_conditions >= 3 & !missing(fam_n_conditions)

* 4-level multimorbidity for the sensitivity analysis in 07a; string for CSV export.
gen str3 fam_disease_cat4 = ""
replace  fam_disease_cat4 = "0"   if fam_n_conditions == 0
replace  fam_disease_cat4 = "1"   if fam_n_conditions == 1
replace  fam_disease_cat4 = "2-3" if fam_n_conditions >= 2 & fam_n_conditions <= 3
replace  fam_disease_cat4 = "4+"  if fam_n_conditions >= 4 & !missing(fam_n_conditions)

gen double prop_male       = sum_male       / fam_size
gen double prop_young      = sum_child      / fam_size
gen double prop_race_hisp  = sum_race_hisp  / fam_size
gen double prop_race_white = sum_race_white / fam_size
gen double prop_race_black = sum_race_black / fam_size
gen double prop_race_asian = sum_race_asian / fam_size
gen double prop_race_other = sum_race_other / fam_size
gen double prop_college    = sum_educ_coll  / fam_size
gen double prop_no_degree  = sum_educ_nodeg / fam_size

gen double prop_employed = sum_employed / sum_emp_assess if sum_emp_assess > 0
replace    prop_employed = 0 if sum_emp_assess == 0

* Marital status and nativity composition.
gen double prop_married = sum_married / sum_adult_marital_assess if sum_adult_marital_assess > 0
replace    prop_married = . if sum_adult_marital_assess == 0

gen double prop_us_born = sum_us_born / sum_us_born_assess if sum_us_born_assess > 0
replace    prop_us_born = . if sum_us_born_assess == 0

gen byte is_multigenerational = (n_age_bands >= 2) if !missing(n_age_bands)

* CHE: 40% (primary), 25% / 10% (sensitivity); non-positive income with OOP>0 → 1.
gen byte che_40 = .
replace  che_40 = 0 if !missing(fam_faminc) & !missing(fam_oop)
replace  che_40 = 1 if fam_faminc >  0 & (fam_oop / fam_faminc) > 0.40 ///
                      & !missing(fam_faminc) & !missing(fam_oop)
replace  che_40 = 1 if fam_faminc <= 0 & !missing(fam_faminc) ///
                      & fam_oop > 0 & !missing(fam_oop)

gen byte che_25 = .
replace  che_25 = 0 if !missing(fam_faminc) & !missing(fam_oop)
replace  che_25 = 1 if fam_faminc >  0 & (fam_oop / fam_faminc) > 0.25 ///
                      & !missing(fam_faminc) & !missing(fam_oop)
replace  che_25 = 1 if fam_faminc <= 0 & !missing(fam_faminc) ///
                      & fam_oop > 0 & !missing(fam_oop)

gen byte che_10 = .
replace  che_10 = 0 if !missing(fam_faminc) & !missing(fam_oop)
replace  che_10 = 1 if fam_faminc >  0 & (fam_oop / fam_faminc) > 0.10 ///
                      & !missing(fam_faminc) & !missing(fam_oop)
replace  che_10 = 1 if fam_faminc <= 0 & !missing(fam_faminc) ///
                      & fam_oop > 0 & !missing(fam_oop)

gen byte fam_any_hardship = .
replace  fam_any_hardship = 1 if che_40 == 1 | fam_debt_any == 1 | fam_barrier_any == 1
replace  fam_any_hardship = 0 if che_40 == 0 & fam_debt_any == 0 & fam_barrier_any == 0

di _n "=== Any financial hardship composite ==="
qui count if missing(fam_any_hardship) & che_40 != 1 & fam_debt_any != 1 & fam_barrier_any != 1
di "  Partially observed non-positive cases coded missing: " r(N)
qui count if fam_any_hardship == 0 & (missing(che_40) | missing(fam_debt_any) | missing(fam_barrier_any))
di "  Composite coded 0 despite missing component(s): " r(N)

rename FAMWTF_key FAMWTF

drop sum_male sum_child sum_employed sum_educ_coll sum_educ_nodeg ///
     sum_race_hisp sum_race_white sum_race_black sum_race_asian  ///
     sum_race_other sum_emp_assess                               ///
     sum_married sum_adult_marital_assess sum_us_born sum_us_born_assess

compress
order DUID FAMIDYR YEAR key_person_id key_person_panel ///
      fam_ins_type fam_disease_cat fam_n_conditions fam_size

di _n "=== Family-level dataset ==="
di "N = " _N
tab fam_disease_cat fam_ins_type, m
di _n "--- 4-category multimorbidity ---"
tab fam_disease_cat4 fam_ins_type, m

save "$data/clean/family_aggregate.dta", replace

timer off 3
timer list 3
di "End: $S_DATE $S_TIME"

log close
