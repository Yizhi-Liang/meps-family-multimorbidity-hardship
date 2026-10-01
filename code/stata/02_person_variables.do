/*
  02_person_variables.do — build person-level analytic variables.
  In:  $data/clean/person_harmonized.dta
  Out: $data/clean/person_variables.dta
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

log using "$code/logs/02_person_variables.log", replace text

timer on 2
di "=== Step 2: Person-Level Variables ==="
di "Start: $S_DATE $S_TIME"

use "$data/clean/person_harmonized.dta", clear

* A. Priority conditions (1=Yes→1, 2=No→0, neg→.).
foreach v in HIBPDX CHDDX ANGIDX MIDX OHRTDX STRKDX EMPHDX ///
             CHOLDX CANCERDX ARTHDX DIABDX ASTHDX ADHDADDX {
    capture confirm variable `v'
    if !_rc {
        replace `v' = . if `v' < 0
        recode  `v' (2 = 0)
    }
}
replace ADHDADDX = . if AGELAST < 5 | AGELAST > 17

* B. Combine round variants (R31 + R53): 1 if either, . if both missing, else 0
foreach v in JTPAIN31 JTPAIN53 CHBRON31 CHBRON53 {
    capture confirm variable `v'
    if !_rc {
        replace `v' = . if `v' < 0
        recode  `v' (2 = 0)
    }
}

gen byte JTPAIN = .
replace  JTPAIN = 0 if !missing(JTPAIN31) | !missing(JTPAIN53)
replace  JTPAIN = 1 if  JTPAIN31 == 1      | JTPAIN53 == 1

gen byte CHBRON = .
replace  CHBRON = 0 if !missing(CHBRON31) | !missing(CHBRON53)
replace  CHBRON = 1 if  CHBRON31 == 1      | CHBRON53 == 1

* C. Condition count (HIBPDX counts).
*    COVID-19 items (collected from 2022 onward) are deliberately EXCLUDED so the
*    count is comparable and additive across all survey years; the tradeoff
*    (COVID illness not contributing to the burden measure) is acknowledged in
*    the supplement eMethods.
egen n_conditions = rowtotal( ///
    HIBPDX CHDDX ANGIDX MIDX OHRTDX STRKDX EMPHDX ///
    CHOLDX CANCERDX ARTHDX DIABDX ASTHDX ADHDADDX JTPAIN CHBRON ///
), missing

* D. Outcomes: barriers and debt
foreach v in DLAYCA42 DLAYDN42 DLAYPM42 AFRDCA42 AFRDDN42 AFRDPM42 {
    replace `v' = . if `v' < 0
    recode  `v' (2 = 0)
}
egen byte barrier_any = rowmax(DLAYCA42 DLAYDN42 DLAYPM42 ///
                               AFRDCA42 AFRDDN42 AFRDPM42)
egen byte _nm_bar = rownonmiss(DLAYCA42 DLAYDN42 DLAYPM42 ///
                               AFRDCA42 AFRDDN42 AFRDPM42)
replace barrier_any = . if _nm_bar == 0
drop _nm_bar

foreach v in PROBPY42 CRFMPY42 PYUNBL42 {
    replace `v' = . if `v' < 0
    recode  `v' (2 = 0)
}
egen byte debt_any = rowmax(PROBPY42 CRFMPY42 PYUNBL42)
egen byte _nm_dbt = rownonmiss(PROBPY42 CRFMPY42 PYUNBL42)
replace debt_any = . if _nm_dbt == 0
drop _nm_dbt

* E. Insurance type
replace INSCOV = . if INSCOV < 0
gen str10 ins_type = ""
replace  ins_type = "Private"   if INSCOV == 1
replace  ins_type = "Public"    if INSCOV == 2
replace  ins_type = "Uninsured" if INSCOV == 3

* F. Demographics
gen byte is_male    = (SEX == 1)      if !missing(SEX)
gen byte is_child   = (AGELAST < 18)  if !missing(AGELAST)

gen byte age_band = .
replace  age_band = 1 if AGELAST < 18
replace  age_band = 2 if AGELAST >= 18 & AGELAST < 65
replace  age_band = 3 if AGELAST >= 65 & !missing(AGELAST)

gen byte race_hispanic = (RACETHX == 1) if !missing(RACETHX)
gen byte race_white    = (RACETHX == 2) if !missing(RACETHX)
gen byte race_black    = (RACETHX == 3) if !missing(RACETHX)
gen byte race_asian    = (RACETHX == 4) if !missing(RACETHX)
gen byte race_other    = (RACETHX == 5) if !missing(RACETHX)

replace HIDEG = . if HIDEG < 0
gen byte educ_college_plus = inlist(HIDEG, 4, 5, 6)  if !missing(HIDEG)
gen byte educ_no_degree    = (HIDEG == 1)             if !missing(HIDEG)

* F1. Marital composition (is_married = 1 if married: MARRY*X 1 = married, 7 = married in round)
*     Use round 53 if available; fall back to 42 then 31. Adults only: members under 18 are set
*     to missing so that the household numerator counts the same adults as the denominator.
gen byte is_married = .
foreach r in 53 42 31 {
    capture confirm variable MARRY`r'X
    if !_rc {
        replace MARRY`r'X = . if MARRY`r'X < 0
        replace is_married = inlist(MARRY`r'X, 1, 7) if !missing(MARRY`r'X) & missing(is_married)
    }
}
gen byte adult_marital_assess = (AGELAST >= 18 & !missing(is_married))
replace is_married = . if AGELAST < 18

* F2. Nativity (is_us_born = 1 if BORNUSA == 1)
capture confirm variable BORNUSA
if !_rc {
    replace BORNUSA = . if BORNUSA < 0
    gen byte is_us_born = (BORNUSA == 1) if !missing(BORNUSA)
}
else {
    gen byte is_us_born = .
}
gen byte us_born_assess = !missing(is_us_born)

* Note: MSA-level geography is suppressed in the MEPS public-use files for
* re-identification risk. Urbanicity is therefore not adjusted for here.
* Region (4 levels) is retained as the geographic covariate.

* G. Employment
foreach v in EMPST31 EMPST42 EMPST53 {
    capture confirm variable `v'
    if !_rc  replace `v' = . if `v' < 0
}
gen byte is_employed = .
replace  is_employed = 0 if !missing(EMPST31) | !missing(EMPST42) | !missing(EMPST53)
replace  is_employed = 1 if inlist(EMPST31,1,2,3) | inlist(EMPST42,1,2,3) | inlist(EMPST53,1,2,3)

* H. OOP
replace TOTSLF = . if TOTSLF < 0

compress
order DUPERSID DUID PID PANEL FAMIDYR YEAR AGELAST SEX RACETHX

di _n "=== Person variables ==="
di "N = " _N
sum n_conditions, detail
tab ins_type, m

save "$data/clean/person_variables.dta", replace

timer off 2
timer list 2
di "End: $S_DATE $S_TIME"

log close
