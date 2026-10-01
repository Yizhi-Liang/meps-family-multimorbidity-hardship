/*
  01_load_harmonize.do — load MEPS FYC 2018-2024, harmonize names, append.
  In:  $rawdata/{h209,h216,h224,h233,h243,h251,h256}.dta
  Out: $data/clean/person_harmonized.dta
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

capture mkdir "$data"
capture mkdir "$data/clean"
capture mkdir "$code/logs"

log using "$code/logs/01_load_harmonize.log", replace text

timer on 1
di "=== Step 1: Load and Harmonize ==="
di "Start: $S_DATE $S_TIME"

local files    `" "h209" "h216" "h224" "h233" "h243" "h251" "h256" "'
local years    `" "2018" "2019" "2020" "2021" "2022" "2023" "2024" "'
local suffixes `" "18"   "19"   "20"   "21"   "22"   "23"   "24"   "'

tempfile combined
local first = 1
local n : word count `years'

forvalues i = 1/`n' {
    local f : word `i' of `files'
    local y : word `i' of `years'
    local s : word `i' of `suffixes'

    di _n "--- `f'.dta  (year `y', suffix `s') ---"
    local fpath "$rawdata/`f'.dta"
    capture confirm file "`fpath'"
    if _rc != 0 {
        di as error "ERROR: FYC file `f'.dta not found in $rawdata"
        exit 601
    }
    use "`fpath'", clear

    * h209 ships lowercase; force uppercase for consistent renames below
    rename *, upper

    foreach v in DIABDX JTPAIN31 JTPAIN53 CHBRON31 CHBRON53 {
        capture rename `v'_M18 `v'
    }

    * STEMyy → STEM
    foreach stem in INSCOV TOTSLF FAMINC POVCAT REFPRS REGION {
        capture rename `stem'`s' `stem'
    }
    capture rename FAMWT`s'F FAMWTF

    gen int YEAR = `y'

    #delimit ;
    local keepvars
        YEAR DUID PID DUPERSID PANEL
        FAMIDYR
        SEX RACETHX HIDEG AGELAST
        INSCOV TOTSLF FAMINC POVCAT
        REFPRS REGION
        FAMWTF
        HIBPDX CHDDX ANGIDX MIDX OHRTDX STRKDX EMPHDX
        CHOLDX CANCERDX ARTHDX DIABDX ASTHDX ADHDADDX
        JTPAIN31 JTPAIN53 CHBRON31 CHBRON53
        PROBPY42 CRFMPY42 PYUNBL42
        DLAYCA42 DLAYDN42 DLAYPM42 AFRDCA42 AFRDDN42 AFRDPM42
        EMPST31 EMPST42 EMPST53
        MARRY31X MARRY42X MARRY53X BORNUSA
    ;
    #delimit cr

    local tokeep ""
    foreach v of local keepvars {
        capture confirm variable `v'
        if !_rc  local tokeep `tokeep' `v'
    }
    keep `tokeep'

    di "  N = " _N "   vars = " c(k)

    if `first' {
        save `combined', replace
        local first = 0
    }
    else {
        append using `combined'
        save `combined', replace
    }
}

use `combined', clear

compress
order DUPERSID DUID PID PANEL FAMIDYR YEAR

di _n "=== Pooled person file ==="
di "N = " _N
tab YEAR, m

save "$data/clean/person_harmonized.dta", replace

timer off 1
timer list 1
di "End: $S_DATE $S_TIME"

log close
