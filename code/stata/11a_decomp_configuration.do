/*
  11a_decomp_configuration.do -- descriptive decomposition by how conditions are
  distributed across members, among households with >=2 priority conditions.

  Two levels: concentrated (all conditions held by a single member) and
  distributed (conditions held by 2 or more members, with no requirement that
  each contributes exactly one).

  The analysis is descriptive. Contrasts hold the total condition count in the
  model, but distributed households have >=2 possible reporters because outcomes
  are household maxima; key-member attributes also differ mechanically between
  the two groups.

  In:  $data/clean/family_analytic.csv
  Out: $data/clean/decomp_config_desc_stata.csv,
       $data/clean/decomp_config_results_stata.csv
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
log using "$code/logs/11a_decomp_configuration.log", replace text

timer on 11
di "=== Step 11a: Configuration Decomposition ==="
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

assert fam_n_conditions >= n_sick_members

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
local cfg_lab1 "Concentrated"
local cfg_lab2 "Distributed"

* C. Eligibility and within-household distribution of conditions
gen byte elig = fam_n_conditions >= 2
label variable elig "1 = family has >=2 priority conditions"

gen byte config = .
replace config = 1 if elig == 1 & n_sick_members == 1
replace config = 2 if elig == 1 & n_sick_members >= 2
label define config_lbl 1 "Concentrated" 2 "Distributed", replace
label values config config_lbl

qui count if elig == 1 & missing(config)
assert r(N) == 0
qui count if elig == 0 & !missing(config)
assert r(N) == 0

di _n "--- Configuration by insurance (eligible households) ---"
tab config ins_num if elig == 1, m

* D. Descriptive table
*    share_of_all_eligible is a JOINT share (denominator = every household with
*    >=2 conditions), so the two configuration rows sum to 1.
qui sum poolwt if elig == 1, meanonly
local w_elig = r(sum)

tempname dh
tempfile dfile
postfile `dh' ///
    str18 level ///
    str24 config ///
    str25 ins_type ///
    long n_unweighted ///
    double share_of_all_eligible ///
    using `dfile'

forvalues c = 1/2 {
    preserve
        keep if elig == 1 & config == `c'
        qui count
        local n_unw = r(N)
        qui sum poolwt, meanonly
        local w_cell = r(sum)
        local w_share = cond(`w_elig' > 0, `w_cell' / `w_elig', .)
        post `dh' ("config_overall") ("`cfg_lab`c''") ("Overall") ///
            (`n_unw') (`w_share')
    restore
}

postclose `dh'

preserve
    use `dfile', clear
    * Guard against silent str-width truncation of the category label: the
    * row-count asserts below check counts only, never label content.
    assert inlist(config, "Concentrated", "Distributed")
    * Joint shares must exhaust the eligible population exactly once.
    qui sum share_of_all_eligible if level == "config_overall", meanonly
    assert abs(r(sum) - 1) < 1e-8
    export delimited using "$data/clean/decomp_config_desc_stata.csv", replace
restore

* E. Cell-size gate: do not merge categories if sparse.
local sparse = 0
forvalues c = 1/2 {
    forvalues i = 1/3 {
        qui count if elig == 1 & config == `c' & ins_num == `i'
        di "Cell N: `cfg_lab`c'' / `ins_lab`i'' = " r(N)
        if r(N) < 30 local sparse = 1
    }
}
if `sparse' == 1 {
    di as error "ERROR: at least one config x insurance cell has N < 30."
    di as error "       The cell counts are listed above."
    exit 459
}

* F. Configuration models
tempname rh
tempfile rfile
* dis_cat carries the CONFIGURATION label for pred_prob rows (11b reads it by
* name). Width is deliberately generous: "Concentrated" is exactly 12 chars, so
* a str12 declaration would sit on the truncation boundary and any future
* relabel would corrupt the category key silently.
postfile `rh' ///
    str12 type ///
    str25 outcome ///
    str25 ins_type ///
    str24 dis_cat ///
    str55 comparison ///
    double estimate ///
    double se ///
    double ci_lower ///
    double ci_upper ///
    double pvalue ///
    using `rfile'

local outcomes "che_40 fam_debt_any fam_barrier_any fam_any_hardship"

foreach outcome of local outcomes {
    di _n "=== Outcome: `outcome' configuration model ==="
    capture noisily svy, subpop(elig): logit `outcome' i.config##i.ins_num c.fam_n_conditions `covars'
    if _rc != 0 {
        local rc = _rc
        di as error "ERROR: model failed for `outcome' (rc=`rc')"
        exit `rc'
    }
    if e(converged) == 0 {
        di as error "ERROR: model did not converge for `outcome'"
        exit 430
    }

    * Cell-size gate on THIS outcome's actual estimation sample. The gate in
    * section E counts eligible households only; outcome-specific missingness
    * shrinks the modeled sample further, so a cell could fall below 30 while
    * every row-count assert still passed.
    capture drop _esamp
    gen byte _esamp = e(sample)
    local sparse_out = 0
    forvalues c = 1/2 {
        forvalues i = 1/3 {
            qui count if _esamp == 1 & elig == 1 & config == `c' & ins_num == `i'
            di "  est-sample cell N: `outcome' / `cfg_lab`c'' / `ins_lab`i'' = " r(N)
            if r(N) < 30 {
                di as error "ERROR: `outcome' cell `cfg_lab`c'' x `ins_lab`i'' has N=" r(N) " (<30) in the estimation sample"
                local sparse_out = 1
            }
        }
    }
    drop _esamp
    if `sparse_out' == 1 exit 459

    capture noisily margins config#ins_num, subpop(elig) post
    if _rc != 0 {
        local rc = _rc
        di as error "ERROR: margins failed for `outcome' (rc=`rc')"
        exit `rc'
    }

    local df   = e(df_r)
    local crit = invttail(`df', 0.025)

    forvalues c = 1/2 {
        forvalues i = 1/3 {
            local b  = _b[`c'.config#`i'.ins_num]
            local se = _se[`c'.config#`i'.ins_num]
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

            post `rh' ("pred_prob") ("`outcome'") ("`ins_lab`i''") ("`cfg_lab`c''") ///
                ("") (`b') (`se') (`lo') (`hi') (.)
        }
    }

    forvalues i = 1/3 {
        lincom _b[1.config#`i'.ins_num] - _b[2.config#`i'.ins_num]
        post `rh' ("config_diff") ("`outcome'") ("`ins_lab`i''") ("") ///
            ("Concentrated vs distributed") ///
            (r(estimate)) (r(se)) (r(lb)) (r(ub)) (r(p))
    }
}

postclose `rh'

use `rfile', clear
tab type outcome
* 4 outcomes x (2 config x 3 insurance pred_probs + 3 config contrasts) = 36
assert _N == 36
* Category-key integrity: 11b joins the figure on these exact strings.
assert inlist(dis_cat, "Concentrated", "Distributed") if type == "pred_prob"
assert dis_cat == "" if type == "config_diff"
export delimited using "$data/clean/decomp_config_results_stata.csv", replace
di "Configuration decomposition saved to decomp_config_results_stata.csv"

timer off 11
timer list 11
di _n "=== Step 11a complete ==="
di "End: $S_DATE $S_TIME"

log close
