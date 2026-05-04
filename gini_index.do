
*************************************************************************************************************
*                                                                                                           *
* Gini Inequality Index                                                                                     *
*                                                                                                           *
*************************************************************************************************************
* Computes the Gini coefficient from income (or any non-negative variable).
* Three approaches are provided:
*   1. Manual computation via the covariance formula
*   2. Manual computation via the Lorenz curve (trapezoidal rule)
*   3. ineqdeco package (must be installed: ssc install ineqdeco)
*
* The Gini coefficient G in [0,1]:
*   G = 0  -> perfect equality
*   G = 1  -> maximum inequality
*
* Formula used in approach 1 (Brown / covariance):
*   G = (2/n^2*mu) * sum_i rank(y_i)*y_i  -  (n+1)/n
*     = 2 * Cov(y, rank(y)) / (n * mu)
*
*Last update: May 2026
*************************************************************************************************************

clear all
set more off

*------------------------------------------------------------------------------------------------------------
* 0. Simulate income data (replace this section with your own dataset)
*------------------------------------------------------------------------------------------------------------

set seed 12345
local N = 1000
set obs `N'
gen double income = exp(rnormal(10, 0.8))
label variable income "Simulated household income (log-normal)"
gen byte group = cond(income <= r(p50), 1, 2) if 0

*------------------------------------------------------------------------------------------------------------
* 1. Gini via covariance formula (fast, exact for large samples)
*------------------------------------------------------------------------------------------------------------

sort income
gen long rank_y = _n
quietly summarize income
local mu = r(mean)
local n  = r(N)
quietly correlate income rank_y, covariance
local cov_yr = r(cov_12)
local gini_cov = (2 * `cov_yr') / (`n' * `mu')

di as text _n "--------------------------------------------------------------"
di as text "Approach 1 - Covariance formula"
di as text "--------------------------------------------------------------"
di as result "Gini coefficient: " %6.4f `gini_cov'
drop rank_y

*------------------------------------------------------------------------------------------------------------
* 2. Gini via Lorenz curve (trapezoidal rule)
*------------------------------------------------------------------------------------------------------------

sort income
gen double cum_pop   = _n / `n'
gen double cum_share = sum(income) / sum(income[_N])
gen double cum_pop_lag   = cum_pop[_n-1]
gen double cum_share_lag = cum_share[_n-1]
replace cum_pop_lag   = 0 in 1
replace cum_share_lag = 0 in 1
gen double trap = 0.5 * (cum_pop - cum_pop_lag) * (cum_share + cum_share_lag)
quietly summarize trap
local lorenz_area = r(sum)
local gini_lorenz = 1 - 2 * `lorenz_area'

di as text _n "--------------------------------------------------------------"
di as text "Approach 2 - Lorenz curve (trapezoidal rule)"
di as text "--------------------------------------------------------------"
di as result "Area under Lorenz curve: " %8.6f `lorenz_area'
di as result "Gini coefficient:        " %6.4f `gini_lorenz'
drop cum_pop cum_share cum_pop_lag cum_share_lag trap

*------------------------------------------------------------------------------------------------------------
* 3. Lorenz curve plot
*------------------------------------------------------------------------------------------------------------

sort income
gen double cum_pop_plot   = _n / `n'
gen double cum_share_plot = sum(income) / sum(income[_N])
local orig_n = `N' + 1
set obs `orig_n'
replace cum_pop_plot   = 0 in `orig_n'
replace cum_share_plot = 0 in `orig_n'
sort cum_pop_plot

twoway ///
    (line cum_share_plot cum_pop_plot, lcolor(navy) lwidth(medthick)) ///
    (function y = x, range(0 1) lcolor(red) lpattern(dash)) , ///
    xtitle("Cumulative population share") ///
    ytitle("Cumulative income share") ///
    title("Lorenz Curve") ///
    subtitle("Gini = " + string(round(`gini_lorenz', 0.0001))) ///
    legend(order(1 "Lorenz curve" 2 "Line of equality") position(5) ring(0)) ///
    scheme(s2color)

drop if cum_pop_plot == 0 & cum_share_plot == 0
drop cum_pop_plot cum_share_plot

*------------------------------------------------------------------------------------------------------------
* 4. Gini using ineqdeco (install once: ssc install ineqdeco)
*------------------------------------------------------------------------------------------------------------

capture which ineqdeco
if _rc == 0 {
    di as text _n "--------------------------------------------------------------"
    di as text "Approach 3 - ineqdeco package"
    di as text "--------------------------------------------------------------"
    ineqdeco income
}
else {
    di as text _n "(ineqdeco not installed - run:  ssc install ineqdeco)"
}

*------------------------------------------------------------------------------------------------------------
* 5. Group-level Gini (between / within decomposition)
*------------------------------------------------------------------------------------------------------------

xtile group = income, nq(3)
label define grp 1 "Bottom third" 2 "Middle third" 3 "Top third"
label values group grp

di as text _n "--------------------------------------------------------------"
di as text "Gini by income tercile"
di as text "--------------------------------------------------------------"

forvalues g = 1/3 {
    preserve
        keep if group == `g'
        local ng = _N
        sort income
        gen long rank_g = _n
        quietly summarize income
        local mu_g  = r(mean)
        quietly correlate income rank_g, covariance
        local gini_g = (2 * r(cov_12)) / (`ng' * `mu_g')
        di as text "Group `g': " as result %6.4f `gini_g'
    restore
}

*------------------------------------------------------------------------------------------------------------
* 6. Summary
*------------------------------------------------------------------------------------------------------------

di as text _n "=============================="
di as text "Summary of Gini estimates"
di as text "=============================="
di as text "Covariance formula : " as result %6.4f `gini_cov'
di as text "Lorenz / trapezoid : " as result %6.4f `gini_lorenz'
di as text "(Both should agree to at least 4 decimal places)"
