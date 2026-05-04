* =============================================================================
* Gini Inequality Index
* =============================================================================
* Formula (exact discrete version, sorted ascending):
*   G = [2 * sum(rank * income) - (n+1) * sum(income)] / [n * sum(income)]
* Equivalent to: 1 - 2 * (area under Lorenz curve)
* =============================================================================

clear
set seed 12345
set obs 200

* --- Generate synthetic income data (log-normal) ---
gen income = exp(rnormal(10, 0.8))
label variable income "Household income"

* --- Sort ascending (required for rank-based formula) ---
sort income
gen n      = _N          // total observations
gen rank   = _n          // rank of each observation (1 = lowest)

* --- Summary statistics ---
quietly sum income
local mean_income = r(mean)
local total_obs   = r(N)

di _newline "--- Income Summary ---"
di "Observations : `total_obs'"
di "Mean income  : " %10.2f `mean_income'
di "Min income   : " %10.2f r(min)
di "Max income   : " %10.2f r(max)

* --- Gini computation ---
quietly {
    egen total_income = sum(income)
    gen  weighted_sum = rank * income
    egen sum_weighted = sum(weighted_sum)

    * Exact discrete Gini formula
    gen gini = (2 * sum_weighted - (n + 1) * total_income) / (n * total_income)
}

* --- Lorenz curve coordinates ---
gen cum_pop_share    = rank / n
gen cum_income_share = sum(income) / total_income

* --- Display Gini result ---
di _newline "--- Gini Coefficient ---"
di "Gini = " %6.4f gini[1]

* --- Lorenz curve plot ---
twoway ///
    (line cum_income_share cum_pop_share, lcolor(navy) lwidth(medium)) ///
    (function y = x, range(0 1) lcolor(red) lpattern(dash)) ///
    , title("Lorenz Curve") ///
      xtitle("Cumulative population share") ///
      ytitle("Cumulative income share") ///
      legend(order(1 "Lorenz curve" 2 "Line of perfect equality")) ///
      note("Gini = " + string(round(gini[1], 0.0001)))
