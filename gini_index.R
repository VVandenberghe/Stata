# =============================================================================
# Gini Inequality Index - R
# =============================================================================
# Computes the Gini coefficient from income (or any non-negative variable).
# Three approaches are provided:
#   1. Manual computation via the covariance formula
#   2. Manual computation via the Lorenz curve (trapezoidal rule)
#   3. ineq package  (install once: install.packages("ineq"))
#
# The Gini coefficient G in [0, 1]:
#   G = 0  -> perfect equality
#   G = 1  -> maximum inequality
#
# Formula used in approach 1 (Brown / covariance):
#   G = 2 * Cov(y, rank(y)) / (n * mu)
#
# Last update: May 2026
# =============================================================================

set.seed(12345)
N      <- 1000
income <- exp(rnorm(N, mean = 10, sd = 0.8))

# -----------------------------------------------------------------------------
# 1. Gini via covariance formula
# -----------------------------------------------------------------------------

gini_cov <- function(x) {
  x  <- sort(x)
  n  <- length(x)
  mu <- mean(x)
  rk <- seq_len(n)
  2 * cov(x, rk) * (n - 1) / (n * mu * n)
}

g_cov <- gini_cov(income)
cat("\n--------------------------------------------------------------\n")
cat("Approach 1 - Covariance formula\n")
cat("--------------------------------------------------------------\n")
cat(sprintf("Gini coefficient: %.4f\n", g_cov))

# -----------------------------------------------------------------------------
# 2. Gini via Lorenz curve (trapezoidal rule)
# -----------------------------------------------------------------------------

gini_lorenz <- function(x) {
  x         <- sort(x)
  n         <- length(x)
  cum_pop   <- c(0, seq_len(n) / n)
  cum_share <- c(0, cumsum(x) / sum(x))
  lorenz_area <- sum(0.5 * diff(cum_pop) * (cum_share[-1] + cum_share[-length(cum_share)]))
  list(gini = 1 - 2 * lorenz_area, lorenz_area = lorenz_area,
       cum_pop = cum_pop, cum_share = cum_share)
}

res_lorenz  <- gini_lorenz(income)
g_lorenz    <- res_lorenz$gini
lorenz_area <- res_lorenz$lorenz_area

cat("\n--------------------------------------------------------------\n")
cat("Approach 2 - Lorenz curve (trapezoidal rule)\n")
cat("--------------------------------------------------------------\n")
cat(sprintf("Area under Lorenz curve: %.6f\n", lorenz_area))
cat(sprintf("Gini coefficient:        %.4f\n", g_lorenz))

# -----------------------------------------------------------------------------
# 3. Lorenz curve plot
# -----------------------------------------------------------------------------

plot(
  res_lorenz$cum_pop, res_lorenz$cum_share,
  type = "l", col = "navy", lwd = 2,
  xlab = "Cumulative population share",
  ylab = "Cumulative income share",
  main = "Lorenz Curve",
  sub  = sprintf("Gini = %.4f", g_lorenz)
)
abline(0, 1, col = "red", lty = 2, lwd = 1.5)
legend("topleft", legend = c("Lorenz curve", "Line of equality"),
       col = c("navy", "red"), lty = c(1, 2), lwd = c(2, 1.5), bty = "n")

# -----------------------------------------------------------------------------
# 4. ineq package
# -----------------------------------------------------------------------------

if (requireNamespace("ineq", quietly = TRUE)) {
  cat("\n--------------------------------------------------------------\n")
  cat("Approach 3 - ineq package\n")
  cat("--------------------------------------------------------------\n")
  g_ineq <- ineq::Gini(income)
  cat(sprintf("Gini coefficient: %.4f\n", g_ineq))
} else {
  cat("\n(ineq not installed - run: install.packages(\"ineq\"))\n")
}

# -----------------------------------------------------------------------------
# 5. Group-level Gini (within each income tercile)
# -----------------------------------------------------------------------------

group <- cut(income,
             breaks = quantile(income, probs = c(0, 1/3, 2/3, 1)),
             labels = c("Bottom third", "Middle third", "Top third"),
             include.lowest = TRUE)

cat("\n--------------------------------------------------------------\n")
cat("Gini by income tercile\n")
cat("--------------------------------------------------------------\n")
for (g in levels(group)) {
  cat(sprintf("%-14s: %.4f\n", g, gini_cov(income[group == g])))
}

# -----------------------------------------------------------------------------
# 6. Summary
# -----------------------------------------------------------------------------

cat("\n==============================\n")
cat("Summary of Gini estimates\n")
cat("==============================\n")
cat(sprintf("Covariance formula : %.4f\n", g_cov))
cat(sprintf("Lorenz / trapezoid : %.4f\n", g_lorenz))
cat("(Both should agree to at least 4 decimal places)\n")
