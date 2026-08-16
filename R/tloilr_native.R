# tloilr.R -- function file (rootcoder007/morie)
# (native R translation; no library() calls)
#
# Optimal individualised treatment under a resource constraint.
#
# The unconstrained optimal rule treats everyone whose conditional
# treatment effect ("blip")
# B(W) = E[Y | A=1, W] - E[Y | A=0, W]
# is positive. Real programmes cannot: at most a proportion kappa of the
# population can be treated.
#
# The constrained rule is a threshold on the blip. Treat the units with
# the largest blip until the budget is exhausted, i.e.
# d_kappa(W) = I{B(W) > tau_kappa} where tau_kappa is the (1-kappa)
# quantile of B(W) -- and tau_kappa = 0 when the constraint does not
# bind, which recovers the unconstrained rule exactly. The value is
# E[Y_{d_kappa}].
#
# The constraint makes estimation easier, not harder, and that is the
# chapter's point. Regular estimation of the unconstrained optimal
# value requires a nonexceptional law: the blip must not have a point
# mass at zero, because there the optimal rule is not uniquely defined
# and the value is not pathwise differentiable. Under an active
# constraint with continuous covariates the relevant condition is
# instead about the blip's density at the threshold tau_kappa > 0,
# which is far more reasonable than assuming nothing sits exactly at
# zero. So the constrained problem admits a root-n estimator with valid
# confidence intervals in settings where the unconstrained one does not.
#
# References
# ----------
# van der Laan, M. J. & Rose, S. (2018) Targeted Learning in Data
# Science, Springer, doi:10.1007/978-3-319-65304-4. Chap. 23
# (Luedtke & van der Laan): a resource constraint under which a maximum
# proportion of the population can be treated; a root-n rate estimator
# for the optimal resource-constrained value with confidence intervals;
# efficiency among all regular asymptotically linear estimators in the
# nonparametric model; and the statement that when the baseline
# covariates are continuous and the resource constraint is ACTIVE --
# the constrained value strictly below the unconstrained one -- the
# conditions are more reasonable than the nonexceptional law assumption
# needed for regular estimation of the optimal unconstrained value in
# Chap. 22. The data structure (W, A, Y) with Y bounded in the unit
# interval, noting any bounded continuous outcome can be rescaled.
#
# Luedtke, A. R. & van der Laan, M. J. (2016) "Optimal Individualized
# Treatments in Resource-Limited Settings", International Journal of
# Biostatistics 12(1), 283-303, doi:10.1515/ijb-2015-0007.
#
# Luedtke, A. R. & van der Laan, M. J. (2016) "Statistical inference
# for the mean outcome under a possibly non-unique optimal treatment
# strategy", Annals of Statistics 44(2), 713-742, doi:10.1214/15-AOS1384.
# The nonexceptional law condition.

.tloilr_eps <- 1e-12

#' .tloilr_blip
#'
#' A step of the tloilr_native implementation. Called by \code{.tloilr_constrained_value}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param Q1 Coerced to numeric by the body, with \code{as.numeric}.
#' @param Q0 Coerced to numeric by the body, with \code{as.numeric}.
#' @return A numeric value.
#' @export
.tloilr_blip <- function(Q1, Q0) {
  a <- as.numeric(Q1)
  b <- as.numeric(Q0)
  if (length(a) != length(b)) {
    stop("tloilr: the two arms differ in length")
  }
  a - b
}

#' .tloilr_resource_threshold
#'
#' A step of the tloilr_native implementation. Called by \code{.tloilr_constrained_rule}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param B Coerced to numeric by the body, with \code{as.numeric}.
#' @param kappa Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{tau}, \code{quantile}, \code{kappa}, \code{binding}, \code{fraction_positive_blip}, \code{note}.
#' @export
.tloilr_resource_threshold <- function(B, kappa) {
  b <- sort(as.numeric(B))
  kp <- as.numeric(kappa)
  if (kp <= 0 || kp > 1) {
    stop(sprintf("tloilr: kappa must lie in (0,1], got %g", kp))
  }
  n <- length(b)
  # Python: idx = int(math.ceil((1.0 - kp) * n)) - 1  (0-based)
  # R is 1-based, so we add 1 to the Python index.
  idx <- ceiling((1.0 - kp) * n)
  idx <- min(max(idx, 1L), as.integer(n))
  q <- b[idx]
  binding <- sum(b > 0) > kp * n
  list(tau = max(0, q),
       quantile = q,
       kappa = kp,
       binding = binding,
       fraction_positive_blip = sum(b > 0) / n,
       note = "tau = 0 exactly when the budget is not binding, which recovers the unconstrained rule")
}

#' .tloilr_constrained_rule
#'
#' A step of the tloilr_native implementation. Called by \code{.tloilr_constrained_value}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param B Coerced to numeric by the body, with \code{as.numeric}.
#' @param kappa Passed to \code{.tloilr_resource_threshold}.
#' @return A list with \code{rule}, \code{tau}, \code{treated_fraction}, \code{binding}.
#' @export
.tloilr_constrained_rule <- function(B, kappa) {
  b <- as.numeric(B)
  t <- .tloilr_resource_threshold(b, kappa)
  d <- ifelse(b > t$tau, 1.0, 0.0)
  list(rule = d,
       tau = t$tau,
       treated_fraction = mean(d),
       binding = t$binding)
}

#' .tloilr_constrained_value
#'
#' A step of the tloilr_native implementation. Called by \code{morie_tloilr}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param Q1 Coerced to numeric by the body, with \code{as.numeric}.
#' @param Q0 Coerced to numeric by the body, with \code{as.numeric}.
#' @param kappa Passed to \code{.tloilr_constrained_rule}.
#' @return A list with \code{estimate}, \code{value}, \code{unconstrained_value}, \code{cost_of_constraint}, \code{tau}, \code{treated_fraction}, \code{kappa}, \code{binding}, \code{method}, \code{note}.
#' @export
.tloilr_constrained_value <- function(Q1, Q0, kappa) {
  q1 <- as.numeric(Q1)
  q0 <- as.numeric(Q0)
  B <- .tloilr_blip(q1, q0)
  r <- .tloilr_constrained_rule(B, kappa)
  n <- length(q1)
  val <- sum(ifelse(r$rule > 0.5, q1, q0)) / n
  unc <- sum(pmax(q1, q0)) / n
  list(estimate = val,
       value = val,
       unconstrained_value = unc,
       cost_of_constraint = unc - val,
       tau = r$tau,
       treated_fraction = r$treated_fraction,
       kappa = as.numeric(kappa),
       binding = r$binding,
       method = "optimal resource-constrained value; van der Laan & Rose (2018) Chap. 23",
       note = "a binding constraint makes the estimation problem EASIER: the condition concerns the blip's density at tau > 0 rather than the absence of an atom at zero")
}

#' .tloilr_exceptional_law
#'
#' A step of the tloilr_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param B Coerced to numeric by the body, with \code{as.numeric}.
#' @param tol Defaults to \code{1e-09}.
#' @return A list with \code{mass_at_zero}, \code{exceptional}, \code{n_at_zero}, \code{note}.
#' @export
.tloilr_exceptional_law <- function(B, tol = 1e-9) {
  b <- as.numeric(B)
  n <- length(b)
  at_zero <- sum(abs(b) <= tol)
  list(mass_at_zero = at_zero / n,
       exceptional = at_zero > 0,
       n_at_zero = at_zero,
       note = "exceptional laws break regular estimation of the UNCONSTRAINED optimal value; the constrained problem is unaffected when tau > 0")
}

#' .tloilr_cheatsheet
#'
#' A step of the tloilr_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.tloilr_cheatsheet <- function() {
  paste0("tloilr: at most a proportion kappa can be treated, so the ",
         "rule is a THRESHOLD on the blip B(W) = Q(1,W) - Q(0,W): ",
         "treat the largest blips until the budget runs out, ",
         "tau = max(0, (1-kappa) quantile), and tau = 0 recovers ",
         "the unconstrained rule. The constraint makes inference ",
         "EASIER: regular estimation of the unconstrained value ",
         "needs a NONEXCEPTIONAL law (no atom of blip at zero), ",
         "while an ACTIVE constraint with continuous covariates ",
         "only needs a condition at tau > 0 -- far more reasonable, ",
         "and root-n estimation follows.")
}

# Main entry point -- compact alias per ledger/NAMING.md
#' Main entry point -- compact alias per ledger/NAMING.md
#'
#' A step of the tloilr_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param Q1 Passed to \code{.tloilr_constrained_value}.
#' @param Q0 Passed to \code{.tloilr_constrained_value}.
#' @param kappa Passed to \code{.tloilr_constrained_value}.
#' @return The value of \code{.tloilr_constrained_value}.
#' @export
morie_tloilr <- function(Q1, Q0, kappa) {
  .tloilr_constrained_value(Q1, Q0, kappa)
}
