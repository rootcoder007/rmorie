# Adjusted population attributable risk from case-control data.
# Sources: Bruzzi, P., Green, S. B., Byar, D. P., Brinton, L. A. and
# Schairer, C. (1985), Estimating the Population Attributable Risk for
# Multiple Risk Factors Using Case-Control Data, American Journal of
# Epidemiology 122(5), 904-914 (eq. (6) the case-based attributable
# risk); Levin, M. L. (1953), The occurrence of lung cancer in man,
# Acta Unio Internationalis Contra Cancrum 9(3), 531-541 (the
# single-factor formula the general one generalises). The Python
# reference is morie.fn.rapaf.
#
# Native implementation mirroring the Python arm exactly: same AR
# = 1 - sum_j rho_j / R_j with rho taken over the cases, same Levin's
# formula, same partial AR with the non-additivity note, same Monte
# Carlo confidence interval resampling the log rate ratios.

#' Population attributable risk
#'
#' Bruzzi et al. (1985) eq. (6): AR = 1 - sum_j rho_j / R_j, where
#' rho_j is the proportion of cases in stratum j and R_j the adjusted
#' rate ratio of stratum j against the baseline. The control
#' distribution is not used.
#'
#' @param case_counts Non-negative numeric vector of case counts per
#'   stratum.
#' @param rate_ratios Positive numeric vector of adjusted rate ratios.
#' @return A list with \code{estimate}, \code{ar},
#'   \code{case_proportions}, \code{rate_ratios}, \code{n_cases},
#'   \code{n_strata}, \code{uses_control_distribution}, \code{method}.
#' @references Bruzzi, P. et al. (1985). Estimating the Population
#'   Attributable Risk for Multiple Risk Factors Using Case-Control
#'   Data. American Journal of Epidemiology, 122(5), 904-914.
#' @export
morie_rapaf_population_attributable_risk <- function(case_counts,
                                                      rate_ratios) {
  cc <- as.numeric(case_counts); rr <- as.numeric(rate_ratios)
  n <- length(cc)
  if (length(rr) != n)
    stop(sprintf("rapaf: %d strata of cases but %d rate ratios",
                 n, length(rr)))
  if (n < 2L)
    stop(sprintf("rapaf: need at least 2 strata, got %d", n))
  if (any(cc < 0))
    stop("rapaf: case counts must be non-negative")
  if (any(rr <= 0))
    stop("rapaf: rate ratios must be positive")
  tot <- sum(cc)
  if (tot <= 1e-12) stop("rapaf: there are no cases")
  rho <- cc / tot
  s <- sum(rho / rr)
  list(estimate = 1.0 - s, ar = 1.0 - s,
       case_proportions = rho, rate_ratios = rr,
       n_cases = tot, n_strata = n,
       uses_control_distribution = FALSE,
       method = "Bruzzi, Green, Byar, Brinton & Schairer (1985) eq. (6), case-based adjusted attributable risk")
}

#' Levin's single-factor attributable risk
#'
#' Computes Levin's p(R - 1) / (1 + p(R - 1)) from the population
#' exposure prevalence and the rate ratio.
#'
#' @param prevalence Population exposure prevalence, in \code{[0, 1]}.
#' @param rate_ratio Rate ratio of exposed to unexposed.
#' @return Numeric attributable risk.
#' @references Levin, M. L. (1953). The occurrence of lung cancer in
#'   man. Acta Unio Internationalis Contra Cancrum, 9(3), 531-541.
#' @export
morie_rapaf_levin_ar <- function(prevalence, rate_ratio) {
  p <- as.numeric(prevalence); R <- as.numeric(rate_ratio)
  if (p < 0 || p > 1)
    stop(sprintf("rapaf: prevalence must be in [0, 1], got %g", p))
  if (R <= 0) stop("rapaf: the rate ratio must be positive")
  d <- 1.0 + p * (R - 1.0)
  if (abs(d) <= 1e-12)
    stop("rapaf: Levin's formula is undefined here (1 + p(R-1) = 0)")
  p * (R - 1.0) / d
}

#' Partial attributable risk
#'
#' AR attributable to a subset of factors, with the other factors
#' held at their stratum values; partial ARs are not additive.
#'
#' @param case_counts Non-negative case counts per stratum.
#' @param rate_ratios Positive rate ratios.
#' @param baseline_map Integer vector giving, for each stratum, the
#'   index of the stratum it would become under the intervention.
#' @return A list with \code{estimate}, \code{ar}, \code{baseline_map},
#'   \code{note}.
#' @references Bruzzi, P. et al. (1985). Estimating the Population
#'   Attributable Risk for Multiple Risk Factors Using Case-Control
#'   Data. American Journal of Epidemiology, 122(5), 904-914.
#' @export
morie_rapaf_partial_ar <- function(case_counts, rate_ratios,
                                   baseline_map) {
  cc <- as.numeric(case_counts); rr <- as.numeric(rate_ratios)
  n <- length(cc)
  if (length(rr) != n)
    stop(sprintf("rapaf: %d strata of cases but %d rate ratios",
                 n, length(rr)))
  if (n < 2L) stop(sprintf("rapaf: need at least 2 strata, got %d", n))
  if (any(cc < 0)) stop("rapaf: case counts must be non-negative")
  if (any(rr <= 0)) stop("rapaf: rate ratios must be positive")
  tot <- sum(cc); if (tot <= 1e-12) stop("rapaf: there are no cases")
  bm <- as.integer(baseline_map)
  if (length(bm) != n)
    stop(sprintf("rapaf: %d baseline targets for %d strata",
                 length(bm), n))
  if (any(bm < 0L) || any(bm >= n))
    stop("rapaf: a baseline target is out of range")
  s <- 0.0
  for (j in seq_len(n)) s <- s + (cc[j] / tot) * (rr[bm[j] + 1L] / rr[j])
  list(estimate = 1.0 - s, ar = 1.0 - s, baseline_map = bm,
       note = "partial ARs are NOT additive across factor sets; a case exposed to two factors is counted once")
}

#' Rate ratios from a logistic regression
#'
#' Fits a logistic regression to the case / control design matrix
#' and returns the odds ratio of each stratum against the baseline.
#'
#' @param case_counts Non-negative case counts per stratum.
#' @param control_counts Non-negative control counts per stratum.
#' @param design Numeric matrix of covariates (one row per stratum).
#' @param ridge Ridge added to the diagonal of X'WX.
#' @return A list with \code{rate_ratios}, \code{coef}, \code{note}.
#' @references Bruzzi, P. et al. (1985). Estimating the Population
#'   Attributable Risk for Multiple Risk Factors Using Case-Control
#'   Data. American Journal of Epidemiology, 122(5), 904-914.
#' @export
morie_rapaf_rate_ratios_from_logit <- function(case_counts,
                                               control_counts, design,
                                               ridge = 1e-8) {
  ca <- as.numeric(case_counts); co <- as.numeric(control_counts)
  D <- as.matrix(design)
  n <- length(ca)
  if (length(co) != n || nrow(D) != n)
    stop("rapaf: cases, controls and design must agree in length")
  rows <- list(); y <- numeric(0); w <- numeric(0)
  for (j in seq_len(n)) {
    if (ca[j] > 0) {
      rows[[length(rows) + 1L]] <- D[j, ]
      y <- c(y, 1.0); w <- c(w, ca[j])
    }
    if (co[j] > 0) {
      rows[[length(rows) + 1L]] <- D[j, ]
      y <- c(y, 0.0); w <- c(w, co[j])
    }
  }
  if (length(rows) == 0L)
    stop("rapaf: no stratum has any observations")
  X <- do.call(rbind, rows)
  if (is.null(X)) X <- matrix(numeric(0), nrow = 0, ncol = ncol(D))
  if (ncol(X) == 0L) X <- matrix(0.0, nrow = nrow(X), ncol = 1L)
  beta <- .rapaf_logit_irls(X, y, ridge = ridge, obs_weights = w)
  lin <- as.numeric(D %*% beta)
  list(rate_ratios = exp(lin - lin[1L]), coef = beta,
       note = "odds ratios; equal to rate ratios only under the rare-disease approximation")
}

#' Monte Carlo confidence interval for AR
#'
#' Resamples the log rate ratios, recomputes the AR, and reports the
#' percentiles. A delta-method interval on the AR scale can cross 1,
#' which is impossible; this cannot.
#'
#' @param case_counts Non-negative case counts per stratum.
#' @param rate_ratios Positive rate ratios.
#' @param log_rr_se Standard errors of the log rate ratios.
#' @param level Confidence level in \code{(0, 1)}.
#' @param draws Number of Monte Carlo draws.
#' @param seed Seed for the shared generator.
#' @return A list with \code{estimate}, \code{lower}, \code{upper},
#'   \code{level}, \code{draws}.
#' @export
morie_rapaf_ar_confidence_interval <- function(case_counts, rate_ratios,
                                                log_rr_se,
                                                level = 0.95,
                                                draws = 2000L,
                                                seed = 0L) {
  cc <- as.numeric(case_counts); rr <- as.numeric(rate_ratios)
  n <- length(cc)
  if (length(rr) != n)
    stop(sprintf("rapaf: %d strata of cases but %d rate ratios",
                 n, length(rr)))
  if (any(cc < 0)) stop("rapaf: case counts must be non-negative")
  if (any(rr <= 0)) stop("rapaf: rate ratios must be positive")
  tot <- sum(cc); if (tot <= 1e-12) stop("rapaf: there are no cases")
  se <- as.numeric(log_rr_se)
  if (length(se) != length(rr))
    stop(sprintf("rapaf: %d standard errors for %d rate ratios",
                 length(se), length(rr)))
  if (any(se < 0)) stop("rapaf: standard errors must be non-negative")
  if (level <= 0 || level >= 1) stop("rapaf: level must be in (0, 1)")
  e <- .ghc_rng(as.integer(seed))
  rho <- cc / tot
  vals <- numeric(as.integer(draws))
  for (k in seq_len(as.integer(draws))) {
    rs <- exp(log(rr) + se * .ghc_norm(e, length(rr)))
    vals[k] <- 1.0 - sum(rho / rs)
  }
  vals <- sort(vals)
  lo_q <- (1.0 - as.numeric(level)) / 2.0
  N <- length(vals)
  list(estimate = 1.0 - sum(rho / rr),
       lower = vals[max(1L, as.integer(lo_q * N))],
       upper = vals[min(N, as.integer((1.0 - lo_q) * N))],
       level = as.numeric(level), draws = as.integer(draws))
}

# ---- internals ----

#' .rapaf_logit_irls
#'
#' A step of the rapaf_native implementation. Called by \code{morie_rapaf_rate_ratios_from_logit}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; indexed by row and column.
#' @param y A vector; its length is taken.
#' @param ridge Numeric; combined arithmetically in the body. Defaults to \code{1e-08}.
#' @param obs_weights Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.
#' @return The value of \code{beta}, as built in the body.
#' @export
.rapaf_logit_irls <- function(X, y, ridge = 1e-8, obs_weights = NULL) {
  if (is.null(obs_weights)) obs_weights <- rep(1.0, length(y))
  X <- cbind(0, X)
  X[, 1L] <- 1.0
  p <- ncol(X)
  beta <- rep(0.0, p)
  eta <- as.numeric(X %*% beta)
  mu <- 1.0 / (1.0 + exp(-eta))
  for (it in seq_len(200L)) {
    w <- obs_weights * mu * (1.0 - mu)
    z <- eta + (y - mu) / pmax(mu * (1.0 - mu), 1e-12)
    W <- diag(w)
    XtWX <- crossprod(X, W %*% X)
    diag(XtWX) <- diag(XtWX) + ridge
    XtWz <- as.numeric(crossprod(X, w * z))
    delta <- solve(XtWX, XtWz) - beta
    beta <- beta + delta
    eta <- as.numeric(X %*% beta)
    mu <- 1.0 / (1.0 + exp(-eta))
    if (sqrt(sum(delta^2)) < 1e-10) break
  }
  beta
}

# house entry point: the package exports one morie_<module>
morie_rapaf <- morie_rapaf_population_attributable_risk
