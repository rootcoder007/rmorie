# SPDX-License-Identifier: AGPL-3.0-or-later

#' Empirical convergence exponent for the index coefficients
#'
#' Horowitz (2009), Semiparametric and Nonparametric Methods in
#' Econometrics, Section 2.2 (page 11), Section 2.4 (page 18) and
#' Theorem 2.2, equation (2.26) (page 21).  Semiparametric estimators
#' of beta in a single-index model attain the parametric rate,
#' (b_n - beta) = O_p(n^(-1/2)) and n^(1/2)(bt_n - bt) -> N(0, Sigma).
#'
#' Given errors on nested subsamples, this fits log||b_n - beta|| on
#' log n by least squares and returns the fitted exponent; -1/2 is the
#' value the theory predicts.
#'
#' @param errors Numeric vector of ||b_n - beta|| at each sample size.
#' @param sizes Numeric vector of the sample sizes, same order, at
#'   least three of them.
#' @param target Numeric exponent the theory predicts; reported
#'   alongside the fit as \code{gap}.
#' @return Named list with exponent, se, intercept, gap, rsq, target,
#'   k, n, method.
#' @keywords internal
#' @examples
#' ns <- c(100, 200, 400, 800, 1600)
#' Simbrate(3 * ns^-0.5, ns)$exponent   # -1/2
#' @export
Simbrate <- function(errors, sizes, target = -0.5) {
  e <- as.numeric(errors)
  ns <- as.numeric(sizes)
  if (length(e) != length(ns) || length(e) < 3L) {
    stop("need at least three matching errors and sizes.", call. = FALSE)
  }
  if (any(e <= 0) || any(ns <= 0)) {
    stop("errors and sizes must be strictly positive.", call. = FALSE)
  }
  k <- length(e)
  ly <- log(e)
  lx <- log(ns)
  mx <- mean(lx)
  my <- mean(ly)
  sxx <- sum((lx - mx)^2)
  if (sxx <= 0) stop("sizes must not all be equal.", call. = FALSE)
  slope <- sum((lx - mx) * (ly - my)) / sxx
  inter <- my - slope * mx
  fit <- inter + slope * lx
  sse <- sum((ly - fit)^2)
  sst <- sum((ly - my)^2)
  se <- sqrt(sse / max(k - 2L, 1L) / sxx)
  list(exponent = slope, se = se, intercept = inter,
       gap = slope - as.numeric(target),
       rsq = if (sst > 0) 1 - sse / sst else NaN,
       target = as.numeric(target), k = k, n = as.integer(ns[k]),
       method = "Horowitz (2009) eq. (2.26), root-n rate for beta")
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Simbrate
#' @keywords internal
#' @export
morie_horowitz_rate_beta_estimation <- Simbrate
