# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Analytic combinatorics. Mirrors morie.fn.anlcmb.
#
# The discipline of the field is that an asymptotic estimate is a
# THEOREM about exact coefficients, so every estimate here is computed
# alongside the exact value it approximates and the two are compared.
# Exact integers run through the bigint layer (bigint_native.R), since
# R has no exact integer beyond 2^53 and Fibonacci passes it at n = 79.
#
# The derangement rounding identity is deliberately verified by a
# DIFFERENT route than the Python mirror: Python encloses e in rational
# arithmetic and bounds |D_n - n!/e| directly; here D_n is recomputed
# from the inclusion-exclusion sum sum_k (-1)^k n!/k! in exact integers
# and required to equal the recurrence value, with the alternating tail
# supplying the bound 1/(n+1) < 1/2. Two independent derivations of the
# same claim are a stronger check than one derivation copied twice.
#
# Flajolet P, Sedgewick R (2009) Analytic Combinatorics, CUP, Ch IV
# and VI; Hardy GH, Ramanujan S (1918) Proc London Math Soc 17:75-115;
# de Bruijn NG (1981) Asymptotic Methods in Analysis, Dover, s 3.10.

#' Power-series coefficients of a rational generating function
#'
#' For \eqn{P(x)/Q(x)} with \eqn{Q(0) \ne 0} the coefficients satisfy
#' the linear recurrence read off \eqn{Q}. With integer coefficients
#' and \eqn{q_0 = \pm 1} the sequence is integral and is computed in
#' arbitrary precision, exact at every index -- Fibonacci leaves the
#' double-exact range at \eqn{n = 79} and keeps going. Otherwise the
#' expansion runs in doubles and says so.
#'
#' @param numerator,denominator Coefficient vectors, constant first.
#' @param n_terms Number of coefficients to return.
#' @return A list with `coefficients` (numeric), `exact_coefficients`
#'   (decimal strings when exact), `all_integral`, `is_exact`.
#' @export
morie_rational_gf_coefficients <- function(numerator, denominator, n_terms) {
  p <- as.numeric(numerator)
  q <- as.numeric(denominator)
  m <- as.integer(n_terms)
  if (length(q) == 0L || q[1] == 0) {
    stop(paste("the denominator must have a non-zero constant term; a",
               "zero there is a pole at the origin, not a power series."),
         call. = FALSE)
  }
  if (is.na(m) || m < 1L) {
    stop(sprintf("n_terms must be positive; got %s", n_terms), call. = FALSE)
  }
  integral_setup <- all(p == floor(p)) && all(q == floor(q)) &&
    abs(q[1]) == 1
  if (integral_setup) {
    pb <- lapply(p, morie_bigint)
    qb <- lapply(q, morie_bigint)
    coeffs <- vector("list", m)
    for (n in seq_len(m)) {
      acc <- if (n <= length(pb)) pb[[n]] else morie_bigint(0)
      for (i in seq_len(min(n - 1L, length(qb) - 1L))) {
        acc <- morie_big_sub(acc, morie_big_mul(qb[[i + 1L]],
                                                coeffs[[n - i]]))
      }
      # q0 = +-1, so division is a sign flip at most
      if (q[1] == -1) acc <- morie_big_sub(morie_bigint(0), acc)
      coeffs[[n]] <- acc
    }
    ex <- vapply(coeffs, as.character, character(1))
    list(coefficients = as.numeric(ex), exact_coefficients = ex,
         all_integral = TRUE, is_exact = TRUE, n = m,
         method = "Analytic combinatorics (Flajolet and Sedgewick 2009)")
  } else {
    coeffs <- numeric(m)
    for (n in seq_len(m)) {
      acc <- if (n <= length(p)) p[n] else 0
      for (i in seq_len(min(n - 1L, length(q) - 1L))) {
        acc <- acc - q[i + 1L] * coeffs[n - i]
      }
      coeffs[n] <- acc / q[1]
    }
    list(coefficients = coeffs, exact_coefficients = NULL,
         all_integral = all(coeffs == floor(coeffs)), is_exact = FALSE,
         n = m,
         warnings = paste(
           "The setup is not integral with unit leading denominator, so",
           "the expansion ran in doubles and low-order digits may be",
           "lost past 2^53."),
         method = "Analytic combinatorics (Flajolet and Sedgewick 2009)")
  }
}

#' Exponential growth rate from the dominant singularity
#'
#' Coefficients of a rational function grow like \eqn{C \rho^{-n}}
#' where \eqn{\rho} is the denominator root closest to the origin
#' (Flajolet-Sedgewick Theorem IV.7). Found by bisection on the
#' positive real axis -- the Pringsheim case, where non-negative
#' coefficients put a singularity there. If `coefficients` are
#' supplied, the measured ratio of the last two is reported against
#' the predicted \eqn{1/\rho}.
#'
#' @param denominator Coefficient vector, constant first.
#' @param coefficients Optional sequence to check the prediction on.
#' @return A list with `radius`, `growth_rate`, `measured_ratio`,
#'   `relative_gap`.
#' @export
morie_dominant_singularity_growth <- function(denominator,
                                              coefficients = NULL) {
  q <- as.numeric(denominator)
  if (length(q) == 0L || q[1] == 0) {
    stop("the denominator must have a non-zero constant term.",
         call. = FALSE)
  }
  qat <- function(x) {
    acc <- 0
    for (c in rev(q)) acc <- acc * x + c
    acc
  }
  q0 <- qat(0)
  lo <- 0; hi <- 1e-6
  found <- FALSE
  while (hi < 1e9) {
    if (qat(hi) * q0 < 0) { found <- TRUE; break }
    lo <- hi; hi <- hi * 2
  }
  if (!found) {
    stop(paste("no positive real root of the denominator was found below",
               "1e9; the dominant singularity is complex or absent, and",
               "this routine only handles the Pringsheim case."),
         call. = FALSE)
  }
  for (i in seq_len(200L)) {
    mid <- 0.5 * (lo + hi)
    if (qat(mid) * q0 < 0) hi <- mid else lo <- mid
  }
  rho <- 0.5 * (lo + hi)
  rate <- 1 / rho
  measured <- NULL; gap <- NULL
  if (!is.null(coefficients)) {
    cs <- as.numeric(coefficients)
    nc <- length(cs)
    if (nc >= 2L && cs[nc - 1L] != 0) {
      measured <- cs[nc] / cs[nc - 1L]
      gap <- abs(measured - rate) / rate
    }
  }
  list(radius = rho, growth_rate = rate, estimate = rate,
       measured_ratio = measured, relative_gap = gap, n = length(q) - 1L,
       method = "Analytic combinatorics (Flajolet and Sedgewick 2009)")
}

#' The basic transfer theorem
#'
#' \eqn{\[x^n\](1-x)^{-\alpha} = \binom{n+\alpha-1}{n}} exactly;
#' \eqn{n^{\alpha-1}/\Gamma(\alpha)} asymptotically
#' (Flajolet-Sedgewick Theorem VI.1), with the first-order correction
#' \eqn{1 + \alpha(\alpha-1)/(2n)}.
#'
#' @param alpha Exponent; must not be a non-positive integer.
#' @param n Coefficient index.
#' @return A list with `exact_coefficient`, `asymptotic`, `corrected`,
#'   `ratio`, `corrected_ratio`.
#' @export
morie_singularity_transfer <- function(alpha, n) {
  a <- as.numeric(alpha)
  n <- as.integer(n)
  if (is.na(n) || n < 1L) {
    stop(sprintf("n must be positive; got %s", n), call. = FALSE)
  }
  if (a <= 0 && a == floor(a)) {
    stop(sprintf(paste(
      "alpha must not be a non-positive integer; got %s:",
      "(1-x)^-alpha is a polynomial there and the transfer theorem",
      "does not apply."), alpha), call. = FALSE)
  }
  i <- seq_len(n)
  exact <- prod((a + i - 1) / i)
  asym <- n^(a - 1) / gamma(a)
  corrected <- asym * (1 + a * (a - 1) / (2 * n))
  list(exact_coefficient = exact, asymptotic = asym, corrected = corrected,
       ratio = exact / asym, corrected_ratio = exact / corrected,
       estimate = asym, alpha = a, n = n,
       method = "Analytic combinatorics (Flajolet and Sedgewick 2009)")
}

#' Stirling's series with its error bound checked
#'
#' The series for \eqn{\ln n!} is alternating-enveloping, so the
#' remainder after \eqn{K} correction terms is at most the first
#' omitted term (de Bruijn 1981, section 3.10). Truth is `lgamma(n+1)`.
#'
#' The promise outruns the arithmetic quickly: at \eqn{n = 50} with
#' three terms the bound is \eqn{7.6 \times 10^{-16}} while
#' \eqn{\ln 50! \approx 148}, whose representable neighbours are
#' \eqn{3 \times 10^{-14}} apart. Below that resolution the check would
#' measure the rounding of the comparison, not the series, so
#' `error_within_bound` is judged against bound plus the
#' double-precision floor, reported separately as `double_floor`.
#'
#' @param n Argument of the factorial.
#' @param terms Correction terms, 0 to 4. The error bound is the first
#'   OMITTED term, so 5 would need B12, which is not tabulated here.
#' @return A list with `log_factorial`, `series_value`, `error`,
#'   `bound`, `double_floor`, `error_within_bound`.
#' @export
morie_stirling_series_error <- function(n, terms = 3) {
  n <- as.integer(n)
  k <- as.integer(terms)
  if (is.na(n) || n < 1L) {
    stop(sprintf("n must be positive; got %s", n), call. = FALSE)
  }
  if (is.na(k) || k < 0L || k > 4L) {
    stop(sprintf(paste(
      "terms must lie in 0..4; got %s: the error bound is the first",
      "OMITTED term, so K = 5 would need B12, which is not tabulated",
      "here. A bound faked from a lower Bernoulli number would be a",
      "promise the theory never made."), terms), call. = FALSE)
  }
  bern <- c(1 / 6, -1 / 30, 1 / 42, -1 / 30, 5 / 66)
  approx <- n * log(n) - n + 0.5 * log(2 * pi * n)
  for (j in seq_len(k)) {
    approx <- approx + bern[j] / (2 * j * (2 * j - 1) * n^(2 * j - 1))
  }
  kk <- k + 1L
  bound <- abs(bern[kk]) / (2 * kk * (2 * kk - 1) * n^(2 * kk - 1))
  truth <- lgamma(n + 1)
  err <- abs(approx - truth)
  floorv <- 8 * abs(truth) * 2^-53
  list(log_factorial = truth, series_value = approx, error = err,
       bound = bound, double_floor = floorv,
       error_within_bound = err <= bound + floorv,
       estimate = approx, terms = k, n = n,
       method = "Stirling's series (de Bruijn 1981, section 3.10)")
}

#' The rounding identity for derangements
#'
#' \eqn{D_n} is the nearest integer to \eqn{n!/e}, with
#' \eqn{|D_n - n!/e| < 1/(n+1)}. Verified here by computing \eqn{D_n}
#' twice in exact integers -- by the recurrence
#' \eqn{D_n = (n-1)(D_{n-1} + D_{n-2})} and by the inclusion-exclusion
#' sum \eqn{\sum_k (-1)^k n!/k!} -- and requiring the two to agree
#' digit for digit. The alternating tail of the sum is what supplies
#' the \eqn{1/(n+1)} bound, so agreement of the two routes IS the
#' identity. Past \eqn{n = 18}, \eqn{n!/e} does not fit a double.
#'
#' @param n Number of objects.
#' @return A list with `derangements`, `exact` (decimal string),
#'   `routes_agree`, `is_nearest_integer`, `within_theoretical_bound`.
#' @export
morie_derangement_rounding <- function(n) {
  n <- as.integer(n)
  if (is.na(n) || n < 0L) {
    stop(sprintf("n must be non-negative; got %s", n), call. = FALSE)
  }
  dm2 <- morie_bigint(1)   # D_0
  dm1 <- morie_bigint(0)   # D_1
  dn <- if (n == 0L) dm2 else dm1
  if (n >= 2L) {
    for (m in 2:n) {
      dn <- morie_big_mul(morie_bigint(m - 1), morie_big_add(dm1, dm2))
      dm2 <- dm1; dm1 <- dn
    }
  }
  # inclusion-exclusion: sum_k (-1)^k n!/k!, built as t_k = t_{k-1}
  # shrinking -- n!/k! = (n!/(k-1)!)/k, exact at every step
  term <- morie_big_factorial(n)   # k = 0
  acc <- term
  if (n >= 1L) {
    for (k in 1:n) {
      term <- morie_big_divmod_small(term, k)$quotient
      acc <- if (k %% 2L == 1L) morie_big_sub(acc, term) else
        morie_big_add(acc, term)
    }
  }
  agree <- morie_big_cmp(dn, acc) == 0
  out <- list(derangements = as.numeric(as.character(dn)),
              exact = as.character(dn),
              estimate = as.numeric(as.character(dn)),
              routes_agree = agree,
              is_nearest_integer = n >= 1L && agree,
              within_theoretical_bound = agree,
              distance_bound = if (n >= 1L) 1 / (n + 1) else 0,
              n = n, warnings = character(0),
              method = "Meromorphic asymptotics: D_n = round(n!/e)")
  if (!agree) {
    out$warnings <- c(out$warnings, paste(
      "The recurrence and the inclusion-exclusion sum disagree, so one",
      "of them is implemented wrongly and no rounding claim is made."))
    out$is_nearest_integer <- FALSE
    out$within_theoretical_bound <- FALSE
  }
  out
}

#' Hardy-Ramanujan asymptotics for the partition function
#'
#' \eqn{p(n) \sim \exp(\pi\sqrt{2n/3}) / (4n\sqrt{3})}, held against
#' \eqn{p(n)} computed exactly by Euler's pentagonal recurrence in
#' arbitrary precision. Convergence is famously slow -- the relative
#' error decays like \eqn{n^{-1/2}} and is still 4.6 per cent at
#' \eqn{n = 100} -- and the point of returning both is that the
#' formula's fame should not be mistaken for accuracy.
#'
#' @param n Integer to partition.
#' @return A list with `partitions`, `exact` (decimal string),
#'   `asymptotic`, `ratio`, `relative_error`.
#' @export
morie_hardy_ramanujan_partitions <- function(n) {
  n <- as.integer(n)
  if (is.na(n) || n < 1L) {
    stop(sprintf("n must be positive; got %s", n), call. = FALSE)
  }
  p <- vector("list", n + 1L)
  p[[1]] <- morie_bigint(1)
  for (m in seq_len(n)) {
    total <- morie_bigint(0)
    k <- 1L
    repeat {
      # PARENTHESES REQUIRED: %/% binds tighter than * in R
      g1 <- (k * (3L * k - 1L)) %/% 2L
      g2 <- (k * (3L * k + 1L)) %/% 2L
      if (g1 > m && g2 > m) break
      if (k %% 2L == 1L) {
        if (g1 <= m) total <- morie_big_add(total, p[[m - g1 + 1L]])
        if (g2 <= m) total <- morie_big_add(total, p[[m - g2 + 1L]])
      } else {
        if (g1 <= m) total <- morie_big_sub(total, p[[m - g1 + 1L]])
        if (g2 <= m) total <- morie_big_sub(total, p[[m - g2 + 1L]])
      }
      k <- k + 1L
    }
    p[[m + 1L]] <- total
  }
  exact <- p[[n + 1L]]
  asym <- exp(pi * sqrt(2 * n / 3)) / (4 * n * sqrt(3))
  ratio <- asym / as.numeric(as.character(exact))
  list(partitions = as.numeric(as.character(exact)),
       exact = as.character(exact), estimate = asym, asymptotic = asym,
       ratio = ratio, relative_error = ratio - 1, n = n,
       method = "Hardy-Ramanujan asymptotic (1918)")
}
