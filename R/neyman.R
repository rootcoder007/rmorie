# SPDX-License-Identifier: AGPL-3.0-or-later
#' Neyman optimal allocation across strata
#'
#' Neyman, J. (1934), "On the Two Different Aspects of the Representative
#' Method: The Method of Stratified Sampling and the Method of Purposive
#' Selection", Journal of the Royal Statistical Society 97(4), 558-625;
#' JSTOR 2342192.  Pages 579 and 580 were rendered as images with pdftoppm
#' and read visually, because the equations are load-bearing.
#'
#' p. 579, eq. (37), the variance of the estimate sum(M_i ubar_i) of the
#' population total under stratified random sampling, is
#' sigma^2 = sum_i { (M_i^2 / m_i) ((M_i - m_i) / (M_i - 1)) sigma_i^2 }.
#'
#' p. 580 puts S_i^2 = M_i sigma_i^2 / (M_i - 1) and rewrites (37) as
#' eq. (39), whose three terms the page names A, B and -C:
#' A = ((M_0 - m_0) / m_0) sum_i M_i S_i^2;
#' B = sum_i m_i ( M_i S_i / m_i - sum_j M_j S_j / m_0 )^2;
#' C = (M_0 / m_0) sum_i M_i ( S_i - sum_j M_j S_j / M_0 )^2,
#' with M_0 = sum_i M_i, m_0 = sum_i m_i and sigma^2 = A + B - C.  Only B
#' depends on the individual m_i.  B is a sum of squares, so it is >= 0,
#' and it vanishes exactly when m_i = m_0 M_i S_i / sum_j M_j S_j, which is
#' Neyman's optimum allocation; there sigma^2 = A - C, the page's eq. (41).
#' Under Bowley's proportional allocation m_i = m_0 M_i / M_0 instead,
#' B = C identically and sigma^2 = A, the page's eq. (40).
#'
#' ERRATUM in the original.  Equation (40) on p. 580 is printed as
#' "sigma^2 = ((M_0 - M_0) / m_0) sum(M_i S_i^2) = A".  The second M_0 must
#' be m_0: A is defined three lines above as the first term of (39), and as
#' printed the leading factor is identically zero, which would make the
#' variance under proportional allocation vanish for every population.
#' This was read off the rendered page image, so it is a misprint in the
#' 1934 typesetting and not a pdftotext artifact.
#'
#' Argument convention.  In eq. (37) sigma_i^2 is the within-stratum
#' variance taken with divisor M_i; S_i^2 = M_i sigma_i^2 / (M_i - 1) is the
#' same variance with divisor M_i - 1, which is the form survey practice
#' reports.  S_h below is S_i, so eq. (37) with sigma_i^2 eliminated is
#' sigma^2 = sum_i M_i (M_i - m_i) S_i^2 / m_i, and that is what the
#' variance element returns.
#'
#' @param y per-stratum sample means ybar_h, one per stratum, or NULL.  When
#'   supplied the stratified estimate of the population total
#'   sum_h N_h ybar_h is returned as total; it plays no part in the
#'   allocation, which by eq. (39) depends only on N_h and S_h.
#' @param N_h stratum sizes M_i, all positive.
#' @param S_h within-stratum standard deviations S_i, divisor M_i - 1.
#' @param n total number of units to allocate, m_0 of eq. (38).
#' @return list: estimate, allocation, allocation_int, variance,
#'   variance_prop, A, B, C, total, n, method.
#' @keywords internal
#' @examples
#' Neyman(NULL, c(100, 200, 300), c(1, 2, 3), 60)$allocation
#' @export
Neyman <- function(y, N_h, S_h, n) {
  ck <- .s03allocCheck(N_h, S_h, n)
  M <- ck$M
  S <- ck$S
  m0 <- ck$m0
  H <- ck$H
  ab <- .s03allocABC(M, S, m0)
  if (!(ab$T > 0)) {
    stop("neyman_allocation: sum(N_h S_h) must be positive; every stratum has S_h = 0")
  }
  alloc <- numeric(H)
  for (i in seq_len(H)) alloc[i] <- m0 * M[i] * S[i] / ab$T
  var <- .s03allocVar(M, S, alloc)
  B <- 0
  for (i in seq_len(H)) {
    if (alloc[i] > 0) {
      d <- M[i] * S[i] / alloc[i] - ab$T / m0
      B <- B + alloc[i] * d * d
    }
  }
  prop <- numeric(H)
  for (i in seq_len(H)) prop[i] <- m0 * M[i] / ab$M0
  var_prop <- .s03allocVar(M, S, prop)
  total <- NULL
  if (!is.null(y)) {
    yv <- .s03vec(y)
    if (length(yv) != H) stop("neyman_allocation: y and N_h have different lengths")
    total <- 0
    for (i in seq_len(H)) total <- total + M[i] * yv[i]
  }
  list(estimate = var, allocation = alloc,
       allocation_int = .s03allocInt(alloc, m0), variance = var,
       variance_prop = var_prop, A = ab$A, B = B, C = ab$C, total = total,
       n = H,
       method = paste0("Neyman (1934) optimum allocation ",
                       "m_i = m_0 M_i S_i / sum(M_j S_j), eq. (39) p. 580"))
}

# Shared by Neyman, Neymal and Propal.  Coerce and validate the common
# stratum arguments; S is all ones when S_h is NULL, which turns Neyman
# allocation into proportional allocation.
#' Shared by Neyman, Neymal and Propal.  Coerce and validate the common
#'
#' stratum arguments; S is all ones when S_h is NULL, which turns Neyman
#' allocation into proportional allocation.
#'
#' @param N_h Passed to \code{.s03vec}.
#' @param S_h Optional; may be \code{NULL}. Passed to \code{.s03vec}.
#' @param n Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{M}, \code{S}, \code{m0}, \code{H}.
#' @export
.s03allocCheck <- function(N_h, S_h, n) {
  M <- .s03vec(N_h)
  H <- length(M)
  if (H == 0L) stop("neyman_allocation: no strata")
  for (v in M) {
    if (is.na(v) || !(v > 0)) {
      stop("neyman_allocation: every stratum size N_h must be positive")
    }
  }
  if (is.null(S_h)) {
    S <- rep(1, H)
  } else {
    S <- .s03vec(S_h)
    if (length(S) != H) stop("neyman_allocation: N_h and S_h have different lengths")
    for (v in S) {
      if (is.na(v) || v < 0) {
        stop("neyman_allocation: every stratum standard deviation S_h must be >= 0")
      }
    }
  }
  m0 <- as.numeric(n)
  if (is.na(m0) || !(m0 > 0)) {
    stop("neyman_allocation: the total sample size n must be positive")
  }
  list(M = M, S = S, m0 = m0, H = H)
}

# Round a real allocation to integers summing to round(m0).  Deterministic:
# the units left over after flooring go to the largest fractional parts,
# ties broken by the lower stratum index, so both language arms land on the
# same integers.
#' Round a real allocation to integers summing to round(m0).
#' Deterministic:
#'
#' the units left over after flooring go to the largest fractional
#' parts, ties broken by the lower stratum index, so both language arms
#' land on the same integers.
#'
#' @param alloc A vector; its length is taken.
#' @param m0 See Usage.
#' @return The value of \code{base}, as built in the body.
#' @export
.s03allocInt <- function(alloc, m0) {
  H <- length(alloc)
  base <- floor(alloc)
  rem <- round(m0) - sum(base)
  if (rem <= 0) return(base)
  ord <- order(-(alloc - base), seq_len(H))
  for (k in seq_len(min(rem, H))) base[ord[k]] <- base[ord[k]] + 1
  base
}

# Neyman (1934) eq. (37), p. 579, with sigma_i^2 eliminated:
# sigma^2 = sum_i M_i (M_i - m_i) S_i^2 / m_i.
#' Neyman (1934) eq. (37), p. 579, with sigma_i^2 eliminated:
#'
#' sigma^2 = sum_i M_i (M_i - m_i) S_i^2 / m_i.
#'
#' @param M A vector; its length is taken and its elements indexed.
#' @param S A vector; indexed elementwise.
#' @param m A vector; indexed elementwise.
#' @return The value of \code{tot}, as built in the body.
#' @export
.s03allocVar <- function(M, S, m) {
  tot <- 0
  for (i in seq_along(M)) {
    if (m[i] <= 0) return(Inf)
    tot <- tot + M[i] * (M[i] - m[i]) * S[i] * S[i] / m[i]
  }
  tot
}

# The A and C pieces of Neyman (1934) eq. (39), p. 580, together with M_0
# and T = sum_j M_j S_j.  B is omitted because it depends on the allocation
# rather than on the population; the callers add it themselves.
#' The A and C pieces of Neyman (1934) eq. (39), p. 580, together with
#' M_0
#'
#' and T = sum_j M_j S_j.  B is omitted because it depends on the
#' allocation rather than on the population; the callers add it
#' themselves.
#'
#' @param M A vector; its length is taken and its elements indexed.
#' @param S A vector; indexed elementwise.
#' @param m0 Numeric; combined arithmetically in the body.
#' @return A list with \code{A}, \code{C}, \code{M0}, \code{T}.
#' @export
.s03allocABC <- function(M, S, m0) {
  M0 <- 0
  for (v in M) M0 <- M0 + v
  Tt <- 0
  for (i in seq_along(M)) Tt <- Tt + M[i] * S[i]
  a <- 0
  for (i in seq_along(M)) a <- a + M[i] * S[i] * S[i]
  A <- (M0 - m0) / m0 * a
  cc <- 0
  for (i in seq_along(M)) {
    d <- S[i] - Tt / M0
    cc <- cc + M[i] * d * d
  }
  C <- M0 / m0 * cc
  list(A = A, C = C, M0 = M0, T = Tt)
}
