# SPDX-License-Identifier: AGPL-3.0-or-later
# Shared pieces for the Manski-style partial-identification bound family.
# Molinari, F. (2021). Microeconometrics with partial identification.
# Handbook of Econometrics 7A, 355-486 (arXiv:2004.11751), eqs (2.11), (2.13).

#' .bnd_yd
#'
#' A step of the helpers_bnd implementation. Called by \code{Bndmoq}, \code{Bndngt}, \code{Bndnpr} and 6 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Passed to \code{unlist}.
#' @param D Passed to \code{unlist}.
#' @param name Passed to \code{paste0}.
#' @return A list with \code{y}, \code{d}.
#' @export
.bnd_yd <- function(y, D, name) {
  yv <- as.numeric(unlist(y))
  dv <- as.numeric(unlist(D))
  if (length(yv) == 0L) stop(paste0(name, ": y is empty"))
  if (length(dv) != length(yv))
    stop(paste0(name, ": y and D must have the same length"))
  if (any(dv != 0 & dv != 1)) stop(paste0(name, ": D must be coded 0/1"))
  list(y = yv, d = dv)
}

#' .bnd_cellmeans
#'
#' A step of the helpers_bnd implementation. Called by \code{.bnd_wc_ate}, \code{Bndngt}, \code{Bndnvg} and 2 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param yv A vector; its length is taken and its elements indexed.
#' @param dv Passed to \code{==}.
#' @return A list with \code{p1}, \code{m1}, \code{p0}, \code{m0}.
#' @export
.bnd_cellmeans <- function(yv, dv) {
  n <- length(yv)
  n1 <- sum(dv == 1)
  n0 <- n - n1
  m1 <- if (n1 > 0L) sum(yv[dv == 1]) / n1 else 0
  m0 <- if (n0 > 0L) sum(yv[dv == 0]) / n0 else 0
  list(p1 = n1 / n, m1 = m1, p0 = n0 / n, m0 = m0)
}

#' .bnd_wc_arm
#'
#' A step of the helpers_bnd implementation. Called by \code{.bnd_wc_ate}, \code{Bndnpr}, \code{Bnssel} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param m_t Numeric; combined arithmetically in the body.
#' @param p_t Numeric; combined arithmetically in the body.
#' @param lo Numeric; combined arithmetically in the body.
#' @param hi Numeric; combined arithmetically in the body.
#' @return A vector, from \code{c}.
#' @export
.bnd_wc_arm <- function(m_t, p_t, lo, hi) {
  c(m_t * p_t + lo * (1 - p_t), m_t * p_t + hi * (1 - p_t))
}

#' .bnd_wc_ate
#'
#' A step of the helpers_bnd implementation. Called by \code{Bndtfm}, \code{Bnscbo}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param yv Passed to \code{.bnd_cellmeans}.
#' @param dv Passed to \code{.bnd_cellmeans}.
#' @param lo Passed to \code{.bnd_wc_arm}.
#' @param hi Passed to \code{.bnd_wc_arm}.
#' @return A vector, from \code{c}.
#' @export
.bnd_wc_ate <- function(yv, dv, lo, hi) {
  cm <- .bnd_cellmeans(yv, dv)
  a1 <- .bnd_wc_arm(cm$m1, cm$p1, lo, hi)
  a0 <- .bnd_wc_arm(cm$m0, cm$p0, lo, hi)
  c(a1[1] - a0[2], a1[2] - a0[1])
}

#' .bnd_q1
#'
#' A step of the helpers_bnd implementation. Called by \code{Bndmoq}, \code{Bndtfm}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v Numeric; passed to \code{sort}.
#' @param p Numeric; combined arithmetically in the body.
#' @return The value of \code{[}.
#' @export
.bnd_q1 <- function(v, p) {
  s <- sort(v)
  m <- length(s)
  i <- ceiling(p * m)
  if (i < 1) i <- 1
  if (i > m) i <- m
  s[i]
}

# Moment-inequality criterion for an interval-identified scalar.
# Molinari (2021) eqs (4.2)-(4.4): only violated inequalities contribute,
# so the criterion is exactly zero on [E yL, E yU].

#' .bnd_interval
#'
#' A step of the helpers_bnd implementation. Called by \code{Bndinf}, \code{Bnsiii}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param moments A matrix; passed to \code{as.matrix}.
#' @param name Passed to \code{paste0}.
#' @return A list with \code{yl}, \code{yu}.
#' @export
.bnd_interval <- function(moments, name) {
  M <- as.matrix(moments)
  if (nrow(M) < 2L) stop(paste0(name, ": need at least two observations"))
  if (ncol(M) != 2L)
    stop(paste0(name, ": moments must have two columns, yL and yU"))
  if (any(M[, 2] < M[, 1]))
    stop(paste0(name, ": yU is below yL at some observation"))
  list(yl = as.numeric(M[, 1]), yu = as.numeric(M[, 2]))
}

#' .bnd_mistats
#'
#' A step of the helpers_bnd implementation. Called by \code{Bndinf}, \code{Bnsiii}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param yl A vector; its length is taken.
#' @param yu Numeric; passed to \code{mean}.
#' @return A list with \code{n}, \code{mL}, \code{sL}, \code{mU}, \code{sU}.
#' @export
.bnd_mistats <- function(yl, yu) {
  sL <- stats::sd(yl)
  sU <- stats::sd(yu)
  if (sL <= 0) sL <- 1e-12
  if (sU <= 0) sU <- 1e-12
  list(n = length(yl), mL = mean(yl), sL = sL, mU = mean(yu), sU = sU)
}

#' .bnd_crit
#'
#' A step of the helpers_bnd implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param theta Numeric; combined arithmetically in the body.
#' @param st A list; the body reads \code{$mL}, \code{$mU}, \code{$n}, \code{$sL}, \code{$sU} from it.
#' @return A numeric value.
#' @export
.bnd_crit <- function(theta, st) {
  rn <- sqrt(st$n)
  a <- max(rn * (st$mL - theta) / st$sL, 0)
  b <- max(rn * (theta - st$mU) / st$sU, 0)
  a * a + b * b
}

#' .bnd_critmax
#'
#' A step of the helpers_bnd implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param theta Numeric; combined arithmetically in the body.
#' @param st A list; the body reads \code{$mL}, \code{$mU}, \code{$n}, \code{$sL}, \code{$sU} from it.
#' @return A numeric value.
#' @export
.bnd_critmax <- function(theta, st) {
  rn <- sqrt(st$n)
  max(rn * (st$mL - theta) / st$sL, rn * (theta - st$mU) / st$sU, 0)
}
