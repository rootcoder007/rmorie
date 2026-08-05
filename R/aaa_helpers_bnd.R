# SPDX-License-Identifier: AGPL-3.0-or-later
# Shared pieces for the Manski-style partial-identification bound family.
# Molinari, F. (2021). Microeconometrics with partial identification.
# Handbook of Econometrics 7A, 355-486 (arXiv:2004.11751), eqs (2.11), (2.13).

.bnd_yd <- function(y, D, name) {
  yv <- as.numeric(unlist(y))
  dv <- as.numeric(unlist(D))
  if (length(yv) == 0L) stop(paste0(name, ": y is empty"))
  if (length(dv) != length(yv))
    stop(paste0(name, ": y and D must have the same length"))
  if (any(dv != 0 & dv != 1)) stop(paste0(name, ": D must be coded 0/1"))
  list(y = yv, d = dv)
}

.bnd_cellmeans <- function(yv, dv) {
  n <- length(yv)
  n1 <- sum(dv == 1)
  n0 <- n - n1
  m1 <- if (n1 > 0L) sum(yv[dv == 1]) / n1 else 0
  m0 <- if (n0 > 0L) sum(yv[dv == 0]) / n0 else 0
  list(p1 = n1 / n, m1 = m1, p0 = n0 / n, m0 = m0)
}

.bnd_wc_arm <- function(m_t, p_t, lo, hi) {
  c(m_t * p_t + lo * (1 - p_t), m_t * p_t + hi * (1 - p_t))
}

.bnd_wc_ate <- function(yv, dv, lo, hi) {
  cm <- .bnd_cellmeans(yv, dv)
  a1 <- .bnd_wc_arm(cm$m1, cm$p1, lo, hi)
  a0 <- .bnd_wc_arm(cm$m0, cm$p0, lo, hi)
  c(a1[1] - a0[2], a1[2] - a0[1])
}

.bnd_q1 <- function(v, p) {
  s <- sort(v)
  m <- length(s)
  i <- ceiling(p * m)
  if (i < 1) i <- 1
  if (i > m) i <- m
  s[i]
}
