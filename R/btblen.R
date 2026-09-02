# SPDX-License-Identifier: AGPL-3.0-or-later
#' Automatic block-length selection via flat-top lag-windows
#'
#' Politis, D. N. and White, H. (2004), "Automatic Block-Length Selection for
#' the Dependent Bootstrap", Econometric Reviews 23(1), 53-70.  Every formula
#' below was read from rendered images of the journal PDF, article pages 58,
#' 59 and 60, because that PDF's text layer drops minus signs.
#'
#' The stub this replaces was labelled "Politis-Romano (2009)" with the form
#' ell = round(C n^(1/5)).  The automatic, spectral-density-based selector the
#' label points at is Politis and White (2004); the n^(1/5) rate belongs to
#' distribution-function estimation, whereas the selector below is the
#' MSE-optimal one for the variance, whose rate is n^(1/3).  The paper is
#' cited, the rate is the paper's, and the discrepancy is recorded here rather
#' than papered over.
#'
#' Page 58, the flat-top lag-window of Politis and Romano (1995):
#' lambda(t) = 1 for |t| in \[0, 1/2\]; 2(1 - |t|) for |t| in \[1/2, 1\]; 0
#' otherwise, with R_hat(k) = N^-1 sum_\{i=1\}^\{N-|k|\} (X_i - Xbar)(X_\{i+|k|\} -
#' Xbar); equation (8): G_hat = sum_\{k=-M\}^\{M\} lambda(k/M) |k| R_hat(k),
#' g_hat(w) = sum_\{k=-M\}^\{M\} lambda(k/M) R_hat(k) cos(wk), and
#' D_hat_SB = 4 g_hat^2(0) + (2/pi) int_\{-pi\}^\{pi\} (1 + cos w) g_hat^2(w) dw;
#' equation (9): b_opt_SB = (2 G_hat^2 / D_hat_SB)^(1/3) N^(1/3); page 60
#' equation (13): D_hat_CB = (4/3) g_hat^2(0); equation (14):
#' b_opt_CB = \[ (2 G_hat^2 / D_hat_CB)^(1/3) N^(1/3) \] with \[x\] the nearest
#' integer.  Page 62 notes the moving-block optimum equals the circular one,
#' so ell serves both.
#'
#' The bandwidth M is chosen by the correlogram rule of the footnote to
#' section 3.2 (page 59): m_hat is the smallest positive integer with
#' |rho_hat(m_hat + k)| < c sqrt(log10(N)/N) for k = 1, ..., K_N, with the
#' paper's recommended c = 2 and K_N = max(5, sqrt(log10 N)), and M = 2 m_hat.
#' The integral is evaluated on a fixed 2000-panel trapezoid grid over
#' \[-pi, pi\] so both language arms produce identical numbers; the integrand is
#' a smooth trigonometric polynomial of degree 2M + 1, for which that grid is
#' far finer than needed.
#'
#' Anchor: for white noise R(k) = 0 for k != 0, so G_hat = 0 and both optimal
#' block lengths collapse to 1 -- the iid bootstrap, correct for independent
#' data.
#'
#' @param x the series, in time order.
#' @param method "circular", "stationary" or "moving"; which optimum ell
#'   reports.  "moving" equals "circular" (Politis and White 2004, p.62).
#' @param c constant in the correlogram cutoff; the paper recommends 2.
#' @param m_max cap on m_hat; NULL uses ceiling(sqrt(n)) + K_N.
#' @return list: ell, b_sb, b_cb, G_hat, g0, D_sb, D_cb, m_hat, M, n,
#'   estimate, method.
#' @keywords internal
#' @examples
#' Btblen(as.numeric(filter(rnorm(200), 0.5, "recursive")))$ell
#' @export
Btblen <- function(x, method = "circular", c = 2, m_max = NULL) {
  xx <- .s03vec(x)
  n <- length(xx)
  if (n < 4L) stop("boot_block_length_pr: need at least four observations")
  if (!(method %in% c("circular", "stationary", "moving")))
    stop("boot_block_length_pr: method must be circular, stationary or moving")
  cc <- as.numeric(c)
  if (cc <= 0) stop("boot_block_length_pr: c must be positive")
  xb <- .s03mean(xx)
  kmax <- n - 1L
  R <- numeric(kmax + 1L)
  for (k in 0:kmax) {
    s <- 0
    if (n - k >= 1L) for (i in seq_len(n - k)) s <- s + (xx[i] - xb) * (xx[i + k] - xb)
    R[k + 1L] <- s / n
  }
  if (R[1] <= 0) stop("boot_block_length_pr: the series has zero variance")
  KN <- as.integer(max(5, sqrt(log10(n))))
  thr <- cc * sqrt(log10(n) / n)
  if (is.null(m_max)) m_max <- as.integer(ceiling(sqrt(n))) + KN
  m_max <- as.integer(m_max)
  mhat <- 1L; found <- FALSE
  for (m in seq_len(min(m_max, kmax))) {
    okm <- TRUE
    for (kk in seq_len(KN)) {
      idx <- m + kk
      if (idx > kmax) break
      if (abs(R[idx + 1L] / R[1]) >= thr) { okm <- FALSE; break }
    }
    if (okm) { mhat <- m; found <- TRUE; break }
  }
  if (!found) mhat <- min(m_max, kmax)
  M <- 2L * as.integer(mhat)
  if (M > kmax) M <- kmax
  G <- 0
  for (k in (-M):M) G <- G + .btblen_lam(k / M) * abs(k) * R[abs(k) + 1L]
  ghat <- function(w) {
    s <- 0
    for (k in (-M):M) s <- s + .btblen_lam(k / M) * R[abs(k) + 1L] * cos(w * k)
    s
  }
  g0 <- ghat(0)
  NP <- 2000L
  h <- 2 * pi / NP
  acc <- 0
  for (i in 0:NP) {
    w <- -pi + i * h
    gv <- ghat(w)
    v <- (1 + cos(w)) * gv * gv
    acc <- acc + v * (if (i == 0L || i == NP) 0.5 else 1)
  }
  integ <- acc * h
  D_sb <- 4 * g0 * g0 + (2 / pi) * integ
  D_cb <- (4 / 3) * g0 * g0
  bopt <- function(D) if (G == 0 || D <= 0) 1 else (2 * G * G / D)^(1 / 3) * n^(1 / 3)
  b_sb <- bopt(D_sb); b_cb <- bopt(D_cb)
  b <- if (identical(method, "stationary")) b_sb else b_cb
  ell <- as.integer(floor(b + 0.5))
  if (ell < 1L) ell <- 1L
  if (ell > n) ell <- as.integer(n)
  list(ell = ell, b_sb = b_sb, b_cb = b_cb, G_hat = G, g0 = g0,
       D_sb = D_sb, D_cb = D_cb, m_hat = as.integer(mhat), M = M, n = n,
       estimate = as.numeric(ell),
       method = "Politis and White (2004) Econometric Reviews 23(1):53-70, eqs. (8), (9), (13), (14)")
}

#' @noRd
.btblen_lam <- function(t) {
  at <- abs(t)
  if (at <= 0.5) 1 else if (at <= 1) 2 * (1 - at) else 0
}
