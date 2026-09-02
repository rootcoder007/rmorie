# SPDX-License-Identifier: AGPL-3.0-or-later
#' Empirical characteristic function of W, the deconvolution sample analogue
#'
#' Horowitz, J. L. (2009), Semiparametric and Nonparametric Methods in
#' Econometrics, Springer, Section 5.1, page 137 (volume \[Pages 135-188\], read
#' as a rendered page image).  In the measurement-error model W = U + epsilon
#' of equation (5.6), the inversion formula (5.7) is
#' f_U(u) = (1 / 2 pi) integral exp(-i tau u) psi_W(tau) / psi_eps(tau) d tau,
#' and the only unknown on the right is psi_W, replaced by the empirical
#' characteristic function printed unnumbered immediately below (5.7),
#' psi_nW(tau) = (1 / n) sum_{j=1}^n exp(i tau W_j).  The book uses j to index
#' observations because i is the imaginary unit; the same convention is kept
#' here.  Only the empirical characteristic function itself is computed: the
#' book notes on the same page that substituting psi_nW directly into (5.7)
#' generally gives a divergent integral, which is why the smoothed estimators
#' of Section 5.1.1 exist.
#'
#' @param w The sample of W.
#' @param tau Frequencies at which to evaluate psi_nW.
#' @return list: estimate, characteristic_function, re, im, modulus, argument,
#'   tau, n, method.
#' @keywords internal
#' @examples
#' Hrzecfw(c(0, 1, 2), c(0, 0.5, 1))$modulus
#' @export
Hrzecfw <- function(w, tau) {
  ww <- .s03vec(w)
  tt <- .s03vec(tau)
  n <- length(ww)
  if (n == 0L) stop("horowitz_empirical_cf: w is empty")
  if (length(tt) == 0L) stop("horowitz_empirical_cf: tau is empty")
  nt <- length(tt)
  re <- numeric(nt)
  im <- numeric(nt)
  md <- numeric(nt)
  ag <- numeric(nt)
  cf <- vector("list", nt)
  for (k in seq_len(nt)) {
    a <- 0
    b <- 0
    for (j in seq_len(n)) {
      a <- a + cos(tt[k] * ww[j])
      b <- b + sin(tt[k] * ww[j])
    }
    a <- a / n
    b <- b / n
    re[k] <- a
    im[k] <- b
    md[k] <- sqrt(a * a + b * b)
    ag[k] <- atan2(b, a)
    cf[[k]] <- c(a, b)
  }
  list(estimate = md[1], characteristic_function = cf, re = re, im = im,
       modulus = md, argument = ag, tau = tt, n = n,
       method = paste0("Horowitz (2009) Section 5.1 p.137, ",
                       "psi_nW(tau) = n^-1 sum exp(i tau W_j)"))
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Hrzecfw
#' @keywords internal
#' @export
morie_horowitz_empirical_cf <- Hrzecfw
