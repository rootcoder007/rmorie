# SPDX-License-Identifier: AGPL-3.0-or-later
#' Treatment effect on a composition, estimated in clr coordinates
#'
#' The parts sum to a constant, so an effect read off the parts is
#' confounded by the closure. Estimating on clr coordinates removes the
#' constraint; the returned effects sum to zero for the same reason. Each
#' coordinate is targeted separately with the shared propensity.
#'
#' Formula: Z = clr(Y); for each coordinate j target
#'   psi_j = E\[Z_j(1)\] - E\[Z_j(0)\]; sum_j psi_j = 0;
#'   the perturbation on the simplex is C(exp(psi))
#'
#' @param Yc Compositional outcome, one composition per row.
#' @param A Binary treatment.
#' @param Q1,Q0 Initial E\[clr(Y)_j | A = 1/0, W\].
#' @param g1W Initial propensity.
#' @param gbound Propensity truncation.
#' @param level Confidence level.
#' @return List with \code{effect}, \code{se}, \code{ci_lower},
#'   \code{ci_upper}, \code{sum_effect}, \code{perturbation}, \code{n},
#'   \code{D}.
#' @references Targeting machinery verified against the CRAN package tmle
#'   2.1.1 (Gruber & van der Laan); the bounded-continuous-outcome
#'   transform is Gruber & van der Laan (2010), International Journal of
#'   Biostatistics 6(1), Article 26. The compositional geometry is
#'   Aitchison (1986), Chapters 2 and 4, matching the sibling modules
#'   aitclr and aitprt. No source combining the two was found; the
#'   combination is documented here as this package's own.
#' @export
Comptml <- function(Yc, A, Q1, Q0, g1W, gbound = 0.025, level = 0.95) {
  Yc <- as.matrix(Yc); n <- nrow(Yc); D <- ncol(Yc)
  if (n < 2L) stop("at least two observations are required")
  if (D < 2L) stop("a composition needs at least two parts")
  if (any(Yc <= 0)) stop("compositions must be strictly positive")
  A <- .t1_vec(A); Q1 <- as.matrix(Q1); Q0 <- as.matrix(Q0)
  g1W <- .t1_vec(g1W)
  if (length(A) != n || length(g1W) != n || nrow(Q1) != n || nrow(Q0) != n)
    stop("every argument must have one entry per observation")
  if (ncol(Q1) != D || ncol(Q0) != D)
    stop("Q1 and Q0 must have one column per part")
  if (any(!(A %in% c(0, 1)))) stop("A must be binary 0/1")
  L <- log(Yc)
  Z <- L - rowSums(L) / D
  z <- stats::qnorm((1 + level) / 2)
  eff <- ses <- lo <- hi <- numeric(D)
  for (j in seq_len(D)) {
    col <- Z[, j]
    a <- min(c(col, Q1[, j], Q0[, j])); b <- max(c(col, Q1[, j], Q0[, j]))
    rng <- b - a
    if (rng <= 0) stop("a clr coordinate is constant; no effect to target")
    Ys <- (col - a) / rng
    q1 <- .b1_bound((Q1[, j] - a) / rng, 1e-6, 1 - 1e-6)
    q0 <- .b1_bound((Q0[, j] - a) / rng, 1e-6, 1 - 1e-6)
    qa <- ifelse(A == 1, q1, q0)
    fit <- .b1_target(Ys, A, qa, q1, q0, g1W, gbound)
    cv <- .b1_curves(Ys, A, fit)
    ic <- (cv$ic1 - cv$ic0) * rng
    e <- (cv$mu1 - cv$mu0) * rng
    s <- sqrt(stats::var(ic) / n)
    eff[j] <- e; ses[j] <- s; lo[j] <- e - z * s; hi[j] <- e + z * s
  }
  mean_e <- mean(eff)
  eff <- eff - mean_e
  ex <- exp(eff)
  .t1_result(effect = eff, se = ses, ci_lower = lo - mean_e,
             ci_upper = hi - mean_e, sum_effect = sum(eff),
             perturbation = ex / sum(ex), n = as.numeric(n),
             D = as.numeric(D),
             method = "TMLE on clr coordinates of a compositional outcome")
}
