# SPDX-License-Identifier: AGPL-3.0-or-later
#' Target E\[Y_a\] at every level of a categorical treatment
#'
#' Each level gets its OWN clever covariate 1\{A = a\}/g_a(W) and its own
#' fluctuation, because one logistic update cannot solve L score
#' equations at once. \code{min_g} exposes levels with almost no support.
#' \code{A} holds one-based level labels.
#'
#' Formula: for each level a, fluctuate logit Q(a,W) on
#'   H_a = 1\{A = a\}/g_a(W); psi_a = mean(Q*(a,W))
#'
#' @param Y Outcome in \[0, 1\].
#' @param A One-based treatment level of each observation.
#' @param Q Initial E\[Y | A = a, W\], one column per level.
#' @param G Initial P(A = a | W), rows summing to 1.
#' @param ref One-based reference level for the contrasts.
#' @param gbound Truncation applied to each g_a.
#' @param level Confidence level.
#' @return List with \code{psi}, \code{se}, \code{contrast},
#'   \code{contrast_se}, \code{ci_lower}, \code{ci_upper}, \code{min_g},
#'   \code{ref}, \code{n}, \code{L}.
#' @references Verified against the CRAN package tmle 2.1.1 (Gruber & van
#'   der Laan); the multi-level extension applies the same fluctuation per
#'   level, as in van der Laan & Rose (2011), Targeted Learning, Chapter 4.
#' @export
Tmlecat <- function(Y, A, Q, G, ref = 1, gbound = 0.025, level = 0.95) {
  Y <- .t1_vec(Y); n <- length(Y)
  A <- as.integer(.t1_vec(A))
  Qm <- as.matrix(Q); Gm <- as.matrix(G)
  if (length(A) != n || nrow(Qm) != n || nrow(Gm) != n)
    stop("every argument must have one entry per observation")
  L <- ncol(Qm)
  if (L < 2L) stop("at least two treatment levels are required")
  if (ncol(Gm) != L) stop("Q and G must have one column per level")
  if (any(A < 1L | A > L))
    stop("A must hold one-based level labels in 1..L")
  ref <- as.integer(ref)
  if (ref < 1L || ref > L) stop("ref must be a level in 1..L")
  if (any(Y < 0 | Y > 1)) stop("Y must lie in [0, 1]")
  psi <- ses <- mg <- numeric(L)
  ics <- matrix(0, n, L)
  for (a in seq_len(L)) {
    ind <- as.numeric(A == a)
    ga <- .b1_bound(Gm[, a], gbound, 1 - gbound)
    mg[a] <- min(ga)
    H <- ind / ga
    QAW <- Qm[cbind(seq_len(n), A)]
    off <- .b1_logit(QAW)
    e <- 0
    for (t in seq_len(100L)) {
      mu <- .b1_expit(off + e * H)
      gr <- sum(H * (Y - mu))
      hs <- sum(H^2 * mu * (1 - mu)) + 1e-10
      st <- gr / hs
      e <- e + st
      if (abs(st) < 1e-12) break
    }
    QAs <- .b1_expit(off + e * H)
    Qas <- .b1_expit(.b1_logit(Qm[, a]) + e / ga)
    p <- mean(Qas)
    ic <- H * (Y - QAs) + Qas - p
    psi[a] <- p; ics[, a] <- ic; ses[a] <- sqrt(stats::var(ic) / n)
  }
  z <- stats::qnorm((1 + level) / 2)
  con <- psi - psi[ref]
  cse <- lo <- hi <- numeric(L)
  for (a in seq_len(L)) {
    icd <- ics[, a] - ics[, ref]
    cse[a] <- sqrt(stats::var(icd) / n)
    lo[a] <- con[a] - z * cse[a]; hi[a] <- con[a] + z * cse[a]
  }
  .t1_result(psi = psi, se = ses, contrast = con, contrast_se = cse,
             ci_lower = lo, ci_upper = hi, min_g = mg,
             ref = as.numeric(ref), n = as.numeric(n), L = as.numeric(L),
             method = "TMLE for a categorical treatment, one fluctuation per level")
}
