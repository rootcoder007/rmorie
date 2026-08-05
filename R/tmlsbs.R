# SPDX-License-Identifier: AGPL-3.0-or-later
#' Sample-split TMLE on a data-adaptively selected covariate subset
#'
#' Selecting the adjustment set and estimating on the same rows makes the
#' target parameter itself a function of the data, and the usual
#' influence-curve SE then prices the estimator but not the selection.
#' The data-adaptive target-parameter framework fixes it by splitting:
#' the subset is chosen on the ODD indices, the parameter is defined and
#' estimated on the EVEN indices, and given the split the target is a
#' fixed (if random) parameter whose influence curve needs no selection
#' correction.
#'
#' Selection ranks covariates by the absolute correlation with the
#' treatment-residualised outcome on the selection half and keeps the top
#' \code{ceiling(p/2)}.  The reported \code{se} is a HALF-sample standard
#' error; it does not shrink by using the selection rows, and that is the
#' price of an honest interval.
#'
#' @param y Outcome.
#' @param D Binary treatment.
#' @param X Candidate covariates.
#' @return List with \code{estimate}, \code{se}, \code{eps},
#'   \code{selected} (1-based column indices kept),
#'   \code{n_selected}, \code{n_est}, \code{n}.
#' @references Hubbard, A. E., Kherad-Pajouh, S. & van der Laan, M. J.
#'   (2016). International Journal of Biostatistics 12(1):3-19.
#' @export
Tmlsbs <- function(y, D, X) {
  yv <- as.numeric(y); Dv <- as.numeric(D); n <- length(yv)
  if (n < 8L || length(Dv) != n)
    stop("Tmlsbs: y and D must share one length >= 8")
  Xm <- as.matrix(X)
  if (nrow(Xm) != n) stop("Tmlsbs: X must have one row per subject")
  p <- ncol(Xm)
  idx <- seq_len(n) - 1L
  sel <- which(idx %% 2L == 1L); est <- which(idx %% 2L == 0L)
  fit <- .s4_ols(cbind(1, Dv[sel]), yv[sel])
  res <- fit$resid
  score <- numeric(p)
  for (j in seq_len(p)) {
    col <- Xm[sel, j]
    mc <- mean(col); mr <- mean(res)
    num <- sum((col - mc) * (res - mr))
    dc <- sqrt(sum((col - mc)^2)); dr <- sqrt(sum((res - mr)^2))
    score[j] <- if (dc > 0 && dr > 0) abs(num) / (dc * dr) else 0
  }
  k <- (p + 1L) %/% 2L
  keep <- sort(order(-score, seq_len(p))[seq_len(k)])
  W <- cbind(1, Xm[est, keep, drop = FALSE])
  out <- .s4_tmle(yv[est], Dv[est], W)
  .t1_result(estimate = out$psi, se = out$se, eps = out$eps,
             selected = keep,
             n_selected = length(keep), n_est = length(est), n = n,
             method = "Sample-split TMLE on a data-adaptively selected covariate subset")
}
