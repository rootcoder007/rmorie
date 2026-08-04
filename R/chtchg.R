# SPDX-License-Identifier: AGPL-3.0-or-later
#' Doubly robust ATT in a changeover (cross-over) design
#'
#' Jones and Kenward (2014), Design and Analysis of Cross-Over Trials, 3rd
#' ed., chapter 2: in a two-period two-sequence cross-over each unit is
#' observed under both conditions, so the within-unit contrast removes
#' every time-invariant unit effect, and the sequence-averaged contrast
#' (dbar_AB - dbar_BA)/2 removes any common period effect.  The book was
#' not available here as a full text; both expressions are the standard
#' published form of the AB/BA analysis.  Covariate adjustment uses the
#' doubly robust moment of Sant'Anna and Zhao (2020), Journal of
#' Econometrics 219(1), 101-122 (arXiv:1812.01723 -- FETCHED), eq. (2.6),
#' with the sequence indicator as D and the within-unit contrast as dY.
#' Carryover is not assumed away: the difference of sequence means, which
#' is the carryover contrast under the standard model, is returned.
#'
#' @param y outcome in long format.
#' @param D treatment indicator for that unit-period.
#' @param period period identifier (two levels).
#' @param unit unit identifier.
#' @param X baseline covariates, one row per unit-period.
#' @return list: estimate, tau_naive, carryover, se, n_units, method.
#' @keywords internal
#' @examples
#' Drchange(c(5, 3, 4, 6), c(1, 0, 0, 1), c(1, 2, 1, 2),
#'          c("a", "a", "b", "b"))$tau_naive
#' @export
Drchange <- function(y, D, period = NULL, unit = NULL, X = NULL) {
  yv <- .s03vec(y); d <- .s03vec(D)
  p <- as.numeric(period); u <- as.character(unit)
  Xr <- if (!is.null(X)) .s03mat(X) else NULL
  pers <- sort(unique(p))
  units <- character(0)
  for (x in u) if (!(x %in% units)) units <- c(units, x)
  contrast <- numeric(0); seq_ <- numeric(0); xs <- list()
  for (uu in units) {
    idx <- which(u == uu)
    if (length(idx) < 2L) next
    i0 <- NA_integer_; i1 <- NA_integer_
    for (i in idx) {
      if (p[i] == pers[1]) i0 <- i else if (p[i] == pers[2]) i1 <- i
    }
    if (is.na(i0) || is.na(i1)) next
    if (d[i0] > d[i1]) {
      contrast <- c(contrast, yv[i0] - yv[i1]); seq_ <- c(seq_, 1)
    } else {
      contrast <- c(contrast, yv[i1] - yv[i0]); seq_ <- c(seq_, 0)
    }
    if (!is.null(Xr)) xs[[length(xs) + 1L]] <- Xr[i0, ]
  }
  m1 <- contrast[seq_ > 0.5]; m0 <- contrast[seq_ < 0.5]
  naive <- 0.5 * ((if (length(m1)) .s03mean(m1) else 0) +
                    (if (length(m0)) .s03mean(m0) else 0))
  carry <- (if (length(m1)) .s03mean(m1) else NaN) -
    (if (length(m0)) .s03mean(m0) else NaN)
  if (length(m1) && length(m0) && length(contrast) >= 3L) {
    fit <- .s03drdid(contrast, seq_, if (!is.null(Xr)) do.call(rbind, xs) else NULL)
    est <- fit$tau; se <- fit$se
  } else {
    est <- naive; se <- NaN
  }
  list(estimate = est, tau_naive = naive, carryover = carry, se = se,
       n_units = length(contrast),
       method = "Two-period cross-over ATT with a doubly robust covariate adjustment")
}
