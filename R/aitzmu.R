# SPDX-License-Identifier: AGPL-3.0-or-later
#' Multiplicative replacement of rounded zeros in compositions
#'
#' Martin-Fernandez, Barcelo-Vidal and Pawlowsky-Glahn (2003), "Dealing with
#' zeros and missing values in compositional data sets using nonparametric
#' imputation", Mathematical Geology 35(3), 253-278,
#' doi:10.1023/a:1023866030544 (citation verified against Crossref).
#'
#' For a composition x with constant sum kappa and zero set Z the
#' multiplicative replacement puts x'_j = delta_j for j in Z and
#' x'_j = x_j (1 - (sum_\{k in Z\} delta_k) / kappa) otherwise.  The rule
#' preserves the total exactly and leaves every ratio between two non-zero
#' parts untouched, which is what makes it a perturbation rather than an
#' additive fudge that would distort the very log-ratios it protects.  A row
#' with no zeros is returned unchanged.
#'
#' @param X one composition, or a matrix whose rows are compositions; parts
#'   must be non-negative.
#' @param delta the imputed value, one number for every zero or one per part;
#'   strictly positive and small relative to the total.
#' @return list: X_imp, estimate, n_zero, n, D, method.
#' @keywords internal
#' @examples
#' Aitzmu(c(0.4, 0, 0.6), 0.01)$X_imp
#' @export
Aitzmu <- function(X, delta) {
  was_matrix <- is.matrix(X) || is.data.frame(X)
  rows <- if (was_matrix) as.matrix(X) else matrix(as.numeric(X), nrow = 1L)
  storage.mode(rows) <- "double"
  if (length(rows) == 0L) stop("compositional_zero_multreplace: X is empty")
  D <- ncol(rows)
  if (D < 2L) stop("compositional_zero_multreplace: a composition needs at least 2 parts")
  dl <- if (length(delta) > 1L) as.numeric(delta) else rep(as.numeric(delta), D)
  if (length(dl) != D) stop("compositional_zero_multreplace: delta has the wrong length")
  if (any(!(dl > 0))) stop("compositional_zero_multreplace: delta must be strictly positive")
  out <- rows
  nz <- 0L
  for (i in seq_len(nrow(rows))) {
    r <- rows[i, ]
    if (any(r < 0)) stop("compositional_zero_multreplace: a part is negative")
    tot <- 0
    for (v in r) tot <- tot + v
    if (!(tot > 0)) stop("compositional_zero_multreplace: a row sums to zero")
    sd <- 0
    for (j in seq_len(D)) if (r[j] == 0) sd <- sd + dl[j]
    if (sd >= tot) stop("compositional_zero_multreplace: the imputed mass exceeds the total")
    f <- 1 - sd / tot
    for (j in seq_len(D)) {
      if (r[j] == 0) {
        out[i, j] <- dl[j]
        nz <- nz + 1L
      } else {
        out[i, j] <- r[j] * f
      }
    }
  }
  list(X_imp = if (was_matrix) out else as.numeric(out[1, ]), estimate = out[1, 1],
       n_zero = nz, n = nrow(rows), D = D,
       method = "Martin-Fernandez et al. (2003) multiplicative replacement")
}
