# SPDX-License-Identifier: AGPL-3.0-or-later

#' Bias correction for exposure misclassification
#'
#' Formula: under misclassification with sensitivity Se and specificity
#' Sp, the expected observed exposed count in a group of size N whose
#' true exposed count is A is E\[A_obs\] = Se A + (1 - Sp) (N - A), so the
#' matrix-corrected true count inverts it,
#' A = (A_obs - (1 - Sp) N) / (Se + Sp - 1).  This is the "divide by
#' (Se + Sp - 1)" rule written out with its offset term.  It is undefined
#' when Se + Sp = 1 and inadmissible when Se + Sp < 1.
#'
#' With exactly two groups -- cases first, then controls -- the corrected
#' and uncorrected odds ratios are both reported.
#'
#' @param A_obs Observed exposed count in each group.
#' @param Se Sensitivity, scalar or one per group, in (0, 1].
#' @param Sp Specificity, scalar or one per group, in (0, 1].
#' @param N Group totals; if omitted, \code{A_obs} must be length 2 and
#'   is read as (exposed, unexposed) of a single group.
#' @return List with \code{estimate}, \code{a_true}, \code{a_obs},
#'   \code{totals}, \code{prevalence}, \code{or_obs}, \code{or_true},
#'   \code{sensitivity}, \code{specificity}, \code{n}, \code{method}.
#' @references Greenland (1988), Statistics in Medicine 7(7):745-757,
#'   doi:10.1002/sim.4780070704.
#' @export
#' @examples
#' Epbias(A_obs = c(50, 30), Se = 0.9, Sp = 0.85, N = c(100, 100))
Epbias <- function(A_obs, Se, Sp, N = NULL) {
  a <- as.numeric(A_obs)
  if (is.null(N)) {
    if (length(a) != 2L)
      stop("N is required unless A_obs is (exposed, unexposed)")
    tot <- a[1] + a[2]
    a <- a[1]
  } else {
    tot <- as.numeric(N)
    if (length(tot) != length(a)) stop("A_obs and N must have the same length")
  }
  g <- length(a)
  if (g == 0L) stop("empty input: A_obs has no groups")
  se <- as.numeric(Se)
  sp <- as.numeric(Sp)
  if (length(se) == 1L) se <- rep(se, g)
  if (length(sp) == 1L) sp <- rep(sp, g)
  if (length(se) != g || length(sp) != g)
    stop("Se and Sp must be scalars or one value per group")
  for (i in seq_len(g)) {
    if (!(se[i] > 0 && se[i] <= 1) || !(sp[i] > 0 && sp[i] <= 1))
      stop("Se and Sp must lie in (0, 1]")
    if (se[i] + sp[i] <= 1)
      stop("Se + Sp must exceed 1 for the correction to invert")
    if (tot[i] <= 0) stop("group totals must be positive")
    if (a[i] < 0 || a[i] > tot[i])
      stop("A_obs must lie between 0 and the group total")
  }
  at <- (a - (1 - sp) * tot) / (se + sp - 1)
  prev <- at / tot
  .or <- function(x) {
    n0 <- tot[1] - x[1]
    n1 <- tot[2] - x[2]
    if (x[2] <= 0 || n0 <= 0 || n1 <= 0 || x[1] <= 0) NaN
    else (x[1] * n1) / (x[2] * n0)
  }
  oro <- if (g == 2L) .or(a) else NaN
  ort <- if (g == 2L) .or(at) else NaN
  .t1_result(estimate = at[1], a_true = at, a_obs = a, totals = tot,
             prevalence = prev, or_obs = oro, or_true = ort,
             sensitivity = se, specificity = sp, n = g,
             method = "Bias correction for exposure misclassification")
}
