# SPDX-License-Identifier: AGPL-3.0-or-later

#' DIF p-difference per item
#'
#' Formula: p_focal - p_reference per item
#'
#' The raw difference in proportion correct.  It is not by itself
#' evidence of item bias, because it confounds impact (a real difference
#' in ability) with DIF -- which is why the ETS rules also use the
#' Mantel-Haenszel odds ratio on the delta scale, -2.35 log(alpha_MH),
#' reported here alongside.  ETS classes: A below 1, B up to 1.5, C
#' above.
#'
#' @param X An n x k matrix of 0/1 item responses.
#' @param group Group label per examinee.
#' @param focal Label of the focal group.
#' @return List with \code{estimate}, \code{p_diff}, \code{p_focal},
#'   \code{p_reference}, \code{mh_alpha}, \code{mh_delta}, \code{ets},
#'   \code{flagged}, \code{n_focal}, \code{n_reference}, \code{k},
#'   \code{method}.
#' @references Holland & Wainer (1993), Differential Item Functioning,
#'   Erlbaum; Dorans & Holland (1993), ibid., ch. 3.
#' @export
Difpst <- function(X, group, focal = 1) {
  M <- .s03mat(X)
  n <- nrow(M)
  if (n == 0L) stop("empty input: X has no rows")
  k <- ncol(M)
  g <- group
  if (length(g) != n) stop("X and group must have the same length")
  if (any(!(M %in% c(0, 1)))) stop("responses must be 0/1")
  fi <- which(g == focal)
  ri <- which(g != focal)
  if (!length(fi) || !length(ri))
    stop("both a focal and a reference group are required")
  total <- numeric(n)
  for (i in seq_len(n)) total[i] <- sum(M[i, ])
  pf <- numeric(k)
  pr <- numeric(k)
  pd <- numeric(k)
  alpha <- numeric(k)
  delta <- numeric(k)
  ets <- integer(k)
  flag <- integer(k)
  for (j in seq_len(k)) {
    a <- sum(M[fi, j]) / length(fi)
    b <- sum(M[ri, j]) / length(ri)
    pf[j] <- a
    pr[j] <- b
    pd[j] <- a - b
    num <- 0
    den <- 0
    for (s in 0:k) {
      f <- fi[total[fi] == s]
      r <- ri[total[ri] == s]
      ns <- length(f) + length(r)
      if (ns == 0L) next
      af <- sum(M[f, j])
      bf <- length(f) - af
      ar <- sum(M[r, j])
      br <- length(r) - ar
      num <- num + ar * bf / ns
      den <- den + af * br / ns
    }
    al <- if (den > 0) num / den else NaN
    alpha[j] <- al
    dl <- if (!is.nan(al) && al > 0) -2.35 * log(al) else NaN
    delta[j] <- dl
    ad <- if (is.nan(dl)) NaN else abs(dl)
    cls <- if (is.nan(ad)) 2L else if (ad < 1) 0L else if (ad <= 1.5) 1L else 2L
    ets[j] <- cls
    flag[j] <- as.integer(cls == 2L)
  }
  .t1_result(estimate = max(abs(pd)), p_diff = pd, p_focal = pf,
             p_reference = pr, mh_alpha = alpha, mh_delta = delta,
             ets = ets, flagged = flag, n_focal = length(fi),
             n_reference = length(ri), k = k,
             method = "DIF p-difference and Mantel-Haenszel delta")
}
