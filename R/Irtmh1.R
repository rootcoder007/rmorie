# SPDX-License-Identifier: AGPL-3.0-or-later
#' Mantel-Haenszel differential item functioning
#'
#' Examinees are stratified on the matching total score; each stratum
#' gives a 2 x 2 table of group by correctness and the common odds ratio
#' pools them.  The MH chi-square uses the hypergeometric mean and
#' variance of the reference-correct cell with the continuity
#' correction, and the ETS delta scale rescales the log odds ratio.
#'
#' Formula: alpha_MH = sum_m A_m D_m / n_m / sum_m B_m C_m / n_m;
#'   Delta = -2.35 log(alpha_MH).
#'
#' @param X Matrix of 0/1 item responses, one row per examinee.
#' @param group Length-n vector, 0 reference and 1 focal.
#' @param total_score Length-n matching variable forming the strata.
#' @param alpha Significance level for the MH chi-square.
#' @return List with \code{estimate} (largest |Delta|),
#'   \code{odds_ratio}, \code{chisq}, \code{p_value}, \code{delta},
#'   \code{ets_class}, \code{flagged}, \code{n}, \code{method}.
#' @references Holland and Thayer (1988), Differential item performance
#'   and the Mantel-Haenszel procedure, in Wainer and Braun (eds), Test
#'   Validity, Lawrence Erlbaum, pp. 129-145.
#' @export
#' @examples
#' set.seed(1)
#' r <- Irtmh1(X = rnorm(10), group = rbinom(10, 1, 0.5), total_score = rnorm(10)); TRUE
Irtmh1 <- function(X, group, total_score, alpha = 0.05) {
  M <- .s03mat(X)
  n <- nrow(M)
  if (n == 0L) stop("dif_mantel_haenszel: X is empty")
  J <- ncol(M)
  g <- as.integer(.s03vec(group))
  s <- .s03vec(total_score)
  if (length(g) != n || length(s) != n) stop("dif_mantel_haenszel: group and total_score must match X")
  if (any(!(g %in% c(0L, 1L)))) stop("dif_mantel_haenszel: group must be coded 0/1")
  strata <- sort(unique(s))
  ors <- numeric(J)
  chis <- numeric(J)
  ps <- numeric(J)
  deltas <- numeric(J)
  cls <- character(J)
  flags <- integer(J)
  for (j in seq_len(J)) {
    x <- as.integer(M[, j])
    if (any(!(x %in% c(0L, 1L)))) stop("dif_mantel_haenszel: responses must be 0/1")
    num <- 0
    den <- 0
    ea <- 0
    va <- 0
    obs <- 0
    for (lev in strata) {
      idx <- which(s == lev)
      if (length(idx) == 0L) next
      A <- sum(g[idx] == 0L & x[idx] == 1L)
      B <- sum(g[idx] == 0L & x[idx] == 0L)
      C <- sum(g[idx] == 1L & x[idx] == 1L)
      D <- sum(g[idx] == 1L & x[idx] == 0L)
      nn <- A + B + C + D
      if (nn <= 1) next
      nR <- A + B
      nF <- C + D
      n1 <- A + C
      n0 <- B + D
      if (nR == 0 || nF == 0 || n1 == 0 || n0 == 0) next
      num <- num + A * D / nn
      den <- den + B * C / nn
      obs <- obs + A
      ea <- ea + nR * n1 / nn
      va <- va + nR * nF * n1 * n0 / (nn * nn * (nn - 1))
    }
    if (den <= 0 || num <= 0) {
      ors[j] <- NaN
      chis[j] <- NaN
      ps[j] <- NaN
      deltas[j] <- NaN
    } else {
      ors[j] <- num / den
      deltas[j] <- -2.35 * log(ors[j])
      if (va <= 0) { chis[j] <- NaN
      ps[j] <- NaN } else {
        chis[j] <- (abs(obs - ea) - 0.5)^2 / va
        ps[j] <- 2 * (1 - .s03pnorm(sqrt(chis[j])))
      }
    }
    sig <- !is.na(ps[j]) && ps[j] < alpha
    d <- deltas[j]
    cls[j] <- if (is.na(d) || abs(d) < 1 || !sig) "A" else if (abs(d) >= 1.5 && sig) "C" else "B"
    flags[j] <- if (cls[j] == "A") 0L else 1L
  }
  fin <- deltas[!is.na(deltas)]
  .t1_result(estimate = if (length(fin)) max(abs(fin)) else NaN,
             odds_ratio = ors, chisq = chis, p_value = ps, delta = deltas,
             ets_class = cls, flagged = flags, n = n,
             method = "MH common odds ratio with ETS delta, Holland & Thayer (1988)")
}
