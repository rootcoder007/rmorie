# SPDX-License-Identifier: AGPL-3.0-or-later
#' Specific and joint natural indirect effects through parallel mediators
#'
#' Each mediator is regressed on the exposure,
#' \code{M_k = alpha_0k + a_k X + ...}, and the outcome on the exposure
#' and ALL mediators at once,
#' \eqn{Y = beta_0 + c prime X + sum_k b_k M_k + ...}. The specific indirect
#' effect through mediator \code{k} is \code{a_k b_k} and the joint
#' indirect effect is \code{sum_k a_k b_k}.
#'
#' Fitting the mediators one at a time and the outcome on all of them is
#' what makes these "parallel": no mediator is allowed to cause another.
#' Daniel et al. (2015) is the reference for what that assumption buys
#' and costs -- with mediator-mediator causation the \code{a_k b_k}
#' products are no longer the natural indirect effects and the sum no
#' longer decomposes the total effect. This routine computes the
#' parallel-model quantities; it does not test the assumption.
#'
#' @param Y Outcome, length n.
#' @param X Exposure, length n.
#' @param M_list Mediators, n by k.
#' @param C Optional baseline covariates.
#' @return List with indirect (per mediator), indirect_total, direct,
#'   total, a, b, k, n.
#' @references Daniel, De Stavola, Cousens and Vansteelandt (2015),
#'   Biometrics 71(1), 1-14, \doi{10.1111/biom.12248}, verified against
#'   Crossref; VanderWeele (2015), Explanation in Causal Inference, OUP,
#'   ch. 5. Neither source was in the local corpus; the
#'   parallel-mediator products above are the standard published form.
#' @export
#' @examples
#' MultM(Y = c(1, 2, 3, 4, 5, 6, 7, 8), X = c(1, 2, 3, 4, 5, 6, 7, 8), M_list = c(1, 2, 3, 4, 5, 6, 7, 8))
MultM <- function(Y, X, M_list, C = NULL) {
  y <- .t1_vec(Y)
  x <- .t1_vec(X)
  n <- length(y)
  M <- as.matrix(M_list)
  if (nrow(M) != n) M <- t(M)
  if (nrow(M) != n || length(x) != n) {
    stop("x, M, y must share their first dimension.")
  }
  k <- ncol(M)
  Cm <- if (is.null(C)) matrix(numeric(0), n, 0) else {
    tmp <- as.matrix(C)
    if (nrow(tmp) != n) tmp <- t(tmp)
    if (nrow(tmp) != n) stop("c has the wrong number of rows")
    tmp
  }
  if (n < k + ncol(Cm) + 4) {
    stop("too few observations for the mediator and outcome regressions.")
  }
  Dx <- cbind(1, x, Cm)
  a <- vapply(seq_len(k), function(j) .t1_lstsq(Dx, M[, j])$beta[2], numeric(1))
  by <- .t1_lstsq(cbind(1, x, M, Cm), y)$beta
  cprime <- by[2]
  b <- by[3:(2 + k)]
  ind <- a * b
  .t1_result(indirect = ind, indirect_total = sum(ind), direct = cprime,
             total = cprime + sum(ind), a = a, b = b, k = k, n = n,
             method = "Parallel multiple mediators (specific and joint NIE)")
}
