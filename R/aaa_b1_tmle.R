# SPDX-License-Identifier: AGPL-3.0-or-later
#' Shared targeted-maximum-likelihood machinery for the big1/s05 batch
#'
#' Internal only. Mirrors \code{morie.fn._b1tmle} on the Python side: one
#' targeting step and one influence curve, used by every tmle* module in
#' this batch rather than copied into each.
#'
#' @name b1_tmle
#' @keywords internal
NULL

#' .b1_bound
#'
#' A step of the b1_tmle implementation. Called by \code{.b1_logit}, \code{.b1_target}, \code{Comptml} and 3 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v See Usage.
#' @param lo See Usage.
#' @param hi See Usage.
#' @return The value of \code{pmin}.
#' @export
.b1_bound <- function(v, lo, hi) pmin(hi, pmax(lo, v))

#' .b1_logit
#'
#' A step of the b1_tmle implementation. Called by \code{.b1_target}, \code{Tmlecat}, \code{Tmlecde}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.b1_logit <- function(p) {
  p <- .b1_bound(p, 1e-12, 1 - 1e-12)
  log(p / (1 - p))
}

#' .b1_expit
#'
#' A step of the b1_tmle implementation. Called by \code{.b1_target}, \code{Tmlecat}, \code{Tmlecde}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Numeric; passed to \code{exp}.
#' @return The value of \code{ifelse}.
#' @export
.b1_expit <- function(x) ifelse(x >= 0, 1 / (1 + exp(-x)),
                                exp(x) / (1 + exp(x)))

#' .b1_target
#'
#' A step of the b1_tmle implementation. Called by \code{Comptml}, \code{Tmleboot}, \code{Tmleor} and 3 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param Y A vector; its length is taken.
#' @param A Numeric; combined arithmetically in the body.
#' @param QAW Passed to \code{.b1_logit}.
#' @param Q1W Passed to \code{.b1_logit}.
#' @param Q0W Passed to \code{.b1_logit}.
#' @param g1W Passed to \code{.b1_bound}.
#' @param gbound Numeric; combined arithmetically in the body. Defaults to \code{0.025}.
#' @param iters Coerced to integer by the body, with \code{as.integer}. Defaults to \code{100}.
#' @param tol Defaults to \code{1e-12}.
#' @return A list with \code{epsilon}, \code{QAstar}, \code{Q1star}, \code{Q0star}, \code{g1}, \code{g0}, \code{H1}, \code{H0}.
#' @export
.b1_target <- function(Y, A, QAW, Q1W, Q0W, g1W, gbound = 0.025,
                       iters = 100, tol = 1e-12) {
  n <- length(Y)
  g1 <- .b1_bound(g1W, gbound, 1 - gbound)
  g0 <- 1 - g1
  H1 <- A / g1
  H0 <- (1 - A) / g0
  off <- .b1_logit(QAW)
  e <- c(0, 0)
  for (t in seq_len(as.integer(iters))) {
    eta <- off + e[1] * H0 + e[2] * H1
    mu <- .b1_expit(eta)
    r <- Y - mu
    w <- mu * (1 - mu)
    gr <- c(sum(H0 * r), sum(H1 * r))
    Hm <- matrix(c(sum(H0 * H0 * w) + 1e-10, sum(H0 * H1 * w),
                   sum(H0 * H1 * w), sum(H1 * H1 * w) + 1e-10), 2, 2)
    st <- as.numeric(solve(Hm, gr))
    e <- e + st
    if (max(abs(st)) < tol) break
  }
  list(epsilon = e,
       QAstar = .b1_expit(off + e[1] * H0 + e[2] * H1),
       Q1star = .b1_expit(.b1_logit(Q1W) + e[2] / g1),
       Q0star = .b1_expit(.b1_logit(Q0W) + e[1] / g0),
       g1 = g1, g0 = g0, H1 = H1, H0 = H0)
}

#' .b1_curves
#'
#' A step of the b1_tmle implementation. Called by \code{Comptml}, \code{Tmleboot}, \code{Tmleor} and 3 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param Y A vector; its length is taken.
#' @param A Numeric; combined arithmetically in the body.
#' @param fit A list; the body reads \code{$g0}, \code{$g1}, \code{$Q0star}, \code{$Q1star}, \code{$QAstar} from it.
#' @return A list with \code{mu1}, \code{mu0}, \code{ic1}, \code{ic0}.
#' @export
.b1_curves <- function(Y, A, fit) {
  n <- length(Y)
  mu1 <- mean(fit$Q1star); mu0 <- mean(fit$Q0star)
  ic1 <- A / fit$g1 * (Y - fit$QAstar) + fit$Q1star - mu1
  ic0 <- (1 - A) / fit$g0 * (Y - fit$QAstar) + fit$Q0star - mu0
  list(mu1 = mu1, mu0 = mu0, ic1 = ic1, ic0 = ic0)
}
