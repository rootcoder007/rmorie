# SPDX-License-Identifier: AGPL-3.0-or-later
#' Semiparametric Bernstein-von Mises for the Cox model
#'
#' sqrt(n)(beta_n - beta0) tends to N(0, I_eff^(-1)) where I_eff is the
#' EFFICIENT information -- the information for beta after projecting out
#' the infinite-dimensional baseline hazard.  Two consequences carry the
#' practical content: Bayesian credible intervals for beta are
#' asymptotically valid CONFIDENCE intervals, which is not automatic in
#' semiparametric problems and does fail for other functionals; and
#' nothing is lost by not knowing the baseline hazard.
#'
#' Formula: Newton on the Cox partial likelihood, with
#'   grad_i = x_i - xbar_i and hess -= (weighted second moment
#'   - outer(xbar_i, xbar_i)) over each risk set; se = sqrt(diag(pinv(I))).
#'
#' @param x Covariates, a vector or a matrix with one row per time.
#' @param time Follow-up times; required.
#' @param event Event indicators 0/1; all events when NULL.
#' @param beta_grid Grid for the normal approximation of the first
#'   coefficient; a 4-standard-error window when NULL.
#' @return List with \code{beta}, \code{se},
#'   \code{efficient_information}, \code{beta_grid},
#'   \code{posterior_normal}, \code{efficient},
#'   \code{credible_equals_confidence}, \code{caveat},
#'   \code{n_events}, \code{n}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, sections 13.6.2 and 13.7.2.
#' @export
Ghosalcoxbvm <- function(x, time = NULL, event = NULL, beta_grid = NULL) {
  X <- if (is.matrix(x)) x else matrix(as.numeric(x), nrow = 1L)
  if (is.null(time))
    stop(paste0("time is required: the Cox partial likelihood needs ",
                "follow-up times as well as covariates."))
  tv <- as.numeric(time)
  if (nrow(X) != length(tv)) X <- t(X)
  if (nrow(X) != length(tv))
    stop("x must have one row per follow-up time.")
  n <- nrow(X)
  p <- ncol(X)
  if (n < 5L) stop(sprintf("need at least 5 observations, got %d.", n))
  ev <- if (is.null(event)) rep(1, n) else as.numeric(event)
  if (length(ev) != n)
    stop(sprintf("event has %d entries for %d times.", length(ev), n))
  if (!all(ev %in% c(0, 1))) stop("event must be binary 0/1.")
  if (sum(ev) < 2) stop("need at least 2 events to fit the Cox model.")
  nll_grad_hess <- function(b) {
    eta <- as.numeric(X %*% b)
    w <- exp(eta - max(eta))
    ll <- 0
    gr <- numeric(p)
    he <- matrix(0, p, p)
    for (i in which(ev == 1)) {
      at <- tv >= tv[i]
      sw <- sum(w[at])
      if (sw <= 0) next
      Xa <- X[at, , drop = FALSE]
      wa <- w[at]
      xb <- as.numeric(crossprod(Xa, wa)) / sw
      ll <- ll + eta[i] - log(sw)
      gr <- gr + X[i, ] - xb
      he <- he - (crossprod(Xa, Xa * wa) / sw - outer(xb, xb))
    }
    list(nll = -ll, gr = -gr, he = -he)
  }
  b <- numeric(p)
  for (it in seq_len(50)) {
    g <- nll_grad_hess(b)
    step <- solve(g$he + 1e-10 * diag(p), g$gr)
    b <- b - step
    if (max(abs(step)) < 1e-10) break
  }
  info <- nll_grad_hess(b)$he
  se <- sqrt(pmax(diag(.ghc_pinv(info)), 0))
  bg <- if (is.null(beta_grid))
    seq(b[1] - 4 * se[1], b[1] + 4 * se[1], length.out = 101) else
      as.numeric(beta_grid)
  s1 <- max(se[1], 1e-12)
  post <- exp(-0.5 * ((bg - b[1]) / s1)^2) / (s1 * sqrt(2 * pi))
  .t1_result(beta = b, se = se, efficient_information = info,
             beta_grid = bg, posterior_normal = post,
             efficient = TRUE, credible_equals_confidence = TRUE,
             caveat = paste0("BvM can FAIL for other semiparametric functionals; ",
                             "validity here is a theorem about this one"),
             n_events = sum(ev), n = n,
             method = "Cox partial likelihood with the semiparametric BvM of Sec. 13.6.2")
}
