# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Native IPW causal mediation (Huber 2014, JAE 29(6), 920-943).
#
# Replaces the former morie_causal_mediation wrapper over
# causalweight::medweight, which was dropped when causalweight was
# archived from CRAN on 2026-05-18 (its dependency LARF was archived
# too). Nothing here delegates to a modelling package: the propensity
# scores are fitted by our own Newton-Raphson binary-choice routine.

# Newton-Raphson binary choice, probit or logit, returning fitted
# probabilities. stats::pnorm/dnorm are used as the normal CDF and
# density -- arithmetic primitives, in the same sense as exp() -- not
# as a model-fitting delegate.
#' Newton-Raphson binary choice, probit or logit, returning fitted
#'
#' probabilities. stats::pnorm/dnorm are used as the normal CDF and
#' density -- arithmetic primitives, in the same sense as exp() -- not
#' as a model-fitting delegate.
#'
#' @param X See Usage.
#' @param y See Usage.
#' @param link Defaults to \code{"probit"}.
#' @param max_iter Defaults to \code{100L}.
#' @param tol Defaults to \code{1e-09}.
#' @return One of two values, depending on the branch taken.
#' @export
.morie_binchoice_fit <- function(X, y, link = "probit",
                                 max_iter = 100L, tol = 1e-9) {
  D <- cbind(1, X)
  beta <- rep(0, ncol(D))
  for (i in seq_len(max_iter)) {
    eta <- pmin(pmax(as.vector(D %*% beta), -35), 35)
    if (link == "probit") {
      p <- stats::pnorm(eta)
      p <- pmin(pmax(p, 1e-12), 1 - 1e-12)
      dens <- stats::dnorm(eta)
      # Probit score and expected Hessian (Fisher scoring):
      # s = phi(eta) (y - p) / (p (1-p)), w = phi(eta)^2 / (p (1-p)).
      w <- dens^2 / (p * (1 - p))
      z <- dens * (y - p) / (p * (1 - p))
    } else {
      p <- 1 / (1 + exp(-eta))
      w <- pmax(p * (1 - p), 1e-10)
      z <- y - p
    }
    grad <- crossprod(D, z)
    H <- crossprod(D * w, D)
    step <- tryCatch(solve(H, grad),
                     error = function(e) .morie_ginv(H) %*% grad)
    beta <- beta + as.vector(step)
    if (max(abs(step)) < tol) break
  }
  eta <- pmin(pmax(as.vector(D %*% beta), -35), 35)
  if (link == "probit") stats::pnorm(eta) else 1 / (1 + exp(-eta))
}

# Normalised weighted mean: sum(y * w) / sum(w). Huber's Section 3
# normalises so the weights within each treatment state add to unity,
# following Imbens (2004) and Busso, DiNardo and McCrary (2009).
#' Normalised weighted mean: sum(y * w) / sum(w). Huber\'s Section 3
#'
#' normalises so the weights within each treatment state add to unity,
#' following Imbens (2004) and Busso, DiNardo and McCrary (2009).
#'
#' @param y See Usage.
#' @param w See Usage.
#' @return A numeric value.
#' @export
.morie_wmean <- function(y, w) sum(y * w) / sum(w)

#' The four normalised means, exactly as written in Huber (2014)
#'
#' Section 3. Reading the two subtracted terms of theta(1) and theta(0)
#' as potential-outcome means gives, in order, E[Y(1,M(1))],
#' E[Y(0,M(1))], E[Y(1,M(0))], E[Y(0,M(0))].
#'
#' @param y See Usage.
#' @param d See Usage.
#' @param pm See Usage.
#' @param px See Usage.
#' @return A vector, from \code{c}.
#' @export
.morie_medweight_point <- function(y, d, pm, px) {
  # The four normalised means, exactly as written in Huber (2014)
  # Section 3. Reading the two subtracted terms of theta(1) and
  # theta(0) as potential-outcome means gives, in order,
  # E[Y(1,M(1))], E[Y(0,M(1))], E[Y(1,M(0))], E[Y(0,M(0))].
  w11 <- d / px
  w01 <- (1 - d) * pm / ((1 - pm) * px)
  w10 <- d * (1 - pm) / (pm * (1 - px))
  w00 <- (1 - d) / (1 - px)
  y11 <- .morie_wmean(y, w11)
  y01 <- .morie_wmean(y, w01)
  y10 <- .morie_wmean(y, w10)
  y00 <- .morie_wmean(y, w00)
  # Propositions 1 and 2 plus equation (3): Delta = theta(1) + delta(0)
  # = theta(0) + delta(1). Taking the indirect effects as the residual
  # of the total and the opposite-state direct effect is what Huber
  # notes is "numerically identical" to the Proposition 2 expression.
  theta1 <- y11 - y01
  theta0 <- y10 - y00
  total <- y11 - y00
  c(total_effect = total,
    direct_treated = theta1,
    direct_control = theta0,
    indirect_treated = total - theta0,
    indirect_control = total - theta1,
    y11 = y11, y01 = y01, y10 = y10, y00 = y00)
}

#' Causal mediation by inverse probability weighting
#'
#' Decomposes the average treatment effect of a binary treatment into a
#' direct effect and an indirect effect running through a mediator,
#' using the inverse-probability-weighting identification of Huber
#' (2014). Two propensity scores are required, \eqn{Pr(D=1|X)} and
#' \eqn{Pr(D=1|M,X)}; no model is imposed on the outcome or on the
#' mediator, so arbitrary nonlinearities in either are permitted.
#'
#' Effects are reported in both treatment states because they differ
#' whenever the treatment interacts with the mediator: the direct
#' effect \eqn{\theta(1)} holds the mediator at its treated value while
#' \eqn{\theta(0)} holds it at its control value. The decomposition
#' \eqn{\Delta = \theta(1) + \delta(0) = \theta(0) + \delta(1)} holds
#' by construction, which is worth checking on any result.
#'
#' @param y numeric outcome vector.
#' @param d binary treatment vector (0/1).
#' @param m mediator: vector or matrix.
#' @param x covariate matrix.
#' @param link \code{"probit"} (the specification Huber uses) or
#'   \code{"logit"}.
#' @param trim propensity scores outside \code{[trim, 1 - trim]} are
#'   dropped. Defaults to \code{0}, i.e. no trimming: Huber's footnote
#'   10 states that no trimming is applied, scores near the boundary not
#'   arising in that paper's simulation or application. Raise it when
#'   common support is doubtful.
#' @param boot number of bootstrap replications for standard errors;
#'   \code{0} skips them. Huber uses the bootstrap for inference.
#' @param seed optional RNG seed for the bootstrap.
#'
#' @return list with \code{total_effect}, \code{direct_treated},
#'   \code{direct_control}, \code{indirect_treated},
#'   \code{indirect_control}, the four weighted means \code{y11},
#'   \code{y01}, \code{y10}, \code{y00}, \code{n_trimmed},
#'   \code{decomposition_holds}, and \code{se} when \code{boot > 0}.
#'
#' @references Huber, M. (2014). Identifying causal mechanisms
#'   (primarily) based on inverse probability weighting. \emph{Journal
#'   of Applied Econometrics}, 29(6), 920-943.
#' @export
morie_causal_mediation <- function(y, d, m, x, link = c("probit", "logit"),
                                   trim = 0, boot = 0L, seed = NULL) {
  link <- match.arg(link)
  y <- as.numeric(y)
  d <- as.numeric(d)
  m <- as.matrix(m)
  x <- as.matrix(x)
  n <- length(y)
  if (length(d) != n || nrow(m) != n || nrow(x) != n) {
    stop("y, d, m and x must describe the same ", n, " observations",
         call. = FALSE)
  }
  if (!all(d %in% c(0, 1))) {
    stop("d must be binary 0/1", call. = FALSE)
  }
  if (trim < 0 || trim >= 0.5) {
    stop("trim must lie in [0, 0.5), got ", trim, call. = FALSE)
  }
  if (!all(is.finite(y)) || !all(is.finite(m)) || !all(is.finite(x))) {
    stop("y, m and x must be finite", call. = FALSE)
  }

  fit <- function(idx) {
    px <- .morie_binchoice_fit(x[idx, , drop = FALSE], d[idx], link)
    pm <- .morie_binchoice_fit(cbind(m, x)[idx, , drop = FALSE], d[idx], link)
    keep <- px > trim & px < 1 - trim & pm > trim & pm < 1 - trim
    if (sum(keep) < 4L || length(unique(d[idx][keep])) < 2L) {
      return(NULL)
    }
    list(est = .morie_medweight_point(y[idx][keep], d[idx][keep],
                                      pm[keep], px[keep]),
         n_trimmed = sum(!keep))
  }

  base <- fit(seq_len(n))
  if (is.null(base)) {
    stop("no usable observations survive the common-support restriction; ",
         "lower `trim` or check overlap", call. = FALSE)
  }
  est <- base$est

  se <- NULL
  if (boot > 0L) {
    if (!is.null(seed)) set.seed(seed)
    reps <- matrix(NA_real_, nrow = boot, ncol = 5L)
    for (b in seq_len(boot)) {
      r <- fit(sample.int(n, n, replace = TRUE))
      if (!is.null(r)) reps[b, ] <- r$est[1:5]
    }
    se <- apply(reps, 2, stats::sd, na.rm = TRUE)
    names(se) <- c("total_effect", "direct_treated", "direct_control",
                   "indirect_treated", "indirect_control")
  }

  scale <- max(1, abs(est[["total_effect"]]))
  list(
    total_effect = unname(est[["total_effect"]]),
    direct_treated = unname(est[["direct_treated"]]),
    direct_control = unname(est[["direct_control"]]),
    indirect_treated = unname(est[["indirect_treated"]]),
    indirect_control = unname(est[["indirect_control"]]),
    y11 = unname(est[["y11"]]), y01 = unname(est[["y01"]]),
    y10 = unname(est[["y10"]]), y00 = unname(est[["y00"]]),
    n_trimmed = as.integer(base$n_trimmed),
    decomposition_holds = isTRUE(
      abs(est[["direct_treated"]] + est[["indirect_control"]] -
            est[["total_effect"]]) < 1e-08 * scale &&
        abs(est[["direct_control"]] + est[["indirect_treated"]] -
              est[["total_effect"]]) < 1e-08 * scale),
    link = link,
    se = se
  )
}
