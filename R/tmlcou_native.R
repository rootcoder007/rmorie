# Targeted maximum likelihood estimation for a bounded continuous or
# count outcome.
# Sources: Gruber, S. & van der Laan, M. J. (2010) "A Targeted
# Maximum Likelihood Estimator of a Causal Effect on a Bounded
# Continuous Outcome", International Journal of Biostatistics 6(1),
# Article 26, doi:10.2202/1557-4679.1260 (rescaling to [0,1],
# quasi-log-likelihood loss for a continuous outcome in [0,1],
# logistic fluctuation, respect for the parameter space);
# van der Laan, M. J. & Rose, S. (2018) Targeted Learning in Data
# Science, Springer, Chap. 4 (Bernoulli log-likelihood loss and
# logistic submodel with the clever covariate, the general TMLE
# roadmap).
#
# Native implementation mirroring Python morie.fn.tmlcou exactly:
# the same affine rescale to [0,1], the same Bernoulli quasi-loss
# with a logistic submodel on the clever covariate, the same
# unscale on return, the same validation messages.

.tmlcou_EPS <- 1e-12

#' .tmlcou_logit
#'
#' A step of the tmlcou_native implementation. Called by \code{morie_tmlcou}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p Coerced to numeric by the body, with \code{as.numeric}.
#' @return A numeric value.
#' @export
#' @examples
#' res <- .tmlcou_logit(p = 0.5)
#' res
.tmlcou_logit <- function(p) {
  q <- min(max(as.numeric(p), 1e-9), 1 - 1e-9)
  log(q / (1 - q))
}

#' .tmlcou_expit
#'
#' A step of the tmlcou_native implementation. Called by \code{morie_tmlcou}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @return One of two values, depending on the branch taken.
#' @export
# vectorised: the scalar if() errors the moment a linear predictor
# VECTOR arrives, which is every call site
.tmlcou_expit <- function(x) ifelse(x > -700, 1 / (1 + exp(-x)), 0)

#' Map the outcome to \[0,1\] by an affine transform
#'
#' @param y Outcome vector.
#' @param lower Lower bound (default: minimum of \code{y}).
#' @param upper Upper bound (default: maximum of \code{y}).
#' @return A list with \code{scaled}, \code{lower}, \code{upper},
#'   \code{range}.
#' @references Gruber, S. & van der Laan, M. J. (2010).
#' @export
rescale <- function(y, lower = NULL, upper = NULL) {
  v <- as.numeric(y)
  if (length(v) == 0L) stop("tmlcou: no outcomes given")
  a <- if (is.null(lower)) min(v) else as.numeric(lower)
  b <- if (is.null(upper)) max(v) else as.numeric(upper)
  if (b <= a) stop("tmlcou: the upper bound must exceed the lower one")
  if (any(v < a - .tmlcou_EPS | v > b + .tmlcou_EPS))
    stop("tmlcou: an outcome lies outside the stated bounds")
  list(scaled = (v - a) / (b - a), lower = a, upper = b,
       range = b - a)
}

#' Invert the affine map -- the influence curve scales with it
#'
#' @param value Value in the rescaled units.
#' @param lower Original lower bound.
#' @param upper Original upper bound.
#' @return The value on the original scale.
#' @export
unscale <- function(value, lower, upper) {
  as.numeric(value) * (as.numeric(upper) - as.numeric(lower)) +
    as.numeric(lower)
}

#' The linear update, kept so its failure is demonstrable
#'
#' Squared-error loss with an additive fluctuation is unbounded: the
#' updated fit can leave the outcome's range, and then so can the
#' substitution estimator.
#'
#' @param Q Initial outcome regression.
#' @param H Clever covariate.
#' @param Y Outcome.
#' @return A list with \code{epsilon}, \code{Q_star},
#'   \code{out_of_range}, \code{caveat}.
#' @export
linear_fluctuation_unsafe <- function(Q, H, Y) {
  q <- as.numeric(Q)
  h <- as.numeric(H)
  yy <- as.numeric(Y)
  n <- length(q)
  if (!(length(h) == length(yy) && length(yy) == n))
    stop("tmlcou: Q, H, Y must have the same length")
  num <- sum(h * (yy - q))
  den <- sum(h * h)
  e <- if (den > .tmlcou_EPS) num / den else 0
  upd <- q + e * h
  list(epsilon = e, Q_star = upd,
       out_of_range = sum(upd < 0 | upd > 1),
       caveat = "an additive fluctuation is not bounded")
}

#' TMLE of the mean-outcome contrast for a count or bounded outcome
#'
#' \code{offset} supplies exposure time, in which case the estimand is
#' a rate. Nuisance fits may be supplied; otherwise they are fitted by
#' logistic and least-squares regression on \code{X}.
#'
#' @param y Outcome vector.
#' @param D Treatment indicator vector.
#' @param X Covariate matrix (rows are observations).
#' @param offset Optional exposure time.
#' @param g Optional propensity score.
#' @param Q1 Optional potential-outcome regression under treatment.
#' @param Q0 Optional potential-outcome regression under control.
#' @param lower Lower bound for the outcome.
#' @param upper Upper bound for the outcome.
#' @param iters Maximum number of Newton steps.
#' @return A \code{RichResult}-style list with \code{estimate},
#'   \code{psi}, \code{epsilon}, \code{se}, \code{ci},
#'   \code{mean_eic}, \code{solves_eic}, \code{scale},
#'   \code{mean_treated}, \code{mean_control}, \code{in_range},
#'   \code{rate_scale}, \code{method}, \code{note}.
#' @references Gruber, S. & van der Laan, M. J. (2010).
#' @export
morie_tmlcou <- function(y, D, X, offset = NULL, g = NULL,
                         Q1 = NULL, Q0 = NULL,
                         lower = NULL, upper = NULL, iters = 100) {
  yv <- as.numeric(y)
  a <- as.numeric(D)
  W <- as.matrix(X)
  storage.mode(W) <- "double"
  n <- length(yv)
  if (!(length(a) == nrow(W) && nrow(W) == n))
    stop("tmlcou: the inputs differ in length")
  if (any(yv < 0)) stop("tmlcou: a count outcome cannot be negative")
  rate <- !is.null(offset)
  if (rate) {
    t <- as.numeric(offset)
    if (length(t) != n || any(t <= 0))
      stop("tmlcou: the offset must be positive and of the same length")
    yv <- yv / t
  }
  sc <- rescale(yv, lower, upper)
  ys <- sc$scaled
  if (is.null(g)) {
    des <- cbind(1, W)
    bhat <- as.numeric(coef(glm(a ~ des - 1, family = binomial())))
    lp <- as.numeric(des %*% bhat)
    gg <- pmin(pmax(.tmlcou_expit(lp), 0.01), 0.99)
  } else {
    gg <- pmin(pmax(as.numeric(g), 1e-6), 1 - 1e-6)
  }
  if (is.null(Q1) || is.null(Q0)) {
    Xa <- cbind(1, a, W)
    co <- as.numeric(solve(crossprod(Xa), crossprod(Xa, ys)))
    pred <- function(av, i) sum(c(1, av, W[i, ]) * co)
    q1 <- pmin(pmax(vapply(seq_len(n), function(i) pred(1, i),
                            numeric(1)), 1e-6), 1 - 1e-6)
    q0 <- pmin(pmax(vapply(seq_len(n), function(i) pred(0, i),
                            numeric(1)), 1e-6), 1 - 1e-6)
  } else {
    q1 <- pmin(pmax(as.numeric(Q1), 1e-6), 1 - 1e-6)
    q0 <- pmin(pmax(as.numeric(Q0), 1e-6), 1 - 1e-6)
  }
  H <- a / gg - (1 - a) / (1 - gg)
  qa <- ifelse(a == 1, q1, q0)
  off <- vapply(qa, .tmlcou_logit, numeric(1))
  e <- 0
  for (k in seq_len(as.integer(iters))) {
    p <- .tmlcou_expit(off + e * H)
    gr <- sum(H * (ys - p))
    he <- sum(H * H * p * (1 - p))
    if (he < 1e-12) break
    step <- gr / he
    e <- e + step
    if (abs(step) < 1e-12) break
  }
  q1s <- .tmlcou_expit(.tmlcou_logit(q1) + e / gg)
  q0s <- .tmlcou_expit(.tmlcou_logit(q0) - e / (1 - gg))
  psi_s <- sum(q1s - q0s) / n
  psi <- psi_s * sc$range
  d <- numeric(n)
  for (i in seq_len(n)) {
    qas <- if (a[i] == 1) q1s[i] else q0s[i]
    d[i] <- (H[i] * (ys[i] - qas) + q1s[i] - q0s[i] - psi_s) *
      sc$range
  }
  m <- sum(d) / n
  se <- sqrt(sum((d - m)^2) / n^2)
  list(estimate = psi, psi = psi, epsilon = e, se = se,
       ci = c(psi - 1.96 * se, psi + 1.96 * se),
       mean_eic = m, solves_eic = abs(m) < 1e-6,
       scale = c(sc$lower, sc$upper),
       mean_treated = unscale(sum(q1s) / n, sc$lower, sc$upper),
       mean_control = unscale(sum(q0s) / n, sc$lower, sc$upper),
       in_range = all(q1s >= 0 & q1s <= 1 & q0s >= 0 & q0s <= 1),
       rate_scale = rate,
       method = paste0("TMLE on a bounded outcome by rescaling to ",
                       "[0,1] with a logistic fluctuation; Gruber & ",
                       "van der Laan (2010)"),
       note = paste0("a LINEAR fluctuation would leave the parameter ",
                     "space; the logistic one cannot"))
}

#' Compact alias per ledger/NAMING.md
#' @export
#' @noRd
morie_tmlcountoutcome <- morie_tmlcou
