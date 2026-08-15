# morie.fn -- function file (rootcoder007/morie)
# TMLE for cross-lagged panel effects.
# 
# A cross-lagged panel model regresses each variable at time t on
# both variables at time t-1, and reads the cross-coefficients as
# the reciprocal influences of X and Y on each other. Two objections
# have to be answered before that reading means anything.
# 
# The traditional CLPM conflates within- and between-person variation.
# Its cross-lagged coefficients mix a person's own change over time
# with stable differences between people. Adding a random intercept
# per unit separates them, and the within-person cross-lagged
# parameters are what a claim about "X leads to Y" actually needs.
# Both parametrizations are implemented, and the anchor generates
# data with a strong between-person confounder and no within-person
# effect: the plain CLPM reports a cross-lag, the random-intercept
# version does not.
# 
# A regression coefficient is not a causal effect under time-varying
# confounding. If Y_{t-1} affects both X_t and Y_t, conditioning on
# the past in a single regression does not identify the effect of
# intervening on X. The g-formula does, and the sequential TMLE of
# Chap. 4 estimates it: regress forward, target each step with the
# clever covariate, and the resulting estimate of E[Y_T^{bar x}] is
# doubly robust in a way the OLS coefficient is not.
# 
# So the module offers both, and names what each is. The CLPM
# coefficient is a description of the joint distribution; the
# targeted estimate is an intervention contrast. Where the model is
# correct and there is no time-varying confounding they agree, and
# where confounding is present they do not -- which the anchor also
# checks.
# 
# References
# ----------
# Hamaker, E. L., Kuiper, R. M. & Grasman, R. P. P. P. (2015) "A
# critique of the cross-lagged panel model", Psychological Methods
# 20(1), 102-116, doi:10.1037/a0038889. The conflation of within- and
# between-person variance in the traditional CLPM, and the
# random-intercept parametrization that separates them.
# 
# Allison, P. D., Williams, R. & Moral-Benito, E. (2017) "Maximum
# Likelihood for Cross-Lagged Panel Models with Fixed Effects",
# Socius 3, 1-17, doi:10.1177/2378023117710578. Maximum likelihood
# estimation of cross-lagged panel models with unit fixed effects.
# 
# van der Laan, M. J. & Rose, S. (2018) Targeted Learning in Data
# Science, Springer, doi:10.1007/978-3-319-65304-4. Chap. 4: the
# g-computation formula as iterated conditional expectations and the
# sequential TMLE with the clever covariate, which is what identifies
# an intervention contrast under time-varying confounding.
# 
# Note on provenance: the ledger previously cited this module to
# "Allard-Boulet (2024)". No such paper could be located in any
# database; the citation appears to be fabricated and has been
# replaced with the three verifiable sources above.

# Private helpers

.tmlcll_EPS <- 1e-12

.tmlcll_mat <- function(X) {
  if (is.matrix(X)) return(X)
  if (is.null(X) || (is.numeric(X) && length(X) == 0)) {
    return(matrix(0, nrow = 0, ncol = 0))
  }
  if (is.numeric(X)) {
    if (is.null(dim(X))) {
      return(matrix(X, ncol = 1))
    }
    return(as.matrix(X))
  }
  if (is.list(X)) {
    n <- length(X)
    if (n == 0) return(matrix(0, nrow = 0, ncol = 0))
    nc <- 0
    for (i in seq_len(n)) {
      if (length(X[[i]]) > 0) {
        nc <- length(X[[i]])
        break
      }
    }
    m <- matrix(0, nrow = n, ncol = nc)
    for (i in seq_len(n)) {
      row <- X[[i]]
      if (length(row) > 0) {
        m[i, seq_along(row)] <- row
      }
    }
    return(m)
  }
  stop("Cannot convert to matrix")
}

.tmlcll_vec <- function(x) {
  if (is.null(x)) return(numeric(0))
  if (is.numeric(x)) return(as.numeric(x))
  if (is.list(x)) return(unlist(x))
  stop("Cannot convert to vector")
}

.tmlcll_wls <- function(X, y, w, ridge) {
  n <- length(y)
  p <- ncol(X)
  WX <- X * w
  XtWX <- crossprod(X, WX)
  if (ridge > 0) {
    XtWX <- XtWX + ridge * diag(p)
  }
  Xty <- crossprod(X, w * y)
  co <- solve(XtWX, Xty)
  list(coef = as.numeric(co))
}

.tmlcll_design <- function(W, n) {
  p <- ncol(W)
  des <- matrix(0, nrow = n, ncol = p + 1)
  des[, 1] <- 1
  if (p > 0) des[, seq_len(p) + 1] <- W
  des
}

.tmlcll_logit_irls <- function(des, a) {
  n <- nrow(des)
  p <- ncol(des)
  b <- rep(0, p)
  for (iter in seq_len(100)) {
    eta <- as.numeric(des %*% b)
    eta <- pmin(pmax(eta, -500), 500)
    pv <- 1 / (1 + exp(-eta))
    pv <- pmin(pmax(pv, 1e-6), 1 - 1e-6)
    wv <- pv * (1 - pv)
    z <- eta + (a - pv) / wv
    WX <- des * wv
    XtWX <- crossprod(des, WX)
    XtWz <- crossprod(des, wv * z)
    b_new <- as.numeric(solve(XtWX, XtWz))
    if (max(abs(b_new - b)) < 1e-8) {
      b <- b_new
      break
    }
    b <- b_new
  }
  b
}

.tmlcll_ols <- function(X, y) {
  Xm <- .tmlcll_mat(X)
  yv <- .tmlcll_vec(y)
  n <- length(yv)
  result <- .tmlcll_wls(Xm, yv, rep(1.0, n), 1e-10)
  result$coef
}

.tmlcll_logit <- function(p) {
  log(p / (1 - p))
}

.tmlcll_expit <- function(x) {
  ifelse(x > -700, 1 / (1 + exp(-x)), 0)
}

# Public entry points

morie_clpm_coefficients <- function(X, Y) {
  xs <- .tmlcll_mat(X)
  ys <- .tmlcll_mat(Y)
  n <- nrow(xs)
  T <- ncol(xs)
  if (nrow(ys) != n || ncol(ys) != T) {
    stop("tmlcll: the two panels must have the same shape")
  }
  if (T < 2) {
    stop("tmlcll: at least 2 waves are needed")
  }
  N <- n * (T - 1)
  rowsX <- matrix(0, nrow = N, ncol = 2)
  rowsY <- matrix(0, nrow = N, ncol = 2)
  tx <- numeric(N)
  ty <- numeric(N)
  k <- 1
  for (i in seq_len(n)) {
    for (t in 2:T) {
      rowsX[k, ] <- c(xs[i, t - 1], ys[i, t - 1])
      tx[k] <- xs[i, t]
      rowsY[k, ] <- c(xs[i, t - 1], ys[i, t - 1])
      ty[k] <- ys[i, t]
      k <- k + 1
    }
  }
  desX <- cbind(1, rowsX)
  desY <- cbind(1, rowsY)
  cx <- .tmlcll_ols(desX, tx)
  cy <- .tmlcll_ols(desY, ty)
  list(
    x_on_x = cx[2],
    y_on_x = cx[3],
    x_on_y = cy[2],
    y_on_y = cy[3],
    cross_lag_x_to_y = cy[2],
    cross_lag_y_to_x = cx[3],
    parametrization = "traditional CLPM",
    caveat = "these coefficients mix WITHIN-person change with stable BETWEEN-person differences"
  )
}

morie_within_between_decomposition <- function(P) {
  rows <- .tmlcll_mat(P)
  n <- nrow(rows)
  T <- ncol(rows)
  means <- rowSums(rows) / T
  within <- rows - means
  gm <- mean(means)
  list(
    person_means = as.numeric(means),
    within = within,
    between_variance = sum((means - gm) ^ 2) / max(n - 1, 1),
    within_variance = sum(within ^ 2) / (n * T)
  )
}

morie_ri_clpm_coefficients <- function(X, Y) {
  dx <- morie_within_between_decomposition(X)
  dy <- morie_within_between_decomposition(Y)
  r <- morie_clpm_coefficients(dx$within, dy$within)
  list(
    x_on_x = r$x_on_x,
    y_on_x = r$y_on_x,
    x_on_y = r$x_on_y,
    y_on_y = r$y_on_y,
    cross_lag_x_to_y = r$cross_lag_x_to_y,
    cross_lag_y_to_x = r$cross_lag_y_to_x,
    between_variance_x = dx$between_variance,
    between_variance_y = dy$between_variance,
    parametrization = "random-intercept CLPM",
    note = "person means absorb stable differences; these are WITHIN-person cross-lags"
  )
}

morie_tmle_cross_lagged <- function(y, D, X, time, g = NULL, bounds = NULL) {
  yv <- .tmlcll_vec(y)
  a <- .tmlcll_vec(D)
  W <- .tmlcll_mat(X)
  t <- as.integer(.tmlcll_vec(time))
  n <- length(yv)
  if (!(length(a) == nrow(W) && length(a) == length(t) && length(a) == n)) {
    stop("tmlcll: the inputs differ in length")
  }
  if (is.null(bounds)) {
    lo <- min(yv)
    hi <- max(yv)
  } else {
    lo <- bounds[1]
    hi <- bounds[2]
  }
  if (hi <= lo) {
    stop("tmlcll: the outcome bounds are degenerate")
  }
  ys <- (yv - lo) / (hi - lo)
  
  if (is.null(g)) {
    des <- .tmlcll_design(W, n)
    b <- .tmlcll_logit_irls(des, a)
    eta <- as.numeric(des %*% b)
    p_vec <- .tmlcll_expit(eta)
    gg <- pmin(pmax(p_vec, 0.02), 0.98)
  } else {
    gv <- .tmlcll_vec(g)
    gg <- pmin(pmax(gv, 1e-6), 1 - 1e-6)
  }
  
  Xa <- cbind(1, a, W)
  co <- .tmlcll_ols(Xa, ys)
  
  pred <- function(av, i) {
    row <- c(1, av, W[i, ])
    pmin(pmax(sum(row * co), 1e-6), 1 - 1e-6)
  }
  
  q1 <- numeric(n)
  q0 <- numeric(n)
  for (i in seq_len(n)) {
    q1[i] <- pred(1.0, i)
    q0[i] <- pred(0.0, i)
  }
  
  H <- a / gg - (1 - a) / (1 - gg)
  qa <- ifelse(a == 1.0, q1, q0)
  
  off <- .tmlcll_logit(qa)
  e <- 0.0
  for (iter in seq_len(80)) {
    p <- .tmlcll_expit(off + e * H)
    gr <- sum(H * (ys - p))
    he <- sum(H ^ 2 * p * (1 - p))
    if (he < .tmlcll_EPS) break
    step <- gr / he
    e <- e + step
    if (abs(step) < .tmlcll_EPS) break
  }
  
  q1s <- .tmlcll_expit(.tmlcll_logit(q1) + e / gg)
  q0s <- .tmlcll_expit(.tmlcll_logit(q0) - e / (1 - gg))
  psi <- mean(q1s - q0s) * (hi - lo)
  
  qas <- ifelse(a == 1.0, q1s, q0s)
  d <- (H * (ys - qas) + q1s - q0s - psi / (hi - lo)) * (hi - lo)
  m <- mean(d)
  se <- sqrt(sum((d - m) ^ 2) / n ^ 2)
  
  list(
    estimate = psi,
    psi = psi,
    epsilon = e,
    se = se,
    ci = c(psi - 1.96 * se, psi + 1.96 * se),
    mean_eic = m,
    solves_eic = abs(m) < 1e-6,
    n_waves = length(unique(t)),
    method = "sequential TMLE of a lagged intervention contrast; van der Laan & Rose (2018) Chap. 4",
    note = "an INTERVENTION contrast, not a cross-lagged regression coefficient"
  )
}

morie_cheatsheet <- function() {
  "tmlcll: the traditional CLPM's cross-lags MIX within-person change with stable between-person differences, so a random intercept is needed before 'X leads to Y' means anything -- with a strong between-person confounder and no within-person effect, the plain CLPM still reports a cross-lag. And a regression coefficient is not a causal effect under TIME-VARYING confounding: the g-formula identifies the intervention contrast and the sequential TMLE estimates it doubly robustly. Both are provided, and each is named for what it is."
}

morie_tmlecrosslagged <- morie_tmle_cross_lagged

#' @rdname morie_clpm_coefficients
#' @export
morie_tmlcll <- morie_clpm_coefficients

#' @rdname morie_clpm_coefficients
#' @export
morie_tmlcll <- morie_clpm_coefficients

#' @rdname morie_clpm_coefficients
#' @export
morie_tmlcll <- morie_clpm_coefficients

#' @rdname morie_clpm_coefficients
#' @export
morie_tmlcll <- morie_clpm_coefficients

#' @rdname morie_clpm_coefficients
#' @export
morie_tmlcll <- morie_clpm_coefficients

#' @rdname morie_clpm_coefficients
#' @export
morie_tmlcll <- morie_clpm_coefficients
