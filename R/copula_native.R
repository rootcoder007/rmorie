# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Bivariate copula CDFs, dependence measures and survival copulas.
#
# Mirrors morie.fn._copula (Python) and its front-ends copgau/copt/
# copcla/copgmb/copfra/copjoe/plkt/taukcp/spcoef/blncop/ginicop/
# copExt/clyfr/copfr/copod. Distinct from R/copul.R, which estimates a
# parameter from data by tau inversion for three families only; this
# file evaluates the copulas themselves and supplies the Frank, Joe
# and Plackett relations copul.R does not carry.
#
# Parameter/Kendall's-tau relations: Czado (2019), Analyzing Dependent
# Data with Vine Copulas, Table 3.2 p. 54; Theorem 3.9 eq. (3.17);
# Table 3.1 p. 52 (Pickands functions).

.morie_copula_families <- c(
  "independence", "gaussian", "t", "clayton",
  "gumbel", "frank", "joe", "plackett"
)

#' .morie_cop_uv
#'
#' A step of the copula_native implementation. Called by \code{morie_copula_cdf}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param u A vector; its length is taken.
#' @param v A vector; its length is taken.
#' @return A list with \code{u}, \code{v}.
#' @export
.morie_cop_uv <- function(u, v) {
  u <- as.numeric(u)
  v <- as.numeric(v)
  n <- max(length(u), length(v))
  u <- rep_len(u, n)
  v <- rep_len(v, n)
  if (any(u < 0 | u > 1) || any(v < 0 | v > 1)) {
    stop("u and v must lie in [0, 1].", call. = FALSE)
  }
  list(u = pmin(pmax(u, 1e-12), 1 - 1e-12), v = pmin(pmax(v, 1e-12), 1 - 1e-12))
}

#' Bivariate copula CDF
#'
#' Evaluates \eqn{C(u, v)} for the eight families in
#' \code{.morie_copula_families}. Mirrors \code{morie.fn._copula.copula_cdf}.
#'
#' @param family one of "independence", "gaussian", "t", "clayton",
#'   "gumbel", "frank", "joe", "plackett".
#' @param u,v numeric vectors in \[0, 1\].
#' @param theta copula parameter (rho for gaussian/t).
#' @param nu degrees of freedom, t copula only.
#' @return numeric vector of CDF values.
#' @references Czado, C. (2019). Analyzing Dependent Data with Vine
#'   Copulas. Springer. Ch. 3.
#' @examples
#' morie_copula_cdf("clayton", 0.5, 0.5, theta = 2)
#' @export
morie_copula_cdf <- function(family, u, v, theta = NULL, nu = NULL) {
  family <- match.arg(family, .morie_copula_families)
  uv <- .morie_cop_uv(u, v)
  u <- uv$u
  v <- uv$v
  if (family == "independence") {
    return(u * v)
  }
  if (family %in% c("gaussian", "t")) {
    rho <- as.numeric(theta)
    if (!isTRUE(rho > -1 && rho < 1)) {
      stop("rho must lie in (-1, 1).", call. = FALSE)
    }
    if (family == "gaussian") {
      x <- stats::qnorm(u)
      y <- stats::qnorm(v)
      return(vapply(seq_along(x), function(i) .morie_bvn_cdf(x[i], y[i], rho), 0))
    }
    if (is.null(nu) || nu <= 0) stop("t copula needs nu > 0.", call. = FALSE)
    x <- stats::qt(u, nu)
    y <- stats::qt(v, nu)
    return(vapply(seq_along(x), function(i) .morie_bvt_cdf(x[i], y[i], rho, nu), 0))
  }
  th <- as.numeric(theta)
  if (family == "clayton") {
    if (!isTRUE(th > 0)) stop("clayton theta must be positive.", call. = FALSE)
    return(pmax(u^(-th) + v^(-th) - 1, 1e-300)^(-1 / th))
  }
  if (family == "gumbel") {
    if (!isTRUE(th >= 1)) stop("gumbel theta must be >= 1.", call. = FALSE)
    return(exp(-(((-log(u))^th + (-log(v))^th)^(1 / th))))
  }
  if (family == "frank") {
    if (!isTRUE(th != 0)) stop("frank theta must be non-zero.", call. = FALSE)
    return(-log1p(expm1(-th * u) * expm1(-th * v) / expm1(-th)) / th)
  }
  if (family == "joe") {
    if (!isTRUE(th >= 1)) stop("joe theta must be >= 1.", call. = FALSE)
    ub <- (1 - u)^th
    vb <- (1 - v)^th
    return(1 - (ub + vb - ub * vb)^(1 / th))
  }
  if (!isTRUE(th > 0)) stop("plackett theta must be positive.", call. = FALSE)
  if (isTRUE(all.equal(th, 1))) {
    return(u * v)
  }
  eta <- th - 1
  s <- 1 + eta * (u + v)
  (s - sqrt(s^2 - 4 * th * eta * u * v)) / (2 * eta)
}

# Bivariate normal CDF by one-dimensional quadrature over the
# conditional normal -- avoids a mvtnorm dependency.
#' Bivariate normal CDF by one-dimensional quadrature over the
#'
#' conditional normal -- avoids a mvtnorm dependency.
#'
#' @param x Numeric; passed to \code{max}.
#' @param y Numeric; passed to \code{max}.
#' @param rho Numeric; combined arithmetically in the body.
#' @return The value of \code{$}.
#' @export
.morie_bvn_cdf <- function(x, y, rho) {
  if (!is.finite(x) || !is.finite(y)) {
    return(as.numeric(is.finite(x) && x > 0) * as.numeric(is.finite(y) && y > 0))
  }
  # Clamp to +/- 8.5 sd: beyond that the normal CDF is 1 to machine
  # precision, and integrating out to a far endpoint (which the t
  # copula's quantiles produce near u = 1) makes R's adaptive
  # quadrature miss the mass entirely and return ~0.
  x <- min(max(x, -8.5), 8.5)
  y <- min(max(y, -8.5), 8.5)
  s <- sqrt(1 - rho^2)
  f <- function(z) stats::dnorm(z) * stats::pnorm((y - rho * z) / s)
  stats::integrate(f, -Inf, x, rel.tol = 1e-10)$value
}

# Bivariate t CDF as a chi-square scale mixture of bivariate normals.
#' Bivariate t CDF as a chi-square scale mixture of bivariate normals
#'
#' A step of the copula_native implementation. Called by \code{morie_copula_cdf}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @param y Numeric; combined arithmetically in the body.
#' @param rho Passed to \code{.morie_bvn_cdf}.
#' @param nu Numeric; combined arithmetically in the body.
#' @return The value of \code{$}.
#' @export
.morie_bvt_cdf <- function(x, y, rho, nu) {
  if (!is.finite(x) || !is.finite(y)) {
    return(as.numeric(is.finite(x) && x > 0) * as.numeric(is.finite(y) && y > 0))
  }
  f <- function(w) {
    vapply(w, function(wi) {
      s <- sqrt(wi / nu)
      .morie_bvn_cdf(x * s, y * s, rho) * stats::dchisq(wi, nu)
    }, 0)
  }
  stats::integrate(f, 1e-8, nu + 12 * sqrt(2 * nu), rel.tol = 1e-8)$value
}

#' Kendall's tau implied by a copula parameter
#'
#' Closed forms from Czado (2019) Table 3.2 p. 54. Mirrors
#' \code{morie.fn.taukcp}.
#'
#' @param family copula family.
#' @param theta parameter (rho for gaussian/t).
#' @param nu t degrees of freedom.
#' @return numeric Kendall's tau.
#' @references Czado, C. (2019). Table 3.2, p. 54.
#' @examples
#' morie_copula_tau("clayton", 2)
#' @export
morie_copula_tau <- function(family, theta = NULL, nu = NULL) {
  family <- match.arg(family, .morie_copula_families)
  if (family == "independence") {
    return(0)
  }
  th <- as.numeric(theta)
  if (family %in% c("gaussian", "t")) {
    if (!isTRUE(th > -1 && th < 1)) stop("rho must lie in (-1, 1).", call. = FALSE)
    return(2 / pi * asin(th))
  }
  if (family == "gumbel") {
    if (!isTRUE(th >= 1)) stop("gumbel theta must be >= 1.", call. = FALSE)
    return(1 - 1 / th)
  }
  if (family == "clayton") {
    if (!isTRUE(th > 0)) stop("clayton theta must be positive.", call. = FALSE)
    return(th / (th + 2))
  }
  if (family == "frank") {
    if (!isTRUE(th != 0)) stop("frank theta must be non-zero.", call. = FALSE)
    d <- abs(th)
    f <- function(x) ifelse(x == 0, 1, x / expm1(x))
    D1 <- stats::integrate(f, 0, d, rel.tol = 1e-10)$value / d
    tau <- 1 - 4 / d * (1 - D1)
    return(if (th > 0) tau else -tau)
  }
  if (family == "joe") {
    if (!isTRUE(th >= 1)) stop("joe theta must be >= 1.", call. = FALSE)
    if (isTRUE(all.equal(th, 1))) {
      return(0)
    }
    g <- 0.5772156649015329
    return(1 + (-2 + 2 * g + 2 * log(2) + digamma(1 / th) +
      digamma(0.5 * (2 + th) / th) + th) / (-2 + th))
  }
  .morie_tau_numeric(family, th, nu)
}

# tau = 4 int int C dC - 1 on a grid; used for families whose closed
# form Czado's Table 3.2 does not give.
#' Tau = 4 int int C dC - 1 on a grid; used for families whose closed
#'
#' form Czado\'s Table 3.2 does not give.
#'
#' @param family Passed to \code{morie_copula_cdf}.
#' @param theta Passed to \code{morie_copula_cdf}.
#' @param nu Passed to \code{morie_copula_cdf}.
#' @param n A count; the body uses it as \code{seq_len(...)}. Defaults to \code{200L}.
#' @return A numeric value.
#' @export
.morie_tau_numeric <- function(family, theta = NULL, nu = NULL, n = 200L) {
  g <- (seq_len(n) - 0.5) / n
  U <- rep(g, each = n)
  V <- rep(g, times = n)
  h <- 1 / (2 * n)
  cl <- function(z) pmin(pmax(z, 1e-12), 1 - 1e-12)
  C <- morie_copula_cdf(family, U, V, theta, nu)
  dens <- (morie_copula_cdf(family, cl(U + h), cl(V + h), theta, nu) -
    morie_copula_cdf(family, cl(U + h), cl(V - h), theta, nu) -
    morie_copula_cdf(family, cl(U - h), cl(V + h), theta, nu) +
    morie_copula_cdf(family, cl(U - h), cl(V - h), theta, nu)) / (4 * h * h)
  4 * sum(C * dens) / n^2 - 1
}

#' Invert the Kendall's tau relation for a copula parameter
#'
#' Mirrors \code{morie.fn._copula.tau_to_theta}. Extends
#' \code{\link{copul}}, which inverts tau for the Gaussian, Clayton and
#' Gumbel families only, to Frank, Joe and Plackett (by bisection on
#' the monotone tau map).
#'
#' @param family copula family.
#' @param tau target Kendall's tau in (-1, 1).
#' @return numeric parameter, or NULL for the independence copula.
#' @examples
#' morie_tau_to_theta("clayton", 0.5)
#' @export
morie_tau_to_theta <- function(family, tau) {
  family <- match.arg(family, .morie_copula_families)
  tau <- as.numeric(tau)
  if (!isTRUE(tau > -1 && tau < 1)) {
    stop("tau must lie in (-1, 1).", call. = FALSE)
  }
  if (family == "independence") {
    return(NULL)
  }
  if (family %in% c("gaussian", "t")) {
    return(sin(pi * tau / 2))
  }
  if (family == "gumbel") {
    if (tau <= 0) stop("gumbel admits only tau > 0.", call. = FALSE)
    return(1 / (1 - tau))
  }
  if (family == "clayton") {
    if (tau <= 0) stop("clayton admits only tau > 0.", call. = FALSE)
    return(2 * tau / (1 - tau))
  }
  lo <- 1e-6
  hi <- 60
  if (family == "joe") lo <- 1 + 1e-9
  if (family == "frank" && tau < 0) {
    lo <- -60
    hi <- -1e-6
  }
  stats::uniroot(function(t) morie_copula_tau(family, t) - tau,
    lower = lo, upper = hi, tol = 1e-12
  )$root
}

#' Spearman's rho implied by a copula
#'
#' \eqn{\rho_S = 12 \int\int C(u,v) du dv - 3}. The Gaussian branch
#' returns the exact elliptical form \eqn{(6/\pi)\arcsin(\rho/2)}.
#' Mirrors \code{morie.fn.spcoef}.
#'
#' @param family copula family.
#' @param theta parameter.
#' @param nu t degrees of freedom.
#' @param n grid resolution for the numeric route.
#' @return list: rho_s, exact, family, theta, method.
#' @examples
#' morie_copula_spearman("gaussian", 0.6)$rho_s
#' @export
morie_copula_spearman <- function(family, theta = NULL, nu = NULL, n = 200L) {
  family <- match.arg(family, .morie_copula_families)
  if (family == "independence") {
    return(list(
      rho_s = 0, exact = TRUE, family = family, theta = NULL,
      method = "Spearman's rho (independence copula)"
    ))
  }
  if (family == "gaussian") {
    r <- as.numeric(theta)
    if (!isTRUE(r > -1 && r < 1)) stop("rho must lie in (-1, 1).", call. = FALSE)
    return(list(
      rho_s = 6 / pi * asin(r / 2), exact = TRUE, family = family, theta = r,
      method = "Spearman's rho, elliptical closed form (6/pi) asin(rho/2)"
    ))
  }
  n <- as.integer(n)
  if (n < 20L) stop("n must be at least 20.", call. = FALSE)
  g <- (seq_len(n) - 0.5) / n
  C <- morie_copula_cdf(family, rep(g, each = n), rep(g, times = n), theta, nu)
  list(
    rho_s = 12 * mean(C) - 3, exact = FALSE, family = family,
    theta = as.numeric(theta),
    method = sprintf("Spearman's rho by grid quadrature (n = %d)", n)
  )
}

#' Blomqvist's beta from a copula
#'
#' \eqn{\beta = 4 C(1/2, 1/2) - 1}: the medial correlation, which
#' depends on the copula only at the centre and is therefore blind to
#' tail behaviour. Mirrors \code{morie.fn.blncop}.
#'
#' @param family copula family.
#' @param theta parameter.
#' @param nu t degrees of freedom.
#' @return list: beta, c_half, family, theta, method.
#' @references Blomqvist, N. (1950). Ann. Math. Statist. 21(4), 593-600.
#' @examples
#' morie_blomqvist_beta("clayton", 5)$beta
#' @export
morie_blomqvist_beta <- function(family, theta = NULL, nu = NULL) {
  family <- match.arg(family, .morie_copula_families)
  cc <- as.numeric(morie_copula_cdf(family, 0.5, 0.5, theta, nu))
  list(
    beta = 4 * cc - 1, c_half = cc, family = family,
    theta = if (is.null(theta)) NULL else as.numeric(theta),
    method = "Blomqvist's beta = 4 C(1/2, 1/2) - 1"
  )
}

#' Gini's gamma from a copula
#'
#' Contrasts the copula's two diagonals:
#' \eqn{\gamma = 4\[\int C(u, 1-u) du - \int (u - C(u,u)) du\]}.
#' Mirrors \code{morie.fn.ginicop}.
#'
#' @param family copula family.
#' @param theta parameter.
#' @param nu t degrees of freedom.
#' @param n quadrature points per diagonal.
#' @return list: gamma, anti_diagonal, diagonal_gap, family, theta, method.
#' @references Nelsen, R. B. (2006). An Introduction to Copulas
#'   (2nd ed.). Springer. Sec. 5.1.4.
#' @examples
#' morie_gini_gamma("gumbel", 5)$gamma
#' @export
morie_gini_gamma <- function(family, theta = NULL, nu = NULL, n = 400L) {
  family <- match.arg(family, .morie_copula_families)
  n <- as.integer(n)
  if (n < 20L) stop("n must be at least 20.", call. = FALSE)
  u <- (seq_len(n) - 0.5) / n
  anti <- mean(morie_copula_cdf(family, u, 1 - u, theta, nu))
  diag_gap <- mean(u - morie_copula_cdf(family, u, u, theta, nu))
  list(
    gamma = 4 * (anti - diag_gap), anti_diagonal = anti,
    diagonal_gap = diag_gap, family = family,
    theta = if (is.null(theta)) NULL else as.numeric(theta),
    method = "Gini's gamma = 4[int C(u,1-u) du - int (u - C(u,u)) du]"
  )
}

#' Bivariate extreme-value copula from a Pickands dependence function
#'
#' \eqn{C(u,v) = \exp\{\log(uv) A(\log v / \log(uv))\}}. Built-in A
#' choices follow Czado (2019) Table 3.1 p. 52. Every extreme-value
#' copula is max-stable, \eqn{C(u^k, v^k) = C(u,v)^k}. Mirrors
#' \code{morie.fn.copExt}.
#'
#' @param u,v numeric vectors strictly inside (0, 1).
#' @param A "gumbel", "galambos", "independence", or a function.
#' @param theta parameter for the built-in families.
#' @return list: cdf, pickands_at_half, valid_pickands, A, theta, method.
#' @examples
#' morie_extreme_value_copula(0.4, 0.7, "gumbel", 2)$cdf
#' @export
morie_extreme_value_copula <- function(u, v, A = "gumbel", theta = 2) {
  u <- as.numeric(u)
  v <- as.numeric(v)
  if (any(u <= 0 | u >= 1) || any(v <= 0 | v >= 1)) {
    stop("u and v must lie strictly inside (0, 1).", call. = FALSE)
  }
  th <- as.numeric(theta)
  fn <- if (is.function(A)) {
    A
  } else {
    switch(match.arg(A, c("gumbel", "galambos", "independence")),
      gumbel = {
        if (th < 1) stop("gumbel/logistic theta must be >= 1.", call. = FALSE)
        function(t) (t^th + (1 - t)^th)^(1 / th)
      },
      galambos = {
        if (th <= 0) stop("galambos delta must be positive.", call. = FALSE)
        function(t) 1 - (t^(-th) + (1 - t)^(-th))^(-1 / th)
      },
      independence = function(t) rep(1, length(t))
    )
  }
  grid <- seq(0.001, 0.999, length.out = 199L)
  Av <- as.numeric(fn(grid))
  valid <- all(Av <= 1 + 1e-8) && all(Av >= pmax(grid, 1 - grid) - 1e-8)
  luv <- log(u * v)
  list(
    cdf = exp(luv * as.numeric(fn(log(v) / luv))),
    pickands_at_half = as.numeric(fn(0.5)), valid_pickands = valid,
    A = if (is.function(A)) "callable" else A, theta = th,
    method = "Extreme-value copula from a Pickands dependence function"
  )
}

# Kaplan-Meier survival evaluated at the observed times.
#' Kaplan-Meier survival evaluated at the observed times
#'
#' A step of the copula_native implementation. Called by \code{morie_copula_survival}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param time A vector; indexed elementwise.
#' @param event A vector; indexed elementwise.
#' @return A list with \code{grid}, \code{vals}.
#' @export
.morie_cop_km <- function(time, event) {
  o <- order(time)
  tt <- time[o]
  ee <- event[o]
  n <- length(tt)
  s <- 1
  at_risk <- n
  grid <- 0
  vals <- 1
  for (i in seq_len(n)) {
    if (ee[i] == 1) {
      s <- s * (1 - 1 / max(at_risk, 1))
      grid <- c(grid, tt[i])
      vals <- c(vals, s)
    }
    at_risk <- at_risk - 1
  }
  list(grid = grid, vals = vals)
}

#' Survival copula for paired event times
#'
#' \eqn{S(t_1, t_2) = C(S_1(t_1), S_2(t_2))} with Kaplan-Meier
#' margins -- Sklar's theorem on the survival scale. The family choice
#' says where in time the association acts: Clayton lower-tail (early
#' events), Gumbel upper-tail (late), Frank neither. Mirrors
#' \code{morie.fn.copfr} and, with family = "clayton",
#' \code{morie.fn.clyfr}.
#'
#' @param time1,time2 numeric observed times.
#' @param event1,event2 binary event indicators.
#' @param family copula family.
#' @param theta parameter; inverted from the sample tau if NULL.
#' @return list: family, theta, tau_sample, joint_survival, s1, s2,
#'   n_pairs, method.
#' @references Clayton, D. G. (1978). Biometrika 65(1), 141-151.
#'   Sklar, A. (1959). Publ. Inst. Statist. Univ. Paris 8, 229-231.
#' @examples
#' set.seed(1)
#' t1 <- rexp(30)
#' t2 <- t1 + rexp(30, 5)
#' morie_copula_survival(t1, rep(1, 30), t2, rep(1, 30))$tau_sample
#' @export
morie_copula_survival <- function(time1, event1, time2, event2,
                                  family = "clayton", theta = NULL) {
  family <- match.arg(family, .morie_copula_families)
  t1 <- as.numeric(time1)
  t2 <- as.numeric(time2)
  e1 <- as.numeric(event1)
  e2 <- as.numeric(event2)
  n <- length(t1)
  if (length(t2) != n || length(e1) != n || length(e2) != n) {
    stop("all four inputs must have the same length.", call. = FALSE)
  }
  if (n < 5L) stop("need at least 5 pairs.", call. = FALSE)
  if (any(t1 <= 0) || any(t2 <= 0)) stop("times must be positive.", call. = FALSE)
  tau_hat <- stats::cor(t1, t2, method = "kendall")
  if (is.null(theta) && family != "independence") {
    theta <- morie_tau_to_theta(family, tau_hat)
  }
  k1 <- .morie_cop_km(t1, e1)
  k2 <- .morie_cop_km(t2, e2)
  s1 <- pmax(k1$vals[findInterval(t1, k1$grid)], 1e-8)
  s2 <- pmax(k2$vals[findInterval(t2, k2$grid)], 1e-8)
  list(
    family = family, theta = if (is.null(theta)) NULL else as.numeric(theta),
    tau_sample = as.numeric(tau_hat),
    joint_survival = morie_copula_cdf(family, s1, s2, theta),
    s1 = s1, s2 = s2, n_pairs = as.integer(n),
    method = sprintf("Survival copula (%s) on Kaplan-Meier margins", family)
  )
}

#' COPOD: copula-based outlier detection
#'
#' Per-feature empirical CDFs and their mirrors give tail
#' probabilities; the score is the larger of the summed negative log
#' tail probabilities, with a per-dimension skewness correction. Being
#' rank-based it needs no distance metric and no scaling. Mirrors
#' \code{morie.fn.copod}.
#'
#' @param X numeric matrix (n x d).
#' @param skew_correction logical; use the skew-selected tail.
#' @return list: scores, left_tail, right_tail, skewness, n, d, method.
#' @references Li, Z., Zhao, Y., Botta, N., Ionescu, C. & Hu, X. (2020).
#'   COPOD: copula-based outlier detection. IEEE ICDM, 1118-1123.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(90), ncol = 3)
#' X[1, ] <- 9
#' which.max(morie_copod(X)$scores)
#' @export
morie_copod <- function(X, skew_correction = TRUE) {
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  n <- nrow(X)
  d <- ncol(X)
  if (n < 3L) stop("need at least 3 samples.", call. = FALSE)
  if (!all(is.finite(X))) stop("X must be finite.", call. = FALSE)
  left <- apply(X, 2, function(z) rank(z) / (n + 1))
  right <- apply(X, 2, function(z) rank(-z) / (n + 1))
  left <- matrix(left, n, d)
  right <- matrix(right, n, d)
  nl <- -log(left)
  nr <- -log(right)
  skew <- apply(X, 2, function(z) {
    m <- mean(z)
    s <- stats::sd(z)
    if (s == 0) 0 else mean((z - m)^3) / (mean((z - m)^2))^1.5
  })
  scores <- pmax(rowSums(nl), rowSums(nr))
  if (isTRUE(skew_correction)) {
    pick <- ifelse(matrix(rep(skew, each = n), n, d) < 0, nl, nr)
    scores <- pmax(scores, rowSums(pick))
  }
  list(
    scores = scores, left_tail = left, right_tail = right,
    skewness = skew, n = as.integer(n), d = as.integer(d),
    method = "COPOD empirical-copula outlier scores"
  )
}
