#' Fay-Herriot and Battese-Harter-Fuller small-area EBLUPs
#'
#' \code{FayHerriot}: area-level EBLUP
#' \eqn{\theta_d = x_d'\beta + \gamma_d (y_d - x_d'\beta)},
#' \eqn{\gamma_d = A / (A + D_d)}, with known sampling variances \code{vardir};
#' \eqn{A} by Fisher scoring from \code{median(vardir)} (\code{"REML"},
#' \code{"ML"} or the \code{"FH"} moment equation), truncated at 0, stopping
#' at relative change \code{tol}; MSE \eqn{g_1 + g_2 + 2 g_3} (Prasad-Rao;
#' ML and FH add the Datta-Lahiri and Datta-Rao-Smith bias terms), as
#' \code{sae::eblupFH} and \code{mseFH}. \code{BhfEblup}: unit-level
#' nested-error EBLUP of area means,
#' \eqn{f_d \bar y_d + (\bar X_d - f_d \bar x_d)'\beta + (1 - f_d) u_d},
#' variance components by REML or ML from the analytic profile score in
#' \eqn{\log(\sigma_u^2 / \sigma_e^2)} (bisection; a non-positive score at
#' the lower end gives \eqn{\sigma_u^2 = 0}), as \code{sae::eblupBHF}.
#' Identical to the Python arm \code{morie.fn.smallarea} (Fay and Herriot
#' 1979; Battese, Harter and Fuller 1988; Datta, Rao and Smith 2005).
#'
#' @param y Direct estimates (\code{FayHerriot}) or unit responses.
#' @param X Design matrix including the intercept column.
#' @param vardir Sampling variances.
#' @param method \code{"REML"}, \code{"ML"} or (\code{FayHerriot}) \code{"FH"}.
#' @param maxiter Maximum scoring iterations.
#' @param tol Relative convergence tolerance.
#' @param area Area label of every unit.
#' @param xbar_pop Matrix of population means, one row per area.
#' @param areas Area labels in the row order of \code{xbar_pop}.
#' @param popsize Population sizes (sampling fraction 0 when \code{NULL}).
#' @return List.
#' @references Fay, R. E. and Herriot, R. A. (1979). Estimates of income for
#'   small places: an application of James-Stein procedures to census data.
#'   Journal of the American Statistical Association 74, 269-277.
#'
#'   Battese, G. E., Harter, R. M. and Fuller, W. A. (1988). An
#'   error-components model for prediction of county crop areas using survey
#'   and satellite data. Journal of the American Statistical Association 83,
#'   28-36.
#'
#'   Datta, G. S., Rao, J. N. K. and Smith, D. D. (2005). On measuring the
#'   variability of small area estimators under a basic area level model.
#'   Biometrika 92, 183-196.
#' @examples
#' FayHerriot(c(1, 2, 3, 5), matrix(1, 4, 1), rep(1, 4))$A
#' BhfEblup(c(1, 2, 2, 3, 5, 6), matrix(1, 6, 1), c(0, 0, 1, 1, 2, 2), matrix(1, 3, 1))$sigma2_u
#' @export
FayHerriot <- function(y, X, vardir, method = "REML", maxiter = 100L, tol = 1e-10) {
  if (!method %in% c("REML", "ML", "FH")) stop("method must be REML, ML or FH", call. = FALSE)
  y <- as.numeric(y)
  X <- as.matrix(X)
  D <- as.numeric(vardir)
  m <- length(y)
  p <- ncol(X)
  gls <- function(A) {
    Vi <- 1 / (A + D)
    Q <- solve(crossprod(X, Vi * X))
    beta <- as.vector(Q %*% crossprod(X, Vi * y))
    list(Vi = Vi, Q = Q, beta = beta, res = as.vector(y - X %*% beta))
  }
  A <- stats::median(D)
  k <- 0L
  diff <- tol + 1
  while (diff > tol && k < maxiter) {
    k <- k + 1L
    g <- gls(A)
    if (method == "FH") {
      s <- sum(g$res^2 * g$Vi) - (m - p)
      Fi <- sum(g$Vi)
    } else {
      Py <- g$Vi * g$res
      if (method == "ML") {
        s <- -0.5 * sum(g$Vi) + 0.5 * sum(Py^2)
        Fi <- 0.5 * sum(g$Vi^2)
      } else {
        XV <- g$Vi * X
        P <- diag(g$Vi, m) - XV %*% g$Q %*% t(XV)
        s <- -0.5 * sum(diag(P)) + 0.5 * sum(Py^2)
        Fi <- 0.5 * sum(P * t(P))
      }
    }
    An <- A + s / Fi
    diff <- if (A != 0) abs((An - A) / A) else abs(An - A)
    A <- An
  }
  converged <- diff <= tol
  A <- max(A, 0)
  g <- gls(A)
  gam <- A * g$Vi
  ll <- -0.5 * sum(log(2 * pi * (A + D)) + g$res^2 / (A + D))
  B <- D * g$Vi
  S2 <- sum(g$Vi^2)
  if (method == "FH") {
    S1 <- sum(g$Vi)
    varA <- 2 * m / S1^2
    bias <- 2 * (m * S2 - S1^2) / S1^3
  } else {
    varA <- 2 / S2
    bias <- if (method == "ML") -sum(diag(g$Q %*% crossprod(X, g$Vi^2 * X))) / S2 else 0
  }
  g2 <- B^2 * rowSums((X %*% g$Q) * X)
  mse <- D * (1 - B) + g2 + 2 * B^2 * varA / (A + D) - bias * B^2
  list(A = A, beta = g$beta, se_beta = sqrt(diag(g$Q)), eblup = y - g$res + gam * g$res, gamma = gam, mse = mse,
       loglik = ll, aic = -2 * ll + 2 * (p + 1), bic = -2 * ll + (p + 1) * log(m), iterations = k,
       converged = converged, method = method)
}

.bhf_profile <- function(rho, groups, p, reml) {
  XtX <- matrix(0, p, p)
  Xty <- numeric(p)
  for (g in groups) {
    cc <- rho / (1 + length(g$y) * rho)
    sx <- colSums(g$X)
    XtX <- XtX + crossprod(g$X) - cc * tcrossprod(sx)
    Xty <- Xty + as.vector(crossprod(g$X, g$y)) - cc * sx * sum(g$y)
  }
  Q <- solve(XtX)
  beta <- as.vector(Q %*% Xty)
  rss <- 0
  drss <- 0
  dld <- 0
  dq <- 0
  for (g in groups) {
    n <- length(g$y)
    r <- as.vector(g$y - g$X %*% beta)
    sx <- colSums(g$X)
    rss <- rss + sum(r^2) - rho / (1 + n * rho) * sum(r)^2
    drss <- drss - sum(r)^2 / (1 + n * rho)^2
    dld <- dld + n / (1 + n * rho)
    dq <- dq - sum(sx * (Q %*% sx)) / (1 + n * rho)^2
  }
  N <- sum(vapply(groups, function(g) length(g$y), 0L))
  dof <- if (reml) N - p else N
  s2 <- rss / dof
  ld <- sum(vapply(groups, function(g) log1p(length(g$y) * rho), 0))
  ll <- -0.5 * (dof * (log(2 * pi * s2) + 1) + ld + (if (reml) as.numeric(determinant(XtX)$modulus) else 0))
  list(ll = ll, beta = beta, s2 = s2, Q = Q, score = -0.5 * (dof * drss / rss + dld + (if (reml) dq else 0)))
}

#' @rdname FayHerriot
#' @export
BhfEblup <- function(y, X, area, xbar_pop, areas = NULL, popsize = NULL, method = "REML") {
  if (!method %in% c("REML", "ML")) stop("method must be REML or ML", call. = FALSE)
  y <- as.numeric(y)
  X <- as.matrix(X)
  xbar_pop <- as.matrix(xbar_pop)
  p <- ncol(X)
  labels <- if (is.null(areas)) unique(area) else areas
  groups <- list()
  for (lab in labels) {
    ix <- which(area == lab)
    if (length(ix)) groups[[length(groups) + 1]] <- list(X = X[ix, , drop = FALSE], y = y[ix])
  }
  reml <- method == "REML"
  lo <- -30
  if (.bhf_profile(exp(lo), groups, p, reml)$score <= 0) {
    rho <- 0
  } else {
    a <- lo
    b <- 15
    for (i in 1:200) {
      mid <- 0.5 * (a + b)
      if (mid == a || mid == b) break
      if (.bhf_profile(exp(mid), groups, p, reml)$score > 0) a <- mid else b <- mid
    }
    rho <- exp(0.5 * (a + b))
  }
  f <- .bhf_profile(rho, groups, p, reml)
  s2u <- rho * f$s2
  out <- t(vapply(seq_along(labels), function(k) {
    ix <- which(area == labels[k])
    xb <- xbar_pop[k, ]
    if (!length(ix)) return(c(sum(xb * f$beta), 0, 0))
    n <- length(ix)
    xs <- colMeans(X[ix, , drop = FALSE])
    ud <- s2u / (s2u + f$s2 / n) * (mean(y[ix]) - sum(xs * f$beta))
    fr <- if (is.null(popsize)) 0 else n / popsize[k]
    c(fr * mean(y[ix]) + sum((xb - fr * xs) * f$beta) + (1 - fr) * ud, ud, n)
  }, numeric(3)))
  list(eblup = out[, 1], random_effects = out[, 2], beta = f$beta, se_beta = sqrt(f$s2 * diag(f$Q)),
       sigma2_u = s2u, sigma2_e = f$s2, loglik = f$ll, sample_sizes = as.integer(out[, 3]), areas = labels,
       method = method)
}

#' Empirical-Bayes disease rates and the Potthoff-Whittinghill test
#'
#' \code{MarshallEb}: Marshall's global method-of-moments empirical-Bayes
#' rates (Poisson or binomial), as \code{spdep::EBest}. \code{PoissonGammaEb}:
#' Clayton-Kaldor Poisson-gamma relative risks, the negative binomial fitted
#' by maximum likelihood (IRLS for \eqn{\beta} alternating with Newton for
#' \eqn{\alpha}), posterior means and medians, as \code{SpatialEpi::eBayes}.
#' \code{PotthoffWhittinghill}: \eqn{T = E_+ \sum O_i (O_i - 1) / E_i} with
#' asymptotic mean \eqn{O_+ (O_+ - 1)} and variance
#' \eqn{2 (k - 1) O_+ (O_+ - 1)}, as \code{DCluster::pottwhitt.stat}.
#' Identical to the Python arm \code{morie.fn.smallarea}.
#'
#' @param cases,population Case counts and populations at risk.
#' @param family \code{"poisson"} or \code{"binomial"}.
#' @param y,E Observed and expected counts.
#' @param X Optional covariate matrix (intercept added).
#' @param tol,maxit Relative parameter tolerance and iteration limit.
#' @param observed,expected Observed and expected counts.
#' @return List.
#' @references Marshall, R. J. (1991). Mapping disease and mortality rates
#'   using empirical Bayes estimators. Applied Statistics 40, 283-294.
#'
#'   Clayton, D. and Kaldor, J. (1987). Empirical Bayes estimates of
#'   age-standardized relative risks for use in disease mapping. Biometrics
#'   43, 671-681.
#'
#'   Potthoff, R. F. and Whittinghill, M. (1966). Testing for homogeneity:
#'   II. The Poisson distribution. Biometrika 53, 183-190.
#' @examples
#' MarshallEb(c(2, 8), c(100, 100))$estimate
#' PoissonGammaEb(c(2, 3, 10, 1, 7, 4), c(3, 4, 5, 2.5, 4.5, 4))$RR
#' PotthoffWhittinghill(c(3, 3, 3, 3), rep(3, 4))$T
#' @export
MarshallEb <- function(cases, population, family = "poisson") {
  n <- as.numeric(cases)
  x <- as.numeric(population)
  if (any(x <= 0)) stop("non-positive risk population", call. = FALSE)
  m <- length(n)
  raw <- n / x
  xs <- sum(x)
  b <- sum(n) / xs
  s2 <- sum(x * (raw - b)^2 / xs)
  if (family == "poisson") {
    a <- max(0, s2 - b / (xs / m))
    est <- b + a * (raw - b) / (a + b / x)
  } else if (family == "binomial") {
    xm <- xs / m
    rho <- (x * s2 - (x / xm) * (b * (1 - b))) / ((x - 1) * s2 + ((xm - x) / xm) * (b * (1 - b)))
    est <- rho * raw + (1 - rho) * b
    a <- s2
  } else {
    stop("family must be poisson or binomial", call. = FALSE)
  }
  list(raw = raw, estimate = est, a = a, b = b)
}

#' @rdname MarshallEb
#' @export
PoissonGammaEb <- function(y, E, X = NULL, tol = 1e-12, maxit = 1000L) {
  y <- as.numeric(y)
  E <- as.numeric(E)
  n <- length(y)
  Xm <- cbind(1, if (is.null(X)) NULL else as.matrix(X))
  p <- ncol(Xm)
  off <- log(E)
  loglik <- function(beta, th) {
    mu <- as.vector(exp(Xm %*% beta + off))
    sum(ifelse(mu > 0, lgamma(th + y) - lgamma(th) - lgamma(y + 1) + th * log(th) + y * log(mu) -
                 (th + y) * log(th + mu), 0))
  }
  irls <- function(beta, th) {
    for (i in 1:100) {
      eta <- as.vector(Xm %*% beta + off)
      mu <- exp(eta)
      w <- mu / (1 + mu / th)
      z <- eta - off + (y - mu) / mu
      nb <- as.vector(solve(crossprod(Xm, w * Xm), crossprod(Xm, w * z)))
      done <- max(abs(nb - beta)) < 1e-14 * (1 + max(abs(beta)))
      beta <- nb
      if (done) break
    }
    beta
  }
  theta_ml <- function(mu, th) {
    for (i in 1:100) {
      sc <- sum(digamma(th + y) - digamma(th) + log(th) + 1 - log(th + mu) - (y + th) / (mu + th))
      inf <- sum(-trigamma(th + y) + trigamma(th) - 1 / th + 2 / (mu + th) - (y + th) / (mu + th)^2)
      step <- sc / inf
      nt <- th + step
      while (nt <= 0) {
        step <- step / 2
        nt <- th + step
      }
      if (abs(nt - th) < 1e-14 * th) return(nt)
      th <- nt
    }
    th
  }
  beta <- c(log(sum(y) / sum(E)), rep(0, p - 1))
  mu <- exp(beta[1] + off)
  th <- n / sum((y / mu - 1)^2)
  it <- 0L
  for (it in seq_len(maxit)) {
    b_old <- beta
    th_old <- th
    beta <- irls(beta, th)
    th <- theta_ml(as.vector(exp(Xm %*% beta + off)), th)
    if (abs(th - th_old) <= tol * th && max(abs(beta - b_old)) <= tol * (1 + max(abs(beta)))) break
  }
  ll <- loglik(beta, th)
  muhat <- as.vector(exp(Xm %*% beta))
  wgt <- E * muhat / (th + E * muhat)
  smr <- y / E
  list(RR = wgt * smr + (1 - wgt) * muhat, RRmed = stats::qgamma(0.5, th + y, (th + E * muhat) / muhat),
       beta = beta, alpha = th, SMR = smr, mu = muhat, loglik = ll, iterations = it)
}

#' @rdname MarshallEb
#' @export
PotthoffWhittinghill <- function(observed, expected) {
  O <- as.numeric(observed)
  E <- as.numeric(expected)
  Tst <- sum(E) * sum(O * (O - 1) / E)
  tot <- sum(O)
  mn <- tot * (tot - 1)
  v <- 2 * (length(O) - 1) * mn
  z <- (Tst - mn) / sqrt(v)
  lo <- stats::pnorm(z)
  list(T = Tst, mean = mn, variance = v, z = z, p_value = min(lo, 1 - lo), p_upper = 1 - lo)
}
