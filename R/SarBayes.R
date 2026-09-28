.sb_logdet_grid <- function(W, grid) {
  n <- nrow(W)
  d <- rowSums(W != 0)
  sym <- min(d) > 0 && max(abs(W * d - t(W * d))) <= 1e-12
  if (sym) {
    M <- W * sqrt(outer(d, d, "/"))
    lam <- eigen(M, symmetric = TRUE, only.values = TRUE)$values
    return(vapply(grid, function(r) sum(log(1 - r * lam)), numeric(1)))
  }
  coarse <- -0.99 + 0.01 * (0:198)
  lc <- vapply(coarse, function(r) as.numeric(determinant(diag(n) - r * W, logarithm = TRUE)$modulus), numeric(1))
  vapply(grid, function(r) {
    t <- min(max((r + 0.99) / 0.01, 0), 197.999999)
    i <- floor(t)
    lc[i + 1] + (t - i) * (lc[i + 2] - lc[i + 1])
  }, numeric(1))
}

.sb_tnorm <- function(m, s, lo, hi, u) {
  a <- if (lo > -Inf) (lo - m) / s else -Inf
  b <- if (hi < Inf) (hi - m) / s else Inf
  if (b == Inf) {
    if (a > -Inf) return(m - s * stats::qnorm(u * stats::pnorm(-a)))
    return(m + s * stats::qnorm(u))
  }
  if (a == -Inf) return(m + s * stats::qnorm(u * stats::pnorm(b)))
  fa <- stats::pnorm(a)
  fb <- stats::pnorm(b)
  if (fb - fa > 1e-12) return(m + s * stats::qnorm(fa + u * (fb - fa)))
  m + s * (a + u * (b - a))
}

.sb_draw_rho <- function(grid, lndet, lnprior, c0, c1, c2, power, u) {
  den <- if (power > 0) {
    lndet + lnprior - power * log(pmax(c0 - 2 * grid * c1 + grid^2 * c2, 1e-300))
  } else {
    lndet + lnprior - 0.5 * (c0 - 2 * grid * c1 + grid^2 * c2)
  }
  w <- exp(den - max(den))
  acc <- 0
  target <- u * sum(w)
  for (i in seq_along(w)) {
    acc <- acc + w[i]
    if (acc >= target) return(grid[i])
  }
  grid[length(grid)]
}

.sb_setup <- function(W, a1, a2) {
  grid <- -0.999 + 0.001 * (0:1998)
  lb <- lgamma(a1 + a2) - lgamma(a1) - lgamma(a2) - (a1 + a2 - 1) * log(2)
  list(grid = grid, lndet = .sb_logdet_grid(W, grid),
       lnprior = lb + (a1 - 1) * log(1 + grid) + (a2 - 1) * log(1 - grid))
}

.sb_latent <- function(y, X, W, ndraw, burn_in, seed, a1, a2, lower, upper, sigma_fixed, cuts = NULL,
                       observed = NULL, ycat = NULL, prior_var = NULL, exact = FALSE) {
  n <- nrow(X)
  k <- ncol(X)
  sp <- .sb_setup(W, a1, a2)
  pp <- if (is.null(prior_var)) 0 else 1 / prior_var
  xtxi <- solve(crossprod(X) + diag(pp, k))
  Lb <- t(chol(xtxi))
  xtxi0 <- solve(crossprod(X))
  total <- ndraw + burn_in
  u <- .morie_random_uniform(total * (3 * n + k + 2), seed = seed)
  pos <- 0
  latent <- any(!is.na(lower))
  WW <- W + t(W)
  WtW <- crossprod(W)
  rho <- 0
  beta <- numeric(k)
  s2 <- 1
  z <- if (is.null(observed)) numeric(n) else ifelse(is.na(observed), 0, observed)
  kb <- matrix(0, ndraw, k)
  kr <- numeric(ndraw)
  ks <- numeric(ndraw)
  kc <- NULL
  for (it in seq_len(total)) {
    A <- diag(n) - rho * W
    if (latent) {
      mu <- as.vector(solve(A, X %*% beta))
      H <- (diag(n) - rho * WW + rho^2 * WtW) / s2
      if (!is.null(cuts)) {
        full <- c(-Inf, cuts, Inf)
        lower <- full[ycat]
        upper <- full[ycat + 1]
      }
    }
    for (i in seq_len(n)) {
      pos <- pos + 1
      if (!latent || is.na(lower[i])) next
      cm <- mu[i] - (sum(H[i, ] * (z - mu)) - H[i, i] * (z[i] - mu[i])) / H[i, i]
      z[i] <- .sb_tnorm(cm, 1 / sqrt(H[i, i]), lower[i], upper[i], u[pos])
    }
    if (!is.null(cuts)) {
      J <- length(cuts) + 1
      for (j in seq_len(J - 2)) {
        lo <- max(c(z[ycat == j + 1], cuts[j]))
        hi <- min(c(z[ycat == j + 2], if (j + 2 <= J - 1) cuts[j + 2] else Inf))
        pos <- pos + 1
        if (hi > lo && hi < Inf) cuts[j + 1] <- lo + u[pos] * (hi - lo)
      }
    }
    Wz <- as.vector(W %*% z)
    xz <- as.vector(crossprod(X, z))
    xw <- as.vector(crossprod(X, Wz))
    Mi <- if (exact) xtxi else xtxi0
    pz <- as.vector(Mi %*% xz)
    pw <- as.vector(Mi %*% xw)
    pos <- pos + 1
    rho <- .sb_draw_rho(sp$grid, sp$lndet, sp$lnprior, sum(z^2) - sum(xz * pz), sum(z * Wz) - sum(xz * pw),
                        sum(Wz^2) - sum(xw * pw), if (exact) 0 else (n - k) / 2, u[pos])
    Az <- z - rho * Wz
    b0 <- as.vector(xtxi %*% crossprod(X, Az))
    e <- Az - as.vector(X %*% b0)
    if (!sigma_fixed) {
      s2 <- sum(e^2) / sum(stats::qnorm(u[pos + seq_len(n - k)])^2)
    }
    pos <- pos + n - k
    beta <- b0 + sqrt(s2) * as.vector(Lb %*% stats::qnorm(u[pos + seq_len(k)]))
    pos <- pos + k
    if (it > burn_in) {
      kb[it - burn_in, ] <- beta
      kr[it - burn_in] <- rho
      ks[it - burn_in] <- s2
      if (!is.null(cuts)) kc <- rbind(kc, cuts)
    }
  }
  list(kb = kb, kr = kr, ks = ks, kc = kc)
}

.sb_summ <- function(r, sigma = TRUE) {
  out <- list(beta = colMeans(r$kb), beta_sd = apply(r$kb, 2, stats::sd), rho = mean(r$kr),
              rho_sd = stats::sd(r$kr), rho_draws = r$kr, beta_draws = r$kb)
  if (sigma) out$sigma2 <- mean(r$ks)
  out
}

#' Bayesian spatial autoregressive models by Gibbs sampling
#'
#' \code{SarProbitGibbs}, \code{SarTobitGibbs} and \code{SarOrderedProbitGibbs}:
#' LeSage-Pace SAR probit, Tobit and ordered probit with truncated normal
#' latent draws and griddy Gibbs for rho. \code{SpatialBayesGibbs}: Bayesian
#' spatial lag, error and Durbin regressions with flat priors. The
#' uniforms come from the Philox stream \code{seed}, so the draws equal the
#' Python arm \code{morie.fn.sarbayes}.
#'
#' @param y Response (0/1, censored at 0, or categories 1 to J).
#' @param X Design matrix with the intercept in the first column.
#' @param W Spatial weights (n by n).
#' @param ndraw Retained draws.
#' @param burn_in Burn-in draws.
#' @param seed Philox seed.
#' @param a1,a2 Beta prior on rho over (-1, 1).
#' @param model \code{"lag"}, \code{"error"} or \code{"durbin"}.
#' @param prior_var Prior variance of the probit coefficients.
#' @param method \code{"exact"} (unit latent variance) or \code{"lesage"}
#'   (concentrated rho conditional of LeSage and Pace).
#' @return A list of posterior means, standard deviations and draws.
#' @references LeSage, J. P. and Pace, R. K. (2009). Introduction to Spatial
#'   Econometrics. CRC Press.
#'
#'   Albert, J. H. and Chib, S. (1993). Bayesian analysis of binary and
#'   polychotomous response data. Journal of the American Statistical
#'   Association 88, 669-679.
#' @examples
#' W <- rbind(c(0, 1, 0, 0), c(0.5, 0, 0.5, 0), c(0, 0.5, 0, 0.5), c(0, 0, 1, 0))
#' X <- cbind(1, c(1, 0.5, -0.5, -1))
#' SpatialBayesGibbs(c(2, 1.4, 0.3, -0.5), X, W, ndraw = 50, burn_in = 10)$beta
#' @export
SarProbitGibbs <- function(y, X, W, ndraw = 1000, burn_in = 200, seed = 0, a1 = 1, a2 = 1, prior_var = 1e12,
                           method = "exact") {
  if (!method %in% c("exact", "lesage")) stop("method must be 'exact' or 'lesage'")
  X <- as.matrix(X)
  r <- .sb_latent(y, X, W, ndraw, burn_in, seed, a1, a2, ifelse(y > 0, 0, -Inf), ifelse(y > 0, Inf, 0), TRUE,
                  prior_var = prior_var, exact = method == "exact")
  .sb_summ(r, FALSE)
}

#' @rdname SarProbitGibbs
#' @export
SarTobitGibbs <- function(y, X, W, ndraw = 1000, burn_in = 200, seed = 0, a1 = 1, a2 = 1) {
  X <- as.matrix(X)
  lower <- ifelse(y > 0, NA, -Inf)
  r <- .sb_latent(y, X, W, ndraw, burn_in, seed, a1, a2, lower, rep(0, length(y)), FALSE,
                  observed = ifelse(y > 0, y, NA))
  .sb_summ(r)
}

#' @rdname SarProbitGibbs
#' @export
SarOrderedProbitGibbs <- function(y, X, W, ndraw = 1000, burn_in = 200, seed = 0, a1 = 1, a2 = 1,
                                  prior_var = 1e12, method = "exact") {
  if (!method %in% c("exact", "lesage")) stop("method must be 'exact' or 'lesage'")
  X <- as.matrix(X)
  J <- max(y)
  cuts <- c(0, seq_len(J - 2))
  r <- .sb_latent(y, X, W, ndraw, burn_in, seed, a1, a2, rep(0, length(y)), rep(0, length(y)), TRUE,
                  cuts = cuts, ycat = y, prior_var = prior_var, exact = method == "exact")
  out <- .sb_summ(r, FALSE)
  out$cutpoints <- unname(colMeans(r$kc))
  out
}

#' @rdname SarProbitGibbs
#' @export
SpatialBayesGibbs <- function(y, X, W, model = "lag", ndraw = 1000, burn_in = 200, seed = 0, a1 = 1, a2 = 1) {
  X <- as.matrix(X)
  n <- length(y)
  if (model == "durbin") X <- cbind(X, W %*% X[, -1, drop = FALSE])
  k <- ncol(X)
  if (model %in% c("lag", "durbin")) {
    r <- .sb_latent(y, X, W, ndraw, burn_in, seed, a1, a2, rep(NA, n), rep(NA, n), FALSE, observed = y)
    return(.sb_summ(r))
  }
  if (model != "error") stop("model must be 'lag', 'error' or 'durbin'")
  sp <- .sb_setup(W, a1, a2)
  u <- .morie_random_uniform((ndraw + burn_in) * (n + k + 2), seed = seed)
  pos <- 0
  lam <- 0
  s2 <- 1
  Wy <- as.vector(W %*% y)
  WX <- W %*% X
  kb <- matrix(0, ndraw, k)
  kr <- numeric(ndraw)
  ks <- numeric(ndraw)
  for (it in seq_len(ndraw + burn_in)) {
    ys <- y - lam * Wy
    Xs <- X - lam * WX
    xtxi <- solve(crossprod(Xs))
    b0 <- as.vector(xtxi %*% crossprod(Xs, ys))
    beta <- b0 + sqrt(s2) * as.vector(t(chol(xtxi)) %*% stats::qnorm(u[pos + seq_len(k)]))
    pos <- pos + k
    e <- ys - as.vector(Xs %*% beta)
    s2 <- sum(e^2) / sum(stats::qnorm(u[pos + seq_len(n)])^2)
    pos <- pos + n
    uu <- as.vector(y - X %*% beta)
    Wu <- as.vector(W %*% uu)
    lp <- sp$lnprior - (sum(uu^2) - 2 * sp$grid * sum(uu * Wu) + sp$grid^2 * sum(Wu^2)) / (2 * s2)
    pos <- pos + 1
    lam <- .sb_draw_rho(sp$grid, sp$lndet, lp, 1, 0, 0, 0, u[pos])
    if (it > burn_in) {
      kb[it - burn_in, ] <- beta
      kr[it - burn_in] <- lam
      ks[it - burn_in] <- s2
    }
  }
  .sb_summ(list(kb = kb, kr = kr, ks = ks))
}
