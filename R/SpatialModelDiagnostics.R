# Shared kernels of the spatial-model diagnostic front ends (Sar*, Sem*,
# Sac*, Sdm*, Slx*); R arm of morie/fn/_spdiag.py.

.sxd_logdet <- function(W, a) {
  if (a == 0) 0 else as.numeric(determinant(diag(nrow(W)) - a * W, logarithm = TRUE)$modulus)
}

.sxd_bounds <- function(W) {
  ev <- Re(eigen(as.matrix(W), only.values = TRUE)$values)
  c(1 / min(ev), 1 / max(ev))
}

.sxd_info <- function(X, W, beta, rho, lam, s2, lag, err) {
  X <- as.matrix(X)
  W <- as.matrix(W)
  n <- nrow(X)
  p <- ncol(X)
  B <- diag(n) - (if (err) lam else 0) * W
  Binv <- if (err) solve(B) else diag(n)
  BX <- B %*% X
  k <- p + lag + err + 1
  info <- matrix(0, k, k)
  info[1:p, 1:p] <- crossprod(BX) / s2
  ir <- p + 1
  il <- p + 1 + lag
  if (lag) {
    WA <- W %*% solve(diag(n) - rho * W)
    C <- if (err) B %*% WA %*% Binv else WA
    g <- as.vector(B %*% (WA %*% (X %*% as.numeric(beta))))
    info[1:p, ir] <- info[ir, 1:p] <- as.vector(crossprod(BX, g)) / s2
    info[ir, ir] <- sum(C * t(C)) + sum(C * C) + sum(g^2) / s2
    info[ir, k] <- info[k, ir] <- sum(diag(WA)) / s2
  }
  if (err) {
    WB <- W %*% Binv
    info[il, il] <- sum(WB * t(WB)) + sum(WB * WB)
    info[il, k] <- info[k, il] <- sum(diag(WB)) / s2
    if (lag) info[ir, il] <- info[il, ir] <- sum(WB * t(C)) + sum(WB * C)
  }
  info[k, k] <- n / (2 * s2^2)
  info
}

.sxd_cov <- function(X, W, beta, rho, lam, s2, lag, err) {
  info <- .sxd_info(X, W, beta, rho, lam, s2, lag, err)
  V <- solve(info)
  list(cov = V, information = info, se = sqrt(diag(V)))
}

.sxd_brent <- function(f, lo, hi, tol = 1e-12, max_iter = 500) {
  g <- (3 - sqrt(5)) / 2
  a <- lo
  b <- hi
  x <- a + g * (b - a)
  w <- x
  v <- x
  fx <- f(x)
  fw <- fx
  fv <- fx
  d <- 0
  e <- 0
  for (it in seq_len(max_iter)) {
    m <- 0.5 * (a + b)
    t1 <- tol * abs(x) + 1e-15
    t2 <- 2 * t1
    if (abs(x - m) <= t2 - 0.5 * (b - a)) break
    golden <- TRUE
    if (abs(e) > t1) {
      r <- (x - w) * (fx - fv)
      q <- (x - v) * (fx - fw)
      pp <- (x - v) * q - (x - w) * r
      q <- 2 * (q - r)
      if (q > 0) pp <- -pp
      q <- abs(q)
      if (abs(pp) < abs(0.5 * q * e) && q * (a - x) < pp && pp < q * (b - x)) {
        e <- d
        d <- pp / q
        u <- x + d
        if (u - a < t2 || b - u < t2) d <- if (x < m) t1 else -t1
        golden <- FALSE
      }
    }
    if (golden) {
      e <- if (x < m) b - x else a - x
      d <- g * e
    }
    u <- x + (if (abs(d) >= t1) d else if (d > 0) t1 else -t1)
    fu <- f(u)
    if (fu <= fx) {
      if (u < x) b <- x else a <- x
      v <- w
      w <- x
      x <- u
      fv <- fw
      fw <- fx
      fx <- fu
    } else {
      if (u < x) a <- u else b <- u
      if (fu <= fw || w == x) {
        v <- w
        w <- u
        fv <- fw
        fw <- fu
      } else if (fu <= fv || v == x || v == w) {
        v <- u
        fv <- fu
      }
    }
  }
  c(x, fx)
}

.sxd_nll <- function(y, X, W, rho, lam) {
  n <- length(y)
  s2 <- .sr_profile(y, X, W, rho, lam)$s2
  v <- 0.5 * n * log(2 * pi * s2) + 0.5 * n
  if (rho != 0) v <- v - .sr_logdet(W, rho)
  if (lam != 0) v <- v - .sr_logdet(W, lam)
  v
}

.sxd_score <- function(y, X, W, rho, lam, which) {
  # d(-log L)/d(which) by the envelope theorem: beta, sigma^2 profiled out
  n <- length(y)
  b <- .sr_profile(y, X, W, rho, lam)$beta
  Wy <- as.vector(W %*% y)
  r <- y - rho * Wy - as.vector(X %*% b)
  e <- r - lam * as.vector(W %*% r)
  if (which == 0) {
    d <- Wy - lam * as.vector(W %*% Wy)
    a <- rho
  } else {
    d <- as.vector(W %*% r)
    a <- lam
  }
  -n * sum(e * d) / sum(e^2) + sum(W * t(solve(diag(n) - a * W)))
}

.sxd_polish <- function(y, X, W, x, other, which, interval) {
  # Newton steps on the analytic score: Brent's minimum is only accurate to ~sqrt(eps)
  sc <- function(t) if (which == 0) .sxd_score(y, X, W, t, other, 0) else .sxd_score(y, X, W, other, t, 1)
  for (it in 1:4) {
    s0 <- sc(x)
    h <- 1e-5
    ds <- (sc(x + h) - sc(x - h)) / (2 * h)
    if (ds <= 0) break
    x1 <- x - s0 / ds
    if (!(interval[1] < x1 && x1 < interval[2])) break
    step <- abs(x1 - x)
    x <- x1
    if (step < 1e-15) break
  }
  x
}

.sxd_fit <- function(y, X, W, model, interval = c(-0.999, 0.999)) {
  rho <- 0
  lam <- 0
  if (model == "lag") {
    rho <- .sxd_brent(function(a) .sxd_nll(y, X, W, a, 0), interval[1], interval[2])[1]
    rho <- .sxd_polish(y, X, W, rho, 0, 0, interval)
  } else if (model == "error") {
    lam <- .sxd_brent(function(a) .sxd_nll(y, X, W, 0, a), interval[1], interval[2])[1]
    lam <- .sxd_polish(y, X, W, lam, 0, 1, interval)
  } else {
    f <- .sxd_nll(y, X, W, 0, 0)
    for (it in 1:200) {
      rho <- .sxd_brent(function(a) .sxd_nll(y, X, W, a, lam), interval[1], interval[2])[1]
      rho <- .sxd_polish(y, X, W, rho, lam, 0, interval)
      lam <- .sxd_brent(function(a) .sxd_nll(y, X, W, rho, a), interval[1], interval[2])[1]
      lam <- .sxd_polish(y, X, W, lam, rho, 1, interval)
      f_new <- .sxd_nll(y, X, W, rho, lam)
      if (abs(f - f_new) < 1e-13 * max(1, abs(f_new))) break
      f <- f_new
    }
  }
  pr <- .sr_profile(y, X, W, rho, lam)
  list(beta = pr$beta, rho = rho, lam = lam, s2 = pr$s2)
}

.sxd_q7 <- function(x, q) {
  s <- sort(x)
  h <- (length(s) - 1) * q
  lo <- floor(h)
  hi <- min(lo + 1, length(s) - 1)
  s[lo + 1] + (h - lo) * (s[hi + 1] - s[lo + 1])
}

.sxd_boot <- function(y, X, W, model, B, seed, level, interval = c(-0.999, 0.999)) {
  y <- as.numeric(y)
  X <- as.matrix(X)
  W <- as.matrix(W)
  n <- length(y)
  f <- .sxd_fit(y, X, W, model, interval)
  Xb <- as.vector(X %*% f$beta)
  r0 <- y - f$rho * as.vector(W %*% y) - Xb
  e <- r0 - f$lam * as.vector(W %*% r0)
  e <- e - sum(e) / n
  Ainv <- solve(diag(n) - f$rho * W)
  Binv <- solve(diag(n) - f$lam * W)
  u <- .morie_random_uniform(B * n, seed = seed)
  draws <- matrix(0, B, 2)
  for (b in seq_len(B)) {
    idx <- pmin(floor(u[(b - 1) * n + seq_len(n)] * n), n - 1) + 1
    ys <- as.vector(Ainv %*% (Xb + as.vector(Binv %*% e[idx])))
    fb <- .sxd_fit(ys, X, W, model, interval)
    draws[b, ] <- c(fb$rho, fb$lam)
  }
  list(fit = f, draws = draws)
}

.sxd_ci <- function(est, draws, level) {
  a <- (1 - level) / 2
  list(statistic = est, ci_lower = .sxd_q7(draws, a), ci_upper = .sxd_q7(draws, 1 - a),
       se_boot = stats::sd(draws), draws = draws)
}

.sxd_lmerr <- function(resid, W) {
  e <- as.numeric(resid)
  W <- as.matrix(W)
  n <- length(e)
  s <- n * sum(e * as.vector(W %*% e)) / sum(e^2)
  stat <- s^2 / sum(W * (W + t(W)))
  list(statistic = stat, p_value = stats::pchisq(stat, 1, lower.tail = FALSE), df = 1)
}

.sxd_resmoran <- function(resid, W, X = NULL) {
  e <- as.numeric(resid)
  W <- as.matrix(W)
  if (!is.null(X)) {
    r <- ResidualMoran(e, as.matrix(X), W)
    return(list(statistic = r$I, expected = r$expected, variance = r$variance, z = r$z, p_value = r$pvalue))
  }
  n <- length(e)
  list(statistic = n / sum(W) * sum(e * as.vector(W %*% e)) / sum(e^2),
       expected = NULL, variance = NULL, z = NULL, p_value = NULL)
}

.sxd_cf <- function(beta, theta, vcov, rho) {
  beta <- as.numeric(beta)
  theta <- as.numeric(theta)
  vcov <- as.matrix(vcov)
  k <- length(beta)
  if (length(theta) != k || nrow(vcov) != 2 * k + 1) {
    stop("need length(theta) == length(beta) and vcov of size 2k + 1 (rho, beta, theta)")
  }
  g <- theta + rho * beta
  G <- matrix(0, k, 2 * k + 1)
  G[, 1] <- beta
  G[cbind(seq_len(k), 1 + seq_len(k))] <- rho
  G[cbind(seq_len(k), 1 + k + seq_len(k))] <- 1
  S <- G %*% vcov %*% t(G)
  stat <- sum(g * solve(S, g))
  list(statistic = stat, p_value = stats::pchisq(stat, k, lower.tail = FALSE), df = k, g = g)
}

.sxd_lrt <- function(l1, l0, df) {
  lr <- 2 * (l1 - l0)
  list(statistic = lr, p_value = stats::pchisq(lr, df, lower.tail = FALSE), df = df)
}

.sxd_wald1 <- function(est, se) {
  z <- est / se
  list(statistic = z^2, p_value = stats::pchisq(z^2, 1, lower.tail = FALSE), z = z, df = 1)
}

.sxd_conv <- function(W, pars) {
  b <- .sxd_bounds(W)
  ok <- all(pars > b[1] & pars < b[2])
  ld <- if (ok) sum(vapply(pars, function(a) .sxd_logdet(as.matrix(W), a), numeric(1))) else NULL
  list(statistic = as.numeric(ok), lower = b[1], upper = b[2], feasible = ok, logdet = ld)
}

.sxd_nagelkerke <- function(ll_model, ll_null, n) {
  cs <- 1 - exp(2 / n * (ll_null - ll_model))
  mx <- 1 - exp(2 / n * ll_null)
  list(statistic = if (abs(mx) > 1e-12) cs / mx else 0, cox_snell = cs)
}

.sxd_filter <- function(y, W, a) {
  y <- as.numeric(y)
  f <- y - a * as.vector(as.matrix(W) %*% y)
  list(statistic = a, filtered = f)
}

.sxd_impacts <- function(coef, rho, W) {
  SpatialImpacts(rho, as.numeric(coef), as.matrix(W))
}
