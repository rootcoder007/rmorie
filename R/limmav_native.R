```r
# voom: precision weights for RNA-seq log-counts.
#
# Law, C. W., Chen, Y., Shi, W., & Smyth, G. K. (2014) "voom: precision weights
# unlock linear model analysis tools for RNA-seq read counts", *Genome Biology*
# 15:R29.
#
# Counts are heteroscedastic, so normal linear models do not apply to them
# directly. voom's move is to transform once and then *carry the variance
# structure along as weights*, which lets the whole normal-theory toolkit run
# on RNA-seq.
#
# The log-counts per million for count r_gi in a library of size R_i is
#
#     y_gi = log2((r_gi + 0.5)/(R_i + 1.0) * 1e6),
#
# where "the counts are offset away from zero by 0.5 to avoid taking the log of
# zero, and to reduce the variability of log-cpm for low expression genes. The
# library size is offset by 1 to ensure that (r_gi+0.5)/(R_i+1) is strictly
# less than 1 as well as strictly greater than zero."
#
# Then, in the paper's own order:
#
# 1. fit the linear model to y_g by ordinary least squares, giving
#    beta_g, fitted values mu_gi and residual standard deviations s_g;
# 2. convert each gene's average log-cpm to an average log-count,
#    r_tilde = ybar_g + log2(R_tilde) - log2(1e6), with R_tilde "the
#    geometric mean of the library sizes plus one";
# 3. fit a LOWESS curve to s_g^{1/2} against r_tilde -- "square-root
#    standard deviations are used because they are roughly symmetrically
#    distributed" -- and read it as a piecewise linear function lo() by
#    interpolating between the ordered r_tilde;
# 4. convert each *fitted* log-cpm to a fitted log-count,
#    lambda_gi = mu_gi + log2(R_i + 1) - log2(1e6);
# 5. the precision weight is w_gi = lo(lambda_gi)^{-4} -- the inverse
#    *variance*, since lo predicts a square-root standard deviation.
#
# The weights are per observation, not per gene, which is the point:
# "different samples may be sequenced to different depths, so different
# count sizes may be quite different even if the cpm values are the same".
#
# What follows the weighting is limma's own pipeline:
#
#     Smyth, G. K. (2004) "Linear models and empirical Bayes methods for
#     assessing differential expression in microarray experiments",
#     *Statistical Applications in Genetics and Molecular Biology* 3(1),
#     Article 3.
#
# The gene-wise variances are moderated toward a prior fitted across all
# genes. With prior information equivalent to an estimator s_0^2 on d_0
# degrees of freedom, the posterior mean of sigma_g^{-2} gives
#
#     s_tilde_g^2 = (d_0 s_0^2 + d_g s_g^2) / (d_0 + d_g),
#     t_tilde_gj = beta_gj / (s_tilde_g sqrt(v_gj)),
#
# and the moderated statistic is t-distributed on d_g + d_0 degrees of
# freedom. The two ends of the spectrum are worth stating because they
# bracket what moderation does: "the moderated t reduces to the ordinary
# t-statistic if d_0 = 0 and at the opposite end of the spectrum is
# proportional to the coefficient beta_gj if d_0 = infinity."
#
# The hyperparameters are estimated by the paper's closed forms, matching
# the first two moments of log s_g^2. With
# e_g = log s_g^2 - psi(d_g/2) + log(d_g/2),
#
#     psi'(d_0/2) = mean{(e_g - ebar)^2 * G/(G-1) - psi'(d_g/2)},
#     s_0^2 = exp{ebar + psi(d_0/2) - log(d_0/2)},
#
# solved for d_0 by the monotone Newton iteration of the paper's appendix.
# When that mean is non-positive "there is no evidence that the underlying
# variances vary between genes", so d_0 = infinity and s_0^2 = exp(ebar) --
# every gene gets the same variance. moderate=False returns the ordinary
# weighted-least-squares t instead.

.limmav_digamma <- function(x) {
  x <- as.numeric(x)
  if (x <= 0) stop("limmav: digamma needs x > 0")
  tot <- 0.0
  while (x < 10.0) {
    tot <- tot - 1.0 / x
    x <- x + 1.0
  }
  inv2 <- 1.0 / (x * x)
  tot + log(x) - 0.5 / x - inv2 * (1.0/12.0 - inv2 * (1.0/120.0 - inv2 / 252.0))
}

.limmav_trigamma <- function(x) {
  x <- as.numeric(x)
  if (x <= 0) stop("limmav: trigamma needs x > 0")
  tot <- 0.0
  while (x < 20.0) {
    tot <- tot + 1.0 / (x * x)
    x <- x + 1.0
  }
  inv <- 1.0 / x
  inv2 <- inv * inv
  tot + inv * (1.0 + 0.5 * inv + inv2 * (
    1.0/6.0 + inv2 * (-1.0/30.0 + inv2 * (1.0/42.0 - inv2 / 30.0))))
}

.limmav_tetragamma <- function(x) {
  x <- as.numeric(x)
  tot <- 0.0
  while (x < 20.0) {
    tot <- tot - 2.0 / (x^3)
    x <- x + 1.0
  }
  inv <- 1.0 / x
  inv2 <- inv * inv
  tot - inv2 * (1.0 + inv * (1.0 + inv2 * (
    1.0/6.0 - inv2 * (1.0/6.0 - 3.0 * inv2 / 10.0))))
}

.limmav_trigamma_inverse <- function(x, tol = 1e-8, max_iter = 60) {
  x <- as.numeric(x)
  if (x <= 0) stop("limmav: trigamma_inverse needs x > 0")
  if (x > 1e7) return(1.0 / sqrt(x))
  if (x < 1e-6) return(1.0 / x)
  y <- 0.5 + 1.0 / x
  for (i in seq_len(max_iter)) {
    tri <- .limmav_trigamma(y)
    tet <- .limmav_tetragamma(y)
    if (tet == 0) break
    d <- tri * (1.0 - tri / x) / tet
    y <- y + d
    if (-d / y < tol) break
  }
  y
}

.limmav_ebayes <- function(sigma2, df, robust_floor = 1e-12) {
  s2 <- as.numeric(sigma2)
  G <- length(s2)
  if (G == 0) stop("limmav: no variances to moderate")
  if (length(df) == 1) {
    dg <- rep(as.numeric(df), G)
  } else {
    dg <- as.numeric(df)
  }
  if (length(dg) != G) stop("limmav: one degrees-of-freedom value per gene")
  use <- which(s2 > robust_floor & dg > 0)
  if (length(use) == 0) stop("limmav: every gene has zero variance or zero degrees of freedom")
  e <- log(s2[use]) - .limmav_digamma(dg[use] / 2.0) + log(dg[use] / 2.0)
  ebar <- mean(e)
  n <- length(e)
  target <- if (n > 1) var(e) else 0.0
  target <- target - mean(sapply(dg[use] / 2.0, .limmav_trigamma))
  if (target <= 0) {
    d0 <- Inf
    s0_sq <- exp(ebar)
    post <- rep(s0_sq, G)
    return(list(d0 = d0, s0_sq = s0_sq, s2_post = post,
                df_total = rep(Inf, G), no_gene_variation = TRUE))
  }
  d0 <- 2.0 * .limmav_trigamma_inverse(target)
  s0_sq <- exp(ebar + .limmav_digamma(d0 / 2.0) - log(d0 / 2.0))
  post <- numeric(G)
  for (g in seq_len(G)) {
    if (dg[g] > 0) {
      post[g] <- (d0 * s0_sq + dg[g] * s2[g]) / (d0 + dg[g])
    } else {
      post[g] <- s0_sq
    }
  }
  list(d0 = d0, s0_sq = s0_sq, s2_post = post,
       df_total = dg + d0, no_gene_variation = FALSE)
}

.limmav_log_cpm <- function(counts, lib_sizes = NULL, prior_count = 0.5, lib_offset = 1.0) {
  K <- as.matrix(counts)
  storage.mode(K) <- "double"
  if (nrow(K) == 0 || ncol(K) == 0) {
    stop("limmav: counts must be a non-empty gene x sample matrix")
  }
  m <- ncol(K)
  if (any(K < 0)) stop("limmav: counts must be non-negative")
  if (is.null(lib_sizes)) {
    R <- colSums(K)
  } else {
    R <- as.numeric(lib_sizes)
    if (length(R) != m) stop("limmav: one library size per sample")
  }
  if (any(R <= 0)) stop("limmav: library sizes must be positive")
  y <- log2((K + prior_count) / (R + lib_offset) * 1e6)
  list(y = y, R = R)
}

.limmav_lowess <- function(x, y, span = 0.5, iterations = 3) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  n <- length(x)
  if (n != length(y)) stop("limmav: x and y must have the same length")
  if (n == 0) stop("limmav: nothing to smooth")
  if (span <= 0 || span > 1) stop("limmav: span must lie in (0, 1]")

  order_idx <- order(x)
  xs <- x[order_idx]
  ys <- y[order_idx]
  q <- max(2, ceiling(span * n))
  rw <- rep(1.0, n)
  fitted <- ys

  for (it in 0:iterations) {
    for (i in seq_len(n)) {
      lo_r <- max(1, min(i - q %/% 2, n - q + 1))
      hi_r <- lo_r + q - 1

      d <- max(abs(xs[i] - xs[lo_r]), abs(xs[hi_r] - xs[i]), 1e-12)
      sw <- 0.0; sx <- 0.0; sy <- 0.0; sxx <- 0.0; sxy <- 0.0
      for (k in lo_r:hi_r) {
        u <- abs(xs[k] - xs[i]) / d
        w <- if (u < 1.0) (1.0 - u^3)^3 else 0.0
        w <- w * rw[k]
        if (w <= 0) next
        sw <- sw + w
        sx <- sx + w * xs[k]
        sy <- sy + w * ys[k]
        sxx <- sxx + w * xs[k]^2
        sxy <- sxy + w * xs[k] * ys[k]
      }
      if (sw <= 0) {
        fitted[i] <- ys[i]
        next
      }
      den <- sw * sxx - sx * sx
      if (abs(den) < 1e-12) {
        fitted[i] <- sy / sw
      } else {
        b <- (sw * sxy - sx * sy) / den
        a <- (sy - b * sx) / sw
        fitted[i] <- a + b * xs[i]
      }
    }
    if (it == iterations) break
    res <- abs(ys - fitted)
    s <- sort(res)[n %/% 2 + 1]
    if (s <= 0) break
    rw <- (1 - pmin(res / (6 * s), 1))^2
  }

  out <- numeric(n)
  out[order_idx] <- fitted
  out
}

.limmav_ols <- function(X, y, w = NULL) {
  X <- as.matrix(X)
  y <- as.numeric(y)
  n <- nrow(X)
  p <- ncol(X)
  if (is.null(w)) {
    ww <- rep(1.0, n)
  } else {
    ww <- as.numeric(w)
  }
  Xw <- ww * X
  M <- crossprod(X, Xw)
  v <- crossprod(X, ww * y)

  beta <- tryCatch(solve(M, v), error = function(e) {
    stop("limmav: the design matrix is singular")
  })
  inv <- tryCatch(solve(M), error = function(e) {
    stop("limmav: the design matrix is singular")
  })

  fit <- as.numeric(X %*% beta)
  df <- n - p
  if (df <= 0) stop("limmav: no residual degrees of freedom")
  rss <- sum(ww * (y - fit)^2)
  sd <- sqrt(rss / df)

  list(beta = as.numeric(beta), fit = fit, sd = sd, inv = inv, df = df)
}

.limmav_voom_weights <- function(counts, design, lib_sizes = NULL, span = 0.5) {
  y_R <- .limmav_log_cpm(counts, lib_sizes)
  y <- y_R$y
  R <- y_R$R
  G <- nrow(y)
  m <- ncol(y)
  X <- as.matrix(design)
  if (nrow(X) != m) {
    stop(sprintf("limmav: the design has %d rows but there are %d samples",
                 nrow(X), m))
  }

  fitted <- matrix(0, G, m)
  sds <- numeric(G)
  means <- numeric(G)
  for (g in seq_len(G)) {
    res <- .limmav_ols(X, y[g, ])
    fitted[g, ] <- res$fit
    sds[g] <- res$sd
    means[g] <- mean(y[g, ])
  }

  logR <- mean(log2(R + 1.0))
  r_tilde <- means + logR - log2(1e6)

  sqrt_sd <- sqrt(sds)
  smooth <- .limmav_lowess(r_tilde, sqrt_sd, span = span)
  order_g <- order(r_tilde)
  kx <- r_tilde[order_g]
  ky <- smooth[order_g]

  lo <- function(t) {
    if (t <= kx[1]) return(ky[1])
    if (t >= kx[length(kx)]) return(ky[length(kx)])
    lo_i <- 1
    hi_i <- length(kx)
    while (hi_i - lo_i > 1) {
      mid <- (lo_i + hi_i) %/% 2
      if (kx[mid] <= t) {
        lo_i <- mid
      } else {
        hi_i <- mid
      }
    }
    x0 <- kx[lo_i]
    x1 <- kx[hi_i]
    if (x1 - x0 < 1e-15) return(ky[lo_i])
    f <- (t - x0) / (x1 - x0)
    ky[lo_i] + f * (ky[hi_i] - ky[lo_i])
  }

  W <- matrix(0, G, m)
  for (g in seq_len(G)) {
    for (i in seq_len(m)) {
      lam <- fitted[g, i] + log2(R[i] + 1.0) - log2(1e6)
      s <- lo(lam)
      W[g, i] <- if (s > 0) 1.0 / (s^4) else 0.0
    }
  }

  list(log_cpm = y, weights = W, mean_log_count = r_tilde,
       sqrt_sd = sqrt_sd, trend_x = kx, trend_y = ky,
       lib_sizes = R, lo = lo)
}

.limmav_weighted_lm <- function(y, X, w, contrast) {
  res <- .limmav_ols(X, y, w)
  beta <- res$beta
  sd <- res$sd
  inv <- res$inv
  df <- res$df
  p <- ncol(X)
  est <- sum(contrast * beta)
  v_un <- sum(contrast * (inv %*% contrast))
  var <- v_un * sd * sd
  se <- sqrt(max(var, 0.0))
  t <- if (se > 0) est / se else 0.0
  list(est = est, se = se, t = t, df = df, sd = sd, v_un = v_un)
}

.limmav_t_sf <- function(t, df) {
  x <- df / (df + t * t)
  a <- 0.5 * df
  b <- 0.5

  betacf <- function(a, b, x) {
    qab <- a + b
    qap <- a + 1.0
    qam <- a - 1.0
    c <- 1.0
    d <- 1.0 - qab * x / qap
    if (abs(d) < 1e-300) d <- 1e-300
    d <- 1.0 / d
    h <- d
    for (mm in 1:300) {
      m2 <- 2 * mm
      aa <- mm * (b - mm) * x / ((qam + m2) * (a + m2))
      d <- 1.0 + aa * d
      if (abs(d) < 1e-300) d <- 1e-300
      c <- 1.0 + aa / c
      if (abs(c) < 1e-300) c <- 1e-300
      d <- 1.0 / d
      h <- h * d * c
      aa <- -(a + mm) * (qab + mm) * x / ((a + m2) * (qap + m2))
      d <- 1.0 + aa * d
      if (abs(d) < 1e-300) d <- 1e-300
      c <- 1.0 + aa / c
      if (abs(c) < 1e-300) c <- 1e-300
      d <- 1.0 / d
      de <- d * c
      h <- h * de
      if (abs(de - 1.0) < 3e-16) break
    }
    h
  }

  lbeta <- if (x > 0 && x < 1) {
    lgamma(a + b) - lgamma(a) - lgamma(b) + a * log(x) + b * log(1.0 - x)
  } else {
    NULL
  }
  if (is.null(lbeta)) {
    return(if (x >= 1) 1.0 else 0.0)
  }
  if (x < (a + 1.0) / (a + b + 2.0)) {
    exp(lbeta) * betacf(a, b, x) / a
  } else {
    1.0 - exp(lbeta) * betacf(b, a, 1.0 - x) / b
  }
}

.limmav_benjamini_hochberg <- function(pvalues) {
  p <- as.numeric(pvalues)
  G <- length(p)
  if (G == 0) return(numeric(0))
  ord <- order(p)
  p_sorted <- p[ord]
  ranks <- seq_len(G)
  adj_sorted <- pmin(p_sorted * G / ranks, 1)
  if (G > 1) {
    for (i in (G-1):1) {
      adj_sorted[i] <- min(adj_sorted[i], adj_sorted[i+1])
    }
  }
  result <- numeric(G)
  result[ord] <- adj_sorted
  result
}

morie_limmav <- function(counts, design, contrast = NULL, lib_sizes = NULL,
                          span = 0.5, weights = TRUE, moderate = TRUE) {
  if (is.matrix(design)) {
    X <- design
  } else {
    labs <- as.character(design)
    levels <- unique(labs)
    if (length(levels) < 2) stop("limmav: the design has only one group")
    X <- matrix(0, nrow = length(labs), ncol = length(levels))
    X[, 1] <- 1
    for (j in 2:length(levels)) {
      X[, j] <- as.numeric(labs ==
