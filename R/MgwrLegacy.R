.mgl_prep <- function(y, X, coords) {
  y <- as.numeric(y)
  n <- length(y)
  list(y = y, X = .gwl_design(X, n), D = unname(as.matrix(stats::dist(unname(as.matrix(coords)) * 1))), n = n)
}

.mgl_smoother <- function(x, D, bw, kernel, adaptive) {
  t(vapply(seq_along(x), function(i) {
    w <- GWRKernelWeights(D[i, ], bw, kernel, adaptive)
    w * x / sum(w * x * x)
  }, numeric(length(x))))
}

.mgl_uni_aicc <- function(x, v, D, bw, kernel, adaptive) {
  n <- length(x)
  C <- .mgl_smoother(x, D, bw, kernel, adaptive)
  fit <- x * as.vector(C %*% v)
  rss <- sum((v - fit)^2)
  tr <- sum(x * diag(C))
  if (tr >= n - 2 || rss <= 0) return(Inf)
  n * log(rss / n) + n * log(2 * pi) + n * (n + tr) / (n - 2 - tr)
}

.mgl_golden <- function(f, lo, hi, tol) {
  g <- (sqrt(5) - 1) / 2
  a <- lo
  b <- hi
  c <- b - g * (b - a)
  d <- a + g * (b - a)
  fc <- f(c)
  fd <- f(d)
  while (abs(b - a) > tol * (abs(a) + abs(b))) {
    if (fc <= fd) {
      b <- d
      d <- c
      fd <- fc
      c <- b - g * (b - a)
      fc <- f(c)
    } else {
      a <- c
      c <- d
      fc <- fd
      d <- a + g * (b - a)
      fd <- f(d)
    }
  }
  (a + b) / 2
}

.mgl_backfit <- function(s, bws, kernel, adaptive, threshold, max_iter, select, hat) {
  y <- s$y
  X <- s$X
  D <- s$D
  n <- s$n
  p <- ncol(X)
  XtXi <- solve(crossprod(X))
  H <- XtXi %*% t(X)
  b0 <- as.vector(H %*% y)
  beta <- matrix(rep(b0, each = n), n, p)
  f <- X * beta
  resid <- y - rowSums(f)
  R <- NULL
  if (hat) {
    R <- lapply(seq_len(p), function(k) matrix(H[k, ], n, n, byrow = TRUE))
    S <- Reduce(`+`, lapply(seq_len(p), function(k) X[, k] * R[[k]]))
  }
  if (is.null(bws)) bws <- rep(NA_real_, p)
  lo <- min(D[D > 0])
  hi <- max(D)
  rss0 <- sum(resid^2)
  it <- 0
  crit <- Inf
  while (it < max_iter && crit > threshold) {
    it <- it + 1
    old <- bws
    for (k in seq_len(p)) {
      yk <- resid + f[, k]
      x <- X[, k]
      if (select) bws[k] <- .mgl_golden(function(b) .mgl_uni_aicc(x, yk, D, b, kernel, adaptive), lo, hi, 1e-8)
      C <- .mgl_smoother(x, D, bws[k], kernel, adaptive)
      bk <- as.vector(C %*% yk)
      beta[, k] <- bk
      f[, k] <- x * bk
      resid <- yk - f[, k]
      if (hat) {
        M <- diag(n) - S + x * R[[k]]
        new <- C %*% M
        S <- S + x * (new - R[[k]])
        R[[k]] <- new
      }
    }
    rss1 <- sum(resid^2)
    crit <- if (rss1 > 0) sqrt(abs(rss1 - rss0) / rss1) else 0
    if (select) crit <- max(crit, if (is.na(old[1])) Inf else max(abs(bws - old) / old))
    rss0 <- rss1
  }
  list(beta = beta, resid = resid, rss = rss0, it = it, bws = bws, R = R)
}

#' Multiscale geographically weighted regression (MGWR)
#'
#' `mgwrfit` fits MGWR (Fotheringham, Yang and Kang 2017) by backfitting
#' from OLS (intercept prepended when `X` has no constant column, predictors
#' not centred) with covariate-specific bandwidths, fixed or re-selected at
#' each pass by golden-section minimisation of the one-covariate AICc
#' (`mgwrbw`), and tracks the covariate-specific hat matrices for inference
#' (Yu et al. 2020): standard errors, ENP, `tr S`, `sigma^2 = RSS/(n - tr S)`
#' and AICc.  `mgwrcof`, `mgwrres`, `mgwrstd`, `mgwrhat` return its
#' coefficients, residuals, standard errors and hat diagonal; `mgwrcv` the
#' leave-one-out score `sum (e_i/(1 - S_ii))^2`; `mgwrbk` runs the
#' backfitting with fixed bandwidths; `mgwraic`, `mgwrdg` and `mgwrsig` the
#' AICc, diagnostic summary and error variance from `ll`, `tr S` and the
#' residuals; `mgwrtst` the Monte Carlo test of coefficient variability with
#' Philox permutations.
#'
#' @param y Response.
#' @param X Regressors.
#' @param coords Coordinates.
#' @param bandwidths,bws Per-covariate bandwidths (intercept first); NULL
#'   selects them.
#' @param kernel Kernel name (see `GWRKernelWeights`).
#' @param adaptive Adaptive bandwidths.
#' @param threshold Convergence threshold of the backfitting.
#' @param max_iter Maximum number of backfitting passes.
#' @param ll Gaussian log-likelihood.
#' @param tr_S Trace of the hat matrix.
#' @param n Number of observations.
#' @param k Number of covariates (for the corrected test level).
#' @param alpha Nominal level.
#' @param resid Residuals.
#' @param nsim Number of permutations.
#' @param seed Philox seed.
#' @return `mgwrfit` a list with `betas`, `se`, `fitted`, `residuals`,
#'   `hat_diagonal`, `enp`, `trS`, `rss`, `sigma2`, `aicc`, `bandwidths`,
#'   `iterations`; the others the corresponding parts.
#' @references Fotheringham, A. S., Yang, W. and Kang, W. (2017).
#'   Multiscale geographically weighted regression (MGWR). Annals of the
#'   American Association of Geographers 107, 1247-1265. Yu, H.,
#'   Fotheringham, A. S., Li, Z., Oshan, T., Kang, W. and Wolf, L. J. (2020).
#'   Inference in multiscale geographically weighted regression. Geographical
#'   Analysis 52, 87-106. Hurvich, C. M., Simonoff, J. S. and Tsai, C.-L.
#'   (1998). Smoothing parameter selection in nonparametric regression using
#'   an improved Akaike information criterion. JRSS B 60, 271-293. da Silva,
#'   A. R. and Fotheringham, A. S. (2016). The multiple testing issue in
#'   geographically weighted regression. Geographical Analysis 48, 233-247.
#' @examples
#' P <- cbind((0:19) %% 5, (0:19) %/% 5)
#' X <- cbind(sin(0:19), (0.3 * (0:19)) %% 1.1)
#' y <- 1 + (1 + 0.2 * P[, 1]) * X[, 1] - X[, 2] + 0.1 * cos(3 * (0:19))
#' mgwrfit(y, X, P, bandwidths = c(6, 3, 8), kernel = "gaussian")$trS
#' mgwraic(-20, 5.5, 40)$statistic
#' @export
mgwrfit <- function(y, X, coords, bandwidths = NULL, kernel = "bisquare", adaptive = FALSE, threshold = 1e-10,
                    max_iter = 500) {
  s <- .mgl_prep(y, X, coords)
  b <- .mgl_backfit(s, bandwidths, kernel, adaptive, threshold, max_iter, is.null(bandwidths), TRUE)
  n <- s$n
  p <- ncol(s$X)
  hd <- rowSums(vapply(seq_len(p), function(k) s$X[, k] * diag(b$R[[k]]), numeric(n)))
  enp <- vapply(seq_len(p), function(k) sum(s$X[, k] * diag(b$R[[k]])), 0)
  trS <- sum(enp)
  s2 <- b$rss / (n - trS)
  se <- vapply(seq_len(p), function(k) sqrt(s2 * rowSums(b$R[[k]]^2)), numeric(n))
  list(betas = b$beta, se = matrix(se, n), fitted = s$y - b$resid, residuals = b$resid, hat_diagonal = hd, enp = enp,
       trS = trS, rss = b$rss, sigma2 = s2,
       aicc = n * log(b$rss / n) + n * log(2 * pi) + n * (n + trS) / (n - 2 - trS),
       bandwidths = b$bws, iterations = b$it)
}

#' @rdname mgwrfit
#' @export
mgwrcof <- function(y, X, coords, bws = NULL, kernel = "bisquare", adaptive = FALSE) {
  mgwrfit(y, X, coords, bws, kernel, adaptive)$betas
}

#' @rdname mgwrfit
#' @export
mgwrres <- function(y, X, coords, bws = NULL, kernel = "bisquare", adaptive = FALSE) {
  mgwrfit(y, X, coords, bws, kernel, adaptive)$residuals
}

#' @rdname mgwrfit
#' @export
mgwrstd <- function(y, X, coords, bws = NULL, kernel = "bisquare", adaptive = FALSE) {
  mgwrfit(y, X, coords, bws, kernel, adaptive)$se
}

#' @rdname mgwrfit
#' @export
mgwrhat <- function(y, X, coords, bws = NULL, kernel = "bisquare", adaptive = FALSE) {
  mgwrfit(y, X, coords, bws, kernel, adaptive)$hat_diagonal
}

#' @rdname mgwrfit
#' @export
mgwrcv <- function(y, X, coords, bws = NULL, kernel = "bisquare", adaptive = FALSE) {
  r <- mgwrfit(y, X, coords, bws, kernel, adaptive)
  sum((r$residuals / (1 - r$hat_diagonal))^2)
}

#' @rdname mgwrfit
#' @export
mgwrbw <- function(y, X, coords, kernel = "bisquare", adaptive = FALSE, threshold = 1e-8, max_iter = 200) {
  .mgl_backfit(.mgl_prep(y, X, coords), NULL, kernel, adaptive, threshold, max_iter, TRUE, FALSE)$bws
}

#' @rdname mgwrfit
#' @export
mgwrbk <- function(y, X, coords, bandwidths, max_iter = 10, kernel = "bisquare", adaptive = FALSE,
                   threshold = 1e-10) {
  b <- .mgl_backfit(.mgl_prep(y, X, coords), bandwidths, kernel, adaptive, threshold, max_iter, FALSE, FALSE)
  list(betas = b$beta, residuals = b$resid, rss = b$rss, iterations = b$it, converged = b$it < max_iter)
}

#' @rdname mgwrfit
#' @export
mgwraic <- function(ll, tr_S, n) {
  if (!(tr_S < n - 2)) stop("tr_S must be below n - 2")
  list(statistic = -2 * ll - n + n * (n + tr_S) / (n - 2 - tr_S), trS = tr_S, n = n)
}

#' @rdname mgwrfit
#' @export
mgwrdg <- function(ll, tr_S, n, k = NULL, alpha = 0.05) {
  if (!(tr_S < n - 2)) stop("tr_S must be below n - 2")
  out <- list(AIC = -2 * ll + 2 * (tr_S + 1), AICc = -2 * ll - n + n * (n + tr_S) / (n - 2 - tr_S),
              BIC = -2 * ll + (tr_S + 1) * log(n), ENP = tr_S, df_residual = n - tr_S)
  if (!is.null(k)) out$alpha_adjusted <- alpha / (tr_S / k)
  out
}

#' @rdname mgwrfit
#' @export
mgwrsig <- function(resid, tr_S, n = NULL) {
  e <- as.numeric(resid)
  if (is.null(n)) n <- length(e)
  list(statistic = sum(e^2) / (n - tr_S), rss = sum(e^2), trS = tr_S)
}

#' @rdname mgwrfit
#' @export
mgwrtst <- function(y, X, coords, nsim = 9, bandwidths = NULL, seed = 0, kernel = "bisquare", adaptive = FALSE,
                    threshold = 1e-8, max_iter = 500) {
  s <- .mgl_prep(y, X, coords)
  if (is.null(bandwidths)) bandwidths <- .mgl_backfit(s, NULL, kernel, adaptive, threshold, max_iter, TRUE, FALSE)$bws
  vb <- function(ss) apply(.mgl_backfit(ss, bandwidths, kernel, adaptive, threshold, max_iter, FALSE, FALSE)$beta, 2,
                           stats::var)
  obs <- vb(s)
  n <- s$n
  sims <- matrix(0, nsim, length(obs))
  for (r in seq_len(nsim)) {
    u <- .morie_random_uniform(n, seed = seed, stream = r - 1)
    perm <- seq_len(n)
    for (i in (n - 1):1) {
      j <- floor(u[i + 1] * (i + 1))
      tmp <- perm[i + 1]
      perm[i + 1] <- perm[j + 1]
      perm[j + 1] <- tmp
    }
    sp <- s
    sp$D <- s$D[perm, perm]
    sims[r, ] <- vb(sp)
  }
  p <- vapply(seq_along(obs), function(a) 1 - (1 + sum(sims[, a] < obs[a])) / (nsim + 1), 0)
  list(observed_variance = obs, simulated_variance = sims, p_values = p, bandwidths = bandwidths)
}
