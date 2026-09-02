# DESeq2-style differential expression. Sources: Love, M. I., Huber,
# W. & Anders, S. (2014) "Moderated estimation of fold change and
# dispersion for RNA-seq data with DESeq2", Genome Biology 15:550,
# equations 1-10 and the methods sections on dispersion and fold-change
# shrinkage (median-of-ratios size factors, Cox-Reid adjusted
# likelihood for the gene-wise estimate, the gamma-GLM trend
# alpha_tr = a1/mu + a0, MAP with a log-normal prior whose width is
# s_lr^2 - trigamma((m-p)/2) floored at 0.25, dispersion outliers not
# shrunk, the zero-centred LFC prior with quantile-matched width,
# ridge-IRLS, Wald test, BH). Independent filtering and Cook's outlier
# replacement are not implemented and that is stated, not silent.


# Base R has no erf/erfc; both are pnorm in disguise. Defined here so
# the arm stays base-R only, as the package requires.
#' Base R has no erf/erfc; both are pnorm in disguise. Defined here so
#'
#' the arm stays base-R only, as the package requires.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.deseq2_erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
#' .deseq2_erfc
#'
#' A step of the deseq2_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.deseq2_erfc <- function(x) 2 * pnorm(-x * sqrt(2))

.ghc_DESEQ2_EPS <- 1e-12

#' Trigamma function
#'
#' \code{psi_1(x)}, with recurrence to a large argument followed by
#' the asymptotic series. Used to translate the chi-squared sampling
#' variance of a log dispersion into the prior width.
#'
#' @param x Positive numeric.
#' @return Numeric scalar.
#' @export
.deseq2_trigamma <- function(x) {
  x <- as.numeric(x)
  if (x <= 0) stop("deseq2: trigamma needs x > 0")
  tot <- 0
  while (x < 20) {
    tot <- tot + 1 / (x * x)
    x <- x + 1
  }
  inv <- 1 / x
  inv2 <- inv * inv
  tot + inv * (1 + 0.5 * inv + inv2 * (1 / 6 + inv2 * (-1 / 30 +
            inv2 * (1 / 42 - inv2 / 30))))
}

#' .ghc_deseq2_median
#'
#' A step of the deseq2_native implementation. Called by \code{.ghc_deseq2_mad}, \code{dispersion_trend}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v Numeric; passed to \code{sort}.
#' @return One of two values, depending on the branch taken.
#' @export
.ghc_deseq2_median <- function(v) {
  s <- sort(v)
  n <- length(s)
  if (n == 0L) stop("deseq2: median of an empty sequence")
  if (n %% 2L == 1L) s[(n + 1L) %/% 2L] else 0.5 * (s[n %/% 2L] + s[n %/% 2L + 1L])
}

#' .ghc_deseq2_mad
#'
#' A step of the deseq2_native implementation. Called by \code{deseq2}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.ghc_deseq2_mad <- function(v) {
  m <- .ghc_deseq2_median(v)
  .ghc_deseq2_median(abs(v - m)) / 0.6744897501960817
}

#' Median-of-ratios size factors
#'
#' The reference for each gene is its geometric mean across samples,
#' and \code{s_j = median_i K_ij / K_i^R} over genes with a positive
#' reference. Genes with any zero count drop out, which is what makes
#' the estimator robust to genes present in only some samples.
#'
#' @param counts Gene-by-sample integer count matrix.
#' @return Numeric vector of size factors, one per sample.
#' @export
size_factors <- function(counts) {
  K <- lapply(counts, function(r) as.numeric(r))
  if (length(K) == 0L || length(K[[1L]]) == 0L)
    stop("deseq2: counts must be a non-empty gene x sample matrix")
  m <- length(K[[1L]])
  ratios <- vector("list", m)
  for (r in seq_along(K)) {
    if (length(K[[r]]) != m)
      stop("deseq2: ragged count matrix")
    if (any(K[[r]] < 0))
      stop("deseq2: counts must be non-negative")
    if (any(K[[r]] <= 0)) next
    gm <- exp(sum(log(K[[r]])) / m)
    for (j in seq_len(m))
      ratios[[j]] <- c(ratios[[j]], K[[r]][j] / gm)
  }
  if (length(ratios[[1L]]) == 0L)
    stop("deseq2: no gene has a positive count in every sample, so median-of-ratios has no reference")
  vapply(ratios, .ghc_deseq2_median, numeric(1))
}

#' .ghc_deseq2_nb_loglik
#'
#' A step of the deseq2_native implementation. Called by \code{cox_reid_loglik}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param K A vector; its length is taken and its elements indexed.
#' @param mu A vector; indexed elementwise.
#' @param alpha Numeric; combined arithmetically in the body.
#' @return The value of \code{tot}, as built in the body.
#' @export
.ghc_deseq2_nb_loglik <- function(K, mu, alpha) {
  if (alpha <= 0) {
    tot <- 0
    for (k in seq_along(K)) tot <- tot + K[k] * log(mu[k]) - mu[k] - lgamma(K[k] + 1)
    return(tot)
  }
  r <- 1 / alpha
  tot <- 0
  for (k in seq_along(K)) {
    tot <- tot + lgamma(K[k] + r) - lgamma(r) - lgamma(K[k] + 1) +
      r * log(r / (r + mu[k])) + K[k] * log(mu[k] / (r + mu[k]))
  }
  tot
}

#' .ghc_deseq2_xtwx_logdet
#'
#' A step of the deseq2_native implementation. Called by \code{cox_reid_loglik}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; indexed by row and column.
#' @param mu A vector; indexed elementwise.
#' @param alpha Numeric; combined arithmetically in the body.
#' @return The value of \code{ld}, as built in the body.
#' @export
.ghc_deseq2_xtwx_logdet <- function(X, mu, alpha) {
  p <- ncol(X)
  M <- matrix(0, p, p)
  for (j in seq_len(nrow(X))) {
    w <- 1 / (1 / mu[j] + alpha)
    for (a in seq_len(p)) for (bb in seq_len(p))
      M[a, bb] <- M[a, bb] + w * X[j, a] * X[j, bb]
  }
  ld <- as.numeric(determinant(M, logarithm = TRUE)$modulus)
  ld
}

#' Cox-Reid adjusted log-likelihood (eq. 7)
#'
#' \code{ell(alpha) - 0.5 log det(X' W X)}. The adjustment is the
#' GLM analogue of Bessel's correction.
#'
#' @param alpha Positive numeric.
#' @param K Count vector.
#' @param mu Fitted mean vector.
#' @param X Design matrix.
#' @return Numeric scalar.
#' @export
cox_reid_loglik <- function(alpha, K, mu, X) {
  if (alpha <= 0) stop("deseq2: alpha must be positive")
  .ghc_deseq2_nb_loglik(K, mu, alpha) -
    0.5 * .ghc_deseq2_xtwx_logdet(X, mu, alpha)
}

#' Negative binomial GLM by ridge-penalised IRLS
#'
#' Update is the paper's own:
#' \code{beta <- (X' W X + lambda I)^{-1} X' W z} with
#' \code{z_j = log(mu_j / s_j) + (K_j - mu_j) / mu_j} and
#' \code{w_jj = 1 / (1/mu_j + alpha)}.
#'
#' @param K Sample-by-1 response vector.
#' @param X Design matrix.
#' @param alpha Dispersion.
#' @param s Optional size factors.
#' @param lam Optional ridge vector (one per coefficient).
#' @param max_iter Maximum iterations.
#' @param tol Convergence tolerance on the parameter step.
#' @param beta0 Optional starting coefficients.
#' @return A list with \code{beta}, \code{mu}, \code{sigma},
#'   \code{converged}, \code{n_iter}.
#' @export
nb_glm_fit <- function(K, X, alpha, s = NULL, lam = NULL,
                       max_iter = 100L, tol = 1e-8, beta0 = NULL) {
  m <- length(K); p <- ncol(X)
  if (is.null(s)) s <- rep(1, m)
  lam <- if (is.null(lam)) rep(0, p) else as.numeric(lam)
  if (is.null(beta0)) {
    base <- max(sum(K / s) / m, 0.1)
    beta <- c(log(base), rep(0, p - 1L))
  } else beta <- as.numeric(beta0)
  Sig <- NULL
  it <- 0L; converged <- FALSE
  for (it in seq_len(max_iter)) {
    mu <- numeric(m)
    for (j in seq_len(m)) {
      eta <- sum(X[j, ] * beta)
      mu[j] <- max(s[j] * exp(min(eta, 50)), 1e-10)
    }
    M <- matrix(0, p, p); v <- rep(0, p)
    for (j in seq_len(m)) {
      w <- 1 / (1 / mu[j] + alpha)
      z <- log(mu[j] / s[j]) + (K[j] - mu[j]) / mu[j]
      v <- v + w * X[j, ] * z
      for (a in seq_len(p)) for (bb in seq_len(p))
        M[a, bb] <- M[a, bb] + w * X[j, a] * X[j, bb]
    }
    Mr <- M
    for (a in seq_len(p)) Mr[a, a] <- Mr[a, a] + lam[a]
    new <- tryCatch(as.numeric(solve(Mr, v)),
                    error = function(e)
                      stop("deseq2: the GLM design is singular; check the design matrix for collinear columns"))
    step <- max(abs(new - beta))
    beta <- new
    Sig <- Mr
    if (step < tol) { converged <- TRUE; break }
  }
  mu <- numeric(m)
  for (j in seq_len(m)) {
    eta <- sum(X[j, ] * beta)
    mu[j] <- max(s[j] * exp(min(eta, 50)), 1e-10)
  }
  inv <- as.matrix(solve(Sig))
  list(beta = beta, mu = mu, sigma = inv,
       converged = converged, n_iter = it)
}

#' .ghc_deseq2_maximise_log_alpha
#'
#' A step of the deseq2_native implementation. Called by \code{deseq2}, \code{dispersion_gene_wise}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param obj Accepted by the signature and not used anywhere in the body.
#' @param lo Numeric; passed to \code{exp}. Defaults to \code{-15}.
#' @param hi Numeric; combined arithmetically in the body. Defaults to \code{5}.
#' @param n_grid A count; the body uses it as \code{seq_len(...)}. Defaults to \code{60L}.
#' @param refine A count; the body uses it as \code{seq_len(...)}. Defaults to \code{60L}.
#' @return A numeric value.
#' @export
.ghc_deseq2_maximise_log_alpha <- function(obj, lo = -15, hi = 5,
                                           n_grid = 60L, refine = 60L) {
  best_u <- lo; best_v <- obj(exp(lo))
  step <- (hi - lo) / n_grid
  for (g in seq_len(n_grid)) {
    u <- lo + (hi - lo) * g / n_grid
    val <- obj(exp(u))
    if (val > best_v) { best_u <- u; best_v <- val }
  }
  a <- best_u - step; b <- best_u + step
  phi <- (sqrt(5) - 1) / 2
  c <- b - phi * (b - a); d <- a + phi * (b - a)
  fc <- obj(exp(c)); fd <- obj(exp(d))
  for (iter in seq_len(refine)) {
    if (fc > fd) {
      b <- d; d <- c; fd <- fc
      c <- b - phi * (b - a); fc <- obj(exp(c))
    } else {
      a <- c; c <- d; fc <- fd
      d <- a + phi * (b - a); fd <- obj(exp(d))
    }
  }
  exp(0.5 * (a + b))
}

#' Gene-wise dispersion estimate
#'
#' Maximises equation 7 with the fitted values from an initial GLM
#' held fixed, exactly as the paper describes.
#'
#' @param K Count vector for one gene.
#' @param X Design matrix.
#' @param s Size factors.
#' @param alpha_init Initial dispersion.
#' @return A list with \code{dispersion} and \code{mu0}.
#' @export
dispersion_gene_wise <- function(K, X, s, alpha_init = 0.1) {
  fit0 <- nb_glm_fit(K, X, alpha_init, s)
  list(dispersion = .ghc_deseq2_maximise_log_alpha(
         function(a) cox_reid_loglik(a, K, fit0$mu, X)),
       mu0 = fit0$mu)
}

#' Fit the dispersion trend (eq. 6)
#'
#' Gamma-family GLM with an identity link, iterating with genes whose
#' dispersion/fit ratio falls outside \code{\[1e-4, 15\]} excluded.
#'
#' @param mu_bar Per-gene mean of normalised counts.
#' @param disp Gene-wise dispersion estimates.
#' @param max_iter Maximum iterations.
#' @param tol Convergence tolerance.
#' @return A list with \code{a1}, \code{a0} and \code{fitted}.
#' @export
dispersion_trend <- function(mu_bar, disp, max_iter = 10L, tol = 1e-6) {
  keep <- which(disp > 0 & mu_bar > 0)
  if (length(keep) < 3L)
    stop("deseq2: too few genes with positive dispersion to fit the trend")
  a1 <- 1; a0 <- max(1e-8, .ghc_deseq2_median(disp[keep]))
  for (iter in seq_len(max_iter)) {
    rows <- keep
    sel <- vapply(keep, function(i) {
      fit <- a1 / mu_bar[i] + a0
      1e-4 <= disp[i] / fit && disp[i] / fit <= 15
    }, logical(1))
    rows <- keep[sel]
    if (length(rows) < 3L) rows <- keep
    M <- matrix(0, 2, 2); v <- rep(0, 2)
    for (i in rows) {
      fit <- max(a1 / mu_bar[i] + a0, 1e-12)
      w <- 1 / (fit * fit)
      xrow <- c(1 / mu_bar[i], 1)
      v <- v + w * xrow * disp[i]
      M <- M + w * outer(xrow, xrow)
    }
    new <- tryCatch(as.numeric(solve(M, v)), error = function(e) NULL)
    if (is.null(new)) break
    new <- c(max(new[1L], 0), max(new[2L], 1e-8))
    delta <- (new[1L] - a1)^2 + (new[2L] - a0)^2
    a1 <- new[1L]; a0 <- new[2L]
    if (delta < tol) break
  }
  fitted <- ifelse(mu_bar > 0, a1 / mu_bar + a0, a0)
  list(a1 = a1, a0 = a0, fitted = fitted)
}

#' Benjamini-Hochberg step-up adjusted p-values
#'
#' @param p Numeric vector of raw p-values.
#' @return Numeric vector of adjusted p-values.
#' @export
.deseq2_benjamini_hochberg <- function(p) {
  n <- length(p)
  order_idx <- order(p)
  adj <- numeric(n); prev <- 1
  for (rank in n:1L) {
    i <- order_idx[rank]
    val <- min(prev, p[i] * n / rank)
    adj[i] <- val; prev <- val
  }
  adj
}

#' .ghc_deseq2_norm_cdf
#'
#' A step of the deseq2_native implementation. Called by \code{.ghc_deseq2_norm_ppf}, \code{deseq2}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param z Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.ghc_deseq2_norm_cdf <- function(z) 0.5 * (1 + .deseq2_erf(z / sqrt(2)))

#' .ghc_deseq2_norm_ppf
#'
#' A step of the deseq2_native implementation. Called by \code{deseq2}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param pr Passed to \code{<}.
#' @return A numeric value.
#' @export
.ghc_deseq2_norm_ppf <- function(pr) {
  lo <- -40; hi <- 40
  for (iter in seq_len(200L)) {
    mid <- 0.5 * (lo + hi)
    if (.ghc_deseq2_norm_cdf(mid) < pr) lo <- mid else hi <- mid
  }
  0.5 * (lo + hi)
}

#' .ghc_deseq2_quantile
#'
#' A step of the deseq2_native implementation. Called by \code{deseq2}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v Numeric; passed to \code{sort}.
#' @param pr Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.ghc_deseq2_quantile <- function(v, pr) {
  s <- sort(v)
  if (length(s) == 0L) stop("deseq2: empty quantile")
  pos <- pr * (length(s) - 1L)
  lo <- floor(pos); hi <- min(lo + 1L, length(s) - 1L)
  s[lo + 1L] + (pos - lo) * (s[hi + 1L] - s[lo + 1L])
}

#' DESeq2-style differential expression
#'
#' Full pipeline: size factors, gene-wise dispersion, trend, MAP
#' dispersion (with outliers left unshrunk), ridge-penalised LFC, Wald
#' test, BH.
#'
#' @param counts Gene-by-sample count matrix.
#' @param design Design matrix or vector of group labels.
#' @param contrast Optional contrast vector.
#' @param size Optional size factors.
#' @param beta_prior Whether to apply the LFC prior.
#' @param quantile_p \code{p} for the quantile matching that sets the
#'   prior width.
#' @param alpha_init Initial dispersion for the starting GLM.
#' @param min_disp Floor on the final dispersion.
#' @param log2 Whether to report log2 fold changes.
#' @return A list mirroring the Python \code{RichResult} payload.
#' @export
deseq2 <- function(counts, design, contrast = NULL, size = NULL,
                    beta_prior = TRUE, quantile_p = 0.05,
                    alpha_init = 0.1, min_disp = 1e-8, log2 = TRUE) {
  K <- lapply(counts, function(r) as.numeric(r))
  if (length(K) == 0L || length(K[[1L]]) == 0L)
    stop("deseq2: counts must be a non-empty matrix")
  n_genes <- length(K); m <- length(K[[1L]])
  first <- design[[1L]]
  if (is.numeric(first)) {
    X <- do.call(rbind, lapply(design, as.numeric))
  } else {
    levels <- character(0)
    for (lab in design) if (!(lab %in% levels)) levels <- c(levels, lab)
    if (length(levels) < 2L)
      stop("deseq2: the design has only one group, so no coefficient can be tested")
    X <- do.call(rbind, lapply(design, function(lab)
      c(1, as.numeric(levels[-1L] == lab))))
  }
  if (nrow(X) != m)
    stop(sprintf("deseq2: the design has %d rows but the counts have %d samples",
                 nrow(X), m))
  p <- ncol(X)
  if (m <= p)
    stop(sprintf("deseq2: %d samples and %d coefficients leaves no residual degrees of freedom",
                 m, p))
  s <- if (is.null(size)) size_factors(K) else as.numeric(size)
  if (length(s) != m || any(s <= 0))
    stop("deseq2: size factors must be positive, one per sample")
  base_mean <- vapply(seq_len(n_genes), function(i)
    sum(K[[i]] / s) / m, numeric(1))
  # step 1: gene-wise dispersions
  gw <- numeric(n_genes); mu0 <- vector("list", n_genes)
  for (i in seq_len(n_genes)) {
    if (base_mean[i] <= 0) {
      gw[i] <- min_disp; mu0[[i]] <- rep(1e-10, m); next
    }
    d <- dispersion_gene_wise(K[[i]], X, s, alpha_init)
    gw[i] <- d$dispersion; mu0[[i]] <- d$mu0
  }
  # step 2: trend
  usable <- which(base_mean > 0 & gw > 0)
  trend <- dispersion_trend(base_mean[usable], gw[usable])
  fitted <- ifelse(base_mean > 0, trend$a1 / base_mean + trend$a0, trend$a0)
  # step 3: prior width and MAP
  resid <- log(gw[usable]) - log(fitted[usable])
  s_lr <- if (length(resid) > 1L) .ghc_deseq2_mad(resid) else 0
  sigma_d2 <- max(s_lr^2 - .deseq2_trigamma((m - p) / 2), 0.25)
  disp <- numeric(n_genes); outlier <- rep(FALSE, n_genes)
  for (i in seq_len(n_genes)) {
    if (base_mean[i] <= 0) { disp[i] <- max(fitted[i], min_disp); next }
    if (log(gw[i]) > log(fitted[i]) + 2 * s_lr) {
      outlier[i] <- TRUE; disp[i] <- max(gw[i], min_disp); next
    }
    lf <- log(fitted[i])
    obj <- function(a) cox_reid_loglik(a, K[[i]], mu0[[i]], X) -
      (log(a) - lf)^2 / (2 * sigma_d2)
    disp[i] <- max(.ghc_deseq2_maximise_log_alpha(obj), min_disp)
  }
  # MLE coefficients
  mle <- lapply(seq_len(n_genes), function(i)
    nb_glm_fit(K[[i]], X, disp[i], s))
  c <- if (is.null(contrast))
    c(rep(0, p - 1L), 1) else as.numeric(contrast)
  if (length(c) != p)
    stop(sprintf("deseq2: the contrast must have one entry per coefficient (%d)", p))
  contrast_of <- function(fit) {
    val <- sum(c * fit$beta)
    var <- 0
    for (a in seq_len(p)) for (bb in seq_len(p))
      var <- var + c[a] * fit$sigma[a, bb] * c[bb]
    list(val = val, se = sqrt(max(var, 0)))
  }
  # LFC prior width by quantile matching
  sigma_r <- rep(Inf, p)
  for (r in 2:p) {
    vals <- abs(vapply(seq_len(n_genes), function(i)
      mle[[i]]$beta[r], numeric(1)))
    vals <- vals[base_mean > 0]
    if (length(vals) == 0L) { sigma_r[r] <- 1; next }
    emp <- .ghc_deseq2_quantile(vals, 1 - quantile_p)
    theo <- .ghc_deseq2_norm_ppf(1 - quantile_p / 2)
    sigma_r[r] <- max(emp / theo, 1e-6)
  }
  lam <- c(0, 1 / sigma_r[-1L]^2)
  lfc_mle <- numeric(n_genes); lfc_map <- numeric(n_genes)
  se_mle <- numeric(n_genes); se_map <- numeric(n_genes)
  for (i in seq_len(n_genes)) {
    cm <- contrast_of(mle[[i]])
    lfc_mle[i] <- cm$val; se_mle[i] <- cm$se
    fit <- if (isTRUE(beta_prior))
      nb_glm_fit(K[[i]], X, disp[i], s, lam, beta0 = mle[[i]]$beta)
      else mle[[i]]
    cf <- contrast_of(fit)
    lfc_map[i] <- cf$val; se_map[i] <- cf$se
  }
  scale_fac <- if (isTRUE(log2)) 1 / log(2) else 1
  est <- lfc_map * scale_fac
  est_mle <- lfc_mle * scale_fac
  se <- se_map * scale_fac
  stat <- ifelse(se > 0, est / se, 0)
  pval <- 2 * (1 - vapply(stat, function(z)
    .ghc_deseq2_norm_cdf(abs(z)), numeric(1)))
  padj <- .deseq2_benjamini_hochberg(pval)
  list(estimate = est, log_fold_change = est, lfc_mle = est_mle,
       lfc_se = se, lfc_se_mle = se_mle * scale_fac,
       stat = stat, pvalue = pval, padj = padj,
       base_mean = base_mean, dispersion = disp,
       dispersion_gene_wise = gw, dispersion_fit = fitted,
       dispersion_outlier = outlier, size_factors = s,
       sigma_d2 = sigma_d2, s_lr = s_lr, prior_sigma = sigma_r,
       trend = trend, beta_prior = isTRUE(beta_prior),
       n_genes = n_genes, n_samples = m, df_residual = m - p,
       scale = if (isTRUE(log2)) "log2" else "natural log",
       note = paste("independent filtering and Cook's-distance outlier",
                    "replacement are NOT applied, so padj here is over",
                    "all genes"),
       method = paste("DESeq2 negative binomial GLM with empirical",
                      "Bayes shrinkage (Love, Huber & Anders 2014)"))
}

#' Cheat sheet for the deseq2 module
#'
#' One-screen reminder of the module's entry points, printed to the console.
#'
#' @return The cheat sheet text, invisibly.
#' @export
.deseq2_cheatsheet <- function() {
  paste("deseq2: RNA-seq differential expression (Love, Huber & Anders",
        "2014). NB GLM with log link, Var = mu + alpha mu^2. Size",
        "factors by median-of-ratios. Dispersion in three steps:",
        "gene-wise by COX-REID adjusted likelihood (the adjustment is",
        "Bessel's correction for GLMs), a trend alpha_tr = a1/mu + a0",
        "fitted by gamma GLM, then MAP under a log-normal prior whose",
        "width is s_lr^2 - trigamma((m-p)/2), floored at 0.25. Genes",
        "more than 2 s_lr above the trend are dispersion OUTLIERS and",
        "are NOT shrunk. LFCs get a zero-centred normal prior whose",
        "width is set by quantile matching, making the fit ridge IRLS;",
        "SEs come from the posterior curvature. Wald test, BH.",
        "Independent filtering and Cook's outlier replacement are not",
        "implemented.")
}

#' @export
differential_expression <- deseq2
#' @export
deseq2_de <- deseq2
#' @export
deseq2_differential <- deseq2

# house entry point: the package exports one morie_<module>
morie_deseq2 <- deseq2
