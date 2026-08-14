# Differential expression for count data: DESeq2.
# Source: Love, M. I., Huber, W. & Anders, S. (2014) "Moderated
# estimation of fold change and dispersion for RNA-seq data with
# DESeq2", Genome Biology 15:550. Equations 1-2 (negative binomial GLM
# with log link, Var = mu + alpha mu^2), equation 5 (log-normal prior
# on the dispersion), equation 6 (the trend a1/mu + a0), equation 7
# (Cox-Reid adjusted likelihood), equation 8 (the robust s_lr), and
# equation 9 (the MAP with the log-normal prior). Equation 10 is the
# zero-centred normal prior on every non-intercept coefficient, and
# the Benjamini-Hochberg adjustment is the standard one.
#
# Native implementation mirroring Python morie.fn.deseq2 exactly: the
# same median-of-ratios size factors, the same three-step dispersion
# estimation, the same dispersion-outlier rule, the same quantile-
# matching LFC prior, the same Wald test, and the same BH adjusted
# p-values. Independent filtering and Cook's-distance outlier
# replacement are NOT applied, so padj here is over all genes.

#' Trigamma function
#'
#' Recurrence up to a large argument, then the asymptotic series.
#' \code{Var(log X) = psi_1(f/2)} for \code{X ~ chi^2_f}.
#'
#' @param x Numeric > 0.
#' @return Numeric.
#' @export
morie_deseq2_trigamma <- function(x) {
  x <- as.numeric(x)
  if (x <= 0) stop("deseq2: trigamma needs x > 0")
  tot <- 0
  while (x < 20) {
    tot <- tot + 1 / (x * x)
    x <- x + 1
  }
  inv <- 1 / x
  inv2 <- inv * inv
  tot + inv * (1 + 0.5 * inv + inv2 * (1 / 6 + inv2 * (-1 / 30 + inv2 * (1 / 42 - inv2 / 30))))
}

#' Median
#'
#' @param v Numeric vector.
#' @return Numeric.
#' @keywords internal
#' @noRd
.deseq_median <- function(v) {
  s <- sort(v)
  n <- length(s)
  if (n == 0L) stop("deseq2: median of an empty sequence")
  if (n %% 2L) s[n %/% 2L + 1L] else 0.5 * (s[n %/% 2L] + s[n %/% 2L + 1L])
}

#' Median absolute deviation scaled to a standard normal
#'
#' @param v Numeric vector.
#' @return Numeric.
#' @keywords internal
#' @noRd
.deseq_mad <- function(v) {
  m <- .deseq_median(v)
  .deseq_median(abs(v - m)) / 0.6744897501960817
}

#' Median-of-ratios size factors
#'
#' The reference is the geometric mean of each gene across samples,
#' and \code{s_j = median_i K_ij / K_i^R} over genes with a non-zero
#' reference. Genes with any zero count have \code{K_i^R = 0} and
#' drop out, which is what makes the estimator robust to genes
#' present in only some samples.
#'
#' @param counts Numeric matrix (genes in rows, samples in columns).
#' @return Numeric vector of size factors, one per sample.
#' @export
morie_deseq2_size_factors <- function(counts) {
  K <- as.matrix(counts)
  if (nrow(K) == 0L || ncol(K) == 0L)
    stop("deseq2: counts must be a non-empty gene x sample matrix")
  m <- ncol(K)
  ratios <- vector("list", m)
  for (i in seq_len(m)) ratios[[i]] <- numeric(0)
  for (i in seq_len(nrow(K))) {
    row <- K[i, ]
    if (length(row) != m)
      stop("deseq2: ragged count matrix")
    if (any(row < 0))
      stop("deseq2: counts must be non-negative")
    if (any(row <= 0)) next
    gm <- exp(sum(log(row)) / m)
    for (j in seq_len(m)) ratios[[j]] <- c(ratios[[j]], row[j] / gm)
  }
  if (length(ratios[[1L]]) == 0L)
    stop("deseq2: no gene has a positive count in every sample, so median-of-ratios has no reference")
  vapply(ratios, .deseq_median, numeric(1))
}

#' Negative binomial log-likelihood summed over samples
#'
#' @param K Numeric vector of counts.
#' @param mu Numeric vector of means.
#' @param alpha Numeric dispersion.
#' @return Numeric.
#' @keywords internal
#' @noRd
.nb_loglik <- function(K, mu, alpha) {
  if (alpha <= 0) {
    return(sum(K * log(mu) - mu - lgamma(K + 1)))
  }
  r <- 1 / alpha
  tot <- 0
  for (k in seq_along(K)) {
    m <- mu[k]
    tot <- tot + (lgamma(K[k] + r) - lgamma(r) - lgamma(K[k] + 1) +
                  r * log(r / (r + m)) + K[k] * log(m / (r + m)))
  }
  tot
}

#' Log-determinant of the Fisher information
#'
#' @param X Numeric design matrix.
#' @param mu Numeric fitted means.
#' @param alpha Numeric dispersion.
#' @return Numeric log-determinant, or -Inf if singular.
#' @keywords internal
#' @noRd
.xtwx_logdet <- function(X, mu, alpha) {
  p <- ncol(X)
  M <- matrix(0, p, p)
  for (j in seq_len(nrow(X))) {
    w <- 1 / (1 / mu[j] + alpha)
    M <- M + w * X[j, , drop = FALSE] %*% t(X)[, j, drop = FALSE]
  }
  s <- svd(M)
  prod(s$d)
}

#' Cox-Reid adjusted profile log-likelihood
#'
#' Equation 7: \code{ell(alpha) - (1/2) log det(X^T W X)}.
#'
#' @param alpha Numeric dispersion.
#' @param K Numeric vector of counts.
#' @param mu Numeric vector of fitted means.
#' @param X Numeric design matrix.
#' @return Numeric.
#' @export
morie_deseq2_cox_reid_loglik <- function(alpha, K, mu, X) {
  if (alpha <= 0) stop("deseq2: alpha must be positive")
  .nb_loglik(K, mu, alpha) - 0.5 * log(.xtwx_logdet(X, mu, alpha))
}

#' Negative binomial GLM by ridge-penalised IRLS
#'
#' @param K Numeric vector of counts.
#' @param X Numeric design matrix.
#' @param alpha Numeric dispersion.
#' @param s Optional numeric size factor vector.
#' @param lam Optional numeric ridge penalty vector, length p.
#' @param max_iter Integer, maximum iterations.
#' @param tol Numeric, convergence tolerance.
#' @param beta0 Optional starting coefficient vector.
#' @return A list with \code{beta}, \code{mu}, \code{sigma},
#'   \code{converged}, \code{n_iter}.
#' @export
morie_deseq2_nb_glm_fit <- function(K, X, alpha, s = NULL, lam = NULL,
                                    max_iter = 100L, tol = 1e-8,
                                    beta0 = NULL) {
  m <- length(K); p <- ncol(X)
  if (is.null(s)) s <- rep(1, m)
  if (is.null(lam)) lam <- rep(0, p)
  if (is.null(beta0)) {
    base <- max(sum(K / s) / m, 0.1)
    beta <- c(log(base), rep(0, p - 1L))
  } else {
    beta <- as.numeric(beta0)
  }
  converged <- FALSE
  it <- 0L
  for (it in seq_len(as.integer(max_iter))) {
    mu <- numeric(m)
    for (j in seq_len(m)) {
      eta <- sum(X[j, ] * beta)
      mu[j] <- max(s[j] * exp(min(eta, 50)), 1e-10)
    }
    M <- matrix(0, p, p)
    v <- numeric(p)
    for (j in seq_len(m)) {
      w <- 1 / (1 / mu[j] + alpha)
      z <- log(mu[j] / s[j]) + (K[j] - mu[j]) / mu[j]
      v <- v + w * X[j, ] * z
      M <- M + w * tcrossprod(X[j, , drop = FALSE])
    }
    Mr <- M + diag(lam, p, p)
    ok <- TRUE
    new <- tryCatch(as.numeric(solve(Mr, v)), error = function(e) NULL)
    if (is.null(new)) {
      stop("deseq2: the GLM design is singular; check the design matrix for collinear columns")
    }
    step <- max(abs(new - beta))
    beta <- new
    if (step < tol) { converged <- TRUE; break }
  }
  mu <- numeric(m)
  for (j in seq_len(m)) {
    eta <- sum(X[j, ] * beta)
    mu[j] <- max(s[j] * exp(min(eta, 50)), 1e-10)
  }
  inv <- solve(M)
  list(beta = beta, mu = mu, sigma = inv, converged = converged, n_iter = it)
}

#' Maximise a function on a log scale by grid then golden-section
#'
#' @param obj Function of \code{alpha} to maximise.
#' @param lo Numeric, lower bound on \code{log alpha}.
#' @param hi Numeric, upper bound on \code{log alpha}.
#' @param n_grid Integer, grid size.
#' @param refine Integer, golden-section iterations.
#' @return Numeric.
#' @keywords internal
#' @noRd
.maximise_log_alpha <- function(obj, lo = -15, hi = 5,
                                n_grid = 60L, refine = 60L) {
  best_u <- lo; best_v <- obj(exp(lo))
  for (g in seq_len(n_grid)) {
    u <- lo + (hi - lo) * g / n_grid
    val <- obj(exp(u))
    if (val > best_v) { best_u <- u; best_v <- val }
  }
  step <- (hi - lo) / n_grid
  a <- best_u - step; b <- best_u + step
  phi <- (sqrt(5) - 1) / 2
  c <- b - phi * (b - a); d <- a + phi * (b - a)
  fc <- obj(exp(c)); fd <- obj(exp(d))
  for (i in seq_len(refine)) {
    if (fc > fd) { b <- d; d <- c; fd <- fc; c <- b - phi * (b - a); fc <- obj(exp(c)) }
    else { a <- c; c <- d; fc <- fd; d <- a + phi * (b - a); fd <- obj(exp(d)) }
  }
  exp(0.5 * (a + b))
}

#' Gene-wise dispersion estimate
#'
#' Maximises equation 7 over \code{alpha} with \code{mu} held fixed at
#' the GLM fit from an initial method-of-moments dispersion.
#'
#' @param K Numeric vector of counts.
#' @param X Numeric design matrix.
#' @param s Numeric size factors.
#' @param alpha_init Numeric initial dispersion.
#' @return A list with \code{alpha} and \code{mu0}.
#' @export
morie_deseq2_dispersion_gene_wise <- function(K, X, s, alpha_init = 0.1) {
  fit0 <- morie_deseq2_nb_glm_fit(K, X, alpha_init, s)
  mu0 <- fit0$mu
  ahat <- .maximise_log_alpha(
    function(a) morie_deseq2_cox_reid_loglik(a, K, mu0, X))
  list(alpha = ahat, mu0 = mu0)
}

#' Fit the dispersion trend a1/mu + a0
#'
#' Gamma-family GLM with an identity link, iterating with genes whose
#' dispersion-to-fit ratio falls outside \code{[1e-4, 15]} excluded.
#'
#' @param mu_bar Numeric vector of mean normalised counts.
#' @param disp Numeric vector of gene-wise dispersions.
#' @param max_iter Integer iterations.
#' @param tol Numeric convergence tolerance.
#' @return A list with \code{a1}, \code{a0} and \code{fitted}.
#' @export
morie_deseq2_dispersion_trend <- function(mu_bar, disp, max_iter = 10L,
                                          tol = 1e-6) {
  keep <- which(disp > 0 & mu_bar > 0)
  if (length(keep) < 3L)
    stop("deseq2: too few genes with positive dispersion to fit the trend")
  a1 <- 1
  a0 <- max(1e-8, .deseq_median(disp[keep]))
  for (it in seq_len(as.integer(max_iter))) {
    rows <- keep[vapply(keep, function(i) {
      fit <- a1 / mu_bar[i] + a0
      1e-4 <= disp[i] / fit && disp[i] / fit <= 15
    }, logical(1))]
    if (length(rows) < 3L) rows <- keep
    M <- matrix(0, 2, 2); v <- numeric(2)
    for (i in rows) {
      fit <- max(a1 / mu_bar[i] + a0, 1e-12)
      w <- 1 / (fit * fit)
      xrow <- c(1 / mu_bar[i], 1)
      v <- v + w * xrow * disp[i]
      M <- M + w * tcrossprod(xrow)
    }
    new <- tryCatch(as.numeric(solve(M, v)), error = function(e) NULL)
    if (is.null(new)) break
    new <- c(max(new[1], 0), max(new[2], 1e-8))
    delta <- (new[1] - a1) ^ 2 + (new[2] - a0) ^ 2
    a1 <- new[1]; a0 <- new[2]
    if (delta < tol) break
  }
  fitted <- vapply(seq_along(mu_bar), function(i) {
    if (mu_bar[i] > 0) a1 / mu_bar[i] + a0 else a0
  }, numeric(1))
  list(a1 = a1, a0 = a0, fitted = fitted)
}

#' Benjamini-Hochberg adjusted p-values
#'
#' @param p Numeric vector of p-values.
#' @return Numeric vector of adjusted p-values.
#' @export
morie_deseq2_benjamini_hochberg <- function(p) {
  n <- length(p)
  ord <- order(p)
  adj <- numeric(n)
  prev <- 1
  for (rank in n:1) {
    i <- ord[rank]
    val <- min(prev, p[i] * n / rank)
    adj[i] <- val
    prev <- val
  }
  adj
}

#' Standard normal CDF
#'
#' @param z Numeric.
#' @return Numeric.
#' @keywords internal
#' @noRd
.norm_cdf <- function(z) 0.5 * (1 + pnorm(z))

#' Standard normal quantile
#'
#' @param pr Numeric in (0, 1).
#' @return Numeric.
#' @keywords internal
#' @noRd
.norm_ppf <- function(pr) qnorm(pr)

#' Linear interpolation quantile
#'
#' @param v Numeric vector.
#' @param pr Numeric in [0, 1].
#' @return Numeric.
#' @keywords internal
#' @noRd
.deq_quantile <- function(v, pr) {
  s <- sort(v)
  pos <- pr * (length(s) - 1)
  lo <- floor(pos); hi <- min(lo + 1, length(s) - 1)
  s[lo + 1] + (pos - lo) * (s[hi + 1] - s[lo + 1])
}

#' Differential expression by the DESeq2 pipeline
#'
#' @param counts Numeric matrix, genes in rows and samples in columns.
#' @param design Numeric design matrix or a sequence of group labels.
#' @param contrast Optional numeric contrast vector.
#' @param size Optional numeric size factor vector.
#' @param beta_prior Logical, apply the LFC prior.
#' @param quantile_p Numeric, the \code{p} in the quantile matching.
#' @param alpha_init Numeric, initial dispersion for the GLM.
#' @param min_disp Numeric, floor on the final dispersion.
#' @param log2 Logical, report fold changes on the log2 scale.
#' @return A list mirroring the Python arm's payload.
#' @references Love, M. I., Huber, W. & Anders, S. (2014). Genome
#'   Biology 15:550.
#' @export
morie_deseq2 <- function(counts, design, contrast = NULL, size = NULL,
                          beta_prior = TRUE, quantile_p = 0.05,
                          alpha_init = 0.1, min_disp = 1e-8,
                          log2 = TRUE) {
  K <- as.matrix(counts)
  if (nrow(K) == 0L || ncol(K) == 0L)
    stop("deseq2: counts must be a non-empty matrix")
  n_genes <- nrow(K); m <- ncol(K)
  first <- design[[1L]]
  if (is.numeric(first) || is.list(first)) {
    X <- as.matrix(design)
  } else {
    levels <- unique(design)
    if (length(levels) < 2L)
      stop("deseq2: the design has only one group, so no coefficient can be tested")
    X <- sapply(design, function(lab) {
      c(1, vapply(levels[-1L], function(lv) if (lab == lv) 1 else 0, numeric(1)))
    })
    X <- t(X)
  }
  if (nrow(X) != m)
    stop(sprintf("deseq2: the design has %d rows but the counts have %d samples",
                 nrow(X), m))
  p <- ncol(X)
  if (m <= p)
    stop(sprintf("deseq2: %d samples and %d coefficients leaves no residual degrees of freedom",
                 m, p))
  s <- if (is.null(size)) morie_deseq2_size_factors(K) else as.numeric(size)
  if (length(s) != m || any(s <= 0))
    stop("deseq2: size factors must be positive, one per sample")
  base_mean <- rowMeans(t(t(K) / s))

  gw <- numeric(n_genes); mu0 <- vector("list", n_genes)
  for (i in seq_len(n_genes)) {
    if (base_mean[i] <= 0) { gw[i] <- min_disp; mu0[[i]] <- rep(1e-10, m); next }
    r <- morie_deseq2_dispersion_gene_wise(K[i, ], X, s, alpha_init)
    gw[i] <- r$alpha; mu0[[i]] <- r$mu0
  }

  usable <- which(base_mean > 0 & gw > 0)
  trend <- morie_deseq2_dispersion_trend(base_mean[usable], gw[usable])
  fitted <- vapply(seq_len(n_genes), function(i) {
    if (base_mean[i] > 0) trend$a1 / base_mean[i] + trend$a0 else trend$a0
  }, numeric(1))

  if (length(usable) > 1L) {
    resid <- log(gw[usable]) - log(fitted[usable])
    s_lr <- .deseq_mad(resid)
  } else {
    s_lr <- 0
  }
  sigma_d2 <- max(s_lr ^ 2 - morie_deseq2_trigamma((m - p) / 2), 0.25)
  disp <- numeric(n_genes); outlier <- rep(FALSE, n_genes)
  for (i in seq_len(n_genes)) {
    if (base_mean[i] <= 0) { disp[i] <- max(fitted[i], min_disp); next }
    if (log(gw[i]) > log(fitted[i]) + 2 * s_lr) {
      outlier[i] <- TRUE
      disp[i] <- max(gw[i], min_disp); next
    }
    lf <- log(fitted[i])
    obj <- function(a) {
      morie_deseq2_cox_reid_loglik(a, K[i, ], mu0[[i]], X) -
        (log(a) - lf) ^ 2 / (2 * sigma_d2)
    }
    disp[i] <- max(.maximise_log_alpha(obj), min_disp)
  }

  mle <- vector("list", n_genes)
  for (i in seq_len(n_genes))
    mle[[i]] <- morie_deseq2_nb_glm_fit(K[i, ], X, disp[i], s)

  cc <- if (is.null(contrast)) c(rep(0, p - 1L), 1) else as.numeric(contrast)
  if (length(cc) != p)
    stop(sprintf("deseq2: the contrast must have one entry per coefficient (%d)", p))

  contrast_of <- function(fit) {
    val <- sum(cc * fit$beta)
    var <- 0
    for (a in seq_len(p)) for (b in seq_len(p)) var <- var + cc[a] * fit$sigma[a, b] * cc[b]
    list(val = val, sd = sqrt(max(var, 0)))
  }

  sigma_r <- rep(Inf, p)
  for (r in 2:p) {
    vals <- abs(vapply(seq_len(n_genes), function(i) mle[[i]]$beta[r], numeric(1)))
    vals <- vals[base_mean > 0]
    if (length(vals) == 0L) { sigma_r[r] <- 1; next }
    emp <- .deq_quantile(vals, 1 - quantile_p)
    theo <- qnorm(1 - quantile_p / 2)
    sigma_r[r] <- max(emp / theo, 1e-6)
  }
  lam <- c(0, 1 / sigma_r[2:p] ^ 2)

  lfc_mle <- numeric(n_genes); lfc_map <- numeric(n_genes)
  se_map <- numeric(n_genes); se_mle <- numeric(n_genes)
  for (i in seq_len(n_genes)) {
    cm <- contrast_of(mle[[i]])
    lfc_mle[i] <- cm$val; se_mle[i] <- cm$sd
    if (beta_prior) {
      fit <- morie_deseq2_nb_glm_fit(K[i, ], X, disp[i], s, lam,
                                     beta0 = mle[[i]]$beta)
    } else fit <- mle[[i]]
    cm2 <- contrast_of(fit)
    lfc_map[i] <- cm2$val; se_map[i] <- cm2$sd
  }
  scale <- if (log2) 1 / log(2) else 1
  est <- lfc_map * scale
  est_mle <- lfc_mle * scale
  se <- se_map * scale
  stat <- ifelse(se > 0, est / se, 0)
  pval <- 2 * (1 - .norm_cdf(abs(stat)))
  padj <- morie_deseq2_benjamini_hochberg(pval)
  list(estimate = est, log_fold_change = est, lfc_mle = est_mle,
       lfc_se = se, lfc_se_mle = se_mle * scale, stat = stat,
       pvalue = pval, padj = padj, base_mean = base_mean,
       dispersion = disp, dispersion_gene_wise = gw,
       dispersion_fit = fitted, dispersion_outlier = outlier,
       size_factors = s, sigma_d2 = sigma_d2, s_lr = s_lr,
       prior_sigma = sigma_r, trend = trend,
       beta_prior = isTRUE(beta_prior), n_genes = n_genes,
       n_samples = m, df_residual = m - p,
       scale = if (log2) "log2" else "natural log",
       note = paste("independent filtering and Cook's-distance outlier",
                    "replacement are NOT applied, so padj here is over",
                    "all genes"),
       method = paste("DESeq2 negative binomial GLM with empirical",
                      "Bayes shrinkage (Love, Huber & Anders 2014)"))
}
