# morie.fn -- function file (rootcoder007/morie)
# MAGMA: gene and gene-set analysis by regression.
#
# Single-marker association is underpowered when individual effects are
# weak. Aggregating markers into genes, and genes into sets, is the
# standard response, and the existing tools shared three problems:
# statistical power strongly affected by **linkage disequilibrium**
# between markers, multi-marker associations hard to detect, and
# reliance on **permutation** for p-values, which is what made the
# analysis expensive.
#
# **Gene analysis as multiple regression.** The markers in a gene are
# projected onto principal components of their LD structure, and the
# gene statistic is an F-test of the joint fit. Because the components
# are orthogonal, LD is accounted for by construction rather than by
# resampling -- and the p-value is analytic, which is where the speed
# comes from.
#
# **Gene-set analysis as a separate layer.** It is built *around* the
# gene analysis rather than fused into it, and that separation is the
# design decision: the gene-level results are computed once and reused,
# so a new gene set costs almost nothing. The set test is itself a
# regression of gene :math:`Z`-scores on set membership,
#
# .. math:: Z_g = \beta_0 + \beta_s S_g + \text{covariates} + \epsilon,
#
# which generalises immediately to **continuous** gene properties, to
# multiple sets at once, and to conditioning one set on another. A
# mean-difference test cannot do any of those.
#
# **Gene size and density must be covariates.** Longer genes carry more
# markers and larger statistics for reasons that have nothing to do with
# the trait; leaving them out yields sets enriched for nothing but
# length. ``gene_covariates`` builds them, and the anchor shows the
# spurious enrichment appearing when they are omitted.
#
# References
# ----------
# de Leeuw, C. A., Mooij, J. M., Heskes, T. & Posthuma, D. (2015)
# "MAGMA: Generalized Gene-Set Analysis of GWAS Data", *PLoS
# Computational Biology* 11(4), e1004219,
# doi:10.1371/journal.pcbi.1004219. The stated problems with existing
# gene and gene-set tools -- power strongly affected by linkage
# disequilibrium between markers, multi-marker associations hard to
# detect, and reliance on permutation making analysis computationally
# expensive; the gene analysis based on a MULTIPLE REGRESSION model for
# better statistical performance; the gene-set analysis built as a
# SEPARATE LAYER around the gene analysis for flexibility; the
# regression structure allowing generalisation to continuous properties
# of genes and simultaneous analysis of multiple gene sets and other
# gene properties; and the demonstration of more power at correct type-1
# error and considerably faster analysis.
#
# Purcell, S. et al. (2007) "PLINK: A Tool Set for Whole-Genome
# Association and Population-Based Linkage Analyses", *American Journal
# of Human Genetics* 81(3), 559-575, doi:10.1086/519795.

# Private helpers (prefixed .genemt_ to avoid namespace collision in
# the shared R/ environment)

.genemt_EPS <- 1e-12

#' .genemt_norm_cdf
#'
#' A step of the genemt_native implementation. Called by \code{morie_genemt_gene_set_regression}, \code{morie_genemt_gene_statistic}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Passed to \code{pnorm}.
#' @return The value of \code{pnorm}.
#' @export
.genemt_norm_cdf <- function(x) {
  pnorm(x)
}

#' .genemt_norm_ppf
#'
#' A step of the genemt_native implementation. Called by \code{morie_genemt_gene_statistic}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p Coerced to numeric by the body, with \code{as.numeric}.
#' @return The value of \code{qnorm}.
#' @export
.genemt_norm_ppf <- function(p) {
  q <- pmin(pmax(as.numeric(p), 1e-12), 1.0 - 1e-12)
  qnorm(q)
}

#' .genemt_wls
#'
#' A step of the genemt_native implementation. Called by \code{morie_genemt_gene_set_regression}, \code{morie_genemt_gene_statistic}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; passed to \code{nrow}.
#' @param y Numeric; combined arithmetically in the body.
#' @param w Numeric; passed to \code{sqrt}.
#' @param ridge Numeric; combined arithmetically in the body.
#' @return A list with \code{coef}.
#' @export
.genemt_wls <- function(X, y, w, ridge) {
  X <- as.matrix(X)
  y <- as.numeric(y)
  w <- as.numeric(w)

  n <- nrow(X)
  p <- ncol(X)

  sqw <- sqrt(w)
  Xw <- X * sqw
  yw <- y * sqw

  A <- crossprod(Xw) + ridge * diag(p)
  b <- crossprod(Xw, yw)
  coef <- solve(A, b)

  list(coef = coef)
}

# Public functions (prefixed morie_genemt_ to avoid namespace collision)

#' morie_genemt_ld_principal_components
#'
#' A step of the genemt_native implementation. Called by \code{morie_genemt_gene_statistic}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param G A matrix; passed to \code{as.matrix}.
#' @param keep Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.999}.
#' @return A list with \code{components}, \code{n_components}, \code{n_markers}, \code{variance_explained}, \code{note}.
#' @export
morie_genemt_ld_principal_components <- function(G, keep = 0.999) {
  M <- as.matrix(G)
  storage.mode(M) <- "numeric"
  n <- nrow(M)
  p <- ncol(M)

  # Standardize each column to mean 0, sd 1 (sample sd, n-1 in denom).
  cols <- matrix(0, nrow = n, ncol = p)
  for (j in seq_len(p)) {
    c <- M[, j]
    m <- mean(c)
    s <- sd(c)
    if (s > .genemt_EPS) {
      cols[, j] <- (c - m) / s
    } else {
      cols[, j] <- 0.0
    }
  }

  # Correlation matrix of the standardized columns.
  C <- crossprod(cols) / max(n - 1, 1)

  # Symmetric eigendecomposition; R returns values in decreasing order.
  ev <- eigen(C, symmetric = TRUE)
  vals <- ev$values
  vecs <- ev$vectors

  tot <- sum(pmax(vals, 0.0))
  if (tot == 0) tot <- 1
  acc <- 0.0
  take <- integer(0)
  for (i in seq_along(vals)) {
    if (vals[i] <= 1e-10) next
    take <- c(take, i)
    acc <- acc + vals[i] / tot
    if (acc >= as.numeric(keep)) break
  }

  PC <- cols %*% vecs[, take, drop = FALSE]

  list(components = PC,
       n_components = length(take),
       n_markers = p,
       variance_explained = acc,
       note = "orthogonal by construction, so LD needs no permutation")
}

#' morie_genemt_gene_statistic
#'
#' A step of the genemt_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @param G Passed to \code{morie_genemt_ld_principal_components}.
#' @param keep Passed to \code{morie_genemt_ld_principal_components}. Defaults to \code{0.999}.
#' @return A list with \code{F}, \code{df1}, \code{df2}, \code{p}, \code{z}, \code{n_markers}, \code{note}.
#' @export
morie_genemt_gene_statistic <- function(y, G, keep = 0.999) {
  yv <- as.numeric(y)
  pc <- morie_genemt_ld_principal_components(G, keep)
  X <- pc$components
  n <- nrow(X)
  m <- ncol(X)

  if (length(yv) != n) {
    stop(sprintf("genemt: %d phenotypes but %d individuals",
                 length(yv), n))
  }
  if (m < 1) {
    stop("genemt: the gene has no non-degenerate components")
  }

  # Design matrix: intercept + m retained principal components.
  Xa <- cbind(1, X)
  co <- .genemt_wls(Xa, yv, rep(1.0, n), 1e-8)$coef
  fit <- as.numeric(Xa %*% co)
  ybar <- sum(yv) / n
  ssr <- sum((fit - ybar)^2)
  sse <- sum((yv - fit)^2)
  d2 <- max(n - m - 1, 1)
  F <- if (sse > .genemt_EPS) (ssr / m) / (sse / d2) else Inf
  z <- sqrt(2.0 * F) - sqrt(2.0 * m - 1.0)
  p <- 1.0 - .genemt_norm_cdf(z)
  list(F = F, df1 = m, df2 = d2, p = p,
       z = .genemt_norm_ppf(1.0 - p),
       n_markers = pc$n_markers,
       note = "an ANALYTIC p-value; no permutation")
}

#' morie_genemt_gene_covariates
#'
#' A step of the genemt_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n_markers Coerced to numeric by the body, with \code{as.numeric}.
#' @param gene_length Coerced to numeric by the body, with \code{as.numeric}.
#' @param ld_scores Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{covariates}, \code{names}, \code{note}.
#' @export
morie_genemt_gene_covariates <- function(n_markers, gene_length,
                                         ld_scores = NULL) {
  nm <- as.numeric(n_markers)
  gl <- as.numeric(gene_length)
  if (length(nm) != length(gl)) {
    stop(sprintf("genemt: %d marker counts but %d lengths",
                 length(nm), length(gl)))
  }
  if (any(nm <= 0.0) || any(gl <= 0.0)) {
    stop("genemt: marker counts and lengths must be positive")
  }
  dens <- nm / gl
  cov <- cbind(log(nm), log(gl), log(dens))
  names_vec <- c("log_n_markers", "log_length", "log_density")
  if (!is.null(ld_scores)) {
    ls <- as.numeric(ld_scores)
    cov <- cbind(cov, ls)
    names_vec <- c(names_vec, "ld_score")
  }
  list(covariates = cov,
       names = names_vec,
       note = paste("not optional: without them, long genes look ",
                    "enriched for everything", sep = ""))
}

#' morie_genemt_gene_set_regression
#'
#' A step of the genemt_native implementation. Called by \code{morie_genemt_conditional_set_test}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param z_scores Coerced to numeric by the body, with \code{as.numeric}.
#' @param membership Coerced to numeric by the body, with \code{as.numeric}.
#' @param covariates Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.
#' @return A list with \code{estimate}, \code{beta}, \code{se}, \code{t}, \code{p}, \code{n_genes}, \code{covariates_used}, \code{method}, \code{note}.
#' @export
morie_genemt_gene_set_regression <- function(z_scores, membership,
                                             covariates = NULL) {
  z <- as.numeric(z_scores)
  s <- as.numeric(membership)
  n <- length(z)
  if (length(s) != n) {
    stop(sprintf("genemt: %d z-scores but %d membership values",
                 n, length(s)))
  }
  # Design matrix: intercept + membership (+ optional covariates).
  X <- cbind(1, s)
  if (!is.null(covariates)) {
    C <- as.matrix(covariates)
    storage.mode(C) <- "numeric"
    if (nrow(C) != n) {
      stop(sprintf("genemt: %d covariate rows for %d genes",
                   nrow(C), n))
    }
    X <- cbind(X, C)
  }
  co <- .genemt_wls(X, z, rep(1.0, n), 1e-8)$coef
  fit <- as.numeric(X %*% co)
  res <- z - fit
  dof <- max(n - ncol(X), 1)
  s2 <- sum(res * res) / dof
  sm <- sum(s) / n
  sxx <- sum((s - sm)^2)
  se <- if (sxx > .genemt_EPS) sqrt(s2 / sxx) else Inf
  t_stat <- if (se > 0) co[2] / se else 0.0
  list(estimate = co[2], beta = co[2], se = se, t = t_stat,
       p = 1.0 - .genemt_norm_cdf(t_stat),
       n_genes = n,
       covariates_used = !is.null(covariates),
       method = "MAGMA gene-set regression; de Leeuw et al. (2015)",
       note = paste("one-sided: enrichment means a POSITIVE ",
                    "coefficient", sep = ""))
}

#' morie_genemt_conditional_set_test
#'
#' A step of the genemt_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param z_scores Coerced to numeric by the body, with \code{as.numeric}.
#' @param set_a Coerced to numeric by the body, with \code{as.numeric}.
#' @param set_b Coerced to numeric by the body, with \code{as.numeric}.
#' @param covariates Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.
#' @return A list with \code{marginal_beta}, \code{marginal_p}, \code{conditional_beta}, \code{conditional_p}, \code{attenuation}, \code{note}.
#' @export
morie_genemt_conditional_set_test <- function(z_scores, set_a, set_b,
                                             covariates = NULL) {
  z <- as.numeric(z_scores)
  a <- as.numeric(set_a)
  b <- as.numeric(set_b)
  n <- length(z)
  if (!(length(a) == length(b) && length(b) == n)) {
    stop("genemt: the sets and z-scores differ in length")
  }
  # Conditional covariates: set_b (+ optional extra covariates).
  # gene_set_regression will prepend its own intercept, so do not
  # include one here.
  base <- cbind(b)
  if (!is.null(covariates)) {
    C <- as.matrix(covariates)
    storage.mode(C) <- "numeric"
    base <- cbind(base, C)
  }
  marg <- morie_genemt_gene_set_regression(z, a, covariates)
  cond <- morie_genemt_gene_set_regression(z, a, base)
  attenuation <- if (abs(marg$beta) > .genemt_EPS) {
    (marg$beta - cond$beta) / marg$beta
  } else {
    0.0
  }
  list(marginal_beta = marg$beta, marginal_p = marg$p,
       conditional_beta = cond$beta, conditional_p = cond$p,
       attenuation = attenuation,
       note = paste("if the signal was really the other set, the ",
                    "conditional coefficient collapses", sep = ""))
}

#' morie_genemt_cheatsheet
#'
#' A step of the genemt_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
morie_genemt_cheatsheet <- function() {
  paste("genemt: single markers are underpowered, so aggregate -- ",
        "but existing tools lost power to LINKAGE DISEQUILIBRIUM ",
        "and needed PERMUTATION for p-values. MAGMA's gene test is ",
        "a MULTIPLE REGRESSION on principal components of the LD ",
        "structure: orthogonal by construction, analytic p-value, ",
        "hence fast. The gene-set test is a SEPARATE LAYER around ",
        "it -- a regression of gene Z-scores on membership, which ",
        "generalises to CONTINUOUS gene properties, multiple sets ",
        "at once, and conditioning one set on another. Gene size ",
        "and density are covariates, or long genes look enriched ",
        "for everything.", sep = "")
}

# Compact alias per ledger/NAMING.md
morie_genemt_magma <- morie_genemt_gene_set_regression

# Public name resolved by fn/_lazy_map.json
morie_genemt_gene_meta_analysis <- morie_genemt_gene_set_regression

#' @rdname morie_genemt_ld_principal_components
#' @export
morie_genemt <- morie_genemt_ld_principal_components





