# morie.fn -- function file (rootcoder007/morie)
# Whole-genome regression in two ridge levels, REGENIE style.
#
# The problem. A genome-wide association scan tests each variant
# against a phenotype, but relatedness and polygenic background inflate
# the statistics. Mixed models fix that and are slow. REGENIE gets the
# same correction from stacked ridge regressions, decoupled into two
# steps so Step 1 is fitted once and reused.
#
# Step 1, level 0. Partition the array markers into consecutive
# blocks of B (the paper uses 1000). Inside a block, fit J ridge
# regressions (the paper uses 5) at different shrinkage parameters,
# which is the maximum a posteriori estimate under a Gaussian prior on
# the block's effect sizes.
#
# Step 1, level 1. Stack those predictors and fit a second ridge
# within cross-validation to combine them into one predictor.
#
# LOCO. The combined predictor is then decomposed by chromosome: the
# prediction for chromosome c is the sum of the level-0 contributions
# from every other chromosome.
#
# Step 2. Each variant is tested with the LOCO prediction as a
# covariate.
#
# References:
# Mbatchou, J., et al. (2021) "Computationally efficient whole-genome
# regression for quantitative and binary traits", Nature Genetics
# 53(7), 1097-1103, doi:10.1038/s41588-021-00870-7.
#
# Hoerl, A. E. & Kennard, R. W. (1970) "Ridge Regression: Biased
# Estimation for Nonorthogonal Problems", Technometrics 12(1), 55-67.
#
# Yang, J., et al. (2014) "Advantages and pitfalls in the application
# of mixed-model association methods", Nature Genetics 46(2),
# 100-106.

CV_SCHEMES <- c("kfold", "loo")

#' .regmlm_lambda_grid
#'
#' A step of the regmlm_native implementation. Called by \code{level0_predictors}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p Numeric; combined arithmetically in the body.
#' @param n Accepted by the signature and not used anywhere in the body.
#' @param n_ridge Coerced to integer by the body, with \code{as.integer}. Defaults to \code{5}.
#' @return A numeric value.
#' @export
#' @examples
#' res <- .regmlm_lambda_grid(p = 0.5, n = 3L)
#' res
.regmlm_lambda_grid <- function(p, n, n_ridge = 5) {
  j <- as.integer(n_ridge)
  if (j < 1) stop("regmlm: need at least one ridge predictor")
  hs <- seq_len(j) / (j + 1)
  p * (1 - hs) / hs
}

#' .regmlm_folds
#'
#' A step of the regmlm_native implementation. Called by \code{level1_stack}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n A count; the body uses it as \code{seq_len(...)}.
#' @param k A count; the body uses it as \code{seq_len(...)}.
#' @param scheme Compared against \code{"loo"}.
#' @return The value of \code{lapply}.
#' @export
.regmlm_folds <- function(n, k, scheme) {
  if (scheme == "loo") {
    return(lapply(seq_len(n), function(i) i))
  }
  k <- max(2, min(as.integer(k), n))
  lapply(seq_len(k) - 1, function(f) which((seq_len(n) - 1) %% k == f))
}

#' .regmlm_residualise
#'
#' A step of the regmlm_native implementation. Called by \code{test_variant}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v A matrix; passed to \code{crossprod}.
#' @param cols A matrix; passed to \code{\%*\%}.
#' @return A numeric value.
#' @export
.regmlm_residualise <- function(v, cols) {
  if (!is.matrix(cols)) {
    cols <- do.call(cbind, cols)
  }
  A <- crossprod(cols)
  b <- crossprod(cols, v)
  coef <- solve(A, b)
  v - as.numeric(cols %*% coef)
}

#' make_blocks
#'
#' A step of the regmlm_native implementation. Called by \code{morie_regmlm}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n_markers Coerced to integer by the body, with \code{as.integer}.
#' @param chromosomes Optional; may be \code{NULL}. Coerced to integer by the body, with
#' \code{as.integer}.
#' @param block_size Coerced to integer by the body, with \code{as.integer}. Defaults to
#' \code{1000}.
#' @return The value of \code{blocks}, as built in the body.
#' @export
make_blocks <- function(n_markers, chromosomes = NULL, block_size = 1000) {
  n <- as.integer(n_markers)
  b <- as.integer(block_size)
  if (n < 1) stop("regmlm: no markers")
  if (b < 1) stop("regmlm: block_size must be positive")

  if (is.null(chromosomes)) {
    chrom <- rep(0L, n)
  } else {
    chrom <- as.integer(chromosomes)
  }

  if (length(chrom) != n) stop("regmlm: one chromosome label per marker")

  blocks <- list()
  start <- 1L
  for (i in seq_len(n)) {
    boundary <- (i == n) || ((i > 1) && (chrom[i] != chrom[i - 1])) || ((i - start + 1) >= b)
    if (boundary) {
      blocks[[length(blocks) + 1]] <- list(
        # Python reports a half-open block [start, stop): start is 0-based,
        # stop is the EXCLUSIVE end, which numerically equals R's 1-based
        # INCLUSIVE stop. So start shifts and stop does not.
        start = start - 1L,
        stop = i,
        chromosome = chrom[start],
        size = i - start + 1
      )
      start <- i + 1
    }
  }
  blocks
}

#' ridge_fit
#'
#' A step of the regmlm_native implementation. Called by \code{level0_predictors},
#' \code{level1_stack}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; passed to \code{nrow}.
#' @param y A matrix; passed to \code{crossprod}.
#' @param lam Numeric; combined arithmetically in the body.
#' @return A list with \code{beta}, \code{fitted}, \code{lam}.
#' @export
ridge_fit <- function(X, y, lam) {
  n <- length(y)
  if (!is.matrix(X)) {
    if (n != length(X)) stop("regmlm: X and y must have the same length")
    p <- length(X[[1]])
    X <- do.call(rbind, X)
  } else {
    if (n != nrow(X)) stop("regmlm: X and y must have the same length")
    p <- ncol(X)
  }
  if (lam < 0) stop("regmlm: the shrinkage parameter cannot be negative")

  y <- as.numeric(y)
  A <- crossprod(X) + lam * diag(p)
  b <- crossprod(X, y)
  beta <- solve(A, b)
  fitted <- as.numeric(X %*% beta)

  list(beta = as.numeric(beta), fitted = fitted, lam = as.numeric(lam))
}

#' level0_predictors
#'
#' A step of the regmlm_native implementation. Called by \code{morie_regmlm}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param G A matrix; indexed by row and column.
#' @param y A vector; its length is taken.
#' @param blocks See Usage.
#' @param n_ridge Passed to \code{.regmlm_lambda_grid}. Defaults to \code{5}.
#' @return A list with \code{predictors}, \code{meta}, \code{n_predictors}, \code{reduction}.
#' @export
level0_predictors <- function(G, y, blocks, n_ridge = 5) {
  n <- length(y)
  if (!is.matrix(G)) G <- do.call(rbind, G)

  preds <- list()
  meta <- list()

  for (b in blocks) {
    idx <- seq_len(b$size) + b$start - 1
    Xb <- G[, idx, drop = FALSE]

    for (lam in .regmlm_lambda_grid(b$size, n, n_ridge)) {
      f <- ridge_fit(Xb, y, lam)
      preds[[length(preds) + 1]] <- f$fitted
      meta[[length(meta) + 1]] <- list(
        chromosome = b$chromosome,
        start = b$start,
        stop = b$stop,
        lam = lam
      )
    }
  }

  n_pred <- length(preds)
  reduction <- if (n_pred > 0) ncol(G) / n_pred else 0

  list(
    predictors = preds,
    meta = meta,
    n_predictors = n_pred,
    reduction = reduction
  )
}

#' level1_stack
#'
#' A step of the regmlm_native implementation. Called by \code{morie_regmlm}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param preds A vector; its length is taken.
#' @param y A vector; its length is taken and its elements indexed.
#' @param cv Passed to \code{.regmlm_folds}. Defaults to \code{"kfold"}.
#' @param k Passed to \code{.regmlm_folds}. Defaults to \code{5}.
#' @param lam Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{weights}, \code{prediction}, \code{out_of_fold}, \code{cv},
#' \code{lam}, \code{n_predictors}.
#' @export
level1_stack <- function(preds, y, cv = "kfold", k = 5, lam = NULL) {
  if (!cv %in% CV_SCHEMES) {
    stop(sprintf("regmlm: cv must be one of %s, got %s",
                 paste(CV_SCHEMES, collapse = ", "), cv))
  }
  n <- length(y)
  m <- length(preds)
  if (m == 0) stop("regmlm: no level-0 predictors")

  X <- do.call(cbind, preds)

  lam <- if (is.null(lam)) as.numeric(m) else as.numeric(lam)

  oof <- rep(0.0, n)
  folds <- .regmlm_folds(n, k, cv)

  for (fold in folds) {
    keep <- setdiff(seq_len(n), fold)
    if (length(keep) == 0) next

    f <- ridge_fit(X[keep, , drop = FALSE], y[keep], lam)
    oof[fold] <- as.numeric(X[fold, , drop = FALSE] %*% f$beta)
  }

  full <- ridge_fit(X, y, lam)

  list(
    weights = full$beta,
    prediction = full$fitted,
    out_of_fold = oof,
    cv = cv,
    lam = lam,
    n_predictors = m
  )
}

#' loco_predictions
#'
#' A step of the regmlm_native implementation. Called by \code{morie_regmlm}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param preds A vector; indexed elementwise.
#' @param meta Iterated over elementwise, with \code{sapply}.
#' @param weights A vector; indexed elementwise.
#' @param chromosomes Optional; may be \code{NULL}. Coerced to integer by the body, with
#' \code{as.integer}.
#' @return A list with \code{loco}, \code{chromosomes}, \code{note}.
#' @export
loco_predictions <- function(preds, meta, weights, chromosomes = NULL) {
  n <- length(preds[[1]])

  if (is.null(chromosomes)) {
    chroms <- sort(unique(sapply(meta, function(m) m$chromosome)))
  } else {
    chroms <- sort(unique(as.integer(chromosomes)))
  }

  out <- list()
  for (c in chroms) {
    keep_idx <- which(sapply(meta, function(m) m$chromosome != c))
    pred <- rep(0, n)
    for (j in keep_idx) {
      pred <- pred + preds[[j]] * weights[j]
    }
    out[[as.character(c)]] <- pred
  }

  list(
    loco = out,
    chromosomes = chroms,
    note = "the chromosome-c background excludes every block on chromosome c, which is what avoids proximal contamination"
  )
}

#' test_variant
#'
#' A step of the regmlm_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param g A vector; its length is taken.
#' @param y A vector; its length is taken.
#' @param offset Optional; may be \code{NULL}. Coerced to numeric by the body, with
#' \code{as.numeric}.
#' @param covariates A vector; its length is taken. Defaults to \code{list()}.
#' @return A list with \code{beta}, \code{se}, \code{chisq}, \code{p_value}, \code{n}.
#' @export
test_variant <- function(g, y, offset = NULL, covariates = list()) {
  n <- length(y)
  if (n != length(g)) stop("regmlm: genotype and phenotype lengths differ")

  if (is.null(offset)) {
    off <- rep(0.0, n)
  } else {
    off <- as.numeric(offset)
  }
  if (length(off) != n) stop("regmlm: the offset must cover every sample")

  if (length(covariates) == 0) {
    cols <- matrix(1.0, n, 1)
  } else {
    cov_mat <- do.call(cbind, covariates)
    cols <- cbind(matrix(1.0, n, 1), cov_mat)
  }

  resid_y <- .regmlm_residualise(as.numeric(y) - off, cols)
  resid_g <- .regmlm_residualise(as.numeric(g), cols)

  sgg <- sum(resid_g^2)
  if (sgg <= 0) stop("regmlm: the variant is monomorphic after adjustment")

  beta <- sum(resid_g * resid_y) / sgg
  dof <- n - ncol(cols) - 1
  rss <- sum((resid_y - beta * resid_g)^2)
  s2 <- rss / max(dof, 1)
  se <- sqrt(s2 / sgg)
  chisq <- if (se > 0) (beta / se)^2 else Inf

  p_value <- if (is.finite(chisq)) {
    2 * pnorm(sqrt(chisq), lower.tail = FALSE)
  } else {
    0
  }

  list(beta = beta, se = se, chisq = chisq, p_value = p_value, n = n)
}

#' morie_regmlm
#'
#' A step of the regmlm_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param G A matrix; passed to \code{nrow}.
#' @param y A vector; its length is taken.
#' @param chromosomes Passed to \code{make_blocks}.
#' @param block_size Passed to \code{make_blocks}. Defaults to \code{1000}.
#' @param n_ridge Passed to \code{level0_predictors}. Defaults to \code{5}.
#' @param cv Passed to \code{level1_stack}. Defaults to \code{"kfold"}.
#' @param k Passed to \code{level1_stack}. Defaults to \code{5}.
#' @return A list with \code{estimate}, \code{blocks}, \code{n_blocks}, \code{level0},
#' \code{level1}, \code{loco}, \code{chromosomes}, \code{n_predictors}, \code{reduction},
#' \code{method}.
#' @export
morie_regmlm <- function(G, y, chromosomes = NULL, block_size = 1000, n_ridge = 5,
                        cv = "kfold", k = 5) {
  if (!is.matrix(G)) G <- do.call(rbind, G)

  n <- length(y)
  if (n != nrow(G)) stop("regmlm: G and y must have the same number of samples")

  blocks <- make_blocks(ncol(G), chromosomes, block_size)
  lvl0 <- level0_predictors(G, y, blocks, n_ridge)
  lvl1 <- level1_stack(lvl0$predictors, y, cv, k)
  loco <- loco_predictions(lvl0$predictors, lvl0$meta, lvl1$weights)

  list(
    estimate = lvl1$prediction,
    blocks = blocks,
    n_blocks = length(blocks),
    level0 = lvl0,
    level1 = lvl1,
    loco = loco$loco,
    chromosomes = loco$chromosomes,
    n_predictors = lvl0$n_predictors,
    reduction = lvl0$reduction,
    method = "two-level ridge whole-genome regression with LOCO; Mbatchou et al. (2021) Step 1"
  )
}

whole_genome_regression <- morie_regmlm

#' .regmlm_cheatsheet
#'
#' A step of the regmlm_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
#' @examples
#' res <- .regmlm_cheatsheet()
#' res
.regmlm_cheatsheet <- function() {
  "regmlm: Step 1 is two stacked ridges. Level 0 fits J ridges per block of B markers at DIFFERENT shrinkages (MAP under a Gaussian prior), turning 500k markers into 2.5k local polygenic scores at B=1000, J=5. Level 1 combines them under cross-validation -- in-sample would be circular. The combined predictor is split by chromosome, each background EXCLUDING its own chromosome, because testing a variant against a background containing it is proximal contamination. Step 2 tests each variant with that background as an offset."
}
