# morie.fn -- function file (rootcoder007/morie)
# FarmCPU: fixed and random models, circulating.
#
# References
# Liu, X., Huang, M., Fan, B., Buckler, E. S. & Zhang, Z. (2016)
# "Iterative Usage of Fixed and Random Effect Models for Powerful and
# Efficient Genome-Wide Association Studies", PLoS Genetics 12(2),
# e1005767, doi:10.1371/journal.pgen.1005767.
# Yu, J. et al. (2006) "A unified mixed-model method for association
# mapping that accounts for multiple levels of relatedness", Nature
# Genetics 38(2), 203-208.
# Segura, V. et al. (2012) "An efficient multi-locus mixed-model
# approach for genome-wide association studies in structured
# populations", Nature Genetics 44(7), 825-830.


# Base R has no erf/erfc; both are pnorm in disguise. Defined here so
# the arm stays base-R only, as the package requires.
#' Base R has no erf/erfc; both are pnorm in disguise. Defined here so
#'
#' the arm stays base-R only, as the package requires.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.farmlmm_erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
#' .farmlmm_erfc
#'
#' A step of the farmlmm_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.farmlmm_erfc <- function(x) 2 * pnorm(-x * sqrt(2))

.farmlmm_EPS <- 1e-12

# mirror _s03core.mat
#' Mirror _s03core.mat
#'
#' A step of the farmlmm_native implementation. Called by \code{.confounding}, \code{.farmcpu}, \code{.farmlmm_wls} and 2 others in the module.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param X A matrix; passed to \code{nrow}.
#' @return The value of \code{X}, as built in the body.
#' @export
.farmlmm_to_mat <- function(X) {
  if (is.data.frame(X)) X <- as.matrix(X)
  if (is.null(dim(X))) X <- matrix(as.numeric(X), nrow = length(X))
  X <- matrix(as.numeric(X), nrow = nrow(X))
  X
}

# mirror _s03core.vec
#' Mirror _s03core.vec
#'
#' A step of the farmlmm_native implementation. Called by \code{.farmcpu}, \code{.farmlmm_wls}, \code{.fixed_effect_scan} and 1 others in the module.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param y A matrix; passed to \code{dim}.
#' @return A vector, from \code{as.numeric}.
#' @export
.to_vec <- function(y) {
  if (is.data.frame(y)) y <- as.matrix(y)
  if (!is.null(dim(y))) y <- as.numeric(y)
  as.numeric(y)
}

#' .norm_cdf
#'
#' A step of the farmlmm_native implementation. Called by \code{.fixed_effect_scan}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.norm_cdf <- function(x) 0.5 * (1 + .farmlmm_erf(x / sqrt(2)))

# mirror _s03core.wls
#' Mirror _s03core.wls
#'
#' A step of the farmlmm_native implementation. Called by \code{.fixed_effect_scan}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param X A matrix; passed to \code{nrow}.
#' @param y A vector; its length is taken.
#' @param w Numeric; passed to \code{sqrt}.
#' @param rcond Passed to \code{qr.solve}.
#' @return A list with \code{coef}.
#' @export
.farmlmm_wls <- function(X, y, w, rcond) {
  X <- .farmlmm_to_mat(X)
  y <- .to_vec(y)
  w <- as.numeric(w)
  if (nrow(X) != length(y)) stop("wls: length mismatch")
  sw <- sqrt(w)
  Xw <- X * sw
  yw <- y * sw
  xtx <- crossprod(Xw, Xw)
  xty <- crossprod(Xw, yw)
  co <- qr.solve(xtx, xty, tol = rcond)
  list(coef = as.numeric(co))
}

#' .kinship_from_markers
#'
#' A step of the farmlmm_native implementation. Called by \code{.random_effect_step}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param G Passed to \code{.farmlmm_to_mat}.
#' @param markers Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{K}, \code{markers_used}, \code{n_markers}, \code{all_markers}.
#' @export
.kinship_from_markers <- function(G, markers = NULL) {
  M <- .farmlmm_to_mat(G)
  n <- nrow(M)
  p <- ncol(M)
  cols <- if (is.null(markers)) seq_len(p) else as.integer(markers) + 1L
  if (length(cols) == 0L) stop("farmlmm: kinship needs at least one marker")
  Z <- matrix(0, nrow = length(cols), ncol = n)
  for (ii in seq_along(cols)) {
    j <- cols[ii]
    col <- M[, j]
    m <- sum(col) / n
    s <- sqrt(sum((col - m) ^ 2) / n)
    if (s == 0) s <- 1
    Z[ii, ] <- (col - m) / s
  }
  K <- (crossprod(Z, Z) / length(cols))
  list(K = K, markers_used = as.integer(cols) - 1L,
       n_markers = length(cols), all_markers = is.null(markers))
}

#' .confounding
#'
#' A step of the farmlmm_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param G Passed to \code{.farmlmm_to_mat}.
#' @param K A matrix; passed to \code{\%*\%}.
#' @param marker Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{correlation}, \code{marker}, \code{note}.
#' @export
.confounding <- function(G, K, marker) {
  M <- .farmlmm_to_mat(G)
  n <- nrow(M)
  g <- M[, as.integer(marker) + 1L]
  kg <- as.numeric(K %*% g) / n
  mg <- mean(g)
  mk <- mean(kg)
  num <- sum((g - mg) * (kg - mk))
  den <- sqrt(sum((g - mg) ^ 2) * sum((kg - mk) ^ 2))
  list(correlation = if (den > .farmlmm_EPS) num / den else 0,
       marker = as.integer(marker),
       note = "kinship from ALL markers contains the tested marker; that is the confounding")
}

#' .fixed_effect_scan
#'
#' A step of the farmlmm_native implementation. Called by \code{.farmcpu}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param y Passed to \code{.to_vec}.
#' @param G Passed to \code{.farmlmm_to_mat}.
#' @param covariates Coerced to integer by the body, with \code{as.integer}. Defaults to \code{integer(0)}.
#' @param K Accepted by the signature and not used anywhere in the body.
#' @return A list with \code{p}, \code{beta}, \code{covariates}, \code{note}.
#' @export
.fixed_effect_scan <- function(y, G, covariates = integer(0), K = NULL) {
  yv <- .to_vec(y)
  M <- .farmlmm_to_mat(G)
  n <- nrow(M)
  p <- ncol(M)
  if (length(yv) != n) {
    stop(sprintf("farmlmm: %d phenotypes but %d genotypes", length(yv), n))
  }
  cov <- as.integer(covariates) + 1L
  pv <- numeric(p)
  betas <- numeric(p)
  for (j in seq_len(p)) {
    cols <- c(j, setdiff(cov, j))
    X <- M[, cols, drop = FALSE]
    co <- tryCatch(.farmlmm_wls(X, yv, rep(1, n), 1e-8)$coef, error = function(e) NULL)
    if (is.null(co)) {
      pv[j] <- 1
      betas[j] <- 0
      next
    }
    fit <- co[1L] + as.numeric(X %*% co[-1L])
    res <- yv - fit
    dof <- max(n - length(cols) - 1L, 1L)
    s2 <- sum(res * res) / dof
    xj <- X[, 1]
    xm <- mean(xj)
    sxx <- sum((xj - xm) ^ 2)
    se <- if (sxx > .farmlmm_EPS) sqrt(s2 / sxx) else Inf
    t <- if (se > 0) co[2L] / se else 0
    pv[j] <- 2 * (1 - .norm_cdf(abs(t)))
    betas[j] <- co[2L]
  }
  list(p = pv, beta = betas, covariates = as.integer(covariates),
       note = "associated markers enter as COVARIATES, which is what controls false positives")
}

#' .random_effect_step
#'
#' A step of the farmlmm_native implementation. Called by \code{.farmcpu}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param y Passed to \code{.to_vec}.
#' @param G Passed to \code{.kinship_from_markers}.
#' @param selected Coerced to integer by the body, with \code{as.integer}.
#' @param bins Accepted by the signature and not used anywhere in the body.
#' @return A list with \code{K}, \code{markers_used}, \code{blup}, \code{note}.
#' @export
.random_effect_step <- function(y, G, selected, bins = NULL) {
  sel <- as.integer(selected) + 1L
  if (length(sel) == 0L) {
    return(list(K = NULL, markers_used = integer(0),
                note = "no associated markers yet; kinship is the identity at the first iteration"))
  }
  kk <- .kinship_from_markers(G, sel - 1L)
  yv <- .to_vec(y)
  n <- length(yv)
  Kk <- kk$K
  m <- mean(yv)
  blup <- as.numeric(Kk %*% (yv - m)) / n
  list(K = Kk, markers_used = sel - 1L, blup = blup,
       note = "kinship from a SMALL selected set, so it no longer contains the marker under test")
}

#' .farmcpu
#'
#' A step of the farmlmm_native implementation. Called by \code{morie_farmlmm}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param y Passed to \code{.to_vec}.
#' @param G Passed to \code{.farmlmm_to_mat}.
#' @param max_iter Coerced to integer by the body, with \code{as.integer}. Defaults to \code{10L}.
#' @param threshold Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param seed Accepted by the signature and not used anywhere in the body. Defaults to \code{0L}.
#' @return A list with \code{estimate}, \code{selected}, \code{p}, \code{iterations}, \code{converged}, \code{oscillating}, \code{threshold}, \code{history}, \code{method}, \code{note}.
#' @export
.farmcpu <- function(y, G, max_iter = 10L, threshold = NULL, seed = 0L) {
  yv <- .to_vec(y)
  M <- .farmlmm_to_mat(G)
  p <- ncol(M)
  thr <- if (is.null(threshold)) 0.05 / p else as.numeric(threshold)
  sel <- integer(0)
  hist <- list()
  converged <- FALSE
  fem <- NULL
  for (it in seq_len(as.integer(max_iter))) {
    fem <- .fixed_effect_scan(yv, M, sel - 1L)
    new <- sort(which(fem$p < thr) - 1L)
    hist[[length(hist) + 1L]] <- new
    if (identical(new, sel)) {
      converged <- TRUE
      break
    }
    if (length(hist) >= 3L && identical(new, hist[[length(hist) - 2L]])) {
      break
    }
    sel <- new
    .random_effect_step(yv, M, sel)
  }
  list(
    estimate = sel, selected = sel, p = fem$p,
    iterations = length(hist), converged = converged,
    oscillating = (!converged && length(hist) >= 3L &&
                     identical(hist[[length(hist)]], hist[[length(hist) - 2L]])),
    threshold = thr, history = hist,
    method = "FarmCPU; Liu, Huang, Fan, Buckler & Zhang (2016)",
    note = "kinship rebuilt from the SELECTED markers each round, so the confounding is removed rather than reduced"
  )
}

#' morie_farmlmm
#'
#' A step of the farmlmm_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param y Passed to \code{.farmcpu}.
#' @param G Passed to \code{.farmcpu}.
#' @param max_iter Passed to \code{.farmcpu}. Defaults to \code{10L}.
#' @param threshold Passed to \code{.farmcpu}.
#' @param seed Passed to \code{.farmcpu}. Defaults to \code{0L}.
#' @return The value of \code{.farmcpu}.
#' @export
morie_farmlmm <- function(y, G, max_iter = 10L, threshold = NULL, seed = 0L) {
  .farmcpu(y = y, G = G, max_iter = max_iter, threshold = threshold, seed = seed)
}

#' erf
#'
#' A step of the farmlmm_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x Passed to \code{pnorm}.
#' @return A numeric value.
#' @export
erf <- function(x) 2 * pnorm(x) - 1
