# CV-TMLE for nonpathwise differentiable target parameters.
# Sources: van der Laan, M. J. & Rose, S. (2018) *Targeted Learning
# in Data Science*, Springer, doi:10.1007/978-3-319-65304-4. Chap. 25
# (van der Laan, Bibaut & Luedtke): TMLE was developed for
# efficient substitution estimators of pathwise differentiable
# target parameters, while many parameters -- a density or
# regression curve at a single point in a nonparametric model --
# are nonpathwise differentiable; the usual recourse to a specific
# estimator under specific smoothness assumptions, which does not
# adapt to the true unknown smoothness and can be outperformed by
# an adaptive estimator; and the fully adaptive estimator
# converging at the adaptive optimal rate implied by the unknown
# smoothness while still providing formal inference, using CV-TMLE
# for a data-adaptively selected smooth approximation of the
# nonpathwise differentiable parameter, integrating efficiency
# theory with super learning. Bibaut, A. F. & van der Laan, M. J.
# (2019) "Fast rates for empirical risk minimization over cadlag
# functions with bounded sectional variation norm",
# arXiv:1907.09244. Lepski, O. V. & Spokoiny, V. G. (1997)
# "Optimal pointwise adaptive methods in nonparametric estimation",
# Annals of Statistics 25(6), 2512-2546, doi:10.1214/aos/1030741083.
# Adaptive bandwidth selection.
#
# Native implementation mirroring Python morie.fn.tlcvnp exactly:
# the same three kernels, the same smoothed-parameter stand-in
# with its influence curve, the same Lepski bandwidth selection,
# the same V-fold CV-TMLE, and the same O(h^s) / O(1/sqrt(nh))
# bias/SE trade.

#' morie_tlcvnp
#'
#' Part of the tlcvnp_native implementation; see the file header for the
#' source it follows.
#'
#' @param X See Usage.
#' @param x0 See Usage.
#' @param bandwidths See Usage.
#' @param kernel Defaults to \code{"epanechnikov"}.
#' @param V Defaults to \code{5L}.
#' @param seed Defaults to \code{0L}.
#' @return The value of \code{cv_tmle_smoothed}.
#' @export
morie_tlcvnp <- function(X, x0, bandwidths, kernel = "epanechnikov",
                         V = 5L, seed = 0L) {
  cv_tmle_smoothed(X, x0, bandwidths, kernel = kernel, V = V,
                   seed = seed)
}

.tlcvnp_kernels <- c("epanechnikov", "gaussian", "uniform")

#' kernel_smooth
#'
#' Part of the tlcvnp_native implementation; see the file header for the
#' source it follows.
#'
#' @param u See Usage.
#' @param kernel Defaults to \code{"epanechnikov"}.
#' @return A numeric value.
#' @export
kernel_smooth <- function(u, kernel = "epanechnikov") {
  if (!(kernel %in% .tlcvnp_kernels))
    stop(sprintf("tlcvnp: kernel must be one of %s, got %s",
                 paste(.tlcvnp_kernels, collapse = ", "), kernel))
  v <- as.numeric(u)
  if (kernel == "epanechnikov")
    return(if (abs(v) <= 1) 0.75 * (1 - v * v) else 0)
  if (kernel == "uniform")
    return(if (abs(v) <= 1) 0.5 else 0)
  exp(-0.5 * v * v) / sqrt(2 * pi)
}

#' smoothed_parameter
#'
#' Part of the tlcvnp_native implementation; see the file header for the
#' source it follows.
#'
#' @param X See Usage.
#' @param x0 See Usage.
#' @param h See Usage.
#' @param kernel Defaults to \code{"epanechnikov"}.
#' @return A list with \code{psi_h}, \code{se}, \code{h}, \code{n}, \code{influence_curve}, \code{note}.
#' @export
smoothed_parameter <- function(X, x0, h, kernel = "epanechnikov") {
  v <- as.numeric(X)
  hh <- as.numeric(h)
  if (hh <= 0)
    stop("tlcvnp: the bandwidth must be positive")
  n <- length(v)
  x0n <- as.numeric(x0)
  val <- sum(vapply(v, function(x)
    kernel_smooth((x - x0n) / hh, kernel), numeric(1))) / (n * hh)
  ic <- vapply(v, function(x)
    kernel_smooth((x - x0n) / hh, kernel) / hh - val, numeric(1))
  m <- mean(ic)
  se <- sqrt(sum((ic - m)^2) / (n - 1) / n)
  list(psi_h = val, se = se, h = hh, n = n,
       influence_curve = ic,
       note = "the SMOOTHED parameter is pathwise differentiable; the density at a point is not")
}

#' smoothing_bias
#'
#' Part of the tlcvnp_native implementation; see the file header for the
#' source it follows.
#'
#' @param true_density See Usage.
#' @param x0 See Usage.
#' @param h See Usage.
#' @param smoothness Defaults to \code{2}.
#' @return A list with \code{bias_order}, \code{h}, \code{smoothness}, \code{note}.
#' @export
smoothing_bias <- function(true_density, x0, h, smoothness = 2.0) {
  hh <- as.numeric(h); s <- as.numeric(smoothness)
  if (hh <= 0 || s <= 0)
    stop("tlcvnp: bandwidth and smoothness must be positive")
  list(bias_order = hh^s, h = hh, smoothness = s,
       note = "inference is for the SMOOTHED parameter; it transfers to the target only when the approximation bias is dominated")
}

#' select_bandwidth
#'
#' Part of the tlcvnp_native implementation; see the file header for the
#' source it follows.
#'
#' @param X See Usage.
#' @param x0 See Usage.
#' @param bandwidths See Usage.
#' @param kernel Defaults to \code{"epanechnikov"}.
#' @param criterion Defaults to \code{"lepski"}.
#' @param C Defaults to \code{1}.
#' @return A list with \code{h}, \code{fit}, \code{criterion}, \code{all}, \code{note}.
#' @export
select_bandwidth <- function(X, x0, bandwidths,
                             kernel = "epanechnikov",
                             criterion = "lepski", C = 1.0) {
  hs <- sort(as.numeric(bandwidths))
  if (length(hs) == 0L)
    stop("tlcvnp: no bandwidths given")
  if (!(criterion %in% c("lepski", "smallest_se")))
    stop(sprintf("tlcvnp: criterion must be lepski or smallest_se, got %s",
                 criterion))
  fits <- lapply(hs, function(hh) smoothed_parameter(X, x0, hh, kernel))
  if (criterion == "smallest_se") {
    ses <- vapply(fits, function(f) f$se, numeric(1))
    j <- which.min(ses) - 1L
    return(list(h = hs[j + 1L], fit = fits[[j + 1L]],
                criterion = criterion))
  }
  chosen <- length(hs) - 1L
  for (i in seq_along(hs)) {
    ok <- TRUE
    for (j in (i + 1L):length(hs)) {
      if (abs(fits[[i]]$psi_h - fits[[j]]$psi_h) >
          as.numeric(C) * (fits[[i]]$se + fits[[j]]$se)) {
        ok <- FALSE
        break
      }
    }
    if (ok) { chosen <- i - 1L; break }
  }
  list(h = hs[chosen + 1L], fit = fits[[chosen + 1L]],
       criterion = criterion,
       all = data.frame(h = hs,
                        psi_h = vapply(fits, function(f)
                          f$psi_h, numeric(1)),
                        se = vapply(fits, function(f)
                          f$se, numeric(1))),
       note = "the smallest bandwidth consistent with every larger one")
}

#' cv_tmle_smoothed
#'
#' Part of the tlcvnp_native implementation; see the file header for the
#' source it follows.
#'
#' @param X See Usage.
#' @param x0 See Usage.
#' @param bandwidths See Usage.
#' @param kernel Defaults to \code{"epanechnikov"}.
#' @param V Defaults to \code{5L}.
#' @param seed Defaults to \code{0L}.
#' @return A list with \code{estimate}, \code{psi}, \code{se}, \code{ci}, \code{bandwidths}, \code{fold_estimates}, \code{V}, \code{method}, \code{note}.
#' @export
cv_tmle_smoothed <- function(X, x0, bandwidths, kernel = "epanechnikov",
                             V = 5L, seed = 0L) {
  v <- as.numeric(X)
  n <- length(v)
  e_rng <- .ghc_rng(as.numeric(seed))
  idx <- seq_len(n)
  for (i in n:2) {
    j <- as.integer(.ghc_unif(e_rng, 1L) * (i + 1)) %% (i + 1)
    if (j == 0L) j <- 1L
    if (j == i) j <- i - 1L
    tmp <- idx[i]; idx[i] <- idx[j]; idx[j] <- tmp
  }
  folds <- lapply(seq_len(as.integer(V)), function(f)
    sort(idx[seq(f, length(idx), by = as.integer(V))]))
  ests <- numeric(length(folds))
  ics <- numeric(n)
  hs <- numeric(length(folds))
  for (k in seq_along(folds)) {
    f <- folds[[k]]
    tr <- v[setdiff(seq_len(n), f)]
    sel <- select_bandwidth(tr, x0, bandwidths, kernel)
    hs[k] <- sel$h
    est <- smoothed_parameter(v[f], x0, sel$h, kernel)
    ests[k] <- est$psi_h
    ics[f] <- est$influence_curve
  }
  psi <- mean(ests)
  m <- mean(ics)
  se <- sqrt(sum((ics - m)^2) / (n - 1) / n)
  list(estimate = psi, psi = psi, se = se,
       ci = c(psi - 1.96 * se, psi + 1.96 * se),
       bandwidths = hs, fold_estimates = ests, V = as.integer(V),
       method = "CV-TMLE for a data-adaptively smoothed nonpathwise parameter; van der Laan & Rose (2018) Chap. 25",
       note = "adapts to the unknown smoothness instead of assuming it, and still supplies formal inference")
}

#' .tlcvnp_cheatsheet
#'
#' Part of the tlcvnp_native implementation; see the file header for the
#' source it follows.
#'
#' @return A character value.
#' @export
.tlcvnp_cheatsheet <- function() {
  paste("tlcvnp: a density or regression curve AT A POINT is ",
        "NONpathwise differentiable -- no efficient influence ",
        "curve, no root-n estimator. The usual fix picks a ",
        "bandwidth under an assumed smoothness and is beaten by ",
        "anything adaptive. Instead approximate the target by a ",
        "SMOOTHED parameter that IS pathwise differentiable, ",
        "estimate it by CV-TMLE, and choose the bandwidth from the ",
        "data (Lepski). The bias is O(h^s) and the standard error ",
        "O(1/sqrt(nh)): inference is for the smoothed parameter ",
        "and transfers only when the bias is dominated.", sep = "")
}
