# SPDX-License-Identifier: AGPL-3.0-or-later

#' Cross-validated bandwidth for a single-index model
#'
#' Horowitz (2009), Semiparametric and Nonparametric Methods in
#' Econometrics, Section 2.7 (pages 44-45) and Appendix A.2.1
#' (page 242).  Section 2.7 reports Haerdle, Hall and Ichimura (1993):
#' optimise the semiparametric WNLS objective (2.25) over betatilde AND
#' the bandwidth jointly, which estimates the bandwidth minimising the
#' asymptotic integrated mean-square error of a kernel estimator of G
#' -- explicitly NOT optimal for estimating beta.  Section 2.7 also
#' records that the asymptotic distribution of n^(1/2)(b_n - beta) does
#' not depend on h at all, so bandwidth choice for beta needs
#' higher-order theory (Haerdle and Tsybakov 1993; Powell and Stoker
#' 1996) with h_opt = h_0 n^(-2/(2P+d+2)).
#'
#' The criterion evaluated here is the leave-one-out cross-validation
#' function of Appendix A.2.1 (page 242) carried to the index,
#' TR(h) = n^-1 sum_i w(X_i) \[Y_i - Ghat_\{-i,h\}(X_i'beta)\]^2,
#' minimised over an EXPLICIT FIXED grid.  Leave-one-out is
#' deterministic: there is no random fold split.
#'
#' @param x Numeric matrix of covariates, n by d.
#' @param y Numeric outcome vector.
#' @param beta Numeric index coefficients (scale normalised).
#' @param grid Optional explicit numeric bandwidth grid.
#' @param nh Integer size of the default grid.
#' @param lo,hi Numeric multiplier range of the default grid.
#' @param weights Optional numeric w(X_i); default ones.
#' @param P Integer kernel order, used only to report the Section 2.7
#'   form h_opt = h_0 n^(-2/(2P+d+2)).
#' @param d Optional integer dimension for that same formula.
#' @return Named list with bandwidth, cv, grid, cvcurve, hreference,
#'   hstokerform, n, method.
#' @keywords internal
#' @examples
#' n <- 200
#' x <- cbind(seq(-2, 2, length.out = n), cos(seq_len(n) * 0.7))
#' b <- c(1, 0.5)
#' Simbwcv(x, sin(as.numeric(x %*% b)), b)$bandwidth
#' @export
Simbwcv <- function(x, y, beta, grid = NULL, nh = 15L, lo = 0.25, hi = 4,
                    weights = NULL, P = 2L, d = NULL) {
  X <- if (is.null(dim(x))) matrix(x, ncol = 1L) else as.matrix(x)
  yv <- as.numeric(y)
  b <- as.numeric(beta)
  if (ncol(X) != length(b) && nrow(X) == length(b)) X <- t(X)
  n <- nrow(X)
  dd <- ncol(X)
  if (length(yv) != n) {
    stop("y must have one entry per row of x.", call. = FALSE)
  }
  W <- if (is.null(weights)) rep(1, n) else as.numeric(weights)
  href <- n^(-0.2)
  hs <- if (!is.null(grid)) as.numeric(grid) else {
    href * exp(seq(log(lo), log(hi), length.out = as.integer(nh)))
  }
  z <- as.numeric(X %*% b)

  cv <- numeric(length(hs))
  for (tt in seq_along(hs)) {
    hh <- hs[tt]
    K <- .hrz2_gk(outer(z, z, "-") / hh)
    diag(K) <- 0
    den <- rowSums(K)
    den <- ifelse(den > 1e-300, den, 1e-300)
    gh <- as.numeric(K %*% yv) / den
    r <- yv - gh
    cv[tt] <- sum(W * r * r) / n
  }
  k <- which.min(cv)
  dim_ <- if (is.null(d)) as.integer(dd) else as.integer(d)
  list(bandwidth = hs[k], cv = cv[k], grid = hs, cvcurve = cv,
       hreference = href,
       hstokerform = n^(-2 / (2 * as.integer(P) + dim_ + 2)), n = n,
       method = "Horowitz (2009) Section 2.7 and Appendix A.2.1 TR(h)")
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Simbwcv
#' @keywords internal
#' @export
morie_horowitz_bw_cv_sim <- Simbwcv
