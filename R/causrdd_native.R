# Sharp regression discontinuity by one-sided local linear regression.
# Sources: Hahn, J., Todd, P. and van der Klaauw, W. (2001),
# Identification and estimation of treatment effects with a
# regression-discontinuity design, Econometrica 69(1), 201-209 (local
# linear estimation at the boundary); Imbens, G. and Lemieux, T.
# (2008), Regression discontinuity designs: a guide to practice,
# Journal of Econometrics 142(2), 615-635, Sec. 4 (the two one-sided
# fits and the triangular/edge kernel); Fan, J. and Gijbels, I.
# (1996), Local Polynomial Modelling and Its Applications, Ch. 3
# (boundary behaviour of local linear vs local constant fits).
#
# Native implementation mirroring Python morie.fn.causrdd exactly:
# same kernel definitions, same positive-weight masks, and the same
# per-side HC0 sandwich variance treated as independent across sides
# (the rdrobust vce = "hc0" convention).

#' .mor_rdd_kernel
#'
#' A step of the causrdd_native implementation. Called by \code{morie_causrdd}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param name One of \code{"triangular"}, \code{"uniform"}.
#' @param u Numeric; passed to \code{abs}.
#' @return Nothing; this branch always raises.
#' @export
.mor_rdd_kernel <- function(name, u) {
  if (name == "triangular") return(pmax(1 - abs(u), 0))
  if (name == "uniform") return(ifelse(abs(u) <= 1, 0.5, 0))
  stop("kernel must be 'triangular' or 'uniform'")
}

# weighted linear fit on one side, with the HC0 sandwich variance of
# the intercept (the boundary value that the RDD contrast uses)
#' Weighted linear fit on one side, with the HC0 sandwich variance of
#'
#' the intercept (the boundary value that the RDD contrast uses)
#'
#' @param dm Passed to \code{cbind}.
#' @param ym A matrix; passed to \code{\%*\%}.
#' @param w A count; the body uses it as \code{rep(...)}.
#' @return A list with \code{a}, \code{b}, \code{v}.
#' @export
.mor_rdd_side <- function(dm, ym, w) {
  X <- cbind(1, dm)
  XtW <- t(X) * rep(w, each = 2L)
  A <- XtW %*% X
  b <- solve(A, XtW %*% ym)
  e <- as.numeric(ym - X %*% b)
  meat <- (XtW * rep(e^2, each = 2L)) %*% (X * w)
  Ainv <- solve(A)
  V <- Ainv %*% meat %*% Ainv
  list(a = b[1], b = b[2], v = V[1, 1])
}

#' Sharp regression discontinuity, local linear
#'
#' Estimates \eqn{\tau = \alpha_+ - \alpha_-}, the difference of the
#' boundary intercepts of two kernel-weighted linear fits, one on each
#' side of the cutoff (Hahn, Todd and van der Klaauw 2001; Imbens and
#' Lemieux 2008, Sec. 4).  Local LINEAR rather than local constant
#' fitting is what removes the first-order boundary bias (Fan and
#' Gijbels 1996, Ch. 3).
#'
#' @param x Running variable.
#' @param y Outcome.
#' @param cutoff Threshold, default 0.
#' @param h Bandwidth; \code{NULL} (default) uses
#'   \code{\link{morie_causrddh}}, the Imbens-Kalyanaraman plug-in.
#' @param kernel \code{"triangular"} (default, the edge kernel that
#'   the IK constant is derived for) or \code{"uniform"}; both routes
#'   the sources give are available.
#' @return A list with \code{estimate} (\eqn{\tau}), \code{se},
#'   \code{ci}, \code{intercept_left}, \code{intercept_right},
#'   \code{slope_left}, \code{slope_right}, \code{h}, \code{kernel},
#'   \code{n_left}, \code{n_right}, \code{n_used}, \code{se_note},
#'   \code{method}.
#' @references Hahn, J., Todd, P. and van der Klaauw, W. (2001).
#'   Identification and estimation of treatment effects with a
#'   regression-discontinuity design. Econometrica, 69(1), 201-209.
#' @export
morie_causrdd <- function(x, y, cutoff = 0, h = NULL,
                          kernel = "triangular") {
  xa <- as.numeric(x); ya <- as.numeric(y)
  cc <- as.numeric(cutoff)
  if (is.null(h)) h <- morie_causrddh(xa, ya, cutoff = cc)$estimate
  h <- as.numeric(h)
  if (h <= 0) stop("bandwidth must be positive")
  d <- xa - cc
  w <- .mor_rdd_kernel(kernel, d / h)
  lm_ <- (d < 0) & (w > 0)
  rm_ <- (d >= 0) & (w > 0)
  n_l <- sum(lm_); n_r <- sum(rm_)
  if (n_l < 3L || n_r < 3L)
    stop("fewer than 3 observations with positive kernel weight on one side")
  L <- .mor_rdd_side(d[lm_], ya[lm_], w[lm_])
  R <- .mor_rdd_side(d[rm_], ya[rm_], w[rm_])
  tau <- R$a - L$a
  se <- sqrt(L$v + R$v)
  z <- 1.959963984540054
  list(estimate = tau, se = se, ci = c(tau - z * se, tau + z * se),
       intercept_left = L$a, intercept_right = R$a,
       slope_left = L$b, slope_right = R$b,
       h = h, kernel = kernel,
       n_left = n_l, n_right = n_r, n_used = n_l + n_r,
       se_note = paste("HC0 sandwich per side, sides independent;",
                       "rdrobust vce='hc0' convention"),
       method = "sharp RDD, one-sided local linear fits at the cutoff")
}
