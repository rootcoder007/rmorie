# Local polynomial regression smoother.
# Source: Fan & Gijbels (1996), Local Polynomial Modelling and Its
# Applications, Chapman & Hall; Hastie, Tibshirani & Friedman (2009),
# The Elements of Statistical Learning, 2nd ed., Sec. 6.1, Eqs.
# 6.7-6.11 (local PDF:
# WD_BLACK/library/pdf/BookAdvanced_elementsofstatisticallearning.pdf).
# Mirrors Python morie.fn.locp exactly (centered design, same
# kernels, normal equations solved by base solve()).

#' .locp_kernel
#'
#' A step of the locp_native implementation. Called by \code{morie_locp}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param name One of \code{"epanechnikov"}, \code{"gaussian"}, \code{"tricube"}.
#' @param t A vector; its length is taken and its elements indexed.
#' @return The value of \code{out}, as built in the body.
#' @export
.locp_kernel <- function(name, t) {
  out <- numeric(length(t))
  if (name == "gaussian") return(exp(-0.5 * t * t))
  inside <- t < 1
  if (name == "tricube") {
    out[inside] <- (1 - t[inside]^3)^3
  } else if (name == "epanechnikov") {
    out[inside] <- 0.75 * (1 - t[inside]^2)
  } else {
    stop("kernel must be tricube, epanechnikov or gaussian")
  }
  out
}

#' Local polynomial regression smoother
#'
#' At each evaluation point x0 solves the kernel-weighted least
#' squares problem (ESL Eq. 6.11) on the centered polynomial design
#' \[1, (x - x0), ..., (x - x0)^d\]; the fitted value is the local
#' intercept and the local slope is the coefficient of (x - x0).
#' Degree-d polynomials are reproduced exactly for any kernel and
#' bandwidth.
#'
#' @param x,y Paired numeric observations.
#' @param x0 Optional numeric vector of evaluation points (default:
#'   sorted unique x).
#' @param degree Polynomial degree d >= 0.
#' @param bandwidth Kernel half-width (default: half the x range).
#' @param kernel "tricube" (default), "epanechnikov" or "gaussian".
#' @return A list with elements \code{fitted}, \code{x0},
#'   \code{slope}, \code{n_effective}, \code{degree},
#'   \code{bandwidth}, \code{kernel}, \code{method}.
#' @references Fan, J. and Gijbels, I. (1996). Local Polynomial
#'   Modelling and Its Applications. Chapman & Hall.  Hastie, T.,
#'   Tibshirani, R. and Friedman, J. (2009). The Elements of
#'   Statistical Learning, 2nd ed. Springer, Sec. 6.1.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_locp(V, V)
morie_locp <- function(x, y, x0 = NULL, degree = 1, bandwidth = NULL,
                       kernel = "tricube") {
  xv <- as.numeric(x)
  yv <- as.numeric(y)
  n <- length(xv)
  if (length(yv) != n || n < 2) stop("x and y must be paired with n >= 2")
  d <- as.integer(degree)
  if (d < 0) stop("degree must be >= 0")
  pts <- if (is.null(x0)) sort(unique(xv)) else as.numeric(x0)
  lam <- if (is.null(bandwidth)) (max(xv) - min(xv)) / 2 else
    as.numeric(bandwidth)
  if (lam <= 0) stop("bandwidth must be positive")
  kern <- tolower(kernel)
  fitted <- slope <- neff <- numeric(length(pts))
  for (ip in seq_along(pts)) {
    p0 <- pts[ip]
    w <- .locp_kernel(kern, abs(xv - p0) / lam)
    sw <- sum(w)
    if (sw <= 0 || sum(w > 0) < d + 1) {
      fitted[ip] <- NaN; slope[ip] <- NaN; neff[ip] <- sw
      next
    }
    z <- xv - p0
    X <- outer(z, 0:d, "^")
    A <- t(X) %*% (w * X)
    b <- t(X) %*% (w * yv)
    beta <- tryCatch(solve(A, b), error = function(e) NULL)
    if (is.null(beta)) {
      fitted[ip] <- NaN; slope[ip] <- NaN
    } else {
      fitted[ip] <- beta[1]
      slope[ip] <- if (d >= 1) beta[2] else NaN
    }
    neff[ip] <- sw
  }
  list(fitted = fitted, x0 = pts, slope = slope, n_effective = neff,
       degree = d, bandwidth = lam, kernel = kern,
       method = "local polynomial WLS (Fan-Gijbels 1996; ESL Eq. 6.11)")
}
