# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Bootstrap percentile CI for the indirect effect (Bsmed). Bit-identical
# mirror of src/morie/fn/bsmed.py. Resampling reproduces the Python arm
# exactly through R's own set.seed + sample.int stream.

#' Bootstrap percentile CI for the indirect effect in simple mediation
#'
#' In the model M = i1 + a X and Y = i2 + cp X + b M, draws B resamples
#' of the n cases with replacement, recomputes a times b in each, sorts
#' the B estimates and takes the (B alpha/2)-th and
#' (B (1 - alpha/2) + 1)-th order statistics as interval limits: for
#' B = 1000 and alpha = .05 these are the 25th and 976th sorted values,
#' the worked rule of Preacher and Hayes (2004, p. 722). The bootstrap
#' point estimate is the mean of the resampled products; the
#' sample-data product is reported alongside.
#'
#' @param x Independent variable.
#' @param m Mediator.
#' @param y Outcome.
#' @param B Number of bootstrap resamples.
#' @param alpha Two-sided miss probability.
#' @param seed Seed for set.seed.
#' @return List with \code{estimate}, \code{boot_estimate}, \code{se},
#'   \code{ci_lower}, \code{ci_upper}, \code{a}, \code{b},
#'   \code{c_prime}, \code{B}, \code{n}, \code{conf_level},
#'   \code{method}.
#' @references Preacher, K. J. and Hayes, A. F. (2004), SPSS and SAS
#'   procedures for estimating indirect effects in simple mediation
#'   models, Behavior Research Methods, Instruments, and Computers
#'   36(4), 717-731, doi:10.3758/BF03206553, procedure p. 722; local
#'   copy fetched-wave3/preacher-hayes-2004-spss-sas-indirect-effects-BRM36.pdf.
#'   Shrout, P. E. and Bolger, N. (2002), Mediation in experimental and
#'   nonexperimental studies: New procedures and recommendations,
#'   Psychological Methods 7(4), 422-445, doi:10.1037/1082-989X.7.4.422.
#' @export
Bsmed <- function(x, m, y, B = 1000L, alpha = 0.05, seed = 1L) {
  x <- as.numeric(x); m <- as.numeric(m); y <- as.numeric(y)
  n <- length(x)
  if (length(m) != n || length(y) != n) {
    stop("x, m, y must have equal length", call. = FALSE)
  }
  B <- as.integer(B)
  if (B < 2L) stop("B must be at least 2", call. = FALSE)
  ab_paths <- function(xv, mv, yv) {
    Xa <- cbind(1, xv)
    ca <- as.vector(solve(crossprod(Xa), crossprod(Xa, mv)))
    Xb <- cbind(1, xv, mv)
    cb <- as.vector(solve(crossprod(Xb), crossprod(Xb, yv)))
    c(a = ca[2L], b = cb[3L], c_prime = cb[2L])
  }
  p0 <- ab_paths(x, m, y)
  set.seed(seed)
  boots <- numeric(B)
  for (r in seq_len(B)) {
    idx <- sample.int(n, n, replace = TRUE)
    pr <- ab_paths(x[idx], m[idx], y[idx])
    boots[r] <- pr[["a"]] * pr[["b"]]
  }
  s <- sort(boots)
  lo_i <- as.integer(B * (alpha / 2))
  hi_i <- as.integer(B * (1 - alpha / 2)) + 1L
  lo_i <- min(max(lo_i, 1L), B)
  hi_i <- min(max(hi_i, 1L), B)
  list(estimate = unname(p0[["a"]] * p0[["b"]]),
       boot_estimate = mean(boots),
       se = stats::sd(boots),
       ci_lower = s[lo_i], ci_upper = s[hi_i],
       a = unname(p0[["a"]]), b = unname(p0[["b"]]),
       c_prime = unname(p0[["c_prime"]]),
       B = B, n = n, conf_level = 1 - alpha,
       method = "Preacher-Hayes (2004) bootstrap percentile CI for a*b")
}
