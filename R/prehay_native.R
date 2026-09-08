# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Preacher-Hayes multiple-mediator bootstrap (Prehay). Bit-identical
# mirror of src/morie/fn/prehay.py; resampling reproduces the Python
# arm exactly through set.seed + sample.int.

#' Specific and total indirect effects with percentile bootstrap CIs
#'
#' In the multiple mediation model with mediators M_k = i_k + a_k X and
#' outcome Y = i + cp X + sum b_k M_k, the specific indirect effect
#' through mediator k is \eqn{a_k b_k} and the total indirect effect is
#' \eqn{\sum_k a_k b_k}, which equals c - c-prime (Preacher and Hayes
#' 2008, pp. 880-881). Each a_k comes from the regression of M_k on X;
#' the b_k and the direct effect come from one regression of Y on X and
#' all mediators. Percentile intervals use the (B alpha/2)-th and
#' (B (1 - alpha/2) + 1)-th order statistics of the B resampled
#' estimates.
#'
#' @param x Independent variable, length n.
#' @param M Mediator matrix, n rows, one column per mediator.
#' @param y Outcome, length n.
#' @param B Number of bootstrap resamples.
#' @param alpha Two-sided miss probability.
#' @param seed Seed for set.seed.
#' @return List with \code{estimate} (total indirect),
#'   \code{specific}, \code{a}, \code{b}, \code{c_prime},
#'   \code{ci_lower}, \code{ci_upper}, \code{specific_lower},
#'   \code{specific_upper}, \code{se}, \code{B}, \code{n},
#'   \code{conf_level}, \code{method}.
#' @references Preacher, K. J. and Hayes, A. F. (2008), Asymptotic and
#'   resampling strategies for assessing and comparing indirect effects
#'   in multiple mediator models, Behavior Research Methods 40(3),
#'   879-891, doi:10.3758/BRM.40.3.879, pp. 880-884; local copy
#'   fetched-wave3/preacher-hayes-2008-asymptotic-resampling-multiple-mediators-BRM40.pdf.
#'   Rank rule: Preacher and Hayes (2004), Behavior Research Methods,
#'   Instruments, and Computers 36(4), 717-731, p. 722.
#' @export
#' @examples
#' Prehay(x = c(2.5, 1.0, 3.5, 4.0, 2.0, 5.5, 3.0, 6.5), M = c(1, 2, 3, 4, 5, 6, 7, 8), y
#' = c(1, 2, 3, 4, 5, 6, 7, 8))
Prehay <- function(x, M, y, B = 1000L, alpha = 0.05, seed = 1L) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  M <- as.matrix(M)
  storage.mode(M) <- "double"
  n <- length(x)
  if (nrow(M) != n || length(y) != n) {
    stop("x, M, y must have matching first dimension", call. = FALSE)
  }
  j <- ncol(M)
  B <- as.integer(B)
  if (B < 2L) stop("B must be at least 2", call. = FALSE)
  paths <- function(xv, Mv, yv) {
    Xa <- cbind(1, xv)
    AtA <- crossprod(Xa)
    a <- vapply(seq_len(j), function(k) {
      as.vector(solve(AtA, crossprod(Xa, Mv[, k])))[2L]
    }, numeric(1))
    Xb <- cbind(Xa, Mv)
    cb <- as.vector(solve(crossprod(Xb), crossprod(Xb, yv)))
    list(a = a, b = cb[2L + seq_len(j)], c_prime = cb[2L])
  }
  p0 <- paths(x, M, y)
  spec <- p0$a * p0$b
  total <- sum(spec)
  set.seed(seed)
  boot_spec <- matrix(0, B, j)
  boot_tot <- numeric(B)
  for (r in seq_len(B)) {
    idx <- sample.int(n, n, replace = TRUE)
    pr <- paths(x[idx], M[idx, , drop = FALSE], y[idx])
    v <- pr$a * pr$b
    boot_spec[r, ] <- v
    boot_tot[r] <- sum(v)
  }
  lo_i <- as.integer(B * (alpha / 2))
  hi_i <- as.integer(B * (1 - alpha / 2)) + 1L
  lo_i <- min(max(lo_i, 1L), B)
  hi_i <- min(max(hi_i, 1L), B)
  st <- sort(boot_tot)
  sl <- numeric(j)
  su <- numeric(j)
  for (k in seq_len(j)) {
    sk <- sort(boot_spec[, k])
    sl[k] <- sk[lo_i]
    su[k] <- sk[hi_i]
  }
  list(estimate = total, specific = spec,
       a = p0$a, b = unname(p0$b), c_prime = unname(p0$c_prime),
       ci_lower = st[lo_i], ci_upper = st[hi_i],
       specific_lower = sl, specific_upper = su,
       se = stats::sd(boot_tot),
       B = B, n = n, conf_level = 1 - alpha,
       method = "Preacher-Hayes (2008) multiple-mediator percentile bootstrap")
}
