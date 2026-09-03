# SPDX-License-Identifier: AGPL-3.0-or-later

#' Covariance of the gamma-kernel functions at bandwidths h and 4h (Theorem 1.4)
#'
#' Theorem 1.4. With `R` the Stirling ratio (1.12) and `s = sqrt(h)`, the
#' interior form (`x/h -> Inf`) is
#' \deqn{\mathrm{Cov}\[A_h, A_{4h}\] =
#' \frac{R(s^{-1}-1)R((2s)^{-1}-1)(\frac32-2s)^{\frac{3}{2s}-\frac32}}{2\sqrt{\pi}R(\frac{3}{2s}-2)(3x+5s)(2-2s)^{\frac1s-\frac12}(1-2s)^{\frac{1}{2s}-\frac12}}\Big(\frac{x+s}{3x+5s}\Big)^{\frac{1}{2s}-1}\Big(\frac{2x+4s}{3x+5s}\Big)^{\frac1s-1}\frac{f_X(x)}{nh^{1/4}},}{Cov\[A_h,
#' A_4h\] = \[R(1/s - 1) R(1/(2s) - 1) (3/2 - 2s)^(3/(2s) - 3/2)\] / \[2 sqrt(pi)
#' R(3/(2s) - 2) (3x + 5s) (2-2s)^(1/s - 1/2) (1-2s)^(1/(2s) - 1/2)\] *
#' ((x+s)/(3x+5s))^(1/(2s) - 1) * ((2x+4s)/(3x+5s))^(1/s - 1) * f(x)/(n h^(1/4)),}
#' and the boundary form (`x/h -> c`) replaces `3x+5s` by `3cs+5`, `x+s` by
#' `cs+1`, `2x+4s` by `2cs+4`, and `h^(1/4)` by `h^(3/4)`.
#'
#' The exponents are unreadable in the book's PDF text layer, which drops
#' stacked fractions. They are taken instead from the primary source, where
#' they are legible: Fauzi, R. R. (2020), "Bias Reduction of Kernel-Type
#' Estimators without Boundary Problems", doctoral thesis, Kyushu University
#' (Kyushu University Institutional Repository, `math0257`), Theorem 2.1.7 --
#' the same result the book reproduces as Theorem 1.4.
#'
#' Everything is evaluated through logarithms: `1/s` is of order 10 for
#' `h = 0.01` and the individual factors overflow long before their product
#' does. The bases `2-2s` and `1-2s` must be positive, so the formula needs
#' `h < 1/4` -- the regime it is an asymptotic statement about anyway.
#'
#' Passing `sample` or `density` instead of `f` returns the EXACT finite-sample
#' covariance `(1/n)(E\[K(X;x,h)K(X;x,4h)\] - J_h J_4h)` rather than its
#' asymptotic evaluation, which is useful for checking how far into the
#' asymptotics a given `h` actually is.
#'
#' @param x Evaluation point, `x >= 0`.
#' @param h Bandwidth of the first estimator; the second uses `4h`.
#' @param n Sample size.
#' @param f `f_X(x)`; selects the Theorem 1.4 closed form.
#' @param boundary Logical; use the boundary branch. Requires `c`.
#' @param c The constant in `x/h -> c`.
#' @param density `f_X` on `\[0, upper\]`; selects the exact plug-in form.
#' @param sample Observed data; selects the exact plug-in form.
#' @param upper Upper limit of the quadrature grid.
#' @param ngrid Number of grid points; fixed, never adapted.
#' @return Named list with ``covariance``, ``form``, ``cross``, ``jh``, ``j4h``, ``h``,
#' ``n``, ``method``.
#' @references Fauzi and Maesono (2023), Theorem 1.4; Fauzi (2020), Kyushu University
#' doctoral thesis, Theorem 2.1.7.
#' @examples
#' Gkcov(x = 1, h = 0.01, n = 100, f = 0.3)
#' @export
Gkcov <- function(x, h, n, f = NULL, boundary = FALSE, c = NULL, density = NULL, sample = NULL, upper = 20, ngrid = 2001L) {
  if (h <= 0) stop("bandwidth must be positive.")
  if (n < 1) stop("sample size must be at least 1.")
  if (x < 0) stop("gamma kernels need x >= 0.")
  given <- c(!is.null(f), !is.null(density), !is.null(sample))
  if (sum(given) != 1L) stop("supply exactly one of f, density or sample.")

  if (!is.null(f)) {
    s <- sqrt(h)
    if (h >= 0.25) stop("Theorem 1.4 needs 1 - 2 sqrt(h) > 0, i.e. h < 1/4.")
    if (isTRUE(boundary)) {
      if (is.null(c)) stop("the boundary branch of Theorem 1.4 needs c.")
      den <- 3 * c * s + 5
      num1 <- c * s + 1
      num2 <- 2 * c * s + 4
      pw <- h^0.75
      form <- "boundary"
    } else {
      den <- 3 * x + 5 * s
      num1 <- x + s
      num2 <- 2 * x + 4 * s
      pw <- h^0.25
      form <- "interior"
    }
    if (den <= 0 || num1 <= 0 || num2 <= 0) {
      stop("Theorem 1.4 needs positive bases; check x, c and h.")
    }
    r1 <- .morie_fauzi_rratio(1 / s - 1)
    r2 <- .morie_fauzi_rratio(1 / (2 * s) - 1)
    r3 <- .morie_fauzi_rratio(3 / (2 * s) - 2)
    log_v <- log(r1) + log(r2) - log(r3) +
      (3 / (2 * s) - 1.5) * log(1.5 - 2 * s) -
      0.5 * log(pi) - log(2) - log(den) -
      (1 / s - 0.5) * log(2 - 2 * s) -
      (1 / (2 * s) - 0.5) * log(1 - 2 * s) +
      (1 / (2 * s) - 1) * log(num1 / den) +
      (1 / s - 1) * log(num2 / den)
    return(list(covariance = exp(log_v) * f / (n * pw), form = form,
                cross = NA_real_, jh = NA_real_, j4h = NA_real_, h = h, n = n,
                method = "Cov[A_h, A_4h], Theorem 1.4 closed form"))
  }

  s1 <- 1 / sqrt(h)
  b1 <- x * sqrt(h) + h
  s2 <- 1 / sqrt(4 * h)
  b2 <- x * sqrt(4 * h) + 4 * h
  if (!is.null(sample)) {
    w <- as.numeric(sample)
    if (any(w < 0)) stop("gamma kernels need data on [0, infinity).")
    k1 <- stats::dgamma(w, shape = s1, scale = b1)
    k2 <- stats::dgamma(w, shape = s2, scale = b2)
    jh <- mean(k1)
    j4h <- mean(k2)
    cross <- mean(k1 * k2)
  } else {
    grid <- seq(0, upper, length.out = ngrid)
    fv <- vapply(grid, function(g) as.numeric(density(g)), numeric(1))
    k1 <- stats::dgamma(grid, shape = s1, scale = b1)
    k2 <- stats::dgamma(grid, shape = s2, scale = b2)
    trap <- function(y, g) sum(diff(g) * (y[-length(y)] + y[-1]) / 2)
    jh <- trap(k1 * fv, grid)
    j4h <- trap(k2 * fv, grid)
    cross <- trap(k1 * k2 * fv, grid)
  }
  list(covariance = (cross - jh * j4h) / n, form = "plugin", cross = cross,
       jh = jh, j4h = j4h, h = h, n = n,
       method = "Cov[A_h, A_4h] from its definition (exact)")
}

# CANONICAL TEST
# r <- Gkcov(x = 1, h = 0.01, n = 100, f = 0.3)
# stopifnot(identical(r$form, "interior"), r$covariance > 0)

#' @rdname Gkcov
#' @keywords internal
#' @export
morie_fauzi_thm1_4_asympnorm_mgkde <- Gkcov
