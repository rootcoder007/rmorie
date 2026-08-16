# Quandt likelihood ratio (sup-Wald) structural-break test.
# Source: Quandt (1960), JASA 55, 324-330; Andrews (1993),
# Econometrica 61, 821-856; Hansen (1997), JBES 15, 60-67, Eq. 8 and
# Table 2 (fetched-wave3/hansen-1997-structural-change-pvalues.pdf;
# coefficients transcribed from the rendered page 64).  Mirrors
# Python morie.fn.qlrtst exactly (lm.fit-free normal equations for
# identical arithmetic).

.qlr_hansen_t2 <- list(
  "1" = list("0.01" = c(-1.79, 1.17, 4.5), "0.05" = c(-1.39, 1.07, 3.6),
             "0.15" = c(-0.99, 1.02, 3.0), "0.25" = c(-0.73, 0.98, 2.5),
             "0.35" = c(-0.50, 0.96, 2.1)),
  "2" = list("0.01" = c(-3.06, 1.18, 6.1), "0.05" = c(-2.38, 1.11, 5.4),
             "0.15" = c(-1.65, 1.06, 4.7), "0.25" = c(-1.16, 1.02, 4.1),
             "0.35" = c(-0.78, 0.97, 3.5)),
  "3" = list("0.01" = c(-4.09, 1.21, 7.8), "0.05" = c(-3.31, 1.10, 6.5),
             "0.15" = c(-2.05, 1.13, 6.8), "0.25" = c(-1.61, 1.03, 5.5),
             "0.35" = c(-1.06, 1.01, 4.9)),
  "4" = list("0.01" = c(-5.33, 1.21, 8.9), "0.05" = c(-4.08, 1.14, 8.2),
             "0.15" = c(-2.52, 1.11, 8.0), "0.25" = c(-1.91, 1.04, 7.0),
             "0.35" = c(-1.45, 0.97, 5.7)),
  "5" = list("0.01" = c(-6.39, 1.18, 9.4), "0.05" = c(-4.84, 1.15, 9.3),
             "0.15" = c(-3.46, 1.07, 8.3), "0.25" = c(-2.63, 1.02, 7.5),
             "0.35" = c(-1.82, 1.00, 7.0)),
  "6" = list("0.01" = c(-7.08, 1.26, 11.8), "0.05" = c(-5.37, 1.19, 11.2),
             "0.15" = c(-4.05, 1.08, 9.5), "0.25" = c(-2.94, 1.05, 9.0),
             "0.35" = c(-1.79, 1.03, 8.6)),
  "7" = list("0.01" = c(-8.49, 1.17, 11.1), "0.05" = c(-6.21, 1.21, 12.6),
             "0.15" = c(-4.42, 1.10, 11.0), "0.25" = c(-3.23, 1.05, 10.1),
             "0.35" = c(-2.21, 1.01, 9.3)),
  "8" = list("0.01" = c(-9.20, 1.17, 12.2), "0.05" = c(-7.24, 1.13, 11.9),
             "0.15" = c(-5.36, 1.08, 11.3), "0.25" = c(-3.65, 1.06, 11.4),
             "0.35" = c(-1.69, 1.10, 12.2)),
  "9" = list("0.01" = c(-10.22, 1.14, 12.3), "0.05" = c(-8.07, 1.11, 12.4),
             "0.15" = c(-5.43, 1.10, 13.1), "0.25" = c(-4.38, 1.01, 11.3),
             "0.35" = c(-2.83, 1.00, 11.1)),
  "10" = list("0.01" = c(-11.01, 1.14, 13.3), "0.05" = c(-8.84, 1.11, 13.2),
              "0.15" = c(-6.47, 1.06, 12.8), "0.25" = c(-4.97, 1.01, 12.0),
              "0.35" = c(-2.92, 1.05, 13.0))
)

#' .qlr_ssr
#'
#' A step of the qlrtst_native implementation. Called by \code{morie_qlrtst}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; passed to \code{t}.
#' @param y A matrix; passed to \code{\%*\%}.
#' @return A numeric value.
#' @export
.qlr_ssr <- function(X, y) {
  A <- t(X) %*% X
  b <- t(X) %*% y
  beta <- tryCatch(solve(A, b), error = function(e) NULL)
  if (is.null(beta)) return(NULL)
  sum((y - X %*% beta)^2)
}

#' Quandt/Andrews sup-Wald structural-break test
#'
#' SupF = max over trimmed break dates of the Wald (Chow) statistic
#' W(k) = (T - 2m)(SSR_0 - SSR_1(k))/SSR_1(k); asymptotic p-value by
#' Hansen (1997) Eq. 8, p = P(chi^2_eta > theta0 + theta1 SupF), with
#' Table 2 coefficients selected by (m, trim).
#'
#' @param y Numeric dependent variable.
#' @param X Optional regressor matrix WITHOUT intercept (added
#'   automatically); default mean-shift model.
#' @param trim Symmetric trimming: 0.01, 0.05, 0.15 (default), 0.25
#'   or 0.35.
#' @return A list with elements \code{statistic}, \code{breakpoint}
#'   (0-based index of the second regime's first observation),
#'   \code{p_value} (NULL for m > 10), \code{f_path},
#'   \code{path_start}, \code{m}, \code{trim}, \code{n},
#'   \code{method}.
#' @references Quandt, R. E. (1960). JASA, 55, 324-330.  Andrews,
#'   D. W. K. (1993). Econometrica, 61, 821-856.  Hansen, B. E.
#'   (1997). Journal of Business & Economic Statistics, 15, 60-67.
#' @export
morie_qlrtst <- function(y, X = NULL, trim = 0.15) {
  yv <- as.numeric(y)
  n <- length(yv)
  Xm <- if (is.null(X)) matrix(1, n, 1) else cbind(1, as.matrix(X))
  if (nrow(Xm) != n) stop("X must have one row per observation")
  m <- ncol(Xm)
  tr <- as.numeric(trim)
  if (!any(abs(tr - c(.01, .05, .15, .25, .35)) < 1e-12)) {
    stop("trim must be one of 0.01, 0.05, 0.15, 0.25, 0.35")
  }
  lo <- max(m + 1, ceiling(tr * n))
  hi <- min(n - m - 1, floor((1 - tr) * n))
  if (hi <= lo) stop("sample too short for this trimming")
  ssr0 <- .qlr_ssr(Xm, yv)
  if (is.null(ssr0)) stop("singular full-sample design")
  path <- numeric(0)
  best_w <- -1
  best_k <- NA
  for (k in lo:hi) {
    s1 <- .qlr_ssr(Xm[seq_len(k), , drop = FALSE], yv[seq_len(k)])
    s2 <- .qlr_ssr(Xm[(k + 1):n, , drop = FALSE], yv[(k + 1):n])
    if (is.null(s1) || is.null(s2)) {
      path <- c(path, NaN)
      next
    }
    ssr1 <- s1 + s2
    w <- if (ssr1 <= 0) Inf else (n - 2 * m) * (ssr0 - ssr1) / ssr1
    path <- c(path, w)
    if (w > best_w) {
      best_w <- w
      best_k <- k
    }
  }
  p <- NULL
  key_m <- as.character(m)
  if (!is.null(.qlr_hansen_t2[[key_m]])) {
    key_t <- sprintf("%.2f", tr)
    th <- .qlr_hansen_t2[[key_m]][[key_t]]
    z <- th[1] + th[2] * best_w
    p <- if (z <= 0) 1 else pchisq(z, th[3], lower.tail = FALSE)
  }
  list(statistic = best_w, breakpoint = best_k, p_value = p,
       f_path = path, path_start = lo, m = m, trim = tr, n = n,
       method = "Quandt/Andrews sup-Wald; Hansen (1997) p-value")
}
