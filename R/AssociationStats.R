#' Association measures and affine moments
#'
#' \code{VariationRatio}: proportion of cases outside the modal category.
#' \code{YuleAssociation}: Yule's Q and Y with standard errors and Wald
#' intervals for a 2 by 2 table. \code{TostCorrelation}: two one-sided tests
#' of correlation equivalence with Fisher's z. \code{AffineMoments}: mean and
#' covariance of affine transformations of a random vector. Identical to the
#' Python arm \code{morie.fn.assocstats}.
#'
#' @param x Observations of a nominal or ordinal variable.
#' @param table A 2 by 2 table (matrix, rows a b / c d).
#' @param conf_level Confidence level.
#' @param r Sample correlation.
#' @param n Sample size.
#' @param low,high Equivalence bounds on the correlation.
#' @param alpha Test level.
#' @param mean Mean vector.
#' @param cov Covariance matrix.
#' @param A,B Transformation matrices.
#' @param c,d Constant vectors.
#' @return A number or a list.
#' @references Freeman, L. C. (1965). Elementary Applied Statistics. Wiley.
#'
#'   Yule, G. U. (1912). On the methods of measuring association between two
#'   attributes. Journal of the Royal Statistical Society 75, 579-652.
#'
#'   Lakens, D. (2017). Equivalence tests: a practical primer for t tests,
#'   correlations, and meta-analyses. Social Psychological and Personality
#'   Science 8, 355-362.
#' @examples
#' VariationRatio(c("a", "b", "a", "c", "a"))
#' YuleAssociation(matrix(c(20, 5, 10, 15), 2))$Q
#' TostCorrelation(0.02, 200, -0.2, 0.2)$p_value
#' @export
VariationRatio <- function(x) {
  if (length(x) == 0) stop("x is empty")
  1 - max(table(x)) / length(x)
}

#' @rdname VariationRatio
#' @export
YuleAssociation <- function(table, conf_level = 0.95) {
  a <- table[1, 1]
  b <- table[1, 2]
  cc <- table[2, 1]
  d <- table[2, 2]
  ad <- a * d
  bc <- b * cc
  Q <- (ad - bc) / (ad + bc)
  Y <- (sqrt(ad) - sqrt(bc)) / (sqrt(ad) + sqrt(bc))
  s <- if (min(a, b, cc, d) > 0) sqrt(1 / a + 1 / b + 1 / cc + 1 / d) else Inf
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  seq <- (1 - Q^2) * s / 2
  sey <- (1 - Y^2) * s / 4
  list(Q = Q, Y = Y, se_Q = seq, se_Y = sey, ci_Q = c(Q - z * seq, Q + z * seq), ci_Y = c(Y - z * sey, Y + z * sey))
}

#' @rdname VariationRatio
#' @export
TostCorrelation <- function(r, n, low, high, alpha = 0.05) {
  if (!(-1 < low && low < high && high < 1 && -1 < r && r < 1 && n > 3)) {
    stop("need -1 < low < high < 1, -1 < r < 1 and n > 3")
  }
  sq <- sqrt(n - 3)
  zl <- (atanh(r) - atanh(low)) * sq
  zh <- (atanh(r) - atanh(high)) * sq
  pl <- stats::pnorm(-zl)
  ph <- stats::pnorm(zh)
  zc <- stats::qnorm(1 - alpha)
  p <- max(pl, ph)
  list(z_low = zl, z_high = zh, p_low = pl, p_high = ph, p_value = p, equivalent = p < alpha,
       ci = c(tanh(atanh(r) - zc / sq), tanh(atanh(r) + zc / sq)))
}

#' @rdname VariationRatio
#' @export
AffineMoments <- function(mean, cov, A, c = NULL, B = NULL, d = NULL) {
  A <- as.matrix(A)
  if (ncol(A) != length(mean)) A <- matrix(A, nrow = 1)
  Bm <- if (is.null(B)) A else as.matrix(B)
  if (ncol(Bm) != length(mean)) Bm <- matrix(Bm, nrow = 1)
  cc <- if (is.null(c)) numeric(nrow(A)) else c
  out <- list(mean = as.vector(A %*% mean) + cc, cov = A %*% cov %*% t(Bm))
  if (!is.null(B)) out$mean_B <- as.vector(Bm %*% mean) + (if (is.null(d)) numeric(nrow(Bm)) else d)
  out
}
