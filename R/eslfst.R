# SPDX-License-Identifier: AGPL-3.0-or-later
#' F-test for a nested pair of linear models (ESL eq. 3.13)
#'
#' Source READ FROM THE CORPUS PDF: Hastie, Tibshirani and Friedman,
#' The Elements of Statistical Learning (2nd ed., 2009), section 3.2,
#' equation (3.13):
#' \code{F = ((RSS0 - RSS1)/(p1 - p0)) / (RSS1/(N - p1 - 1))}, which
#' under the Gaussian null is \code{F(p1 - p0, N - p1 - 1)}.
#'
#' @param model0,model1 Integer column indices of \code{X} in each
#'   model, 1-based; \code{model0} must be nested in \code{model1}.  An
#'   intercept is always added.
#' @param X Numeric matrix of predictors without an intercept column.
#' @param y Numeric response.
#' @return list: statistic, p_value, df1, df2, rss0, rss1, p0, p1, n,
#'   method.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(60), 20, 3)
#' Fnested(1L, c(1L, 2L, 3L), X, X[, 1] + rnorm(20))$statistic
#' @export
Fnested <- function(model0, model1, X, y) {
  X <- as.matrix(X)
  y <- as.numeric(y)
  n <- length(y)
  s0 <- sort(unique(as.integer(model0)))
  s1 <- sort(unique(as.integer(model1)))
  if (!all(s0 %in% s1)) stop("model0 must be nested inside model1")
  rss <- function(cols) {
    D <- cbind(1, if (length(cols)) X[, cols, drop = FALSE] else NULL)
    b <- qr.solve(D, y)
    r <- y - D %*% b
    sum(r * r)
  }
  r0 <- rss(s0)
  r1 <- rss(s1)
  p0 <- length(s0)
  p1 <- length(s1)
  df1 <- p1 - p0
  df2 <- n - p1 - 1
  if (df1 <= 0 || df2 <= 0) stop("need p1 > p0 and N > p1 + 1")
  stat <- ((r0 - r1) / df1) / (r1 / df2)
  list(
    statistic = stat, p_value = stats::pf(stat, df1, df2, lower.tail = FALSE),
    df1 = as.integer(df1), df2 = as.integer(df2),
    rss0 = r0, rss1 = r1, p0 = as.integer(p0), p1 = as.integer(p1), n = n,
    method = "F-test for nested linear models (ESL eq. 3.13)"
  )
}
