# SPDX-License-Identifier: AGPL-3.0-or-later

#' Efficient score for OLS beta in the linear model
#'
#' S_eff(X, Y) = (Y - E(Y|X))(X - E(X)) / sigma^2.
#'
#' @param x Numeric covariate vector.
#' @param y Numeric outcome vector.
#' @return Named list with estimate (mean efficient score),
#'   se (residual sd), n, method.
#' @references Kosorok (2008), Ch 6.
#' @examples
#' morie_ksr11_kosorok_efficient_score(x = rnorm(50), y = rnorm(50))
#' @export
morie_ksr11_kosorok_efficient_score <- function(x, y) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  n <- length(x)
  xc <- x - mean(x)
  yc <- y - mean(y)
  beta <- sum(xc * yc) / sum(xc^2)
  resid <- yc - beta * xc
  sigma2 <- sum(resid^2) / (n - 2)
  s_eff <- (resid * xc) / sigma2
  list(
    estimate = mean(s_eff),
    se       = sqrt(sigma2),
    n        = n,
    method   = "Efficient score for OLS beta: (resid)(x-xbar)/sigma^2"
  )
}

# CANONICAL TEST
# set.seed(0); xs <- rnorm(200); ys <- 1.5*xs + rnorm(200)
# morie_ksr11_kosorok_efficient_score(xs, ys)

#' @rdname morie_ksr11_kosorok_efficient_score
#' @keywords internal
#' @export
morie_kosorok_efficient_score <- morie_ksr11_kosorok_efficient_score
