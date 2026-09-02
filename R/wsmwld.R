# SPDX-License-Identifier: AGPL-3.0-or-later
#' Wald test of H0: theta = theta0 against a two-sided alternative
#'
#' Formula: W = (theta_hat - theta0)/se; reject when |W| > z_\{alpha/2\};
#'   p = 2(1 - Phi(|W|))
#'
#' @param theta_hat The estimate.
#' @param se Its estimated standard error, se > 0.
#' @param theta0 The null value.
#' @param level Confidence level for the returned interval.
#' @return List with \code{statistic}, \code{p_value}, \code{estimate},
#'   \code{se}, \code{ci_lower}, \code{ci_upper}, \code{z_critical},
#'   \code{reject}.
#' @references Wasserman (2004), All of Statistics, Definition 10.3 and
#'   equation (10.5), with Theorem 10.4 giving the asymptotic size.
#'   Fetched as the full text of the book.
#' @export
#' @examples
#' Waldstat(theta_hat = c(1, 2, 3, 4, 5, 6, 7, 8), se = 5L)
Waldstat <- function(theta_hat, se, theta0 = 0, level = 0.95) {
  se <- as.numeric(se)
  if (se <= 0) stop("the standard error must be positive")
  if (level <= 0 || level >= 1)
    stop("level must lie strictly between 0 and 1")
  theta_hat <- as.numeric(theta_hat); theta0 <- as.numeric(theta0)
  W <- (theta_hat - theta0) / se
  p <- 2 * (1 - stats::pnorm(abs(W)))
  z <- stats::qnorm((1 + level) / 2)
  .t1_result(statistic = W, p_value = p, estimate = theta_hat, se = se,
             ci_lower = theta_hat - z * se, ci_upper = theta_hat + z * se,
             z_critical = z, reject = as.numeric(abs(W) > z),
             method = "Wald test, Wasserman Definition 10.3")
}
