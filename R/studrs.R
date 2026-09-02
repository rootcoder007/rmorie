# SPDX-License-Identifier: AGPL-3.0-or-later
#' Residuals rescaled by a variance the point itself did not inflate
#'
#' An outlier drags the fitted line toward itself and inflates the
#' residual standard error, so the ordinary standardised residual
#' understates how odd the point is, twice over. Deleting the point from
#' the scale estimate breaks the circularity, which is why this version
#' follows a t distribution.
#'
#' Formula: \code{t_i = e_i / (s_(i) sqrt(1 - h_ii))},
#' \code{s_(i)^2 = \[(n-p)s^2 - e_i^2/(1-h_ii)\]/(n-p-1)}.
#'
#' @param y Response.
#' @param X Design; supply your own intercept column.
#' @return List with \code{estimate}, \code{t}, \code{leverage},
#'   \code{sigma}, \code{df}, \code{n}.
#' @references Weisberg, S. (2014). Applied Linear Regression, 4th ed,
#'   section 9.1; Belsley, Kuh & Welsch (1980), Regression Diagnostics,
#'   ch 2.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Studrs(V, V)
Studrs <- function(y, X) {
  Xm <- as.matrix(X)
  yv <- as.numeric(y)
  n <- nrow(Xm)
  p <- ncol(Xm)
  fit <- .s4_ols(Xm, yv)
  h <- rowSums((Xm %*% fit$xtxinv) * Xm)
  s2 <- sum(fit$resid^2) / (n - p)
  si2 <- ((n - p) * s2 - fit$resid^2 / (1 - h)) / (n - p - 1)
  den <- sqrt(si2 * (1 - h))
  t_ <- ifelse(!is.na(den) & den > 0, fit$resid / den, NaN)
  big <- which.max(ifelse(is.nan(t_), -1, abs(t_)))
  .t1_result(estimate = t_[big], t = t_, leverage = h, sigma = sqrt(s2),
             df = n - p - 1, n = n,
             method = "Externally studentized residuals")
}
