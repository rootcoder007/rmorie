# SPDX-License-Identifier: AGPL-3.0-or-later

#' Mean residual life plot
#'
#' Formula: e(u) = mean(X - u | X > u)
#'
#' Linear in u above a threshold where the GPD holds, with slope
#' xi/(1-xi) and intercept sigma_u0/(1-xi); constant when xi = 0.  That
#' linearity is what the plot is read for.
#'
#' @param x Sample.
#' @param u_grid Thresholds, or NULL for twenty equally spaced values
#'   from the minimum to the 90th percentile.
#' @return List with \code{u}, \code{e_u}, \code{se}, \code{n_exceed},
#'   \code{estimate} (slope), \code{n}, \code{method}.
#' @references Davison & Smith (1990), JRSS B 52(3):393-442.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Evmrlp(V)
Evmrlp <- function(x, u_grid = NULL) {
  x <- .s03vec(x)
  n <- length(x)
  if (n < 2L) stop("empty input: need at least two observations")
  if (is.null(u_grid)) {
    lo <- min(x)
    hi <- .s03quantile7(x, 0.9)
    u_grid <- lo + (hi - lo) * (0:19) / 19
  } else u_grid <- .s03vec(u_grid)
  if (!length(u_grid)) stop("u_grid is empty")
  us <- c()
  es <- c()
  se <- c()
  nex <- c()
  for (u in u_grid) {
    ex <- x[x > u] - u
    k <- length(ex)
    if (k < 2L) next
    m <- sum(ex) / k
    v <- sum((ex - m)^2) / (k - 1)
    us <- c(us, u)
    es <- c(es, m)
    se <- c(se, sqrt(v / k))
    nex <- c(nex, k)
  }
  if (length(us) < 2L)
    stop("no threshold leaves two exceedances; grid too high")
  mu_u <- sum(us) / length(us)
  mu_e <- sum(es) / length(es)
  sxx <- sum((us - mu_u)^2)
  sxy <- sum((us - mu_u) * (es - mu_e))
  .t1_result(u = us, e_u = es, se = se, n_exceed = nex,
             estimate = if (sxx > 0) sxy / sxx else NaN, n = n,
             method = "mean residual life (mean excess) plot")
}
