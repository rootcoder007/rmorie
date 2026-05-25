# SPDX-License-Identifier: AGPL-3.0-or-later

#' Glivenko-Cantelli theorem verification (one-sample KS)
#'
#' sup_t |F_n(t) - F(t)| under a hypothesised CDF F.  By
#' Glivenko-Cantelli the statistic -> 0 a.s. when F is correct.
#'
#' @param x Numeric vector.
#' @param cdf Name of CDF (default \"pnorm\").
#' @return Named list with statistic, p_value, n, method.
#' @references Kosorok (2008), Ch 2.
#' @examples
#' morie_ksr03_kosorok_glivenko_cantelli(x = rnorm(50))
#' @export
morie_ksr03_kosorok_glivenko_cantelli <- function(x, cdf = "pnorm") {
  x <- as.numeric(x)
  n <- length(x)
  res <- suppressWarnings(stats::ks.test(x, cdf))
  list(
    statistic = unname(res$statistic),
    p_value   = res$p.value,
    n         = n,
    method    = "Glivenko-Cantelli / KS sup|F_n - F|"
  )
}

# CANONICAL TEST
# set.seed(0); morie_ksr03_kosorok_glivenko_cantelli(rnorm(200))

#' @rdname morie_ksr03_kosorok_glivenko_cantelli
#' @keywords internal
#' @export
morie_kosorok_glivenko_cantelli <- morie_ksr03_kosorok_glivenko_cantelli
