# SPDX-License-Identifier: AGPL-3.0-or-later
#' Ali-Mikhail-Haq copula
#'
#' C(u, v) = u v / (1 - theta (1 - u)(1 - v)) for theta in [-1, 1), with
#' Kendall tau = (3 theta - 2)/(3 theta) - 2 (1 - theta)^2 log(1 - theta) /
#' (3 theta^2).  Source consulted: Ali, Mikhail and Haq (1978), A class of
#' bivariate distributions including the bivariate logistic, Journal of
#' Multivariate Analysis 8(3), 405-412.
#'
#' @param u numeric vector in (0, 1).
#' @param v numeric vector in (0, 1), same length as u.
#' @param theta association parameter in [-1, 1).
#' @return list: estimate, cdf, density, loglik, tau, theta, n, method.
#' @keywords internal
#' @examples
#' ali(c(0.5), c(0.4), 0)
#' @export
ali <- function(u, v, theta = 0) {
  uu <- as.numeric(u); vv <- as.numeric(v)
  n <- min(length(uu), length(vv))
  uu <- uu[seq_len(n)]; vv <- vv[seq_len(n)]
  th <- as.numeric(theta)
  d <- 1 - th * (1 - uu) * (1 - vv)
  cdf <- uu * vv / d
  dens <- (1 - th + 2 * th * uu * vv / d - th * (1 - uu) * (1 - vv) / d) / (d * d)
  tau <- if (th == 0) 0 else if (th >= 1) 1 / 3 else
    (3 * th - 2) / (3 * th) - 2 * (1 - th)^2 * log(1 - th) / (3 * th^2)
  list(estimate = mean(cdf), cdf = cdf, density = dens,
       loglik = sum(log(dens)), tau = as.numeric(tau), theta = th,
       n = as.integer(n),
       method = "Ali-Mikhail-Haq copula (Ali, Mikhail & Haq 1978)")
}

# CANONICAL TEST
# r <- ali(0.5, 0.4, 0)
# stopifnot(abs(r$estimate - 0.2) < 1e-12, abs(r$loglik) < 1e-12, abs(r$tau) < 1e-12)

#' @rdname ali
#' @keywords internal
#' @export
morie_ali_mikhail_haq_copula <- ali
