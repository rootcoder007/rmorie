# SPDX-License-Identifier: AGPL-3.0-or-later
#' Hansch QSAR (rho-sigma-pi analysis)
#'
#' log(1/C) = -a (log P)^2 + b log P + rho sigma + k, fitted by least squares;
#' the optimum partition coefficient is log P0 = b / (2 a).  Source consulted:
#' Hansch and Fujita (1964), rho-sigma-pi analysis, Journal of the American
#' Chemical Society 86(8), 1616-1626.
#'
#' @param activities numeric log(1/C) values.
#' @param logP numeric partition coefficient (or substituent constant pi).
#' @param sigma optional Hammett electronic constant.
#' @param es optional Taft steric constant.
#' @param parabolic include the quadratic log P term.
#' @return list: estimate, coefficients, names, r2, s, rss, logp0, rho, n, method.
#' @keywords internal
#' @examples
#' lp <- c(0, 1, 2, 3, 4)
#' qsarh(-(lp - 2)^2 + 5, lp)
#' @export
qsarh <- function(activities, logP, sigma = NULL, es = NULL, parabolic = TRUE) {
  y <- as.numeric(activities)
  lp <- as.numeric(logP)
  n <- min(length(y), length(lp))
  y <- y[seq_len(n)]
  lp <- lp[seq_len(n)]
  X <- cbind(1, lp)
  nms <- c("k", "logP")
  if (parabolic) { X <- cbind(X, lp^2)
  nms <- c(nms, "logP2") }
  if (!is.null(sigma)) { X <- cbind(X, as.numeric(sigma)[seq_len(n)])
  nms <- c(nms, "sigma") }
  if (!is.null(es)) { X <- cbind(X, as.numeric(es)[seq_len(n)])
  nms <- c(nms, "Es") }
  beta <- t3ols(X, y)
  fit <- as.numeric(X %*% beta)
  resid <- y - fit
  rss <- sum(resid^2)
  tss <- sum((y - mean(y))^2)
  r2 <- if (tss > 0) 1 - rss / tss else NA_real_
  p <- ncol(X)
  s <- if (n > p) sqrt(rss / (n - p)) else NA_real_
  b <- beta[2]
  a <- if (parabolic) -beta[3] else 0
  logp0 <- if (a != 0) b / (2 * a) else NA_real_
  rho <- if ("sigma" %in% nms) beta[which(nms == "sigma")] else NA_real_
  list(estimate = as.numeric(beta[1]), coefficients = beta, names = nms,
       r2 = as.numeric(r2), s = as.numeric(s), rss = as.numeric(rss),
       logp0 = as.numeric(logp0), rho = as.numeric(rho), n = as.integer(n),
       method = "Hansch rho-sigma-pi QSAR (Hansch & Fujita 1964)")
}

# CANONICAL TEST
# lp <- c(0, 1, 2, 3, 4); r <- qsarh(-(lp - 2)^2 + 5, lp)
# stopifnot(abs(r$r2 - 1) < 1e-9, abs(r$logp0 - 2) < 1e-9)

#' @rdname qsarh
#' @keywords internal
#' @export
morie_hansch_qsar <- qsarh
