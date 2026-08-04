# SPDX-License-Identifier: AGPL-3.0-or-later
#' Leave-one-out influence diagnostics for a random-effects meta-analysis
#'
#' hat = w/sum(w) with w = 1/(v + tau^2);
#' rstudent_i = (y_i - mu_(-i)) / sqrt(v_i + tau2_(-i) + Var(mu_(-i)));
#' dffits_i = (mu - mu_(-i)) / sqrt(hat_i (v_i + tau2_(-i)));
#' cook_i = (mu - mu_(-i))^2 / Var(mu).  Source consulted: Viechtbauer and
#' Cheung (2010), Research Synthesis Methods 1, 112-125.  Reproduces
#' metafor::influence().
#'
#' @param yi,vi study effects and their within-study variances.
#' @return list: estimate, rstudent, dffits, cook_d, hat, tau2_del, Q_del,
#'   estimate_del, n, method.
#' @keywords internal
#' @examples
#' mainf(c(0.1, 0.3, -0.2, 0.45), c(0.02, 0.05, 0.03, 0.08))$cook_d
#' @export
mainf <- function(yi, vi) {
  y <- as.numeric(yi); v <- as.numeric(vi); k <- length(y)
  d0 <- k02dl(y, v)
  w <- 1 / (v + d0$tau2)
  hat <- w / sum(w)
  rst <- numeric(k); dff <- numeric(k); ck <- numeric(k)
  t2d <- numeric(k); qd <- numeric(k); mud <- numeric(k)
  for (i in seq_len(k)) {
    d <- k02dl(y[-i], v[-i])
    q <- k02fe(y[-i], v[-i])$Q
    rst[i] <- (y[i] - d$mu) / sqrt(v[i] + d$tau2 + d$var)
    dff[i] <- (d0$mu - d$mu) / sqrt(hat[i] * (v[i] + d$tau2))
    ck[i] <- (d0$mu - d$mu)^2 / d0$var
    t2d[i] <- d$tau2; qd[i] <- q; mud[i] <- d$mu
  }
  list(estimate = max(ck), rstudent = rst, dffits = dff, cook_d = ck,
       hat = hat, tau2_del = t2d, Q_del = qd, estimate_del = mud, n = k,
       method = "Leave-one-out meta-analysis influence diagnostics (Viechtbauer & Cheung 2010)")
}

# CANONICAL TEST
# r <- mainf(c(0.10,0.30,-0.20,0.45,0.05,0.22), c(0.02,0.05,0.03,0.08,0.01,0.04))
# stopifnot(abs(r$rstudent[3] + 1.772369963) < 1e-8)

#' @rdname mainf
#' @keywords internal
#' @export
morie_mainf <- mainf
