.ivv_stats <- function(y, d, z, sets) {
  n <- length(y)
  m0 <- sum(z == 0)
  m1 <- n - m0
  Tn <- m0 * m1 / n
  p0 <- m0 / n
  p1 <- m1 / n
  out <- vapply(seq_len(nrow(sets)), function(k) {
    if (sets$kind[k] == "B") {
      ind <- as.numeric(y >= sets$a[k] & y <= sets$b[k] & d == sets$d[k])
      sgn <- if (sets$d[k] == 1) -1 else 1
    } else {
      ind <- as.numeric(d == 0)
      sgn <- 1
    }
    q1 <- sum(ind[z == 1]) / n
    q0 <- sum(ind[z == 0]) / n
    v <- Tn / n * (q1 / p1^2 - q1^2 / p1^3 + q0 / p0^2 - q0^2 / p0^3)
    c(sgn * (q1 / p1 - q0 / p0), sqrt(max(v, 0)))
  }, numeric(2))
  list(phi = out[1, ], sig = out[2, ], Tn = Tn)
}

#' Kitagawa (2015) test of instrument validity
#'
#' Variance-weighted Kolmogorov-Smirnov test of the testable implications of
#' IV validity with a binary treatment and a binary instrument (for every
#' closed interval `B`: `P(Y in B, D = 1 | Z = 1) >= P(Y in B, D = 1 | Z = 0)`,
#' `P(Y in B, D = 0 | Z = 0) >= P(Y in B, D = 0 | Z = 1)`, and `P(D = 0 | Z = 1)
#' <= P(D = 0 | Z = 0)`), with the plug-in standard errors, trimming `xi` and
#' the contact-set bootstrap of Sun (2023); bootstrap draw `b` uses Philox
#' stream `b` of `seed`, as in the Python arm.
#'
#' @param y Outcome.
#' @param D Binary treatment.
#' @param Z Binary instrument.
#' @param xi Trimming constant (0.07 suggested by Kitagawa).
#' @param tau Contact-set tuning (2 recommended by Sun for n below 3000).
#' @param n_boot Number of bootstrap draws.
#' @param seed Philox seed.
#' @return List with `statistic`, `p_value`, `critical_value_05`, `n_boot`,
#'   `contact_set_size`, `xi`, `tau`, `T_n`, `method`.
#' @references Kitagawa, T. (2015). A test for instrument validity.
#'   Econometrica 83, 2043-2063. Sun, Z. (2023). Instrument validity for
#'   heterogeneous causal effects. Journal of Econometrics 237, 105523.
#' @examples
#' z <- rep(0:1, 30)
#' d <- as.numeric(sin(2.7 * (0:59)) + 0.8 * z > 0.3)
#' y <- round(cos(1.3 * (0:59)) + d, 1)
#' bound_monotone_test(y, 1 - d, z, n_boot = 99)$statistic
#' @export
bound_monotone_test <- function(y, D, Z, xi = 0.07, tau = 2, n_boot = 500, seed = 0) {
  y <- as.numeric(y)
  D <- as.integer(D)
  Z <- as.integer(Z)
  n <- length(y)
  if (!all(D %in% 0:1) || !all(Z %in% 0:1) || length(unique(Z)) < 2) {
    stop("D and Z must be binary and Z must take both values")
  }
  g <- sort(unique(y))
  u <- length(g)
  ab <- which(upper.tri(matrix(0, u, u), diag = TRUE), arr.ind = TRUE)
  ab <- ab[order(ab[, 1], ab[, 2]), , drop = FALSE]
  sets <- rbind(
    data.frame(kind = "B", a = g[ab[, 1]], b = g[ab[, 2]], d = 0L),
    data.frame(kind = "B", a = g[ab[, 1]], b = g[ab[, 2]], d = 1L),
    data.frame(kind = "C", a = NA, b = NA, d = NA)
  )
  s <- .ivv_stats(y, D, Z, sets)
  ts <- max(sqrt(s$Tn) * s$phi / pmax(xi, s$sig))
  contact <- which(sqrt(s$Tn) * abs(s$phi) / pmax(0.001, s$sig) <= tau)
  boot <- numeric(0)
  for (bb in seq_len(n_boot) - 1) {
    idx <- pmin(n - 1, floor(.morie_random_uniform(n, seed = seed, stream = bb) * n)) + 1
    if (length(unique(Z[idx])) < 2) next
    if (!length(contact)) {
      boot <- c(boot, 0)
      next
    }
    sb <- .ivv_stats(y[idx], D[idx], Z[idx], sets[contact, , drop = FALSE])
    boot <- c(boot, max(sqrt(sb$Tn) * (sb$phi - s$phi[contact]) / pmax(xi, sb$sig)))
  }
  srt <- sort(boot)
  list(statistic = ts, p_value = mean(boot >= ts), critical_value_05 = srt[min(length(srt), ceiling(0.95 * length(srt)))],
       n_boot = length(boot), contact_set_size = length(contact), xi = xi, tau = tau, T_n = s$Tn,
       method = "Kitagawa (2015) / Sun (2023) variance-weighted KS test of IV validity")
}
