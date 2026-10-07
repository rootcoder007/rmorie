#' Effect modification in matched observational studies
#'
#' \code{TruncatedProductPvalue}: Zaykin's truncated product of independent
#' P-values and its P-value as a binomial mixture of gamma laws.
#' \code{WilcoxonSensitivityMoments}: upper-bound null mean and variance of
#' Wilcoxon's signed-rank statistic at bias Gamma. \code{SubmaxComparisons}:
#' comparison matrix for p binary covariates. \code{SubmaxTest}: maximum of
#' K correlated standardized deviates against the Monte Carlo quantile of
#' the maximum of N(0, rho). Identical to the Python arm
#' \code{morie.fn.effectmod}.
#'
#' @param p P-values, or number of binary covariates for \code{SubmaxComparisons}.
#' @param alpha_tilde Truncation point.
#' @param n_pairs Number of matched pairs.
#' @param Gamma Sensitivity parameter (at least 1).
#' @param T Group statistics.
#' @param mu,V Null upper-bound means and variances of the group statistics.
#' @param C Comparison matrix (K rows, one column per group).
#' @param alpha Level.
#' @param nsim Monte Carlo draws for the critical constant.
#' @param seed Philox seed.
#' @return List (matrix for \code{SubmaxComparisons}).
#' @references Zaykin, D. V., Zhivotovsky, L. A., Westfall, P. H. and Weir,
#'   B. S. (2002). Truncated product method for combining P-values. Genetic
#'   Epidemiology 22, 170-185.
#'
#'   Lee, K., Small, D. S. and Rosenbaum, P. R. (2018). A powerful approach to
#'   the study of moderate effect modification in observational studies.
#'   Biometrics 74, 1161-1170.
#' @examples
#' TruncatedProductPvalue(c(0.025, 0.4))$pvalue
#' m <- WilcoxonSensitivityMoments(10)
#' SubmaxTest(c(40, 30), rep(m$mu, 2), rep(m$nu, 2), SubmaxComparisons(1), nsim = 2000)$D
#' @export
TruncatedProductPvalue <- function(p, alpha_tilde = 0.05) {
  .morie_arg(p, "n")
  p <- as.numeric(p)
  L <- length(p)
  a <- alpha_tilde
  if (a <= 0 || a > 1 || L == 0 || min(p) < 0 || max(p) > 1) stop("need 0 < alpha_tilde <= 1 and P-values in [0, 1]")
  w <- 1
  sel <- 0L
  for (v in p) {
    if (v <= a) {
      w <- w * v
      sel <- sel + 1L
    }
  }
  if (sel == 0L) return(list(w = 1, pvalue = 1, n_selected = 0L))
  tot <- 0
  for (k in seq_len(L)) {
    x <- if (w > 0) -log(w / a^k) else Inf
    if (x <= 0) {
      tail <- 1
    } else if (is.infinite(x)) {
      tail <- 0
    } else {
      term <- 1
      s <- 1
      if (k > 1) {
        for (j in seq_len(k - 1)) {
          term <- term * x / j
          s <- s + term
        }
      }
      tail <- exp(-x) * s
    }
    tot <- tot + choose(L, k) * a^k * (1 - a)^(L - k) * tail
  }
  list(w = w, pvalue = min(tot, 1), n_selected = sel)
}

#' @rdname TruncatedProductPvalue
#' @export
WilcoxonSensitivityMoments <- function(n_pairs, Gamma = 1) {
  I <- n_pairs
  if (I < 1 || Gamma < 1) stop("need n_pairs >= 1 and Gamma >= 1")
  k <- Gamma / (1 + Gamma)
  list(mu = k * I * (I + 1) / 2, nu = Gamma / (1 + Gamma)^2 * I * (I + 1) * (2 * I + 1) / 6)
}

#' @rdname TruncatedProductPvalue
#' @export
SubmaxComparisons <- function(p) {
  G <- 2^p
  g <- 0:(G - 1)
  rows <- list(rep(1, G))
  if (p > 0) {
    for (j in 0:(p - 1)) {
      bit <- bitwAnd(bitwShiftR(g, j), 1L)
      rows[[length(rows) + 1]] <- as.numeric(bit == 0)
      rows[[length(rows) + 1]] <- as.numeric(bit == 1)
    }
  }
  do.call(rbind, rows)
}

#' @rdname TruncatedProductPvalue
#' @export
SubmaxTest <- function(T, mu, V, C, alpha = 0.05, nsim = 100000L, seed = 1) {
  C <- as.matrix(C)
  K <- nrow(C)
  G <- length(T)
  ss <- function(v) {
    s <- 0
    for (a in v) s <- s + a
    s
  }
  theta <- vapply(seq_len(K), function(k) ss(C[k, ] * mu), 0)
  S <- vapply(seq_len(K), function(k) ss(C[k, ] * T), 0)
  Sig <- matrix(0, K, K)
  for (k in seq_len(K)) for (m in seq_len(K)) Sig[k, m] <- ss(C[k, ] * V * C[m, ])
  sd <- sqrt(diag(Sig))
  D <- (S - theta) / sd
  rho <- Sig / outer(sd, sd)
  Lc <- .s03chol(rho + diag(1e-12, K))
  e <- matrix(.morie_random_normal(nsim * K, seed = seed), K, nsim)
  mx <- numeric(nsim)
  for (s in seq_len(nsim)) {
    best <- -Inf
    for (k in seq_len(K)) {
      z <- 0
      for (j in seq_len(k)) z <- z + Lc[k, j] * e[j, s]
      if (z > best) best <- z
    }
    mx[s] <- best
  }
  mx <- sort(mx)
  kappa <- mx[ceiling((1 - alpha) * nsim)]
  list(D = D, rho = rho, Dmax = max(D), kappa = kappa, reject = max(D) > kappa)
}
