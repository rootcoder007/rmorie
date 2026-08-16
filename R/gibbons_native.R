# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Nonparametric theory mirrors for the Gibbons shelf (batch 4).
#
# Mirrors the computational surface of the morie.fn gb* modules
# (Chakraborti & Gibbons, Nonparametric Statistical Inference,
# 5th ed.). Deliberately does NOT duplicate what R already carries:
# morie_kendall_tau / morie_spearman_rho (R/inference.R),
# morie_runs_test (R/stats_primitives_native.R),
# morie_concordance_incomplete (R/cncrd.R), and
# morie_tolerance_limits (R/tolim.R, the Wilks r = 1, s = n case --
# morie_tolerance_beta below is its general-(r, s) extension).
#
# PDF-verified sources: Theorem 3.2.1 (runs joint pmf, printed
# p. 77), Theorem 4.3.3 (Kolmogorov limit, p. 108), Theorem 2.11.1
# (coverage ~ Beta, p. 61), Table 13.3.1 (ARE values, p. 492),
# Sec. 13.3.3 (ARE(Mood, F) = 15/(2 pi^2)), Theorem 7.3.6 (folded
# scores need EVEN N -- the half-swap conjugate Z'_i = Z_{i+N/2},
# p. 282).

#' Null distributions of the two-sample runs counts
#'
#' Theorem 3.2.1 / Corollary / Theorem 3.2.2: joint, marginal and
#' total-runs pmfs under randomness. Mirrors morie.fn.gb321/gb321c/
#' gb322.
#'
#' @param n1,n2 counts of the two element types.
#' @param r1,r2 run counts for the joint pmf (optional).
#' @param r total runs for the total pmf (optional).
#' @return list: joint (if r1, r2 given), marginal_r1 (vector over
#'   r1 = 1..n1), total (vector over r = 2..n1+n2), mean, var.
#' @references Gibbons & Chakraborti (2021), Nonparametric
#'   Statistical Inference, 5th ed., Theorems 3.2.1-3.2.2,
#'   eqs. (3.2.6), (3.2.8).
#' @examples
#' morie_runs_pmf(4, 5)$mean
#' @export
morie_runs_pmf <- function(n1, n2, r1 = NULL, r2 = NULL, r = NULL) {
  n1 <- as.integer(n1)
  n2 <- as.integer(n2)
  if (n1 < 1L || n2 < 1L) stop("n1 and n2 must be at least 1.", call. = FALSE)
  denom <- choose(n1 + n2, n1)
  joint <- NULL
  if (!is.null(r1) && !is.null(r2)) {
    r1 <- as.integer(r1)
    r2 <- as.integer(r2)
    if (r1 < 1L || r1 > n1 || r2 < 1L || r2 > n2) {
      stop("run counts must lie in 1..n of their type.", call. = FALSE)
    }
    joint <- if (abs(r1 - r2) > 1L) {
      0
    } else {
      cc <- if (r1 == r2) 2 else 1
      cc * choose(n1 - 1, r1 - 1) * choose(n2 - 1, r2 - 1) / denom
    }
  }
  marg <- vapply(seq_len(n1), function(k) {
    choose(n1 - 1, k - 1) * choose(n2 + 1, k) / denom
  }, 0)
  total <- vapply(2:(n1 + n2), function(rr) {
    if (rr %% 2L == 0L) {
      k <- rr %/% 2L
      2 * choose(n1 - 1, k - 1) * choose(n2 - 1, k - 1) / denom
    } else {
      k <- (rr - 1L) %/% 2L
      (choose(n1 - 1, k) * choose(n2 - 1, k - 1) +
        choose(n1 - 1, k - 1) * choose(n2 - 1, k)) / denom
    }
  }, 0)
  tot_p <- if (!is.null(r)) {
    r <- as.integer(r)
    if (r < 2L || r > n1 + n2) stop("r must lie in 2..n1+n2.", call. = FALSE)
    total[r - 1L]
  } else {
    NULL
  }
  n <- n1 + n2
  list(
    joint = joint, marginal_r1 = marg, total = total, total_at_r = tot_p,
    mean = 1 + 2 * n1 * n2 / n,
    var = 2 * n1 * n2 * (2 * n1 * n2 - n1 - n2) / (n^2 * (n - 1)),
    n1 = n1, n2 = n2,
    method = "Runs pmfs and moments (Gibbons Theorems 3.2.1-3.2.2)"
  )
}

#' Exact null distribution of runs up and down
#'
#' Enumeration over all n! orderings (n <= 9), mirroring
#' morie.fn.gb32lu; moments (2n-1)/3 and (16n-29)/90 from Ch. 3.4
#' (gb34mn) are returned for comparison.
#'
#' @param n sequence length, 3..9.
#' @param x optional observed sequence of distinct values to score.
#' @return list: support, pmf, mean, var, mean_formula, var_formula,
#'   observed, p_le, p_ge.
#' @references Gibbons & Chakraborti (2021), Ch. 3.4.
#' @examples
#' morie_runs_updown(n = 5)$mean
#' @export
morie_runs_updown <- function(n = NULL, x = NULL) {
  obs <- NULL
  if (!is.null(x)) {
    x <- as.numeric(x)
    if (anyDuplicated(x)) stop("runs up/down need distinct values.", call. = FALSE)
    n <- length(x)
    d <- sign(diff(x))
    obs <- 1L + sum(d[-1] != d[-length(d)])
  }
  if (is.null(n)) stop("supply n or x.", call. = FALSE)
  n <- as.integer(n)
  if (n < 3L || n > 9L) {
    stop("exact enumeration is limited to 3 <= n <= 9.", call. = FALSE)
  }
  perms <- .morie_permutations(n)
  runs <- apply(perms, 1L, function(p) {
    d <- sign(diff(p))
    1L + sum(d[-1] != d[-length(d)])
  })
  tab <- table(runs)
  support <- as.integer(names(tab))
  pmf <- as.numeric(tab) / factorial(n)
  mu <- sum(support * pmf)
  out <- list(
    support = support, pmf = pmf, mean = mu,
    var = sum(support^2 * pmf) - mu^2,
    mean_formula = (2 * n - 1) / 3, var_formula = (16 * n - 29) / 90,
    n = n, method = "Exact runs up/down pmf by enumeration (Gibbons Ch. 3.4)"
  )
  if (!is.null(obs)) {
    out$observed <- obs
    out$p_le <- sum(pmf[support <= obs])
    out$p_ge <- sum(pmf[support >= obs])
  }
  out
}

#' .morie_permutations
#'
#' A step of the gibbons_native implementation. Called by \code{morie_rank_exact_null}, \code{morie_runs_updown}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param n A count; the body uses it as \code{seq_len(...)}.
#' @return The value of \code{out}, as built in the body.
#' @export
.morie_permutations <- function(n) {
  if (n == 1L) {
    return(matrix(1L, 1L, 1L))
  }
  sub <- .morie_permutations(n - 1L)
  out <- matrix(0L, nrow(sub) * n, n)
  row <- 1L
  for (i in seq_len(nrow(sub))) {
    for (pos in seq_len(n)) {
      out[row, ] <- append(sub[i, ], n, after = pos - 1L)
      row <- row + 1L
    }
  }
  out
}

#' Distribution-free tolerance coefficient for (X(r), X(s))
#'
#' Theorem 2.11.1 (PDF-verified): coverage of the order-statistic
#' interval is Beta(s - r, n - s + r + 1), so
#' gamma = 1 - I_p(s - r, n - s + r + 1). Extends
#' \code{\link{morie_tolerance_limits}} (the r = 1, s = n Wilks case)
#' to arbitrary indices, and inverts for the required n. Mirrors
#' morie.fn.gb2111.
#'
#' @param n sample size (with r, s, p).
#' @param r,s order-statistic indices, 1 <= r < s <= n; s defaults
#'   to n.
#' @param p required coverage in (0, 1).
#' @param gamma when supplied INSTEAD of n, the smallest n with
#'   (1, n) endpoints achieving this tolerance coefficient is found.
#' @return list: gamma or n_required, coverage_dist, r, s, p.
#' @references Gibbons & Chakraborti (2021), Theorem 2.11.1.
#' @examples
#' morie_tolerance_beta(n = 50, p = 0.8)$gamma
#' @export
morie_tolerance_beta <- function(n = NULL, r = 1L, s = NULL, p = 0.9,
                                 gamma = NULL) {
  if (!isTRUE(p > 0 && p < 1)) stop("p must lie in (0, 1).", call. = FALSE)
  if (!is.null(gamma) && is.null(n)) {
    if (!isTRUE(gamma > 0 && gamma < 1)) {
      stop("gamma must lie in (0, 1).", call. = FALSE)
    }
    for (m in 2:100000) {
      if (1 - stats::pbeta(p, m - 1, 2) >= gamma) {
        return(list(
          n_required = m, gamma = gamma, p = p, r = 1L, s = m,
          coverage_dist = c(m - 1, 2),
          method = "Smallest n for (X(1), X(n)) tolerance (Thm 2.11.1)"
        ))
      }
    }
    stop("no n below 100000 achieves that tolerance.", call. = FALSE)
  }
  n <- as.integer(n)
  if (n < 2L) stop("n must be at least 2.", call. = FALSE)
  if (is.null(s)) s <- n
  r <- as.integer(r)
  s <- as.integer(s)
  if (!(r >= 1L && r < s && s <= n)) {
    stop("need 1 <= r < s <= n.", call. = FALSE)
  }
  a <- s - r
  b <- n - s + r + 1L
  list(
    gamma = 1 - stats::pbeta(p, a, b), coverage_dist = c(a, b),
    r = r, s = s, p = p, n = n,
    method = "Coverage ~ Beta(s-r, n-s+r+1) (Gibbons Theorem 2.11.1)"
  )
}

#' Kolmogorov-Smirnov limits and exact one-sided tail
#'
#' Theorem 4.3.3 (two-sided Kolmogorov series L(d)), Theorem 4.3.5
#' (one-sided limit 1 - exp(-2 d^2)) and the Birnbaum-Tingey exact
#' one-sided tail (eq. 4.3.5). Mirrors morie.fn.gb433/gb434bt/gb435/
#' gb4351.
#'
#' @param d argument of the limiting laws (d > 0).
#' @param n sample size for the exact one-sided tail at c = d.
#' @param exact_c threshold for the Birnbaum-Tingey formula in (0, 1).
#' @return list: L (two-sided cdf), p_two_sided, one_sided_cdf,
#'   p_one_sided, bt_p_exceed (if exact_c and n given), chi2_stat.
#' @references Gibbons & Chakraborti (2021), Theorems 4.3.3, 4.3.5,
#'   eq. (4.3.5), Corollary 4.3.5.1.
#' @examples
#' morie_ks_limit(1.36)$L
#' @export
morie_ks_limit <- function(d, n = NULL, exact_c = NULL) {
  d <- as.numeric(d)
  if (d <= 0) stop("d must be positive.", call. = FALSE)
  i <- 1:200
  L <- 1 - 2 * sum((-1)^(i - 1) * exp(-2 * i^2 * d^2))
  L <- min(max(L, 0), 1)
  out <- list(
    L = L, p_two_sided = 1 - L,
    one_sided_cdf = 1 - exp(-2 * d^2), p_one_sided = exp(-2 * d^2),
    d = d, method = "Kolmogorov L(d) + one-sided limits (Thms 4.3.3, 4.3.5)"
  )
  if (!is.null(exact_c) && !is.null(n)) {
    cc <- as.numeric(exact_c)
    n <- as.integer(n)
    if (!(cc > 0 && cc < 1)) stop("exact_c must lie in (0, 1).", call. = FALSE)
    if (n < 1L) stop("n must be at least 1.", call. = FALSE)
    pexc <- (1 - cc)^n
    jmax <- floor(n * (1 - cc))
    if (jmax >= 1) {
      j <- 1:jmax
      pexc <- pexc + cc * sum(choose(n, j) * (1 - cc - j / n)^(n - j) *
        (cc + j / n)^(j - 1))
    }
    out$bt_p_exceed <- min(max(pexc, 0), 1)
    out$chi2_stat <- 4 * n * cc^2
    out$chi2_p <- stats::pchisq(4 * n * cc^2, 2, lower.tail = FALSE)
  }
  out
}

#' Tie-corrected null variances for the rank-sum family
#'
#' The combined-sample tie sums shrink the null variance of U/W and
#' inflate Kruskal-Wallis H (Gibbons Chs. 6.6, 8.2, 10.4). Mirrors
#' morie.fn.gb661t/gb821t/gb1041t.
#'
#' @param x,y the two samples (two-sample mode), or
#' @param groups list of samples (Kruskal-Wallis mode).
#' @return two-sample: list U, W, mean_U, var_corrected,
#'   var_uncorrected, z, p_two_sided; KW: list H, H_uncorrected,
#'   correction, df, p_value.
#' @references Gibbons & Chakraborti (2021), Chs. 6.6, 8.2, 10.4.
#' @examples
#' morie_rank_tie_variance(c(1, 2, 2, 3), c(2, 3, 3, 4))$U
#' @export
morie_rank_tie_variance <- function(x = NULL, y = NULL, groups = NULL) {
  if (!is.null(groups)) {
    gs <- lapply(groups, as.numeric)
    k <- length(gs)
    if (k < 2L) stop("need at least 2 groups.", call. = FALSE)
    combined <- unlist(gs)
    N <- length(combined)
    rk <- rank(combined)
    H <- 0
    pos <- 0L
    for (g in gs) {
      idx <- (pos + 1L):(pos + length(g))
      H <- H + sum(rk[idx])^2 / length(g)
      pos <- pos + length(g)
    }
    H <- 12 / (N * (N + 1)) * H - 3 * (N + 1)
    tt <- table(combined)
    corr <- 1 - sum(tt * (tt^2 - 1)) / (N^3 - N)
    if (corr <= 0) stop("all observations tied.", call. = FALSE)
    H_adj <- H / corr
    return(list(
      H = H_adj, H_uncorrected = H, correction = corr, df = k - 1L,
      p_value = stats::pchisq(H_adj, k - 1L, lower.tail = FALSE),
      method = "Kruskal-Wallis with tie correction (Gibbons Ch. 10.4)"
    ))
  }
  x <- as.numeric(x)
  y <- as.numeric(y)
  m <- length(x)
  n <- length(y)
  if (m < 1L || n < 1L) stop("both samples must be non-empty.", call. = FALSE)
  N <- m + n
  rk <- rank(c(x, y))
  W <- sum(rk[seq_len(m)])
  U <- W - m * (m + 1) / 2
  tt <- table(c(x, y))
  tie_sum <- sum(tt * (tt^2 - 1))
  v <- m * n / 12 * ((N + 1) - tie_sum / (N * (N - 1)))
  if (v <= 0) stop("all observations tied.", call. = FALSE)
  z <- (U - m * n / 2) / sqrt(v)
  list(
    U = U, W = W, mean_U = m * n / 2, var_corrected = v,
    var_uncorrected = m * n * (N + 1) / 12, z = z,
    p_two_sided = 2 * stats::pnorm(abs(z), lower.tail = FALSE),
    tie_sum = tie_sum, m = m, n = n,
    method = "Mann-Whitney/rank-sum tie-corrected variance (Chs. 6.6, 8.2)"
  )
}

#' Exact null distributions of Kendall's tau and Spearman's rho
#'
#' Enumeration over all n! rankings, n <= 8. The exact variances
#' 2(2n+5)/(9n(n-1)) and 1/(n-1) fall out of the pmfs, which is the
#' cross-check. Mirrors morie.fn.gb_kt2/gb_sp2/gb_ktv/gb_spv.
#'
#' @param n number of objects, 2..8.
#' @param statistic "kendall" or "spearman".
#' @return list: support, pmf, mean, var, var_formula.
#' @references Gibbons & Chakraborti (2021), Chs. 11.2-11.3.
#' @examples
#' morie_rank_exact_null(5, "spearman")$var
#' @export
morie_rank_exact_null <- function(n, statistic = "kendall") {
  statistic <- match.arg(statistic, c("kendall", "spearman"))
  n <- as.integer(n)
  if (n < 2L || n > 8L) {
    stop("exact enumeration is limited to 2 <= n <= 8.", call. = FALSE)
  }
  perms <- .morie_permutations(n)
  ref <- seq_len(n)
  vals <- apply(perms, 1L, function(p) {
    if (statistic == "kendall") {
      s <- 0L
      for (i in 1:(n - 1)) {
        for (j in (i + 1):n) s <- s + sign(p[j] - p[i])
      }
      s / choose(n, 2)
    } else {
      1 - 6 * sum((p - ref)^2) / (n^3 - n)
    }
  })
  tab <- table(round(vals, 12))
  support <- as.numeric(names(tab))
  pmf <- as.numeric(tab) / factorial(n)
  mu <- sum(support * pmf)
  vf <- if (statistic == "kendall") {
    2 * (2 * n + 5) / (9 * n * (n - 1))
  } else {
    1 / (n - 1)
  }
  list(
    support = support, pmf = pmf, mean = mu,
    var = sum(support^2 * pmf) - mu^2, var_formula = vf,
    statistic = statistic, n = n,
    method = "Exact rank-correlation null by enumeration (Chs. 11.2-11.3)"
  )
}

#' Asymptotic relative efficiencies of the classical location tests
#'
#' Table 13.3.1 (PDF-verified, printed p. 492) plus the efficacy
#' integrals re-deriving it from the density, the Hodges-Lehmann
#' bounds, and the scale-test values of Sec. 13.3.3. Mirrors the
#' morie.fn gb_are*/gb_ar* family.
#'
#' NOTE the Mood value: ARE(Mood, F | normal) = 15/(2 pi^2) = 0.760
#' (the book's own e(M_N) derivation), not the 3/pi a placeholder
#' once claimed.
#'
#' @param distribution "uniform", "normal", "logistic" or
#'   "double_exponential".
#' @return list: wilcoxon_vs_t, sign_vs_t, sign_vs_wilcoxon, derived
#'   (same three from numerical integration), hl_bound_wilcoxon
#'   (0.864), are_mood_f, are_klotz_f.
#' @references Gibbons & Chakraborti (2021), Table 13.3.1,
#'   Sec. 13.3.3. Hodges & Lehmann (1956), Ann. Math. Statist. 27(2),
#'   324-335.
#' @examples
#' morie_are_nonparametric("normal")$wilcoxon_vs_t
#' @export
morie_are_nonparametric <- function(distribution = "normal") {
  tab <- list(
    uniform = c(w = 1, s = 1 / 3, sw = 1 / 3),
    normal = c(w = 3 / pi, s = 2 / pi, sw = 2 / 3),
    logistic = c(w = pi^2 / 9, s = pi^2 / 12, sw = 3 / 4),
    double_exponential = c(w = 1.5, s = 2, sw = 4 / 3)
  )
  distribution <- match.arg(distribution, names(tab))
  dens <- switch(distribution,
    uniform = function(x) stats::dunif(x, -sqrt(3), sqrt(3)),
    normal = stats::dnorm,
    logistic = stats::dlogis,
    double_exponential = function(x) 0.5 * exp(-abs(x))
  )
  m2 <- stats::integrate(function(x) x^2 * dens(x), -Inf, Inf,
    rel.tol = 1e-10
  )$value
  if2 <- stats::integrate(function(x) dens(x)^2, -Inf, Inf,
    rel.tol = 1e-10
  )$value
  f0 <- dens(0)
  ex <- tab[[distribution]]
  list(
    wilcoxon_vs_t = as.numeric(ex["w"]), sign_vs_t = as.numeric(ex["s"]),
    sign_vs_wilcoxon = as.numeric(ex["sw"]),
    derived = list(
      wilcoxon_vs_t = 12 * m2 * if2^2, sign_vs_t = 4 * m2 * f0^2,
      sign_vs_wilcoxon = f0^2 / (3 * if2^2)
    ),
    hl_bound_wilcoxon = 0.864,
    are_mood_f = 15 / (2 * pi^2), are_klotz_f = 1,
    distribution = distribution,
    method = "Gibbons Table 13.3.1 + efficacy re-derivation"
  )
}

#' Order-statistic laws: median, coverage, binomial-beta identity
#'
#' Bundles the Ch. 2 exact results mirrored from gb_med/gb2111c/
#' gb2431/gb2311: the odd-n sample-median CDF I_F(m+1, m+1), the
#' elementary-coverage law Beta(1, n), the binomial-tail =
#' incomplete-beta identity, and the pointwise EDF moments.
#'
#' @param n sample size.
#' @param F_x CDF value(s) for the median CDF / EDF moments.
#' @param t,r arguments of the binomial-beta identity (optional).
#' @return list: median_cdf (odd n only, else NULL), coverage_mean,
#'   coverage_var, edf_mean, edf_var, identity_lhs/rhs (if t, r
#'   given).
#' @references Gibbons & Chakraborti (2021), Chs. 2.3, 2.7.1, 2.11.1,
#'   Corollary 2.4.3.1.
#' @examples
#' morie_order_statistic_laws(11, F_x = 0.5)$median_cdf
#' @export
morie_order_statistic_laws <- function(n, F_x = NULL, t = NULL, r = NULL) {
  n <- as.integer(n)
  if (n < 1L) stop("n must be at least 1.", call. = FALSE)
  out <- list(
    coverage_mean = 1 / (n + 1),
    coverage_var = n / ((n + 1)^2 * (n + 2)),
    n = n, method = "Order-statistic exact laws (Gibbons Ch. 2)"
  )
  if (!is.null(F_x)) {
    Fv <- as.numeric(F_x)
    if (any(Fv < 0 | Fv > 1)) stop("F_x must lie in [0, 1].", call. = FALSE)
    out$edf_mean <- Fv
    out$edf_var <- Fv * (1 - Fv) / n
    if (n %% 2L == 1L) {
      m <- (n - 1L) %/% 2L
      out$median_cdf <- stats::pbeta(Fv, m + 1, m + 1)
    } else {
      out$median_cdf <- NULL # even n averages two order statistics
    }
  }
  if (!is.null(t) && !is.null(r)) {
    t <- as.numeric(t)
    r <- as.integer(r)
    if (!(t >= 0 && t <= 1)) stop("t must lie in [0, 1].", call. = FALSE)
    if (!(r >= 1L && r <= n)) stop("need 1 <= r <= n.", call. = FALSE)
    i <- r:n
    out$identity_lhs <- sum(choose(n, i) * t^i * (1 - t)^(n - i))
    out$identity_rhs <- stats::pbeta(t, r, n - r + 1)
  }
  out
}
