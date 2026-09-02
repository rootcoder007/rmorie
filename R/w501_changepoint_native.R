# SPDX-License-Identifier: AGPL-3.0-or-later
#
# w5_01 changepoint family: Pelt, Chgseg, Binseg, EDivisive,
# KernelCusum. Bit-identical mirrors of src/morie/fn/{pelt,chgseg,
# binseg,e_div,kcusum}.py. Anchors: changepoint::cpt.mean/cpt.meanvar
# (PELT and BinSeg), ecp::e.divisive locations, and the closed-form
# linear-kernel KFDR.

#' .w501_cost_tables
#'
#' Part of the w501_changepoint_native implementation; see the file
#' header for the source it follows.
#'
#' @param x A vector; its length is taken.
#' @return A list with \code{cs}, \code{css}.
#' @export
.w501_cost_tables <- function(x) {
  n <- length(x)
  cs <- c(0, cumsum(x))
  css <- c(0, cumsum(x * x))
  list(cs = cs, css = css)
}

#' 0-based half-open [a, b): R indices a+1 .. b
#'
#' Part of the w501_changepoint_native implementation; see the file
#' header for the source it follows.
#'
#' @param tb A list; the body reads \code{$cs}, \code{$css} from it.
#' @param a Numeric; combined arithmetically in the body.
#' @param b Numeric; combined arithmetically in the body.
#' @param cost One of \code{"mean"}, \code{"meanvar"}.
#' @return Nothing; this branch always raises.
#' @export
.w501_seg_cost <- function(tb, a, b, cost) {
  # 0-based half-open [a, b): R indices a+1 .. b
  nl <- b - a
  s <- tb$cs[b + 1] - tb$cs[a + 1]
  ssdev <- tb$css[b + 1] - tb$css[a + 1] - s * s / nl
  if (cost == "mean") return(ssdev)
  if (cost == "meanvar") {
    sig <- ssdev / nl
    if (sig <= 1e-300) sig <- 1e-300
    return(nl * (log(2 * pi) + log(sig) + 1))
  }
  stop("cost must be 'mean' or 'meanvar'", call. = FALSE)
}

#' .w501_pelt_core
#'
#' Part of the w501_changepoint_native implementation; see the file
#' header for the source it follows.
#'
#' @param x A vector; its length is taken.
#' @param cost Passed to \code{.w501_seg_cost}.
#' @param penalty See Usage.
#' @param min_seglen Passed to \code{seq}. Defaults to \code{1L}.
#' @return A list with \code{taus}, \code{Fn}.
#' @export
.w501_pelt_core <- function(x, cost, penalty, min_seglen = 1L) {
  n <- length(x)
  tb <- .w501_cost_tables(x)
  beta <- penalty
  F <- rep(0, n + 1)
  F[1] <- -beta
  cp <- rep(0L, n + 1)
  Rset <- c(0L)
  K <- 0
  for (t in seq(min_seglen, n)) {
    best <- Inf; barg <- 0L
    for (tau in Rset) {
      if (t - tau < min_seglen) next
      v <- F[tau + 1] + .w501_seg_cost(tb, tau, t, cost) + beta
      if (v < best) { best <- v; barg <- tau }
    }
    F[t + 1] <- best
    cp[t + 1] <- barg
    keep <- vapply(Rset, function(tau) {
      (t - tau < min_seglen) ||
        (F[tau + 1] + .w501_seg_cost(tb, tau, t, cost) + K <= F[t + 1])
    }, logical(1))
    Rset <- c(Rset[keep], t)
  }
  taus <- integer(0)
  t <- n
  while (cp[t + 1] > 0L) {
    taus <- c(cp[t + 1], taus)
    t <- cp[t + 1]
  }
  list(taus = taus, Fn = F[n + 1])
}

#' PELT changepoint detection (pruned exact linear time)
#'
#' Minimises the penalised segmentation objective (Killick, Fearnhead
#' and Eckley 2012, eq 1 with f(m) = m, eq 3) by the Optimal
#' Partitioning recursion with PELT pruning (their Theorem 3.1,
#' Algorithm 2; pruning constant K = 0 for log-likelihood costs).
#' Cost "mean" is the Normal change-in-mean cost (sum of squared
#' deviations from the segment mean, unit variance); "meanvar" is the
#' Normal change in mean and variance,
#' \eqn{n_l (\log 2\pi + \log \hat\sigma^2 + 1)}.
#'
#' @param x Numeric series.
#' @param cost "mean" or "meanvar".
#' @param penalty Penalty beta; default p log(n) (SIC), p = 1 for
#'   "mean", 2 for "meanvar".
#' @param min_seglen Minimum segment length.
#' @return List with \code{changepoints} (1-based last index of each
#'   segment except the final), \code{n_changepoints},
#'   \code{objective}, \code{penalty}, \code{segment_means}.
#' @references Killick, R., Fearnhead, P. and Eckley, I. A. (2012),
#'   Optimal detection of changepoints with a linear computational
#'   cost, Journal of the American Statistical Association 107(500),
#'   1590-1598 (arXiv:1101.1438), equations 1-5, Algorithms 1-2.
#'   Source: fetched-wave3/killick-fearnhead-eckley-2012-pelt-optimal-
#'   changepoint-linear-cost.pdf
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Pelt(V)
Pelt <- function(x, cost = "mean", penalty = NULL, min_seglen = 1L) {
  x <- as.numeric(x)
  n <- length(x)
  if (cost == "meanvar" && min_seglen < 2L) {
    # sigma2_hat = 0 on singletons makes the likelihood unbounded; a
    # minimum segment length of 2 is required (changepoint convention).
    min_seglen <- 2L
  }
  if (n < 2L * min_seglen) stop("series too short", call. = FALSE)
  if (is.null(penalty)) {
    p <- if (cost == "mean") 1 else 2
    penalty <- p * log(n)
  }
  fit <- .w501_pelt_core(x, cost, penalty, min_seglen)
  bounds <- c(0L, fit$taus, n)
  seg_means <- vapply(seq_len(length(bounds) - 1L), function(i) {
    mean(x[(bounds[i] + 1L):bounds[i + 1L]])
  }, numeric(1))
  list(changepoints = as.integer(fit$taus),
       n_changepoints = length(fit$taus),
       objective = fit$Fn,
       penalty = penalty,
       segment_means = seg_means,
       estimate = as.integer(fit$taus),
       n = n,
       method = "PELT (Killick-Fearnhead-Eckley 2012)")
}

#' Penalised mean-change segmentation via PELT
#'
#' The eq (3) optimal-partitioning objective of Killick, Fearnhead and
#' Eckley (2012) solved exactly by PELT (their Algorithm 2) with the
#' Normal change-in-mean cost. Thin specialisation of \code{Pelt}.
#'
#' @param y Numeric series.
#' @param penalty Penalty beta; default log(n).
#' @return As \code{Pelt}.
#' @references Killick, R., Fearnhead, P. and Eckley, I. A. (2012),
#'   Journal of the American Statistical Association 107(500),
#'   1590-1598 (arXiv:1101.1438), eq 3, Algorithm 2. Source:
#'   fetched-wave3/killick-fearnhead-eckley-2012-pelt-optimal-
#'   changepoint-linear-cost.pdf
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Chgseg(V)
Chgseg <- function(y, penalty = NULL) {
  out <- Pelt(y, cost = "mean", penalty = penalty)
  out$method <- "PELT mean-change segmentation (Killick et al. 2012, eq 3)"
  out
}

#' Binary segmentation changepoint search
#'
#' Greedy recursive bisection: a split at tau is accepted when
#' C(left) + C(right) + beta < C(segment) (Killick, Fearnhead and
#' Eckley 2012, Sec. 2.1 eq 2); at each step the split with the
#' largest cost reduction over all current segments is taken, up to K
#' changepoints. Method originally due to Scott and Knott (1974).
#'
#' @param x Numeric series.
#' @param K Maximum number of changepoints.
#' @param cost "mean" or "meanvar".
#' @param penalty Penalty beta in eq 2 (default 0).
#' @param min_seglen Minimum segment length.
#' @return List with \code{changepoints} (sorted), \code{order}
#'   (detection order), \code{improvements}, \code{n_changepoints},
#'   \code{segment_means}.
#' @references Killick, R., Fearnhead, P. and Eckley, I. A. (2012),
#'   Journal of the American Statistical Association 107(500),
#'   1590-1598 (arXiv:1101.1438), Sec. 2.1 eq 2; Scott, A. J. and
#'   Knott, M. (1974), A cluster analysis method for grouping means in
#'   the analysis of variance, Biometrics 30(3), 507-512. Source:
#'   fetched-wave3/killick-fearnhead-eckley-2012-pelt-optimal-
#'   changepoint-linear-cost.pdf
#' @export
#' @examples
#' Binseg(x = c(1, 2, 3, 4, 5, 6, 7, 8), K = 5L)
Binseg <- function(x, K, cost = "mean", penalty = 0, min_seglen = 1L) {
  x <- as.numeric(x)
  n <- length(x)
  K <- as.integer(K)
  if (n < 2L * min_seglen) stop("series too short", call. = FALSE)
  tb <- .w501_cost_tables(x)
  best_split <- function(a, b) {
    best_gain <- -Inf; best_tau <- -1L
    base <- .w501_seg_cost(tb, a, b, cost)
    for (tau in seq(a + min_seglen, b - min_seglen)) {
      g <- base - (.w501_seg_cost(tb, a, tau, cost) +
                     .w501_seg_cost(tb, tau, b, cost)) - penalty
      if (g > best_gain) { best_gain <- g; best_tau <- tau }
    }
    list(tau = best_tau, gain = best_gain)
  }
  segments <- list(c(0L, n))
  ord <- integer(0)
  gains <- numeric(0)
  while (length(ord) < K) {
    cand <- NULL
    for (si in seq_along(segments)) {
      a <- segments[[si]][1]; b <- segments[[si]][2]
      if (b - a < 2L * min_seglen) next
      sp <- best_split(a, b)
      if (sp$tau > 0L && (is.null(cand) || sp$gain > cand$gain)) {
        cand <- list(si = si, a = a, b = b, gain = sp$gain, tau = sp$tau)
      }
    }
    if (is.null(cand) || cand$gain <= 0) break
    ord <- c(ord, cand$tau)
    gains <- c(gains, cand$gain)
    segments[[cand$si]] <- NULL
    segments <- c(segments, list(c(cand$a, cand$tau)),
                  list(c(cand$tau, cand$b)))
  }
  taus <- sort(ord)
  bounds <- c(0L, taus, n)
  seg_means <- vapply(seq_len(length(bounds) - 1L), function(i) {
    mean(x[(bounds[i] + 1L):bounds[i + 1L]])
  }, numeric(1))
  list(changepoints = as.integer(taus),
       order = as.integer(ord),
       improvements = gains,
       n_changepoints = length(taus),
       segment_means = seg_means,
       estimate = as.integer(taus),
       n = n,
       method = "Binary segmentation (Scott-Knott 1974; Killick et al. 2012 Sec. 2.1)")
}

#' Z: matrix with observations in rows
#'
#' Part of the w501_changepoint_native implementation; see the file
#' header for the source it follows.
#'
#' @param Z A matrix; indexed by row and column.
#' @param alpha Numeric; combined arithmetically in the body.
#' @return The value of \code{D}, as built in the body.
#' @export
.w501_pairwise_alpha <- function(Z, alpha) {
  # Z: matrix with observations in rows
  n <- nrow(Z)
  D <- matrix(0, n, n)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (j > i) {
        d <- sqrt(sum((Z[i, ] - Z[j, ])^2))
        D[i, j] <- d^alpha
        D[j, i] <- D[i, j]
      }
    }
  }
  D
}

#' .w501_prefix2d
#'
#' Part of the w501_changepoint_native implementation; see the file
#' header for the source it follows.
#'
#' @param D A matrix; indexed by row and column.
#' @return The value of \code{P}, as built in the body.
#' @export
.w501_prefix2d <- function(D) {
  n <- nrow(D)
  P <- matrix(0, n + 1, n + 1)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      P[i + 1, j + 1] <- P[i + 1, j] + P[i, j + 1] - P[i, j] + D[i, j]
    }
  }
  P
}

#' .w501_block
#'
#' Part of the w501_changepoint_native implementation; see the file
#' header for the source it follows.
#'
#' @param P A matrix; indexed by row and column.
#' @param a1 Numeric; combined arithmetically in the body.
#' @param b1 Numeric; combined arithmetically in the body.
#' @param a2 Numeric; combined arithmetically in the body.
#' @param b2 Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.w501_block <- function(P, a1, b1, a2, b2) {
  P[b1 + 1, b2 + 1] - P[a1 + 1, b2 + 1] - P[b1 + 1, a2 + 1] + P[a1 + 1, a2 + 1]
}

#' .w501_qhat
#'
#' Part of the w501_changepoint_native implementation; see the file
#' header for the source it follows.
#'
#' @param P Passed to \code{.w501_block}.
#' @param a Numeric; combined arithmetically in the body.
#' @param tau Numeric; combined arithmetically in the body.
#' @param kappa Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.w501_qhat <- function(P, a, tau, kappa) {
  n1 <- tau - a
  m1 <- kappa - tau
  between <- .w501_block(P, a, tau, tau, kappa)
  withinX <- .w501_block(P, a, tau, a, tau) / 2
  withinY <- .w501_block(P, tau, kappa, tau, kappa) / 2
  e <- (2 / (n1 * m1)) * between -
    withinX / (n1 * (n1 - 1) / 2) -
    withinY / (m1 * (m1 - 1) / 2)
  (n1 * m1 / (n1 + m1)) * e
}

#' .w501_best_split
#'
#' Part of the w501_changepoint_native implementation; see the file
#' header for the source it follows.
#'
#' @param P Passed to \code{.w501_qhat}.
#' @param a Numeric; combined arithmetically in the body.
#' @param b Numeric; combined arithmetically in the body.
#' @param min_size Numeric; combined arithmetically in the body.
#' @return A list with \code{q}, \code{tau}, \code{kappa}.
#' @export
.w501_best_split <- function(P, a, b, min_size) {
  best_q <- -Inf; best_tau <- -1L; best_kappa <- -1L
  for (tau in seq(a + min_size, b - min_size)) {
    for (kappa in seq(tau + min_size, b)) {
      q <- .w501_qhat(P, a, tau, kappa)
      if (q > best_q) { best_q <- q; best_tau <- tau; best_kappa <- kappa }
    }
  }
  list(q = best_q, tau = best_tau, kappa = best_kappa)
}

#' Fisher-Yates within each 0-based half-open cluster, mirroring the
#'
#' Python arm swap-for-swap.
#'
#' @param ord A vector; indexed elementwise.
#' @param clusters See Usage.
#' @param us A vector; indexed elementwise.
#' @param pos Numeric; combined arithmetically in the body.
#' @return A list with \code{ord}, \code{pos}.
#' @export
.w501_shuffle_within <- function(ord, clusters, us, pos) {
  # Fisher-Yates within each 0-based half-open cluster, mirroring the
  # Python arm swap-for-swap.
  for (cl in clusters) {
    a <- cl[1]; b <- cl[2]
    L <- b - a
    if (L > 1L) {
      for (i in seq(L - 1L, 1L)) {
        j <- floor(us[pos] * (i + 1))
        if (j > i) j <- i
        pos <- pos + 1L
        tmp <- ord[a + i + 1L]
        ord[a + i + 1L] <- ord[a + j + 1L]
        ord[a + j + 1L] <- tmp
      }
    }
  }
  list(ord = ord, pos = pos)
}

#' E-divisive multiple changepoint estimation (energy distance)
#'
#' Matteson and James (2014): empirical divergence \eqn{\hat{E}} of
#' eq 5 (energy statistic on Euclidean distances raised to alpha),
#' scaled statistic \eqn{\hat{Q} = \frac{mn}{m+n}\hat{E}} (eq 6),
#' single-changepoint search over (tau, kappa) (eq 7), hierarchical
#' application within existing clusters (Sec. 2.3, eq 8), and the
#' within-cluster permutation stopping rule of Sec. 2.4 with p-value
#' (number of permuted statistics at least the observed) / (R + 1),
#' stopping when p exceeds \code{sig}. Permutations use the native
#' Philox stream, bit-identical to the Python arm.
#'
#' @param x Numeric vector or matrix with observations in rows.
#' @param sig Stopping significance level p0.
#' @param R Number of permutations.
#' @param alpha Divergence index in (0, 2).
#' @param min_size Minimum segment size (at least 2).
#' @param max_cp Optional cap on the number of changepoints.
#' @param seed Philox seed.
#' @return List with \code{changepoints} (detection order),
#'   \code{changepoints_sorted}, \code{p_values}, \code{q_stats},
#'   \code{n_changepoints}.
#' @references Matteson, D. S. and James, N. A. (2014), A
#'   nonparametric approach for multiple change point analysis of
#'   multivariate data, Journal of the American Statistical
#'   Association 109(505), 334-345 (arXiv:1306.4933), equations 4-8,
#'   Sections 2.1-2.4. Source: fetched-wave3/matteson-james-2014-
#'   edivisive-nonparametric-changepoint.pdf
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' EDivisive(V)
EDivisive <- function(x, sig = 0.05, R = 199L, alpha = 1, min_size = 2L,
                      max_cp = NULL, seed = 20260809) {
  Z <- if (is.matrix(x)) x else matrix(as.numeric(x), ncol = 1)
  n <- nrow(Z)
  if (n < 2L * min_size) stop("series too short", call. = FALSE)
  if (alpha <= 0 || alpha >= 2) stop("alpha must be in (0, 2)", call. = FALSE)
  D <- .w501_pairwise_alpha(Z, alpha)
  P <- .w501_prefix2d(D)
  cps <- integer(0)
  pvals <- numeric(0)
  qstats <- numeric(0)
  repeat {
    if (!is.null(max_cp) && length(cps) >= max_cp) break
    st <- sort(cps)
    clusters <- lapply(seq_len(length(st) + 1L), function(i) {
      c(c(0L, st)[i], c(st, n)[i])
    })
    best_q <- -Inf; best_tau <- -1L
    for (cl in clusters) {
      if (cl[2] - cl[1] >= 2L * min_size) {
        sp <- .w501_best_split(P, cl[1], cl[2], min_size)
        if (sp$q > best_q) { best_q <- sp$q; best_tau <- sp$tau }
      }
    }
    if (best_tau < 0L) break
    needed <- 0L
    for (cl in clusters) if (cl[2] - cl[1] > 1L) needed <- needed + cl[2] - cl[1] - 1L
    count_ge <- 0L
    for (r in seq_len(R)) {
      us <- .morie_random_uniform(needed, seed = seed, stream = r)
      sh <- .w501_shuffle_within(seq_len(n) - 1L, clusters, us, 1L)
      perm <- sh$ord
      Zp <- Z[perm + 1L, , drop = FALSE]
      Dp <- .w501_pairwise_alpha(Zp, alpha)
      Pp <- .w501_prefix2d(Dp)
      bq <- -Inf
      for (cl in clusters) {
        if (cl[2] - cl[1] >= 2L * min_size) {
          sp <- .w501_best_split(Pp, cl[1], cl[2], min_size)
          if (sp$q > bq) bq <- sp$q
        }
      }
      if (bq >= best_q) count_ge <- count_ge + 1L
    }
    p <- count_ge / (R + 1)
    pvals <- c(pvals, p)
    qstats <- c(qstats, best_q)
    if (p > sig) break
    cps <- c(cps, best_tau)
  }
  list(changepoints = as.integer(cps),
       changepoints_sorted = as.integer(sort(cps)),
       p_values = pvals,
       q_stats = qstats,
       n_changepoints = length(cps),
       estimate = as.integer(sort(cps)),
       n = n,
       method = "E-divisive (Matteson-James 2014)")
}

#' Kernel change-point analysis (KFDR running-maximum scan)
#'
#' Harchaoui, Moulines and Bach (2008): for each candidate k the
#' kernel Fisher discriminant ratio
#' \eqn{KFDR = \frac{k(n-k)}{n} \| (\Sigma_W + \gamma I)^{-1/2}
#' (\hat\mu_{k+1:n} - \hat\mu_{1:k}) \|^2} with within-class operator
#' \eqn{n \Sigma_W = k \hat\Sigma_{1:k} + (n-k) \hat\Sigma_{k+1:n}},
#' studentised as \eqn{T(k) = (KFDR - d_1)/\sqrt{2 d_2}} with
#' \eqn{d_1 = tr[(\Sigma_W + \gamma I)^{-1}\Sigma_W]},
#' \eqn{d_2 = tr[(\Sigma_W + \gamma I)^{-2}\Sigma_W^2]}; the estimate
#' is the running maximum over k (their Sec. 2-3). Operators are
#' represented on the span of the mapped sample via the Gram-matrix
#' eigendecomposition; the reported scalars are basis-invariant.
#'
#' @param x Numeric vector or matrix with observations in rows.
#' @param kernel "gaussian" (median-heuristic bandwidth default) or
#'   "linear".
#' @param threshold Optional decision threshold for max T(k).
#' @param gamma Ridge regularisation.
#' @param bandwidth Gaussian bandwidth; default median heuristic.
#' @param kmin,kmax Scan interval, 1 < k < n.
#' @return List with \code{estimate} (k_hat), \code{statistic}
#'   (max T), \code{kfdr}, \code{d1}, \code{d2}, \code{T},
#'   \code{detected} (if threshold given), \code{bandwidth},
#'   \code{gamma}.
#' @references Harchaoui, Z., Moulines, E. and Bach, F. R. (2008),
#'   Kernel change-point analysis, Advances in Neural Information
#'   Processing Systems 21, 609-616. Section 3 (KFDR, d1/d2,
#'   scan statistic), Section 2 (running maximum strategy),
#'   Corollary 2. Source: fetched-wave3/harchaoui-moulines-bach-2008-
#'   kernel-changepoint-analysis-nips.pdf
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' KernelCusum(V)
KernelCusum <- function(x, kernel = "gaussian", threshold = NULL,
                        gamma = 0.1, bandwidth = NULL, kmin = 2L,
                        kmax = NULL) {
  Z <- if (is.matrix(x)) x else matrix(as.numeric(x), ncol = 1)
  n <- nrow(Z)
  if (n < 4L) stop("need n >= 4", call. = FALSE)
  if (is.null(kmax)) kmax <- n - 2L
  kmin <- as.integer(kmin); kmax <- as.integer(kmax)
  if (!(kmin > 1L && kmin <= kmax && kmax < n)) {
    stop("need 1 < kmin <= kmax < n", call. = FALSE)
  }
  bw <- NULL
  if (kernel == "linear") {
    K <- Z %*% t(Z)
  } else if (kernel == "gaussian") {
    d2m <- as.matrix(stats::dist(Z))^2
    if (is.null(bandwidth)) {
      dv <- sqrt(d2m[upper.tri(d2m)])
      bandwidth <- stats::median(dv)
      if (bandwidth <= 0) bandwidth <- 1
    }
    bw <- bandwidth
    K <- exp(-d2m / (2 * bandwidth * bandwidth))
  } else {
    stop("kernel must be 'linear' or 'gaussian'", call. = FALSE)
  }
  eg <- eigen(K, symmetric = TRUE)
  lmax <- max(abs(eg$values))
  if (lmax <= 0) lmax <- 1
  keep <- which(eg$values > 1e-12 * lmax)
  r <- length(keep)
  Cm <- t(eg$vectors[, keep, drop = FALSE] *
            rep(sqrt(eg$values[keep]), each = n))
  Ts <- numeric(0); kf_all <- numeric(0)
  d1_all <- numeric(0); d2_all <- numeric(0)
  for (k in seq(kmin, kmax)) {
    mu1 <- rowMeans(Cm[, 1:k, drop = FALSE])
    mu2 <- rowMeans(Cm[, (k + 1):n, drop = FALSE])
    delta <- mu2 - mu1
    A1 <- Cm[, 1:k, drop = FALSE] - mu1
    A2 <- Cm[, (k + 1):n, drop = FALSE] - mu2
    S1 <- tcrossprod(A1) / k
    S2 <- tcrossprod(A2) / (n - k)
    Sw <- (k * S1 + (n - k) * S2) / n
    M <- Sw + gamma * diag(r)
    sol <- solve(M, delta)
    kfdr <- (k * (n - k) / n) * sum(delta * sol)
    d1 <- sum(diag(solve(M, Sw)))
    d2 <- sum(diag(solve(M, solve(M, Sw %*% Sw))))
    Ts <- c(Ts, (kfdr - d1) / sqrt(2 * d2))
    kf_all <- c(kf_all, kfdr)
    d1_all <- c(d1_all, d1)
    d2_all <- c(d2_all, d2)
  }
  ib <- 1L
  for (i in seq_along(Ts)) if (Ts[i] > Ts[ib]) ib <- i
  khat <- kmin + ib - 1L
  out <- list(estimate = as.integer(khat),
              statistic = Ts[ib],
              kfdr = kf_all[ib],
              d1 = d1_all[ib],
              d2 = d2_all[ib],
              T = Ts,
              kmin = kmin, kmax = kmax,
              gamma = gamma,
              bandwidth = bw,
              n = n,
              method = "Kernel change-point analysis (Harchaoui-Moulines-Bach 2008)")
  if (!is.null(threshold)) {
    out$threshold <- threshold
    out$detected <- Ts[ib] > threshold
  }
  out
}
