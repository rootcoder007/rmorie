# Rao-Wu rescaled survey bootstrap.
# Source: Rao, Wu & Yue (1992), Survey Methodology 18(2), 209-217,
# Eqs. 2.1-2.3, 3.4, 3.5
# (fetched-wave3/rao-wu-yue-1992-survey-bootstrap.pdf; formulas read
# from the rendered pages -- the scan has no text layer).  Mirrors
# Python morie.fn.bootss exactly: the shared SplitMix64 stream
# (.ghc_rng/.ghc_unif) drives cluster selection draw for draw.

#' Rao-Wu-Yue rescaled bootstrap for stratified cluster samples
#'
#' Within each stratum draw m_h of the n_h clusters with replacement
#' and rescale the survey weights by Eq. 3.4:
#' w* = \[(1 - r) + r (n_h/m_h) m*\] w with r = sqrt(m_h/(n_h - 1));
#' the bootstrap variance is (1/B) sum_b (theta*_b - theta_hat)^2
#' (Eq. 3.5).  m_h = n_h - 1 (default) keeps all weights
#' non-negative; for n_h = 2 a selected cluster doubles and the other
#' zeroes, as printed in the paper.
#'
#' @param y Numeric observations (ultimate units).
#' @param weights Positive survey weights.
#' @param strata Stratum label per observation.
#' @param clusters Cluster (PSU) label per observation.
#' @param statistic Function(y, w) -> scalar; default the weighted
#'   total sum(w * y).
#' @param B Number of bootstrap replicates.
#' @param m Clusters drawn per stratum (scalar or named list);
#'   default n_h - 1.
#' @param seed SplitMix64 seed (mirrors the Python arm).
#' @return A list with elements \code{estimate}, \code{variance},
#'   \code{se}, \code{replicates}, \code{B}, \code{n_strata},
#'   \code{seed}, \code{method}.
#' @references Rao, J. N. K., Wu, C. F. J. and Yue, K. (1992). Some
#'   recent work on resampling methods for complex surveys. Survey
#'   Methodology, 18(2), 209-217.  Rao, J. N. K. and Wu, C. F. J.
#'   (1988). Resampling inference with complex survey data. JASA,
#'   83, 231-241.
#' @export
morie_bootss <- function(y, weights, strata, clusters,
                         statistic = NULL, B = 200, m = NULL,
                         seed = 0) {
  yv <- as.numeric(y)
  wv <- as.numeric(weights)
  n <- length(yv)
  if (length(wv) != n || length(strata) != n || length(clusters) != n ||
      n < 2) {
    stop("y, weights, strata, clusters must be paired")
  }
  if (any(wv <= 0)) stop("weights must be positive")
  if (is.null(statistic)) statistic <- function(yy, ww) sum(ww * yy)
  hs <- as.character(strata)
  cs <- as.character(clusters)
  strat_order <- unique(hs)
  clus <- list()
  for (h in strat_order) {
    idx_h <- which(hs == h)
    cl_names <- unique(cs[idx_h])
    if (length(cl_names) < 2) stop("every stratum needs >= 2 clusters")
    clus[[h]] <- lapply(cl_names, function(cn) idx_h[cs[idx_h] == cn])
  }
  mh <- list()
  for (h in strat_order) {
    nh <- length(clus[[h]])
    mh[[h]] <- if (is.null(m)) nh - 1 else if (is.list(m)) {
      as.integer(m[[h]])
    } else {
      as.integer(m)
    }
    if (mh[[h]] < 1 || mh[[h]] > nh - 1) {
      stop("need 1 <= m_h <= n_h - 1 for non-negative weights")
    }
  }
  theta <- as.numeric(statistic(yv, wv))
  e <- .ghc_rng(seed)
  reps <- numeric(B)
  for (b in seq_len(B)) {
    wb <- wv
    for (h in strat_order) {
      nh <- length(clus[[h]])
      m_h <- mh[[h]]
      counts <- integer(nh)
      for (d in seq_len(m_h)) {
        pick <- min(floor(.ghc_unif(e, 1) * nh), nh - 1) + 1
        counts[pick] <- counts[pick] + 1L
      }
      root <- sqrt(m_h / (nh - 1))
      for (ci in seq_len(nh)) {
        factor <- (1 - root) + root * (nh / m_h) * counts[ci]
        wb[clus[[h]][[ci]]] <- wv[clus[[h]][[ci]]] * factor
      }
    }
    reps[b] <- as.numeric(statistic(yv, wb))
  }
  v <- mean((reps - theta)^2)
  list(estimate = theta, variance = v, se = sqrt(v),
       replicates = reps, B = as.integer(B),
       n_strata = length(strat_order), seed = seed,
       method = "Rao-Wu-Yue rescaled bootstrap (Eqs. 3.4-3.5)")
}
