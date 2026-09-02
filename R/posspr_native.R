# P?lya urn predictive, and the density it implies.
# Sources: Muller, P. & Quintana, F. A. (2004) "Nonparametric
# Bayesian Data Analysis", Statistical Science 19(1), 95-110,
# doi:10.1214/088342304000000017 (the review that organises DP
# inference problems and the predictive rules used for posterior
# simulation); Blackwell, D. & MacQueen, J. B. (1973) "Ferguson
# Distributions via Polya Urn Schemes", The Annals of Statistics
# 1(2), 353-355, doi:10.1214/aos/1176342372 (the urn
# representation itself); Ferguson, T. S. (1973) "A Bayesian
# Analysis of Some Nonparametric Problems", The Annals of
# Statistics 1(2), 209-230, doi:10.1214/aos/1176342360 (the
# prior); Escobar, M. D. & West, M. (1995) "Bayesian Density
# Estimation and Inference Using Mixtures", JASA 90(430),
# 577-588, doi:10.1080/01621459.1995.10476550 (the DP mixture
# density estimate).
#
# Native implementation mirroring Python morie.fn.posspr
# exactly: the same urn weights, the same per-step inverse-CDF
# over the existing clusters, the same new-cluster fallback
# when the uniform lands in the alpha/(alpha+n) interval, and
# the same predictive density as a sum over occupied clusters
# plus one prior-predictive term. Random draws go through the
# shared SplitMix64 generator so the R arm and a Python arm
# fed the same rng produce the same stream.

#' Polya urn weights
#'
#' Returns the urn's mixture weights at the current state.
#' The new-cluster weight is \code{alpha/(alpha+n)} exactly,
#' and is returned separately because it IS the model's answer
#' to "how likely is something new".
#'
#' @param counts Numeric vector of positive cluster occupancies.
#' @param alpha Concentration, positive.
#' @return A list with \code{existing}, \code{new}, \code{n},
#'   \code{K}, \code{total}, \code{note}.
#' @references Muller, P. & Quintana, F. A. (2004). Statistical
#'   Science, 19(1), 95-110.
#' @export
urn_weights <- function(counts, alpha) {
  if (length(counts) == 0L) c <- numeric(0) else c <- as.numeric(counts)
  a <- as.numeric(alpha)
  if (a <= 0)
    stop("posspr: the concentration must be positive")
  if (length(c) > 0L && any(c <= 0))
    stop("posspr: an occupied cluster must have a positive count")
  n <- sum(c)
  list(existing = c / (a + n),
       new      = a / (a + n),
       n        = n,
       K        = length(c),
       total    = (n + a) / (a + n),
       note     = "repeat in proportion to how often it has already appeared; that is the clustering")
}

#' Draw a Polya urn sequence of cluster labels
#'
#' Repeatedly draws the next cluster label from the urn: with
#' probability \code{alpha/(alpha+n)} a brand new cluster
#' appears, otherwise an existing one is repeated in proportion
#' to its current occupancy. Ties between draws therefore have
#' positive probability, which is what makes the DP a
#' clustering prior at all, and why a DP mixture (not a DP) is
#' used for continuous data.
#'
#' @param n Number of draws, at least 1.
#' @param alpha Concentration, positive.
#' @param seed Seed for the generator shared with the Python arm.
#' @return A list with \code{labels}, \code{counts},
#'   \code{n_clusters}, \code{alpha}, \code{note}.
#' @references Muller, P. & Quintana, F. A. (2004). Statistical
#'   Science, 19(1), 95-110.
#' @export
morie_posspr <- function(n, alpha, seed = 0) {
  a <- as.numeric(alpha)
  N <- as.integer(n)
  if (N < 1L)
    stop("posspr: n must be at least 1")
  e <- .ghc_rng(seed)
  counts <- numeric(0)
  labels <- integer(0)
  for (k in seq_len(N)) {
    if (length(counts) == 0L) {
      counts <- c(counts, 1.0)
      labels <- c(labels, 0L)
    } else {
      w <- urn_weights(counts, a)
      u <- as.numeric(.ghc_unif(e, 1L))
      acc <- 0.0
      chosen <- NA_integer_
      for (j in seq_along(counts)) {
        acc <- acc + w$existing[j]
        if (u <= acc) { chosen <- as.integer(j - 1L)
        break }
      }
      if (is.na(chosen)) {
        counts <- c(counts, 1.0)
        labels <- c(labels, length(counts) - 1L)
      } else {
        counts[chosen + 1L] <- counts[chosen + 1L] + 1.0
        labels <- c(labels, chosen)
      }
    }
  }
  list(labels     = labels,
       counts     = counts,
       n_clusters = length(counts),
       alpha      = a,
       note       = "ties occur with POSITIVE probability, which is why the DP clusters")
}

#' Python name for \code{morie_posspr}
#' @export
#' @noRd
sample_urn <- morie_posspr

#' Predictive density
#'
#' The DP mixture posterior predictive density at each point
#' of \code{grid}: a weighted sum over the occupied clusters
#' of their kernel values, plus one term
#' \code{alpha/(alpha+n) * base_predictive(y)} for a cluster
#' not yet seen. The new-cluster weight is reported separately
#' because it IS the model's stated probability that the next
#' observation is unlike everything seen so far.
#'
#' @param grid Numeric vector of evaluation points.
#' @param cluster_params Parameters of the occupied clusters,
#'   one per entry in \code{counts} (may be any R object the
#'   user's \code{kernel} knows how to consume).
#' @param counts Numeric vector of positive cluster occupancies.
#' @param alpha Concentration, positive.
#' @param kernel Function \code{(y, theta)} returning the kernel
#'   value at \code{y} for cluster parameter \code{theta}.
#' @param base_predictive Function \code{(y)} returning the
#'   prior predictive \code{integrate k(y|theta) dG_0(theta)}.
#' @return A list with \code{estimate}, \code{density},
#'   \code{grid}, \code{new_cluster_weight},
#'   \code{occupied_weights}, \code{K}, \code{n}, \code{method},
#'   \code{note}.
#' @references Muller, P. & Quintana, F. A. (2004). Statistical
#'   Science, 19(1), 95-110.
#' @export
predictive_density <- function(grid, cluster_params, counts, alpha,
                               kernel, base_predictive) {
  w <- urn_weights(counts, alpha)
  out <- numeric(length(grid))
  new_share <- w$new
  existing_w <- w$existing
  Kc <- length(counts)
  if (Kc > 0L) {
    for (i in seq_along(grid)) {
      y <- grid[i]
      v <- new_share * as.numeric(base_predictive(y))
      for (j in seq_len(Kc)) {
        v <- v + existing_w[j] * as.numeric(kernel(y, cluster_params[[j]]))
      }
      out[i] <- v
    }
  } else {
    for (i in seq_along(grid)) {
      out[i] <- new_share * as.numeric(base_predictive(grid[i]))
    }
  }
  list(estimate            = out,
       density             = out,
       grid                = as.numeric(grid),
       new_cluster_weight  = new_share,
       occupied_weights    = existing_w,
       K                   = w$K,
       n                   = w$n,
       method              = "DP mixture posterior predictive; Muller & Quintana (2004)",
       note                = "the weight on the unseen component is exactly alpha/(alpha+n) -- report it rather than bury it")
}

#' Expected number of clusters
#'
#' The sum \code{sum alpha/(alpha+i)} for \code{i = 0..n-1},
#' which grows like \code{alpha log n} -- so the concentration
#' is not a smoothing parameter whose doubling doubles the
#' cluster count.
#'
#' @param n Number of draws, at least 1.
#' @param alpha Concentration, positive.
#' @return A list with \code{expected}, \code{n}, \code{alpha},
#'   \code{log_approximation}, \code{note}.
#' @references Muller, P. & Quintana, F. A. (2004). Statistical
#'   Science, 19(1), 95-110.
#' @export
expected_clusters <- function(n, alpha) {
  a <- as.numeric(alpha)
  N <- as.integer(n)
  if (a <= 0 || N < 1L)
    stop("posspr: need alpha > 0 and n >= 1")
  e <- sum(a / (a + seq_len(N) - 1L))
  list(expected          = e,
       n                 = N,
       alpha             = a,
       log_approximation = a * log(1.0 + N / a),
       note              = "logarithmic in n, so alpha is not a smoothing knob that scales the cluster count linearly")
}

#' Tie probability for two consecutive draws
#'
#' The probability that the second draw from the urn equals
#' the first, \code{1/(1+alpha)}, and the complementary
#' probability that the second draw opens a new cluster. This
#' closed form falls straight out of the urn at \code{n=1} and
#' is the anchor that fails if the two mixture weights in
#' \code{urn_weights} are ever swapped.
#'
#' @param alpha Concentration, positive.
#' @return A list with \code{tie}, \code{new}, \code{alpha}.
#' @references Muller, P. & Quintana, F. A. (2004). Statistical
#'   Science, 19(1), 95-110.
#' @export
tie_probability <- function(alpha) {
  a <- as.numeric(alpha)
  if (a <= 0)
    stop("posspr: the concentration must be positive")
  list(tie   = 1.0 / (1.0 + a),
       new   = a / (1.0 + a),
       alpha = a)
}
