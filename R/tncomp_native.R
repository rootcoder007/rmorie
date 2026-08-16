# morie.fn -- function file (rootcoder007/morie)
# Dissimilarity-based compound selection: MaxMin and MaxSum.
#
# The task.  Pick k compounds from N so that the selection covers the
# collection rather than clumping in whatever region happens to be
# over-represented.  Both algorithms here are greedy and differ only
# in the objective used at each step, with d = 1 - T the Tanimoto
# distance:
#
#   MaxMin:  argmax_{i not in S} min_{j in S} d(i, j)
#   MaxSum:  argmax_{i not in S} sum_{j in S} d(i, j)
#
# Why the difference matters.  MaxSum maximises a total, so a compound
# far from most of the selection wins even if it sits almost on top of
# one member -- the large distances outvote the small one.  MaxMin
# maximises the worst distance, so nothing is added next to something
# already chosen.  Snarey et al. found MaxMin gives better coverage
# of a collection, and the anchor exhibits the mechanism directly:
# on a set with four well-separated groups and a crowd of
# near-duplicates in one of them, MaxMin takes one from each group
# and MaxSum does not.
#
# Neither is optimal -- maximum diversity is NP-hard -- and neither
# is random: both are deterministic given the seed, and the seed is
# reported.
#
# References
# ----------
# Snarey, M., Terrett, N. K., Willett, P. & Wilton, D. J. (1997)
# "Comparison of algorithms for dissimilarity-based compound
# selection", Journal of Molecular Graphics and Modelling 15(6),
# 372-385, doi:10.1016/S1093-3263(98)00008-4.  The MaxMin and
# MaxSum objectives reproduced above, their greedy implementation,
# and the finding that MaxMin covers a collection better.
#
# Willett, P., Barnard, J. M. & Downs, G. M. (1998) "Chemical
# similarity searching", Journal of Chemical Information and
# Computer Sciences 38(6), 983-996, doi:10.1021/ci9800211, for
# the Tanimoto distance the objectives are measured in; see
# morie.fn.sasimi.

OBJECTIVES <- c("maxmin", "maxsum")

#' .tncomp_fingerprint
#'
#' A step of the tncomp_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x A vector; its length is taken.
#' @return One of two values, depending on the branch taken.
#' @export
.tncomp_fingerprint <- function(x) {
  if (is.character(x)) {
    chars <- strsplit(x, "")[[1]]
    which(chars == "1")
  } else if (is.logical(x)) {
    which(x)
  } else if (is.numeric(x)) {
    if (length(x) > 0L && all(x %in% c(0, 1))) {
      which(x == 1)
    } else {
      sort(unique(as.integer(x)))
    }
  } else {
    sort(unique(as.integer(x)))
  }
}

#' .tncomp_tanimoto
#'
#' A step of the tncomp_native implementation. Called by \code{distance_matrix}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param f1 A vector; its length is taken.
#' @param f2 A vector; its length is taken.
#' @return A numeric value.
#' @export
.tncomp_tanimoto <- function(f1, f2) {
  s1 <- length(f1)
  s2 <- length(f2)
  if (s1 == 0L && s2 == 0L) return(0.0)
  intersect_len <- length(intersect(f1, f2))
  union_len <- s1 + s2 - intersect_len
  if (union_len == 0L) return(0.0)
  intersect_len / union_len
}

#' distance_matrix
#'
#' A step of the tncomp_native implementation. Called by \code{.tncomp_select}, \code{diversity}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param fps Iterated over elementwise, with \code{lapply}.
#' @return The value of \code{D}, as built in the body.
#' @export
distance_matrix <- function(fps) {
  F <- lapply(fps, .tncomp_fingerprint)
  n <- length(F)
  if (n < 2L) stop("tncomp: need at least two compounds")
  D <- matrix(0.0, n, n)
  for (i in 1:(n - 1L)) {
    for (j in (i + 1L):n) {
      d <- 1.0 - .tncomp_tanimoto(F[[i]], F[[j]])
      D[i, j] <- d
      D[j, i] <- d
    }
  }
  D
}

#' .tncomp_seed
#'
#' A step of the tncomp_native implementation. Called by \code{.tncomp_select}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param D A matrix; passed to \code{nrow}.
#' @param seed Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @return The value of \code{which.max}.
#' @export
.tncomp_seed <- function(D, seed) {
  n <- nrow(D)
  if (!is.null(seed)) {
    s <- as.integer(seed)
    if (s < 0L || s >= n) {
      stop(sprintf("tncomp: seed %d is not a compound index", s))
    }
    return(s + 1L)
  }
  tot <- colSums(D)
  which.max(tot)
}

#' .tncomp_select
#'
#' A step of the tncomp_native implementation. Called by \code{maxmin_selection}, \code{maxsum_selection}, \code{morie_tncomp}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param fps See Usage.
#' @param k Coerced to integer by the body, with \code{as.integer}.
#' @param objective Compared against \code{"maxmin"}.
#' @param seed Passed to \code{.tncomp_seed}.
#' @param D Defaults to \code{NULL}.
#' @return A list with \code{chosen}, \code{M}.
#' @export
.tncomp_select <- function(fps, k, objective, seed = NULL, D = NULL) {
  if (!(objective %in% OBJECTIVES)) {
    stop(sprintf("tncomp: objective must be one of %s, got %s",
                 paste(OBJECTIVES, collapse = ", "), objective))
  }
  M <- if (is.null(D)) distance_matrix(fps) else D
  n <- nrow(M)
  kk <- as.integer(k)
  if (kk < 1L || kk > n) {
    stop(sprintf("tncomp: k must lie in [1, %d], got %d", n, kk))
  }
  chosen <- .tncomp_seed(M, seed)
  while (length(chosen) < kk) {
    rest <- setdiff(1:n, chosen)
    if (objective == "maxmin") {
      scores <- sapply(rest, function(i) min(M[i, chosen]))
    } else {
      scores <- sapply(rest, function(i) sum(M[i, chosen]))
    }
    scores <- unname(scores)
    max_score <- max(scores)
    candidates <- rest[scores == max_score]
    best <- min(candidates)
    chosen <- c(chosen, best)
  }
  list(chosen = chosen, M = M)
}

#' maxmin_selection
#'
#' A step of the tncomp_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param fps Passed to \code{.tncomp_select}.
#' @param k Passed to \code{.tncomp_select}.
#' @param seed Passed to \code{.tncomp_select}.
#' @return The value of \code{$}.
#' @export
maxmin_selection <- function(fps, k, seed = NULL) {
  .tncomp_select(fps, k, "maxmin", seed)$chosen
}

#' maxsum_selection
#'
#' A step of the tncomp_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param fps Passed to \code{.tncomp_select}.
#' @param k Passed to \code{.tncomp_select}.
#' @param seed Passed to \code{.tncomp_select}.
#' @return The value of \code{$}.
#' @export
maxsum_selection <- function(fps, k, seed = NULL) {
  .tncomp_select(fps, k, "maxsum", seed)$chosen
}

#' diversity
#'
#' A step of the tncomp_native implementation. Called by \code{morie_tncomp}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param fps See Usage.
#' @param subset Coerced to integer by the body, with \code{as.integer}.
#' @param D Defaults to \code{NULL}.
#' @return A list with \code{min_distance}, \code{mean_distance}, \code{max_distance}, \code{n_pairs}.
#' @export
diversity <- function(fps, subset, D = NULL) {
  M <- if (is.null(D)) distance_matrix(fps) else D
  S <- as.integer(subset)
  if (length(S) < 2L) {
    stop("tncomp: diversity needs at least two selected compounds")
  }
  if (length(unique(S)) != length(S)) {
    stop("tncomp: the selection repeats a compound")
  }
  ds <- numeric(0)
  Slen <- length(S)
  for (i in 1:(Slen - 1L)) {
    for (j in (i + 1L):Slen) {
      ds <- c(ds, M[S[i], S[j]])
    }
  }
  list(
    min_distance = min(ds),
    mean_distance = sum(ds) / length(ds),
    max_distance = max(ds),
    n_pairs = length(ds)
  )
}

#' morie_tncomp
#'
#' A step of the tncomp_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param fps Passed to \code{.tncomp_select}.
#' @param k Passed to \code{.tncomp_select}.
#' @param objective Passed to \code{.tncomp_select}. Defaults to \code{"maxmin"}.
#' @param seed Passed to \code{.tncomp_select}.
#' @return The value of \code{out}, as built in the body.
#' @export
morie_tncomp <- function(fps, k, objective = "maxmin", seed = NULL) {
  result <- .tncomp_select(fps, k, objective, seed)
  chosen <- result$chosen
  M <- result$M
  out <- list(
    estimate = chosen,
    selection = chosen,
    objective = objective,
    k = as.integer(k),
    seed = chosen[1],
    n_compounds = nrow(M),
    method = sprintf("Snarey et al. (1997) greedy %s selection on Tanimoto distance",
                     objective)
  )
  if (length(chosen) > 1L) {
    div <- diversity(fps, chosen, M)
    out <- c(out, div)
  }
  out
}
