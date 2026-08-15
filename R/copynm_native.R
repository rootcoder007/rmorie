# Circular binary segmentation for DNA copy number.
#
# Olshen, A. B., Venkatraman, E. S., Lucito, R., & Wigler, M. (2004)
# "Circular binary segmentation for the analysis of array-based DNA copy
# number data", *Biostatistics* 5(4), 557-572.
#
# Copy-number aberrations are discrete gains and losses over *contiguous*
# regions, measured noisily, so the task is to split a chromosome into
# segments of equal copy number. That is a change-point problem.
#
# Ordinary **binary segmentation** (Sen & Srivastava 1975) tests for one
# change at position :math:`i` with
#
# .. math:: Z_i = \Big\{\tfrac{1}{i} + \tfrac{1}{n-i}\Big\}^{-1/2}
#           \Big\{\tfrac{S_i}{i} - \tfrac{S_n - S_i}{n - i}\Big\},
#
# :math:`S_i` the partial sum, and recurses on the pieces. Its weakness
# is stated plainly in the paper: it "cannot detect a small changed
# segment buried in the middle of a large segment", because it looks for
# only one change-point at a time -- and a small internal aberration
# shifts neither half's mean much.
#
# **Circular binary segmentation** fixes that by splicing the segment's
# two ends into a circle and testing whether the arc from :math:`i+1` to
# :math:`j` differs from its complement:
#
# .. math:: Z_{ij} = \Big\{\tfrac{1}{j-i} + \tfrac{1}{n-j+i}\Big\}^{-1/2}
#           \Big\{\tfrac{S_j - S_i}{j - i}
#           - \tfrac{S_n - S_j + S_i}{n - j + i}\Big\},
#           \qquad Z_C = \max_{1 \le i < j \le n} |Z_{ij}|.
#
# That statistic covers *both* alternatives at once: a single change when
# :math:`j = n`, and the epidemic or square-wave alternative (Levin &
# Kline 1985) when :math:`j < n`. Reject when :math:`Z_C` exceeds the
# threshold, take the maximising :math:`(i, j)` as the change-points, and
# recurse.
#
# Two details from the paper that are easy to skip and are implemented
# here:
#
# **The permutation reference distribution.** Rather than lean on
# normality, permute the segment, recompute :math:`Z_C^*`, and take the
# upper :math:`\alpha` quantile. The paper notes this needs on the order
# of 10 000 permutations, and that "considerable computational efficiency
# can be achieved by stopping the permutation procedure once the number
# of :math:`Z_C^* > Z_C` exceeds :math:`\alpha P`" -- early stopping is
# implemented, since it is the difference between usable and not.
#
# **Undoing edge-effect splits.** If the maximising :math:`i` is close to
# 1 or :math:`j` close to :math:`n`, a ternary split may be an artefact
# of a single real change. The paper tests each change-point of a ternary
# split for viability as a *binary* split and undoes it if unsupported.
#
# The paper is explicit that it does **not** correct for multiple
# testing: :math:`\alpha` is the type I error for a single segment, and
# because the procedure recurses "the probability of finding spurious
# change-points is a function of the number of true change-points and
# could be larger than :math:`\alpha`". That is reported rather than
# quietly papered over.
#
# Native implementation mirroring morie.fn.copynm exactly. Permutations
# use .ghc_rng so the seed=0 stream matches numpy default_rng(0); the
# O(n^2) Z_C computation is the price paid for keeping the exact
# maximising (i, j) and matches the formula verbatim.

# CBS statistic Z_C = max_{i<j} |Z_{ij}| on v; returns list(z, i, j)
# where indices are 0-based half-open into v (i < j), matching Python.
.copynm_cbs_stat <- function(v) {
  n <- length(v)
  if (n < 3L) stop("copynm: need at least 3 points to test for a change")
  S <- numeric(n + 1L)
  for (t in seq_len(n)) S[t + 1L] <- S[t] + v[t]
  Sn <- S[n + 1L]
  best <- -1.0
  bi <- 0L; bj <- 0L
  # i in [0, n-1], j in [i+1, n]; both half-open with Python
  for (i in 0:(n - 1L)) {
    for (j in (i + 1L):n) {
      m <- j - i
      k <- n - m
      if (m == 0L || k == 0L) next
      inner <- (S[j + 1L] - S[i + 1L]) / m - (Sn - S[j + 1L] + S[i + 1L]) / k
      z <- abs(inner) / sqrt(1.0 / m + 1.0 / k)
      if (z > best) {
        best <- z; bi <- i; bj <- j
      }
    }
  }
  list(z = best, i = bi, j = bj)
}

# In-place Fisher-Yates shuffle with one .ghc_unif draw per swap.
.copynm_shuffle <- function(w, e) {
  n <- length(w)
  # Python: for t in range(n - 1, 0, -1):  u = int(rng.random() * (t + 1))
  for (t in (n - 1L):1) {
    u <- as.integer(.ghc_unif(e, 1L) * (t + 1L))
    if (u > t) u <- t                    # match Python's int() truncation
    if (u < 0L) u <- 0L
    tmp <- w[t + 1L]; w[t + 1L] <- w[u + 1L]; w[u + 1L] <- tmp
  }
  invisible(w)
}

# Edge-effect undo: is `cut` a viable BINARY change-point for v?
.copynm_binary_supported <- function(v, cut, alpha, perms, e) {
  n <- length(v)
  if (cut <= 0L || cut >= n) return(FALSE)
  S <- sum(v)
  Sc <- sum(v[seq_len(cut)])
  z <- abs(Sc / cut - (S - Sc) / (n - cut)) / sqrt(1.0 / cut + 1.0 / (n - cut))
  exceed <- 0L
  limit <- alpha * perms
  for (p in seq_len(as.integer(perms))) {
    w <- .copynm_shuffle(v, e)
    Sw <- sum(w)
    best <- 0.0
    run <- 0.0
    for (c in seq_len(n - 1L)) {
      run <- run + w[c]
      zz <- abs(run / c - (Sw - run) / (n - c)) / sqrt(1.0 / c + 1.0 / (n - c))
      if (zz > best) best <- zz
    }
    if (best >= z - 1e-12) {
      exceed <- exceed + 1L
      if (exceed > limit) return(FALSE)
    }
  }
  TRUE
}

#' Circular binary segmentation statistic
#'
#' The Z_C statistic of Olshen et al. (2004) section 2: splice the
#' segment into a circle and test the arc i+1..j against its complement.
#'
#' @param x numeric vector of log-ratio intensities along a chromosome.
#' @return list with `z` (Z_C), `i` and `j` (the maximising half-open
#'         arc indices, 0-based).
#' @export
cbs_statistic <- function(x) {
  v <- as.numeric(x)
  v <- v[is.finite(v)]
  if (length(v) < 1L) stop("copynm: x must be non-empty")
  r <- .copynm_cbs_stat(v)
  r$z <- unname(r$z); r$i <- as.integer(r$i); r$j <- as.integer(r$j)
  r
}

#' Circular binary segmentation (Olshen et al. 2004)
#'
#' Segment a copy-number profile by circular binary segmentation.
#' Recursively finds the (i, j) maximising Z_C and tests against a
#' permutation reference with early stopping. Ternary splits are
#' undone if either change-point is not viable as a binary split.
#'
#' @param x numeric vector of log-ratio intensities along a chromosome.
#' @param alpha type I error for testing a single segment. The paper
#'        warns the recursion is not corrected for multiple testing.
#' @param permutations number of permutations P. Early stopping makes
#'        large P cheap when the null is clearly true.
#' @param min_width minimum segment length to attempt a split on.
#' @param undo_splits apply the edge-effect undo.
#' @param seed integer seed for the permutation stream.
#' @param max_depth recursion guard.
#' @return list with `segments` (list of (start, end, mean) half-open),
#'         `changepoints`, `n_segments`, `fitted`, `pvalues`,
#'         `estimate` (alias of segments), `alpha`, `n`,
#'         `multiplicity_note`, `method`.
#' @export
copynm <- function(x, alpha = 0.01, permutations = 1000L, min_width = 2L,
                   undo_splits = TRUE, seed = 0L, max_depth = 50L) {
  v <- as.numeric(x); v <- v[is.finite(v)]
  if (length(v) < 1L) stop("copynm: x must be non-empty")
  alpha <- as.numeric(alpha)
  if (!is.finite(alpha) || alpha <= 0 || alpha >= 1)
    stop("copynm: alpha must lie in (0, 1)")
  permutations <- as.integer(permutations)
  if (permutations < 1L)
    stop("copynm: permutations must be >= 1")
  min_width <- as.integer(min_width)
  if (min_width < 2L)
    stop("copynm: min_width must be >= 2")
  max_depth <- as.integer(max_depth)

  e <- .ghc_rng(seed)
  cuts <- integer(0)
  pvals <- list()

  .copynm_recurse <- function(a, b, depth) {
    n <- b - a
    if (n < max(3L, min_width) || depth > max_depth) return(invisible())
    seg <- v[(a + 1L):b]
    lo_v <- min(seg); hi_v <- max(seg)
    if (hi_v - lo_v <= 0.0) return(invisible())  # constant: skip
    st <- .copynm_cbs_stat(seg)
    z <- st$z; i <- st$i; j <- st$j
    exceed <- 0L
    limit <- alpha * permutations
    used <- 0L
    for (p in seq_len(permutations)) {
      w <- .copynm_shuffle(seg, e)
      sw <- .copynm_cbs_stat(w)
      used <- used + 1L
      if (sw$z >= z - 1e-12) {
        exceed <- exceed + 1L
        if (exceed > limit) break
      }
    }
    pval <- (exceed + 1.0) / (used + 1.0)
    if (exceed > limit) return(invisible())
    new <- integer(0)
    if (i > 0L) new <- c(new, a + i)
    if (j < n)  new <- c(new, a + j)
    if (length(new) == 0L) return(invisible())
    if (isTRUE(undo_splits) && length(new) == 2L) {
      keep <- integer(0)
      sub_perms <- max(50L, as.integer(permutations / 10L))
      if (.copynm_binary_supported(v[(a + 1L):(a + j)], i, alpha, sub_perms, e))
        keep <- c(keep, a + i)
      if (.copynm_binary_supported(v[(a + i + 1L):b], j - i, alpha, sub_perms, e))
        keep <- c(keep, a + j)
      new <- keep
      if (length(new) == 0L) return(invisible())
    }
    cuts <<- unique(c(cuts, new))
    for (c in new) pvals[[as.character(c)]] <<- pval
    bounds <- c(a, sort(new), b)
    for (t in seq_len(length(bounds) - 1L)) {
      if (bounds[t + 1L] - bounds[t] < n)
        .copynm_recurse(bounds[t], bounds[t + 1L], depth + 1L)
    }
    invisible()
  }

  .copynm_recurse(0L, length(v), 0L)

  edges <- c(0L, sort(cuts), length(v))
  segs <- list()
  fitted <- numeric(length(v))
  for (t in seq_len(length(edges) - 1L)) {
    a <- edges[t]; b <- edges[t + 1L]
    m <- mean(v[(a + 1L):b])
    segs[[length(segs) + 1L]] <- list(start = a, end = b, mean = m)
    fitted[(a + 1L):b] <- m
  }
  cp <- sort(unique(cuts))
  pvs <- list()
  for (c in cp) pvs[[length(pvs) + 1L]] <- pvals[[as.character(c)]]
  list(
    estimate   = segs,
    segments   = segs,
    changepoints = cp,
    n_segments = length(segs),
    fitted     = fitted,
    pvalues    = pvs,
    alpha      = alpha,
    n          = length(v),
    multiplicity_note = paste0("alpha is the type I error for a SINGLE ",
                               "segment; the recursion is not corrected ",
                               "for multiple testing (Olshen et al. 2004)"),
    method     = "circular binary segmentation (Olshen et al. 2004)"
  )
}

# Compact alias per ledger/NAMING.md
circular_binary_segmentation <- copynm

# Public name resolved by fn/_lazy_map.json
copy_number_variant <- copynm
