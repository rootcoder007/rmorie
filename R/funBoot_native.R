# Curve-level functional bootstrap bands (Cuevas, Febrero & Fraiman 2006).
# Sources: Cuevas, A., Febrero, M. & Fraiman, R. (2006). On the
# use of the bootstrap for estimating functions with functional
# data. *Computational Statistics & Data Analysis*, 51(2),
# 1063-1074, Sec. 3(e).
#
# Native implementation mirroring Python morie.fn.funBoot exactly:
# the same curve-level bootstrap with the same
# pick = X[floor(u * n) clipped to n-1] resampling (matching the
# Python arm's generator.uniform + min(., n-1) clamping), the same
# optional Gaussian smoothed-perturbation z_j with std = smooth,
# the same default statistic being the pointwise mean, the same D
# = sorted distances [max(ceil((1 - alpha) B) - 1, 0)] giving
# n_within = ceil((1 - alpha) B) at the cut, the same L2 and sup
# metrics with the same /m normalisation for L2, and the same
# payload keys.

morie_funBoot <- function(curves, statistic = NULL, alpha = 0.05, B = 500L,
                          metric = "l2", smooth = 0.0, seed = 0) {
  X <- lapply(seq_len(nrow(curves)),
              function(i) as.numeric(curves[i, , drop = TRUE]))
  n <- length(X)
  m <- length(X[[1L]])
  if (n < 3L || any(lengths(X) != m))
    stop("curves must be rectangular with n >= 3")
  stat_fn <- if (is.null(statistic)) {
    function(cs) vapply(seq_len(m),
                        function(j) sum(vapply(cs, function(c) c[j], numeric(1L))) / length(cs),
                        numeric(1L))
  } else statistic
  alpha_v <- as.numeric(alpha)
  if (!(alpha_v > 0 && alpha_v < 1.0))
    stop("alpha must be in (0, 1)")
  met <- tolower(as.character(metric))
  if (!met %in% c("l2", "sup"))
    stop("metric must be 'l2' or 'sup'")
  smooth_v <- as.numeric(smooth)
  e <- .ghc_rng(as.numeric(seed))
  t_obs <- as.numeric(stat_fn(X))
  B_i <- as.integer(B)
  reps <- matrix(0, B_i, m)
  for (b in seq_len(B_i)) {
    sample_b <- vector("list", n)
    for (i in seq_len(n)) {
      u <- .ghc_unif(e, 1L)
      pick_idx <- min(as.integer(floor(u * n)), n - 1L) + 1L
      pick <- X[[pick_idx]]
      if (smooth_v > 0.0) {
        z <- .ghc_norm(e, 1L)
        sample_b[[i]] <- pick + smooth_v * z
      } else {
        sample_b[[i]] <- pick
      }
    }
    reps[b, ] <- as.numeric(stat_fn(sample_b))
  }
  center <- colSums(reps) / B_i

  .funboot_dist <- function(row) {
    d <- reps[row, , drop = TRUE] - center
    if (met == "sup") return(max(abs(d)))
    sqrt(sum(d * d) / m)
  }
  dists <- vapply(seq_len(B_i), .funboot_dist, numeric(1L))
  idx <- max(min(as.integer(ceiling((1.0 - alpha_v) * B_i)) - 1L, B_i - 1L), 0L)
  D <- sort(dists)[idx + 1L]
  n_within <- sum(dists <= D)
  list(center = center,
       radius = D,
       estimate = t_obs,
       distances = dists,
       n_within = n_within,
       metric = met,
       alpha = alpha_v,
       B = B_i,
       seed = as.integer(seed),
       method = "functional bootstrap band (Cuevas et al. 2006, Sec. 3e)")
}

functional_bootstrap_band <- morie_funBoot
functional_bootstrap <- morie_funBoot

.funBoot_cheatsheet <- function() {
  "funBoot: D = q_{1-a}(dist(T*_b, mean T*)); band = ball(center, D)"
}
