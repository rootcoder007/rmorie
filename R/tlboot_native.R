# The targeted bootstrap.
# Sources: van der Laan, M. J. & Rose, S. (2018) *Targeted Learning
# in Data Science*, Springer, doi:10.1007/978-3-319-65304-4. Chap. 28
# (Coyle & van der Laan): the bootstrap's use for confidence
# intervals and hypothesis tests, with higher-order accuracy over
# Wald intervals in some settings, and its wide adoption in
# contexts not all of which have theoretical support; the typical
# targeted-learning workflow of initial super learner fits
# followed by a TMLE; the description of why the bootstrap as
# typically applied fails in that framework; and the solution as
# a TARGETED BOOTSTRAP designed to be consistent for the first
# two moments of the sampling distribution. Efron, B. &
# Tibshirani, R. J. (1993) *An Introduction to the Bootstrap*,
# Chapman and Hall, doi:10.1201/9780429246593. Hall, P. (1992)
# *The Bootstrap and Edgeworth Expansion*, Springer,
# doi:10.1007/978-1-4612-4384-7. The higher-order accuracy
# result.
#
# Native implementation mirroring Python morie.fn.tlboot exactly:
# the same naive nonparametric bootstrap with a clamped index, the
# same targeted- and multiplier-bootstraps, the same moment check,
# and the same warning string kept so the failure is visible.

morie_tlboot <- function(data, estimator, B = 200L, seed = 0L,
                         method = c("naive", "targeted",
                                    "multiplier"),
                         ic = NULL) {
  method <- match.arg(method)
  B <- as.integer(B)
  if (length(B) != 1L || is.na(B) || B < 1L)
    stop("tlboot: B must be a positive integer")

  if (method == "naive") {
    rows <- as.list(data)
    n <- length(rows)
    if (n < 2L)
      stop("tlboot: at least 2 observations are needed")
    e <- .ghc_rng(as.numeric(seed))
    out <- numeric(B)
    for (b in seq_len(B)) {
      u <- .ghc_unif(e, n)
      idx <- as.integer(u * n)
      idx <- idx %% n + 1L
      s <- rows[idx]
      out[b] <- as.numeric(estimator(s))
    }
    m <- mean(out)
    if (B > 1L)
      sd <- sqrt(sum((out - m)^2) / (B - 1))
    else
      sd <- 0
    return(list(replicates = out, mean = m, se = sd, B = B,
                caveat = "refitting a data-adaptive learner on each resample mixes the learner's instability into the sampling distribution"))
  }

  if (method == "targeted") {
    e <- .ghc_rng(as.numeric(seed))
    out <- numeric(B)
    for (b in seq_len(B)) {
      out[b] <- as.numeric(estimator(P_star_sampler(e)))
    }
    m <- mean(out)
    if (B > 1L)
      sd <- sqrt(sum((out - m)^2) / (B - 1))
    else
      sd <- 0
    return(list(estimate = m, mean = m, se = sd,
                replicates = out, B = B,
                method = "targeted bootstrap from the fitted P_n^*; van der Laan & Rose (2018) Chap. 28",
                note = "designed to be consistent for the first two moments of the sampling distribution"))
  }

  # multiplier bootstrap on the influence curve
  if (is.null(ic))
    stop("tlboot: multiplier bootstrap requires an influence curve")
  d <- as.numeric(ic)
  n <- length(d)
  if (n < 2L)
    stop("tlboot: at least 2 influence values are needed")
  e <- .ghc_rng(as.numeric(seed))
  out <- numeric(B)
  for (b in seq_len(B)) {
    u <- pmax(.ghc_unif(e, n), 1e-12)
    w <- -log(u)
    s <- sum(w)
    out[b] <- sum(w * d) / s
  }
  m <- mean(out)
  if (B > 1L)
    sd <- sqrt(sum((out - m)^2) / (B - 1))
  else
    sd <- 0
  mm <- mean(d)
  if (n > 1L)
    icse <- sqrt(sum((d - mm)^2) / (n - 1) / n)
  else
    icse <- 0
  list(replicates = out, mean = m, se = sd,
       influence_curve_se = icse,
       ratio = if (icse > 0) sd / icse else NaN,
       note = "matches the influence-curve standard error by construction, at no refitting cost")
}

# compat with the Python arm: expose the three entry points
naive_bootstrap <- function(data, estimator, B = 200L, seed = 0L) {
  morie_tlboot(data = data, estimator = estimator, B = B,
               seed = seed, method = "naive")
}

targeted_bootstrap <- function(P_star_sampler, estimator, B = 200L,
                               seed = 0L) {
  morie_tlboot(data = NULL, estimator = estimator, B = B,
               seed = seed, method = "targeted")
}

multiplier_bootstrap <- function(ic, B = 1000L, seed = 0L) {
  morie_tlboot(data = NULL, estimator = NULL, B = B,
               seed = seed, method = "multiplier", ic = ic)
}

moment_check <- function(replicates, target_mean, target_se,
                         tol = 0.15) {
  v <- as.numeric(replicates)
  n <- length(v)
  if (n < 2L)
    stop("tlboot: at least 2 replicates are needed")
  m <- mean(v)
  sd <- sqrt(sum((v - m)^2) / (n - 1))
  list(mean = m, se = sd,
       mean_error = abs(m - as.numeric(target_mean)),
       se_ratio = if (as.numeric(target_se) > 0)
                    sd / as.numeric(target_se) else NaN,
       first_two_moments_ok =
         abs(sd / as.numeric(target_se) - 1) < as.numeric(tol),
       note = "consistency for the first two moments is the stated design goal")
}

cheatsheet <- function() {
  paste("tlboot: the ordinary bootstrap FAILS for TMLE. Refitting ",
        "a super learner on every resample makes the nuisance fits ",
        "move with the resample, and that is not the sampling ",
        "variability of the target -- it is instability of an ",
        "infinite-dimensional object converging slower than ",
        "root-n, so more resamples do not help. Instead resample ",
        "FROM THE TARGETED FIT P_n^*, holding the nuisances fixed; ",
        "the design goal is consistency for the first TWO MOMENTS. ",
        "The multiplier bootstrap on the influence curve is the ",
        "cheap equivalent.", sep = "")
}
