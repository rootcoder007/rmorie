```r
# morie.fn -- function file (rootcoder007/morie)
# Replicate weights: variance without a variance formula.
#
# The problem. A survey estimate is a function of weighted data
# under a design with strata, clusters and unequal weights. Writing
# down its sampling variance analytically means differentiating that
# function -- feasible for a total, painful for a ratio, unpleasant
# for a quantile or a regression coefficient. Replication sidesteps
# it: build R sets of weights that mimic resampling the design,
# recompute the same estimator under each, and read the variance off
# the spread.
#
# Jackknife (JK1, JKn). Drop one PSU and inflate the survivors in
# its stratum. jk1 treats the sample as unstratified and uses
# c = (n-1)/n; jkn drops one PSU per stratum in turn with
# c_h = (n_h - 1)/n_h.
#
# BRR. With exactly two PSUs per stratum the jackknife needs
# 2H replicates; balanced repeated replication needs the next
# multiple of four. Each replicate keeps one PSU per stratum at
# double weight and drops the other, and the pattern of which is
# taken comes from a Hadamard matrix.
#
# Fay's variant. Dropping a PSU entirely makes subdomain estimates
# undefined whenever a domain lives in the dropped half. Fay's
# modification keeps the dropped half at rho and the retained half
# at 2 - rho, with c = 1/(R(1-rho)^2).
#
# Rao-Wu bootstrap. Resample n_h - 1 PSUs with replacement per
# stratum and rescale so the weights stay design-unbiased.
#
# References
# ----------
# Wolter, K. M. (2007) Introduction to Variance Estimation, 2nd
# edition, Springer, ISBN 978-0-387-32917-8,
# doi:10.1007/978-0-387-35099-8. Ch. 2 (random groups), Ch. 3
# (balanced half-samples, the Hadamard construction, Fay's
# modification), Ch. 4 (the jackknife, JK1 and JKn multipliers) and
# Ch. 5 (the bootstrap); the exact equivalence of full-orthogonal
# BRR to the stratified variance estimator for linear statistics.
#
# McCarthy, P. J. (1969) Pseudo-replication: half samples, Review
# of the International Statistical Institute 37(3), 239-264,
# doi:10.2307/1402116.
#
# Rao, J. N. K. & Wu, C. F. J. (1988) Resampling inference with
# complex survey data, Journal of the American Statistical
# Association 83(401), 231-241, doi:10.1080/01621459.1988.10478591.

METHODS <- c("jk1", "jkn", "brr", "fay", "bootstrap")

.replwt_design <- function(weights, strata = NULL, psu = NULL) {
  w <- as.numeric(weights)
  n <- length(w)
  if (n < 2) stop("replwt: a design needs at least two units")
  if (any(w <= 0)) stop("replwt: sampling weights must be positive")

  if (is.null(strata)) {
    h <- rep("1", n)
  } else {
    h <- as.character(strata)
  }

  if (is.null(psu)) {
    p <- as.character(seq_len(n) - 1L)
  } else {
    p <- as.character(psu)
  }

  if (length(h) != n || length(p) != n) {
    stop(sprintf("replwt: strata and psu must have one entry per unit (%d)", n))
  }

  psu_order <- list()
  psu_units <- list()

  for (i in seq_len(n)) {
    key <- c(h[i], p[i])
    found <- FALSE
    for (k in seq_along(psu_order)) {
      if (identical(psu_order[[k]], key)) {
        psu_units[[k]] <- c(psu_units[[k]], i)
        found <- TRUE
        break
      }
    }
    if (!found) {
      psu_order[[length(psu_order) + 1L]] <- key
      psu_units[[length(psu_units) + 1L]] <- i
    }
  }

  stratum_psus <- list()
  stratum_order <- character()

  for (k in seq_along(psu_order)) {
    hh <- psu_order[[k]][1]
    if (is.null(stratum_psus[[hh]])) {
      stratum_order <- c(stratum_order, hh)
      stratum_psus[[hh]] <- k
    } else {
      stratum_psus[[hh]] <- c(stratum_psus[[hh]], k)
    }
  }

  for (hh in stratum_order) {
    if (length(stratum_psus[[hh]]) < 2L) {
      stop(sprintf("replwt: stratum %s has a single PSU, so its contribution to the variance is not estimable", hh))
    }
  }

  list(
    weights = w,
    strata = h,
    psu = p,
    n = n,
    psu_order = psu_order,
    psu_units = psu_units,
    stratum_psus = stratum_psus,
    stratum_order = stratum_order
  )
}

.replwt_hadamard <- function(order) {
  k <- as.integer(order)
  if (k < 1L || bitwAnd(k, k - 1L) != 0L) {
    stop(sprintf("replwt: this construction gives Hadamard matrices of order a power of two; %d is not one", k))
  }
  H <- list(c(1L))
  while (length(H) < k) {
    H <- c(lapply(H, function(r) c(r, r)),
           lapply(H, function(r) c(r, -r)))
  }
  H
}

.replwt_jackknife_weights <- function(d, method = "jkn") {
  if (!method %in% c("jk1", "jkn")) {
    stop(sprintf("replwt: jackknife method must be jk1 or jkn, got %s", method))
  }

  reps <- list()
  drop_list <- list()

  if (method == "jk1") {
    m <- length(d$psu_order)
    for (k in seq_along(d$psu_order)) {
      units <- d$psu_units[[k]]
      w <- d$weights
      w[units] <- 0
      f <- m / (m - 1)
      others <- setdiff(seq_len(d$n), units)
      w[others] <- w[others] * f
      reps[[length(reps) + 1L]] <- w
      drop_list[[length(drop_list) + 1L]] <- d$psu_order[[k]]
    }
    return(list(
      weights = reps,
      dropped = drop_list,
      scale = rep((m - 1) / m, m),
      method = "jk1"
    ))
  }

  scale <- numeric()
  for (hh in d$stratum_order) {
    ps_indices <- d$stratum_psus[[hh]]
    nh <- length(ps_indices)
    for (k in ps_indices) {
      units <- d$psu_units[[k]]
      w <- d$weights
      w[units] <- 0
      for (other_k in ps_indices) {
        if (other_k == k) next
        other_units <- d$psu_units[[other_k]]
        w[other_units] <- w[other_units] * (nh / (nh - 1))
      }
      reps[[length(reps) + 1L]] <- w
      drop_list[[length(drop_list) + 1L]] <- d$psu_order[[k]]
      scale <- c(scale, (nh - 1) / nh)
    }
  }
  list(
    weights = reps,
    dropped = drop_list,
    scale = scale,
    method = "jkn"
  )
}

.replwt_brr_weights <- function(d, fay = 0.0)
