# SPDX-License-Identifier: AGPL-3.0-or-later

# Internal: per-player Banzhaf swing increment for one coalition.
# Given the in-coalition membership `mask` and the coalition weight
# `tot_in`, returns a length-n 0/1 vector marking which players are
# pivotal. Shared by the Monte-Carlo and exact-enumeration paths.
.vtpwr_swing_increment <- function(mask, tot_in, w, quota, n) {
  inc <- numeric(n)
  for (i in seq_len(n)) {
    if (mask[i]) {
      inc[i] <- tot_in >= quota && (tot_in - w[i]) < quota
    } else {
      inc[i] <- (tot_in + w[i]) >= quota && tot_in < quota
    }
  }
  inc
}

# Internal: pivotal player in one ordering, for Shapley-Shubik.
.vtpwr_pivot <- function(ord, w, quota) {
  cum <- 0
  for (idx in ord) {
    prev <- cum
    cum <- cum + w[idx]
    if (prev < quota && quota <= cum) {
      return(idx)
    }
  }
  NA_integer_
}

# Internal: all permutations of a vector, as a matrix (one per row).
.vtpwr_perms <- function(v) {
  if (length(v) == 1L) {
    return(matrix(v, 1L, 1L))
  }
  do.call(rbind, lapply(seq_along(v), function(i) {
    cbind(v[i], .vtpwr_perms(v[-i]))
  }))
}

# Internal: Monte-Carlo voting-power indices for large games (n > 10).
.vtpwr_mc <- function(w, quota, n) {
  set.seed(0L)
  n_mc <- 20000L
  swings <- rep(0, n)
  ss <- rep(0, n)
  for (k in seq_len(n_mc)) {
    mask <- as.logical(sample.int(2L, n, replace = TRUE) - 1L)
    swings <- swings + .vtpwr_swing_increment(mask, sum(w[mask]), w, quota, n)
    piv <- .vtpwr_pivot(sample.int(n), w, quota)
    if (!is.na(piv)) ss[piv] <- ss[piv] + 1L
  }
  list(
    banzhaf = swings / max(sum(swings), 1),
    shapley_shubik = ss / n_mc, quota = quota, weights = w,
    method = "voting_power_index_mc"
  )
}

# Internal: exact voting-power indices by full enumeration (n <= 10).
.vtpwr_exact <- function(w, quota, n) {
  swings <- rep(0, n)
  for (mask_int in 0:(2^n - 1L)) {
    mask <- as.logical(intToBits(mask_int)[1:n])
    swings <- swings + .vtpwr_swing_increment(mask, sum(w[mask]), w, quota, n)
  }
  perms <- .vtpwr_perms(seq_len(n))
  shapley <- rep(0, n)
  for (r in seq_len(nrow(perms))) {
    piv <- .vtpwr_pivot(perms[r, ], w, quota)
    if (!is.na(piv)) shapley[piv] <- shapley[piv] + 1L
  }
  list(
    banzhaf = swings / max(sum(swings), 1),
    shapley_shubik = shapley / factorial(n), quota = quota, weights = w,
    method = "voting_power_index_exact"
  )
}

#' Banzhaf and Shapley-Shubik voting-power indices (Armstrong Ch 10)
#'
#' Exact enumeration for n <= 10; Monte Carlo for larger games.
#' Quota defaults to strict simple majority.
#'
#' @param x Numeric weight vector w.
#' @param quota Winning threshold q (default sum(w)/2).
#' @return Named list with `banzhaf`, `shapley_shubik`, `quota`,
#'   `weights`, `method`.
#' @examples
#' vtpwr(x = rnorm(50))
#' @export
vtpwr <- function(x, quota = NULL) {
  w <- as.numeric(x)
  n <- length(w)
  if (n == 0L) {
    return(list(
      banzhaf = numeric(0), shapley_shubik = numeric(0),
      quota = NA_real_, weights = w,
      method = "morie_voting_power_index"
    ))
  }
  if (is.null(quota)) quota <- sum(w) / 2 + 1e-9
  if (n > 10L) .vtpwr_mc(w, quota, n) else .vtpwr_exact(w, quota, n)
}

#' @keywords internal
#' @rdname vtpwr
#' @export
morie_voting_power_index <- vtpwr
