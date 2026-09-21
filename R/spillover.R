# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Research P3: policing effects under interference (partial interference
# through a stated exposure mapping).
#
# Every identity here is a machine-checked theorem in
# research/lean/P3Interference.lean (Lean 4 + Mathlib, 0 sorry, standard
# axioms only), on a finite weighted population of places:
#
#   Research.P3.Model.exposure_adjustment          E[Y(l)] = sum_k m_k * mean(Y_obs | stratum k, exposure l)
#                                                   under positivity and exposure ignorability given the stratum
#   Research.P3.Model.direct_spillover_decomposition
#                                                   E[Y(2)] - E[Y(0)] = (E[Y(1)] - E[Y(0)]) + (E[Y(2)] - E[Y(1)])
#   Research.P3.Model.misspecified_exposure_bias   mean(pooled 0+1) - mean(0) = share(1) * (mean(1) - mean(0))
#
# The theorems say what is identified given an exposure mapping and
# ignorability; whether patrols only spill over to the ring the analyst
# named is a claim about the world, which is why the pooling bias is
# reported rather than assumed away.

#' Three-level exposure from a treatment on a place network
#'
#' Assigns each place the exposure used by the P3 theorems: 2 if treated,
#' 1 if untreated with at least one treated neighbour, 0 if untreated and
#' isolated from treatment. Pass the adjacency of the ring you are willing
#' to assume spillover stops at (distance-1 edges, or the union of rings up
#' to distance d).
#'
#' @param treated Integer or logical vector, one entry per place.
#' @param edges Two-column matrix or data frame of place indices (1-based)
#'   giving the undirected adjacency.
#' @return A data frame with \code{place}, \code{treated},
#'   \code{treated_neighbours} and \code{exposure} (0, 1 or 2).
#' @examples
#' edges <- rbind(c(1, 2), c(2, 3), c(3, 4))
#' morie_spillover_exposure(c(1, 0, 0, 0), edges)
#' @export
morie_spillover_exposure <- function(treated, edges) {
  treated <- as.integer(as.logical(treated))
  if (anyNA(treated)) stop("treated must not contain NA", call. = FALSE)
  edges <- as.matrix(edges)
  if (ncol(edges) != 2L) stop("edges must have two columns", call. = FALSE)
  storage.mode(edges) <- "integer"
  if (anyNA(edges) || any(edges < 1L) || any(edges > length(treated))) {
    stop("edges must index places 1..length(treated)", call. = FALSE)
  }
  r <- .morie_spillover_exposure_cpp(treated, edges[, 1L] - 1L, edges[, 2L] - 1L)
  data.frame(place = seq_along(treated), treated = treated,
             treated_neighbours = r$treated_neighbours, exposure = r$exposure)
}

#' Direct, spillover and net effects under a stated exposure mapping
#'
#' Stratified exposure adjustment: within each stratum the observed
#' outcome is averaged by exposure level, and the stratum means are
#' combined with the stratum weights. Under positivity (every stratum
#' holds every exposure level) and exposure ignorability given the
#' stratum, the three adjusted means are the mean potential outcomes
#' \eqn{E[Y(0)], E[Y(1)], E[Y(2)]} (\code{Research.P3.Model.exposure_adjustment}),
#' and the treated-versus-isolated contrast decomposes exactly into a
#' spillover part and a direct-beyond-spillover part
#' (\code{direct_spillover_decomposition}). The pooling bias reports how far
#' an analysis that ignores spillover (pooling exposures 0 and 1 as
#' "control") would sit from \eqn{E[Y(0)]}
#' (\code{misspecified_exposure_bias}).
#'
#' @param y Outcome per place.
#' @param exposure Exposure level per place (0, 1, 2), from
#'   \code{\link{morie_spillover_exposure}}.
#' @param stratum Covariate stratum per place (factor or vector).
#' @param weights Optional non-negative place weights; equal by default.
#' @return A list with \code{means} (adjusted \eqn{E[Y(0)], E[Y(1)], E[Y(2)]}),
#'   \code{spillover} (\eqn{E[Y(1)] - E[Y(0)]}), \code{direct}
#'   (\eqn{E[Y(2)] - E[Y(1)]}), \code{total} (\eqn{E[Y(2)] - E[Y(0)]}),
#'   \code{pooling_bias} (pooled-control mean minus \eqn{E[Y(0)]}, stratum
#'   by stratum and overall), \code{positivity} (whether every stratum has
#'   every level) and \code{theorems}.
#' @examples
#' set.seed(1)
#' n <- 400
#' stratum <- rep(c("core", "edge"), each = n / 2)
#' exposure <- sample(0:2, n, replace = TRUE)
#' y <- 5 - 1.0 * (exposure == 2) - 0.4 * (exposure == 1) + (stratum == "core") + rnorm(n, 0, 0.1)
#' morie_spillover_effects(y, exposure, stratum)[c("spillover", "direct", "total")]
#' @export
morie_spillover_effects <- function(y, exposure, stratum, weights = NULL) {
  n <- length(y)
  if (length(exposure) != n || length(stratum) != n) stop("y, exposure and stratum must have equal length", call. = FALSE)
  if (!all(exposure %in% 0:2)) stop("exposure must take values 0, 1, 2", call. = FALSE)
  if (is.null(weights)) weights <- rep(1, n)
  if (length(weights) != n || any(weights < 0) || sum(weights) <= 0) {
    stop("weights must be non-negative with positive total", call. = FALSE)
  }
  w <- weights / sum(weights)
  strata <- unique(stratum)
  levels_ <- 0:2
  cell_mean <- function(s, l) {
    idx <- stratum == s & exposure == l
    if (!any(idx)) return(NA_real_)
    sum(w[idx] * y[idx]) / sum(w[idx])
  }
  m_k <- vapply(strata, function(s) sum(w[stratum == s]), numeric(1))
  cells <- sapply(levels_, function(l) vapply(strata, cell_mean, numeric(1), l = l))
  cells <- matrix(cells, nrow = length(strata), dimnames = list(strata, paste0("Y", levels_)))
  positivity <- !anyNA(cells)
  means <- colSums(m_k * cells)
  # pooling bias: pooled control (levels 0 and 1) versus level 0, per stratum and overall
  pooled <- vapply(strata, function(s) {
    idx <- stratum == s & exposure %in% c(0L, 1L)
    sum(w[idx] * y[idx]) / sum(w[idx])
  }, numeric(1))
  bias_k <- pooled - cells[, "Y0"]
  list(
    means = means,
    spillover = unname(means["Y1"] - means["Y0"]),
    direct = unname(means["Y2"] - means["Y1"]),
    total = unname(means["Y2"] - means["Y0"]),
    pooling_bias = list(by_stratum = bias_k, overall = sum(m_k * bias_k)),
    cell_means = cells, stratum_weights = stats::setNames(m_k, strata),
    positivity = positivity,
    theorems = c("Research.P3.Model.exposure_adjustment",
                 "Research.P3.Model.direct_spillover_decomposition",
                 "Research.P3.Model.misspecified_exposure_bias")
  )
}
