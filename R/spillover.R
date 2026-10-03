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
# and in research/lean/P3HorvitzThompson.lean and P3Variance.lean, over a
# finite randomised design with assignment probabilities q(a):
#
#   Research.P3.Design.ht_unbiased                  E_q[T_hat(l)] = sum_i Y_i(l) when every pi_i(l) > 0
#   Research.P3.Design.ht_contrast_unbiased         same for T_hat(l2) - T_hat(l0)
#   Research.P3.Design.ht_variance                  Var T_hat(l) = sum_ij (pi_ij - pi_i pi_j) Y_i Y_j / (pi_i pi_j)
#   Research.P3.Design.ht_variance_estimator_unbiased
#                                                   E_q[V_hat(l)] = Var T_hat(l) when every joint pi_ij(l) > 0,
#                                                   V_hat(l) = sum_ij 1{E_i=E_j=l} (pi_ij - pi_i pi_j)/pi_ij * Y_i Y_j/(pi_i pi_j)
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

#' Horvitz-Thompson totals and contrasts under a randomised deployment
#'
#' When the deployment was randomised, every place's exposure probability
#' \eqn{\pi_i(\ell) = P(E_i = \ell)} is known from the design (or can be
#' computed by re-drawing assignments, see
#' \code{\link{morie_spillover_exposure_probs}}). The estimator
#' \eqn{\hat T(\ell) = \sum_i 1\{E_i=\ell\}\, y_i / \pi_i(\ell)} is unbiased
#' for the population total of \eqn{Y(\ell)} whenever every
#' \eqn{\pi_i(\ell) > 0}, and so is any contrast between levels
#' (\code{Research.P3.Design.ht_unbiased},
#' \code{Research.P3.Design.ht_contrast_unbiased}). No ignorability
#' assumption is used: the design supplies it.
#'
#' When the joint exposure probabilities \eqn{\pi_{ij}(\ell) = P(E_i = \ell, E_j = \ell)}
#' are supplied, the variance of each total is estimated by the
#' Horvitz-Thompson variance estimator
#' \eqn{\hat V(\ell) = \sum_{i,j} 1\{E_i = E_j = \ell\}\,(\pi_{ij} - \pi_i\pi_j)/\pi_{ij}\; y_i y_j/(\pi_i \pi_j)},
#' which is unbiased for
#' \eqn{\mathrm{Var}\,\hat T(\ell) = \sum_{i,j} (\pi_{ij} - \pi_i\pi_j)\, Y_i(\ell) Y_j(\ell)/(\pi_i\pi_j)}
#' whenever every \eqn{\pi_{ij}(\ell) > 0}
#' (\code{Research.P3.Design.ht_variance},
#' \code{Research.P3.Design.ht_variance_estimator_unbiased}). A pair of
#' places that can never share a level leaves that variance unidentified,
#' and the function then reports \code{NA} for it rather than a number.
#'
#' @param y Observed outcome per place.
#' @param exposure Realised exposure per place (0, 1, 2).
#' @param probs Matrix with one row per place and columns \code{"0"},
#'   \code{"1"}, \code{"2"} giving \eqn{\pi_i(\ell)}; all entries positive.
#' @param joint Optional \code{n} by \code{n} by 3 array of joint exposure
#'   probabilities \eqn{\pi_{ij}(\ell)}, as returned in the \code{joint}
#'   element of \code{morie_spillover_exposure_probs(..., joint = TRUE)}.
#' @return A list with \code{totals} (\eqn{\hat T(0), \hat T(1), \hat T(2)}),
#'   \code{means} (totals divided by the number of places), \code{spillover},
#'   \code{direct}, \code{total_effect} (per-place means), and when
#'   \code{joint} is given \code{variance} and \code{se} of the three
#'   totals (\code{NA} where a joint probability is zero), and
#'   \code{theorems}.
#' @examples
#' set.seed(2)
#' edges <- cbind(1:19, 2:20)
#' pr <- morie_spillover_exposure_probs(n = 20, edges, n_treated = 6, n_draws = 2000, joint = TRUE)
#' trt <- sample(20, 6); exposure <- morie_spillover_exposure(seq_len(20) %in% trt, edges)$exposure
#' y <- 5 - 1.5 * (exposure == 2) - 0.5 * (exposure == 1)
#' morie_spillover_ht(y, exposure, pr$marginal,
#'                    pr$joint)[c("spillover", "direct", "total_effect", "se")]
#' @export
morie_spillover_ht <- function(y, exposure, probs, joint = NULL) {
  n <- length(y)
  probs <- as.matrix(probs)
  if (length(exposure) != n || nrow(probs) != n || ncol(probs) != 3L) {
    stop(sprintf("y, exposure and probs (n x 3) must describe the same places: y has %d, exposure %d, probs %d x %d",
                 n, length(exposure), nrow(probs), ncol(probs)), call. = FALSE)
  }
  if (any(probs <= 0)) stop("every exposure probability must be positive (positivity)", call. = FALSE)
  if (!all(exposure %in% 0:2)) stop("exposure must take values 0, 1, 2", call. = FALSE)
  totals <- vapply(0:2, function(l) sum(ifelse(exposure == l, y / probs[, l + 1L], 0)), numeric(1))
  names(totals) <- paste0("T", 0:2)
  means <- totals / n
  out <- list(totals = totals, means = means,
              spillover = unname(means[2] - means[1]), direct = unname(means[3] - means[2]),
              total_effect = unname(means[3] - means[1]))
  theorems <- c("Research.P3.Design.ht_unbiased", "Research.P3.Design.ht_contrast_unbiased")
  if (!is.null(joint)) {
    if (!is.array(joint) || !identical(dim(joint), c(n, n, 3L))) {
      stop("joint must be an n x n x 3 array of joint exposure probabilities", call. = FALSE)
    }
    variance <- vapply(0:2, function(l) {
      pij <- joint[, , l + 1L]
      pi_l <- probs[, l + 1L]
      ind <- as.numeric(exposure == l)
      if (any(pij[outer(ind, ind) > 0] <= 0)) return(NA_real_)
      w <- outer(ind, ind) * (pij - outer(pi_l, pi_l)) / ifelse(pij > 0, pij, 1)
      sum(w * outer(y / pi_l, y / pi_l))
    }, numeric(1))
    names(variance) <- paste0("T", 0:2)
    out$variance <- variance
    out$se <- sqrt(pmax(variance, 0))
    out$variance_identified <- vapply(0:2, function(l) all(joint[, , l + 1L] > 0), logical(1))
    theorems <- c(theorems, "Research.P3.Design.ht_variance",
                  "Research.P3.Design.ht_variance_estimator_unbiased")
  }
  out$theorems <- theorems
  out
}

#' Design variance of the Horvitz-Thompson total (population formula)
#'
#' The exact variance
#' \eqn{\sum_{i,j} (\pi_{ij} - \pi_i\pi_j)\, Y_i Y_j/(\pi_i\pi_j)}
#' of the Horvitz-Thompson total of a known potential-outcome vector
#' under a design with marginal and joint exposure probabilities
#' (\code{Research.P3.Design.ht_variance}). Used to plan a deployment:
#' it says how precisely a given design can estimate a given effect size
#' before any outcome is observed.
#'
#' @param y_pot Potential outcome per place at the level of interest.
#' @param pi Marginal exposure probability per place at that level.
#' @param pij \code{n} by \code{n} joint exposure probabilities at that level.
#' @param form \code{"ht"} (the Horvitz-Thompson form above), \code{"syg"}
#'   (the Sen-Yates-Grundy form
#'   \eqn{\tfrac12\sum_{i,k}(\pi_i\pi_k-\pi_{ik})(c_i-c_k)^2}, \eqn{c_i = Y_i/\pi_i})
#'   or \code{"both"}. The two agree exactly on a fixed-size design, one in
#'   which \eqn{\sum_k \pi_{ik} = m\pi_i} with \eqn{m} the constant number of
#'   places at the level (\code{Research.P3.SYG.syg_eq_ht}); the SYG form is
#'   zero when the potential outcomes are proportional to the inclusion
#'   probabilities (\code{syg_zero_of_const}). Exposure levels of a
#'   randomised deployment are rarely fixed-size, so \code{"both"} also
#'   reports whether the condition holds.
#' @return The variance (a number) for \code{"ht"} or \code{"syg"}; for
#'   \code{"both"} a list with \code{ht}, \code{syg}, \code{fixed_size},
#'   \code{level_count} and \code{theorems}.
#' @examples
#' pr <- morie_spillover_exposure_probs(n = 6, edges = cbind(1:5, 2:6), n_treated = 2, joint = TRUE)
#' morie_spillover_ht_variance(rep(1, 6), pr$marginal[, "2"], pr$joint[, , 3])
#' @export
morie_spillover_ht_variance <- function(y_pot, pi, pij, form = c("ht", "syg", "both")) {
  form <- match.arg(form)
  n <- length(y_pot)
  pij <- as.matrix(pij)
  if (length(pi) != n || !identical(dim(pij), c(n, n))) stop("y_pot, pi and pij must describe the same places", call. = FALSE)
  if (any(pi <= 0)) stop("every marginal probability must be positive", call. = FALSE)
  c_ <- y_pot / pi
  ht <- sum((pij - outer(pi, pi)) * outer(c_, c_))
  if (form == "ht") return(ht)
  syg <- 0.5 * sum((outer(pi, pi) - pij) * outer(c_, c_, "-")^2)
  m <- sum(pi)
  fixed_size <- max(abs(rowSums(pij) - m * pi)) < 1e-10
  if (form == "syg") return(syg)
  list(ht = ht, syg = syg, fixed_size = fixed_size, level_count = m,
       theorems = c("Research.P3.Design.ht_variance", "Research.P3.SYG.syg_eq_ht", "Research.P3.SYG.syg_zero_of_const"))
}

#' Exposure probabilities of a completely randomised deployment
#'
#' Draws \code{n_draws} assignments of \code{n_treated} treated places out
#' of \code{n} and returns, per place, the share of draws in which it sat
#' at each exposure level of \code{\link{morie_spillover_exposure}}. Exact
#' enumeration is used when it is small enough. Monte Carlo draws come from
#' the Philox stream: draw \code{k} treats the \code{n_treated} places with
#' the smallest of the uniforms \code{(k - 1) n + 1, ..., k n} of
#' \code{.morie_random_uniform(n * n_draws, seed, stream = 0)}, so the R and
#' Python arms return identical probabilities for the same seed.
#'
#' @param n Number of places.
#' @param edges Adjacency as in \code{\link{morie_spillover_exposure}}.
#' @param n_treated Number of treated places per assignment.
#' @param n_draws Monte Carlo draws (ignored when exact enumeration is feasible).
#' @param exact_max Enumerate all assignments when \code{choose(n, n_treated)}
#'   is at most this.
#' @param joint Also return the joint probabilities
#'   \eqn{\pi_{ij}(\ell) = P(E_i = \ell, E_j = \ell)} needed for the variance
#'   in \code{\link{morie_spillover_ht}}.
#' @param seed Philox seed for the Monte Carlo branch (non-negative).
#' @return An \code{n} by 3 matrix of exposure probabilities (columns
#'   \code{"0"}, \code{"1"}, \code{"2"}); with \code{joint = TRUE} a list
#'   with \code{marginal} (that matrix) and \code{joint} (an \code{n} by
#'   \code{n} by 3 array).
#' @examples
#' morie_spillover_exposure_probs(n = 6, edges = cbind(1:5, 2:6), n_treated = 2)
#' @export
morie_spillover_exposure_probs <- function(n, edges, n_treated, n_draws = 5000L, exact_max = 20000,
                                           joint = FALSE, seed = 0) {
  if (n_treated < 1 || n_treated >= n) stop("n_treated must lie in 1..n-1", call. = FALSE)
  counts <- matrix(0, n, 3L, dimnames = list(NULL, c("0", "1", "2")))
  jcounts <- if (joint) array(0, c(n, n, 3L), dimnames = list(NULL, NULL, c("0", "1", "2"))) else NULL
  tally <- function(trt) {
    e <- morie_spillover_exposure(seq_len(n) %in% trt, edges)$exposure
    for (l in 0:2) {
      ind <- e == l
      counts[ind, l + 1L] <<- counts[ind, l + 1L] + 1
      if (joint) jcounts[, , l + 1L] <<- jcounts[, , l + 1L] + outer(ind, ind)
    }
  }
  if (choose(n, n_treated) <= exact_max) {
    cmb <- utils::combn(n, n_treated)
    for (k in seq_len(ncol(cmb))) tally(cmb[, k])
    m <- ncol(cmb)
  } else {
    if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) || seed < 0) {
      stop("seed must be a single non-negative number", call. = FALSE)
    }
    n_draws <- as.integer(n_draws)
    u <- .morie_random_uniform(n * n_draws, seed = seed, stream = 0)
    for (k in seq_len(n_draws)) tally(order(u[(k - 1L) * n + seq_len(n)])[seq_len(n_treated)])
    m <- n_draws
  }
  if (joint) list(marginal = counts / m, joint = jcounts / m) else counts / m
}
