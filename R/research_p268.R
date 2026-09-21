# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Research P2, P6, P8: three identification results with their R side.
#
# Every statement below is a machine-checked theorem (Lean 4 + Mathlib, 0
# sorry, standard axioms only) in research/lean/:
#
#   P2Selection.lean
#     Research.P2.rate_bounds              exposure within a factor gamma of the proxy  =>
#                                          proxy_rate/gamma <= true rate <= gamma * proxy_rate (sharp)
#     Research.P2.disparity_bounds         true ratio within gamma^2 of the proxy ratio
#     Research.P2.disparity_sign_identified proxy ratio > gamma^2  =>  true ratio > 1
#   P6AgeCrime.lean
#     Research.P6.aggregate_not_identifying  any mixture's aggregate curve is a one-type curve
#     Research.P6.invariance_sufficient / invariance_not_necessary
#   P8Deterrence.lean
#     Research.P8.constant_dimension_not_identified  a sanction dimension held fixed in the
#                                                    design has an unidentified partial effect
#     Research.P8.identified_iff_injective, corner_design_identified

#' Bounds on a police-outcome disparity when exposure is measured by a proxy
#'
#' Recorded outcomes per group (\code{y}) are rates per unit of exposure to
#' police, and exposure is only known to lie within a factor \code{gamma}
#' of an observed proxy \code{m} (mobility, presence, calls for service).
#' The true rate of each group then lies in
#' \eqn{[y/(\gamma m),\ \gamma y/m]} and the ratio of two groups' true
#' rates within \eqn{\gamma^2} of the proxy ratio, all ends attained
#' (\code{Research.P2.rate_bounds}, \code{Research.P2.disparity_bounds}).
#' The direction of the disparity is identified when the proxy ratio
#' exceeds \eqn{\gamma^2} (\code{Research.P2.disparity_sign_identified}).
#'
#' @param y Named vector of recorded outcome counts, one per group.
#' @param m Named vector of exposure proxies, same names.
#' @param gamma Factor within which true exposure is assumed to lie; at least 1.
#' @param reference Name of the reference group for ratios.
#' @return A data frame with one row per group: \code{y}, \code{m},
#'   \code{proxy_rate}, \code{rate_lower}, \code{rate_upper},
#'   \code{ratio_proxy}, \code{ratio_lower}, \code{ratio_upper} and
#'   \code{direction_identified}.
#' @examples
#' morie_disparity_exposure_bounds(y = c(A = 300, B = 100), m = c(A = 1000, B = 1000),
#'                                 gamma = 1.5, reference = "B")
#' @export
morie_disparity_exposure_bounds <- function(y, m, gamma = 1, reference = names(y)[1]) {
  if (is.null(names(y)) || is.null(names(m)) || !setequal(names(y), names(m))) {
    stop("y and m must be named vectors over the same groups", call. = FALSE)
  }
  if (any(y <= 0) || any(m <= 0)) stop("y and m must be positive", call. = FALSE)
  if (!is.numeric(gamma) || length(gamma) != 1L || is.na(gamma) || gamma < 1) {
    stop("gamma must be a single number >= 1", call. = FALSE)
  }
  if (!reference %in% names(y)) stop("reference must be one of the groups", call. = FALSE)
  m <- m[names(y)]
  pr <- y / m
  ratio <- pr / pr[[reference]]
  data.frame(
    group = names(y), y = unname(y), m = unname(m), proxy_rate = unname(pr),
    rate_lower = unname(pr / gamma), rate_upper = unname(gamma * pr),
    ratio_proxy = unname(ratio),
    ratio_lower = unname(ratio / gamma^2), ratio_upper = unname(gamma^2 * ratio),
    direction_identified = unname(ratio > gamma^2 | ratio < 1 / gamma^2)
  )
}

#' Aggregate age-crime curve of a mixture of latent types
#'
#' Returns the mixture curve \eqn{A(a) = \sum_g \pi_g \lambda_g(a)} and,
#' with it, the one-type population that produces the identical
#' aggregate (\code{Research.P6.aggregate_not_identifying}): the aggregate
#' curve alone cannot tell how many types there are or what their curves
#' look like, which is why an invariant aggregate curve is not evidence
#' for invariance at the individual level
#' (\code{Research.P6.invariance_not_necessary}).
#'
#' @param shares Type shares, summing to 1.
#' @param curves Matrix with one column per type and one row per age.
#' @param ages Optional age labels for the rows.
#' @return A data frame with \code{age}, \code{aggregate}, and one column
#'   per type curve; attribute \code{"equivalent_single_type"} holds the
#'   one-type curve with the same aggregate.
#' @examples
#' ages <- 10:60
#' early <- dgamma(ages - 9, shape = 3, rate = 0.5) * 40
#' late <- dgamma(ages - 9, shape = 8, rate = 0.35) * 40
#' morie_age_crime_aggregate(c(0.5, 0.5), cbind(early, late), ages)[1:5, ]
#' @export
morie_age_crime_aggregate <- function(shares, curves, ages = NULL) {
  curves <- as.matrix(curves)
  if (length(shares) != ncol(curves)) stop("one share per type column", call. = FALSE)
  if (any(shares < 0) || abs(sum(shares) - 1) > 1e-8) stop("shares must be non-negative and sum to 1", call. = FALSE)
  if (is.null(ages)) ages <- seq_len(nrow(curves))
  agg <- as.numeric(curves %*% shares)
  out <- data.frame(age = ages, aggregate = agg)
  cn <- colnames(curves)
  if (is.null(cn)) cn <- paste0("type", seq_len(ncol(curves)))
  for (j in seq_len(ncol(curves))) out[[cn[j]]] <- curves[, j]
  attr(out, "equivalent_single_type") <- agg
  attr(out, "theorems") <- c("Research.P6.aggregate_not_identifying",
                             "Research.P6.invariance_not_necessary")
  out
}

#' Which deterrence partial effects a design can identify
#'
#' For the linear response \eqn{y = \beta_0 + \beta_p p + \beta_s s + \beta_c c}
#' over the observed sanction regimes, a dimension that never varies has
#' an unidentified coefficient: another coefficient vector reproduces every
#' fitted value (\code{Research.P8.constant_dimension_not_identified}).
#' More generally the coefficients are identified exactly when the design
#' columns \code{(1, p, s, c)} are linearly independent
#' (\code{Research.P8.identified_iff_injective}). This function reports the
#' rank of the design and, for each dimension, whether its partial effect
#' is identified given the others.
#'
#' @param p,s,c Certainty, severity and celerity of the observed regimes.
#' @return A list with \code{rank}, \code{identified} (logical per
#'   dimension), \code{n_regimes}, and \code{theorem}.
#' @examples
#' # certainty varied at fixed severity and celerity: only certainty is identified
#' morie_deterrence_design_check(p = c(0.1, 0.3, 0.5), s = c(2, 2, 2), c = c(30, 30, 30))
#' # all three varied off a base regime: all identified
#' morie_deterrence_design_check(p = c(0, 1, 0, 0), s = c(0, 0, 1, 0), c = c(0, 0, 0, 1))
#' @export
morie_deterrence_design_check <- function(p, s, c) {
  n <- length(p)
  if (length(s) != n || length(c) != n) stop("p, s and c must have equal length", call. = FALSE)
  X <- cbind(1, p, s, c)
  rk <- qr(X)$rank
  full <- rk == 4L
  identified <- c(certainty = full, severity = full, celerity = full)
  if (!full) {
    # a dimension is identified iff dropping its column lowers the rank
    for (j in 2:4) {
      identified[j - 1L] <- qr(X[, -j, drop = FALSE])$rank < rk
    }
  }
  list(rank = rk, identified = identified, n_regimes = n,
       constant = c(certainty = length(unique(p)) == 1L, severity = length(unique(s)) == 1L,
                    celerity = length(unique(c)) == 1L),
       theorem = "Research.P8.constant_dimension_not_identified")
}


#' Disparity benchmarks: the product identity and the exposure-offset shift
#'
#' Two identities from \code{research/lean/P2Benchmark.lean}. First, with
#' population, contact and force counts per group, the disparity of force
#' per resident against a reference group equals the product of the
#' disparity of contact per resident and the disparity of force per
#' contact (\code{Research.P2.benchmark_product}); the two stages do not
#' add (\code{benchmark_not_additive}). Second, in a log-link rate model
#' with the exposure as an offset, scaling a group's exposure by a factor
#' \eqn{\kappa} while fitting the same counts shifts that group's
#' coefficient by exactly \eqn{-\log\kappa}
#' (\code{Research.P2.offset_shift}), so a disparity ratio is identified
#' only up to the ratio of the two groups' exposure errors
#' (\code{disparity_ratio_shift}).
#'
#' @param pop,contact,force Named numeric vectors (same names) of residents,
#'   police contacts and force incidents per group; all positive.
#' @param reference Name of the reference group.
#' @param exposure_error_factor Optional named vector of multiplicative
#'   errors \eqn{\kappa_g} in each group's exposure (1 means none); the
#'   resident-benchmark disparity is re-expressed under the corrected
#'   exposure.
#' @return A data frame with one row per group: \code{contact_disparity}
#'   (contact per resident relative to the reference),
#'   \code{force_given_contact_disparity}, \code{resident_disparity}
#'   (their product), \code{additive_claim} (their sum, for comparison),
#'   \code{log_shift} and \code{resident_disparity_corrected} when
#'   exposure errors are given; attribute \code{"theorems"}.
#' @examples
#' morie_disparity_benchmark(pop = c(A = 100, B = 100), contact = c(A = 30, B = 10),
#'                           force = c(A = 12, B = 2), reference = "B")
#' @export
morie_disparity_benchmark <- function(pop, contact, force, reference, exposure_error_factor = NULL) {
  g <- names(pop)
  if (is.null(g) || !identical(g, names(contact)) || !identical(g, names(force))) stop("pop, contact and force must share the same group names", call. = FALSE)
  if (any(c(pop, contact, force) <= 0)) stop("all counts must be positive", call. = FALSE)
  if (!reference %in% g) stop("reference must be one of the group names", call. = FALSE)
  r <- reference
  cd <- (contact / pop) / (contact[r] / pop[r])
  fd <- (force / contact) / (force[r] / contact[r])
  rd <- (force / pop) / (force[r] / pop[r])
  out <- data.frame(group = g, contact_disparity = unname(cd), force_given_contact_disparity = unname(fd),
                    resident_disparity = unname(rd), additive_claim = unname(cd + fd),
                    product_check = unname(rd - cd * fd))
  if (!is.null(exposure_error_factor)) {
    k <- exposure_error_factor[g]
    if (anyNA(k) || any(k <= 0)) stop("exposure_error_factor must be positive for every group", call. = FALSE)
    out$log_shift <- unname(-log(k))
    out$resident_disparity_corrected <- unname(rd * (k[r] / k))
  }
  attr(out, "theorems") <- c("Research.P2.benchmark_product", "Research.P2.benchmark_not_additive",
                             "Research.P2.offset_shift", "Research.P2.disparity_ratio_shift")
  out
}
