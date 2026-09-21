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


#' Relative risk from an odds ratio: the interval that arrest-only data allow
#'
#' Case-control and arrest-only samples identify the odds ratio of an outcome
#' between two groups but not the two risks themselves. The relative risk
#' nevertheless lies between 1 and the odds ratio
#' (\code{Research.P2.rr_between}), since \eqn{OR = RR (1-b)/(1-a)}
#' (\code{or_eq_rr_mul}), and the rare-outcome substitution \eqn{RR \approx OR}
#' always overstates the relative risk in log magnitude (\code{or_overstates}).
#' With a base rate supplied the two risks and the relative risk are point
#' identified.
#'
#' @param odds_ratio Identified odds ratio, positive.
#' @param base_rate Optional overall outcome rate \eqn{P(y=1)}; with
#'   \code{exposed_share} it point-identifies the two risks.
#' @param exposed_share Share of the population with \eqn{x = 1}.
#' @return A list with \code{rr_bounds} (between 1 and the odds ratio),
#'   \code{overstatement_factor} (\eqn{OR/RR} when identified),
#'   \code{risks} (when identified) and \code{theorems}.
#' @examples
#' morie_relative_risk_from_or(3)
#' morie_relative_risk_from_or(3, base_rate = 0.2, exposed_share = 0.3)
#' @export
morie_relative_risk_from_or <- function(odds_ratio, base_rate = NULL, exposed_share = NULL) {
  if (length(odds_ratio) != 1L || is.na(odds_ratio) || odds_ratio <= 0) stop("odds_ratio must be a single positive number", call. = FALSE)
  out <- list(rr_bounds = c(lower = min(1, odds_ratio), upper = max(1, odds_ratio)),
              theorems = c("Research.P2.or_eq_rr_mul", "Research.P2.rr_between", "Research.P2.or_overstates"))
  if (!is.null(base_rate) && !is.null(exposed_share)) {
    if (base_rate <= 0 || base_rate >= 1 || exposed_share <= 0 || exposed_share >= 1) stop("base_rate and exposed_share must lie in (0, 1)", call. = FALSE)
    # solve for b: q = s a + (1 - s) b with a = OR b / (1 - b + OR b)
    f <- function(b) exposed_share * (odds_ratio * b / (1 - b + odds_ratio * b)) + (1 - exposed_share) * b - base_rate
    b <- stats::uniroot(f, c(1e-12, 1 - 1e-12))$root
    a <- odds_ratio * b / (1 - b + odds_ratio * b)
    out$risks <- c(exposed = a, unexposed = b, relative_risk = a / b)
    out$overstatement_factor <- odds_ratio / (a / b)
  }
  out
}


#' Direction of deterrence without convexity: optimal offending under certainty and severity
#'
#' An offender chooses a level \code{x} from any finite menu to maximise
#' \eqn{y(x) - p f(x)} with \eqn{f} strictly increasing. Any optimum at a higher
#' certainty \eqn{p} is at most any optimum at a lower one
#' (\code{Research.P8.certainty_monotone}), the same holds for a sanction
#' schedule that grows faster in \eqn{x} (\code{severity_monotone}), and
#' aggregate offending over heterogeneous offenders is non-increasing
#' (\code{aggregate_monotone}). No concavity of the benefit, no
#' differentiability, no interior solution: convexity buys uniqueness and
#' smoothness of the response, not its direction. What remains open in P8 is
#' separating certainty from severity in data, not the sign.
#'
#' @param x Numeric vector, the menu of offending levels.
#' @param benefit Numeric vector, the benefit at each level (any shape).
#' @param sanction Numeric vector, the sanction at each level, strictly increasing in \code{x}.
#' @param p Numeric vector of certainty levels to evaluate.
#' @return A data frame with one row per \code{p}: \code{p}, \code{x_opt} (the
#'   largest optimal level), \code{value}; attribute \code{"theorems"}. The
#'   \code{x_opt} column is non-increasing in \code{p}.
#' @examples
#' x <- 0:10; morie_deterrence_response(x, benefit = sqrt(x) * 3 - (x %% 3 == 0), sanction = x^1.5, p = c(0.1, 0.3, 0.5, 1))
#' @export
morie_deterrence_response <- function(x, benefit, sanction, p) {
  n <- length(x)
  if (length(benefit) != n || length(sanction) != n) stop("x, benefit and sanction must have equal length", call. = FALSE)
  o <- order(x); x <- x[o]; benefit <- benefit[o]; sanction <- sanction[o]
  if (any(diff(sanction) <= 0)) stop("sanction must be strictly increasing in x", call. = FALSE)
  if (any(p < 0)) stop("p must be non-negative", call. = FALSE)
  res <- do.call(rbind, lapply(p, function(pp) {
    u <- benefit - pp * sanction
    best <- which(u >= max(u) - 1e-12)
    data.frame(p = pp, x_opt = max(x[best]), value = max(u))
  }))
  attr(res, "theorems") <- c("Research.P8.certainty_monotone", "Research.P8.severity_monotone", "Research.P8.aggregate_monotone")
  res
}
