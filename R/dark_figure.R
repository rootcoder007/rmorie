# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Research P1: the dark figure of crime as a partial-identification problem.
#
# Every identity and bound here is a machine-checked theorem in
# research/lean/P1DarkFigure.lean (Lean 4 + Mathlib, 0 sorry, standard
# axioms only), building on research/lean/P5Fairness.lean:
#
#   Research.P1.TwoSource.petersen_identity  N = theta * n1 * n2 / m
#   Research.P1.TwoSource.lincoln_petersen   theta = 1  =>  N = n1 * n2 / m
#   Research.P1.TwoSource.petersen_bounds    theta in [1/kappa, kappa]  =>
#                                            n1 n2 / (kappa m) <= N <= kappa n1 n2 / m
#   Research.P1.petersen_lower_attained / petersen_upper_attained: both ends reached
#   Research.P1.true_rate_bounds             max(r, (v_obs - a)/(1 - a)) <= v <= v_obs/(1 - b)
#   Research.P1.dark_figure_bounds           the same interval shifted by r, floored at 0
#   Research.P1.conclusion_holds_below_breakdown / conclusion_fails_above_breakdown:
#                                            "v < t" survives every admissible noise iff beta_max < 1 - v_obs/t
#
# The theorems are about expected counts and rates. Whether two lists are
# independent, or what the misreporting boxes are, is a claim about the
# world that no proof supplies; the functions take those as arguments.

#' Two-source (capture-recapture) count with a dependence box
#'
#' Two lists of the same events (police records and a survey, hospital or
#' NGO register) of sizes \code{n1} and \code{n2} share \code{m} events.
#' Under independence the true count is the Lincoln-Petersen ratio
#' \eqn{n_1 n_2 / m}. Independence is rarely credible: events recorded by
#' the police are often more (or less) likely to reach a second list. With
#' the dependence factor \eqn{\theta} only known to lie in
#' \eqn{[1/\kappa, \kappa]}, the true count lies in
#' \eqn{[n_1 n_2/(\kappa m),\ \kappa n_1 n_2/m]} and both ends are
#' attainable (\code{Research.P1.TwoSource.petersen_bounds}).
#'
#' @param n1,n2 Sizes of the two lists; positive.
#' @param m Number of events on both lists; positive and at most
#'   \code{min(n1, n2)}.
#' @param kappa Largest credible dependence factor, at least 1;
#'   \code{kappa = 1} asserts independence.
#' @param chapman If \code{TRUE}, also return Chapman's small-sample
#'   version \eqn{(n_1+1)(n_2+1)/(m+1) - 1} of the point estimate.
#' @return A list with \code{point} (Lincoln-Petersen), \code{lower},
#'   \code{upper}, \code{kappa}, \code{theorem}, and \code{chapman} when
#'   requested.
#' @references Petersen CGJ (1896); Lincoln FC (1930); Chapman DG (1951).
#'   Bird SM, King R (2018). Multiple systems estimation (or
#'   capture-recapture estimation) to inform public policy. Annual Review
#'   of Statistics and Its Application 5, 95-118.
#' @examples
#' # police records 400, survey 250, 80 in both
#' morie_dark_figure_two_source(400, 250, 80)              # independence: 1250
#' morie_dark_figure_two_source(400, 250, 80, kappa = 2)   # [625, 2500]
#' @export
morie_dark_figure_two_source <- function(n1, n2, m, kappa = 1, chapman = FALSE) {
  for (v in list(n1, n2, m, kappa)) {
    if (!is.numeric(v) || length(v) != 1L || is.na(v)) {
      stop("n1, n2, m and kappa must be single non-missing numbers", call. = FALSE)
    }
  }
  if (n1 <= 0 || n2 <= 0 || m <= 0) stop("n1, n2 and m must be positive", call. = FALSE)
  if (m > min(n1, n2)) stop("m cannot exceed the smaller list", call. = FALSE)
  if (kappa < 1) stop("kappa must be at least 1", call. = FALSE)
  point <- n1 * n2 / m
  out <- list(point = point, lower = point / kappa, upper = kappa * point, kappa = kappa,
              theorem = if (kappa == 1) "Research.P1.TwoSource.lincoln_petersen"
                        else "Research.P1.TwoSource.petersen_bounds")
  if (isTRUE(chapman)) out$chapman <- (n1 + 1) * (n2 + 1) / (m + 1) - 1
  out
}

#' Sharp bounds on a true victimisation rate and its dark figure
#'
#' A recorded rate \code{r} (police records per capita) never exceeds the
#' true rate \code{v}, and a survey rate \code{v_obs} is a noisy proxy
#' \eqn{v(1-\beta) + (1-v)\alpha} with under-reporting \eqn{\beta} (stigma,
#' non-recall) and over-reporting \eqn{\alpha} (misclassified incidents).
#' With only the boxes \eqn{\alpha \in [0, a]}, \eqn{\beta \in [0, b]},
#' \eqn{a + b < 1} credible, the true rate is identified exactly up to
#' \deqn{\max\!\left(r, \frac{v_{obs}-a}{1-a}\right) \le v \le \frac{v_{obs}}{1-b},}
#' and the dark figure \eqn{v - r} up to that interval shifted by \code{r}
#' (\code{Research.P1.true_rate_bounds}, \code{Research.P1.dark_figure_bounds}).
#' Both ends of the survey part are attained, so nothing narrower follows
#' from the boxes alone.
#'
#' @param v_obs Survey victimisation rate(s) in [0, 1].
#' @param r Recorded rate(s) in [0, 1], same length as \code{v_obs} or length 1.
#' @param alpha_max Largest credible over-reporting rate.
#' @param beta_max Largest credible under-reporting rate;
#'   \code{alpha_max + beta_max} must be below 1.
#' @return A data frame with \code{v_obs}, \code{r}, \code{v_lower},
#'   \code{v_upper}, \code{dark_lower}, \code{dark_upper} and
#'   \code{ratio_upper} (the largest credible ratio of true to recorded
#'   rate, \code{NA} when \code{r = 0}).
#' @examples
#' # survey says 6 percent were victimised, police recorded 2 percent
#' morie_dark_figure_bounds(v_obs = 0.06, r = 0.02, alpha_max = 0.01, beta_max = 0.30)
#' @export
morie_dark_figure_bounds <- function(v_obs, r, alpha_max, beta_max) {
  if (!is.numeric(v_obs) || any(is.na(v_obs)) || any(v_obs < 0) || any(v_obs > 1)) {
    stop("v_obs must be numeric in [0, 1]", call. = FALSE)
  }
  if (!is.numeric(r) || any(is.na(r)) || any(r < 0) || any(r > 1)) {
    stop("r must be numeric in [0, 1]", call. = FALSE)
  }
  if (length(r) != 1L && length(r) != length(v_obs)) {
    stop("r must have length 1 or the length of v_obs", call. = FALSE)
  }
  for (v in list(alpha_max, beta_max)) {
    if (!is.numeric(v) || length(v) != 1L || is.na(v) || v < 0) {
      stop("alpha_max and beta_max must be single non-negative numbers", call. = FALSE)
    }
  }
  if (alpha_max + beta_max >= 1) stop("alpha_max + beta_max must be below 1", call. = FALSE)
  r <- rep_len(r, length(v_obs))
  v_lower <- pmax(r, (v_obs - alpha_max) / (1 - alpha_max))
  v_upper <- pmin(1, v_obs / (1 - beta_max))
  v_upper <- pmax(v_upper, v_lower)   # a recorded rate above the survey ceiling: interval collapses to r
  data.frame(
    v_obs = v_obs, r = r,
    v_lower = v_lower, v_upper = v_upper,
    dark_lower = v_lower - r, dark_upper = v_upper - r,
    ratio_upper = ifelse(r > 0, v_upper / r, NA_real_)
  )
}

#' Breakdown analysis for a dark-figure conclusion
#'
#' How much survey under-reporting would it take to overturn "the true
#' victimisation rate is below \code{threshold}"? The conclusion holds for
#' every admissible noise pair while the under-reporting box
#' \eqn{\bar\beta} stays below \eqn{1 - v_{obs}/t}
#' (\code{Research.P1.conclusion_holds_below_breakdown}); at or above that
#' value there is an admissible noise pair with a true rate of at least
#' \code{threshold} (\code{Research.P1.conclusion_fails_above_breakdown}).
#' Report the breakdown value next to the interval so a reader can judge
#' whether the required under-reporting is plausible.
#'
#' @param v_obs Survey victimisation rate in (0, 1].
#' @param threshold The rate the conclusion claims is not reached; above
#'   \code{v_obs}.
#' @return A list with \code{breakdown_beta_max} (\eqn{1 - v_{obs}/t}), the
#'   \code{ratio} \eqn{t/v_{obs}} of the threshold to the survey rate, and
#'   \code{theorems}.
#' @examples
#' # survey 6 percent; does the true rate stay below 10 percent?
#' morie_dark_figure_breakdown(0.06, threshold = 0.10)   # only if under-reporting < 40 percent
#' @export
morie_dark_figure_breakdown <- function(v_obs, threshold) {
  if (!is.numeric(v_obs) || length(v_obs) != 1L || is.na(v_obs) || v_obs <= 0 || v_obs > 1) {
    stop("v_obs must be a single number in (0, 1]", call. = FALSE)
  }
  if (!is.numeric(threshold) || length(threshold) != 1L || is.na(threshold) || threshold <= v_obs) {
    stop("threshold must be a single number above v_obs", call. = FALSE)
  }
  list(breakdown_beta_max = 1 - v_obs / threshold, ratio = threshold / v_obs,
       theorems = c("Research.P1.conclusion_holds_below_breakdown",
                    "Research.P1.conclusion_fails_above_breakdown"))
}


#' Three-list capture-recapture: what is and is not identified
#'
#' With three lists the observed table has seven cells; the count of
#' units on no list is the eighth. The saturated log-linear model (all
#' pairwise interactions and the three-way interaction) reproduces any
#' positive eight-cell table exactly (\code{Research.P1.three_list_saturated_fits}),
#' so the missing cell can take any positive value while the seven
#' observed cells are matched (\code{Research.P1.missing_cell_unconstrained}).
#' The population size is identified only once the analyst fixes the
#' three-way interaction; the conventional choice sets it to zero, which
#' gives the closed form
#' \eqn{m_{000} = m_{111} m_{100} m_{010} m_{001} / (m_{110} m_{101} m_{011})}.
#' The function reports that estimate, the logical floor (the number of
#' distinct units seen), the two-list Petersen and Chapman estimates for
#' each pair of lists (each proved to respect the floor,
#' \code{Research.P1.petersen_ge_floor}, \code{chapman_ge_floor}), and the
#' three-way interaction that any candidate missing cell would imply.
#'
#' @param counts Named vector or list of the seven observed cells with
#'   names \code{"100"}, \code{"010"}, \code{"001"}, \code{"110"},
#'   \code{"101"}, \code{"011"}, \code{"111"} (digits: on list 1, list 2,
#'   list 3); all positive.
#' @param candidate_missing Optional positive values of the missing cell for
#'   which the implied three-way interaction is reported.
#' @return A list with \code{observed} (units seen, the floor),
#'   \code{missing_no_three_way}, \code{N_no_three_way}, \code{pairwise}
#'   (a data frame of Petersen and Chapman estimates per pair of lists with
#'   the floor for that pair), \code{implied_three_way} (for
#'   \code{candidate_missing}) and \code{theorems}.
#' @examples
#' morie_dark_figure_three_list(c("100" = 120, "010" = 90, "001" = 70,
#'                                "110" = 40, "101" = 30, "011" = 25, "111" = 15))
#' @export
morie_dark_figure_three_list <- function(counts, candidate_missing = NULL) {
  need <- c("100", "010", "001", "110", "101", "011", "111")
  counts <- unlist(counts)
  if (is.null(names(counts)) || !all(need %in% names(counts))) stop("counts must be named by the seven cells 100, 010, 001, 110, 101, 011, 111", call. = FALSE)
  m <- as.numeric(counts[need])
  names(m) <- need
  if (anyNA(m) || any(m <= 0)) stop("all seven observed cells must be positive", call. = FALSE)
  observed <- sum(m)
  m000 <- m["111"] * m["100"] * m["010"] * m["001"] / (m["110"] * m["101"] * m["011"])
  on <- function(cell, k) substr(cell, k, k) == "1"
  pairs <- list(c(1, 2), c(1, 3), c(2, 3))
  pairwise <- do.call(rbind, lapply(pairs, function(pr) {
    n1 <- sum(m[on(need, pr[1])])
    n2 <- sum(m[on(need, pr[2])])
    mm <- sum(m[on(need, pr[1]) & on(need, pr[2])])
    data.frame(lists = paste(pr, collapse = "-"), n1 = n1, n2 = n2, m = mm,
               floor = n1 + n2 - mm, petersen = n1 * n2 / mm,
               chapman = (n1 + 1) * (n2 + 1) / (mm + 1) - 1)
  }))
  implied <- NULL
  if (!is.null(candidate_missing)) {
    if (any(candidate_missing <= 0)) stop("candidate_missing must be positive", call. = FALSE)
    lm_ <- log(m)
    implied <- data.frame(
      missing = candidate_missing,
      N = observed + candidate_missing,
      three_way = lm_["111"] - lm_["110"] - lm_["101"] - lm_["011"] +
        lm_["100"] + lm_["010"] + lm_["001"] - log(candidate_missing))
    rownames(implied) <- NULL
  }
  list(observed = observed, missing_no_three_way = unname(m000),
       N_no_three_way = unname(observed + m000), pairwise = pairwise,
       implied_three_way = implied,
       theorems = c("Research.P1.three_list_saturated_fits", "Research.P1.missing_cell_unconstrained",
                    "Research.P1.petersen_ge_floor", "Research.P1.chapman_ge_floor"))
}


#' Incident versus offence counting: the hierarchy-rule arithmetic
#'
#' Under a hierarchy rule an incident with several offences is counted once
#' (its most serious offence); an offence-based system counts every offence.
#' With \eqn{N} incidents carrying \eqn{k_i \ge 1} offences each, bounded by
#' \eqn{K}, the offence count lies in \eqn{[N, KN]} and equals
#' \eqn{N(1 + \bar{e})} with \eqn{\bar{e}} the mean number of extra offences per
#' incident (\code{Research.P1.offence_count_bounds},
#' \code{offence_count_eq}); the offence total is not a function of the
#' incident total (\code{category_not_identified}). A switch from incident
#' to offence counting therefore raises recorded crime with no change in
#' crime, by exactly the mean extra offences per incident.
#'
#' @param offences_per_incident Integer vector, one entry per incident, each at least 1.
#' @return A list with \code{incidents}, \code{offences}, \code{ratio}
#'   (offences per incident), \code{mean_extra}, \code{bounds} (\eqn{[N, KN]}
#'   with \eqn{K} the observed maximum) and \code{theorems}.
#' @examples
#' morie_dark_figure_hierarchy(c(1, 1, 2, 1, 3, 1, 1, 2))
#' @export
morie_dark_figure_hierarchy <- function(offences_per_incident) {
  k <- as.integer(offences_per_incident)
  if (length(k) == 0L || anyNA(k) || any(k < 1L)) stop("every incident must carry at least one offence", call. = FALSE)
  N <- length(k)
  tot <- sum(k)
  list(incidents = N, offences = tot, ratio = tot / N, mean_extra = mean(k - 1),
       bounds = c(lower = N, upper = max(k) * N),
       theorems = c("Research.P1.offence_count_bounds", "Research.P1.offence_count_eq",
                    "Research.P1.category_not_identified"))
}
