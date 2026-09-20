#' Design and measurement tools for small or under-enumerated subpopulations
#'
#' Planning and adjustment functions for studies whose target group is a small
#' share of the sampling frame, is reached through a design that is not simple
#' random sampling, or is subject to identification error in the frame itself.
#' The functions are written in general survey-methodology terms; the
#' motivating application is estimation for Indigenous populations in Canada,
#' where all three conditions typically hold at once (see the
#' \code{subpopulation-design} vignette).
#'
#' @name subpop_design
#' @keywords internal
NULL

.spd_check_prob <- function(x, nm) {
  x <- as.numeric(x)
  if (length(x) != 1L || is.na(x) || x < 0 || x > 1) {
    stop(sprintf("%s must be a single probability in [0, 1]", nm), call. = FALSE)
  }
  x
}

.spd_z <- function(conf) {
  conf <- .spd_check_prob(conf, "conf")
  if (conf <= 0 || conf >= 1) stop("conf must be strictly between 0 and 1", call. = FALSE)
  stats::qnorm(1 - (1 - conf) / 2)
}

# ---------------------------------------------------------------------------
# Design effects
# ---------------------------------------------------------------------------

#' Design effect from clustering
#'
#' The standard cluster design effect \eqn{deff = 1 + (m - 1)\rho}, where
#' \eqn{m} is the average number of completed interviews per cluster and
#' \eqn{\rho} the intracluster correlation. When a survey is clustered by
#' community, the effective sample size is the nominal size divided by this
#' quantity, and planning that ignores it understates the sample required.
#'
#' Use \code{\link{morie_design_effect}} instead when unequal weights, not
#' clustering, are the source of the loss; the two sources multiply.
#'
#' @param m Average number of completed units per cluster. Must be at least 1.
#' @param icc Intracluster correlation \eqn{\rho}. May be negative, but must
#'   not produce a negative design effect.
#' @return A single numeric design effect.
#' @seealso \code{\link{morie_neff_cluster}}, \code{\link{morie_design_effect}}
#' @export
#' @examples
#' # 25 interviews per community, ICC 0.02
#' morie_deff_cluster(25, 0.02)
morie_deff_cluster <- function(m, icc) {
  m <- as.numeric(m)
  icc <- as.numeric(icc)
  if (length(m) != 1L || is.na(m) || m < 1) stop("m must be a single number >= 1", call. = FALSE)
  if (length(icc) != 1L || is.na(icc)) stop("icc must be a single number", call. = FALSE)
  deff <- 1 + (m - 1) * icc
  if (deff <= 0) stop("icc implies a non-positive design effect", call. = FALSE)
  deff
}

#' Effective sample size under clustering
#'
#' @param n Nominal number of completed units.
#' @param m Average number of completed units per cluster.
#' @param icc Intracluster correlation.
#' @return A single numeric effective sample size, \eqn{n / deff}.
#' @seealso \code{\link{morie_deff_cluster}}
#' @export
#' @examples
#' morie_neff_cluster(500, 25, 0.02)
morie_neff_cluster <- function(n, m, icc) {
  n <- as.numeric(n)
  if (length(n) != 1L || is.na(n) || n <= 0) stop("n must be a single positive number", call. = FALSE)
  n / morie_deff_cluster(m, icc)
}

# ---------------------------------------------------------------------------
# Sample size
# ---------------------------------------------------------------------------

#' Sample size for a proportion under a complex design
#'
#' Returns the completed sample size needed to estimate a proportion to a
#' stated margin of error, with a finite population correction, a design
#' effect, and an inflation for non-response.
#'
#' The unadjusted requirement is \eqn{n_0 = z^2 p (1 - p) / e^2}. The finite
#' population correction gives \eqn{n = n_0 / (1 + (n_0 - 1)/N)}; the design
#' effect multiplies; non-response inflates by \eqn{1/r}. The order matters
#' and is the one used here: precision first, then the population limit, then
#' the design, then fieldwork loss.
#'
#' @param p Anticipated proportion. Use \code{0.5} for the conservative
#'   maximum-variance value when no prior estimate exists.
#' @param moe Target margin of error (half-width of the confidence interval),
#'   on the proportion scale.
#' @param conf Confidence level. Default \code{0.95}.
#' @param N Population size. Default \code{Inf} (no finite population
#'   correction).
#' @param deff Design effect. Default \code{1}.
#' @param response_rate Expected proportion of selected units that yield a
#'   completed response. Default \code{1}.
#' @return A list with \code{n_srs} (before design effect and non-response),
#'   \code{n_design} (after the finite population correction and design
#'   effect), \code{n_invite} (units that must be selected), and the inputs.
#' @seealso \code{\link{morie_sample_size_domain}}
#' @export
#' @examples
#' # 5 percentage points, clustered design, 60% response
#' morie_sample_size_proportion(0.5, 0.05, deff = 1.5, response_rate = 0.6)
morie_sample_size_proportion <- function(p, moe, conf = 0.95, N = Inf,
                                         deff = 1, response_rate = 1) {
  p <- .spd_check_prob(p, "p")
  moe <- as.numeric(moe)
  if (length(moe) != 1L || is.na(moe) || moe <= 0 || moe >= 1) {
    stop("moe must be a single number in (0, 1)", call. = FALSE)
  }
  deff <- as.numeric(deff)
  if (length(deff) != 1L || is.na(deff) || deff <= 0) {
    stop("deff must be a single positive number", call. = FALSE)
  }
  response_rate <- .spd_check_prob(response_rate, "response_rate")
  if (response_rate <= 0) stop("response_rate must be greater than 0", call. = FALSE)
  N <- as.numeric(N)
  if (length(N) != 1L || is.na(N) || N <= 0) {
    stop("N must be a single positive number or Inf", call. = FALSE)
  }
  z <- .spd_z(conf)
  n0 <- z^2 * p * (1 - p) / moe^2
  n_fpc <- if (is.finite(N)) n0 / (1 + (n0 - 1) / N) else n0
  n_design <- n_fpc * deff
  list(
    n_srs = n0,
    n_design = n_design,
    n_invite = n_design / response_rate,
    p = p, moe = moe, conf = conf, N = N,
    deff = deff, response_rate = response_rate
  )
}

#' Sample size for a subpopulation (domain) within a larger frame
#'
#' Answers the planning question "how large must the whole sample be so that
#' the subgroup is itself large enough?". The subgroup requirement is computed
#' first, exactly as in \code{\link{morie_sample_size_proportion}}; the overall
#' requirement is then that subgroup size divided by the subgroup's share of
#' the frame, further divided by the coverage rate if the frame under-identifies
#' the group.
#'
#' Two facts drive the result and both are easy to lose. A subgroup forming a
#' small share of the population requires a very large general sample for even
#' a modest subgroup target, which is why general-population surveys are
#' usually unable to report on small subpopulations without oversampling. And
#' if the frame identifies only a fraction of the group, that fraction divides
#' the achievable subgroup size a second time.
#'
#' @param p Anticipated proportion within the subgroup.
#' @param moe Target margin of error for the subgroup estimate.
#' @param domain_prevalence Share of the frame belonging to the subgroup.
#' @param conf Confidence level. Default \code{0.95}.
#' @param N_domain Subgroup population size. Default \code{Inf}.
#' @param deff Design effect within the subgroup. Default \code{1}.
#' @param response_rate Expected response rate. Default \code{1}.
#' @param coverage Proportion of subgroup members correctly identified as such
#'   by the frame. Default \code{1}.
#' @return A list with \code{n_domain} (completed subgroup interviews needed),
#'   \code{n_domain_invite}, \code{n_overall} (units that must be selected from
#'   the full frame under proportionate sampling), and the inputs.
#' @seealso \code{\link{morie_oversample_factor}},
#'   \code{\link{morie_screen_design}}
#' @export
#' @examples
#' # Subgroup is 5% of the frame; want 5 points on a 50% attribute
#' morie_sample_size_domain(0.5, 0.05, domain_prevalence = 0.05)
morie_sample_size_domain <- function(p, moe, domain_prevalence, conf = 0.95,
                                     N_domain = Inf, deff = 1,
                                     response_rate = 1, coverage = 1) {
  dp <- .spd_check_prob(domain_prevalence, "domain_prevalence")
  if (dp <= 0) stop("domain_prevalence must be greater than 0", call. = FALSE)
  cov <- .spd_check_prob(coverage, "coverage")
  if (cov <= 0) stop("coverage must be greater than 0", call. = FALSE)
  base <- morie_sample_size_proportion(p, moe, conf = conf, N = N_domain,
                                       deff = deff,
                                       response_rate = response_rate)
  list(
    n_domain = base$n_design,
    n_domain_invite = base$n_invite,
    n_overall = base$n_invite / (dp * cov),
    domain_prevalence = dp,
    coverage = cov,
    deff = base$deff,
    response_rate = base$response_rate,
    moe = base$moe,
    conf = conf
  )
}

#' Oversampling factor to reach a target subgroup composition
#'
#' The multiplier applied to the subgroup's selection probability so that it
#' forms a stated share of the achieved sample rather than its natural share
#' of the frame. A factor above 1 means disproportionate stratified selection,
#' which buys subgroup precision at the cost of unequal weights; the resulting
#' weight variation feeds back into the design effect, so the two should be
#' planned together rather than in sequence.
#'
#' @param domain_prevalence Share of the frame belonging to the subgroup.
#' @param target_share Desired share of the achieved sample.
#' @return A list with \code{factor} (the selection-probability multiplier),
#'   \code{weight_ratio} (the ratio of subgroup to non-subgroup design weights
#'   that results), and \code{deff_weights}, the Kish design effect implied by
#'   those two weights at the target composition.
#' @seealso \code{\link{morie_sample_size_domain}},
#'   \code{\link{morie_design_effect}}
#' @export
#' @examples
#' # Lift a 5% subgroup to 20% of the achieved sample
#' morie_oversample_factor(0.05, 0.20)
morie_oversample_factor <- function(domain_prevalence, target_share) {
  dp <- .spd_check_prob(domain_prevalence, "domain_prevalence")
  ts <- .spd_check_prob(target_share, "target_share")
  if (dp <= 0 || dp >= 1) stop("domain_prevalence must be in (0, 1)", call. = FALSE)
  if (ts <= 0 || ts >= 1) stop("target_share must be in (0, 1)", call. = FALSE)
  fac <- (ts / (1 - ts)) * ((1 - dp) / dp)
  # Design weights are inversely proportional to selection probability.
  wr <- 1 / fac
  # Kish deff for a two-group weight structure at the target composition.
  wbar <- ts * wr + (1 - ts) * 1
  w2bar <- ts * wr^2 + (1 - ts) * 1
  list(factor = fac, weight_ratio = wr, deff_weights = w2bar / wbar^2)
}

#' Two-phase screening design for a rare subpopulation
#'
#' When the frame does not identify the subgroup, members are found by
#' screening. This returns the expected number of screens, the expected cost,
#' and the share of the budget spent on screening rather than on interviews.
#'
#' @param prevalence Share of screened units belonging to the subgroup.
#' @param target_n Number of completed subgroup interviews required.
#' @param cost_screen Cost per screening contact. Default \code{1}.
#' @param cost_interview Cost per completed interview. Default \code{1}.
#' @param screen_response Proportion of contacts that yield a screening
#'   outcome. Default \code{1}.
#' @param interview_response Proportion of eligible units that complete the
#'   interview. Default \code{1}.
#' @return A list with \code{n_screen}, \code{n_eligible}, \code{cost_total},
#'   \code{cost_per_completed_interview} and \code{screening_share_of_cost}.
#' @seealso \code{\link{morie_sample_size_domain}}
#' @export
#' @examples
#' morie_screen_design(0.05, 400, cost_screen = 5, cost_interview = 120)
morie_screen_design <- function(prevalence, target_n, cost_screen = 1,
                                cost_interview = 1, screen_response = 1,
                                interview_response = 1) {
  pr <- .spd_check_prob(prevalence, "prevalence")
  if (pr <= 0) stop("prevalence must be greater than 0", call. = FALSE)
  sr <- .spd_check_prob(screen_response, "screen_response")
  ir <- .spd_check_prob(interview_response, "interview_response")
  if (sr <= 0 || ir <= 0) stop("response rates must be greater than 0", call. = FALSE)
  target_n <- as.numeric(target_n)
  if (length(target_n) != 1L || is.na(target_n) || target_n <= 0) {
    stop("target_n must be a single positive number", call. = FALSE)
  }
  cost_screen <- as.numeric(cost_screen)
  cost_interview <- as.numeric(cost_interview)
  if (any(c(cost_screen, cost_interview) < 0)) {
    stop("costs must be non-negative", call. = FALSE)
  }
  n_eligible <- target_n / ir
  n_screen <- n_eligible / (pr * sr)
  cost_s <- n_screen * cost_screen
  cost_i <- target_n * cost_interview
  total <- cost_s + cost_i
  list(
    n_screen = n_screen,
    n_eligible = n_eligible,
    cost_total = total,
    cost_per_completed_interview = total / target_n,
    screening_share_of_cost = if (total > 0) cost_s / total else NA_real_
  )
}

#' Cost-constrained optimal allocation across strata
#'
#' Optimal allocation when the cost per unit differs by stratum:
#' \eqn{n_h \propto N_h S_h / \sqrt{c_h}}. With equal costs this reduces to
#' Neyman allocation, \eqn{n_h \propto N_h S_h}, which is also available
#' directly as \code{\link{Neyman}}. The cost form matters whenever some
#' strata are markedly more expensive to enumerate than others, as remote
#' communities usually are.
#'
#' @param N_h Stratum population sizes.
#' @param S_h Stratum standard deviations of the study variable.
#' @param n Total sample size to allocate.
#' @param cost_h Per-unit cost in each stratum. Default \code{NULL}, meaning
#'   equal costs (Neyman allocation).
#' @return A list with \code{n_h} (unrounded allocation), \code{n_h_int}
#'   (integers summing to \code{n}), and \code{share}.
#' @seealso \code{\link{Neyman}}, \code{\link{Propalloc}}
#' @export
#' @examples
#' morie_alloc_optimal(N_h = c(8000, 1500, 500), S_h = c(1, 1.4, 2),
#'                     n = 900, cost_h = c(1, 3, 9))
morie_alloc_optimal <- function(N_h, S_h, n, cost_h = NULL) {
  N_h <- as.numeric(N_h)
  S_h <- as.numeric(S_h)
  if (length(N_h) != length(S_h)) stop("N_h and S_h must have the same length", call. = FALSE)
  if (length(N_h) < 1L) stop("at least one stratum is required", call. = FALSE)
  if (anyNA(N_h) || anyNA(S_h) || any(N_h <= 0) || any(S_h < 0)) {
    stop("N_h must be positive and S_h non-negative, with no missing values", call. = FALSE)
  }
  n <- as.numeric(n)
  if (length(n) != 1L || is.na(n) || n <= 0) stop("n must be a single positive number", call. = FALSE)
  if (is.null(cost_h)) {
    cost_h <- rep(1, length(N_h))
  } else {
    cost_h <- as.numeric(cost_h)
    if (length(cost_h) != length(N_h)) stop("cost_h must match N_h in length", call. = FALSE)
    if (anyNA(cost_h) || any(cost_h <= 0)) stop("cost_h must be positive", call. = FALSE)
  }
  num <- N_h * S_h / sqrt(cost_h)
  denom <- sum(num)
  if (denom <= 0) stop("allocation is undefined when every stratum has zero variance", call. = FALSE)
  share <- num / denom
  nh <- n * share
  # Largest-remainder rounding so the integers sum to n exactly.
  base <- floor(nh)
  rem <- n - sum(base)
  if (rem > 0) {
    ord <- order(nh - base, decreasing = TRUE)
    base[ord[seq_len(round(rem))]] <- base[ord[seq_len(round(rem))]] + 1
  }
  list(n_h = nh, n_h_int = as.integer(base), share = share)
}

# ---------------------------------------------------------------------------
# Weighting
# ---------------------------------------------------------------------------

#' Rake a weight vector to known population margins
#'
#' Iterative proportional fitting. Each margin is enforced in turn and the
#' cycle repeats until every margin is matched within tolerance. Raking is the
#' usual way to make a sample agree with published counts for a small group
#' when the joint distribution of those counts is not published, only the
#' one-way totals.
#'
#' @param data A data frame containing the raking variables.
#' @param margins A named list; each element is a named numeric vector of
#'   population totals for the levels of the corresponding column of
#'   \code{data}.
#' @param weights Starting weights. Default \code{NULL} for unit weights.
#' @param max_iter Maximum number of cycles. Default \code{50}.
#' @param tol Convergence tolerance on the largest absolute margin discrepancy.
#'   Default \code{1e-8}.
#' @return A list with \code{weights}, \code{converged}, \code{iterations} and
#'   \code{max_discrepancy}.
#' @seealso \code{\link{morie_calibration_weights}}, \code{\link{Postrt}}
#' @export
#' @examples
#' df <- data.frame(
#'   region = c("north", "north", "south", "south"),
#'   group = c("a", "b", "a", "b")
#' )
#' morie_rake(df, list(region = c(north = 60, south = 40),
#'                     group = c(a = 70, b = 30)))
morie_rake <- function(data, margins, weights = NULL, max_iter = 50L,
                       tol = 1e-8) {
  if (!is.data.frame(data)) stop("data must be a data frame", call. = FALSE)
  if (!is.list(margins) || is.null(names(margins)) || any(!nzchar(names(margins)))) {
    stop("margins must be a named list", call. = FALSE)
  }
  miss <- setdiff(names(margins), names(data))
  if (length(miss)) {
    stop(sprintf("margin variables not found in data: %s",
                 paste(miss, collapse = ", ")), call. = FALSE)
  }
  n <- nrow(data)
  if (n == 0L) stop("data has no rows", call. = FALSE)
  w <- if (is.null(weights)) rep(1, n) else as.numeric(weights)
  if (length(w) != n) stop("weights must have one element per row", call. = FALSE)
  if (anyNA(w) || any(w <= 0)) stop("starting weights must be positive", call. = FALSE)
  totals <- vapply(margins, sum, numeric(1))
  if (any(abs(totals - totals[1]) > 1e-6 * max(1, abs(totals[1])))) {
    stop("every margin must sum to the same population total", call. = FALSE)
  }
  max_iter <- as.integer(max_iter)
  if (is.na(max_iter) || max_iter < 1L) stop("max_iter must be a positive integer", call. = FALSE)
  converged <- FALSE
  it <- 0L
  disc <- NA_real_
  for (it in seq_len(max_iter)) {
    for (v in names(margins)) {
      lev <- as.character(data[[v]])
      tgt <- margins[[v]]
      unknown <- setdiff(unique(lev), names(tgt))
      if (length(unknown)) {
        stop(sprintf("variable %s has levels absent from its margin: %s",
                     v, paste(unknown, collapse = ", ")), call. = FALSE)
      }
      cur <- tapply(w, factor(lev, levels = names(tgt)), sum)
      cur[is.na(cur)] <- 0
      for (l in names(tgt)) {
        if (cur[[l]] > 0) {
          w[lev == l] <- w[lev == l] * tgt[[l]] / cur[[l]]
        } else if (tgt[[l]] > 0) {
          stop(sprintf("level %s of %s has a positive target but no sample units",
                       l, v), call. = FALSE)
        }
      }
    }
    disc <- max(vapply(names(margins), function(v) {
      lev <- factor(as.character(data[[v]]), levels = names(margins[[v]]))
      cur <- tapply(w, lev, sum)
      cur[is.na(cur)] <- 0
      max(abs(cur - margins[[v]]))
    }, numeric(1)))
    if (disc < tol) {
      converged <- TRUE
      break
    }
  }
  list(weights = w, converged = converged, iterations = it,
       max_discrepancy = disc)
}

# ---------------------------------------------------------------------------
# Identification error
# ---------------------------------------------------------------------------

#' Correct a prevalence for identification error
#'
#' The Rogan-Gladen estimator,
#' \eqn{p = (p_{obs} + spec - 1) / (sens + spec - 1)}, which recovers a true
#' prevalence from an observed one when the classifier has known sensitivity
#' and specificity. Where group membership is recorded by an administrative
#' identifier rather than by self-identification, sensitivity below 1 means
#' members are recorded as non-members, and every rate computed on the
#' recorded counts is biased. Correction requires an external estimate of
#' sensitivity and specificity, usually from a linkage or validation study.
#'
#' @param p_obs Observed proportion classified as members.
#' @param sensitivity Probability a true member is classified as a member.
#' @param specificity Probability a true non-member is classified as a
#'   non-member.
#' @param n Sample size, used for the confidence interval. Default \code{NULL}
#'   for no interval. The interval propagates sampling error in
#'   \code{p_obs} only: \code{sensitivity} and \code{specificity} are
#'   treated as known constants. When they come from a validation study of
#'   finite size, their own uncertainty widens the true interval, and this
#'   one is optimistic.
#' @param conf Confidence level. Default \code{0.95}.
#' @return A list with \code{p_corrected}, \code{p_obs}, \code{youden}
#'   (\eqn{sens + spec - 1}), \code{ratio} (corrected over observed) and, when
#'   \code{n} is supplied, \code{se} and \code{ci}.
#' @references Rogan, W. J. and Gladen, B. (1978) Estimating prevalence from
#'   the results of a screening test. \emph{American Journal of Epidemiology}
#'   107(1), 71--76.
#' @seealso \code{\link{morie_misclass_count}}
#' @export
#' @examples
#' # 4% recorded, identifier misses a quarter of true members
#' morie_misclass_correct(0.04, sensitivity = 0.75, specificity = 0.999,
#'                        n = 10000)
morie_misclass_correct <- function(p_obs, sensitivity, specificity, n = NULL,
                                   conf = 0.95) {
  p_obs <- .spd_check_prob(p_obs, "p_obs")
  se_ <- .spd_check_prob(sensitivity, "sensitivity")
  sp_ <- .spd_check_prob(specificity, "specificity")
  youden <- se_ + sp_ - 1
  if (abs(youden) < .Machine$double.eps^0.5) {
    stop("sensitivity + specificity must differ from 1; the classifier carries no information",
         call. = FALSE)
  }
  p_true <- (p_obs + sp_ - 1) / youden
  out <- list(
    p_corrected = p_true,
    p_obs = p_obs,
    youden = youden,
    ratio = if (p_obs > 0) p_true / p_obs else NA_real_,
    sensitivity = se_,
    specificity = sp_
  )
  if (!is.null(n)) {
    n <- as.numeric(n)
    if (length(n) != 1L || is.na(n) || n <= 0) {
      stop("n must be a single positive number", call. = FALSE)
    }
    se_p <- sqrt(p_obs * (1 - p_obs) / n) / abs(youden)
    z <- .spd_z(conf)
    out$se <- se_p
    out$ci <- c(p_true - z * se_p, p_true + z * se_p)
    out$conf <- conf
  }
  out
}

#' Adjust an observed count for identification error
#'
#' Applies \code{\link{morie_misclass_correct}} to a count rather than a
#' proportion and reports the number of members the recorded data miss.
#'
#' @param observed_count Number of units recorded as members.
#' @param n Total number of units classified.
#' @param sensitivity Probability a true member is recorded as a member.
#' @param specificity Probability a true non-member is recorded as a
#'   non-member.
#' @return A list with \code{count_corrected}, \code{count_observed},
#'   \code{undercount} (corrected minus observed) and
#'   \code{undercount_pct} relative to the corrected count.
#' @seealso \code{\link{morie_misclass_correct}}
#' @export
#' @examples
#' morie_misclass_count(400, 10000, sensitivity = 0.75, specificity = 0.999)
morie_misclass_count <- function(observed_count, n, sensitivity, specificity) {
  observed_count <- as.numeric(observed_count)
  n <- as.numeric(n)
  if (length(observed_count) != 1L || is.na(observed_count) || observed_count < 0) {
    stop("observed_count must be a single non-negative number", call. = FALSE)
  }
  if (length(n) != 1L || is.na(n) || n <= 0 || n < observed_count) {
    stop("n must be a single positive number at least as large as observed_count",
         call. = FALSE)
  }
  fit <- morie_misclass_correct(observed_count / n, sensitivity, specificity)
  corrected <- fit$p_corrected * n
  list(
    count_corrected = corrected,
    count_observed = observed_count,
    undercount = corrected - observed_count,
    undercount_pct = if (corrected > 0) {
      100 * (corrected - observed_count) / corrected
    } else NA_real_
  )
}
