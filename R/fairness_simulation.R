# SPDX-License-Identifier: AGPL-3.0-or-later
#' Simulation primitives for the predictive-policing audit subsystem
#'
#' Pure base-R ports of the Noisy-OR detection model and the biased
#' crime-data simulator from \code{morie.fairness.simulation}, both
#' originally distilled from Barman & Barman (arXiv:2603.18987).
#' No optional dependencies.
#'
#' @name morie_fairness_simulation
NULL


.sim_result <- function(title, call, summary_lines = list(),
                         warnings = character(0),
                         interpretation = "", ...) {
  out <- list(
    title = title, call = call, summary_lines = summary_lines,
    warnings = warnings, interpretation = interpretation, ...
  )
  class(out) <- c("morie_fairness_result", "morie_rich_result", "list")
  out
}


#' Noisy-OR patrol-detection probabilities
#'
#' For each crime location, computes \code{1 - (1 - p_detect)^k} where
#' \code{k} is the number of officers within \code{radius}.
#'
#' @param crime_xy Numeric (n, 2) matrix of crime coordinates.
#' @param officer_xy Numeric (m, 2) matrix of officer coordinates.
#' @param radius Detection radius (positive).
#' @param p_detect Per-officer detection probability in (0, 1].
#' @param seed Optional integer; when supplied, a Bernoulli outcome is
#'   sampled per crime and returned in \code{$detected}.
#' @return \code{morie_fairness_result} with \code{$probabilities},
#'   \code{$officers_in_range}, optional \code{$detected}.
#' @export
morie_fairness_noisy_or_detection <- function(crime_xy, officer_xy,
                                               radius, p_detect = 0.85,
                                               seed = NULL) {
  crime <- as.matrix(crime_xy)
  officer <- as.matrix(officer_xy)
  if (!is.numeric(crime) || ncol(crime) != 2L) {
    stop("crime_xy must be an (n, 2) numeric matrix")
  }
  if (!is.numeric(officer) || ncol(officer) != 2L) {
    stop("officer_xy must be an (m, 2) numeric matrix")
  }
  if (!(p_detect > 0.0 && p_detect <= 1.0)) {
    stop("p_detect must be in (0, 1]")
  }
  if (radius <= 0) stop("radius must be positive")

  n <- nrow(crime)
  m <- nrow(officer)
  if (m == 0L) {
    k <- integer(n)
  } else {
    # n x m pairwise distances
    dx <- outer(crime[, 1L], officer[, 1L], "-")
    dy <- outer(crime[, 2L], officer[, 2L], "-")
    d <- sqrt(dx * dx + dy * dy)
    k <- rowSums(d <= radius)
  }
  prob <- 1.0 - (1.0 - p_detect) ^ k
  detected <- NULL
  if (!is.null(seed)) {
    set.seed(as.integer(seed))
    detected <- as.integer(stats::runif(length(prob)) < prob)
  }
  mean_p <- if (length(prob)) mean(prob) else NA_real_

  interp <- sprintf(
    "Across %d crime events and %d patrol officers, the mean detection probability is %.3f. %d crime(s) had no officer within radius (detection probability 0).",
    n, m, mean_p, sum(k == 0L)
  )

  .sim_result(
    "Noisy-OR Patrol Detection",
    sprintf("morie_fairness_noisy_or_detection(n=%d, m=%d, radius=%g, p_detect=%g)",
            n, m, radius, p_detect),
    summary_lines = list(
      Crimes = n, Officers = m,
      `Mean detection probability` = mean_p,
      `Per-officer probability` = p_detect
    ),
    interpretation = interp,
    n = n,
    value = mean_p,
    probabilities = prob,
    officers_in_range = k,
    detected = detected
  )
}


#' Synthetic predictive-policing dataset with a known disparity
#'
#' Generates per-record data with \code{area}, \code{group},
#' \code{true_outcome} (group-independent Bernoulli at \code{base_rate}),
#' \code{detected} (group-dependent), and \code{risk_score} (0--500,
#' shifted up by \code{bias * 100} points for non-reference groups).
#' The \code{bias} input is the ground truth the audits should recover.
#'
#' @param n Number of records.
#' @param groups Character vector of group labels (groups[1] = reference).
#' @param group_props Optional sampling proportions.
#' @param n_areas Number of areas (>= number of groups).
#' @param base_rate Reference-group favourable-outcome rate in `[0, 1]`.
#' @param bias Injected disparity in `[-1, 1]`.
#' @param seed Reproducibility seed.
#' @return A data.frame with columns area, group, true_outcome,
#'   detected, risk_score.
#' @export
morie_fairness_simulate_biased_crime_data <- function(n = 2000L,
                                                       groups = c("A", "B"),
                                                       group_props = NULL,
                                                       n_areas = 20L,
                                                       base_rate = 0.3,
                                                       bias = 0.5,
                                                       seed = 0L) {
  groups <- as.character(groups)
  G <- length(groups)
  if (G < 2L) stop("need at least two groups")
  if (!(base_rate >= 0.0 && base_rate <= 1.0)) {
    stop("base_rate must be in [0, 1]")
  }
  if (!(bias >= -1.0 && bias <= 1.0)) stop("bias must be in [-1, 1]")
  if (n_areas < G) stop("n_areas must be >= the number of groups")

  set.seed(as.integer(seed))
  if (is.null(group_props)) {
    props <- rep(1.0 / G, G)
  } else {
    props <- as.numeric(group_props)
    if (length(props) != G) {
      stop("group_props must have one entry per group")
    }
    props <- props / sum(props)
  }

  gi <- sample.int(G, size = as.integer(n), replace = TRUE, prob = props)
  group <- groups[gi]

  # Areas are group-segregated: area a belongs to group (a mod G) + 1.
  area_group <- ((seq_len(n_areas) - 1L) %% G) + 1L
  areas_by_group <- lapply(seq_len(G),
                           function(i) which(area_group == i))
  area_idx <- vapply(gi, function(i) {
    pool <- areas_by_group[[i]]
    pool[sample.int(length(pool), 1L)]
  }, integer(1))
  area <- sprintf("area_%02d", area_idx - 1L)

  true_outcome <- as.integer(stats::runif(n) < base_rate)
  det_rate <- ifelse(gi == 1L, base_rate, base_rate * (1.0 - bias))
  det_rate <- pmin(pmax(det_rate, 0.0), 1.0)
  detected <- as.integer(stats::runif(n) < det_rate)

  loc <- ifelse(gi == 1L, 250.0, 250.0 + bias * 100.0)
  risk_score <- pmin(pmax(stats::rnorm(n, mean = loc, sd = 40.0),
                          0.0), 500.0)

  data.frame(
    area = area, group = group,
    true_outcome = true_outcome,
    detected = detected,
    risk_score = risk_score,
    stringsAsFactors = FALSE
  )
}
