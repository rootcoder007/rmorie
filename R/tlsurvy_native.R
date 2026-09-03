# morie.fn -- function file (rootcoder007/morie)
# Targeted learning using adaptive survey sampling.
#
# N observations are available and N is too large to use. The response
# is not to approximate the estimator but to SAMPLE the data: select n
# of the N with unequal inclusion probabilities, and run a TMLE on the
# smaller set. The asymptotics are in both, with n -> infinity as N ->
# infinity and n/N -> 0 -- so the computational saving is not vanishing.
#
# A low-dimensional summary must be observed for everyone. Each O_i is
# summarised by a cheap V_i, and V_1,...,V_N are all available. That
# is what makes ADAPTIVE sampling possible: the design can use V to
# decide whom to look at, even though the expensive part of O is never
# read for the unsampled.
#
# Unequal probabilities, and the Horvitz-Thompson correction that
# follows. Selecting with probability pi_i and weighting by 1/pi_i
# keeps the estimator unbiased for the full-data quantity. Choosing
# pi_i proportional to the INFLUENCE an observation carries -- large
# |D*| given V -- minimises the variance for a given n, which is the
# whole point of adapting the design rather than sampling uniformly.
#
# The two error sources are separate, and reporting them separately
# matters. Sampling variance from having only n observations, and the
# full-data variance that would remain at n = N. The first is under
# the analyst's control through the design; the second is not.
# design_efficiency compares an adaptive design with uniform sampling
# at the same n, and the anchor requires the adaptive one to win
# when the influence is unevenly distributed and to tie when it is
# not -- the case where adaptation cannot help.
#
# References
# ----------
# van der Laan, M. J. & Rose, S. (2018) Targeted Learning in Data
# Science, Springer, doi:10.1007/978-3-319-65304-4. Chap. 29 (Chambaz,
# Joly & Mary): building a confidence interval for a real-valued
# pathwise differentiable parameter from N independent observations when
# N is so large that not all data can be used; the two-part response of
# selecting n among N randomly with UNEQUAL INCLUSION PROBABILITIES and
# adapting TMLE to the resulting smaller data set; the asymptotics with
# N to infinity and n to infinity such that n/N goes to zero; the
# selection as the random outcome of a survey sampling design; and the
# assumption that each observation is summarised by a low-dimensional
# V_i with V_1, ..., V_N all observed.
#
# Horvitz, D. G. & Thompson, D. J. (1952) "A Generalization of Sampling
# Without Replacement From a Finite Universe", Journal of the American
# Statistical Association 47(260), 663-685,
# doi:10.1080/01621459.1952.10483446.
#
# Chambaz, A., Joly, E. & Mary, X. (2018) "Targeted Learning Using
# Adaptive Survey Sampling", in Targeted Learning in Data Science,
# Springer, doi:10.1007/978-3-319-65304-4_29.

# Private constants
.tlsurvy_eps <- 1e-12
.tlsurvy_designs <- c("uniform", "proportional", "adaptive")

# inclusion_probabilities: Choose pi_i, summing to n.
# adaptive sets pi_i proportional to the expected influence given V_i,
# which minimises the variance for a fixed n. A floor keeps every unit
# reachable -- a zero inclusion probability makes the estimand
# unidentifiable for that stratum.
#' Inclusion_probabilities: Choose pi_i, summing to n
#'
#' adaptive sets pi_i proportional to the expected influence given V_i,
#' which minimises the variance for a fixed n. A floor keeps every unit
#' reachable -- a zero inclusion probability makes the estimand
#' unidentifiable for that stratum.
#'
#' @param V Coerced to numeric by the body, with \code{as.numeric}.
#' @param n Coerced to integer by the body, with \code{as.integer}.
#' @param design One of \code{"proportional"}, \code{"uniform"}. Defaults to \code{"adaptive"}.
#' @param influence Optional; may be \code{NULL}. Coerced to numeric by the body, with
#' \code{as.numeric}.
#' @param floor Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.01}.
#' @return A list with \code{pi}, \code{design}, \code{n_expected}, \code{N},
#' \code{min_pi}, \code{note}.
#' @export
morie_tlsurvy_inclusion_probabilities <- function(V, n, design = "adaptive",
                                                  influence = NULL,
                                                  floor = 0.01) {
  if (!(design %in% .tlsurvy_designs)) {
    stop(sprintf(
      "tlsurvy: design must be one of %s, got %s",
      paste(.tlsurvy_designs, collapse = ", "), design
    ))
  }
  v <- as.numeric(V)
  N <- length(v)
  nn <- as.integer(n)
  if (nn < 1 || nn > N) {
    stop(sprintf("tlsurvy: n must lie in 1..%d, got %d", N, nn))
  }
  if (design == "uniform") {
    base <- rep(1.0, N)
  } else if (design == "proportional") {
    base <- abs(v) + .tlsurvy_eps
  } else {
    if (is.null(influence)) {
      stop("tlsurvy: the adaptive design needs the expected influence given V")
    }
    base <- abs(as.numeric(influence)) + .tlsurvy_eps
    if (length(base) != N) {
      stop(sprintf(
        "tlsurvy: %d influence values for %d units",
        length(base), N
      ))
    }
  }
  # Rescale to sum exactly to n, iterating because capping at 1 and
  # flooring both remove mass that has to go somewhere -- a single
  # pass leaves the expected sample size short of n.
  s <- sum(base)
  pi <- nn * base / s
  fl <- as.numeric(floor)
  for (iter in seq_len(100)) {
    pi <- pmin(1.0, pmax(fl, pi))
    tot <- sum(pi)
    if (abs(tot - nn) < 1e-9) {
      break
    }
    free <- which(pi > fl & pi < 1.0)
    if (length(free) == 0) {
      break
    }
    slack <- nn - tot
    if (slack > 0) {
      room <- sum(1.0 - pi[free])
    } else {
      room <- sum(pi[free] - fl)
    }
    if (room <= .tlsurvy_eps) {
      break
    }
    for (i in free) {
      if (slack > 0) {
        share <- (1.0 - pi[i]) / room
      } else {
        share <- (pi[i] - fl) / room
      }
      pi[i] <- pi[i] + slack * share
    }
  }
  return(list(
    pi = pi,
    design = design,
    n_expected = sum(pi),
    N = N,
    min_pi = min(pi),
    note = "a zero inclusion probability makes that stratum unidentifiable, so the floor is not cosmetic"
  ))
}

# draw_sample: Poisson sampling: include unit i with probability pi_i.
#' Draw_sample: Poisson sampling: include unit i with probability pi_i
#'
#' A step of the tlsurvy_native implementation. Called by
#' \code{morie_tlsurvy_adaptive_survey_tmle}, \code{morie_tlsurvy_design_efficiency}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param pi Coerced to numeric by the body, with \code{as.numeric}.
#' @param seed Passed to \code{.ghc_rng}. Defaults to \code{0}.
#' @return A list with \code{selected}, \code{n}, \code{fraction}.
#' @export
morie_tlsurvy_draw_sample <- function(pi, seed = 0) {
  p <- as.numeric(pi)
  e <- .ghc_rng(seed)
  u <- .ghc_unif(e, length(p))
  idx <- which(u < p)
  if (length(idx) == 0) {
    stop("tlsurvy: the draw selected nothing; raise the inclusion probabilities")
  }
  return(list(
    selected = idx,
    n = length(idx),
    fraction = length(idx) / as.numeric(length(p))
  ))
}

# horvitz_thompson: The design-unbiased mean: (1/N) sum_{i in S} y_i/pi_i.
# Unbiased for the FULL-data mean, which is the quantity of interest
# -- the sample is a computational device, not the population.
#' Horvitz_thompson: The design-unbiased mean: (1/N) sum_\{i in S\}
#' y_i/pi_i
#'
#' Unbiased for the FULL-data mean, which is the quantity of interest --
#' the sample is a computational device, not the population.
#'
#' @param values Coerced to numeric by the body, with \code{as.numeric}.
#' @param pi Coerced to numeric by the body, with \code{as.numeric}.
#' @param selected Coerced to integer by the body, with \code{as.integer}.
#' @param N Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{estimate}, \code{se}, \code{n_used}, \code{N}.
#' @export
morie_tlsurvy_horvitz_thompson <- function(values, pi, selected, N = NULL) {
  y <- as.numeric(values)
  p <- as.numeric(pi)
  idx <- as.integer(selected)
  n_total <- if (is.null(N)) length(p) else as.integer(N)
  if (any(p[idx] <= 0.0)) {
    stop("tlsurvy: a selected unit has zero inclusion probability")
  }
  est <- sum(y[idx] / p[idx]) / n_total
  var <- sum((1.0 - p[idx]) * (y[idx] / p[idx])^2) / (n_total^2)
  return(list(
    estimate = est,
    se = sqrt(max(var, 0.0)),
    n_used = length(idx),
    N = n_total
  ))
}

# design_efficiency: Adaptive against uniform sampling at the same n.
# Adaptation can only help when the influence is unevenly spread;
# where it is flat the two coincide, and claiming otherwise would be
# claiming something for nothing.
#' Design_efficiency: Adaptive against uniform sampling at the same n
#'
#' Adaptation can only help when the influence is unevenly spread; where
#' it is flat the two coincide, and claiming otherwise would be claiming
#' something for nothing.
#'
#' @param values Coerced to numeric by the body, with \code{as.numeric}.
#' @param influence Passed to \code{morie_tlsurvy_inclusion_probabilities}.
#' @param n Passed to \code{morie_tlsurvy_inclusion_probabilities}.
#' @param seed Passed to \code{morie_tlsurvy_draw_sample}. Defaults to \code{0}.
#' @return A list with \code{uniform_se}, \code{adaptive_se}, \code{ratio}, \code{note}.
#' @export
morie_tlsurvy_design_efficiency <- function(values, influence, n, seed = 0) {
  y <- as.numeric(values)
  out <- list()
  for (d in c("uniform", "adaptive")) {
    pi_res <- if (d == "uniform") {
      morie_tlsurvy_inclusion_probabilities(y, n, d, NULL)
    } else {
      morie_tlsurvy_inclusion_probabilities(y, n, d, influence)
    }
    s <- morie_tlsurvy_draw_sample(pi_res$pi, seed)
    out[[d]] <- morie_tlsurvy_horvitz_thompson(y, pi_res$pi, s$selected)
  }
  ratio <- if (out[["uniform"]]$se > 0) {
    out[["adaptive"]]$se / out[["uniform"]]$se
  } else {
    NaN
  }
  return(list(
    uniform_se = out[["uniform"]]$se,
    adaptive_se = out[["adaptive"]]$se,
    ratio = ratio,
    note = "with a flat influence the designs coincide -- adaptation cannot buy anything there"
  ))
}

# adaptive_survey_tmle: Sample by the adaptive design, then run the
# estimator on the sample. Reports both error sources: the sampling
# variance from using n of N, and the estimator's own standard error.
#' Adaptive_survey_tmle: Sample by the adaptive design, then run the
#'
#' estimator on the sample. Reports both error sources: the sampling
#' variance from using n of N, and the estimator\'s own standard error.
#'
#' @param V Passed to \code{morie_tlsurvy_inclusion_probabilities}.
#' @param influence_proxy Passed to \code{morie_tlsurvy_inclusion_probabilities}.
#' @param full_estimator Accepted by the signature and not used anywhere in the body.
#' @param n Passed to \code{morie_tlsurvy_inclusion_probabilities}.
#' @param seed Passed to \code{morie_tlsurvy_draw_sample}. Defaults to \code{0}.
#' @return A list with \code{estimate}, \code{psi}, \code{se_estimator}, \code{n_used},
#' \code{N}, \code{sampling_fraction}, \code{inclusion_probabilities}, \code{method},
#' \code{note}.
#' @export
morie_tlsurvy_adaptive_survey_tmle <- function(V, influence_proxy,
                                               full_estimator, n,
                                               seed = 0) {
  pi_res <- morie_tlsurvy_inclusion_probabilities(
    V, n, "adaptive",
    influence_proxy
  )
  s <- morie_tlsurvy_draw_sample(pi_res$pi, seed)
  r <- full_estimator(s$selected, 1.0 / pi_res$pi[s$selected])
  se_val <- if (!is.null(r$se)) as.numeric(r$se) else NaN
  return(list(
    estimate = as.numeric(r$estimate),
    psi = as.numeric(r$estimate),
    se_estimator = se_val,
    n_used = s$n,
    N = length(pi_res$pi),
    sampling_fraction = s$fraction,
    inclusion_probabilities = pi_res$pi,
    method = "TMLE on an adaptive survey sample; van der Laan & Rose (2018) Chap. 29",
    note = "asymptotics in both n and N with n/N -> 0, so the computational saving does not vanish"
  ))
}

# cheatsheet: Brief description of the module.
#' Cheatsheet: Brief description of the module
#'
#' A step of the tlsurvy_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @return A character value.
#' @export
morie_tlsurvy_cheatsheet <- function() {
  return("tlsurvy: N too large to use, so SAMPLE the data rather than approximate the estimator -- select n of N with UNEQUAL inclusion probabilities and run TMLE on the sample, with n/N -> 0 so the saving persists. A cheap low-dimensional V is observed for ALL N, which is what makes the design adaptive: set pi_i proportional to the expected INFLUENCE given V and the variance is minimised for that n. Weight by 1/pi (Horvitz-Thompson) to stay unbiased for the FULL-data parameter. Where the influence is flat, adaptation buys nothing.")
}

# Compact alias per ledger/NAMING.md
morie_tlsurvy_adaptivesurveytmle <- morie_tlsurvy_adaptive_survey_tmle
