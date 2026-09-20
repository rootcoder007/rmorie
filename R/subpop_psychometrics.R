#' Measurement-equivalence planning and reporting across groups
#'
#' Tools for the measurement side of subgroup research: how many focal-group
#' respondents a differential item functioning (DIF) analysis needs, how to
#' report a Mantel-Haenszel result on the scale test agencies use, how to
#' decide a measurement-invariance sequence from fit indices, and what item
#' information implies for score precision. The estimators themselves are
#' elsewhere in the package (\code{\link{Difmh}}, \code{\link{Difsib}},
#' \code{\link{Irt1pl}}, \code{\link{Irtgrm}}); these functions plan and
#' interpret rather than fit.
#'
#' @name subpop_psychometrics
#' @keywords internal
NULL

# ---------------------------------------------------------------------------
# DIF planning
# ---------------------------------------------------------------------------

#' Focal-group sample size for detecting differential item functioning
#'
#' Sample size for a two-group comparison of item success probabilities, the
#' quantity a Mantel-Haenszel or logistic DIF test is powered on. Uses the
#' normal approximation for two independent proportions with unequal group
#' sizes.
#'
#' The practical point in subgroup work is that power is governed by the
#' \emph{smaller} group. A focal group that is a small share of the sample
#' caps DIF detection regardless of how large the total is, so a scale can be
#' reported as invariant simply because the analysis never had the power to
#' find otherwise.
#'
#' @param p_reference Probability of a correct or endorsed response in the
#'   reference group.
#' @param odds_ratio Odds ratio to detect, focal relative to reference. Values
#'   below 1 mean the item is harder for the focal group. Either this or
#'   \code{p_focal} must be supplied.
#' @param p_focal Probability in the focal group. Overrides
#'   \code{odds_ratio} when supplied.
#' @param ratio Reference-to-focal size ratio \eqn{n_{ref}/n_{focal}}. Default
#'   \code{1}.
#' @param power Target power. Default \code{0.8}.
#' @param alpha Two-sided significance level. Default \code{0.05}.
#' @return A list with \code{n_focal}, \code{n_reference}, \code{n_total},
#'   \code{p_focal}, \code{p_reference} and the inputs.
#' @seealso \code{\link{morie_dif_delta_mh}}, \code{\link{Difmh}}
#' @export
#' @examples
#' # Detect an odds ratio of 1.5 against a reference rate of 0.6
#' morie_dif_sample_size(0.6, odds_ratio = 1.5)
#' # The same target when the focal group is a fifth of the sample
#' morie_dif_sample_size(0.6, odds_ratio = 1.5, ratio = 4)
morie_dif_sample_size <- function(p_reference, odds_ratio = NULL,
                                  p_focal = NULL, ratio = 1, power = 0.8,
                                  alpha = 0.05) {
  p_r <- .spd_check_prob(p_reference, "p_reference")
  if (p_r <= 0 || p_r >= 1) stop("p_reference must be in (0, 1)", call. = FALSE)
  if (is.null(p_focal)) {
    if (is.null(odds_ratio)) {
      stop("supply either odds_ratio or p_focal", call. = FALSE)
    }
    or <- as.numeric(odds_ratio)
    if (length(or) != 1L || is.na(or) || or <= 0) {
      stop("odds_ratio must be a single positive number", call. = FALSE)
    }
    odds_r <- p_r / (1 - p_r)
    odds_f <- or * odds_r
    p_f <- odds_f / (1 + odds_f)
  } else {
    p_f <- .spd_check_prob(p_focal, "p_focal")
    or <- (p_f / (1 - p_f)) / (p_r / (1 - p_r))
  }
  if (isTRUE(all.equal(p_f, p_r))) {
    stop("the two groups have the same response probability; no effect to detect",
         call. = FALSE)
  }
  ratio <- as.numeric(ratio)
  if (length(ratio) != 1L || is.na(ratio) || ratio <= 0) {
    stop("ratio must be a single positive number", call. = FALSE)
  }
  power <- .spd_check_prob(power, "power")
  alpha <- .spd_check_prob(alpha, "alpha")
  if (power <= 0 || power >= 1) stop("power must be in (0, 1)", call. = FALSE)
  if (alpha <= 0 || alpha >= 1) stop("alpha must be in (0, 1)", call. = FALSE)
  z_a <- stats::qnorm(1 - alpha / 2)
  z_b <- stats::qnorm(power)
  p_bar <- (p_f + ratio * p_r) / (1 + ratio)
  term1 <- z_a * sqrt((1 + 1 / ratio) * p_bar * (1 - p_bar))
  term2 <- z_b * sqrt(p_f * (1 - p_f) + p_r * (1 - p_r) / ratio)
  n_focal <- (term1 + term2)^2 / (p_r - p_f)^2
  list(
    n_focal = n_focal,
    n_reference = ratio * n_focal,
    n_total = n_focal * (1 + ratio),
    p_focal = p_f,
    p_reference = p_r,
    odds_ratio = or,
    ratio = ratio,
    power = power,
    alpha = alpha
  )
}

#' Mantel-Haenszel DIF effect on the ETS delta scale
#'
#' Converts a Mantel-Haenszel odds ratio to the delta metric,
#' \eqn{\Delta_{MH} = -2.35 \ln(\alpha_{MH})}, and applies the
#' three-category classification used in operational test review: negligible
#' (A) below 1, moderate (B) from 1 to below 1.5, large (C) at 1.5 and above.
#' Negative delta indicates the item favours the focal group.
#'
#' The classification is a reporting convention, not a significance test; an
#' item may be classified C on a large sample and B on a small one for the
#' same underlying difference, which is why \code{\link{morie_dif_sample_size}}
#' belongs in the same planning step.
#'
#' @param or_mh Mantel-Haenszel odds ratio, or a vector of them.
#' @return A data frame with \code{or_mh}, \code{delta_mh}, \code{magnitude}
#'   (A, B or C) and \code{favours} (reference, focal or neither).
#' @references Holland, P. W. and Thayer, D. T. (1988) Differential item
#'   performance and the Mantel-Haenszel procedure. In H. Wainer and H. I.
#'   Braun (eds), \emph{Test Validity}, 129--145. Lawrence Erlbaum.
#' @seealso \code{\link{Difmh}}, \code{\link{morie_dif_sample_size}}
#' @export
#' @examples
#' morie_dif_delta_mh(c(1.0, 1.35, 1.9, 0.5))
morie_dif_delta_mh <- function(or_mh) {
  or_mh <- as.numeric(or_mh)
  if (!length(or_mh)) stop("or_mh must have at least one element", call. = FALSE)
  if (any(!is.na(or_mh) & or_mh <= 0)) {
    stop("or_mh must be positive", call. = FALSE)
  }
  delta <- -2.35 * log(or_mh)
  ad <- abs(delta)
  mag <- ifelse(is.na(delta), NA_character_,
                ifelse(ad < 1, "A", ifelse(ad < 1.5, "B", "C")))
  fav <- ifelse(is.na(delta), NA_character_,
                ifelse(abs(delta) < .Machine$double.eps^0.5, "neither",
                       ifelse(delta < 0, "focal", "reference")))
  data.frame(or_mh = or_mh, delta_mh = delta, magnitude = mag,
             favours = fav, stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# Measurement invariance
# ---------------------------------------------------------------------------

#' Compare nested measurement-invariance models
#'
#' Takes fit statistics for a sequence of increasingly constrained models
#' (configural, metric, scalar, and optionally strict) and reports, for each
#' step, the change in comparative fit index and root mean square error of
#' approximation together with the chi-square difference test.
#'
#' Invariance is what licenses comparing scores across groups at all. Without
#' at least metric invariance a difference in means between groups may be a
#' difference in what the instrument measures rather than in the trait, so
#' this step precedes any substantive group comparison.
#'
#' @param fits A data frame or list of rows, each with \code{model},
#'   \code{chisq}, \code{df}, \code{cfi} and \code{rmsea}. Rows must be ordered
#'   from least to most constrained.
#' @param cfi_cut Largest tolerable decrease in CFI. Default \code{0.01}.
#' @param rmsea_cut Largest tolerable increase in RMSEA. Default \code{0.015}.
#' @return A data frame with one row per comparison: \code{step},
#'   \code{delta_chisq}, \code{delta_df}, \code{p_value}, \code{delta_cfi},
#'   \code{delta_rmsea} and \code{supported}, the last being \code{TRUE} when
#'   both index changes stay inside their cut-offs.
#' @references Cheung, G. W. and Rensvold, R. B. (2002) Evaluating
#'   goodness-of-fit indexes for testing measurement invariance.
#'   \emph{Structural Equation Modeling} 9(2), 233--255.
#' @export
#' @examples
#' fits <- data.frame(
#'   model = c("configural", "metric", "scalar"),
#'   chisq = c(120.3, 128.9, 162.4),
#'   df = c(48, 54, 60),
#'   cfi = c(0.981, 0.979, 0.964),
#'   rmsea = c(0.041, 0.040, 0.052)
#' )
#' morie_invariance_compare(fits)
morie_invariance_compare <- function(fits, cfi_cut = 0.01, rmsea_cut = 0.015) {
  if (is.list(fits) && !is.data.frame(fits)) fits <- do.call(rbind, lapply(fits, as.data.frame))
  if (!is.data.frame(fits)) stop("fits must be a data frame", call. = FALSE)
  need <- c("model", "chisq", "df", "cfi", "rmsea")
  miss <- setdiff(need, names(fits))
  if (length(miss)) {
    stop(sprintf("fits is missing columns: %s", paste(miss, collapse = ", ")),
         call. = FALSE)
  }
  k <- nrow(fits)
  if (k < 2L) stop("at least two models are required to compare", call. = FALSE)
  chisq <- as.numeric(fits$chisq)
  df <- as.numeric(fits$df)
  if (any(diff(df) <= 0)) {
    stop("models must be ordered from least to most constrained (df must increase)",
         call. = FALSE)
  }
  cfi <- as.numeric(fits$cfi)
  rmsea <- as.numeric(fits$rmsea)
  cfi_cut <- as.numeric(cfi_cut)
  rmsea_cut <- as.numeric(rmsea_cut)
  idx <- seq_len(k - 1L)
  d_chisq <- chisq[idx + 1L] - chisq[idx]
  d_df <- df[idx + 1L] - df[idx]
  # A negative chi-square difference is possible with robust estimators and
  # carries no evidence against the constraint; report it rather than hide it.
  pval <- ifelse(d_chisq > 0, stats::pchisq(d_chisq, d_df, lower.tail = FALSE),
                 NA_real_)
  d_cfi <- cfi[idx + 1L] - cfi[idx]
  d_rmsea <- rmsea[idx + 1L] - rmsea[idx]
  data.frame(
    step = paste(as.character(fits$model)[idx], "->",
                 as.character(fits$model)[idx + 1L]),
    delta_chisq = d_chisq,
    delta_df = d_df,
    p_value = pval,
    delta_cfi = d_cfi,
    delta_rmsea = d_rmsea,
    supported = (-d_cfi <= cfi_cut) & (d_rmsea <= rmsea_cut),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# Score precision
# ---------------------------------------------------------------------------

#' Standard error of an item response theory score
#'
#' \eqn{SE(\theta) = 1 / \sqrt{I(\theta)}} for a test information value
#' \eqn{I(\theta)}. Exact, not an approximation: it is the definition of the
#' standard error in the item response theory measurement model.
#'
#' @param information Test information at the trait level of interest.
#' @return Numeric standard errors, one per element of \code{information}.
#' @seealso \code{\link{morie_irt_marginal_reliability}}
#' @export
#' @examples
#' morie_irt_theta_se(c(4, 9, 16))
morie_irt_theta_se <- function(information) {
  information <- as.numeric(information)
  if (!length(information)) stop("information must have at least one element", call. = FALSE)
  if (any(!is.na(information) & information <= 0)) {
    stop("information must be positive", call. = FALSE)
  }
  1 / sqrt(information)
}

#' Marginal reliability of an item response theory scale
#'
#' \eqn{1 - \bar{SE}^2 / \sigma^2_\theta}, the share of trait variance not
#' attributable to measurement error, averaged over the trait distribution.
#' Reporting this separately by group is the reliability counterpart of a DIF
#' analysis: a scale can be adequately reliable in one group and not in
#' another, and pooling conceals it.
#'
#' @param se Standard errors of the trait estimates, one per respondent.
#' @param var_theta Variance of the trait in the population. Default \code{1},
#'   the usual identification convention.
#' @return A single numeric reliability. Values at or below 0 indicate error
#'   variance at least as large as trait variance and are returned as such
#'   rather than truncated.
#' @seealso \code{\link{morie_irt_theta_se}}, \code{\link{CttAlpha}}
#' @export
#' @examples
#' morie_irt_marginal_reliability(c(0.3, 0.35, 0.4, 0.5))
morie_irt_marginal_reliability <- function(se, var_theta = 1) {
  se <- as.numeric(se)
  if (!length(se)) stop("se must have at least one element", call. = FALSE)
  if (any(!is.na(se) & se < 0)) stop("se must be non-negative", call. = FALSE)
  var_theta <- as.numeric(var_theta)
  if (length(var_theta) != 1L || is.na(var_theta) || var_theta <= 0) {
    stop("var_theta must be a single positive number", call. = FALSE)
  }
  1 - mean(se^2, na.rm = TRUE) / var_theta
}
