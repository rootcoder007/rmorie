# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Reliability, IRT ability estimation and meta-analysis.
#
# R mirror of morie.fn.{icc1k,icc2k,icc3k,mleth,mapth,eapth,wleth,
# theteap,theteap2,mapaule,mareml,marve,maloo}.
#
# Three families, one recurring shape: a variance decomposition whose
# COMPONENTS are the quantity of interest, and whose reported value
# depends on assumptions the arithmetic cannot see.
#
#   ICC: Shrout and Fleiss (1979) give SIX coefficients, not one, and
#   they are different numbers on the same data. Reporting "the ICC"
#   without the case is the field's standard error.
#
#   IRT: the maximum-likelihood ability estimate does NOT exist for a
#   perfect response pattern. MAP, EAP and Warm's WLE each return a
#   finite number there by a different mechanism.
#
#   Meta-analysis: the weights are 1/(v_i + tau^2), so a different
#   tau^2 estimator is a different POOLED EFFECT, not a footnote.

# Two-way ANOVA mean squares for a complete crossed subjects-by-raters
# table. An unbalanced table makes the mean squares non-orthogonal and
# every downstream ICC ill-defined, so it is an error rather than a
# number from whatever cells happen to be present.
#' Two-way ANOVA mean squares for a complete crossed subjects-by-raters
#'
#' table. An unbalanced table makes the mean squares non-orthogonal and
#' every downstream ICC ill-defined, so it is an error rather than a
#' number from whatever cells happen to be present.
#'
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @param subject Coerced to vector by the body, with \code{as.vector}.
#' @param rater Coerced to vector by the body, with \code{as.vector}.
#' @return A list with \code{MSR}, \code{MSC}, \code{MSE}, \code{MSW}, \code{n}, \code{k}, \code{matrix}.
#' @export
.psy_anova2 <- function(y, subject, rater) {
  yv <- as.numeric(y)
  s <- as.vector(subject)
  r <- as.vector(rater)
  if (length(yv) != length(s) || length(yv) != length(r)) {
    stop("y, subject and rater must have the same length.", call. = FALSE)
  }
  subs <- unique(s)
  rats <- unique(r)
  n <- length(subs)
  k <- length(rats)
  if (n < 2L || k < 2L) {
    stop(sprintf("need at least 2 subjects and 2 raters, got %d and %d.",
                 n, k), call. = FALSE)
  }
  if (length(yv) != n * k) {
    stop(sprintf(paste("the design must be complete and crossed: %d subjects",
                       "x %d raters needs %d observations, got %d."),
                 n, k, n * k, length(yv)), call. = FALSE)
  }
  M <- matrix(NA_real_, n, k)
  for (i in seq_along(yv)) {
    M[match(s[i], subs), match(r[i], rats)] <- yv[i]
  }
  if (any(is.na(M))) {
    stop("the subject-by-rater table has empty cells.", call. = FALSE)
  }
  grand <- mean(M)
  ss_r <- k * sum((rowMeans(M) - grand)^2)
  ss_c <- n * sum((colMeans(M) - grand)^2)
  ss_t <- sum((M - grand)^2)
  list(MSR = ss_r / (n - 1), MSC = ss_c / (k - 1),
       MSE = (ss_t - ss_r - ss_c) / ((n - 1) * (k - 1)),
       MSW = (ss_t - ss_r) / (n * (k - 1)),
       n = n, k = k, matrix = M)
}

# Spearman-Brown: reliability of a mean of k measurements. Always at
# least the single-measure value, which is why every average-measure
# ICC exceeds its single-measure counterpart.
#' Spearman-Brown: reliability of a mean of k measurements. Always at
#'
#' least the single-measure value, which is why every average-measure
#' ICC exceeds its single-measure counterpart.
#'
#' @param icc1 Numeric; combined arithmetically in the body.
#' @param k Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.psy_spearman_brown <- function(icc1, k) {
  den <- 1 + (k - 1) * icc1
  if (den == 0) return(NA_real_)
  k * icc1 / den
}

# 3PL item response function. c is the lower asymptote (guessing):
# with c > 0 the curve never reaches zero.
#' 3PL item response function. c is the lower asymptote (guessing):
#'
#' with c > 0 the curve never reaches zero.
#'
#' @param theta A vector; its length is taken.
#' @param a Numeric; combined arithmetically in the body.
#' @param b A vector; its length is taken.
#' @param cc A count; the body uses it as \code{matrix(...)}.
#' @return A numeric value.
#' @export
.psy_p3pl <- function(theta, a, b, cc) {
  z <- pmin(pmax(outer(theta, b, function(t, bb) a * (t - bb)), -500), 500)
  sweep(1 / (1 + exp(-z)), 2L, 1 - cc, "*") +
    matrix(cc, nrow = length(theta), ncol = length(b), byrow = TRUE)
}

#' .psy_dp3pl
#'
#' A step of the psycho_native implementation. Called by \code{.psy_info}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param theta Passed to \code{.psy_p3pl}.
#' @param a Numeric; combined arithmetically in the body.
#' @param b Passed to \code{.psy_p3pl}.
#' @param cc Numeric; combined arithmetically in the body.
#' @return The value of \code{sweep}.
#' @export
.psy_dp3pl <- function(theta, a, b, cc) {
  P <- .psy_p3pl(theta, a, b, cc)
  star <- sweep(P, 2L, cc, "-")
  star <- sweep(star, 2L, pmax(1 - cc, 1e-12), "/")
  sweep(star * (1 - star), 2L, a * (1 - cc), "*")
}

#' .psy_items
#'
#' A step of the psycho_native implementation. Called by \code{morie_psy_eap_theta}, \code{morie_psy_map_theta}, \code{morie_psy_mle_theta} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param m A count; the body uses it as \code{rep(...)}.
#' @param a Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param b Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param cc Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{a}, \code{b}, \code{c}.
#' @export
.psy_items <- function(m, a, b, cc) {
  if (is.null(b)) stop("item difficulties b are required.", call. = FALSE)
  bv <- as.numeric(b)
  av <- if (is.null(a)) rep(1, m) else as.numeric(a)
  cv <- if (is.null(cc)) rep(0, m) else as.numeric(cc)
  if (length(bv) != m || length(av) != m || length(cv) != m) {
    stop("a, b, c must each have one entry per item.", call. = FALSE)
  }
  if (any(cv < 0 | cv >= 1)) {
    stop("guessing parameters must lie in [0, 1).", call. = FALSE)
  }
  list(a = av, b = bv, c = cv)
}

#' .psy_check_y
#'
#' A step of the psycho_native implementation. Called by \code{morie_psy_eap_theta}, \code{morie_psy_map_theta}, \code{morie_psy_mle_theta} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @return The value of \code{yv}, as built in the body.
#' @export
.psy_check_y <- function(y) {
  yv <- as.numeric(y)
  if (!all(yv %in% c(0, 1))) {
    stop("responses must be binary 0/1.", call. = FALSE)
  }
  yv
}

#' .psy_ll
#'
#' A step of the psycho_native implementation. Called by \code{morie_psy_eap_theta}, \code{morie_psy_map_theta}, \code{morie_psy_mle_theta} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param grid Passed to \code{.psy_p3pl}.
#' @param yv A matrix; passed to \code{\%*\%}.
#' @param it A list; the body reads \code{$a}, \code{$b}, \code{$c} from it.
#' @return A vector, from \code{as.numeric}.
#' @export
.psy_ll <- function(grid, yv, it) {
  P <- pmin(pmax(.psy_p3pl(grid, it$a, it$b, it$c), 1e-12), 1 - 1e-12)
  as.numeric(log(P) %*% yv + log(1 - P) %*% (1 - yv))
}

#' .psy_info
#'
#' A step of the psycho_native implementation. Called by \code{morie_psy_map_theta}, \code{morie_psy_mle_theta}, \code{morie_psy_wle_theta}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param theta Passed to \code{.psy_p3pl}.
#' @param it A list; the body reads \code{$a}, \code{$b}, \code{$c} from it.
#' @return A vector, from \code{as.numeric}.
#' @export
.psy_info <- function(theta, it) {
  P <- pmin(pmax(.psy_p3pl(theta, it$a, it$b, it$c), 1e-12), 1 - 1e-12)
  dP <- .psy_dp3pl(theta, it$a, it$b, it$c)
  as.numeric(rowSums(dP^2 / (P * (1 - P))))
}

#' .psy_fixed_pool
#'
#' A step of the psycho_native implementation. Called by \code{.psy_dl}, \code{morie_psy_ma_paule_mandel}, \code{morie_psy_ma_reml}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param yi Coerced to numeric by the body, with \code{as.numeric}.
#' @param vi Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{mu}, \code{Q}, \code{w}, \code{k}.
#' @export
.psy_fixed_pool <- function(yi, vi) {
  y <- as.numeric(yi)
  v <- as.numeric(vi)
  if (length(y) != length(v)) {
    stop(sprintf("yi has %d entries and vi has %d.", length(y), length(v)),
         call. = FALSE)
  }
  if (length(y) < 2L) {
    stop(sprintf("need at least 2 studies, got %d.", length(y)),
         call. = FALSE)
  }
  if (any(v <= 0)) {
    stop("every within-study variance must be positive.", call. = FALSE)
  }
  w <- 1 / v
  mu <- sum(w * y) / sum(w)
  list(mu = mu, Q = sum(w * (y - mu)^2), w = w, k = length(y))
}

# DerSimonian-Laird: closed-form, downward-biased, and the truncation
# at zero is not symmetric -- a biased tau^2 that hits the floor
# understates heterogeneity AND overstates precision together.
#' DerSimonian-Laird: closed-form, downward-biased, and the truncation
#'
#' at zero is not symmetric -- a biased tau^2 that hits the floor
#' understates heterogeneity AND overstates precision together.
#'
#' @param yi Passed to \code{.psy_fixed_pool}.
#' @param vi Passed to \code{.psy_fixed_pool}.
#' @return A numeric value.
#' @export
.psy_dl <- function(yi, vi) {
  f <- .psy_fixed_pool(yi, vi)
  den <- sum(f$w) - sum(f$w^2) / sum(f$w)
  if (den <= 0) return(0)
  max(0, (f$Q - (f$k - 1)) / den)
}


#' ICC(1,k): one-way random, average of k ratings
#'
#' Shrout and Fleiss (1979) Case 1: `(MSR - MSW)/MSR`, the
#' reliability of the MEAN of k ratings when each target is rated by
#' a DIFFERENT randomly chosen set of raters. Rater identity is not
#' crossed with target, so no rater effect is estimable and MSW pools
#' rater with error -- which is why ICC(1,*) is the SMALLEST of the
#' three cases on the same data. If the same raters rated everyone,
#' this is the wrong coefficient.
#'
#' @param y ratings.
#' @param cluster target identifier.
#' @param rater accepted for signature parity; Case 1 does not model
#'   rater identity and the output records that it was ignored.
#' @return list: value (ICC(1,k)), icc_single, k, n, MSR, MSW, case,
#'   design_assumption, method.
#' @references Shrout and Fleiss (1979), *Psychological Bulletin*
#'   86:420-428, Case 1 and Table 4.
#' @examples
#' morie_psy_icc1k(c(9, 2, 5, 8, 6, 1, 3, 2), rep(1:2, each = 4))$value
#' @export
morie_psy_icc1k <- function(y, cluster, rater = NULL) {
  yv <- as.numeric(y)
  g <- as.vector(cluster)
  if (length(yv) != length(g)) {
    stop(sprintf("y has %d entries and cluster has %d.", length(yv),
                 length(g)), call. = FALSE)
  }
  groups <- unique(g)
  n <- length(groups)
  if (n < 2L) stop(sprintf("need at least 2 targets, got %d.", n),
                   call. = FALSE)
  sizes <- vapply(groups, function(v) sum(g == v), numeric(1))
  if (length(unique(sizes)) != 1L) {
    stop(paste("ICC(1,k) assumes k ratings per target; the group sizes",
               "differ. An unbalanced one-way design needs a",
               "variance-components fit, not this formula."), call. = FALSE)
  }
  k <- as.integer(sizes[1L])
  if (k < 2L) stop(sprintf("need at least 2 ratings per target, got %d.", k),
                   call. = FALSE)
  grand <- mean(yv)
  means <- vapply(groups, function(v) mean(yv[g == v]), numeric(1))
  ms_r <- k * sum((means - grand)^2) / (n - 1)
  ss_w <- sum(vapply(seq_along(groups), function(i) {
    sum((yv[g == groups[i]] - means[i])^2)
  }, numeric(1)))
  ms_w <- ss_w / (n * (k - 1))
  if (ms_r <= 0) {
    stop("between-target mean square is zero; no reliability is defined.",
         call. = FALSE)
  }
  list(value = (ms_r - ms_w) / ms_r,
       icc_single = (ms_r - ms_w) / (ms_r + (k - 1) * ms_w),
       k = k, n = n, MSR = ms_r, MSW = ms_w, case = "ICC(1,k)",
       design_assumption = paste("each target rated by a DIFFERENT randomly",
                                 "chosen set of raters"),
       smallest_because = paste("rater and error variance cannot be",
                                "separated, so MSW pools them and systematic",
                                "rater differences are charged to error"),
       rater_ignored = !is.null(rater),
       method = "Shrout-Fleiss (1979) ICC(1,k) = (MSR - MSW)/MSR")
}


#' ICC(2,k): two-way random, average measure
#'
#' Shrout and Fleiss (1979) Case 2:
#' `(MSR - MSE)/(MSR + (MSC - MSE)/n)`, for raters who are a RANDOM
#' SAMPLE and results meant to generalise to other raters. The
#' `(MSC - MSE)/n` term charges SYSTEMATIC RATER DIFFERENCES against
#' reliability, because a future rater brings their own bias.
#' ICC(3,k) drops it and is always at least as large -- reporting
#' Case 3 for sampled raters is the standard way to overstate
#' reliability, so both are returned.
#'
#' @param y ratings.
#' @param subject,rater identifiers; a complete crossed design.
#' @return list: value (ICC(2,k)), icc_single, icc3k, rater_penalty,
#'   k, n, MSR, MSC, MSE, case, method.
#' @references Shrout and Fleiss (1979), Case 2 and Table 4;
#'   McGraw and Wong (1996), *Psychological Methods* 1:30-46.
#' @examples
#' y <- c(9, 2, 5, 8, 6, 1, 3, 2, 8, 4, 6, 8)
#' morie_psy_icc2k(y, rep(1:3, each = 4), rep(1:4, 3))$value
#' @export
morie_psy_icc2k <- function(y, subject, rater) {
  a <- .psy_anova2(y, subject, rater)
  n <- a$n
  k <- a$k
  den_k <- a$MSR + (a$MSC - a$MSE) / n
  if (den_k <= 0) {
    stop("the ICC(2,k) denominator is not positive.", call. = FALSE)
  }
  den_1 <- a$MSR + (k - 1) * a$MSE + k * (a$MSC - a$MSE) / n
  icc3k <- if (a$MSR > 0) (a$MSR - a$MSE) / a$MSR else NA_real_
  list(value = (a$MSR - a$MSE) / den_k,
       icc_single = if (den_1 > 0) (a$MSR - a$MSE) / den_1 else NA_real_,
       icc3k = icc3k,
       rater_penalty = icc3k - (a$MSR - a$MSE) / den_k,
       k = k, n = n, MSR = a$MSR, MSC = a$MSC, MSE = a$MSE,
       case = "ICC(2,k)",
       design_assumption = paste("raters are a RANDOM SAMPLE and the result",
                                 "should generalise to other raters"),
       why_smaller_than_icc3 = paste("the (MSC - MSE)/n term charges",
                                     "systematic rater differences against",
                                     "reliability; ICC(3,k) drops it"),
       method = "Shrout-Fleiss (1979) ICC(2,k), two-way random, average measure")
}


#' ICC(3,k): two-way mixed, average measure
#'
#' Shrout and Fleiss (1979) Case 3: `1 - MSE/MSR`, for when THESE
#' raters are the only ones of interest. No rater term appears, so
#' systematic offsets cost nothing -- this is a CONSISTENCY
#' coefficient, not an agreement one. Two raters differing by a
#' constant on every target score near 1 here and well below it on
#' ICC(2,k); both are returned with the measured offset so the choice
#' is informed rather than habitual.
#'
#' @param y ratings.
#' @param subject,rater identifiers; a complete crossed design.
#' @return list: value (ICC(3,k)), icc_single, icc2k,
#'   max_rater_offset, k, n, MSR, MSC, MSE, case, method.
#' @references Shrout and Fleiss (1979), Case 3 and Table 4;
#'   McGraw and Wong (1996).
#' @examples
#' y <- c(9, 2, 5, 8, 6, 1, 3, 2, 8, 4, 6, 8)
#' morie_psy_icc3k(y, rep(1:3, each = 4), rep(1:4, 3))$value
#' @export
morie_psy_icc3k <- function(y, subject, rater) {
  a <- .psy_anova2(y, subject, rater)
  n <- a$n
  k <- a$k
  if (a$MSR <= 0) {
    stop("between-target mean square is zero; no reliability is defined.",
         call. = FALSE)
  }
  den2 <- a$MSR + (a$MSC - a$MSE) / n
  col_means <- colMeans(a$matrix)
  list(value = (a$MSR - a$MSE) / a$MSR,
       icc_single = (a$MSR - a$MSE) / (a$MSR + (k - 1) * a$MSE),
       icc2k = if (den2 > 0) (a$MSR - a$MSE) / den2 else NA_real_,
       max_rater_offset = max(col_means) - min(col_means),
       k = k, n = n, MSR = a$MSR, MSC = a$MSC, MSE = a$MSE,
       case = "ICC(3,k)",
       design_assumption = "THESE raters are the only ones of interest",
       consistency_not_agreement = paste("systematic rater offsets are not",
                                         "charged; report Case 3 only when",
                                         "ratings are used relatively, not",
                                         "interchangeably"),
       method = "Shrout-Fleiss (1979) ICC(3,k), two-way mixed, average measure")
}


#' Maximum-likelihood ability estimate
#'
#' The theta maximising the 3PL log-likelihood. **No finite estimate
#' exists for an all-correct or all-wrong pattern** -- the likelihood
#' is monotone and its supremum is at infinity. This returns
#' `finite = FALSE` and an infinite theta rather than the edge of a
#' search interval dressed as an estimate, which is the standard
#' silent failure. The 3PL likelihood can also be multimodal under
#' guessing (Samejima 1973), so a dense scan precedes local
#' refinement.
#'
#' @param y binary responses.
#' @param a,b,c item parameters; b required.
#' @param bounds search interval.
#' @return list: theta, se, finite, information, loglik,
#'   n_local_maxima, pattern, n_items, method.
#' @references Lord (1980), Ch. 4; Samejima (1973), *Psychometrika*
#'   38:221-233; Yen, Burket and Sykes (1991).
#' @examples
#' morie_psy_mle_theta(c(1, 1, 0, 0), b = c(-1, -0.5, 0.5, 1))$theta
#' @export
morie_psy_mle_theta <- function(y, a = NULL, b = NULL, c = NULL,
                                bounds = c(-6, 6)) {
  yv <- .psy_check_y(y)
  m <- length(yv)
  it <- .psy_items(m, a, b, c)
  allc <- all(yv == 1)
  allw <- all(yv == 0)
  if (allc || allw) {
    return(list(theta = if (allc) Inf else -Inf, se = Inf, finite = FALSE,
                information = 0, loglik = NA_real_, n_local_maxima = 0L,
                pattern = if (allc) "all correct" else "all incorrect",
                n_items = m,
                why_infinite = paste("the likelihood is monotone in theta",
                                     "for a perfect pattern, so its supremum",
                                     "is at infinity and no maximum exists"),
                method = "3PL maximum likelihood (no finite maximum here)"))
  }
  grid <- seq(bounds[1L], bounds[2L], length.out = 4001L)
  ll <- .psy_ll(grid, yv, it)
  interior <- which(ll[-c(1, length(ll))] > ll[-c(length(ll) - 1, length(ll))] &
                      ll[-c(1, length(ll))] >= ll[-c(1, 2)]) + 1L
  i <- which.max(ll)
  negll <- function(t) -.psy_ll(t, yv, it)
  left <- grid[max(i - 1L, 1L)]
  right <- grid[min(i + 1L, length(grid))]
  opt <- stats::optimize(negll, c(left, right), tol = 1e-10)
  th <- opt$minimum
  info <- .psy_info(th, it)
  list(theta = th, se = if (info > 0) 1 / sqrt(info) else Inf,
       finite = TRUE, information = info, loglik = -opt$objective,
       n_local_maxima = length(interior),
       multimodality_note = paste("the 3PL likelihood can be multimodal when",
                                  "guessing is present, so a dense scan",
                                  "precedes local refinement"),
       pattern = "mixed", n_items = m,
       method = "3PL maximum-likelihood theta by scan plus local refinement")
}


#' Maximum a posteriori ability estimate
#'
#' The mode of the posterior under a normal prior. The prior makes
#' the posterior log-concave in the tails whatever the pattern, so a
#' MAP estimate EXISTS for every pattern including perfect ones,
#' where the ML estimate does not -- the reason MAP is the usual
#' production choice. The price is SHRINKAGE toward the prior mean,
#' largest where information is smallest, and `shrinkage_vs_ml`
#' reports the gap rather than leaving it assumed away. The standard
#' error uses the posterior curvature, so the prior's information
#' narrows it.
#'
#' @param y binary responses.
#' @param a,b,c item parameters; b required.
#' @param prior `c(mean, sd)`.
#' @param bounds search interval.
#' @return list: theta, se, prior_mean, prior_sd, information,
#'   posterior_information, shrinkage_vs_ml,
#'   exists_for_perfect_patterns, n_items, method.
#' @references Samejima (1969); Bock and Aitkin (1981),
#'   *Psychometrika* 46:443-459; Mislevy (1986).
#' @examples
#' morie_psy_map_theta(rep(1, 4), b = c(-1, -0.5, 0.5, 1))$theta
#' @export
morie_psy_map_theta <- function(y, a = NULL, b = NULL, c = NULL,
                                prior = c(0, 1), bounds = c(-6, 6)) {
  yv <- .psy_check_y(y)
  m <- length(yv)
  it <- .psy_items(m, a, b, c)
  mu <- as.numeric(prior[1L])
  sd <- as.numeric(prior[2L])
  if (sd <= 0) {
    stop(sprintf("the prior standard deviation must be positive, got %g.",
                 sd), call. = FALSE)
  }
  grid <- seq(bounds[1L], bounds[2L], length.out = 8001L)
  post <- .psy_ll(grid, yv, it) - (grid - mu)^2 / (2 * sd^2)
  i <- which.max(post)
  th <- grid[i]
  if (i > 1L && i < length(grid)) {
    den <- post[i - 1L] - 2 * post[i] + post[i + 1L]
    if (den != 0) {
      th <- grid[i] - 0.5 * (grid[2L] - grid[1L]) *
        (post[i + 1L] - post[i - 1L]) / den
    }
  }
  info <- .psy_info(th, it)
  shrink <- NULL
  if (!(all(yv == 1) || all(yv == 0))) {
    ml <- morie_psy_mle_theta(y, a = it$a, b = it$b, c = it$c,
                              bounds = bounds)
    if (ml$finite) shrink <- th - ml$theta
  }
  list(theta = th, se = 1 / sqrt(info + 1 / sd^2),
       prior_mean = mu, prior_sd = sd, information = info,
       posterior_information = info + 1 / sd^2,
       shrinkage_vs_ml = shrink, exists_for_perfect_patterns = TRUE,
       why_it_exists = paste("the normal prior makes the posterior",
                             "log-concave in the tails whatever the pattern"),
       shrinkage_note = paste("the price is bias toward the prior mean,",
                              "largest where information is smallest"),
       n_items = m,
       method = "MAP (Bayes modal) theta under a normal prior")
}


#' Expected a posteriori ability estimate
#'
#' The posterior MEAN by Gauss-Hermite quadrature (Bock-Mislevy
#' 1982). Three properties distinguish it from MAP: no optimisation
#' at all (two weighted sums, so no convergence or multimodality
#' problem -- the reason adaptive testing prefers it), more shrinkage
#' toward the prior mean since it is the minimum-MSE estimator, and a
#' genuine posterior standard deviation rather than a curvature
#' approximation. The node count is the only tuning parameter and is
#' reported, because too few nodes silently biases the tails.
#'
#' @param y binary responses.
#' @param a,b,c item parameters; b required.
#' @param prior `c(mean, sd)`.
#' @param n_nodes quadrature nodes.
#' @return list: theta, se, posterior_sd, prior_mean, prior_sd,
#'   n_nodes, no_optimisation, n_items, method.
#' @references Bock and Mislevy (1982), *Applied Psychological
#'   Measurement* 6:431-444.
#' @examples
#' morie_psy_eap_theta(rep(1, 4), b = c(-1, -0.5, 0.5, 1))$theta
#' @export
morie_psy_eap_theta <- function(y, a = NULL, b = NULL, c = NULL,
                                prior = c(0, 1), n_nodes = 61L) {
  yv <- .psy_check_y(y)
  m <- length(yv)
  it <- .psy_items(m, a, b, c)
  mu <- as.numeric(prior[1L])
  sd <- as.numeric(prior[2L])
  if (sd <= 0) {
    stop(sprintf("the prior standard deviation must be positive, got %g.",
                 sd), call. = FALSE)
  }
  nn <- as.integer(n_nodes)
  if (is.na(nn) || nn < 5L) {
    stop(sprintf("need at least 5 quadrature nodes, got %s.",
                 format(n_nodes)), call. = FALSE)
  }
  # probabilists' Gauss-Hermite by Golub-Welsch on the Jacobi matrix
  j <- sqrt(seq_len(nn - 1L))
  J <- matrix(0, nn, nn)
  J[cbind(seq_len(nn - 1L), 2:nn)] <- j
  J[cbind(2:nn, seq_len(nn - 1L))] <- j
  e <- eigen(J, symmetric = TRUE)
  ord <- order(e$values)
  X <- mu + sd * e$values[ord]
  W <- (e$vectors[1L, ord])^2
  W <- W / sum(W)
  ll <- .psy_ll(X, yv, it)
  ll <- ll - max(ll)
  post <- exp(ll) * W
  tot <- sum(post)
  if (tot <= 0) stop("the posterior mass underflowed.", call. = FALSE)
  post <- post / tot
  th <- sum(X * post)
  v <- sum((X - th)^2 * post)
  list(theta = th, se = sqrt(v), posterior_sd = sqrt(v),
       prior_mean = mu, prior_sd = sd, n_nodes = nn,
       no_optimisation = TRUE,
       why_no_optimisation = paste("EAP is two weighted sums, so it cannot",
                                   "fail to converge and has no",
                                   "multimodality problem"),
       se_note = paste("a genuine posterior standard deviation, not a",
                       "curvature approximation"),
       n_items = m,
       method = "EAP theta by Gauss-Hermite quadrature (Bock-Mislevy 1982)")
}


#' Warm's weighted likelihood ability estimate
#'
#' Maximise `loglik + log sqrt(I(theta))`. The weight is NOT a prior:
#' Warm (1989) chooses it so the O(1/n) BIAS of the ML estimator is
#' removed to first order. It also makes perfect patterns finite --
#' sqrt(I) tends to zero in both tails, so the weighted objective
#' turns over where the raw likelihood does not, giving the existence
#' property MAP buys with a prior, without one.
#'
#' @param y binary responses.
#' @param a,b,c item parameters; b required.
#' @param bounds search interval.
#' @return list: theta, se, information, loglik, weight_term,
#'   bias_corrected, finite_for_perfect_patterns, vs_ml, n_items,
#'   method.
#' @references Warm (1989), *Psychometrika* 54:427-450.
#' @examples
#' morie_psy_wle_theta(rep(1, 4), b = c(-1, -0.5, 0.5, 1))$theta
#' @export
morie_psy_wle_theta <- function(y, a = NULL, b = NULL, c = NULL,
                                bounds = c(-6, 6)) {
  yv <- .psy_check_y(y)
  m <- length(yv)
  it <- .psy_items(m, a, b, c)
  grid <- seq(bounds[1L], bounds[2L], length.out = 8001L)
  info <- .psy_info(grid, it)
  obj <- .psy_ll(grid, yv, it) + 0.5 * log(pmax(info, 1e-300))
  i <- which.max(obj)
  th <- grid[i]
  if (i > 1L && i < length(grid)) {
    den <- obj[i - 1L] - 2 * obj[i] + obj[i + 1L]
    if (den != 0) {
      th <- grid[i] - 0.5 * (grid[2L] - grid[1L]) *
        (obj[i + 1L] - obj[i - 1L]) / den
    }
  }
  info_t <- .psy_info(th, it)
  ml_theta <- NULL
  ml <- morie_psy_mle_theta(y, a = it$a, b = it$b, c = it$c, bounds = bounds)
  if (ml$finite) ml_theta <- ml$theta
  list(theta = th, se = if (info_t > 0) 1 / sqrt(info_t) else Inf,
       information = info_t,
       loglik = .psy_ll(th, yv, it),
       weight_term = 0.5 * log(max(info_t, 1e-300)),
       bias_corrected = TRUE, finite_for_perfect_patterns = TRUE,
       why_finite = paste("sqrt(I(theta)) tends to zero in both tails, so",
                          "the weighted objective turns over where the raw",
                          "likelihood does not"),
       not_a_prior = paste("the weight removes the O(1/n) bias of the ML",
                           "estimator to first order; it is a bias",
                           "correction, not prior information"),
       vs_ml = if (is.null(ml_theta)) NULL else th - ml_theta,
       n_items = m,
       method = "Warm (1989) weighted likelihood")
}


#' EAP theta over a response matrix -- alias
#'
#' One implementation: each row goes to [morie_psy_eap_theta()].
#' `items` has columns a, b and optionally c, one row per item.
#'
#' @param X response matrix, examinees by items.
#' @param items item-parameter matrix.
#' @param prior `c(mean, sd)`.
#' @param n_nodes quadrature nodes.
#' @return list: theta, se, n_examinees, n_items, alias_of, method.
#' @references Bock RD & Mislevy RJ (1982). Adaptive EAP estimation of
#'   ability in a microcomputer environment. \emph{Applied
#'   Psychological Measurement}, 6(4), 431-444.
#' @examples
#' X <- matrix(c(1, 0, 1, 1), 1)
#' morie_psy_theta_eap(X, cbind(rep(1, 4), c(-1, -0.5, 0.5, 1)))$theta
#' @export
morie_psy_theta_eap <- function(X, items, prior = c(0, 1), n_nodes = 61L) {
  Xm <- as.matrix(X)
  it <- as.matrix(items)
  if (nrow(it) != ncol(Xm)) it <- t(it)
  if (nrow(it) != ncol(Xm)) {
    stop(sprintf("items has %d rows for %d item columns.", nrow(it),
                 ncol(Xm)), call. = FALSE)
  }
  cc <- if (ncol(it) > 2L) it[, 3L] else NULL
  th <- se <- numeric(nrow(Xm))
  for (i in seq_len(nrow(Xm))) {
    o <- morie_psy_eap_theta(Xm[i, ], a = it[, 1L], b = it[, 2L], c = cc,
                             prior = prior, n_nodes = n_nodes)
    th[i] <- o$theta
    se[i] <- o$se
  }
  list(theta = th, se = se, n_examinees = nrow(Xm), n_items = ncol(Xm),
       prior_mean = prior[1L], prior_sd = prior[2L], n_nodes = n_nodes,
       alias_of = "morie_psy_eap_theta",
       method = "EAP theta over a response matrix")
}


#' MAP theta over a response matrix -- alias
#'
#' One implementation: each row goes to [morie_psy_map_theta()].
#' This is the MODE where [morie_psy_theta_eap()] is the MEAN, and
#' they differ whenever the posterior is skewed -- short tests and
#' extreme patterns, which is most of the time.
#'
#' @param X response matrix.
#' @param items item-parameter matrix.
#' @param prior `c(mean, sd)`.
#' @return list: theta, se, n_examinees, n_items, mode_not_mean,
#'   alias_of, method.
#' @references Mislevy (1986), *Psychometrika* 51:177-195.
#' @examples
#' X <- matrix(c(1, 0, 1, 1), 1)
#' morie_psy_theta_map(X, cbind(rep(1, 4), c(-1, -0.5, 0.5, 1)))$theta
#' @export
morie_psy_theta_map <- function(X, items, prior = c(0, 1)) {
  Xm <- as.matrix(X)
  it <- as.matrix(items)
  if (nrow(it) != ncol(Xm)) it <- t(it)
  if (nrow(it) != ncol(Xm)) {
    stop(sprintf("items has %d rows for %d item columns.", nrow(it),
                 ncol(Xm)), call. = FALSE)
  }
  cc <- if (ncol(it) > 2L) it[, 3L] else NULL
  th <- se <- numeric(nrow(Xm))
  for (i in seq_len(nrow(Xm))) {
    o <- morie_psy_map_theta(Xm[i, ], a = it[, 1L], b = it[, 2L], c = cc,
                             prior = prior)
    th[i] <- o$theta
    se[i] <- o$se
  }
  list(theta = th, se = se, n_examinees = nrow(Xm), n_items = ncol(Xm),
       mode_not_mean = paste("this is the posterior MODE; morie_psy_theta_eap",
                             "is the MEAN, and they differ whenever the",
                             "posterior is skewed"),
       alias_of = "morie_psy_map_theta",
       method = "MAP theta over a response matrix")
}


#' Paule-Mandel between-study variance
#'
#' The tau^2 solving generalised `Q(tau^2) = k - 1`. The left side is
#' strictly decreasing in tau^2, so the root is unique and bisection
#' cannot fail -- unlike the likelihood-based estimators. Markedly
#' less downward-biased than DerSimonian-Laird, which is returned
#' alongside. Since the weights are 1/(v + tau^2), a different tau^2
#' is a different POOLED EFFECT. A Q already below its expectation at
#' zero gives a boundary truncation, and `at_boundary` says so.
#'
#' @param yi,vi effects and within-study variances.
#' @param max_iter,tol bisection controls.
#' @return list: tau2, tau, mu, se, ci, Q, I2, tau2_dl, at_boundary,
#'   weights, k, method.
#' @references Paule and Mandel (1982), *J. Research NBS* 87:377-385;
#'   Veroniki et al. (2016), *Research Synthesis Methods* 7:55-79.
#' @examples
#' morie_psy_ma_paule_mandel(c(0.2, 0.5, 0.1), c(0.02, 0.03, 0.04))$tau2
#' @export
morie_psy_ma_paule_mandel <- function(yi, vi, max_iter = 200L, tol = 1e-12) {
  f <- .psy_fixed_pool(yi, vi)
  y <- as.numeric(yi)
  v <- as.numeric(vi)
  k <- f$k
  gen_q <- function(t2) {
    w <- 1 / (v + t2)
    mu <- sum(w * y) / sum(w)
    sum(w * (y - mu)^2)
  }
  at_boundary <- gen_q(0) <= k - 1
  if (at_boundary) {
    t2 <- 0
  } else {
    lo <- 0
    hi <- max(1, stats::var(y))
    while (gen_q(hi) > k - 1 && hi < 1e12) hi <- hi * 2
    for (i in seq_len(as.integer(max_iter))) {
      mid <- 0.5 * (lo + hi)
      if (gen_q(mid) > k - 1) lo <- mid else hi <- mid
      if (hi - lo < tol * max(1, hi)) break
    }
    t2 <- 0.5 * (lo + hi)
  }
  w <- 1 / (v + t2)
  mu <- sum(w * y) / sum(w)
  se <- sqrt(1 / sum(w))
  z <- stats::qnorm(0.975)
  list(tau2 = t2, tau = sqrt(t2), mu = mu, se = se,
       ci = c(mu - z * se, mu + z * se), Q = f$Q,
       I2 = if (f$Q > 0) max(0, (f$Q - (k - 1)) / f$Q) else 0,
       tau2_dl = .psy_dl(y, v), at_boundary = at_boundary,
       boundary_note = if (!at_boundary) NULL else
         paste("Q is already below its expectation at tau^2 = 0, so the",
               "estimate is a truncation at the boundary"),
       weights = w, k = k,
       uniqueness_note = paste("the generalised Q is strictly decreasing in",
                               "tau^2, so the root is unique and bisection",
                               "cannot fail"),
       why_it_matters = paste("the weights are 1/(v_i + tau^2), so a",
                              "different tau^2 is a different POOLED",
                              "EFFECT"),
       method = "Paule-Mandel (1982) tau^2 by bisection on the generalised Q")
}


#' REML between-study variance
#'
#' Viechtbauer's (2005) fixed-point iteration. REML rather than ML
#' for the same reason a sample variance divides by n - 1: ML does
#' not account for the degree of freedom spent estimating mu and is
#' biased DOWNWARD. The `1/sum(w)` term in the numerator is exactly
#' that correction, and dropping it recovers the ML estimator, which
#' is returned alongside. Convergence is not guaranteed on a flat
#' likelihood, so it is reported rather than assumed.
#'
#' @param yi,vi effects and within-study variances.
#' @param max_iter,tol iteration controls.
#' @return list: tau2, tau, mu, se, ci, tau2_ml, tau2_dl,
#'   reml_correction, converged, n_iter, Q, I2, k, method.
#' @references Viechtbauer (2005), *JEBS* 30:261-293; Viechtbauer
#'   (2010), *JSS* 36(3).
#' @examples
#' morie_psy_ma_reml(c(0.2, 0.5, 0.1), c(0.02, 0.03, 0.04))$tau2
#' @export
morie_psy_ma_reml <- function(yi, vi, max_iter = 200L, tol = 1e-12) {
  f <- .psy_fixed_pool(yi, vi)
  y <- as.numeric(yi)
  v <- as.numeric(vi)
  iterate <- function(reml) {
    t2 <- max(0, .psy_dl(y, v))
    conv <- FALSE
    it <- 0L
    for (i in seq_len(as.integer(max_iter))) {
      it <- i
      w <- 1 / (v + t2)
      mu <- sum(w * y) / sum(w)
      num <- sum(w^2 * ((y - mu)^2 - v))
      if (reml) num <- num + 1 / sum(w)
      new <- max(0, num / sum(w^2))
      if (abs(new - t2) < tol * max(1, t2)) {
        t2 <- new
        conv <- TRUE
        break
      }
      t2 <- new
    }
    list(t2 = t2, conv = conv, it = it)
  }
  r <- iterate(TRUE)
  ml <- iterate(FALSE)
  w <- 1 / (v + r$t2)
  mu <- sum(w * y) / sum(w)
  se <- sqrt(1 / sum(w))
  z <- stats::qnorm(0.975)
  list(tau2 = r$t2, tau = sqrt(r$t2), mu = mu, se = se,
       ci = c(mu - z * se, mu + z * se),
       tau2_ml = ml$t2, tau2_dl = .psy_dl(y, v),
       reml_correction = r$t2 - ml$t2,
       why_reml = paste("ML does not account for the degree of freedom spent",
                        "estimating mu and is biased DOWNWARD; the 1/sum(w)",
                        "term is exactly that correction"),
       converged = r$conv, n_iter = r$it, Q = f$Q,
       I2 = if (f$Q > 0) max(0, (f$Q - (f$k - 1)) / f$Q) else 0,
       k = f$k,
       method = "REML tau^2 by fixed-point iteration (Viechtbauer 2005)")
}


#' Robust variance estimation for dependent effect sizes
#'
#' Hedges, Tipton and Johnson (2010): fit weighted least squares with
#' any working weights, then use the cluster-robust sandwich, which
#' is consistent as the number of STUDIES grows whatever the
#' within-study correlation is. The working weights affect efficiency,
#' never validity. With few clusters the sandwich is badly
#' downward-biased, so Tipton's (2015) small-sample correction is
#' applied by default with Satterthwaite degrees of freedom; her rule
#' of thumb that `df < 4` is untrustworthy is flagged rather than
#' left to be discovered.
#'
#' @param yi effect sizes, possibly several per study.
#' @param X meta-regression design; a constant is added if absent.
#' @param cluster study identifiers.
#' @param w working weights; equal when `NULL`.
#' @param small_sample apply the HTZ correction.
#' @return list: beta, se, t, df, p, vcov, n_clusters, n_effects,
#'   df_warning, method.
#' @references Hedges, Tipton and Johnson (2010), *Research Synthesis
#'   Methods* 1:39-65; Tipton (2015), *Psychological Methods*
#'   20:375-393.
#' @examples
#' set.seed(1)
#' cl <- rep(1:10, each = 3)
#' x <- stats::rnorm(30)
#' morie_psy_ma_rve(0.4 + 0.6 * x + stats::rnorm(30), x, cl)$beta
#' @export
morie_psy_ma_rve <- function(yi, X, cluster, w = NULL, small_sample = TRUE) {
  y <- as.numeric(yi)
  A <- as.matrix(X)
  storage.mode(A) <- "double"
  if (nrow(A) != length(y)) A <- t(A)
  if (nrow(A) != length(y)) {
    stop(sprintf("X has %d rows for %d effects.", nrow(A), length(y)),
         call. = FALSE)
  }
  if (!any(apply(A, 2L, function(cc)
    isTRUE(all.equal(cc, rep(1, nrow(A))))))) {
    A <- cbind(1, A)
  }
  g <- as.vector(cluster)
  if (length(g) != length(y)) {
    stop(sprintf("cluster has %d entries for %d.", length(g), length(y)),
         call. = FALSE)
  }
  m <- nrow(A)
  p <- ncol(A)
  wv <- if (is.null(w)) rep(1, m) else as.numeric(w)
  if (length(wv) != m) {
    stop(sprintf("w has %d entries for %d effects.", length(wv), m),
         call. = FALSE)
  }
  if (any(wv <= 0)) stop("working weights must be positive.", call. = FALSE)
  groups <- unique(g)
  G <- length(groups)
  if (G <= p) {
    stop(sprintf(paste("robust variance estimation needs more clusters than",
                       "parameters: %d studies for %d coefficients. The",
                       "asymptotics are in the number of STUDIES, not effect",
                       "sizes."), G, p), call. = FALSE)
  }
  XtWX <- crossprod(A, A * wv)
  bread <- solve(XtWX)
  beta <- as.numeric(bread %*% crossprod(A, wv * y))
  e <- y - as.numeric(A %*% beta)
  meat <- matrix(0, p, p)
  for (gg in groups) {
    s <- g == gg
    Xg <- A[s, , drop = FALSE]
    eg <- e[s]
    wg <- wv[s]
    if (isTRUE(small_sample)) {
      Hg <- Xg %*% bread %*% t(Xg * wg)
      I_H <- diag(nrow(Hg)) - Hg
      ei <- eigen((I_H + t(I_H)) / 2, symmetric = TRUE)
      vals <- ifelse(ei$values > 1e-10, ei$values^-0.5, 0)
      eg <- ei$vectors %*% diag(vals, nrow = length(vals)) %*%
        t(ei$vectors) %*% eg
    }
    u <- crossprod(Xg, wg * as.numeric(eg))
    meat <- meat + tcrossprod(u)
  }
  V <- bread %*% meat %*% bread
  se <- sqrt(pmax(diag(V), 0))
  df <- rep(G - p, p)
  if (isTRUE(small_sample)) {
    for (j in seq_len(p)) {
      num <- den <- 0
      for (gg in groups) {
        s <- g == gg
        Xg <- A[s, , drop = FALSE]
        wg <- wv[s]
        cj <- (bread %*% (t(Xg) * wg))[j, ]
        q <- sum(cj^2)
        num <- num + q
        den <- den + q^2
      }
      df[j] <- if (den > 0) num^2 / den else G - 1
    }
  }
  t <- ifelse(se > 0, beta / se, NA_real_)
  list(beta = beta, se = se, t = t, df = df,
       p = 2 * stats::pt(abs(t), pmax(df, 1), lower.tail = FALSE),
       vcov = V, n_clusters = G, n_effects = m,
       small_sample = isTRUE(small_sample),
       df_warning = if (any(df < 4)) {
         paste("Tipton's rule of thumb: df below 4 makes the test",
               "untrustworthy regardless of the correction")
       } else NULL,
       asymptotics_note = paste("consistent as the number of STUDIES grows,",
                                "whatever the within-study correlation is"),
       method = "Robust variance estimation for dependent effects (Hedges-Tipton-Johnson 2010)")
}


#' Leave-one-out influence for meta-analysis
#'
#' Refit omitting each study in turn. Refitting is the point: deleting
#' a study changes tau^2 and therefore ALL the weights, so a deletion
#' can move the pooled estimate far more than that study's own weight
#' suggests -- the channel a quick recomputation with the full-data
#' tau^2 misses. The diagnostic worth reading is whether any single
#' deletion changes a CONCLUSION, which `flips_significance` records.
#'
#' @param yi,vi effects and within-study variances.
#' @param method heterogeneity estimator for each refit: `"PM"`,
#'   `"REML"` or `"DL"`.
#' @return list: mu_full, tau2_full, mu_loo, tau2_loo, delta_mu,
#'   ci_loo, flips_significance, most_influential, significant_full,
#'   method_used, k, method.
#' @references Viechtbauer and Cheung (2010), *Research Synthesis
#'   Methods* 1:112-125.
#' @examples
#' morie_psy_ma_loo(c(0.2, 0.5, 0.1, 0.4), c(0.02, 0.03, 0.04, 0.02))$mu_full
#' @export
morie_psy_ma_loo <- function(yi, vi, method = "PM") {
  y <- as.numeric(yi)
  v <- as.numeric(vi)
  if (length(y) != length(v)) {
    stop(sprintf("yi has %d entries and vi has %d.", length(y), length(v)),
         call. = FALSE)
  }
  k <- length(y)
  if (k < 3L) {
    stop(sprintf("leave-one-out needs at least 3 studies, got %d.", k),
         call. = FALSE)
  }
  if (any(v <= 0)) {
    stop("every within-study variance must be positive.", call. = FALSE)
  }
  if (!method %in% c("PM", "REML", "DL")) {
    stop("method must be 'PM', 'REML' or 'DL'.", call. = FALSE)
  }
  fit <- function(yy, vv) {
    t2 <- switch(method,
                 DL = .psy_dl(yy, vv),
                 PM = morie_psy_ma_paule_mandel(yy, vv)$tau2,
                 REML = morie_psy_ma_reml(yy, vv)$tau2)
    w <- 1 / (vv + t2)
    c(sum(w * yy) / sum(w), sqrt(1 / sum(w)), t2)
  }
  full <- fit(y, v)
  z <- stats::qnorm(0.975)
  sig_full <- abs(full[1L]) > z * full[2L]
  mu_l <- t2_l <- numeric(k)
  ci_l <- matrix(0, k, 2L)
  flips <- logical(k)
  for (i in seq_len(k)) {
    r <- fit(y[-i], v[-i])
    mu_l[i] <- r[1L]
    t2_l[i] <- r[3L]
    ci_l[i, ] <- c(r[1L] - z * r[2L], r[1L] + z * r[2L])
    flips[i] <- (abs(r[1L]) > z * r[2L]) != sig_full
  }
  d <- mu_l - full[1L]
  list(mu_full = full[1L], tau2_full = full[3L], mu_loo = mu_l,
       tau2_loo = t2_l, ci_loo = ci_l, delta_mu = d,
       flips_significance = flips, most_influential = which.max(abs(d)),
       max_abs_delta = max(abs(d)), significant_full = sig_full,
       refit_note = paste("each fit re-estimates tau^2, so deleting a study",
                          "changes ALL the weights"),
       method_used = method, k = k,
       method = "Leave-one-out influence for random-effects meta-analysis")
}
