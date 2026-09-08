# Augmented outcome-weighted learning for dynamic treatment regimens.
# Sources: Liu, Y., Wang, Y., Kosorok, M. R., Zhao, Y. & Zeng, D. (2018)
# "Augmented outcome-weighted learning for estimating optimal dynamic
# treatment regimens", Statistics in Medicine, doi:10.1002/sim.7844
# (Sec. 1 the four contributions -- negative outcomes without an
# additive constant, residual weights, combining nonparametric
# robustness with model-based augmentation, and the same asymptotic
# bias as OWL with smaller stochastic variability; Sec. 2.1 the
# K-stage notation, value function and OWL formulation; Sec. 2 the
# AOL formulation for single and multiple stages); Zhao, Y., Zeng,
# D., Rush, A. J. & Kosorok, M. R. (2012) "Estimating individualized
# treatment rules using outcome weighted learning", JASA 107(499),
# 1106-1118, doi:10.1080/01621459.2012.695674 (the single-stage OWL
# this augments); Zhao, Y.-Q., Zeng, D., Laber, E. B. & Kosorok,
# M. R. (2015) "New statistical learning methods for estimating
# optimal dynamic treatment regimes", JASA 110(510), 583-598,
# doi:10.1080/01621459.2014.937488 (the multi-stage OWL whose
# discarding of subjects AOL removes).
#
# Native implementation mirroring Python morie.fn.awltrn exactly:
# the same residual weights (R - m(H))/pi with the label flip on
# negative residuals, the same additive-constant OWL weights for
# comparison, the same weighted least-squares surrogate for the 0-1
# loss, and the same augmented backward induction that keeps every
# subject at every stage. No random numbers are drawn inside these
# routines, so the shared generator is not touched.

# Internal: validate R, A, H and propensity -- mirrors _check.
#' @keywords internal
#' @noRd
.awltrn_check <- function(R, A, H, propensity) {
  r <- as.numeric(R)
  a <- as.integer(A)
  n <- length(r)
  Hm <- .awltrn_to_Hm(H, n)
  if (length(a) != n || nrow(Hm) != n)
    stop(sprintf("awltrn: R, A and H must agree in length (%d, %d, %d)",
                 n, length(a), nrow(Hm)))
  if (n < 4L)
    stop(sprintf("awltrn: need at least 4 subjects, got %d", n))
  bad <- which(!(a %in% c(-1L, 1L)))
  if (length(bad) > 0L)
    stop(sprintf("awltrn: treatments must be coded -1/+1, got %s",
                 a[bad[1L]]))
  if (is.null(propensity)) {
    p <- rep(0.5, n)
  } else if (is.numeric(propensity) && length(propensity) == 1L) {
    p <- rep(as.numeric(propensity), n)
  } else {
    p <- as.numeric(propensity)
  }
  if (length(p) != n)
    stop(sprintf("awltrn: %d propensities for %d subjects",
                 length(p), n))
  if (any(p <= 0 | p >= 1))
    stop("awltrn: randomisation probabilities must lie strictly in (0, 1)")
  list(r = r, a = a, Hm = Hm, p = p, n = n)
}

# Internal: coerce H to a numeric matrix (n subjects x p features).
#' @keywords internal
#' @noRd
.awltrn_to_Hm <- function(H, fallback_n) {
  if (is.matrix(H)) return(H)
  if (is.data.frame(H)) return(as.matrix(H))
  if (is.list(H)) return(do.call(rbind, lapply(H, as.numeric)))
  matrix(as.numeric(H), ncol = 1L)
}

# Internal: design matrix with intercept -- mirrors k.design in _s03core.
#' @keywords internal
#' @noRd
.awltrn_design <- function(Hm, n) {
  cbind(1, Hm)
}

# Internal: ridge-regularised least squares -- mirrors k.lstsq in
# _s03core. Solves (D'D + ridge*I) beta = D'y.
#' @keywords internal
#' @noRd
.awltrn_lstsq <- function(D, y, ridge) {
  p <- ncol(D)
  A <- crossprod(D) + ridge * diag(p)
  solve(A, crossprod(D, y))
}

# Internal: weighted ridge-regularised least squares -- mirrors k.wls
# in _s03core. The Python wls is called with the raw Hm (not the
# pre-built design), so the design matrix is built here.
#' @keywords internal
#' @noRd
.awltrn_wls <- function(Hm, y, w, ridge) {
  n <- nrow(Hm)
  D <- .awltrn_design(Hm, n)
  p <- ncol(D)
  Wsqrt <- sqrt(w)
  Dw <- D * Wsqrt
  A <- crossprod(Dw) + ridge * diag(p)
  b <- crossprod(Dw, y * Wsqrt)
  list(coef = solve(A, b))
}

#' OWL weights R_i/pi_i, with the additive constant
#'
#' Computes the outcome-weighted-learning weights of Zhao et al.
#' (2012). Because the weighted classifier needs non-negative weights
#' and outcomes can be negative, OWL adds a constant \code{shift} to
#' every outcome first. The default is the smallest value that makes
#' all weights non-negative. The constant changes the relative
#' weights, so the value used is reported back.
#'
#' @param R Numeric vector of outcomes.
#' @param A Integer vector of treatments, coded -1 or +1.
#' @param H Matrix of covariates (one row per subject).
#' @param propensity Randomisation probabilities, scalar or vector;
#'   defaults to 0.5 for every subject.
#' @param shift Additive constant; if \code{NULL} the smallest value
#'   that makes all weights non-negative is used.
#' @return A list with \code{weights}, \code{labels} (the treatment
#'   codes, unchanged), \code{shift} (the constant actually used),
#'   \code{cv} (coefficient of variation of the weights) and
#'   \code{note}.
#' @references Zhao, Y., Zeng, D., Rush, A. J. & Kosorok, M. R.
#'   (2012), JASA 107(499), 1106-1118.
#' @export
owl_weights <- function(R, A, H, propensity = NULL, shift = NULL) {
  chk <- .awltrn_check(R, A, H, propensity)
  r <- chk$r
  a <- chk$a
  p <- chk$p
  n <- chk$n
  c <- if (is.null(shift)) {
    if (min(r) >= 0) 0 else -min(r)
  } else as.numeric(shift)
  w <- (r + c) / p
  if (any(w < 0))
    stop(sprintf(paste0("awltrn: OWL weights must be non-negative; ",
                        "increase shift (smallest weight %.4g)"),
                 min(w)))
  mw <- sum(w) / n
  sd <- sqrt(sum((w - mw)^2) / max(n - 1L, 1L))
  cv <- if (mw > 1e-12) sd / mw else Inf
  list(weights = w, labels = a, shift = c, cv = cv,
       note = paste0("the additive constant changes the RELATIVE ",
                     "weights; a large one flattens them toward ",
                     "equality and discards signal"))
}

#' AOL weights |R_i - m(H_i)|/pi_i
#'
#' Augmented outcome-weighted-learning weights of Liu et al. (2018).
#' The prognostic part \code{m(H)} is removed from the outcome before
#' weighting, so a negative residual is not shifted away but flips
#' the label: a subject who did worse than predicted under the arm
#' they received is evidence for the other arm. If \code{prognostic}
#' is omitted it is fitted by least squares on H alone -- deliberately
#' without treatment, since the point is to remove the part of the
#' outcome common to both arms.
#'
#' @param R Numeric vector of outcomes.
#' @param A Integer vector of treatments, coded -1 or +1.
#' @param H Matrix of covariates (one row per subject).
#' @param propensity Randomisation probabilities, scalar or vector.
#' @param prognostic Optional numeric vector of fitted prognostic
#'   values; if \code{NULL} it is estimated by ridge-regularised
#'   least squares.
#' @param ridge Ridge penalty for the prognostic fit.
#' @return A list with \code{weights}, \code{labels} (possibly with
#'   some signs flipped), \code{residual}, \code{prognostic},
#'   \code{n_flipped}, \code{cv} and \code{note}.
#' @references Liu, Y., Wang, Y., Kosorok, M. R., Zhao, Y. & Zeng,
#'   D. (2018), Statistics in Medicine, doi:10.1002/sim.7844.
#' @export
aol_weights <- function(R, A, H, propensity = NULL, prognostic = NULL,
                        ridge = 1e-8) {
  chk <- .awltrn_check(R, A, H, propensity)
  r <- chk$r
  a <- chk$a
  Hm <- chk$Hm
  p <- chk$p
  n <- chk$n
  if (is.null(prognostic)) {
    D <- .awltrn_design(Hm, n)
    beta <- .awltrn_lstsq(D, r, ridge)
    m <- as.numeric(D %*% beta)
  } else {
    m <- as.numeric(prognostic)
    if (length(m) != n)
      stop(sprintf("awltrn: %d prognostic values for %d subjects",
                   length(m), n))
  }
  resid <- r - m
  w <- abs(resid) / p
  lab <- ifelse(resid >= 0, a, -a)
  mw <- sum(w) / n
  sd <- sqrt(sum((w - mw)^2) / max(n - 1L, 1L))
  cv <- if (mw > 1e-12) sd / mw else Inf
  list(weights = w, labels = lab, residual = resid, prognostic = m,
       n_flipped = sum(resid < 0), cv = cv,
       note = paste0("a negative residual FLIPS the label -- doing ",
                     "worse than predicted under the arm received is ",
                     "evidence for the other arm"))
}

#' Weighted linear classifier, returned as a decision function
#'
#' Minimises weighted squared error against the labels, the surrogate
#' used here for the weighted 0-1 loss. The sign of the fit is the
#' rule. A subject is labelled +1 if the decision function is
#' non-negative and -1 otherwise.
#'
#' @param H Matrix of covariates (one row per subject).
#' @param labels Numeric vector of -1/+1 labels.
#' @param weights Numeric vector of non-negative weights.
#' @param ridge Ridge penalty for the weighted fit.
#' @return A list with \code{rule} (a function from covariate vectors
#'   to -1/+1) and \code{coef} (intercept followed by feature
#'   coefficients).
#' @export
weighted_rule <- function(H, labels, weights, ridge = 1e-6) {
  Hm <- .awltrn_to_Hm(H, length(labels))
  n <- nrow(Hm)
  if (length(labels) != n || length(weights) != n)
    stop("awltrn: H, labels and weights must agree in length")
  if (any(weights < 0))
    stop("awltrn: weights must be non-negative")
  fit <- .awltrn_wls(Hm, as.numeric(labels), as.numeric(weights), ridge)
  b <- fit$coef
  rule <- function(x) {
    xv <- as.numeric(x)
    s <- b[1L] + sum(b[-1L] * xv)
    if (s >= 0) 1L else -1L
  }
  list(rule = rule, coef = b)
}

#' Inverse-probability value of a single-stage rule
#'
#' Estimates the expected outcome had treatment been assigned by the
#' rule, using inverse-probability weighting.
#'
#' @param R Numeric vector of outcomes.
#' @param A Integer vector of observed treatments, -1 or +1.
#' @param H Matrix of covariates.
#' @param rule Function mapping a covariate vector to -1 or +1.
#' @param propensity Randomisation probabilities.
#' @return Numeric scalar, the estimated value of the rule.
#' @export
regimen_value <- function(R, A, H, rule, propensity = NULL) {
  chk <- .awltrn_check(R, A, H, propensity)
  r <- chk$r
  a <- chk$a
  Hm <- chk$Hm
  p <- chk$p
  n <- chk$n
  ind <- vapply(seq_len(n), function(i) {
    rule(Hm[i, ]) == a[i]
  }, logical(1))
  num <- sum(r * ind / p)
  den <- sum(ind / p)
  if (den <= 1e-12)
    stop("awltrn: no subject's observed treatment agrees with the rule")
  num / den
}

#' Single-stage AOL (or plain OWL, for comparison)
#'
#' Computes the weights, fits a weighted linear classifier, and
#' reports its inverse-probability value on the observed data. The
#' return is a named list whose names match the Python RichResult
#' payload keys.
#'
#' @param R Numeric vector of outcomes.
#' @param A Integer vector of treatments.
#' @param H Matrix of covariates.
#' @param propensity Randomisation probabilities.
#' @param method Either \code{"aol"} or \code{"owl"}.
#' @param prognostic Optional prognostic values for AOL.
#' @param shift Optional additive constant for OWL.
#' @param ridge Ridge penalty for the weighted classifier.
#' @return A list with \code{estimate}, \code{value}, \code{rule},
#'   \code{coef}, \code{weights}, \code{labels}, \code{weight_cv},
#'   \code{method}, \code{n}, \code{n_flipped} (AOL only, else
#'   \code{NULL}), \code{shift} (OWL only, else \code{NULL}) and
#'   \code{reference}.
#' @references Liu, Y., Wang, Y., Kosorok, M. R., Zhao, Y. & Zeng,
#'   D. (2018), Statistics in Medicine, doi:10.1002/sim.7844.
#' @export
fit_aol <- function(R, A, H, propensity = NULL, method = "aol",
                    prognostic = NULL, shift = NULL, ridge = 1e-6) {
  if (!(method %in% c("aol", "owl")))
    stop(sprintf("awltrn: method must be aol or owl, got '%s'", method))
  w <- if (method == "aol") {
    aol_weights(R, A, H, propensity = propensity, prognostic = prognostic)
  } else {
    owl_weights(R, A, H, propensity = propensity, shift = shift)
  }
  cl <- weighted_rule(H, w$labels, w$weights, ridge = ridge)
  v <- regimen_value(R, A, H, cl$rule, propensity = propensity)
  list(estimate = v, value = v, rule = cl$rule, coef = cl$coef,
       weights = w$weights, labels = w$labels, weight_cv = w$cv,
       method = method, n = length(w$weights),
       n_flipped = if ("n_flipped" %in% names(w)) w$n_flipped else NULL,
       shift = if ("shift" %in% names(w)) w$shift else NULL,
       reference = "Liu, Wang, Kosorok, Zhao & Zeng (2018)")
}

#' Backward induction across K stages, using every subject
#'
#' Working backwards, the pseudo-outcome carried into stage k is the
#' reward at k plus the value achieved downstream. For subjects
#' whose later treatment matched the estimated optimum that is their
#' observed future reward, and for the rest it is the model-based
#' prediction from a fresh AOL prognostic fit. That augmentation is
#' what stops the sample from shrinking stage by stage: every
#' subject contributes at every stage.
#'
#' @param stages A list of \code{(R, A, H)} triples, one per stage
#'   in chronological order.
#' @param propensity Randomisation probabilities.
#' @param ridge Ridge penalty.
#' @return A list with \code{estimate}, \code{rules},
#'   \code{n_stages}, \code{n_used_per_stage}, \code{n},
#'   \code{method} and \code{note}.
#' @export
fit_stages <- function(stages, propensity = NULL, ridge = 1e-6) {
  if (length(stages) == 0L)
    stop("awltrn: no stages given")
  K <- length(stages)
  n <- length(stages[[1L]][[1L]])
  for (j in seq_len(K)) {
    st <- stages[[j]]
    Rk <- st[[1L]]
    Ak <- st[[2L]]
    Hk <- st[[3L]]
    Hm_j <- .awltrn_to_Hm(Hk, n)
    if (length(Rk) != n || length(Ak) != n || nrow(Hm_j) != n)
      stop(sprintf("awltrn: stage %d has a different number of subjects",
                   j - 1L))
  }
  future <- rep(0, n)
  rules <- list()
  used <- list()
  for (j in K:1L) {
    st <- stages[[j]]
    Rk <- st[[1L]]
    Ak <- st[[2L]]
    Hk <- st[[3L]]
    rv <- as.numeric(Rk)
    pseudo <- rv + future
    fit <- fit_aol(pseudo, Ak, Hk, propensity = propensity,
                   method = "aol", ridge = ridge)
    rules <- c(list(fit$rule), rules)
    used <- c(list(n), used)
    Hm <- .awltrn_to_Hm(Hk, n)
    av <- as.integer(Ak)
    aw <- aol_weights(pseudo, Ak, Hk, propensity = propensity)
    future <- vapply(seq_len(n), function(i) {
      if (fit$rule(Hm[i, ]) == av[i]) pseudo[i] else aw$prognostic[i]
    }, numeric(1))
  }
  list(estimate = sum(future) / n, rules = rules,
       n_stages = as.integer(K), n_used_per_stage = used, n = n,
       method = paste0("augmented backward induction; Liu et al. ",
                       "(2018) Sec. 2"),
       note = paste0("every subject contributes at every stage -- ",
                     "OWL's backward induction keeps only those ",
                     "whose later treatments were optimal"))
}

#' One-paragraph summary of the AOL vs OWL trade-off
#'
#' @return A character string.
#' @export
#' @examples
#' res <- .awltrn_cheatsheet()
#' res
.awltrn_cheatsheet <- function() {
  paste0("awltrn: AOL. OWL weights R/pi and needs R >= 0, so it ",
         "ADDS A CONSTANT -- which changes the relative weights ",
         "and flattens them. AOL weights |R - m(H)|/pi and lets a ",
         "negative residual FLIP THE LABEL instead. Removing the ",
         "prognostic part cuts weight variance without changing ",
         "the asymptotic bias, and stays correct even if m is ",
         "misspecified. Multi-stage: augmentation keeps ALL ",
         "subjects at every stage rather than discarding those ",
         "whose later treatments were not optimal.")
}

#' Augmented outcome-weighted learning (native R arm)
#'
#' Native R arm of \code{morie.fn.awltrn}. Dispatches to
#' \code{fit_stages} when \code{stages} is supplied and to
#' \code{fit_aol} otherwise. No random numbers are drawn, so the
#' shared generator is not touched and the two arms are
#' deterministic given the same inputs.
#'
#' @param R Numeric vector of outcomes (single-stage only).
#' @param A Integer vector of treatments, -1 or +1 (single-stage only).
#' @param H Matrix of covariates (single-stage only).
#' @param propensity Randomisation probabilities.
#' @param method \code{"aol"} or \code{"owl"} (single-stage only).
#' @param prognostic Optional prognostic values (single-stage only).
#' @param shift Optional additive constant (single-stage only).
#' @param ridge Ridge penalty.
#' @param stages Optional list of \code{(R, A, H)} triples for
#'   multi-stage backward induction.
#' @return A named list; see \code{fit_aol} and \code{fit_stages}.
#' @references Liu, Y., Wang, Y., Kosorok, M. R., Zhao, Y. & Zeng,
#'   D. (2018); Zhao, Y., Zeng, D., Rush, A. J. & Kosorok, M. R.
#'   (2012); Zhao, Y.-Q., Zeng, D., Laber, E. B. & Kosorok, M. R.
#'   (2015).
#' @export
morie_awltrn <- function(R = NULL, A = NULL, H = NULL,
                          propensity = NULL, method = "aol",
                          prognostic = NULL, shift = NULL,
                          ridge = 1e-6, stages = NULL) {
  if (!is.null(stages))
    fit_stages(stages, propensity = propensity, ridge = ridge)
  else
    fit_aol(R, A, H, propensity = propensity, method = method,
            prognostic = prognostic, shift = shift, ridge = ridge)
}
