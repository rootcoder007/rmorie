# morie.fn -- function file (rootcoder007/morie)
# The targeted learning roadmap, as executable structure.
#
# The roadmap is a sequence, and its order is the argument: (1) the data
# are a realisation of a random variable with distribution P_0; (2) the
# statistical model M represents what is genuinely known about the
# experiment that generated them -- no more; (3) the scientific question
# becomes a target parameter Psi : M -> R; (4) TMLE estimates it and
# supplies inference.
#
# Why the order matters. Fitting first and asking afterwards is what
# produces an estimator biased for the question and non-normal in the
# limit. The book's metaphor is exact: one cannot shoot the arrow and
# then paint the bullseye -- the target must be specified in advance.
#
# A TMLE is three ingredients, and they are not independent.
#   1. a target parameter Psi that is pathwise differentiable, with
#      canonical gradient (efficient influence curve) D*(P);
#   2. a least favorable submodel {P(epsilon)} through the initial
#      estimator, used as an offset;
#   3. a loss function L whose score along that submodel at epsilon = 0
#      spans D*.
# That last condition is the whole mechanism. Because the score spans
# the efficient influence curve, the maximum likelihood step along the
# submodel makes the updated estimator solve P_n D*(P_n*) = 0, and that
# equation is what delivers double robustness and asymptotic efficiency
# of the substitution estimator. score_spans_eic checks it numerically
# rather than taking it on faith -- if the submodel and loss are
# mismatched, this is where it shows.
#
# Machine learning is used, but not trusted for inference. The initial
# fit should be a super learner: a cross-validated ensemble. Its own
# bias is then removed by the targeting step, which is why an estimator
# built on flexible learning can still be asymptotically linear -- and
# why the roadmap does not require, or want, a pre-specified parametric
# model.
#
# References
# ----------
# van der Laan, M. J. & Rose, S. (2018) Targeted Learning in Data
# Science: Causal Inference for Complex Longitudinal Studies, Springer,
# doi:10.1007/978-3-319-65304-4. Chap. 1 (the roadmap: data as a random
# variable; a statistical model representing true knowledge of the
# experiment; translation of the scientific question into a statistical
# target parameter; TMLE with inference; the three requirements of a
# TMLE -- a target parameter defined as a mapping from an infinite
# dimensional parameter, a least favorable submodel through the initial
# estimator, and a loss function whose score condition on the submodel
# spans the efficient score, so the resulting substitution estimator
# solves the efficient score equation, giving double robustness and
# asymptotic efficiency; the use of a super learner for the initial
# fit; and the warning that an untargeted fit is overly biased and not
# normally distributed).
#
# van der Laan, M. J. & Rose, S. (2011) Targeted Learning: Causal
# Inference for Observational and Experimental Data, Springer,
# doi:10.1007/978-1-4419-9782-1. The first book, which this one is a
# sequel to.

.tlroad_EPS <- 1e-12
.tlroad_STEPS <- c("data", "model", "target", "estimate")

#' .tlroad_vec
#'
#' A step of the tlroad_native implementation. Called by \code{.tlroad_eic_ate},
#' \code{.tlroad_plugin}, \code{.tlroad_score_spans_eic}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @return A vector, from \code{as.numeric}.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .tlroad_vec(x = x)
#' res
.tlroad_vec <- function(x) {
  as.numeric(x)
}

#' .tlroad_roadmap
#'
#' A step of the tlroad_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param data_description Coerced to character by the body, with \code{as.character}.
#' @param model_assumptions A vector; its length is taken.
#' @param target_name Coerced to character by the body, with \code{as.character}.
#' @param estimator Coerced to character by the body, with \code{as.character}. Defaults
#' to \code{"TMLE"}.
#' @return A list with \code{steps}, \code{data}, \code{model}, \code{target},
#' \code{estimator}, \code{note}.
#' @export
.tlroad_roadmap <- function(data_description, model_assumptions, target_name,
                            estimator = "TMLE") {
  if (!nzchar(trimws(as.character(data_description)))) {
    stop("tlroad: the data-generating experiment must be described")
  }
  if (length(model_assumptions) == 0L) {
    stop("tlroad: the statistical model must state what is known; an unstated model is a parametric assumption you have not admitted")
  }
  if (!nzchar(trimws(as.character(target_name)))) {
    stop("tlroad: the target parameter must be specified BEFORE estimation")
  }
  list(
    steps = as.list(.tlroad_STEPS),
    data = as.character(data_description),
    model = as.list(model_assumptions),
    target = as.character(target_name),
    estimator = as.character(estimator),
    note = "the target is specified first: one cannot shoot the arrow and then paint the bullseye"
  )
}

#' .tlroad_eic_ate
#'
#' A step of the tlroad_native implementation. Called by \code{.tlroad_solves_eic_equation}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A Passed to \code{.tlroad_vec}.
#' @param Y Passed to \code{.tlroad_vec}.
#' @param Q1 Passed to \code{.tlroad_vec}.
#' @param Q0 Passed to \code{.tlroad_vec}.
#' @param g Passed to \code{.tlroad_vec}.
#' @param psi Coerced to numeric by the body, with \code{as.numeric}.
#' @return The value of \code{out}, as built in the body.
#' @export
.tlroad_eic_ate <- function(A, Y, Q1, Q0, g, psi) {
  a <- .tlroad_vec(A)
  y <- .tlroad_vec(Y)
  q1 <- .tlroad_vec(Q1)
  q0 <- .tlroad_vec(Q0)
  gg <- .tlroad_vec(g)
  n <- length(a)
  if (!(length(y) == n && length(q1) == n &&
        length(q0) == n && length(gg) == n)) {
    stop("tlroad: the inputs differ in length")
  }
  if (any(gg <= 0.0 | gg >= 1.0)) {
    stop("tlroad: the propensity score must lie strictly inside (0,1) -- a positivity violation")
  }
  out <- numeric(n)
  for (i in seq_len(n)) {
    qa <- if (a[i] == 1.0) q1[i] else q0[i]
    h <- a[i] / gg[i] - (1.0 - a[i]) / (1.0 - gg[i])
    out[i] <- h * (y[i] - qa) + q1[i] - q0[i] - as.numeric(psi)
  }
  out
}

#' .tlroad_score_spans_eic
#'
#' A step of the tlroad_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A Passed to \code{.tlroad_vec}.
#' @param Y Passed to \code{.tlroad_vec}.
#' @param Q1 Passed to \code{.tlroad_vec}.
#' @param Q0 Passed to \code{.tlroad_vec}.
#' @param g Passed to \code{.tlroad_vec}.
#' @param h Numeric; combined arithmetically in the body. Defaults to \code{1e-06}.
#' @return A list with \code{score}, \code{eic_component}, \code{difference},
#' \code{spans}, \code{note}.
#' @export
.tlroad_score_spans_eic <- function(A, Y, Q1, Q0, g, h = 1e-6) {
  a <- .tlroad_vec(A)
  y <- .tlroad_vec(Y)
  q1 <- .tlroad_vec(Q1)
  q0 <- .tlroad_vec(Q0)
  gg <- .tlroad_vec(g)
  n <- length(a)

  loss <- function(eps) {
    tot <- 0.0
    for (i in seq_len(n)) {
      qa <- if (a[i] == 1.0) q1[i] else q0[i]
      cc <- a[i] / gg[i] - (1.0 - a[i]) / (1.0 - gg[i])
      lo <- log(qa / (1.0 - qa)) + eps * cc
      p <- 1.0 / (1.0 + exp(-lo))
      p <- min(max(p, .tlroad_EPS), 1.0 - .tlroad_EPS)
      tot <- tot + -(y[i] * log(p) + (1.0 - y[i]) * log(1.0 - p))
    }
    tot / n
  }

  score <- -(loss(h) - loss(-h)) / (2.0 * h)

  direct <- 0.0
  for (i in seq_len(n)) {
    qa <- if (a[i] == 1.0) q1[i] else q0[i]
    cc <- a[i] / gg[i] - (1.0 - a[i]) / (1.0 - gg[i])
    direct <- direct + cc * (y[i] - qa)
  }
  direct <- direct / n

  list(
    score = score,
    eic_component = direct,
    difference = abs(score - direct),
    spans = abs(score - direct) < 1e-5,
    note = "the score of the loss along the submodel at epsilon = 0 IS the efficient influence curve component; that is what makes the update solve the efficient score equation"
  )
}

#' .tlroad_plugin
#'
#' A step of the tlroad_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param Q1 Passed to \code{.tlroad_vec}.
#' @param Q0 Passed to \code{.tlroad_vec}.
#' @return A numeric value.
#' @export
.tlroad_plugin <- function(Q1, Q0) {
  q1 <- .tlroad_vec(Q1)
  q0 <- .tlroad_vec(Q0)
  if (length(q1) != length(q0)) {
    stop("tlroad: the two arms differ in length")
  }
  sum(q1 - q0) / length(q1)
}

#' .tlroad_solves_eic_equation
#'
#' A step of the tlroad_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A Passed to \code{.tlroad_eic_ate}.
#' @param Y Passed to \code{.tlroad_eic_ate}.
#' @param Q1 Passed to \code{.tlroad_eic_ate}.
#' @param Q0 Passed to \code{.tlroad_eic_ate}.
#' @param g Passed to \code{.tlroad_eic_ate}.
#' @param psi Passed to \code{.tlroad_eic_ate}.
#' @param tol Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1e-08}.
#' @return A list with \code{estimate}, \code{mean_eic}, \code{solved}, \code{se},
#' \code{ci}, \code{method}.
#' @export
.tlroad_solves_eic_equation <- function(A, Y, Q1, Q0, g, psi, tol = 1e-8) {
  d <- .tlroad_eic_ate(A, Y, Q1, Q0, g, psi)
  m <- mean(d)
  se <- sqrt(sum((d - m)^2) / length(d)^2)
  list(
    estimate = m,
    mean_eic = m,
    solved = abs(m) < as.numeric(tol),
    se = se,
    ci = c(as.numeric(psi) - 1.96 * se, as.numeric(psi) + 1.96 * se),
    method = "efficient score equation and influence-curve inference; van der Laan & Rose (2018) Chap. 1"
  )
}

#' .tlroad_cheatsheet
#'
#' A step of the tlroad_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
#' @examples
#' res <- .tlroad_cheatsheet()
#' res
.tlroad_cheatsheet <- function() {
  "tlroad: (1) data as a random variable, (2) a statistical model stating only what is KNOWN, (3) the scientific question as a target parameter, (4) TMLE plus inference -- in that order, because you cannot shoot the arrow then paint the bullseye. A TMLE needs three matched pieces: a pathwise differentiable parameter with canonical gradient D*, a least favorable submodel through the initial fit, and a LOSS WHOSE SCORE SPANS D*. That span is the mechanism: it makes the update solve P_n D* = 0, which is where double robustness and efficiency come from."
}

# Main entry point
morie_tlroad <- .tlroad_roadmap

# Compact alias per ledger/NAMING.md
targetedroadmap <- .tlroad_roadmap
