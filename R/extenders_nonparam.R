# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) morie contributors
#
# This file is part of morie. morie is free software: you can
# redistribute it and/or modify it under the terms of the GNU Affero
# General Public License as published by the Free Software Foundation,
# either version 3 of the License, or (at your option) any later
# version. See LICENSE for the full text.
#
# Phase 1.n FDR / nonparametric / quantile / latent-class extenders
# (2026-05-26).
#
# Thin wrapper-as-extender entry points under the canonical
# `morie_<domain>_*` prefix that delegate to six CRAN packages so
# MRM / paper callers can reach their full surface from inside
# rmorie without taking a hard dependency:
#
#   * locfdr           -- local-FDR estimation from z-scores
#   * fdrtool          -- FDR + q-values + tail-area p-values
#   * quantreg         -- quantile regression (Koenker)
#   * np               -- nonparametric kernel regression
#   * dirichletprocess -- Bayesian nonparametric DP mixtures
#   * lcmm             -- latent-class mixed models
#
# Each function follows the same shape: a requireNamespace guard
# with a hard error pointing to install.packages(), then forwards
# the call and returns a thin two-slot list with
# `$method` (qualified upstream function name) and `$raw` (the
# upstream return object).

#' FDR / nonparametric / quantile / latent-class extenders (Phase 1.n)
#'
#' Thin wrapper-as-extender entry points that delegate to canonical
#' CRAN packages for local-FDR estimation (\pkg{locfdr}), FDR /
#' q-values (\pkg{fdrtool}), quantile regression (\pkg{quantreg}),
#' nonparametric kernel regression (\pkg{np}), Bayesian
#' nonparametric Dirichlet-process mixtures
#' (\pkg{dirichletprocess}), and latent-class mixed models
#' (\pkg{lcmm}).  Each function returns a two-element list with
#' \code{$method} (the qualified upstream function name, or a
#' multi-step string for compound pipelines) and \code{$raw} (the
#' upstream return object), so downstream callers can pattern-match
#' on shape while keeping the full upstream object available for
#' inspection.
#'
#' @name extenders_nonparam
NULL


# ---------------------------------------------------------------------
# Internal helper
# ---------------------------------------------------------------------

.morie_nonparam_need <- function(pkg, fn) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      sprintf("%s: install.packages(\"%s\")", fn, pkg),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


# ---------------------------------------------------------------------
# locfdr
# ---------------------------------------------------------------------

#' Local FDR estimation via \pkg{locfdr}
#'
#' Thin extender over \code{locfdr::locfdr} for Efron's
#' empirical-Bayes local false-discovery-rate estimation from a
#' vector of z-scores (Efron, 2004; Efron, 2010).
#'
#' @param zz Numeric vector of test statistics (typically z-scores)
#'   for the locfdr empirical-null fit.
#' @param ... Further arguments forwarded to \code{locfdr::locfdr}
#'   (e.g. \code{bre}, \code{df}, \code{pct}, \code{pct0},
#'   \code{nulltype}, \code{type}, \code{plot}, \code{mult},
#'   \code{mlests}, \code{main}, \code{sw}).
#'
#' @return A list with \code{$method = "locfdr::locfdr"} and
#'   \code{$raw} (the \code{locfdr} object with the fitted local-FDR
#'   curve, empirical-null parameters, and the fdr / Fdrleft /
#'   Fdrright summaries).
#' @export
#' @examples
#' \dontrun{
#'   if (requireNamespace("locfdr", quietly = TRUE)) {
#'     set.seed(1)
#'     zz <- c(stats::rnorm(900), stats::rnorm(100, mean = 3))
#'     morie_locfdr_estimate(zz)
#'   }
#' }
morie_locfdr_estimate <- function(zz, ...) {
  .morie_nonparam_need("locfdr", "morie_locfdr_estimate")
  raw <- locfdr::locfdr(zz = zz, ...)
  list(method = "locfdr::locfdr", raw = raw)
}


# ---------------------------------------------------------------------
# fdrtool
# ---------------------------------------------------------------------

#' FDR / q-values / tail-area p-values via \pkg{fdrtool}
#'
#' Thin extender over \code{fdrtool::fdrtool} for the
#' Strimmer (2008) shrinkage estimator of local and tail-area
#' false-discovery rates, q-values, and the underlying
#' null-distribution scale parameter.
#'
#' @param x Numeric vector of test statistics or p-values, as
#'   appropriate to \code{statistic}.
#' @param statistic Character; the type of statistic in \code{x}.
#'   One of \code{"normal"} (default), \code{"correlation"},
#'   \code{"pvalue"}, or \code{"studentt"}.
#' @param ... Further arguments forwarded to
#'   \code{fdrtool::fdrtool} (e.g. \code{plot}, \code{color.figure},
#'   \code{verbose}, \code{cutoff.method}, \code{pct0}).
#'
#' @return A list with \code{$method = "fdrtool::fdrtool"} and
#'   \code{$raw} (the \code{fdrtool} return list with \code{pval},
#'   \code{qval}, \code{lfdr}, and \code{param}).
#' @export
#' @examples
#' \dontrun{
#'   if (requireNamespace("fdrtool", quietly = TRUE)) {
#'     set.seed(1)
#'     x <- c(stats::rnorm(900), stats::rnorm(100, mean = 3))
#'     morie_fdr_qvalues(x, statistic = "normal")
#'   }
#' }
morie_fdr_qvalues <- function(x, statistic = "normal", ...) {
  .morie_nonparam_need("fdrtool", "morie_fdr_qvalues")
  raw <- fdrtool::fdrtool(x = x, statistic = statistic, ...)
  list(method = "fdrtool::fdrtool", raw = raw)
}


# ---------------------------------------------------------------------
# quantreg
# ---------------------------------------------------------------------

#' Quantile regression via \pkg{quantreg}
#'
#' Thin extender over \code{quantreg::rq} for Koenker-Bassett
#' quantile regression at one or more conditional quantiles
#' (Koenker & Bassett, 1978; Koenker, 2005).
#'
#' @param formula A model formula of the form
#'   \code{y ~ x1 + x2 + ...}.
#' @param tau Numeric scalar or vector in \code{(0, 1)}; the
#'   conditional quantile(s) at which to fit the regression
#'   (default \code{0.5}, the median).
#' @param data A data frame containing the variables in
#'   \code{formula}.
#' @param ... Further arguments forwarded to \code{quantreg::rq}
#'   (e.g. \code{subset}, \code{weights}, \code{na.action},
#'   \code{method}, \code{model}, \code{contrasts}).
#'
#' @return A list with \code{$method = "quantreg::rq"} and
#'   \code{$raw} (an \code{rq} / \code{rqs} object with the fitted
#'   coefficients at each \code{tau}).
#' @export
#' @examples
#' \dontrun{
#'   if (requireNamespace("quantreg", quietly = TRUE)) {
#'     set.seed(1)
#'     n  <- 100
#'     df <- data.frame(x = stats::rnorm(n))
#'     df$y <- 1 + 2 * df$x + stats::rnorm(n)
#'     morie_quantile_reg(y ~ x, tau = c(0.25, 0.5, 0.75), data = df)
#'   }
#' }
morie_quantile_reg <- function(formula, tau = 0.5, data, ...) {
  .morie_nonparam_need("quantreg", "morie_quantile_reg")
  raw <- quantreg::rq(formula = formula, tau = tau, data = data, ...)
  list(method = "quantreg::rq", raw = raw)
}


# ---------------------------------------------------------------------
# np
# ---------------------------------------------------------------------

#' Nonparametric kernel regression via \pkg{np}
#'
#' Thin extender over \code{np::npregbw} + \code{np::npreg} for
#' kernel-smoothed nonparametric regression with data-driven
#' bandwidth selection (Hayfield & Racine, 2008).  Runs the
#' bandwidth-selection routine first and then fits the regression
#' using the chosen bandwidths.
#'
#' @param formula A model formula of the form
#'   \code{y ~ x1 + x2 + ...} passed to \code{np::npregbw}.
#' @param data A data frame containing the variables in
#'   \code{formula}.
#' @param ... Further arguments forwarded to \code{np::npregbw}
#'   (e.g. \code{bwmethod}, \code{bwtype}, \code{ckertype},
#'   \code{regtype}, \code{tol}, \code{ftol}).
#'
#' @return A list with \code{$method = "np::npreg (bws via
#'   npregbw)"} and \code{$raw}, where \code{$raw} is itself a
#'   list with \code{$bws} (the \code{rbandwidth} object from
#'   \code{np::npregbw}) and \code{$fit} (the \code{npregression}
#'   object from \code{np::npreg}).
#' @export
#' @examples
#' \dontrun{
#'   if (requireNamespace("np", quietly = TRUE)) {
#'     set.seed(1)
#'     n  <- 50
#'     df <- data.frame(x = stats::runif(n, -1, 1))
#'     df$y <- sin(pi * df$x) + stats::rnorm(n, sd = 0.1)
#'     morie_np_kernel_reg(y ~ x, data = df)
#'   }
#' }
morie_np_kernel_reg <- function(formula, data, ...) {
  .morie_nonparam_need("np", "morie_np_kernel_reg")
  bws <- np::npregbw(formula = formula, data = data, ...)
  fit <- np::npreg(bws = bws)
  list(
    method = "np::npreg (bws via npregbw)",
    raw = list(bws = bws, fit = fit)
  )
}


# ---------------------------------------------------------------------
# dirichletprocess
# ---------------------------------------------------------------------

#' Bayesian nonparametric DP Gaussian mixture via \pkg{dirichletprocess}
#'
#' Thin extender over
#' \code{dirichletprocess::DirichletProcessGaussian} +
#' \code{dirichletprocess::Fit} for a Bayesian nonparametric
#' Dirichlet-process Gaussian mixture model (Ross & Markwick, 2018;
#' MacEachern, 1994).  Constructs the DP object on \code{y} and
#' then runs the Gibbs sampler for \code{iterations} sweeps.
#'
#' @param y Numeric vector of observations to model with the DP
#'   Gaussian mixture.
#' @param iterations Integer; number of Gibbs-sampler iterations to
#'   run via \code{dirichletprocess::Fit} (default \code{1000}).
#' @param ... Further arguments forwarded to
#'   \code{dirichletprocess::DirichletProcessGaussian}
#'   (e.g. \code{g0Priors}, \code{alphaPriors},
#'   \code{mhDraws}, \code{verbose}).
#'
#' @return A list with
#'   \code{$method = "dirichletprocess::DirichletProcessGaussian + Fit"}
#'   and \code{$raw} (the fitted \code{dirichletprocess} object
#'   after the Gibbs run, containing the cluster assignments,
#'   cluster parameters, and concentration-parameter trace).
#' @export
#' @examples
#' \dontrun{
#'   if (requireNamespace("dirichletprocess", quietly = TRUE)) {
#'     set.seed(1)
#'     y <- c(stats::rnorm(50, -2), stats::rnorm(50, 2))
#'     morie_dp_gaussian_mixture(y, iterations = 200)
#'   }
#' }
morie_dp_gaussian_mixture <- function(y, iterations = 1000, ...) {
  .morie_nonparam_need("dirichletprocess", "morie_dp_gaussian_mixture")
  dp <- dirichletprocess::DirichletProcessGaussian(y, ...)
  fitted <- dirichletprocess::Fit(dp, its = iterations)
  list(
    method = "dirichletprocess::DirichletProcessGaussian + Fit",
    raw = fitted
  )
}


# ---------------------------------------------------------------------
# lcmm
# ---------------------------------------------------------------------

#' Latent-class mixed models via \pkg{lcmm}
#'
#' Thin extender over \code{lcmm::lcmm} for the
#' Proust-Lima et al. (2017) latent-class linear mixed model on
#' longitudinal / repeated-measures data.
#'
#' @param fixed A two-sided formula for the fixed-effects part of
#'   the model.
#' @param random A one-sided formula for the random-effects part
#'   (default \code{~1}, random intercept only).
#' @param subject Character; the name of the column in \code{data}
#'   identifying the subject / grouping variable.
#' @param data A data frame containing the variables in
#'   \code{fixed}, \code{random}, and \code{subject}.
#' @param ng Integer; the number of latent classes (default
#'   \code{2}).
#' @param ... Further arguments forwarded to \code{lcmm::lcmm}
#'   (e.g. \code{mixture}, \code{classmb}, \code{idiag},
#'   \code{nwg}, \code{link}, \code{intnodes}, \code{epsa},
#'   \code{epsb}, \code{epsd}, \code{maxiter}, \code{B},
#'   \code{convB}, \code{convL}, \code{convG}, \code{verbose}).
#'
#' @return A list with \code{$method = "lcmm::lcmm"} and \code{$raw}
#'   (an \code{lcmm} object with the class-membership probabilities,
#'   class-specific fixed-effect estimates, and convergence
#'   diagnostics).
#' @export
#' @examples
#' \dontrun{
#'   if (requireNamespace("lcmm", quietly = TRUE)) {
#'     data("data_hlme", package = "lcmm")
#'     morie_lcmm_latent_class(
#'       fixed   = Y ~ Time,
#'       random  = ~ Time,
#'       subject = "ID",
#'       data    = data_hlme,
#'       ng      = 2,
#'       mixture = ~ Time
#'     )
#'   }
#' }
morie_lcmm_latent_class <- function(fixed, random = ~1, subject, data,
                                    ng = 2, ...) {
  .morie_nonparam_need("lcmm", "morie_lcmm_latent_class")
  raw <- lcmm::lcmm(
    fixed   = fixed,
    random  = random,
    subject = subject,
    data    = data,
    ng      = ng,
    ...
  )
  list(method = "lcmm::lcmm", raw = raw)
}
