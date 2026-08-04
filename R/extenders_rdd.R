# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) morie contributors
#
# This file is part of morie. morie is free software: you can
# redistribute it and/or modify it under the terms of the GNU Affero
# General Public License as published by the Free Software Foundation,
# either version 3 of the License, or (at your option) any later
# version. See LICENSE for the full text.
#
# Phase 1.l RDD / IRT extenders (2026-05-26).
#
# Thin wrapper-as-extender entry points under the canonical
# `morie_<domain>_*` prefix that delegate to five CRAN packages so
# MRM / paper callers can reach their full surface from inside
# rmorie without taking a hard dependency:
#
#   * rddensity   -- McCrary-style RD manipulation / density tests
#   * rdlocrand   -- local-randomisation inference for RDD
#   * rdpower     -- power / sample-size calcs for RDD designs
#   * anchors     -- vignette / anchor analysis for ordinal surveys
#   * anominate   -- alpha-NOMINATE ideal-point estimation (IRT)
#
# Each function follows the same shape: a requireNamespace guard
# with a hard error pointing to install.packages(), then forwards
# the call and returns a thin two-slot list with
# `$method` (qualified upstream function name) and `$raw` (the
# upstream return object).

#' RDD / IRT extenders (Phase 1.l)
#'
#' Thin wrapper-as-extender entry points that delegate to canonical
#' CRAN packages for regression-discontinuity diagnostics
#' (\pkg{rddensity}, \pkg{rdlocrand}, \pkg{rdpower}), ordinal
#' vignette analysis (\pkg{anchors}), and alpha-NOMINATE
#' ideal-point estimation (\pkg{anominate}).  Each function returns
#' a two-element list with \code{$method} (the qualified upstream
#' function name) and \code{$raw} (the upstream return object), so
#' downstream callers can pattern-match on shape while keeping the
#' full upstream object available for inspection.
#'
#' @name extenders_rdd
NULL


# ---------------------------------------------------------------------
# Internal helper
# ---------------------------------------------------------------------

#' Internal helper: Morie Rdd Need
#' @noRd
.morie_rdd_need <- function(pkg, fn) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      sprintf("%s: install.packages(\"%s\")", fn, pkg),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


# ---------------------------------------------------------------------
# rddensity
# ---------------------------------------------------------------------

#' McCrary-style RD density test via \pkg{rddensity}
#'
#' Thin extender over \code{rddensity::rddensity} for the
#' manipulation / discontinuity-in-density test of Cattaneo, Jansson
#' & Ma (2020), the modern replacement for the original McCrary
#' (2008) test.
#'
#' @param X Numeric vector of the running / forcing variable.
#' @param cutoff Numeric scalar; the threshold value of \code{X}
#'   defining the discontinuity (default \code{0}).
#' @param ... Further arguments forwarded to
#'   \code{rddensity::rddensity} (e.g. \code{p}, \code{q},
#'   \code{kernel}, \code{vce}, \code{h}).
#'
#' @return A list with \code{$method = "rddensity::rddensity"} and
#'   \code{$raw} (an \code{rddensity} object containing the
#'   estimates and the manipulation test statistic).
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("rddensity", quietly = TRUE)) {
#'   set.seed(1)
#'   x <- c(rnorm(500, -0.2), rnorm(500, 0.2))
#'   morie_rdd_density_test(x, cutoff = 0)
#' }
#' }
morie_rdd_density_test <- function(X, cutoff = 0, ...) {
  .morie_rdd_need("rddensity", "morie_rdd_density_test")
  raw <- rddensity::rddensity(X = X, c = cutoff, ...)
  list(method = "rddensity::rddensity", raw = raw)
}


# ---------------------------------------------------------------------
# rdlocrand
# ---------------------------------------------------------------------

#' Local-randomisation RDD inference via \pkg{rdlocrand}
#'
#' Thin extender over \code{rdlocrand::rdrandinf} for finite-sample
#' randomisation-inference around the cutoff window
#' (Cattaneo, Frandsen & Titiunik, 2015; Cattaneo, Titiunik & Vazquez-Bare,
#' 2016).
#'
#' @param Y Numeric outcome vector.
#' @param R Numeric running / forcing variable.
#' @param wl Numeric scalar; left edge of the randomisation window.
#' @param wr Numeric scalar; right edge of the randomisation window.
#' @param ... Further arguments forwarded to
#'   \code{rdlocrand::rdrandinf} (e.g. \code{statistic}, \code{p},
#'   \code{nulltau}, \code{reps}, \code{seed}).
#'
#' @return A list with \code{$method = "rdlocrand::rdrandinf"} and
#'   \code{$raw} (an \code{rdrandinf} object with the
#'   randomisation-inference p-values and the observed statistic).
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("rdlocrand", quietly = TRUE)) {
#'   set.seed(1)
#'   R <- runif(200, -1, 1)
#'   Y <- 0.5 * R + (R >= 0) * 0.3 + rnorm(200, sd = 0.5)
#'   morie_rdd_local_randinf(Y, R, wl = -0.1, wr = 0.1)
#' }
#' }
morie_rdd_local_randinf <- function(Y, R, wl, wr, ...) {
  .morie_rdd_need("rdlocrand", "morie_rdd_local_randinf")
  raw <- rdlocrand::rdrandinf(Y = Y, R = R, wl = wl, wr = wr, ...)
  list(method = "rdlocrand::rdrandinf", raw = raw)
}


# ---------------------------------------------------------------------
# rdpower
# ---------------------------------------------------------------------

#' Power calculations for RDD designs via \pkg{rdpower}
#'
#' Thin extender over \code{rdpower::rdpower} for sharp / fuzzy RDD
#' power analysis (Cattaneo, Titiunik & Vazquez-Bare, 2019).
#'
#' Named \code{morie_rdd_power_calc} rather than \code{morie_rdd_power}
#' because the latter is already taken in \code{R/rdd.R} by a closed-form
#' analytical power formula that takes scalar \code{(n, tau, sigma)} rather
#' than a data frame; this wrapper preserves that function and offers the
#' full \pkg{rdpower} simulation-based surface alongside it.
#'
#' @param data Numeric matrix or data frame with two columns: the
#'   outcome \eqn{Y} and the running variable \eqn{R} (as expected
#'   by \code{rdpower::rdpower}'s \code{data} argument).
#' @param cutoff Numeric scalar; the cutoff for the running
#'   variable (default \code{0}).
#' @param ... Further arguments forwarded to
#'   \code{rdpower::rdpower} (e.g. \code{tau}, \code{nsamples},
#'   \code{kernel}, \code{vce}, \code{alpha}, \code{rho}).
#'
#' @return A list with \code{$method = "rdpower::rdpower"} and
#'   \code{$raw} (an \code{rdpower} object with the simulated
#'   power and effective sample sizes).
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("rdpower", quietly = TRUE)) {
#'   set.seed(1)
#'   R <- runif(500, -1, 1)
#'   Y <- 0.4 * R + (R >= 0) * 0.2 + rnorm(500, sd = 0.5)
#'   morie_rdd_power_calc(cbind(Y, R), cutoff = 0, tau = 0.2)
#' }
#' }
morie_rdd_power_calc <- function(data, cutoff = 0, ...) {
  .morie_rdd_need("rdpower", "morie_rdd_power_calc")
  raw <- rdpower::rdpower(data = data, cutoff = cutoff, ...)
  list(method = "rdpower::rdpower", raw = raw)
}


# anchors package was archived from CRAN on 2022-03-06 (check problems
# not corrected). The morie_anchors_analyze wrapper that previously
# lived here was dropped because the upstream package is no longer
# available via install.packages(). If anchors returns to CRAN, restore
# this wrapper from git history (commit a9469ec).


# ---------------------------------------------------------------------
# anominate
# ---------------------------------------------------------------------

#' Alpha-NOMINATE ideal-point estimation via \pkg{anominate}
#'
#' Thin extender over \code{anominate::anominate} for Bayesian
#' alpha-NOMINATE ideal-point estimation on roll-call legislative
#' data (Carroll, Lewis, Lo, Poole & Rosenthal, 2013).
#'
#' @param rcObject A roll-call object as built by
#'   \code{pscl::rollcall}, suitable for passing to
#'   \code{anominate::anominate}.
#' @param ... Further arguments forwarded to
#'   \code{anominate::anominate} (e.g. \code{dims}, \code{nsamp},
#'   \code{thin}, \code{burnin}, \code{minvotes}, \code{lop},
#'   \code{polarity}, \code{random.starts}, \code{verbose}).
#'
#' @return A list with \code{$method = "anominate::anominate"} and
#'   \code{$raw} (an \code{anominate} posterior-sample object).
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("anominate", quietly = TRUE) &&
#'   requireNamespace("pscl", quietly = TRUE)) {
#'   data("sen111", package = "anominate") # the rollcall anominate ships
#'   morie_anominate_ideal_points(sen111, dims = 1, nsamp = 200, burnin = 100)
#' }
#' }
morie_anominate_ideal_points <- function(rcObject, ...) {
  .morie_rdd_need("anominate", "morie_anominate_ideal_points")
  raw <- anominate::anominate(rcObject, ...)
  list(method = "anominate::anominate", raw = raw)
}
