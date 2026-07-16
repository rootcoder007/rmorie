# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) morie contributors
#
# This file is part of morie. morie is free software: you can
# redistribute it and/or modify it under the terms of the GNU Affero
# General Public License as published by the Free Software Foundation,
# either version 3 of the License, or (at your option) any later
# version. See LICENSE for the full text.
#
# Phase 1.k stats extenders (2026-05-26).
#
# Thin wrapper-as-extender entry points under the canonical
# `morie_<pkg>_*` prefix that delegate to five CRAN statistics
# packages so MRM / paper callers can reach the full surface of
# these packages from inside rmorie without taking a hard
# dependency:
#
#   * DescTools  -- effect sizes, agreement, inequality, winsorising
#   * performance -- regression diagnostics & R^2 family
#   * ppcor      -- (semi-)partial correlations with tests
#   * coin       -- conditional / permutation inference
#   * randtests  -- non-parametric tests of randomness
#
# Each function follows the same shape: a requireNamespace guard
# with a hard error pointing to install.packages(), then forwards
# the call and returns a thin two-slot list with
# `$method` (qualified function name) and `$raw` (the CRAN object).

#' Stats extenders (Phase 1.k)
#'
#' Thin wrapper-as-extender entry points that delegate to canonical
#' CRAN statistics packages.  Each function returns a two-element
#' list with \code{$method} (the qualified upstream function name)
#' and \code{$raw} (the upstream return object), so downstream
#' callers can pattern-match on shape while keeping the full
#' upstream object available for inspection.
#'
#' @name extenders_stats
NULL


# ---------------------------------------------------------------------
# Internal helper
# ---------------------------------------------------------------------

#' Internal helper: Morie Stats Need
#' @noRd
.morie_stats_need <- function(pkg, fn) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      sprintf("%s: install.packages(\"%s\")", fn, pkg),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


# ---------------------------------------------------------------------
# DescTools extenders
# ---------------------------------------------------------------------

#' Cramer's V via \pkg{DescTools}
#'
#' Thin extender over \code{DescTools::CramerV} for the symmetric
#' association statistic between two categorical variables.
#'
#' @param x A factor, character vector, or contingency table.
#' @param y Optional second categorical vector when \code{x} is not
#'   already a contingency table.
#' @param ... Further arguments forwarded to
#'   \code{DescTools::CramerV} (e.g. \code{conf.level}).
#' @return A list with \code{$method = "DescTools::CramerV"} and
#'   \code{$raw} (the upstream numeric or matrix).
#' @export
morie_desc_cramers_v <- function(x, y = NULL, ...) {
  .morie_stats_need("DescTools", "morie_desc_cramers_v")
  raw <- if (is.null(y)) {
    DescTools::CramerV(x, ...)
  } else {
    DescTools::CramerV(x, y, ...)
  }
  list(method = "DescTools::CramerV", raw = raw)
}

#' Cohen / Fleiss kappa via \pkg{DescTools}
#'
#' Thin extender over \code{DescTools::CohenKappa} (when \code{y}
#' is supplied) or \code{DescTools::KappaM} (when \code{x} is a
#' multi-rater matrix / data frame) for inter-rater agreement.
#'
#' @param x A vector of ratings (paired with \code{y}) or a matrix /
#'   data frame whose columns are raters.
#' @param y Optional second rater vector when \code{x} is a vector.
#' @param ... Further arguments forwarded to the upstream function.
#' @return A list with \code{$method} (qualified upstream name) and
#'   \code{$raw} (the upstream return object).
#' @export
morie_desc_kappa <- function(x, y = NULL, ...) {
  .morie_stats_need("DescTools", "morie_desc_kappa")
  if (is.null(y)) {
    raw <- DescTools::KappaM(x, ...)
    list(method = "DescTools::KappaM", raw = raw)
  } else {
    raw <- DescTools::CohenKappa(x, y, ...)
    list(method = "DescTools::CohenKappa", raw = raw)
  }
}

#' Winsorize a numeric vector via \pkg{DescTools}
#'
#' Thin extender over \code{DescTools::Winsorize} for symmetric
#' winsorisation by quantile (default 5\% each tail).
#'
#' @param x A numeric vector.
#' @param probs Length-2 numeric vector of lower / upper quantile
#'   probabilities, forwarded to \code{DescTools::Winsorize}.
#' @param ... Further arguments forwarded to
#'   \code{DescTools::Winsorize} (e.g. \code{minval}, \code{maxval},
#'   \code{na.rm}).
#' @return A list with \code{$method = "DescTools::Winsorize"} and
#'   \code{$raw} (the winsorised numeric vector).
#' @export
morie_desc_winsorize <- function(x, probs = c(0.05, 0.95), ...) {
  .morie_stats_need("DescTools", "morie_desc_winsorize")
  # DescTools::Winsorize signature varies across versions: some accept
  # `probs =`, others require explicit `val =` (the numeric boundary
  # pair). Compute the boundaries here so we are version-agnostic.
  val <- stats::quantile(x, probs = probs, na.rm = TRUE)
  raw <- DescTools::Winsorize(x, val = val, ...)
  list(method = "DescTools::Winsorize", raw = raw)
}

#' Gini coefficient via \pkg{DescTools}
#'
#' Thin extender over \code{DescTools::Gini} for the Gini index of
#' (in)equality on a non-negative numeric vector.
#'
#' @param x A non-negative numeric vector.
#' @param ... Further arguments forwarded to \code{DescTools::Gini}
#'   (e.g. \code{weights}, \code{unbiased}, \code{conf.level},
#'   \code{na.rm}).
#' @return A list with \code{$method = "DescTools::Gini"} and
#'   \code{$raw} (the Gini estimate, optionally with CI).
#' @export
morie_desc_gini <- function(x, ...) {
  .morie_stats_need("DescTools", "morie_desc_gini")
  raw <- DescTools::Gini(x, ...)
  list(method = "DescTools::Gini", raw = raw)
}

#' Atkinson inequality index via \pkg{DescTools}
#'
#' Thin extender over \code{DescTools::Atkinson} for the
#' inequality-aversion-parameter family of inequality indices.
#'
#' @param x A non-negative numeric vector.
#' @param parameter Inequality-aversion parameter (default 0.5),
#'   forwarded to \code{DescTools::Atkinson}.
#' @param ... Further arguments forwarded to
#'   \code{DescTools::Atkinson} (e.g. \code{na.rm}).
#' @return A list with \code{$method = "DescTools::Atkinson"} and
#'   \code{$raw} (the Atkinson index).
#' @export
morie_desc_atkinson <- function(x, parameter = 0.5, ...) {
  .morie_stats_need("DescTools", "morie_desc_atkinson")
  raw <- DescTools::Atkinson(x, parameter = parameter, ...)
  list(method = "DescTools::Atkinson", raw = raw)
}


# ---------------------------------------------------------------------
# performance extenders
# ---------------------------------------------------------------------

#' Regression-diagnostic plots via \pkg{performance}
#'
#' Thin extender over \code{performance::check_model} that returns
#' the diagnostic-plot grob list for a fitted model.
#'
#' @param model A fitted model object supported by \pkg{insight} /
#'   \pkg{performance}.
#' @param ... Further arguments forwarded to
#'   \code{performance::check_model} (e.g. \code{check}, \code{panel},
#'   \code{theme}).
#' @return A list with \code{$method = "performance::check_model"}
#'   and \code{$raw} (the upstream \code{check_model} object).
#' @export
morie_performance_check_model <- function(model, ...) {
  .morie_stats_need("performance", "morie_performance_check_model")
  raw <- performance::check_model(model, ...)
  list(method = "performance::check_model", raw = raw)
}

#' R-squared family via \pkg{performance}
#'
#' Thin extender over \code{performance::r2} returning the
#' appropriate R-squared (Nakagawa, McFadden, Tjur, ...) for a
#' supported fitted model.
#'
#' @param model A fitted model object supported by \pkg{performance}.
#' @param ... Further arguments forwarded to \code{performance::r2}
#'   (e.g. \code{tolerance}, \code{ci}, \code{verbose}).
#' @return A list with \code{$method = "performance::r2"} and
#'   \code{$raw} (the upstream R-squared object).
#' @export
morie_performance_r2 <- function(model, ...) {
  .morie_stats_need("performance", "morie_performance_r2")
  raw <- performance::r2(model, ...)
  list(method = "performance::r2", raw = raw)
}

#' Collinearity / VIF check via \pkg{performance}
#'
#' Thin extender over \code{performance::check_collinearity} for
#' the variance-inflation-factor diagnostic.
#'
#' @param model A fitted model object supported by \pkg{performance}.
#' @param ... Further arguments forwarded to
#'   \code{performance::check_collinearity} (e.g. \code{ci},
#'   \code{verbose}).
#' @return A list with \code{$method =
#'   "performance::check_collinearity"} and \code{$raw} (the
#'   upstream VIF data frame).
#' @export
morie_performance_check_collinearity <- function(model, ...) {
  .morie_stats_need(
    "performance", "morie_performance_check_collinearity"
  )
  raw <- performance::check_collinearity(model, ...)
  list(
    method = "performance::check_collinearity",
    raw = raw
  )
}

#' Outlier check via \pkg{performance}
#'
#' Thin extender over \code{performance::check_outliers} for
#' composite outlier detection on a fitted model or numeric data.
#'
#' @param x A fitted model object or numeric data frame supported
#'   by \code{performance::check_outliers}.
#' @param ... Further arguments forwarded to
#'   \code{performance::check_outliers} (e.g. \code{method},
#'   \code{threshold}).
#' @return A list with \code{$method =
#'   "performance::check_outliers"} and \code{$raw} (the upstream
#'   outlier-check object).
#' @export
morie_performance_check_outliers <- function(x, ...) {
  .morie_stats_need(
    "performance", "morie_performance_check_outliers"
  )
  raw <- performance::check_outliers(x, ...)
  list(method = "performance::check_outliers", raw = raw)
}


# ---------------------------------------------------------------------
# ppcor extenders
# ---------------------------------------------------------------------

#' Partial correlation via \pkg{ppcor}
#'
#' Thin extender over \code{ppcor::pcor} (matrix-wise) or
#' \code{ppcor::pcor.test} (when \code{y} and \code{z} are
#' supplied) for partial correlations controlling for one or more
#' variables.
#'
#' @param x Numeric vector, matrix, or data frame.  When \code{y}
#'   and \code{z} are \code{NULL}, the full pairwise partial
#'   correlation matrix is returned via \code{ppcor::pcor}.
#' @param y Optional second numeric vector.
#' @param z Optional numeric vector / matrix of control variables.
#' @param method Correlation method (\code{"pearson"},
#'   \code{"spearman"}, or \code{"kendall"}), forwarded to ppcor.
#' @param ... Further arguments forwarded to the upstream function.
#' @return A list with \code{$method} (qualified upstream name) and
#'   \code{$raw} (the upstream return object).
#' @export
morie_ppcor_partial <- function(x, y = NULL, z = NULL,
                                method = "pearson", ...) {
  .morie_stats_need("ppcor", "morie_ppcor_partial")
  if (is.null(y) || is.null(z)) {
    raw <- morie_partial_cor(x, method = method)
    list(method = "partial_cor (rmorie native)", raw = raw)
  } else {
    raw <- morie_partial_cor_test(x, y, z, method = method)
    list(method = "partial_cor_test (rmorie native)", raw = raw)
  }
}

#' Semi-partial correlation via \pkg{ppcor}
#'
#' Thin extender over \code{ppcor::spcor} (matrix-wise) or
#' \code{ppcor::spcor.test} (when \code{y} and \code{z} are
#' supplied) for semi-partial (part) correlations.
#'
#' @param x Numeric vector, matrix, or data frame.  When \code{y}
#'   and \code{z} are \code{NULL}, the full pairwise semi-partial
#'   correlation matrix is returned via \code{ppcor::spcor}.
#' @param y Optional second numeric vector.
#' @param z Optional numeric vector / matrix of control variables.
#' @param method Correlation method (\code{"pearson"},
#'   \code{"spearman"}, or \code{"kendall"}), forwarded to ppcor.
#' @param ... Further arguments forwarded to the upstream function.
#' @return A list with \code{$method} (qualified upstream name) and
#'   \code{$raw} (the upstream return object).
#' @export
morie_ppcor_semipartial <- function(x, y = NULL, z = NULL,
                                    method = "pearson", ...) {
  .morie_stats_need("ppcor", "morie_ppcor_semipartial")
  if (is.null(y) || is.null(z)) {
    raw <- morie_semipartial_cor(x, method = method)
    list(method = "semipartial_cor (rmorie native)", raw = raw)
  } else {
    zz <- as.matrix(z)
    sp <- morie_semipartial_cor(cbind(x = as.numeric(x),
                                      y = as.numeric(y), zz),
                                method = method)
    raw <- data.frame(estimate = sp$estimate["x", "y"],
                      p.value = sp$p.value["x", "y"],
                      statistic = sp$statistic["x", "y"],
                      n = sp$n, gp = ncol(zz), Method = method,
                      stringsAsFactors = FALSE)
    list(method = "semipartial_cor_test (rmorie native)", raw = raw)
  }
}


# ---------------------------------------------------------------------
# coin extenders
# ---------------------------------------------------------------------

#' General independence test via \pkg{coin}
#'
#' Thin extender over \code{coin::independence_test} for
#' conditional / permutation tests of independence between
#' arbitrary response and covariate combinations.
#'
#' @param formula A model formula, e.g. \code{y ~ x | block}.
#' @param data A data frame.
#' @param ... Further arguments forwarded to
#'   \code{coin::independence_test} (e.g. \code{distribution},
#'   \code{teststat}, \code{ytrafo}, \code{xtrafo}).
#' @return A list with \code{$method =
#'   "coin::independence_test"} and \code{$raw} (an \code{IndependenceTest}
#'   object).
#' @export
morie_coin_independence <- function(formula, data, ...) {
  .morie_stats_need("coin", "morie_coin_independence")
  raw <- coin::independence_test(formula, data = data, ...)
  list(method = "coin::independence_test", raw = raw)
}

#' Permutation Wilcoxon test via \pkg{coin}
#'
#' Thin extender over \code{coin::wilcox_test} for two-sample
#' permutation Wilcoxon (Mann-Whitney) tests.
#'
#' @param formula A two-sided formula \code{y ~ group}, where
#'   \code{group} is a two-level factor.
#' @param data A data frame.
#' @param ... Further arguments forwarded to
#'   \code{coin::wilcox_test} (e.g. \code{distribution},
#'   \code{alternative}).
#' @return A list with \code{$method = "coin::wilcox_test"} and
#'   \code{$raw} (an \code{IndependenceTest} object).
#' @export
morie_coin_wilcoxon <- function(formula, data, ...) {
  .morie_stats_need("coin", "morie_coin_wilcoxon")
  raw <- coin::wilcox_test(formula, data = data, ...)
  list(method = "coin::wilcox_test", raw = raw)
}

#' Permutation one-way ANOVA via \pkg{coin}
#'
#' Thin extender over \code{coin::oneway_test} for the
#' permutation analogue of the classical one-way ANOVA.
#'
#' @param formula A formula \code{y ~ group}, where \code{group} is
#'   a (multi-level) factor.
#' @param data A data frame.
#' @param ... Further arguments forwarded to
#'   \code{coin::oneway_test} (e.g. \code{distribution},
#'   \code{teststat}).
#' @return A list with \code{$method = "coin::oneway_test"} and
#'   \code{$raw} (an \code{IndependenceTest} object).
#' @export
morie_coin_oneway <- function(formula, data, ...) {
  .morie_stats_need("coin", "morie_coin_oneway")
  raw <- coin::oneway_test(formula, data = data, ...)
  list(method = "coin::oneway_test", raw = raw)
}


# ---------------------------------------------------------------------
# randtests extenders
# ---------------------------------------------------------------------

#' Wald-Wolfowitz runs test via \pkg{randtests}
#'
#' Thin extender over \code{randtests::runs.test} for a
#' non-parametric test of randomness in a numeric sequence.
#'
#' @param x A numeric vector.
#' @param ... Further arguments forwarded to
#'   \code{randtests::runs.test} (e.g. \code{alternative},
#'   \code{threshold}, \code{pvalue}, \code{plot}).
#' @return A list with \code{$method = "randtests::runs.test"}
#'   and \code{$raw} (an \code{htest} object).
#' @export
morie_randtests_runs <- function(x, ...) {
  .morie_stats_need("randtests", "morie_randtests_runs")
  raw <- morie_runs_test(x, ...)
  list(method = "runs_test (rmorie native)", raw = raw)
}

#' Turning-point test via \pkg{randtests}
#'
#' Thin extender over \code{randtests::turning.point.test} for the
#' classical turning-point test of randomness in a numeric
#' sequence.
#'
#' @param x A numeric vector.
#' @param ... Further arguments forwarded to
#'   \code{randtests::turning.point.test} (e.g.
#'   \code{alternative}).
#' @return A list with \code{$method =
#'   "randtests::turning.point.test"} and \code{$raw} (an
#'   \code{htest} object).
#' @export
morie_randtests_turning_point <- function(x, ...) {
  .morie_stats_need("randtests", "morie_randtests_turning_point")
  raw <- morie_turning_point_test(x)
  list(method = "turning_point_test (rmorie native)", raw = raw)
}

#' Bartels rank test via \pkg{randtests}
#'
#' Thin extender over \code{randtests::bartels.rank.test} for the
#' Bartels rank von-Neumann test of randomness in a numeric
#' sequence.
#'
#' @param x A numeric vector.
#' @param ... Further arguments forwarded to
#'   \code{randtests::bartels.rank.test} (e.g.
#'   \code{alternative}, \code{pvalue}).
#' @return A list with \code{$method =
#'   "randtests::bartels.rank.test"} and \code{$raw} (an
#'   \code{htest} object).
#' @export
morie_randtests_bartels <- function(x, ...) {
  .morie_stats_need("randtests", "morie_randtests_bartels")
  raw <- morie_bartels_rank_test(x, ...)
  list(method = "bartels_rank_test (rmorie native)", raw = raw)
}
