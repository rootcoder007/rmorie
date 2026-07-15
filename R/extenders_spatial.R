# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) morie contributors
#
# This file is part of morie. morie is free software: you can
# redistribute it and/or modify it under the terms of the GNU Affero
# General Public License as published by the Free Software Foundation,
# either version 3 of the License, or (at your option) any later
# version. See LICENSE for the full text.
#
# Phase 1.m spatial / multivariate / meta-analysis extenders (2026-05-26).
#
# Thin wrapper-as-extender entry points under the canonical
# `morie_<domain>_*` prefix that delegate to five CRAN packages so
# MRM / paper callers can reach their full surface from inside
# rmorie without taking a hard dependency:
#
#   * gstat    -- geostatistical analysis (variograms, kriging)
#   * copula   -- multivariate dependence via copulas
#   * kernlab  -- kernel methods (kernel PCA, spectral clustering)
#   * metafor  -- meta-analysis (random / fixed effects, moderators)
#   * mvtnorm  -- multivariate normal & t distributions
#
# Each function follows the same shape: a requireNamespace guard
# with a hard error pointing to install.packages(), then forwards
# the call and returns a thin two-slot list with
# `$method` (qualified upstream function name) and `$raw` (the
# upstream return object).

#' Spatial / multivariate / meta-analysis extenders (Phase 1.m)
#'
#' Thin wrapper-as-extender entry points that delegate to canonical
#' CRAN packages for geostatistics (\pkg{gstat}), multivariate
#' dependence (\pkg{copula}), kernel methods (\pkg{kernlab}),
#' meta-analysis (\pkg{metafor}) and the multivariate normal /
#' \eqn{t} distributions (\pkg{mvtnorm}).  Each function returns a
#' two-element list with \code{$method} (the qualified upstream
#' function name) and \code{$raw} (the upstream return object), so
#' downstream callers can pattern-match on shape while keeping the
#' full upstream object available for inspection.
#'
#' @name extenders_spatial
NULL


# ---------------------------------------------------------------------
# Internal helper
# ---------------------------------------------------------------------

#' Internal helper: Morie Spatial Need
#' @noRd
.morie_spatial_need <- function(pkg, fn) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      sprintf("%s: install.packages(\"%s\")", fn, pkg),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


# ---------------------------------------------------------------------
# gstat
# ---------------------------------------------------------------------

#' Empirical variogram via \pkg{gstat}
#'
#' Thin extender over \code{gstat::variogram} that computes the
#' sample (semi-)variogram of a spatially-indexed response for use
#' in kriging and other geostatistical workflows.
#'
#' @param formula A formula describing the response and any trend
#'   terms (e.g. \code{z ~ 1} for an intercept-only model), as
#'   expected by \code{gstat::variogram}.
#' @param data A spatial object (e.g. \code{sp::SpatialPointsDataFrame}
#'   or \code{sf} object) or data frame with coordinates available
#'   that \code{gstat::variogram} can consume.
#' @param ... Further arguments forwarded to
#'   \code{gstat::variogram} (e.g. \code{cutoff}, \code{width},
#'   \code{cressie}, \code{cloud}).
#'
#' @return A list with \code{$method = "gstat::variogram"} and
#'   \code{$raw} (a \code{gstatVariogram} data frame with the
#'   binned distances and semivariance estimates).
#' @export
#' @examples
#' \dontrun{
#'   if (requireNamespace("gstat", quietly = TRUE) &&
#'       requireNamespace("sp", quietly = TRUE)) {
#'     data(meuse, package = "sp")
#'     sp::coordinates(meuse) <- ~ x + y
#'     morie_geostat_variogram(log(zinc) ~ 1, data = meuse)
#'   }
#' }
morie_geostat_variogram <- function(formula, data, ...) {
  .morie_spatial_need("gstat", "morie_geostat_variogram")
  raw <- gstat::variogram(object = formula, data = data, ...)
  list(method = "gstat::variogram", raw = raw)
}


#' Kriging interpolation via \pkg{gstat}
#'
#' Thin extender over \code{gstat::krige} for simple, ordinary or
#' universal kriging given a fitted variogram model.
#'
#' @param formula A formula describing the response and trend terms
#'   (e.g. \code{z ~ 1} for ordinary kriging), as expected by
#'   \code{gstat::krige}.
#' @param data Spatial object with the observed locations and
#'   response.
#' @param newdata Spatial object with the prediction locations.
#' @param model A fitted variogram model (e.g. from
#'   \code{gstat::fit.variogram}) describing the spatial
#'   covariance.
#' @param ... Further arguments forwarded to \code{gstat::krige}
#'   (e.g. \code{nmax}, \code{nmin}, \code{block}, \code{beta},
#'   \code{debug.level}).
#'
#' @return A list with \code{$method = "gstat::krige"} and
#'   \code{$raw} (a spatial object with kriging predictions and
#'   variances).
#' @export
#' @examples
#' \dontrun{
#'   if (requireNamespace("gstat", quietly = TRUE) &&
#'       requireNamespace("sp", quietly = TRUE)) {
#'     data(meuse, package = "sp")
#'     data(meuse.grid, package = "sp")
#'     sp::coordinates(meuse) <- ~ x + y
#'     sp::coordinates(meuse.grid) <- ~ x + y
#'     vg <- gstat::variogram(log(zinc) ~ 1, data = meuse)
#'     mod <- gstat::fit.variogram(vg, gstat::vgm(1, "Sph", 900, 1))
#'     morie_geostat_krige(log(zinc) ~ 1, meuse, meuse.grid, mod)
#'   }
#' }
morie_geostat_krige <- function(formula, data, newdata, model, ...) {
  .morie_spatial_need("gstat", "morie_geostat_krige")
  # gstat::krige is an S4 generic; the second formal arg is named
  # `locations`, not `data`. Passing `data = data` leaves `locations`
  # as "missing" and dispatch fails with
  #   unable to find an inherited method for function 'krige'
  #   for signature 'formula = "formula", locations = "missing"'
  # Keep the morie-side parameter named `data` for caller ergonomics
  # but rename at the gstat call site.
  raw <- gstat::krige(
    formula = formula,
    locations = data,
    newdata = newdata,
    model = model,
    ...
  )
  list(method = "gstat::krige", raw = raw)
}


# ---------------------------------------------------------------------
# copula
# ---------------------------------------------------------------------

#' Fit a copula by maximum-likelihood via \pkg{copula}
#'
#' Thin extender over \code{copula::fitCopula} that estimates
#' copula parameters from pseudo-observations on \eqn{[0, 1]^d}.
#'
#' @param copula A \code{copula} object specifying the parametric
#'   family (e.g. \code{copula::normalCopula()},
#'   \code{copula::claytonCopula()}).
#' @param data Numeric matrix of pseudo-observations on
#'   \eqn{[0, 1]^d}, with one column per margin (typically obtained
#'   via \code{copula::pobs}).
#' @param ... Further arguments forwarded to
#'   \code{copula::fitCopula} (e.g. \code{method}, \code{start},
#'   \code{optim.method}, \code{estimate.variance}).
#'
#' @return A list with \code{$method = "copula::fitCopula"} and
#'   \code{$raw} (a \code{fitCopula} object with the estimated
#'   parameters, log-likelihood and variance estimates).
#' @export
#' @examples
#' \dontrun{
#'   if (requireNamespace("copula", quietly = TRUE)) {
#'     set.seed(1)
#'     cop <- copula::normalCopula(0.5, dim = 2)
#'     u <- copula::rCopula(200, cop)
#'     morie_copula_fit(copula::normalCopula(dim = 2), data = u)
#'   }
#' }
morie_copula_fit <- function(copula, data, ...) {
  .morie_spatial_need("copula", "morie_copula_fit")
  raw <- copula::fitCopula(copula = copula, data = data, ...)
  list(method = "copula::fitCopula", raw = raw)
}


#' Draw a random sample from a copula via \pkg{copula}
#'
#' Thin extender over \code{copula::rCopula} that generates
#' \eqn{n} draws on \eqn{[0, 1]^d} from a specified copula.
#'
#' @param n Integer; the number of multivariate observations to
#'   draw.
#' @param copula A \code{copula} object specifying the dependence
#'   structure (e.g. \code{copula::normalCopula()},
#'   \code{copula::claytonCopula()}).
#' @param ... Further arguments forwarded to \code{copula::rCopula}.
#'
#' @return A list with \code{$method = "copula::rCopula"} and
#'   \code{$raw} (a numeric matrix of dimension \eqn{n \times d}
#'   with values in \eqn{[0, 1]}).
#' @export
#' @examples
#' \dontrun{
#'   if (requireNamespace("copula", quietly = TRUE)) {
#'     set.seed(1)
#'     morie_copula_sample(100, copula::claytonCopula(2, dim = 3))
#'   }
#' }
morie_copula_sample <- function(n, copula, ...) {
  .morie_spatial_need("copula", "morie_copula_sample")
  raw <- copula::rCopula(n = n, copula = copula, ...)
  list(method = "copula::rCopula", raw = raw)
}


# ---------------------------------------------------------------------
# kernlab
# ---------------------------------------------------------------------

#' Kernel principal components analysis via \pkg{kernlab}
#'
#' Thin extender over \code{kernlab::kpca} that performs PCA in a
#' feature space induced by a reproducing-kernel.
#'
#' @param x Numeric matrix or data frame of features (rows =
#'   observations).
#' @param ... Further arguments forwarded to \code{kernlab::kpca}
#'   (e.g. \code{kernel}, \code{kpar}, \code{features},
#'   \code{th}).
#'
#' @return A list with \code{$method = "kernlab::kpca"} and
#'   \code{$raw} (a \code{kpca} S4 object with eigenvalues,
#'   eigenvectors and the projected rotated data).
#' @export
#' @examples
#' \dontrun{
#'   if (requireNamespace("kernlab", quietly = TRUE)) {
#'     set.seed(1)
#'     x <- matrix(stats::rnorm(200), ncol = 4)
#'     morie_kernel_pca(x, kernel = "rbfdot", features = 2)
#'   }
#' }
morie_kernel_pca <- function(x, ...) {
  .morie_spatial_need("kernlab", "morie_kernel_pca")
  raw <- kernlab::kpca(x, ...)
  list(method = "kernlab::kpca", raw = raw)
}


#' Spectral clustering via \pkg{kernlab}
#'
#' Thin extender over \code{kernlab::specc} that performs Ng / Jordan
#' / Weiss spectral clustering on a feature matrix.
#'
#' @param x Numeric matrix or data frame of features (rows =
#'   observations).
#' @param centers Integer; the number of clusters to extract.
#' @param ... Further arguments forwarded to \code{kernlab::specc}
#'   (e.g. \code{kernel}, \code{kpar}, \code{nystrom.red},
#'   \code{iterations}).
#'
#' @return A list with \code{$method = "kernlab::specc"} and
#'   \code{$raw} (a \code{specc} S4 object containing the cluster
#'   assignments, centres and within-cluster sums of squares).
#' @export
#' @examples
#' \dontrun{
#'   if (requireNamespace("kernlab", quietly = TRUE)) {
#'     set.seed(1)
#'     x <- rbind(
#'       matrix(stats::rnorm(80, mean = -2), ncol = 2),
#'       matrix(stats::rnorm(80, mean =  2), ncol = 2)
#'     )
#'     morie_spectral_cluster(x, centers = 2)
#'   }
#' }
morie_spectral_cluster <- function(x, centers, ...) {
  .morie_spatial_need("kernlab", "morie_spectral_cluster")
  raw <- kernlab::specc(x, centers = centers, ...)
  list(method = "kernlab::specc", raw = raw)
}


# ---------------------------------------------------------------------
# metafor
# ---------------------------------------------------------------------

#' Random- / fixed-effects meta-analysis via \pkg{metafor}
#'
#' Thin extender over \code{metafor::rma} that fits a (possibly
#' moderated) random- or fixed-effects meta-analytic model to
#' per-study effect sizes and their sampling variances.
#'
#' @param yi Numeric vector of study-level effect-size estimates.
#' @param vi Numeric vector of sampling variances corresponding to
#'   \code{yi}.
#' @param data Optional data frame to evaluate \code{yi}, \code{vi}
#'   and any moderators against; passed straight through to
#'   \code{metafor::rma}.
#' @param ... Further arguments forwarded to \code{metafor::rma}
#'   (e.g. \code{mods}, \code{method}, \code{weights}, \code{test},
#'   \code{level}, \code{slab}).
#'
#' @return A list with \code{$method = "metafor::rma"} and
#'   \code{$raw} (an \code{rma.uni} object with the pooled
#'   estimate, heterogeneity statistics and moderator effects).
#' @export
#' @examples
#' \dontrun{
#'   if (requireNamespace("metafor", quietly = TRUE)) {
#'     set.seed(1)
#'     k <- 12
#'     vi <- stats::runif(k, 0.02, 0.10)
#'     yi <- stats::rnorm(k, mean = 0.3, sd = sqrt(vi))
#'     morie_meta_rma(yi = yi, vi = vi)
#'   }
#' }
morie_meta_rma <- function(yi, vi, data = NULL, ...) {
  .morie_spatial_need("metafor", "morie_meta_rma")
  raw <- metafor::rma(yi = yi, vi = vi, data = data, ...)
  list(method = "metafor::rma", raw = raw)
}


# ---------------------------------------------------------------------
# mvtnorm
# ---------------------------------------------------------------------

#' Sample from the multivariate normal via \pkg{mvtnorm}
#'
#' Thin extender over \code{mvtnorm::rmvnorm} that draws \eqn{n}
#' observations from the multivariate normal distribution with a
#' given mean vector and covariance matrix.
#'
#' @param n Integer; the number of multivariate observations to
#'   draw.
#' @param mean Numeric vector of length \code{ncol(sigma)} giving
#'   the mean (defaults to a zero vector).
#' @param sigma Numeric positive-(semi)definite covariance matrix.
#' @param ... Further arguments forwarded to
#'   \code{mvtnorm::rmvnorm} (e.g. \code{method}, \code{pre0.9_9994},
#'   \code{checkSymmetry}).
#'
#' @return A list with \code{$method = "mvtnorm::rmvnorm"} and
#'   \code{$raw} (a numeric matrix of dimension
#'   \eqn{n \times \mathrm{ncol}(\Sigma)}).
#' @export
#' @examples
#' \dontrun{
#'   if (requireNamespace("mvtnorm", quietly = TRUE)) {
#'     set.seed(1)
#'     S <- matrix(c(1, 0.4, 0.4, 1), 2, 2)
#'     morie_mvnorm_sample(100, mean = c(0, 0), sigma = S)
#'   }
#' }
morie_mvnorm_sample <- function(n, mean = rep(0, ncol(sigma)), sigma, ...) {
  .morie_spatial_need("mvtnorm", "morie_mvnorm_sample")
  raw <- mvtnorm::rmvnorm(n = n, mean = mean, sigma = sigma, ...)
  list(method = "mvtnorm::rmvnorm", raw = raw)
}


#' Multivariate normal rectangle probability via \pkg{mvtnorm}
#'
#' Thin extender over \code{mvtnorm::pmvnorm} that evaluates the
#' multivariate normal CDF over a hyper-rectangle
#' \eqn{[lower, upper]}.
#'
#' @param lower Numeric vector of lower integration limits
#'   (\code{-Inf} permitted).
#' @param upper Numeric vector of upper integration limits
#'   (\code{Inf} permitted).
#' @param mean Numeric mean vector of the same length as
#'   \code{lower} (defaults to a zero vector).
#' @param sigma Numeric positive-(semi)definite covariance matrix.
#' @param ... Further arguments forwarded to
#'   \code{mvtnorm::pmvnorm} (e.g. \code{corr}, \code{algorithm},
#'   \code{keepAttr}).
#'
#' @return A list with \code{$method = "mvtnorm::pmvnorm"} and
#'   \code{$raw} (a numeric scalar with the estimated probability
#'   and Monte-Carlo error attributes attached).
#' @export
#' @examples
#' \dontrun{
#'   if (requireNamespace("mvtnorm", quietly = TRUE)) {
#'     set.seed(1)
#'     S <- matrix(c(1, 0.4, 0.4, 1), 2, 2)
#'     morie_mvnorm_pmv(
#'       lower = c(-1, -1), upper = c(1, 1),
#'       mean = c(0, 0), sigma = S
#'     )
#'   }
#' }
morie_mvnorm_pmv <- function(lower, upper, mean = rep(0, length(lower)),
                             sigma, ...) {
  .morie_spatial_need("mvtnorm", "morie_mvnorm_pmv")
  raw <- mvtnorm::pmvnorm(
    lower = lower,
    upper = upper,
    mean = mean,
    sigma = sigma,
    ...
  )
  list(method = "mvtnorm::pmvnorm", raw = raw)
}
