# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Research P14: judge-leniency designs (research/lean/P14Instrument.lean; Imbens & Angrist 1994;
# Angrist, Imbens & Rubin 1996; Dobbie, Goldin & Yang 2018).
#
#   Research.P14.itt_decomposition         E[y(d(1))] - E[y(d(0))] = sum_compliers w (y1 - y0) - sum_defiers w (y1 - y0)
#   Research.P14.first_stage_decomposition E[d(1)] - E[d(0)] = P(complier) - P(defier)
#   Research.P14.late_identification       no defiers, P(complier) > 0  =>  Wald = mean effect among compliers
#   Research.P14.wald_with_defiers         otherwise Wald = (P(c) tau_c - P(d) tau_d) / (P(c) - P(d))
#   Research.P14.defiers_can_flip          a witness where every effect is positive and the Wald ratio is -5

#' Judge-leniency instrument on a known population: the exact decomposition
#'
#' With every person's potential treatment under a lenient (\code{d0}) and a
#' harsh (\code{d1}) judge and potential outcomes under release (\code{y0})
#' and detention (\code{y1}), the intention-to-treat contrast splits exactly
#' into the compliers' effect mass minus the defiers' effect mass
#' (\code{Research.P14.itt_decomposition}), the first stage into
#' \eqn{P(c) - P(d)} (\code{first_stage_decomposition}), and with no defiers
#' the Wald ratio is the compliers' mean effect (\code{late_identification});
#' with defiers it is \eqn{(P(c)\tau_c - P(d)\tau_d)/(P(c)-P(d))}
#' (\code{wald_with_defiers}), whose sign can be wrong when every individual
#' effect is positive (\code{defiers_can_flip}). Use it on a simulated or
#' fully specified population to see what a design can and cannot recover;
#' \code{\link{morie_judge_iv}} is the observational counterpart.
#' @param d0,d1 Potential treatment (0/1) under instrument 0 and 1.
#' @param y0,y1 Potential outcomes under treatment 0 and 1.
#' @param weights Optional non-negative weights.
#' @return A list with \code{shares} (complier, defier, always, never),
#'   \code{effects} (mean effect by type, \code{NA} for an empty type),
#'   \code{itt}, \code{first_stage}, \code{wald}, \code{late} (the compliers'
#'   effect), \code{monotone}, \code{ate} (the population mean effect, for
#'   comparison) and \code{theorems}.
#' @examples
#' d0 <- c(0, 0, 1, 0, 1); d1 <- c(1, 1, 1, 0, 0)       # two compliers, an always-taker, a never-taker, a defier
#' y0 <- c(0, 0, 1, 0, 0); y1 <- c(1, 0, 1, 1, 1)
#' morie_judge_iv_population(d0, d1, y0, y1)[c("shares", "wald", "late")]
#' @export
morie_judge_iv_population <- function(d0, d1, y0, y1, weights = NULL) {
  n <- length(d0)
  if (length(d1) != n || length(y0) != n || length(y1) != n) stop("d0, d1, y0 and y1 must have equal length", call. = FALSE)
  if (!all(c(d0, d1) %in% c(0, 1))) stop("d0 and d1 must be 0/1", call. = FALSE)
  if (is.null(weights)) weights <- rep(1, n)
  if (length(weights) != n || any(weights < 0) || sum(weights) <= 0) stop("weights must be non-negative with positive total", call. = FALSE)
  w <- weights / sum(weights)
  type <- ifelse(d0 == 0 & d1 == 1, "complier", ifelse(d0 == 1 & d1 == 0, "defier", ifelse(d0 == 1, "always", "never")))
  eff <- y1 - y0
  shares <- vapply(c("complier", "defier", "always", "never"), function(t) sum(w[type == t]), numeric(1))
  effects <- vapply(c("complier", "defier", "always", "never"), function(t) if (shares[[t]] > 0) sum(w[type == t] * eff[type == t]) / shares[[t]] else NA_real_, numeric(1))
  yz1 <- ifelse(d1 == 1, y1, y0); yz0 <- ifelse(d0 == 1, y1, y0)
  itt <- sum(w * yz1) - sum(w * yz0)
  fs <- sum(w * d1) - sum(w * d0)
  list(shares = shares, effects = effects, itt = itt, first_stage = fs,
       wald = if (fs != 0) itt / fs else NA_real_,
       late = effects[["complier"]], monotone = shares[["defier"]] == 0, ate = sum(w * eff),
       theorems = c("Research.P14.itt_decomposition", "Research.P14.first_stage_decomposition",
                    "Research.P14.late_identification", "Research.P14.wald_with_defiers", "Research.P14.defiers_can_flip"))
}

#' Judge-leniency instrument on observed data: Wald ratio, compliers and the defier sensitivity
#'
#' From an instrument \code{z} (0 lenient, 1 harsh), the treatment \code{d}
#' it moved and the outcome \code{y}, reports the first stage, the
#' intention-to-treat contrast and the Wald ratio. Under monotonicity the
#' Wald ratio is the mean effect among compliers and the type shares are
#' identified: always-takers \eqn{P(d=1 \mid z=0)}, never-takers
#' \eqn{P(d=0 \mid z=1)}, compliers the rest
#' (\code{Research.P14.late_identification}). The defier sensitivity reports,
#' for assumed defier shares and defier effects, the compliers' effect the
#' same Wald ratio would imply (\code{wald_with_defiers}): the estimand depends
#' on an assumption the data cannot check.
#' @param z Instrument (0/1), e.g. a harsh-judge indicator or leniency above the median.
#' @param d Treatment (0/1), e.g. pretrial detention.
#' @param y Outcome.
#' @param weights Optional non-negative weights.
#' @param defier_share,defier_effect Assumed defier shares and effects for the sensitivity table.
#' @return A list with \code{first_stage}, \code{itt}, \code{wald},
#'   \code{shares_if_monotone}, \code{sensitivity} (a data frame of implied
#'   complier effects) and \code{theorems}.
#' @examples
#' set.seed(2)
#' n <- 2000; z <- rbinom(n, 1, 0.5)
#' d0 <- rbinom(n, 1, 0.2); d1 <- pmax(d0, rbinom(n, 1, 0.5))     # monotone
#' d <- ifelse(z == 1, d1, d0); y <- 0.3 * d + rnorm(n)
#' morie_judge_iv(z, d, y)[c("first_stage", "wald")]
#' @export
morie_judge_iv <- function(z, d, y, weights = NULL, defier_share = c(0, 0.05, 0.1), defier_effect = 0) {
  n <- length(z)
  if (length(d) != n || length(y) != n) stop("z, d and y must have equal length", call. = FALSE)
  if (!all(z %in% c(0, 1)) || !all(d %in% c(0, 1))) stop("z and d must be 0/1", call. = FALSE)
  if (is.null(weights)) weights <- rep(1, n)
  if (length(weights) != n || any(weights < 0) || sum(weights) <= 0) stop("weights must be non-negative with positive total", call. = FALSE)
  if (!any(z == 1) || !any(z == 0)) stop("z must take both values", call. = FALSE)
  w <- weights / sum(weights)
  m <- function(v, idx) sum(w[idx] * v[idx]) / sum(w[idx])
  fs <- m(d, z == 1) - m(d, z == 0)
  itt <- m(y, z == 1) - m(y, z == 0)
  wald <- if (fs != 0) itt / fs else NA_real_
  always <- m(d, z == 0); never <- 1 - m(d, z == 1)
  shares <- c(complier = 1 - always - never, always = always, never = never)
  grid <- expand.grid(defier_share = defier_share, defier_effect = defier_effect)
  grid$complier_share <- shares[["complier"]] + grid$defier_share
  grid$implied_complier_effect <- ifelse(grid$complier_share > 0 & !is.na(wald),
                                         (wald * (grid$complier_share - grid$defier_share) + grid$defier_share * grid$defier_effect) / grid$complier_share,
                                         NA_real_)
  list(first_stage = fs, itt = itt, wald = wald, shares_if_monotone = shares, sensitivity = grid,
       theorems = c("Research.P14.late_identification", "Research.P14.wald_with_defiers", "Research.P14.first_stage_decomposition"))
}
