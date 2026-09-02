# SPDX-License-Identifier: AGPL-3.0-or-later

#' Bayesian hierarchical spatial models for disease mapping
#'
#' The first stage is the relative-risk model of eq (6.99), where the
#' observed-to-expected ratio is the standardized mortality ratio and the
#' maximum likelihood estimate of the relative risk. Covariates and random
#' effects enter through eq (6.100).
#'
#' What distinguishes the models is the prior on the random effects.
#' `exchangeable` is eq (6.102), aspatial, adding excess variation only.
#' `icar` is eq (6.104), conditionally specified with a singular structure
#' matrix, so the prior is improper. `lcar` is the Leroux prior, which nests
#' both -- rho = 0 is exchangeable, rho = 1 is intrinsic -- replacing two
#' variance components the data cannot separate with one interpretable
#' parameter.
#'
#' Every space-time interaction type except Type I has a rank-deficient
#' structure matrix, and without constraints the interaction is confounded
#' with the main time effect. The remedy is to condition on
#' \eqn{A\delta = 0} with the rows of A spanning the null space, and the
#' number of constraints always equals the rank deficiency. Both are computed
#' and returned, so the count is never guessed.
#'
#' This specifies and diagnoses the model rather than sampling from it.
#'
#' @param counts,expected Observed and expected counts.
#' @param adjacency Symmetric 0/1 contiguity matrix.
#' @param spatial_prior One of "exchangeable", "icar", "lcar".
#' @param rho Leroux smoothing parameter in \[0, 1\].
#' @param n_time Number of periods; required for temporal or interaction terms.
#' @param temporal_prior One of "none", "rw1", "rw2".
#' @param interaction One of "I", "II", "III", "IV", or NULL.
#' @param sigma2 Scale parameter.
#' @return A list with `smr`, `precision`, `spatial_prior`, and when a
#'   temporal structure is present `temporal_structure`, `interaction_rank`,
#'   `rank_deficiency`, `n_constraints` and `constraint_matrix`.
#' @references Schabenberger Ch 6, Sec 6.4, eqs (6.99)-(6.104); Tonui,
#'   Mwalili and Wanjoya (2018), Open Journal of Statistics 8:811-830
#' @export
spbayr <- function(counts, expected, adjacency, spatial_prior = "lcar",
                   rho = 0.5, n_time = NULL, temporal_prior = "none",
                   interaction = NULL, sigma2 = 1) {
  if (!spatial_prior %in% c("exchangeable", "icar", "lcar")) {
    stop("`spatial_prior` must be 'exchangeable', 'icar' or 'lcar'",
         call. = FALSE)
  }
  if (!temporal_prior %in% c("none", "rw1", "rw2")) {
    stop("`temporal_prior` must be 'none', 'rw1' or 'rw2'", call. = FALSE)
  }
  y <- as.numeric(counts)
  e <- as.numeric(expected)
  R <- .schab_neighbour_structure(adjacency)
  n <- nrow(R)
  if (identical(spatial_prior, "exchangeable")) {
    Q <- diag(n)
    note <- "eq (6.102): aspatial, adds excess variation only"
  } else if (identical(spatial_prior, "icar")) {
    Q <- R
    note <- paste("eq (6.104): singular, so the prior is improper and the",
                  "covariance requires a Moore-Penrose inverse")
  } else {
    Q <- .schab_lcar_precision(R, rho, sigma2)
    note <- sprintf(paste("Leroux: rho = %s; rho = 0 is exchangeable, rho = 1",
                          "is intrinsic, and unlike the BYM convolution it is",
                          "identifiable"), format(rho))
  }
  out <- list(smr = .schab_smr(y, e), precision = Q, structure = R,
              spatial_prior = spatial_prior, prior_note = note, n_areas = n,
              sigma2 = as.numeric(sigma2),
              rank_deficiency_spatial = n - qr(Q)$rank)
  if (identical(spatial_prior, "lcar")) out$rho <- as.numeric(rho)

  if (!identical(temporal_prior, "none") || !is.null(interaction)) {
    if (is.null(n_time)) {
      stop("`n_time` is required for a temporal or interaction structure",
           call. = FALSE)
    }
    order <- if (identical(temporal_prior, "rw2")) 2L else 1L
    Rt <- .schab_random_walk_structure(n_time, order)
    out$temporal_structure <- Rt
    out$temporal_prior <- temporal_prior
    out$rank_deficiency_temporal <- as.integer(n_time) - qr(Rt)$rank
    if (!is.null(interaction)) {
      inter <- .schab_interaction_structure(R, Rt, interaction)
      con <- .schab_null_space_constraints(inter$structure)
      out$interaction <- interaction
      out$interaction_structure <- inter$structure
      out$interaction_rank <- inter$rank
      out$rank_deficiency <- inter$rank_deficiency
      out$n_constraints <- con$n_constraints
      out$constraint_matrix <- con$A
      out$constraint_note <- if (identical(interaction, "I")) {
        paste("Type I needs none, being of full rank; every other type is",
              "rank deficient and without A delta = 0 the interaction is",
              "confounded with the main time effect")
      } else {
        sprintf(paste("%d constraints are required, one per unit of rank",
                      "deficiency; without them the interaction is confounded",
                      "with the main time effect"), con$n_constraints)
      }
    }
  }
  out$fitting_note <- paste(
    "this specifies and diagnoses the model; fitting requires MCMC or INLA,",
    "and convergence is sensitive to the hyperprior parameters")
  out
}
