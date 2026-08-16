# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of cthrgr -- three-layer causal forest with a mediator. Mirrors
# src/morie/fn/cthrgr.py operation for operation, on the honest forest
# in R/sdcfst_native.R and the shared numerics in
# R/aaa_helpers_w3num.R.
#
# A treatment can work through a mediator or around it. A drug lowers
# mortality partly by lowering blood pressure and partly by something
# else; a training programme raises wages partly by raising skill and
# partly by signalling. Splitting the total effect into those two parts
# is mediation analysis, and doing it without assuming linear models
# anywhere is what the three layers are for:
#
#   layer 1   e(x)      = E[D | X]            the propensity
#   layer 2   mbar(d,x) = E[M | D=d, X]       the mediator model
#   layer 3   mu(d,m,x) = E[Y | D=d, M=m, X]  the outcome model
#
# each an honest regression forest. With those three, the natural
# effects follow from the mediation formula:
#
#   NDE(x) = mu(1, M(0), x) - mu(0, M(0), x)   treatment's own path
#   NIE(x) = mu(1, M(1), x) - mu(1, M(0), x)   the path through M
#   total  = NDE + NIE                          identically
#
# The identity is not approximate and it is checked rather than assumed:
# the two parts are constructed to telescope, and if they ever failed to
# add to the total the decomposition would be meaningless.
#
# How M(d) enters is the choice that separates an honest implementation
# from a convenient one. Plugging the mediator's conditional MEAN is
# exact only if mu is linear in m, and a forest is not linear in
# anything, so that route is offered and labelled rather than hidden.
# The "gcomputed" route averages mu over the mediator's whole
# conditional distribution, represented by the empirical residuals of
# layer two added back to mbar(d, x); it is the g-formula and it is the
# default.
#
# The propensity layer is reported rather than used to reweight: with
# all three forests fitted the plug-in decomposition needs no weighting,
# but a propensity near zero or one means the comparison at that x is
# being extrapolated, and the OVERLAP the module reports is how you find
# that out. A mediation estimate at a covariate value where nobody was
# treated is arithmetic, not evidence.
#
# References
#   Cui, Y. and Tchetgen Tchetgen, E.J. (2024) "Machine intelligence for
#     individualized decision making under a counterfactual world: a
#     rejoinder." Journal of the American Statistical Association
#     119(545), 97-102.
#   Pearl, J. (2001) "Direct and indirect effects." Proceedings of the
#     Seventeenth Conference on Uncertainty in Artificial Intelligence,
#     411-420.
#   Imai, K., Keele, L. and Yamamoto, T. (2010) "Identification,
#     inference and sensitivity analysis for causal mediation effects."
#     Statistical Science 25(1), 51-71.
#   Athey, S., Tibshirani, J. and Wager, S. (2019) "Generalized random
#     forests." The Annals of Statistics 47(2), 1148-1178.
#   VanderWeele, T.J. (2015) "Explanation in Causal Inference: Methods
#     for Mediation and Interaction." Oxford University Press.

.CTHRGR_ROUTES <- c("gcomputed", "mean")

#' Natural direct and indirect effects from three honest forests
#'
#' @param y Outcome.
#' @param D Binary treatment.
#' @param M Mediator.
#' @param X Covariates.
#' @param route A member of the route list.
#' @param n_trees Trees per forest.
#' @param min_leaf Minimum leaf size.
#' @param max_depth Maximum depth.
#' @param seed The random stream.
#' @param n_draw Mediator residuals averaged over, for the g-computed
#'   route. They are an evenly spaced sweep through the sorted
#'   residuals rather than a resample, so the estimate is a fixed
#'   function of the data and not of a random stream.
#' @param newX Points to report at, or NULL for the training rows.
#' @return A list with per-point direct, indirect and total effects,
#'   their averages, the fitted propensity and its overlap, and the
#'   mediator models.
#' @export
morie_cthrgr <- function(y, D, M, X, route = "gcomputed", n_trees = 8L,
                         min_leaf = 3L, max_depth = 3L, seed = 0,
                         n_draw = 8L, newX = NULL) {
  if (!(route %in% .CTHRGR_ROUTES))
    stop("route must be one of ", paste(.CTHRGR_ROUTES, collapse = ", "))
  ys <- as.numeric(y)
  d <- as.numeric(ifelse(as.numeric(D) != 0, 1, 0))
  m <- as.numeric(M)
  xs <- as.matrix(X); storage.mode(xs) <- "double"
  n <- length(ys)
  if (length(d) != n || length(m) != n || nrow(xs) != n)
    stop("y, D, M and X must agree in length")
  if (n < 8L) stop("need at least eight observations")
  if (sum(d) == 0 || sum(d) == n)
    stop("both treatment arms must be present")
  rows <- seq_len(n)

  # Layer one: the propensity, reported for overlap rather than used to
  # reweight.
  e <- .ghc_rng(seed)
  fe <- morie_sdcfst_forest(xs, d, rows, n_trees, NULL, min_leaf,
                            max_depth, e)
  # Layer two: the mediator, with treatment in the design so the arms
  # may differ.
  mx <- cbind(d, xs)
  fm <- morie_sdcfst_forest(mx, m, rows, n_trees, NULL, min_leaf,
                            max_depth, e)
  # Layer three: the outcome on treatment, mediator and covariates.
  ax <- cbind(d, m, xs)
  fy <- morie_sdcfst_forest(ax, ys, rows, n_trees, NULL, min_leaf,
                            max_depth, e)

  # The mediator residuals carry its conditional spread. Sorting them
  # and sweeping evenly is a deterministic quadrature over the empirical
  # distribution -- a resample would put a random stream inside an
  # estimate that has no business being random.
  resid <- sort(vapply(seq_len(n), function(i)
    m[i] - morie_sdcfst_predict(fm, mx[i, ]), numeric(1)),
    method = "radix")
  k <- max(1L, as.integer(n_draw))
  draws <- if (route == "mean" || k == 1L) 0 else
    vapply(0:(k - 1L), function(t) {
      idx <- floor((t + 0.5) * length(resid) / k)
      if (idx >= length(resid)) idx <- length(resid) - 1
      resid[idx + 1L]
    }, numeric(1))

  qx <- if (is.null(newX)) xs else {
    mm <- as.matrix(newX); storage.mode(mm) <- "double"; mm
  }
  q <- nrow(qx)
  nde <- numeric(q); nie <- numeric(q); tot <- numeric(q)
  ps <- numeric(q); m0v <- numeric(q); m1v <- numeric(q)
  for (i in seq_len(q)) {
    x <- qx[i, ]
    ps[i] <- morie_sdcfst_predict(fe, x)
    mb0 <- morie_sdcfst_predict(fm, c(0, x))
    mb1 <- morie_sdcfst_predict(fm, c(1, x))
    m0v[i] <- mb0; m1v[i] <- mb1
    a <- numeric(length(draws)); b <- numeric(length(draws))
    cc <- numeric(length(draws))
    for (j in seq_along(draws)) {
      r <- draws[j]
      y10 <- morie_sdcfst_predict(fy, c(1, mb0 + r, x))
      y00 <- morie_sdcfst_predict(fy, c(0, mb0 + r, x))
      y11 <- morie_sdcfst_predict(fy, c(1, mb1 + r, x))
      a[j] <- y10 - y00; b[j] <- y11 - y10; cc[j] <- y11 - y00
    }
    nde[i] <- .w3_csum(a) / length(a)
    nie[i] <- .w3_csum(b) / length(b)
    tot[i] <- .w3_csum(cc) / length(cc)
  }

  ande <- .w3_csum(nde) / q
  anie <- .w3_csum(nie) / q
  atot <- .w3_csum(tot) / q
  list(direct = nde, indirect = nie, total = tot, propensity = ps,
       mediator_control = m0v, mediator_treated = m1v, nde = ande,
       nie = anie, estimate = atot, se = NaN,
       proportion_mediated = if (atot != 0) anie / atot else NaN,
       overlap_min = min(ps), overlap_max = max(ps),
       n_extreme = sum(ps < 0.05 | ps > 0.95), n_draw = length(draws),
       residual_spread = if (length(resid))
         resid[length(resid)] - resid[1] else 0,
       n = n, n_treated = as.integer(sum(d)), n_query = q, route = route,
       method = "three-layer causal forest with a mediator")
}

#' One-line summary of the cthrgr module
#'
#' @return A character scalar.
#' @export
morie_cthrgr_cheatsheet <- function()
  paste0("cthrgr: three-layer causal forest with a mediator. routes ",
         paste(.CTHRGR_ROUTES, collapse = ", "),
         "; direct plus indirect is the total")
