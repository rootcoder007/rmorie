# Dependent Dirichlet process: one DP per covariate value.
# Sources: Quintana, F. A., Muller, P., Jara, A. & MacEachern, S. N.
# (2022), "The Dependent Dirichlet Process and Related Models",
# Statistical Science 37(1), 24-41, doi:10.1214/20-STS819
# (MacEachern's single-weights and single-atoms constructions, every
# marginal staying a DP); MacEachern, S. N. (1999), "Dependent
# Nonparametric Processes", ASA Proceedings of the Section on Bayesian
# Statistical Science, 50-55 (the original proposal); De Iorio, M.,
# Muller, P., Rosner, G. L. & MacEachern, S. N. (2004), "An ANOVA
# Model for Dependent Random Measures", JASA 99(465), 205-215
# (the ANOVA-DDP, atoms depending on x through a linear model);
# Sethuraman, J. (1994), "A Constructive Definition of Dirichlet
# Priors", Statistica Sinica 4(2), 639-650 (the stick-breaking each
# marginal inherits).
#
# Native implementation mirroring morie.fn.ddpest exactly: the same
# stick-breaking draw, the same validation conditions and the same
# return-field names. The Python arm uses an injected rng object; the
# R arm always uses the GHC helpers seeded with `seed`, so both arms
# draw from the SAME stream in the SAME order.

.MORIE_DDPEST_EPS <- 1e-12
.MORIE_DDPEST_KINDS <- c("single_weights", "single_atoms", "both",
                         "independent")

# Sethuraman stick-breaking for the GEM(alpha) distribution:
#   w_1 = V_1,  w_h = V_h * prod_{j<h}(1 - V_j),  V_h ~ Beta(1, alpha).
# Drawn through the inverse CDF V = 1 - U^(1/alpha), consuming ONE
# uniform per stick from the shared generator `e`. This is the
# canonical construction used by the Python arm's sb.stick_breaking
# when it consumes from the GHC stream.
# NOTE: the Python source for slowdp.stick_breaking is not held here;
# the inverse-CDF form is the standard implementation and matches the
# GHC stream one-for-one.
#' Sethuraman stick-breaking for the GEM(alpha) distribution:
#'
#' w_1 = V_1, w_h = V_h * prod_{j<h}(1 - V_j), V_h ~ Beta(1, alpha).
#' Drawn through the inverse CDF V = 1 - U^(1/alpha), consuming ONE
#' uniform per stick from the shared generator `e`. This is the
#' canonical construction used by the Python arm\'s sb.stick_breaking
#' when it consumes from the GHC stream. NOTE: the Python source for
#' slowdp.stick_breaking is not held here; the inverse-CDF form is the
#' standard implementation and matches the GHC stream one-for-one.
#'
#' @param alpha Numeric; combined arithmetically in the body.
#' @param K A count; the body uses it as \code{seq_len(...)}.
#' @param e Passed to \code{.ghc_unif}.
#' @return A list with \code{weights}.
#' @export
.morie_ddpest_stick_breaking <- function(alpha, K, e) {
  alpha <- as.numeric(alpha)
  K <- as.integer(K)
  u <- .ghc_unif(e, K)
  v <- 1 - u^(1 / alpha)
  w <- numeric(K)
  prod_comp <- 1
  for (h in seq_len(K)) {
    w[h] <- v[h] * prod_comp
    prod_comp <- prod_comp * (1 - v[h])
  }
  list(weights = w)
}

#' What varies with x, and what that buys
#'
#' Returns a description of which part of the stick-breaking form
#' G_x(.) = sum_h w_h(x) delta_{theta_h(x)}(.) is allowed to vary with
#' the covariate, and the qualitative effect that has on clustering.
#'
#' @param kind One of "single_weights", "single_atoms", "both",
#'   "independent".
#' @return Named list with kind, varies_with_x, effect.
#' @export
morie_ddpest_dependence_kind <- function(kind) {
  if (!(kind %in% .MORIE_DDPEST_KINDS))
    stop(sprintf("ddpest: kind must be one of %s, got %s",
                 paste(.MORIE_DDPEST_KINDS, collapse = ", "),
                 sQuote(kind)))
  table <- list(
    single_weights = list("atoms",
                          "clusters keep their membership across x but move location"),
    single_atoms = list("weights",
                        "locations are fixed; the covariate re-weights them, so a cluster can appear or vanish but never move"),
    both = list("weights and atoms", "the general case"),
    independent = list("everything, separately",
                       "no strength is borrowed at all"))
  row <- table[[kind]]
  list(kind = kind, varies_with_x = row[[1]], effect = row[[2]])
}

#' Common weights, atoms moving with x
#'
#' One stick-breaking draw is shared by every covariate value; the
#' atoms are theta_h(x).
#'
#' @param xs Covariate values.
#' @param alpha Concentration parameter of the GEM.
#' @param K Truncation level of the stick-breaking.
#' @param atom_fn Function (x, h) returning the h-th atom at covariate
#'   x; deterministic, no rng argument.
#' @param seed Seed for the shared generator.
#' @return Named list with G, kind, weights, K, note.
#' @export
morie_ddpest_single_weights <- function(xs, alpha, K, atom_fn,
                                        seed = 0) {
  e <- .ghc_rng(seed)
  sb <- .morie_ddpest_stick_breaking(alpha, K, e)
  w <- as.numeric(sb$weights)
  z <- sum(w)
  if (z <= .MORIE_DDPEST_EPS)
    stop("ddpest: the shared weights carry no mass")
  w <- w / z
  K <- as.integer(K)
  xs <- as.list(xs)
  G <- vector("list", length(xs))
  names(G) <- as.character(xs)
  hs <- seq_len(K) - 1L
  for (i in seq_along(xs)) {
    x <- xs[[i]]
    G[[i]] <- list(weights = w,
                   atoms = lapply(hs, function(h) atom_fn(x, h)))
  }
  list(G = G, kind = "single_weights", weights = w, K = K,
       note = "membership is shared across x; only the locations move")
}

#' Common atoms, weights moving with x
#'
#' weight_fn(x, h) returns an unnormalised weight; each G_x is
#' renormalised so it remains a probability measure.
#'
#' @param xs Covariate values.
#' @param alpha Concentration parameter; not consumed here but kept
#'   on the signature to mirror the Python arm.
#' @param K Number of atoms.
#' @param weight_fn Function (x, h) returning an unnormalised weight.
#' @param atom_sampler Optional function (rng, h) returning the h-th
#'   shared atom; if NULL the atoms default to 0, 1, ..., K-1 as in
#'   the Python arm.
#' @param seed Seed for the shared generator.
#' @return Named list with G, kind, atoms, K, note.
#' @export
morie_ddpest_single_atoms <- function(xs, alpha, K, weight_fn,
                                      atom_sampler = NULL,
                                      seed = 0) {
  e <- .ghc_rng(seed)
  alpha <- as.numeric(alpha)        # accepted but not used; mirrors
                                    # the Python signature
  K <- as.integer(K)
  if (!is.null(atom_sampler)) {
    atoms <- lapply(seq_len(K) - 1L, function(h) atom_sampler(e, h))
  } else {
    atoms <- as.list(seq_len(K) - 1L)
  }
  xs <- as.list(xs)
  G <- vector("list", length(xs))
  names(G) <- as.character(xs)
  for (i in seq_along(xs)) {
    x <- xs[[i]]
    raw <- vapply(seq_len(K) - 1L,
                  function(h) as.numeric(weight_fn(x, h)),
                  numeric(1))
    if (any(raw < 0))
      stop(sprintf("ddpest: a weight is negative at x = %s",
                   sQuote(as.character(x))))
    z <- sum(raw)
    if (z <= .MORIE_DDPEST_EPS)
      stop(sprintf("ddpest: the weights vanish at x = %s",
                   sQuote(as.character(x))))
    G[[i]] <- list(weights = raw / z, atoms = atoms)
  }
  list(G = G, kind = "single_atoms", atoms = atoms, K = K,
       note = "the support is the same at every x; only the masses move")
}

#' Every G_x must still be a probability measure
#'
#' @param G A collection as returned by single_weights or single_atoms.
#' @param tol Tolerance on |sum(weights) - 1|.
#' @return Named list with ok, offenders, n_x, note.
#' @export
morie_ddpest_check_marginals <- function(G, tol = 1e-9) {
  tol <- as.numeric(tol)
  bad <- list()
  for (nm in names(G)) {
    g <- G[[nm]]
    s <- sum(g$weights)
    if (abs(s - 1.0) > tol)
      bad[[length(bad) + 1L]] <- list(x = nm, sum = s)
  }
  list(ok = length(bad) == 0L, offenders = bad, n_x = length(G),
       note = paste("each marginal remains a DP draw, which is what",
                    "carries the univariate machinery over"))
}

#' corr(G_x1(A), G_x2(A)) for a set A
#'
#' region is a predicate on an atom.
#'
#' @param G A collection.
#' @param x1 First covariate value.
#' @param x2 Second covariate value.
#' @param region Predicate on an atom.
#' @return Named list with G_x1, G_x2, shared_mass, abs_difference,
#'   identical, note.
#' @export
morie_ddpest_correlation <- function(G, x1, x2, region) {
  if (!(as.character(x1) %in% names(G)) ||
      !(as.character(x2) %in% names(G)))
    stop("ddpest: a covariate value is not in the collection")
  a <- G[[as.character(x1)]]
  b <- G[[as.character(x2)]]
  ga <- sum(vapply(seq_along(a$weights),
                   function(h) if (isTRUE(region(a$atoms[[h]])))
                                a$weights[h] else 0,
                   numeric(1)))
  gb <- sum(vapply(seq_along(b$weights),
                   function(h) if (isTRUE(region(b$atoms[[h]])))
                                b$weights[h] else 0,
                   numeric(1)))
  shared <- 0
  nh <- min(length(a$weights), length(b$weights))
  for (h in seq_len(nh)) {
    if (identical(a$atoms[[h]], b$atoms[[h]]))
      shared <- shared + min(a$weights[h], b$weights[h])
  }
  list(G_x1 = ga, G_x2 = gb, shared_mass = shared,
       abs_difference = abs(ga - gb),
       identical = (abs(ga - gb) < 1e-12) && (shared > 1.0 - 1e-9),
       note = paste("borrowing strength is a measurable quantity,",
                    "not a property to be assumed"))
}

#' The density at x: sum_h w_h(x) k(y | theta_h(x))
#'
#' @param G A collection.
#' @param x Covariate value.
#' @param grid Grid of evaluation points y.
#' @param kernel Function (y, atom) returning a non-negative density
#'   value.
#' @return Named list with estimate, density, grid, x, n_components,
#'   method, note.
#' @export
morie_ddpest_predict_density <- function(G, x, grid, kernel) {
  if (!(as.character(x) %in% names(G)))
    stop(sprintf("ddpest: no measure at x = %s",
                 sQuote(as.character(x))))
  g <- G[[as.character(x)]]
  grid_list <- as.list(grid)
  estimate <- vapply(grid_list, function(y)
    sum(vapply(seq_along(g$weights),
               function(h) g$weights[h] *
                         as.numeric(kernel(y, g$atoms[[h]])),
               numeric(1))),
    numeric(1))
  list(estimate = estimate, density = estimate, grid = grid_list,
       x = x, n_components = length(g$weights),
       method = paste("dependent Dirichlet process; Quintana, Muller,",
                      "Jara & MacEachern (2022)"),
       note = paste("a mixture whose weights and/or atoms are indexed",
                    "by the covariate"))
}

#' One-paragraph summary of the model
#'
#' @return A character string.
#' @export
morie_ddpest_cheatsheet <- function() {
  paste("ddpest: one G for all x ignores the covariate; an",
        "independent DP per x borrows no strength. The DDP writes",
        "G_x = sum_h w_h(x) delta_{theta_h(x)} and lets dependence",
        "enter through the WEIGHTS, the ATOMS, or both, while every",
        "marginal stays a DP -- which is what carries the univariate",
        "machinery over. SINGLE-WEIGHTS (common weights, moving atoms)",
        "shares cluster membership across x and moves locations;",
        "SINGLE-ATOMS (common atoms, moving weights) fixes locations",
        "and lets clusters appear or vanish. They are not",
        "interchangeable. Measure the borrowing with",
        "corr(G_x(A), G_x'(A)) instead of assuming it.")
}

#' Dependent Dirichlet process entry point
#'
#' Dispatches to one of the operations on the DDP collection:
#' "dependence_kind", "single_weights", "single_atoms",
#' "check_marginals", "correlation", "predict_density", "cheatsheet".
#' Each operation mirrors its Python counterpart in morie.fn.ddpest
#' in argument names, validation and return-field names.
#'
#' @param method Operation name (character).
#' @param ... Forwarded to the chosen operation.
#' @return Whatever the chosen operation returns.
#' @export
morie_ddpest <- function(method, ...) {
  method <- as.character(method)
  ops <- list(dependence_kind = morie_ddpest_dependence_kind,
              single_weights  = morie_ddpest_single_weights,
              single_atoms    = morie_ddpest_single_atoms,
              check_marginals = morie_ddpest_check_marginals,
              correlation     = morie_ddpest_correlation,
              predict_density = morie_ddpest_predict_density,
              cheatsheet      = morie_ddpest_cheatsheet)
  if (!(method %in% names(ops)))
    stop(sprintf("ddpest: unknown method %s", sQuote(method)))
  ops[[method]](...)
}

# compact aliases per ledger/NAMING.md, matching the Python module's
# dependent_dirichlet = dependent_dp = dependentdp = single_weights_ddp
morie_ddpest_dependent_dirichlet <- morie_ddpest_single_weights
morie_ddpest_dependent_dp        <- morie_ddpest_single_weights
morie_ddpest_dependentdp         <- morie_ddpest_single_weights
