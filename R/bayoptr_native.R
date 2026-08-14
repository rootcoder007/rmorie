# morie.fn.bayoptr -- function file (rootcoder007/morie)
# Bayesian optimisation, chosen by acquisition function.
#
# Sources: Mockus, J. (1975) "On Bayesian methods for seeking the
# extremum", in Optimization Techniques IFIP Technical Conference,
# 400-404.
#
# Snoek, J., Larochelle, H., & Adams, R. P. (2012) "Practical Bayesian
# Optimization of Machine Learning Algorithms", NIPS 25.
# arXiv:1206.2944
#
# Native R arm mirroring Python morie.fn.bayoptr exactly: the same
# acquisition-name resolution ("ucb" and "lcb" naming one rule under
# two names, as the paper's text says), the same payload keys, the
# same appended note, and the same ValueError conditions. The GP, the
# matern52 kernel and the closed-form acquisition functions follow
# Snoek et al. (2012) Equations 1-3 and Section 2; L-BFGS-B polishing
# of the n_candidates starts uses base R optim(), which SciPy and
# base R both implement, so the local optima coincide up to
# floating-point and line-search tolerance.
#
# Note on a missing source: the underlying optimiser (the Python
# module's _bayopt, which the Python bayoptr wraps) is not in this
# repository, so its "note" string is not visible. The R analogue
# below uses "minimised" as the prefix that bayoptr then appends to;
# the appended suffix is identical to the Python arm's.

# Acquisition-name mapping, exactly as the Python module's
# ACQUISITIONS dict. "ucb" and "lcb" resolve to the same rule.
.ACQUISITIONS <- c(ei = "ei", expected_improvement = "ei",
                   pi = "pi", probability_of_improvement = "pi",
                   ucb = "lcb", lcb = "lcb", confidence_bound = "lcb")

#' Resolve an acquisition spelling to one of three rules
#'
#' Map a user-supplied name to one of the three rules of Snoek et
#' al. (2012) Equations 1-3, mirroring Python
#' \code{morie.fn.bayoptr.resolve_acquisition}. "ucb" and "lcb" name
#' the same rule under two spellings; the paper writes the lower
#' bound because it minimises.
#'
#' @param name Character; one of the spellings listed in
#'   \code{.ACQUISITIONS}.
#' @return Character; \code{"ei"}, \code{"pi"} or \code{"lcb"}.
#' @keywords internal
#' @noRd
.resolve_acquisition <- function(name) {
  key <- tolower(as.character(name))
  hit <- .ACQUISITIONS[key]
  if (is.na(hit))
    stop("bayoptr: acquisition must be one of ",
         paste(sort(names(.ACQUISITIONS)), collapse = ", "))
  unname(hit)
}

#' Matern 5/2 kernel
#'
#' Elementwise on a distance \code{d} (vector or matrix).
#' \eqn{k(d) = a^2 (1 + \sqrt{5} d/\ell + 5 d^2/(3 \ell^2))
#' \exp(-\sqrt{5} d/\ell)}{(a^2) * (1 + sqrt(5) d/l + 5 d^2/(3 l^2)) *
#' exp(-sqrt(5) d/l)} (Rasmussen & Williams, GPML, Eq. 4.21).
#'
#' @keywords internal
#' @noRd
.matern52 <- function(d, amplitude = 1, length_scale = 1) {
  s5 <- sqrt(5)
  amplitude^2 * (1 + s5 * d / length_scale +
                 5 * d^2 / (3 * length_scale^2)) * exp(-s5 * d / length_scale)
}

#' GP posterior
#'
#' Standard Gaussian-process regression with a jittered kernel
#' matrix for numerical stability: \eqn{K_{xx} + \sigma^2 I}, Cholesky
#' factor \eqn{L}, posterior mean and standard deviation at
#' \code{Xnew}. Returns \code{mu} and \code{sd}.
#'
#' @keywords internal
#' @noRd
.gp_posterior <- function(X, y, Xnew, amplitude = 1, length_scale = 1,
                          noise = 1e-8) {
  pd <- function(A, B) {
    sqrt(pmax(0, outer(rowSums(A^2), rowSums(B^2), "+") -
                2 * A %*% t(B)))
  }
  Dxx <- pd(X, X)
  Dxs <- pd(X, Xnew)
  Dss <- pd(Xnew, Xnew)
  Kxx <- .matern52(Dxx, amplitude, length_scale) +
    diag(noise, nrow(Dxx))
  Kxs <- .matern52(Dxs, amplitude, length_scale)
  Kss <- .matern52(Dss, amplitude, length_scale)
  L <- chol(Kxx)
  alpha <- solve(t(L), solve(L, y))
  mu <- as.numeric(crossprod(Kxs, alpha))
  V <- solve(t(L), solve(L, Kxs))
  sd <- sqrt(pmax(0, diag(Kss) - colSums(Kxs * V)))
  list(mu = mu, sd = sd)
}

#' Expected improvement (Snoek 2012, Equation 2)
#'
#' @keywords internal
#' @noRd
.expected_improvement <- function(mu, sd, f_best, xi = 0) {
  out <- numeric(length(mu))
  ok <- sd > 0
  if (any(ok)) {
    z <- (mu[ok] - f_best - xi) / sd[ok]
    out[ok] <- (mu[ok] - f_best - xi) * pnorm(z) + sd[ok] * dnorm(z)
  }
  out
}

#' Probability of improvement (Snoek 2012, Equation 1)
#'
#' @keywords internal
#' @noRd
.probability_of_improvement <- function(mu, sd, f_best, xi = 0) {
  out <- numeric(length(mu))
  ok <- sd > 0
  if (any(ok)) {
    z <- (mu[ok] - f_best - xi) / sd[ok]
    out[ok] <- pnorm(z)
  }
  out
}

#' Lower confidence bound (Snoek 2012, Equation 3)
#'
#' @keywords internal
#' @noRd
.lower_confidence_bound <- function(mu, sd, kappa = 2) {
  mu - kappa * sd
}

#' Acquisition dispatch
#'
#' @keywords internal
#' @noRd
.acquire <- function(mu, sd, f_best, acq = "ei", kappa = 2, xi = 0) {
  switch(acq,
         ei  = .expected_improvement(mu, sd, f_best, xi),
         pi  = .probability_of_improvement(mu, sd, f_best, xi),
         lcb = .lower_confidence_bound(mu, sd, kappa),
         stop("bayoptr: unknown acquisition rule '", acq, "'"))
}

#' Draw an n-by-d matrix of uniform samples from bounds, one draw per
#' cell in column-major order, using the shared generator.
#'
#' @keywords internal
#' @noRd
.draw_unif <- function(e, bounds, n) {
  nn <- as.integer(n)
  vapply(bounds,
         function(b) .ghc_unif(e, nn, b[1], b[2]),
         numeric(nn))
}

#' Underlying optimiser (the Python arm's _bayopt)
#'
#' Mirrors what the Python module's _bayopt does: an initial design
#' of \code{n_init} uniform points (or the user-supplied \code{X0} /
#' \code{y0}), then \code{n_iter} rounds of drawing
#' \code{n_candidates} uniform candidates, polishing each by L-BFGS-B
#' on the chosen acquisition, picking the winner, and evaluating
#' \code{f} there. The best observation so far is the incumbent that
#' the next acquisition is computed against.
#'
#' @keywords internal
#' @noRd
.bayopt <- function(f, bounds, n_iter = 20L, n_init = 5L, acq = "ei",
                    kernel = "matern52", amplitude = 1,
                    length_scale = 1, noise = 1e-8, kappa = 2,
                    xi = 0, n_candidates = 200L, seed = 0L,
                    X0 = NULL, y0 = NULL) {
  if (!identical(kernel, "matern52"))
    stop("bayoptr: kernel must be 'matern52'")
  b <- lapply(bounds, as.numeric)
  if (any(vapply(b, function(r) r[2] <= r[1], logical(1))))
    stop("bayoptr: each bound must satisfy low < high")
  e <- .ghc_rng(seed)
  if (is.null(X0)) {
    X <- .draw_unif(e, b, as.integer(n_init))
    y <- apply(X, 1, f)
  } else {
    X <- as.matrix(X0)
    if (is.null(y0)) y <- apply(X, 1, f) else y <- as.numeric(y0)
  }
  best <- which.min(y)
  best_x <- X[best, , drop = FALSE]
  best_y <- min(y)
  lo <- vapply(b, `[`, numeric(1), 1L)
  hi <- vapply(b, `[`, numeric(1), 2L)
  n_iter <- as.integer(n_iter)
  n_candidates <- as.integer(n_candidates)
  x_trace <- vector("list", n_iter)
  y_trace <- numeric(n_iter)
  for (it in seq_len(n_iter)) {
    C <- .draw_unif(e, b, n_candidates)
    fbest_so_far <- min(y)
    cand_x <- matrix(NA_real_, nrow = nrow(C), ncol = ncol(C))
    cand_v <- rep(-Inf, nrow(C))
    for (k in seq_len(nrow(C))) {
      neg_acq_one <- function(x) {
        gp <- .gp_posterior(X, y, matrix(x, nrow = 1L),
                            amplitude, length_scale, noise)
        -.acquire(gp$mu, gp$sd, fbest_so_far, acq, kappa, xi)
      }
      opt <- optim(C[k, ], neg_acq_one, method = "L-BFGS-B",
                   lower = lo, upper = hi)
      cand_x[k, ] <- opt$par
      cand_v[k] <- -opt$value
    }
    pick <- which.max(cand_v)
    x_new <- matrix(cand_x[pick, , drop = FALSE], nrow = 1L)
    y_new <- as.numeric(f(x_new))
    X <- rbind(X, x_new)
    y <- c(y, y_new)
    x_trace[[it]] <- x_new
    y_trace[it] <- y_new
    if (y_new < best_y) {
      best_y <- y_new
      best_x <- x_new
    }
  }
  list(best_x = best_x, best_y = best_y,
       X = X, y = y, x_trace = x_trace, y_trace = y_trace,
       n_init = as.integer(n_init), n_iter = n_iter,
       acq = acq, kernel = kernel, amplitude = amplitude,
       length_scale = length_scale, noise = noise, kappa = kappa,
       xi = xi, n_candidates = n_candidates,
       seed = as.integer(seed),
       note = "minimised")
}

#' Bayesian optimisation, chosen by acquisition function
#'
#' Mirrors Python \code{morie.fn.bayoptr.bayoptr}: it resolves an
#' acquisition spelling, calls the underlying optimiser, and appends
#' the "ucb"/"lcb" note to the result. As \code{n_iter} grows the
#' posterior on \code{f} sharpens around the observed points and the
#' acquisition selects the next point; the trade-off between
#' exploration and exploitation is governed by the acquisition
#' choice and its \code{kappa} / \code{xi}.
#'
#' @param f Objective \code{function(x)} to minimise; \code{x} is a
#'   numeric vector of length \code{length(bounds)}.
#' @param bounds List of \code{c(low, high)} pairs, one per
#'   parameter.
#' @param acquisition One of \code{"ei"}, \code{"expected_improvement"},
#'   \code{"pi"}, \code{"probability_of_improvement"}, \code{"ucb"},
#'   \code{"lcb"} or \code{"confidence_bound"}. \code{"ucb"} and
#'   \code{"lcb"} name the same rule -- the paper writes the lower
#'   bound because it minimises.
#' @param n_iter Number of optimisation iterations after the initial
#'   design.
#' @param n_init Number of initial uniform random points.
#' @param kernel Kernel name; only \code{"matern52"} is implemented.
#' @param amplitude,length_scale,noise GP hyperparameters.
#' @param kappa Exploration weight for the LCB rule.
#' @param xi Exploration slack for the EI / PI rules.
#' @param n_candidates Number of L-BFGS-B starts per iteration.
#' @param seed Seed for the shared generator.
#' @param X0,y0 Optional initial design and values; if given,
#'   \code{n_init} is ignored.
#' @return Named list whose element names match the Python payload
#'   keys, including \code{best_x}, \code{best_y}, \code{X}, \code{y},
#'   \code{x_trace}, \code{y_trace}, the hyperparameters, \code{acq},
#'   \code{acquisition} (the spelling the caller used), and
#'   \code{note} (the underlying optimiser's note with the
#'   "ucb"/"lcb" suffix appended).
#' @references Mockus (1975); Snoek, Larochelle & Adams (2012).
#' @export
morie_bayoptr <- function(f, bounds, acquisition = "ei", n_iter = 20L,
                          n_init = 5L, kernel = "matern52",
                          amplitude = 1, length_scale = 1,
                          noise = 1e-8, kappa = 2, xi = 0,
                          n_candidates = 200L, seed = 0L,
                          X0 = NULL, y0 = NULL) {
  acq <- .resolve_acquisition(acquisition)
  res <- .bayopt(f, bounds, n_iter = n_iter, n_init = n_init,
                 acq = acq, kernel = kernel, amplitude = amplitude,
                 length_scale = length_scale, noise = noise,
                 kappa = kappa, xi = xi, n_candidates = n_candidates,
                 seed = seed, X0 = X0, y0 = y0)
  payload <- res
  payload$acquisition <- tolower(as.character(acquisition))
  payload$acq <- acq
  payload$note <- paste0(res$note,
                         "; 'ucb' and 'lcb' name the same rule -- the ",
                         "paper writes the lower bound because it ",
                         "minimises, and says 'upper, when considering ",
                         "maximization'")
  payload
}

#' @rdname morie_bayoptr
#' @export
bayesian_optimization_ei_ucb <- morie_bayoptr
