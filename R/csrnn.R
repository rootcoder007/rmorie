# SPDX-License-Identifier: AGPL-3.0-or-later

# Internal: nearest-neighbour distance for each row of an (n x d) pattern.
.csrnn_distances <- function(P) {
  d <- as.matrix(stats::dist(P))
  diag(d) <- Inf
  apply(d, 1L, min)
}

# Internal: G-hat(y) = #(y_i <= y) / n, evaluated on `grid`.
.csrnn_G <- function(nn, grid) {
  findInterval(grid, sort(nn)) / length(nn)
}

# Internal: normalise `window` to a d x 2 matrix of (min, max) rows.
.csrnn_window <- function(window, P) {
  d <- ncol(P)
  if (is.null(window)) {
    return(cbind(apply(P, 2L, min), apply(P, 2L, max)))
  }
  if (length(window) != 2L * d) {
    stop("window must give ", d, " (min, max) pairs for ", d,
         "-dimensional coords; got ", length(window), " values.", call. = FALSE)
  }
  # One convention only: d rows of (min, max). Reading a 2 x d matrix as
  # lower/upper corners is equally natural, but at d = 2 both readings
  # have the same shape and denote different regions, so accepting both
  # would allow a silent misinterpretation.
  #
  # A matrix is used as given. Flattening it first would be wrong: R
  # unwinds column-major, so matrix(c(0,1,0,1), 2, 2, byrow = TRUE) would
  # come back as rows (0,0) and (1,1) -- a different, empty region. The
  # Python mirror reshapes row-major and needs no such care.
  bounds <- if (is.matrix(window)) {
    if (!identical(dim(window), c(d, 2L))) {
      stop("a matrix window must be ", d, " x 2, with one (min, max) row ",
           "per dimension; got ", nrow(window), " x ", ncol(window), ".",
           call. = FALSE)
    }
    matrix(as.numeric(window), nrow = d, ncol = 2L)
  } else {
    matrix(as.numeric(window), nrow = d, ncol = 2L, byrow = TRUE)
  }
  if (any(bounds[, 2L] <= bounds[, 1L])) {
    stop("window upper bounds must exceed lower bounds.", call. = FALSE)
  }
  bounds
}

#' Test a point pattern against complete spatial randomness
#'
#' Compares the empirical nearest-neighbour distance distribution
#' \eqn{\hat G(y) = \#(y_i \le y) / n} against complete spatial
#' randomness, through the supremum distance of a Kolmogorov-Smirnov
#' statistic.
#'
#' The reference distribution comes from simulation rather than a closed
#' form. Under CSR on the whole plane the nearest-neighbour distance
#' follows \eqn{G(y) = 1 - \exp(-\lambda\pi y^2)}, but a real pattern is
#' observed inside a bounded window, and points near the edge have no
#' neighbours beyond it. Their nearest-neighbour distances are biased
#' upward, so the closed form no longer holds. Schabenberger & Gotway
#' accordingly compare against the theoretical \eqn{G(h)} "or, if
#' \eqn{G(h)} is not attainable, against the average empirical
#' distribution function from the simulation". Simulating inside the same
#' window makes the edge effect common to data and reference, so it
#' cancels.
#'
#' The p-value is the rank of the observed statistic among the simulated
#' ones, \eqn{p = (1 + \#\{D_i \ge D_{obs}\}) / (1 + nsim)}. That is the
#' convention behind the same authors' worked example: with 200
#' simulations and none exceeding the observed value, they report
#' p = 0.00498 = 1/201.
#'
#' This is a first-order (nearest-neighbour) test. For the second-order
#' Ripley's K comparison against the same null, see
#' \code{\link{morie_tps_ripley_k}}.
#'
#' Mirrors \code{morie.fn.csrkstst} on the Python side.
#'
#' @param coords Numeric matrix of event locations (n x d). A vector is
#'   read as n points on a line.
#' @param window Observation region, as \code{d} (min, max) pairs: a
#'   \code{d x 2} matrix or a flat vector of length \code{2 * d}.
#'   Defaults to the bounding box of \code{coords}. Give it explicitly
#'   when the true region is larger than the observed extent, since the
#'   bounding box of the data is slightly too small and inflates the
#'   simulated intensity.
#' @param cdf Optional function giving the null CDF of the
#'   nearest-neighbour distance. Supplying it replaces the simulation
#'   with a one-sample KS test, which suits only the case where edge
#'   effects are negligible or already corrected.
#' @param nsim Number of CSR patterns simulated in \code{window}.
#'   Default 199. Ignored when \code{cdf} is given.
#' @return Named list with \code{statistic}, \code{p_value},
#'   \code{nn_distances}, \code{mean_nn}, \code{n}, \code{nsim},
#'   \code{method}.
#' @references Schabenberger O & Gotway CA (2005). \emph{Statistical
#'   Methods for Spatial Data Analysis}. Chapman & Hall/CRC, sections
#'   3.3-3.4.
#'
#'   Diggle PJ (2003). \emph{Statistical Analysis of Spatial Point
#'   Patterns}, 2nd edn. Arnold, London.
#' @examples
#' set.seed(1)
#' morie_csr_nn_test(matrix(runif(200), 100, 2), nsim = 49)$p_value
#' @export
morie_csr_nn_test <- function(coords, window = NULL, cdf = NULL,
                              nsim = 199L) {
  P <- if (is.null(dim(coords))) matrix(coords, ncol = 1L) else as.matrix(coords)
  n <- nrow(P)
  if (n < 3L) {
    stop("Need at least 3 events to form nearest-neighbour distances, got ",
         n, ".", call. = FALSE)
  }
  if (!all(is.finite(P))) stop("coords must be finite.", call. = FALSE)

  nn <- .csrnn_distances(P)

  if (!is.null(cdf)) {
    ks <- stats::ks.test(nn, cdf)
    return(list(
      statistic = unname(ks$statistic),
      p_value = ks$p.value,
      nn_distances = nn,
      mean_nn = mean(nn),
      n = n, nsim = 0L,
      method = "One-sample KS against a supplied null CDF"
    ))
  }

  nsim <- as.integer(nsim)
  if (nsim < 1L) stop("nsim must be at least 1, got ", nsim, ".", call. = FALSE)

  bounds <- .csrnn_window(window, P)
  grid <- sort(nn)
  d <- ncol(P)

  sim_G <- matrix(NA_real_, nsim, length(grid))
  for (i in seq_len(nsim)) {
    Q <- matrix(NA_real_, n, d)
    for (j in seq_len(d)) {
      Q[, j] <- stats::runif(n, bounds[j, 1L], bounds[j, 2L])
    }
    sim_G[i, ] <- .csrnn_G(.csrnn_distances(Q), grid)
  }

  G_bar <- colMeans(sim_G)
  d_obs <- max(abs(.csrnn_G(nn, grid) - G_bar))

  # Each simulated pattern is scored against the mean of the others, so
  # none is compared with a reference it helped to build.
  d_sim <- numeric(nsim)
  for (i in seq_len(nsim)) {
    others <- if (nsim > 1L) (G_bar * nsim - sim_G[i, ]) / (nsim - 1L) else G_bar
    d_sim[i] <- max(abs(sim_G[i, ] - others))
  }

  list(
    statistic = d_obs,
    p_value = (1 + sum(d_sim >= d_obs)) / (1 + nsim),
    nn_distances = nn,
    mean_nn = mean(nn),
    simulated_statistics = d_sim,
    n = n, nsim = nsim,
    method = "Monte Carlo KS on G-hat against CSR simulated in the same window"
  )
}
