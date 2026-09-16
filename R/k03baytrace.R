# SPDX-License-Identifier: AGPL-3.0-or-later

#' MCMC trace summaries: running mean and running quantile bands
#'
#' The quantities a trace plot is read for, computed rather than drawn.
#' For each chain and each iteration \eqn{t} the cumulative statistics of
#' the draws so far are reported: the running mean
#' \eqn{m_t = t^{-1}\sum_{s \le t} x_s}, and the running sample quantiles
#' of \eqn{x_1, \ldots, x_t} at \code{probs}. A chain that has converged
#' shows these settling to horizontal lines; a chain that has not shows
#' them still drifting. Comparing the lines across chains is the visual
#' form of the between-chain comparison Gelman--Rubin makes numerically.
#'
#' The running quantiles are the cumulative quantiles
#' \code{coda::cumuplot} draws, computed with the type-7 definition that
#' is the default of \code{quantile}, so that both arms and coda agree.
#'
#' The statistics are cumulative and unsmoothed: nothing is thinned, no
#' burn-in is discarded, and the first few iterations are reported as
#' they are even though a quantile of one or two draws is nearly
#' meaningless.
#'
#' Mirrors \code{morie.fn.baytrace} on the Python side.
#'
#' @param chains Numeric vector (a single chain) or matrix whose columns
#'   are chains and whose rows are iterations.
#' @param probs Numeric vector of probabilities in \[0, 1\] at which the
#'   running bands are reported.
#' @return Named list with \code{running_mean} (one numeric vector per
#'   chain), \code{bands} (per chain, a list of one numeric vector per
#'   probability), \code{probs}, \code{n_chains}, \code{n_iter},
#'   \code{final_mean}, \code{method}.
#' @references Plummer M, Best N, Cowles K & Vines K (2006). CODA:
#'   convergence diagnosis and output analysis for MCMC. \emph{R News}
#'   6(1), 7--11.
#' @examples
#' set.seed(2)
#' Baytrace(matrix(rnorm(200), ncol = 2))$final_mean
#' @export
Baytrace <- function(chains, probs = c(0.025, 0.5, 0.975)) {
  if (is.null(dim(chains))) {
    cols <- list(as.numeric(chains))
  } else {
    m <- as.matrix(chains)
    cols <- lapply(seq_len(ncol(m)), function(j) as.numeric(m[, j]))
  }
  if (length(cols) == 0L || length(cols[[1L]]) == 0L) {
    stop("chains must be non-empty", call. = FALSE)
  }
  n_iter <- length(cols[[1L]])
  pv <- as.numeric(probs)
  if (any(!is.finite(pv)) || any(pv < 0) || any(pv > 1)) {
    stop("probs must lie in [0, 1]", call. = FALSE)
  }

  running_mean <- vector("list", length(cols))
  bands <- vector("list", length(cols))
  for (ci in seq_along(cols)) {
    col <- cols[[ci]]
    run <- numeric(n_iter)
    total <- 0
    for (t in seq_len(n_iter)) {
      total <- total + col[t]
      run[t] <- total / t
    }
    running_mean[[ci]] <- run
    per_prob <- lapply(pv, function(p) numeric(n_iter))
    for (t in seq_len(n_iter)) {
      qs <- stats::quantile(col[seq_len(t)], probs = pv, names = FALSE,
                            type = 7)
      for (k in seq_along(pv)) per_prob[[k]][t] <- qs[k]
    }
    bands[[ci]] <- per_prob
  }

  list(running_mean = running_mean,
       bands = bands,
       probs = pv,
       n_chains = length(cols),
       n_iter = n_iter,
       final_mean = vapply(running_mean, function(r) r[length(r)], 0),
       method = "MCMC running trace summaries (coda cumuplot)")
}
