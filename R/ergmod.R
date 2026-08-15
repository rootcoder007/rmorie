# SPDX-License-Identifier: AGPL-3.0-or-later

#' Exponential random graph model
#'
#' Formula: P(G = g) = exp(theta' s(g)) / kappa(theta), the Markov graph
#' family of Frank & Strauss (1986).
#'
#' Fitted by MAXIMUM PSEUDO-LIKELIHOOD.  The normalising constant kappa
#' is intractable, so the full likelihood needs MCMC; the
#' pseudo-likelihood replaces it with the product of the conditional dyad
#' probabilities, each exactly logistic in the CHANGE STATISTICS
#' delta_ij = s(g with edge ij) - s(g without),
#' logit P(A_ij = 1 | rest) = theta' delta_ij, so the fit is an ordinary
#' logistic regression over the C(n,2) dyads solved by Newton-Raphson.
#' Deterministic and exact -- no sampler -- and the standard starting
#' value for the MCMC-MLE of Hunter & Handcock (2006).  It is NOT the
#' MLE; for dependent terms the pseudo-likelihood estimate is biased,
#' which is precisely why that paper exists.
#'
#' @param G Symmetric 0/1 adjacency matrix with zero diagonal.
#' @param statistics Any of "edges", "twostar", "triangle".
#' @param theta_init Starting values for Newton-Raphson (default zeros).
#' @param iters Maximum Newton steps.
#' @param tol Convergence tolerance on the coefficient change.
#' @return List with \code{estimate}, \code{theta}, \code{se},
#'   \code{observed_stats}, \code{pseudo_loglik}, \code{n_dyads},
#'   \code{iters_used}, \code{n}, \code{method}.
#' @references Frank & Strauss (1986), JASA 81(395):832-842,
#'   doi:10.2307/2289017; Hunter & Handcock (2006), Journal of
#'   Computational and Graphical Statistics 15(3):565-583,
#'   doi:10.1198/106186006X133069.
#' @export
Ergmod <- function(G, statistics = "edges", theta_init = NULL, iters = 100,
                   tol = 1e-11) {
  SUP <- c("edges", "twostar", "triangle")
  A <- .s03mat(G)
  n <- nrow(A)
  if (n == 0L) stop("empty input: G has no nodes")
  if (ncol(A) != n) stop("G must be square")
  if (any(diag(A) != 0)) stop("G must have a zero diagonal")
  if (any(A != 0 & A != 1)) stop("G must be a 0/1 adjacency matrix")
  if (any(A != t(A))) stop("G must be symmetric")
  names <- as.character(statistics)
  if (length(names) == 0L) stop("at least one statistic is required")
  for (nm in names) if (!(nm %in% SUP)) stop(sprintf("unsupported statistic: %s", nm))
  p <- length(names)
  .chg <- function(i, j) {
    vapply(names, function(nm) {
      if (nm == "edges") 1
      else if (nm == "twostar") {
        di <- sum(A[i, ]) - A[i, j]
        dj <- sum(A[j, ]) - A[i, j]
        di + dj
      } else {
        kk <- setdiff(seq_len(n), c(i, j))
        sum(A[i, kk] == 1 & A[j, kk] == 1)
      }
    }, 0, USE.NAMES = FALSE)
  }
  nd <- (n * (n - 1L)) %/% 2L
  if (nd < p) stop("fewer dyads than parameters")
  X <- matrix(0, nd, p); yv <- numeric(nd)
  d <- 0L
  for (i in seq_len(n)) if (i < n) for (j in (i + 1L):n) {
    d <- d + 1L
    X[d, ] <- .chg(i, j)
    yv[d] <- A[i, j]
  }
  th <- if (is.null(theta_init)) numeric(p) else as.numeric(theta_init)
  if (length(th) != p) stop("theta_init must have one entry per statistic")
  used <- 0L
  for (k in seq_len(as.integer(iters))) {
    used <- k
    H <- matrix(0, p, p); g <- numeric(p)
    for (dd in seq_len(nd)) {
      eta <- sum(X[dd, ] * th)
      mu <- .s03sigmoid(eta)
      wv <- mu * (1 - mu)
      r <- yv[dd] - mu
      for (a in seq_len(p)) {
        g[a] <- g[a] + X[dd, a] * r
        for (cc in seq_len(p)) H[a, cc] <- H[a, cc] + wv * X[dd, a] * X[dd, cc]
      }
    }
    step <- tryCatch(.s03cholsolve(H, g), error = function(e)
      stop(paste("pseudo-likelihood Hessian is singular: the dyad",
                 "regression is separated or the change statistics are",
                 "collinear on this graph")))
    th <- th + step
    if (max(abs(step)) < as.numeric(tol)) break
  }
  H <- matrix(0, p, p); ll <- 0
  for (dd in seq_len(nd)) {
    eta <- sum(X[dd, ] * th)
    mu <- .s03sigmoid(eta)
    ll <- ll + yv[dd] * log(mu) + (1 - yv[dd]) * log(1 - mu)
    wv <- mu * (1 - mu)
    for (a in seq_len(p)) for (cc in seq_len(p))
      H[a, cc] <- H[a, cc] + wv * X[dd, a] * X[dd, cc]
  }
  se <- numeric(p)
  for (a in seq_len(p)) {
    e <- numeric(p); e[a] <- 1
    se[a] <- sqrt(tryCatch(.s03cholsolve(H, e), error = function(err)
      stop(paste("pseudo-likelihood Hessian is singular: the dyad",
                 "regression is separated or the change statistics are",
                 "collinear on this graph")))[a])
  }
  obs <- numeric(p)
  for (q in seq_len(p)) {
    nm <- names[q]
    obs[q] <- if (nm == "edges") sum(yv)
      else if (nm == "twostar") { deg <- rowSums(A); sum(deg * (deg - 1) / 2) }
      else {
        t <- 0
        for (i in seq_len(n)) if (i < n) for (j in (i + 1L):n)
          if (A[i, j] == 1 && j < n)
            t <- t + sum(A[i, (j + 1L):n] == 1 & A[j, (j + 1L):n] == 1)
        t
      }
  }
  .t1_result(estimate = th[1], theta = th, se = se, observed_stats = obs,
             pseudo_loglik = ll, n_dyads = nd, iters_used = used, n = n,
             method = "Exponential random graph model (MPLE)")
}
