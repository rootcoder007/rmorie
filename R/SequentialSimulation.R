.ss_d <- function(a, b) sqrt(sum((a - b)^2))

.ss_krige <- function(P, V, q, model, kriging, mean, extra = NULL) {
  n <- nrow(P)
  c00 <- KrigingCovariance(0, model)
  c0 <- if (n) vapply(seq_len(n), function(i) KrigingCovariance(.ss_d(P[i, ], q), model), 0) else numeric(0)
  C <- if (n) outer(seq_len(n), seq_len(n), Vectorize(function(i, j) KrigingCovariance(.ss_d(P[i, ], P[j, ]), model)))
  if (!is.null(extra)) {
    rho <- extra[1]
    C2 <- if (n) rbind(cbind(C, rho * c0), c(rho * c0, 1)) else matrix(1)
    rhs <- c(c0, rho)
    w <- solve(C2, rhs)
    return(c(mean + sum(w * c(V - mean, extra[2])), c00 - sum(w * rhs)))
  }
  if (n == 0) return(c(mean, c00))
  if (kriging == "simple") {
    w <- solve(C, c0)
    return(c(mean + sum(w * (V - mean)), c00 - sum(w * c0)))
  }
  A <- rbind(cbind(C, 1), c(rep(1, n), 0))
  sol <- solve(A, c(c0, 1))
  w <- sol[1:n]
  c(sum(w * V), c00 - sum(w * c0) - sol[n + 1])
}

.ss_path <- function(m, seed, s) {
  u <- .morie_random_uniform(m, seed = seed, stream = 3 * s)
  idx <- seq_len(m)
  for (t in seq_len(m)) {
    j <- t + floor(u[t] * (m - t + 1))
    tmp <- idx[t]
    idx[t] <- idx[j]
    idx[j] <- tmp
  }
  idx
}

.ss_nearest <- function(P, q, nmax) {
  d <- vapply(seq_len(nrow(P)), function(i) .ss_d(P[i, ], q), 0)
  order(d, seq_along(d))[seq_len(min(nmax, length(d)))]
}

#' Sequential Gaussian and indicator simulation
#'
#' \code{NormalScore} and \code{BackTransform}: GSLIB normal-score transform
#' and its linear back-transform. \code{SgsSimulate}: sequential Gaussian
#' simulation with simple or ordinary kriging on a Philox random path, and
#' collocated co-simulation under Markov model 1. \code{SisSimulate}:
#' sequential indicator simulation (continuous thresholds with order-relation
#' correction, or categories). \code{SimulationSummary}: E-type, variance,
#' percentile and exceedance maps, histogram reproduction and block
#' averages. \code{SgsCrossValidation}: leave-one-out validation.
#' \code{TransitionMatrix} and \code{MarkovChainSimulate}: transition
#' probabilities and categorical Markov-chain simulation. Identical to the
#' Python arm \code{morie.fn.seqsim} (paths and classes are 1-based here).
#'
#' @param z Data values (normal scores for \code{SgsSimulate}).
#' @param y Normal scores to back-transform.
#' @param table_z,table_y Transformation table.
#' @param coords Two-column data coordinates.
#' @param grid Two-column simulation nodes.
#' @param model Covariance model (see \code{KrigingCovariance}).
#' @param kriging \code{"simple"} or \code{"ordinary"}.
#' @param mean Simple kriging mean.
#' @param nmax Maximum conditioning values.
#' @param nsim Number of realisations.
#' @param seed Philox seed.
#' @param secondary Standardised secondary value at each grid node.
#' @param rho Primary-secondary correlation.
#' @param thresholds Thresholds or category codes.
#' @param models Indicator covariance model(s).
#' @param categorical Categorical variable.
#' @param proportions Prior proportions.
#' @param realizations Matrix of realisations (rows).
#' @param probs Percentile probabilities.
#' @param threshold Exceedance threshold.
#' @param data Data for histogram reproduction.
#' @param blocks Block id per node.
#' @param level Interval level.
#' @param sequences List of categorical sequences.
#' @param categories Category order.
#' @param T Transition matrix.
#' @param start Initial category (0-based).
#' @param n Sequence length.
#' @param conditioning Named list mapping 0-based positions to categories.
#' @return List or vector.
#' @references Deutsch, C. V. and Journel, A. G. (1998). GSLIB:
#'   Geostatistical Software Library and User's Guide, 2nd ed. Oxford
#'   University Press.
#'
#'   Almeida, A. S. and Journel, A. G. (1994). Joint simulation of multiple
#'   variables with a Markov-type coregionalization model. Mathematical Geology
#'   26, 565-588.
#'
#'   Carle, S. F. and Fogg, G. E. (1996). Transition probability-based
#'   indicator geostatistics. Mathematical Geology 28, 453-476.
#' @examples
#' NormalScore(c(3, 1, 2))$scores
#' m <- list(model = "Exp", psill = 1, range = 2)
#' SgsSimulate(rbind(c(0, 0)), 1, rbind(c(0, 0), c(5, 0)), m, seed = 2)$realizations
#' @export
NormalScore <- function(z) {
  n <- length(z)
  o <- order(z, seq_len(n))
  y <- numeric(n)
  y[o] <- stats::qnorm((seq_len(n) - 0.5) / n)
  list(scores = y, table_z = z[o], table_y = y[o])
}

#' @rdname NormalScore
#' @export
BackTransform <- function(y, table_z, table_y) {
  vapply(y, function(v) {
    if (v <= table_y[1]) return(table_z[1])
    if (v >= table_y[length(table_y)]) return(table_z[length(table_z)])
    k <- which(table_y >= v)[1]
    t <- if (table_y[k] > table_y[k - 1]) (v - table_y[k - 1]) / (table_y[k] - table_y[k - 1]) else 0
    table_z[k - 1] + t * (table_z[k] - table_z[k - 1])
  }, 0)
}

#' @rdname NormalScore
#' @export
SgsSimulate <- function(coords, z, grid, model, kriging = "simple", mean = 0, nmax = 16, nsim = 1, seed = 1,
                        secondary = NULL, rho = 0) {
  D <- matrix(as.numeric(as.matrix(coords)), ncol = ncol(as.matrix(coords)))
  G <- matrix(as.numeric(as.matrix(grid)), ncol = ncol(as.matrix(grid)))
  m <- nrow(G)
  reals <- matrix(0, nsim, m)
  paths <- matrix(0L, nsim, m)
  for (s in seq_len(nsim) - 1) {
    path <- .ss_path(m, seed, s)
    e <- .morie_random_normal(m, seed = seed, stream = 3 * s + 1)
    cP <- D
    cV <- z
    for (k in seq_len(m)) {
      g <- path[k]
      q <- G[g, ]
      same <- which(vapply(seq_len(nrow(D)), function(i) .ss_d(D[i, ], q) == 0, TRUE))
      if (length(same)) {
        reals[s + 1, g] <- z[same[1]]
        next
      }
      nb <- .ss_nearest(cP, q, nmax)
      extra <- if (!is.null(secondary)) c(rho, secondary[g]) else NULL
      kv <- .ss_krige(cP[nb, , drop = FALSE], cV[nb], q, model, if (is.null(extra)) kriging else "simple", mean, extra)
      val <- kv[1] + sqrt(max(kv[2], 0)) * e[k]
      reals[s + 1, g] <- val
      cP <- rbind(cP, q)
      cV <- c(cV, val)
    }
    paths[s + 1, ] <- path
  }
  list(realizations = reals, paths = paths)
}

.ss_order <- function(F) {
  F <- pmin(1, pmax(0, F))
  up <- cummax(F)
  dn <- rev(cummin(rev(F)))
  (up + dn) / 2
}

#' @rdname NormalScore
#' @export
SisSimulate <- function(coords, z, grid, thresholds, models, nmax = 16, nsim = 1, seed = 1, categorical = FALSE,
                        proportions = NULL) {
  D <- matrix(as.numeric(as.matrix(coords)), ncol = ncol(as.matrix(coords)))
  G <- matrix(as.numeric(as.matrix(grid)), ncol = ncol(as.matrix(grid)))
  Tk <- if (categorical) thresholds else sort(thresholds)
  K <- length(Tk)
  mods <- if (!is.null(models$model)) rep(list(models), K) else models
  ind <- if (categorical) outer(z, Tk, function(a, b) (abs(a - b) < 1e-12) + 0) else outer(z, Tk, `<=`) + 0
  prior <- if (is.null(proportions)) colMeans(ind) else proportions
  reals <- matrix(0L, nsim, nrow(G))
  for (s in seq_len(nsim) - 1) {
    path <- .ss_path(nrow(G), seed, s)
    u <- .morie_random_uniform(nrow(G), seed = seed, stream = 3 * s + 2)
    cP <- D
    cI <- ind
    for (k in seq_len(nrow(G))) {
      g <- path[k]
      q <- G[g, ]
      same <- which(vapply(seq_len(nrow(D)), function(i) .ss_d(D[i, ], q) == 0, TRUE))
      if (length(same)) {
        row <- ind[same[1], ]
        reals[s + 1, g] <- if (categorical) which(row == 1)[1] - 1 else (c(which(row == 1), K + 1)[1] - 1)
        next
      }
      nb <- .ss_nearest(cP, q, nmax)
      est <- vapply(seq_len(K), function(j) .ss_krige(cP[nb, , drop = FALSE], cI[nb, j], q, mods[[j]], "simple",
                                                        prior[j])[1], 0)
      if (categorical) {
        pr <- pmax(0, est)
        pr <- if (sum(pr) > 0) pr / sum(pr) else prior
        cls <- which(u[k] < cumsum(pr))[1]
        if (is.na(cls)) cls <- K
        code <- numeric(K)
        code[cls] <- 1
        cls <- cls - 1
      } else {
        pr <- .ss_order(est)
        cls <- c(which(u[k] <= pr), K + 1)[1] - 1
        code <- as.numeric(cls <= seq_len(K) - 1)
      }
      reals[s + 1, g] <- cls
      cP <- rbind(cP, q)
      cI <- rbind(cI, code)
    }
  }
  list(realizations = reals, thresholds = Tk)
}

#' @rdname NormalScore
#' @export
SimulationSummary <- function(realizations, probs = c(0.1, 0.5, 0.9), threshold = NULL, data = NULL, blocks = NULL) {
  R <- as.matrix(realizations)
  ns <- nrow(R)
  out <- list(etype = colMeans(R), variance = if (ns > 1) apply(R, 2, stats::var) else rep(NaN, ncol(R)),
              percentiles = lapply(stats::setNames(probs, format(probs)), function(p) {
                apply(R, 2, stats::quantile, probs = p, type = 7, names = FALSE)
              }))
  if (!is.null(threshold)) out$exceedance <- colMeans(R > threshold)
  if (!is.null(data)) {
    out$ks_distance <- apply(R, 1, function(r) {
      pts <- sort(unique(c(data, r)))
      max(abs(stats::ecdf(r)(pts) - stats::ecdf(data)(pts)))
    })
  }
  if (!is.null(blocks)) {
    ids <- sort(unique(blocks))
    ba <- unname(t(apply(R, 1, function(r) vapply(ids, function(b) mean(r[blocks == b]), 0))))
    if (length(ids) == 1) ba <- t(ba)
    out$block_ids <- ids
    out$block_averages <- ba
    out$block_mean <- colMeans(ba)
    out$block_variance <- if (ns > 1) apply(ba, 2, stats::var) else rep(NaN, length(ids))
  }
  out
}

#' @rdname NormalScore
#' @export
SgsCrossValidation <- function(coords, z, model, kriging = "simple", mean = 0, nmax = 16, nsim = 100, seed = 1,
                               level = 0.9) {
  P <- as.matrix(coords)
  err <- numeric(0)
  inside <- 0
  width <- numeric(0)
  for (i in seq_along(z)) {
    r <- SgsSimulate(P[-i, , drop = FALSE], z[-i], P[i, , drop = FALSE], model, kriging, mean, nmax, nsim, seed + i - 1)
    v <- r$realizations[, 1]
    lo <- stats::quantile(v, (1 - level) / 2, type = 7, names = FALSE)
    hi <- stats::quantile(v, 1 - (1 - level) / 2, type = 7, names = FALSE)
    err <- c(err, z[i] - mean(v))
    inside <- inside + (lo <= z[i] && z[i] <= hi)
    width <- c(width, hi - lo)
  }
  list(etype_error = err, rmse = sqrt(mean(err^2)), coverage = inside / length(z), mean_width = mean(width))
}

#' @rdname NormalScore
#' @export
TransitionMatrix <- function(sequences, categories = NULL) {
  seqs <- if (is.list(sequences)) sequences else list(sequences)
  cats <- if (is.null(categories)) sort(unique(unlist(seqs))) else categories
  K <- length(cats)
  N <- matrix(0, K, K)
  for (s in seqs) {
    a <- match(s[-length(s)], cats)
    b <- match(s[-1], cats)
    for (k in seq_along(a)) N[a[k], b[k]] <- N[a[k], b[k]] + 1
  }
  M <- N / ifelse(rowSums(N) > 0, rowSums(N), 1)
  all <- unlist(seqs)
  list(matrix = M, categories = cats, proportions = vapply(cats, function(k) mean(all == k), 0),
       mean_run_length = ifelse(diag(M) < 1, 1 / (1 - diag(M)), Inf), counts = N)
}

#' @rdname NormalScore
#' @export
MarkovChainSimulate <- function(T, start, n, seed = 1, conditioning = NULL) {
  P <- as.matrix(T)
  u <- .morie_random_uniform(n, seed = seed, stream = 0)
  fix <- if (is.null(conditioning)) list() else conditioning
  get_fix <- function(t) fix[[as.character(t)]]
  out <- if (!is.null(get_fix(0))) get_fix(0) else start
  for (t in seq_len(n - 1)) {
    if (!is.null(get_fix(t))) {
      out <- c(out, get_fix(t))
      next
    }
    row <- P[out[length(out)] + 1, ]
    nx <- which(u[t + 1] < cumsum(row))[1]
    out <- c(out, if (is.na(nx)) length(row) - 1 else nx - 1)
  }
  out
}
