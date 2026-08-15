# morie.fn -- function file (rootcoder007/morie)
# r"""Harmony: integrating single-cell data across batches.
#
# Korsunsky, I., Millard, N., Fan, J., Slowikowski, K., Zhang, F., Wei, K.,
# Baglaenko, Y., Brenner, M., Loh, P., & Raychaudhuri, S. (2019) "Fast,
# sensitive and accurate integration of single-cell data with Harmony",
# Nature Methods 16(12), 1289-1296. doi:10.1038/s41592-019-0619-0
#
# Harmony alternates two steps until the embedding stops moving
# (Algorithm 1): cluster the cells so that every cluster is *diverse* in
# batch (Algorithm 2), then regress the batch out of each cluster
# (Algorithm 3).
#
# Maximum diversity clustering. Soft spherical k-means with two extra
# terms: an entropy regulariser on the assignments R weighted by sigma,
# and a penalty on the statistical dependence between cluster and batch
# weighted by theta. The dependence is measured by the KL divergence
# between the observed and expected cluster/batch counts,
#
#   O_{kb} = sum_{i in b} R_{ki},   E_{kb} = (N_b/N) * sum_i R_{ki},
#
# (Equations 5 and 6: what is observed, against what independence would
# give). Minimising the whole objective in R has a closed form
# (Equation 8):
#
#   R_{ki} proportional to (O_{ki}/E_{ki})^theta
#                          exp(-2(1 - Y_k^T Z_i)/sigma),
#
# normalised so each cell's memberships sum to one. Distances are cosine,
# so cells and centroids are L2-normalised and 1 - Y_k^T Z_i is the
# cosine distance; centroids are Y = Z R^T followed by that normalisation,
# the soft version of Dhillon's spherical k-means.
#
# At theta = 0 the ratio term is 1 and this is ordinary soft spherical
# k-means; raising theta pushes clusters toward batch independence.
#
# The sign of that exponent. Equation 8 is printed with (O/E)^+theta,
# which raises a cell's membership of clusters where its own batch is
# already over-represented -- the opposite of diversity. The objective
# it is derived from adds +sigma*theta*D_KL and is minimised, and
# d/dR_{ki} of that term is +sigma*theta*log(O/E), so the stationary
# point carries (O/E)^-theta. diversity="penalise" (default) uses the
# negative exponent and "as_printed" the literal one.
#
# Mixture-of-experts correction. Within each cluster the batch is
# regressed out by a ridge fit (Equation 14),
#
#   W_k = (phi* diag(R_k) phi*^T + lambda I)^-1 phi* diag(R_k) Z^T,
#
# where phi* = 1 || phi is the one-hot batch design with an intercept
# prepended. The ridge is not decoration: the one-hot columns sum to the
# intercept, so the unpenalised matrix is singular. The paper sets
# lambda_0 = 0 and lambda_b = 1, penalising every batch term but never
# the intercept.
#
# Then the intercept row of W_k is zeroed before
#
#   Z_hat = Z - sum_k W_k^T phi* diag(R_k),
#
# which is what makes the correction remove batch and keep cell type:
# the intercept carries the batch-independent (cell-type) variation and
# is left in place. It also gives an exactly checkable consequence,
# which the paper states for reference mapping -- a cell whose design
# row is [1, 0, ..., 0] is explained "in terms of an intercept and
# nothing else", so it never moves.
#
# Correction is done in the unnormalised space and the result is
# re-normalised for the next round of clustering, as the paper's caveat
# requires: regression in the normalised space would need rotation
# matrices.
#
# The default cluster count follows the paper's heuristic,
# K = min(100, N/30).
# """

.scintg_signs <- c("penalise", "as_printed")

.scintg_matrix <- function(Z) {
  if (is.data.frame(Z)) Z <- as.matrix(Z)
  if (!is.matrix(Z)) {
    if (is.list(Z)) {
      n <- length(Z)
      if (n == 0) stop("scintg: Z is empty")
      d <- length(Z[[1]])
      if (d == 0) stop("scintg: Z has no columns")
      for (i in seq_len(n)) {
        if (length(Z[[i]]) != d) stop("scintg: Z is ragged")
        for (v in Z[[i]]) {
          if (!is.finite(v)) stop("scintg: Z contains a non-finite value")
        }
      }
      Z <- matrix(unlist(Z), nrow=n, ncol=d, byrow=TRUE)
    } else {
      stop("scintg: Z must be a matrix or list of vectors")
    }
  }
  if (!is.numeric(Z)) stop("scintg: Z must be numeric")
  storage.mode(Z) <- "double"
  if (nrow(Z) == 0) stop("scintg: Z is empty")
  if (ncol(Z) == 0) stop("scintg: Z has no columns")
  if (any(!is.finite(Z))) stop("scintg: Z contains a non-finite value")
  Z
}

.scintg_l2_normalise <- function(Z) {
  n <- sqrt(rowSums(Z * Z))
  n[n <= 0] <- 1
  sweep(Z, 1, n, "/")
}

.scintg_design <- function(batches) {
  names <- sort(unique(as.character(batches)))
  B <- length(names)
  N <- length(batches)
  phi <- matrix(0, nrow=N, ncol=B + 1)
  phi[, 1] <- 1
  idx <- match(as.character(batches), names)
  for (i in seq_len(N)) {
    phi[i, 1 + idx[i]] <- 1
  }
  list(phi=phi, names=names)
}

.scintg_cluster_batch_counts <- function(R, batches, names=NULL) {
  K <- nrow(R)
  N <- ncol(R)
  if (length(batches) != N) stop("scintg: one batch label per cell is required")
  if (is.null(names)) names <- sort(unique(as.character(batches)))
  B <- length(names)
  O <- matrix(0, nrow=K, ncol=B)
  E <- matrix(0, nrow=K, ncol=B)
  batches_chr <- as.character(batches)
  for (bi in seq_len(B)) {
    members <- which(batches_chr == names[bi])
    Nb <- length(members)
    O[, bi] <- rowSums(R[, members, drop=FALSE])
    E[, bi] <- (Nb / N) * rowSums(R)
  }
  list(O=O, E=E, batches=names)
}

.scintg_harmony_objective <- function(Z, R, Y, batches, sigma=0.1, theta=2.0) {
  K <- nrow(R)
  N <- ncol(R)
  cos_sim <- Y %*% t(Z)
  fit <- 2 * sum(R * (1 - cos_sim))
  R_pos <- pmax(R, 1e-300)
  ent <- sum(R * log(R_pos))
  c <- .scintg_cluster_batch_counts(R, batches)
  O <- c$O
  E <- c$E
  mask <- (O > 0) & (E > 0)
  kl <- sum(O[mask] * log(O[mask] / E[mask]))
  list(total=fit + sigma * ent + sigma * theta * kl,
       fit=fit, entropy=ent, kl=kl)
}

.scintg_kmeans_init <- function(Zn, K, seed) {
  N <- nrow(Zn)
  d <- ncol(Zn)
  st <- bitwAnd(as.integer(seed), 0x7FFFFFFF)
  if (st == 0) st <- 1L

  rnd <- function() {
    st <<- .ghc_lcg31(st)
    st / 2147483648.0
  }

  centres <- matrix(0, nrow=K, ncol=d)
  centres[1, ] <- Zn[as.integer(rnd() * N) + 1L, ]

  if (K > 1) {
    for (kk in 2:K) {
      cos_sim <- Zn %*% t(centres[seq_len(kk - 1), , drop=FALSE])
      dists <- 1 - cos_sim
      best <- dists[, 1]
      if (ncol(dists) > 1) {
        for (j in 2:ncol(dists)) {
          best <- pmin(best, dists[, j])
        }
      }
      d2 <- pmax(best, 0)^2
      tot <- sum(d2)
      if (tot <= 0) {
        centres[kk, ] <- Zn[as.integer(rnd() * N) + 1L, ]
      } else {
        t <- rnd() * tot
        acc <- 0
        chosen <- N
        for (i in seq_len(N)) {
          acc <- acc + d2[i]
          if (acc >= t) {
            chosen <- i
            break
          }
        }
        centres[kk, ] <- Zn[chosen, ]
      }
    }
  }

  for (iter in seq_len(10)) {
    cos_sim <- Zn %*% t(centres)
    groups <- max.col(cos_sim)
    for (k in seq_len(K)) {
      members <- which(groups == k)
      if (length(members) > 0) {
        centres[k, ] <- colMeans(Zn[members, , drop=FALSE])
      }
    }
    centres <- .scintg_l2_normalise(centres)
  }
  centres
}

.scintg_maximum_diversity_clustering <- function(Z, batches, K=NULL, sigma=0.1, theta=2.0,
                                                max_iter=25, tol=1e-5, seed=0, Y=NULL,
                                                diversity="penalise") {
  if (!(diversity %in% .scintg_signs)) {
    stop(sprintf("scintg: diversity must be one of %s",
                 paste(.scintg_signs, collapse=", ")))
  }
  rows <- .scintg_matrix(Z)
  N <- nrow(rows)
  d <- ncol(rows)
  if (length(batches) != N) stop("scintg: one batch label per cell is required")
  if (sigma <= 0) stop("scintg: sigma must be positive")
  if (theta < 0) stop("scintg: theta must be non-negative")
  if (max_iter < 1) stop("scintg: max_iter must be at least 1")
  if (is.null(K)) {
    K <- max(2, min(100, N %/% 30))
  }
  K <- as.integer(K)
  if (K < 1 || K > N) stop("scintg: K must be between 1 and the cell count")

  Zn <- .scintg_l2_normalise(rows)

  if (!is.null(Y)) {
    centres <- .scintg_l2_normalise(Y)
    if (nrow(centres) != K) stop("scintg: Y must have one row per cluster")
  } else {
    centres <- .scintg_kmeans_init(Zn, K, seed)
  }

  names <- sort(unique(as.character(batches)))
  batches_chr <- as.character(batches)
  bidx <- match(batches_chr, names)

  R <- matrix(1.0 / K, nrow=K, ncol=N)
  prev <- NULL
  for (iter in seq_len(as.integer(max_iter))) {
    counts <- .scintg_cluster_batch_counts(R, batches_chr, names)
    O <- counts$O
    E <- counts$E
    newR <- matrix(0, nrow=K, ncol=N)
    for (i in seq_len(N)) {
      bi <- bidx[i]
      col <- numeric(K)
      for (k in seq_len(K)) {
        dist <- 1 - sum(centres[k, ] * Zn[i, ])
        val <- -2.0 * dist / sigma
        if (theta > 0) {
          o <- O[k, bi]
          e <- E[k, bi]
          ratio <- if (o > 0 && e > 0) o / e else 1e-12
          sign <- if (diversity == "as_printed") 1.0 else -1.0
          val <- val + sign * theta * log(ratio)
        }
        col[k] <- val
      }
      m <- max(col)
      ex <- exp(col - m)
      s <- sum(ex)
      if (s > 0) {
        newR[, i] <- ex / s
      } else {
        newR[, i] <- rep(1.0 / K, K)
      }
    }
    R <- newR
    centres <- .scintg_l2_normalise(R %*% Zn)
    obj <- .scintg_harmony_objective(Zn, R, centres, batches_chr, sigma, theta)
    if (!is.null(prev) && abs(prev - obj$total) <= tol * max(abs(prev), 1e-12)) {
      break
    }
    prev <- obj$total
  }
  list(R=R, Y=centres, K=K,
       objective=.scintg_harmony_objective(Zn, R, centres, batches_chr, sigma, theta))
}

.scintg_correct_batch <- function(Z, R, batches, lam=1.0, reference=NULL) {
  rows <- .scintg_matrix(Z)
  N <- nrow(rows)
  d <- ncol(rows)
  K <- nrow(R)
  if (K == 0 || ncol(R) != N) stop("scintg: R must be K by N")
  if (length(batches) != N) stop("scintg: one batch label per cell is required")
  if (lam < 0) stop("scintg: lambda must be non-negative")

  des <- .scintg_design(batches)
  phi <- des$phi
  names <- des$names
  B <- length(names)

  if (!is.null(reference)) {
    if (length(reference) != N) stop("scintg: one reference flag per cell")
    ref <- as.logical(reference)
    ref[is.na(ref)] <- FALSE
    for (i in seq_len(N)) {
      if (ref[i]) {
        phi[i, ] <- c(1, rep(0, B))
      }
    }
  }

  out <- rows
  Ws <- list()

  for (k in seq_len(K)) {
    Rk <- R[k, ]
    weighted_phi <- phi * Rk
    A <- crossprod(phi, weighted_phi)
    if (B >= 1) {
      diag_A <- diag(A)
      diag_A[2:(B + 1)] <- diag_A[2:(B + 1)] + lam
      diag(A) <- diag_A
    }
    rhs <- crossprod(phi, rows * Rk)
    W <- solve(A, rhs)
    W[1, ] <- 0
    Ws[[k]] <- W
    correction <- phi %*% W
    out <- out - Rk * correction
  }

  list(Z=out, W=Ws, batches=names)
}

morie_scintg <- function(Z, batches, K=NULL, sigma=0.1, theta=2.0, lam=1.0,
                        max_iter=10, cluster_iter=25, tol=1e-4, seed=0,
                        reference=NULL, diversity="penalise") {
  rows <- .scintg_matrix(Z)
  N <- nrow(rows)
  d <- ncol(rows)
  batches_chr <- as.character(batches)
  if (length(batches_chr) != N) stop("scintg: one batch label per cell is required")
  if (length(unique(batches_chr)) < 2) stop("scintg: at least two batches are needed")
  if (max_iter < 1) stop("scintg: max_iter must be at least 1")

  cur <- rows
  Y <- NULL
  hist <- numeric(0)
  for (iter in seq_len(as.integer(max_iter))) {
    cl <- .scintg_maximum_diversity_clustering(cur, batches_chr, K, sigma, theta,
                                              cluster_iter, seed=seed, Y=Y,
                                              diversity=diversity)
    Y <- cl$Y
    got <- .scintg_correct_batch(cur, cl$R, batches_chr, lam, reference)
    shift <- max(abs(got$Z - cur))
    cur <- got$Z
    hist <- c(hist, cl$objective$total)
    if (shift <= tol) break
  }
  final <- .scintg_maximum_diversity_clustering(cur, batches_chr, K, sigma, theta,
                                               cluster_iter, seed=seed, Y=Y,
                                               diversity=diversity)
  list(
    estimate = cur,
    embedding = cur,
    R = final$R,
    Y = final$Y,
    K = final$K,
    objective = final$objective,
    history = hist,
    n_rounds = length(hist),
    theta = as.numeric(theta),
    sigma = as.numeric(sigma),
    lam = as.numeric(lam),
    diversity = diversity,
    method = ("Harmony (Korsunsky et al. 2019): maximum diversity "
              "clustering (eq. 8) alternated with mixture-of-experts "
              "ridge correction (eq. 14)"),
    note = ("theta=0 reduces the cluster step to ordinary soft "
            "spherical k-means; the intercept row of W_k is zeroed "
            "so batch-independent variation is kept, which is why a "
            "reference cell (design row [1, 0, ...]) never moves. "
            "Equation 8 is printed with (O/E)^+theta, which raises "
            "cluster/batch dependence rather than lowering it; "
            "diversity='penalise' uses the -theta the stated "
            "objective implies, 'as_printed' the literal form")
  )
}

morie_harmony_integrate <- morie_scintg
morie_singlecell_integration <- morie_scintg

.scintg_cheatsheet <- function() {
  ("scintg: Harmony (Korsunsky et al. 2019). Alternates maximum "
   "diversity clustering -- soft spherical k-means whose "
   "assignment R_ki is proportional to (O_ki/E_ki)^theta "
   "exp(-2(1 - Y_k'Z_i)/sigma), with O the observed and E the "
   "independence-expected cluster/batch mass -- with a "
   "mixture-of-experts ridge correction W_k = (phi* diag(R_k) "
   "phi*' + lambda I)^-1 phi* diag(R_k) Z' whose intercept row "
   "is zeroed, so batch goes and cell type stays.")
}
