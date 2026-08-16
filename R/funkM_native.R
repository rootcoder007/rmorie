# morie.fn -- function file (rootcoder007/morie)
# Native R translation.
#
# Funk SVD: factorise only the ratings that exist.
#
# The name is misleading and the misunderstanding matters. A true SVD
# requires a **complete** matrix, and a ratings matrix is almost
# entirely missing -- 99% or more. Filling the holes with zeros or with
# column means and running an SVD does not give a better estimate of the
# missing entries; it fits the imputation.
#
# **So do not complete the matrix: fit only the observed cells.**
# Minimise
#
# .. math:: \sum_{(u,i)\in\mathcal{K}} (r_{ui} - \mu - b_u - b_i
#           - q_i^\top p_u)^2
#           + \lambda(\|p_u\|^2 + \|q_i\|^2 + b_u^2 + b_i^2)
#
# by stochastic gradient descent over the observed set
# :math:`\mathcal{K}`. The regulariser is not optional decoration: with
# one latent vector per user and per item, an unregularised fit
# reproduces the observed ratings and says nothing about the rest.
#
# **Baselines before factors.** :math:`\mu + b_u + b_i` absorbs the fact
# that some users rate high and some items are widely liked. Those
# effects are large, and if the factors have to represent them they have
# less capacity for the interaction that is actually being modelled --
# which is what the factors are for.
#
# **Funk's own procedure trained one factor at a time**, fitting feature
# :math:`f` to convergence against the residual left by features
# :math:`0\ldots f-1`, rather than all :math:`k` jointly. That is a
# greedy, deflation-style fit, and it is the thing that makes "Funk SVD"
# a distinct recipe rather than a synonym for regularised MF, so
# ``fit`` exposes ``incremental`` and the anchor compares the two.
#
# References
# ----------
# Funk, S. (2006) "Netflix Update: Try This at Home",
# https://sifter.org/~simon/journal/20061211.html. The original
# description of the incremental, regularised gradient-descent
# factorisation of the observed entries during the Netflix Prize.
# NOTE: this is a blog post, not a peer-reviewed paper -- it is the
# origin of the method and is cited as such; the published statements
# of the same model are the two references below, and the
# implementation follows those.
#
# Koren, Y. (2008) "Factorization Meets the Neighborhood: a
# Multifaceted Collaborative Filtering Model", *KDD '08*, 426-434,
# doi:10.1145/1401890.1401944. The baseline decomposition
# b_ui = mu + b_u + b_i and the regularised squared-error objective
# over the OBSERVED ratings, optimised by stochastic gradient descent.
#
# Koren, Y., Bell, R. & Volinsky, C. (2009) "Matrix Factorization
# Techniques for Recommender Systems", *Computer* 42(8), 30-37,
# doi:10.1109/MC.2009.263. The statement that earlier work relying on
# imputation to fill in the missing ratings is both expensive and prone
# to distortion, and that modelling only the observed entries with
# regularisation is preferable.
#
# Salakhutdinov, R. & Mnih, A. (2008) "Probabilistic Matrix
# Factorization", *NIPS 2007*, 1257-1264. The probabilistic reading of
# the same objective.

#' morie_funkM
#'
#' A step of the funkM_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param ratings Passed to \code{.funkM_as_ratings}.
#' @param n_users Coerced to integer by the body, with \code{as.integer}.
#' @param n_items Coerced to integer by the body, with \code{as.integer}.
#' @param factors Coerced to integer by the body, with \code{as.integer}. Defaults to \code{8}.
#' @param epochs Coerced to integer by the body, with \code{as.integer}. Defaults to \code{60}.
#' @param lr Passed to \code{.funkM_sgd_epoch}. Defaults to \code{0.005}.
#' @param reg Passed to \code{.funkM_sgd_epoch}. Defaults to \code{0.02}.
#' @param seed Passed to \code{.ghc_rng}. Defaults to \code{0}.
#' @param incremental A flag; the body branches on it. Defaults to \code{FALSE}.
#' @param epochs_per_factor Coerced to integer by the body, with \code{as.integer}. Defaults to \code{20}.
#' @return The value of \code{result}, as built in the body.
#' @export
morie_funkM <- function(ratings, n_users, n_items, factors = 8,
                        epochs = 60, lr = 0.005, reg = 0.02, seed = 0,
                        incremental = FALSE, epochs_per_factor = 20) {
  R <- .funkM_as_ratings(ratings)
  if (nrow(R) == 0L) stop("funkM: no ratings given")
  nu <- as.integer(n_users)
  ni <- as.integer(n_items)
  d  <- as.integer(factors)
  if (min(nu, ni, d) < 1L) stop("funkM: the counts must be positive")
  if (reg < 0) stop("funkM: the regularisation cannot be negative")

  mu <- .funkM_global_mean(R)
  bu <- rep(0.0, nu)
  bi <- rep(0.0, ni)

  # Random init via the shared bit-identical glibc LCG helpers.
  e <- .ghc_rng(seed)
  P <- matrix(0.0, nrow = nu, ncol = d)
  Q <- matrix(0.0, nrow = ni, ncol = d)
  for (u_idx in seq_len(nu)) {
    for (a in seq_len(d)) {
      u_val <- .ghc_unif(e, 1L)
      P[u_idx, a] <- 0.1 * (u_val - 0.5)
    }
  }
  for (i_idx in seq_len(ni)) {
    for (a in seq_len(d)) {
      u_val <- .ghc_unif(e, 1L)
      Q[i_idx, a] <- 0.1 * (u_val - 0.5)
    }
  }

  hist <- numeric(0)
  if (isTRUE(incremental)) {
    for (f in seq_len(d) - 1L) {
      for (ep in seq_len(as.integer(epochs_per_factor))) {
        hist <- c(hist, .funkM_sgd_epoch(R, mu, bu, bi, P, Q, lr, reg,
                                         factor = f))
      }
    }
  } else {
    for (ep in seq_len(as.integer(epochs))) {
      hist <- c(hist, .funkM_sgd_epoch(R, mu, bu, bi, P, Q, lr, reg))
    }
  }

  result <- list(
    estimate    = hist[length(hist)],
    rmse        = hist[length(hist)],
    rmse_history = hist,
    mu          = mu,
    b_user      = bu,
    b_item      = bi,
    P           = P,
    Q           = Q,
    factors     = d,
    incremental = isTRUE(incremental),
    observed    = nrow(R),
    density     = as.numeric(nrow(R)) / as.numeric(nu * ni),
    method      = paste("regularised MF on the observed entries; Funk",
                        "(2006), published form in Koren (2008)"),
    note        = paste("the missing entries are never imputed -- only",
                        "the observed set is summed over")
  )
  class(result) <- c("RichResult", "list")
  result
}

# Coerce the ratings argument to a data.frame with columns u, i, r.
# Accepts a data.frame (with those columns) or a list of length-3
# vectors. Indices are kept 0-based, matching the Python source.
#' Coerce the ratings argument to a data.frame with columns u, i, r
#'
#' Accepts a data.frame (with those columns) or a list of length-3
#' vectors. Indices are kept 0-based, matching the Python source.
#'
#' @param ratings A list; the body reads \code{$i}, \code{$r}, \code{$u} from it.
#' @return Nothing; this branch always raises.
#' @export
.funkM_as_ratings <- function(ratings) {
  if (is.data.frame(ratings)) {
    if (!all(c("u", "i", "r") %in% names(ratings))) {
      stop("funkM: data.frame ratings must have columns u, i, r")
    }
    return(data.frame(
      u = as.integer(ratings$u),
      i = as.integer(ratings$i),
      r = as.numeric(ratings$r)
    ))
  }
  if (is.list(ratings)) {
    n <- length(ratings)
    if (n == 0L) {
      return(data.frame(u = integer(0), i = integer(0), r = numeric(0)))
    }
    u  <- integer(n)
    iv <- integer(n)
    rv <- numeric(n)
    for (k in seq_along(ratings)) {
      x <- ratings[[k]]
      if (length(x) < 3L) stop("funkM: each rating needs (u, i, r)")
      u[k]  <- as.integer(x[1])
      iv[k] <- as.integer(x[2])
      rv[k] <- as.numeric(x[3])
    }
    return(data.frame(u = u, i = iv, r = rv))
  }
  stop("funkM: ratings must be a data.frame or list of length-3 vectors")
}

# mu over the OBSERVED entries only.
#' Mu over the OBSERVED entries only
#'
#' A step of the funkM_native implementation. Called by \code{morie_funkM}, \code{morie_funkM_imputed_svd_error}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param R A list; the body reads \code{$r} from it.
#' @return A numeric value.
#' @export
.funkM_global_mean <- function(R) {
  if (nrow(R) == 0L) stop("funkM: no ratings given")
  sum(R$r) / as.numeric(nrow(R))
}

# r_hat = mu + b_u + b_i + q_i^T p_u.
#' R_hat = mu + b_u + b_i + q_i^T p_u
#'
#' A step of the funkM_native implementation. Called by \code{.funkM_sgd_epoch}, \code{morie_funkM_rmse}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param mu Coerced to numeric by the body, with \code{as.numeric}.
#' @param b_user Coerced to numeric by the body, with \code{as.numeric}.
#' @param b_item Coerced to numeric by the body, with \code{as.numeric}.
#' @param p_u A vector; its length is taken.
#' @param q_i A vector; its length is taken.
#' @return A numeric value.
#' @export
.funkM_predict <- function(mu, b_user, b_item, p_u, q_i) {
  if (length(p_u) != length(q_i)) {
    stop(sprintf("funkM: the factors differ in width (%d, %d)",
                 length(p_u), length(q_i)))
  }
  as.numeric(mu) + as.numeric(b_user) + as.numeric(b_item) +
    sum(p_u * q_i)
}

# One pass over the observed ratings. `factor` is 0-based; NULL = all.
#' One pass over the observed ratings. `factor` is 0-based; NULL = all
#'
#' A step of the funkM_native implementation. Called by \code{morie_funkM}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param R A list; the body reads \code{$i}, \code{$r}, \code{$u} from it.
#' @param mu Passed to \code{.funkM_predict}.
#' @param bu A vector; indexed elementwise.
#' @param bi A vector; indexed elementwise.
#' @param P A matrix; indexed by row and column.
#' @param Q A matrix; indexed by row and column.
#' @param lr Numeric; combined arithmetically in the body.
#' @param reg Numeric; combined arithmetically in the body.
#' @param factor Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @return A numeric value.
#' @export
.funkM_sgd_epoch <- function(R, mu, bu, bi, P, Q, lr, reg, factor = NULL) {
  se <- 0.0
  n  <- nrow(R)
  for (k in seq_len(n)) {
    u  <- R$u[k]
    i  <- R$i[k]
    r  <- R$r[k]
    u1 <- u + 1L
    i1 <- i + 1L
    err <- r - .funkM_predict(mu, bu[u1], bi[i1],
                              P[u1, , drop = FALSE],
                              Q[i1, , drop = FALSE])
    se <- se + err * err
    bu[u1] <- bu[u1] + lr * (err - reg * bu[u1])
    bi[i1] <- bi[i1] + lr * (err - reg * bi[i1])
    a_range <- if (is.null(factor)) {
      seq_len(ncol(P))
    } else {
      as.integer(factor) + 1L
    }
    for (a in a_range) {
      pu <- P[u1, a]
      qi <- Q[i1, a]
      P[u1, a] <- pu + lr * (err * qi - reg * pu)
      Q[i1, a] <- qi + lr * (err * pu - reg * qi)
    }
  }
  sqrt(se / as.numeric(n))
}

# Root mean squared error on a held-out set.
#' Root mean squared error on a held-out set
#'
#' A step of the funkM_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param ratings Passed to \code{.funkM_as_ratings}.
#' @param mu Passed to \code{.funkM_predict}.
#' @param bu A vector; indexed elementwise.
#' @param bi A vector; indexed elementwise.
#' @param P A matrix; indexed by row and column.
#' @param Q A matrix; indexed by row and column.
#' @return A numeric value.
#' @export
morie_funkM_rmse <- function(ratings, mu, bu, bi, P, Q) {
  R <- .funkM_as_ratings(ratings)
  if (nrow(R) == 0L) stop("funkM: no ratings to score")
  se <- 0.0
  for (k in seq_len(nrow(R))) {
    u  <- R$u[k]
    i  <- R$i[k]
    r  <- R$r[k]
    u1 <- u + 1L
    i1 <- i + 1L
    pred <- .funkM_predict(mu, bu[u1], bi[i1],
                           P[u1, , drop = FALSE],
                           Q[i1, , drop = FALSE])
    se <- se + (r - pred)^2
  }
  sqrt(se / as.numeric(nrow(R)))
}

# What filling the holes and running an SVD actually gives.
#' What filling the holes and running an SVD actually gives
#'
#' A step of the funkM_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param ratings Passed to \code{.funkM_as_ratings}.
#' @param n_users Coerced to integer by the body, with \code{as.integer}.
#' @param n_items Coerced to integer by the body, with \code{as.integer}.
#' @param rank Coerced to integer by the body, with \code{as.integer}. Defaults to \code{2}.
#' @param fill Carried through into a list the body builds. Defaults to \code{"zero"}.
#' @return A list with \code{rmse_on_observed}, \code{fill}, \code{rank}, \code{note}.
#' @export
morie_funkM_imputed_svd_error <- function(ratings, n_users, n_items,
                                          rank = 2, fill = "zero") {
  R  <- .funkM_as_ratings(ratings)
  nu <- as.integer(n_users)
  ni <- as.integer(n_items)
  kk <- as.integer(rank)
  base_val <- if (identical(fill, "zero")) {
    0.0
  } else if (identical(fill, "mean")) {
    .funkM_global_mean(R)
  } else {
    stop(sprintf("funkM: fill must be zero or mean, got %s", fill))
  }
  M <- matrix(base_val, nrow = nu, ncol = ni)
  for (k in seq_len(nrow(R))) {
    M[R$u[k] + 1L, R$i[k] + 1L] <- R$r[k]
  }
  sv <- svd(M)
  U <- sv$u
  S <- sv$d
  V <- sv$v
  kk_actual <- min(kk, length(S))
  if (kk_actual > 0L) {
    approx <- (U[, seq_len(kk_actual), drop = FALSE] *
               S[seq_len(kk_actual)]) %*%
              t(V[, seq_len(kk_actual), drop = FALSE])
  } else {
    approx <- matrix(0.0, nrow = nu, ncol = ni)
  }
  err <- 0.0
  for (k in seq_len(nrow(R))) {
    err <- err + (R$r[k] - approx[R$u[k] + 1L, R$i[k] + 1L])^2
  }
  err <- sqrt(err / as.numeric(nrow(R)))
  list(
    rmse_on_observed = err,
    fill             = fill,
    rank             = kk,
    note             = paste("the SVD spent its rank on the imputed",
                            "cells, which outnumber the real ones")
  )
}

#' .funkM_cheatsheet
#'
#' A step of the funkM_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.funkM_cheatsheet <- function() {
  paste("funkM: a true SVD needs a COMPLETE matrix and a ratings",
        "matrix is >99% missing -- filling the holes with zeros or",
        "means fits the IMPUTATION, not the data. So sum only over",
        "the OBSERVED entries: minimise (r - mu - b_u - b_i -",
        "q'p)^2 + lambda(||p||^2 + ||q||^2 + b_u^2 + b_i^2) by",
        "SGD. Regularisation is load-bearing -- one vector per user",
        "and item would otherwise just memorise. Baselines",
        "mu + b_u + b_i first, or the factors waste capacity on",
        "effects that are not interactions. Funk's own schedule",
        "trained ONE FACTOR AT A TIME against the previous",
        "residual, which is what makes the recipe distinct.")
}

# compact alias per ledger/NAMING.md
funk_svd <- morie_funkM
# public names resolved by fn/_lazy_map.json
funksvd  <- morie_funkM
