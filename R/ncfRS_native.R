# morie.fn -- function file (rootcoder007/morie)
# Neural collaborative filtering: the inner product is a choice.
#
# Matrix factorisation scores a pair by :math:`p_u^\top q_i`. NCF's
# argument is that this fixed, un-weighted combination of latent
# dimensions is a *modelling assumption* rather than a necessity, and it
# limits what user-item similarity structure the model can express.
#
# **GMF: matrix factorisation is a special case, and recovering it
# matters.** Take the element-wise product and pass it through a learned
# output layer,
#
# .. math:: \hat y_{ui} = a_{out}\big(h^\top (p_u \odot q_i)\big).
#
# With :math:`a_{out}` the identity and :math:`h` the all-ones vector,
# this *is* matrix factorisation. Letting :math:`h` be learned makes the
# dimensions differently weighted; making :math:`a_{out}` a sigmoid
# makes it non-linear. The generalisation is exact, and the anchor
# checks the recovery numerically rather than asserting it.
#
# **MLP: concatenation plus depth.** Concatenating :math:`p_u` and
# :math:`q_i` and stacking hidden layers lets the model learn the
# interaction instead of fixing it -- but concatenation alone accounts
# for no interaction at all, which is precisely why the hidden layers
# are needed.
#
# **NeuMF: separate embeddings, fused late.** The two pathways are given
# *different* embeddings and combined only in the last layer,
#
# .. math:: \hat y_{ui} = \sigma\big(h^\top [\,\phi^{GMF};
#           \phi^{MLP}\,]\big),
#
# because forcing them to share one embedding would constrain both to
# the same dimension and tie the two models' capacities together.
#
# **The loss follows from the data being implicit.** Interactions are
# binary -- observed or not -- so the model is trained with the log
# loss under a Bernoulli likelihood, with negatives sampled from the
# unobserved entries.
#
# References
# ----------
# He, X., Liao, L., Zhang, H., Nie, L., Hu, X. & Chua, T.-S. (2017)
# "Neural Collaborative Filtering", *Proceedings of the 26th
# International Conference on World Wide Web (WWW '17)*, 173-182,
# doi:10.1145/3038912.3052569. Sec. 3.1 (the general NCF framework and
# the log loss with sampled negatives for implicit data), Sec. 3.2
# (GMF: eq. (9), and the demonstration that MF is recovered when a_out
# is the identity and h is uniform), Sec. 3.3 (MLP over the
# concatenation), and Sec. 3.4 (NeuMF: separate GMF and MLP embeddings
# fused in the last layer, and why sharing one embedding would limit
# the fused model).
#
# Koren, Y., Bell, R. & Volinsky, C. (2009) "Matrix Factorization
# Techniques for Recommender Systems", *Computer* 42(8), 30-37,
# doi:10.1109/MC.2009.263. The inner-product model being generalised.

.ncfRS_EPS <- 1e-12

#' .ncfRS_sig
#'
#' A step of the ncfRS_native implementation. Called by \code{morie_ncfRS_fit_gmf},
#' \code{morie_ncfRS_gmf}, \code{morie_ncfRS_neumf}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @return One of two values, depending on the branch taken.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .ncfRS_sig(x = x)
#' res
.ncfRS_sig <- function(x) {
  # vectorised clamp: the scalar if() errors on any vector input
  xc <- pmax(x, -700)
  1.0 / (1.0 + exp(-xc))
}

#' morie_ncfRS_gmf
#'
#' A step of the ncfRS_native implementation. Called by \code{morie_ncfRS_fit_gmf}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p_u Coerced to numeric by the body, with \code{as.numeric}.
#' @param q_i Coerced to numeric by the body, with \code{as.numeric}.
#' @param h Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param activation Passed to \code{identical}. Defaults to \code{"sigmoid"}.
#' @return Nothing; this branch always raises.
#' @export
morie_ncfRS_gmf <- function(p_u, q_i, h = NULL, activation = "sigmoid") {
  p <- as.numeric(p_u)
  q <- as.numeric(q_i)
  if (length(p) != length(q)) {
    stop(sprintf("ncfRS: embeddings differ in length (%d, %d)", length(p), length(q)))
  }
  if (is.null(h)) {
    hh <- rep(1.0, length(p))
  } else {
    hh <- as.numeric(h)
  }
  z <- sum(hh * p * q)
  if (identical(activation, "identity")) {
    return(z)
  }
  if (identical(activation, "sigmoid")) {
    return(.ncfRS_sig(z))
  }
  stop(sprintf("ncfRS: activation must be identity or sigmoid, got %s", activation))
}

#' morie_ncfRS_mlp_layers
#'
#' A step of the ncfRS_native implementation. Called by \code{morie_ncfRS_neumf}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p_u Coerced to numeric by the body, with \code{as.numeric}.
#' @param q_i Coerced to numeric by the body, with \code{as.numeric}.
#' @param Ws A vector; its length is taken and its elements indexed.
#' @param bs A vector; indexed elementwise.
#' @return The value of \code{z}, as built in the body.
#' @export
morie_ncfRS_mlp_layers <- function(p_u, q_i, Ws, bs) {
  z <- c(as.numeric(p_u), as.numeric(q_i))
  for (l in seq_along(Ws)) {
    W <- Ws[[l]]
    b <- bs[[l]]
    z <- vapply(seq_along(b), function(o) {
      max(0.0, b[o] + sum(W[o, ] * z))
    }, numeric(1))
  }
  z
}

#' morie_ncfRS_neumf
#'
#' A step of the ncfRS_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p_gmf Coerced to numeric by the body, with \code{as.numeric}.
#' @param q_gmf Coerced to numeric by the body, with \code{as.numeric}.
#' @param p_mlp Passed to \code{morie_ncfRS_mlp_layers}.
#' @param q_mlp Passed to \code{morie_ncfRS_mlp_layers}.
#' @param Ws Passed to \code{morie_ncfRS_mlp_layers}.
#' @param bs Passed to \code{morie_ncfRS_mlp_layers}.
#' @param h Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{score}, \code{gmf_part}, \code{mlp_part}, \code{note}.
#' @export
morie_ncfRS_neumf <- function(p_gmf, q_gmf, p_mlp, q_mlp, Ws, bs, h) {
  g <- as.numeric(p_gmf) * as.numeric(q_gmf)
  m <- morie_ncfRS_mlp_layers(p_mlp, q_mlp, Ws, bs)
  cat <- c(g, m)
  hh <- as.numeric(h)
  if (length(hh) != length(cat)) {
    stop(sprintf("ncfRS: h has %d entries for a fused vector of %d", length(hh), length(cat)))
  }
  list(
    score = .ncfRS_sig(sum(hh * cat)),
    gmf_part = g,
    mlp_part = m,
    note = "separate embeddings per pathway -- sharing one would tie both models to the same dimension"
  )
}

#' morie_ncfRS_log_loss
#'
#' A step of the ncfRS_native implementation. Called by \code{morie_ncfRS_fit_gmf}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @param y_hat Coerced to numeric by the body, with \code{as.numeric}.
#' @return A numeric value.
#' @export
morie_ncfRS_log_loss <- function(y, y_hat) {
  p <- min(max(as.numeric(y_hat), .ncfRS_EPS), 1.0 - .ncfRS_EPS)
  yv <- as.numeric(y)
  -(yv * log(p) + (1.0 - yv) * log(1.0 - p))
}

#' morie_ncfRS_fit_gmf
#'
#' A step of the ncfRS_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param pos A vector; indexed elementwise.
#' @param n_users Coerced to integer by the body, with \code{as.integer}.
#' @param n_items Coerced to integer by the body, with \code{as.integer}.
#' @param k_dim Coerced to integer by the body, with \code{as.integer}. Defaults to \code{8}.
#' @param alpha Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.05}.
#' @param iters Coerced to integer by the body, with \code{as.integer}. Defaults to \code{2000}.
#' @param n_neg Coerced to integer by the body, with \code{as.integer}. Defaults to \code{4}.
#' @param seed Passed to \code{.ghc_rng}. Defaults to \code{0}.
#' @param learn_h A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return A list with \code{estimate}, \code{P}, \code{Q}, \code{h},
#' \code{loss_history}, \code{final_loss}, \code{k}, \code{learned_h}, \code{method}.
#' @export
morie_ncfRS_fit_gmf <- function(pos, n_users, n_items, k_dim = 8, alpha = 0.05,
                                iters = 2000, n_neg = 4, seed = 0, learn_h = TRUE) {
  U <- as.integer(n_users)
  I <- as.integer(n_items)
  K <- as.integer(k_dim)
  if (U < 1 || I < 2 || K < 1) {
    stop("ncfRS: need at least 1 user, 2 items, 1 factor")
  }

  if (is.null(names(pos))) {
    stop("ncfRS: pos must be a named list (user ID -> item IDs)")
  }
  user_ids <- as.integer(names(pos))
  users <- sort(user_ids)

  if (length(users) == 0) {
    stop("ncfRS: no observed interactions")
  }

  e <- .ghc_rng(seed)

  P <- matrix(0, nrow = U, ncol = K)
  for (u in seq_len(U)) {
    P[u, ] <- (.ghc_unif(e, K) - 0.5) * 0.2
  }

  Q <- matrix(0, nrow = I, ncol = K)
  for (i in seq_len(I)) {
    Q[i, ] <- (.ghc_unif(e, K) - 0.5) * 0.2
  }

  h <- rep(1.0, K)
  a <- as.numeric(alpha)
  hist <- numeric(0)
  log_interval <- max(1, as.integer(iters) %/% 20)

  for (it in seq_len(as.integer(iters))) {
    n_u <- length(users)
    idx_0 <- as.integer(.ghc_unif(e, 1) * n_u) %% n_u
    u_id <- users[idx_0 + 1]

    u_pos_idx <- match(u_id, user_ids)
    seen <- as.integer(pos[[u_pos_idx]])

    pool_items <- seen
    pool_labels <- rep(1.0, length(seen))

    for (nn in seq_len(as.integer(n_neg))) {
      j_0 <- as.integer(.ghc_unif(e, 1) * I) %% I
      j <- j_0 + 1
      if (!(j %in% seen)) {
        pool_items <- c(pool_items, j)
        pool_labels <- c(pool_labels, 0.0)
      }
    }

    for (k in seq_along(pool_items)) {
      i_id <- pool_items[k]
      y <- pool_labels[k]

      z <- sum(h * P[u_id, ] * Q[i_id, ])
      e_sig <- .ncfRS_sig(z) - y

      P[u_id, ] <- P[u_id, ] - a * e_sig * h * Q[i_id, ]
      Q[i_id, ] <- Q[i_id, ] - a * e_sig * h * P[u_id, ]
      if (isTRUE(learn_h)) {
        h <- h - a * e_sig * P[u_id, ] * Q[i_id, ]
      }
    }

    if (it %% log_interval == 0) {
      L <- 0.0
      n <- 0
      for (uu in users) {
        uu_pos_idx <- match(uu, user_ids)
        uu_seen <- as.integer(pos[[uu_pos_idx]])
        for (i in seq_len(I)) {
          y <- if (i %in% uu_seen) 1.0 else 0.0
          L <- L + morie_ncfRS_log_loss(y, morie_ncfRS_gmf(P[uu, ], Q[i, ], h))
          n <- n + 1
        }
      }
      hist <- c(hist, L / n)
    }
  }

  list(
    estimate = list(P, Q, h),
    P = P,
    Q = Q,
    h = h,
    loss_history = hist,
    final_loss = if (length(hist) > 0) hist[length(hist)] else NaN,
    k = K,
    learned_h = isTRUE(learn_h),
    method = "GMF by SGD with sampled negatives; He et al. (2017) eq. (9)"
  )
}

#' morie_ncfRS_cheatsheet
#'
#' A step of the ncfRS_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
morie_ncfRS_cheatsheet <- function() {
  paste0("ncfRS: the inner product is an ASSUMPTION, not a ",
         "necessity. GMF = a_out(h' (p_u * q_i)) elementwise, which ",
         "IS matrix factorisation when a_out is the identity and h ",
         "is all ones -- learning h weights the dimensions, a ",
         "sigmoid makes it non-linear. MLP concatenates, and ",
         "concatenation alone models NO interaction, which is why ",
         "the depth is required. NeuMF gives each pathway its OWN ",
         "embedding and fuses only at the last layer. Implicit ",
         "data, so log loss with sampled negatives.")
}

# compact alias per ledger/NAMING.md
morie_ncfRS_neuralcollaborativefiltering <- morie_ncfRS_fit_gmf

# public names resolved by fn/_lazy_map.json
morie_ncfRS_ncf <- morie_ncfRS_fit_gmf

# main entry point
morie_ncfRS <- morie_ncfRS_fit_gmf
