# SPDX-License-Identifier: AGPL-3.0-or-later
#' Multi-output DNN for multi-trait genomic prediction
#'
#' SOURCE.  Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate
#' Statistical Machine Learning Methods for Genomic Prediction, Springer,
#' doi:10.1007/978-3-030-89010-0.
#'
#' The ARCHITECTURE is Chapter 12, Section 12.4.1, "DNN with Multivariate
#' Continuous Outcomes", volume [Pages 477-532], pp.490-493.  That section gives
#' the model as Keras code rather than as an equation: an input layer, a stack of
#' shared hidden dense layers each followed by dropout, then ONE one-unit linear
#' output head per trait, compiled with a per-head loss and a per-head
#' loss_weights entry.  That is exactly the model here -- shared hidden stack, T
#' linear heads, joint loss sum_t w_t L_t.
#'
#' The TRAINING EQUATIONS are Chapter 10, Section 10.8.1, volume
#' [Pages 379-425], pp.412-413, read from rendered page images because the text
#' layer of this chapter drops minus signs:
#'
#'   Step 4  z_ik^(h) = sum_{p=0}^{P} w_kp^(h) x_ip
#'   Step 5  V_ik^(h) = g^(h)(z_ik^(h))
#'   Step 6  z_ij^(l) = sum_{k=0}^{M} w_jk^(l) V_ik^(h)
#'   Step 7  yhat_ij  = g^(l)(z_ij^(l))
#'   Step 8  E(w) = (1/(2 n L)) sum_i sum_j (yhat_ij - y_ij)^2
#'   Step 9  delta_ij = (y_ij - yhat_ij) g^(l)-prime(z_ij^(l))
#'   Step 10 psi_ik   = g^(h)-prime(z_ik^(h)) sum_{j=1}^{L} delta_ij w_jk^(l)
#'   Step 11 w_jk^(l) <- w_jk^(l) + eta delta_ij V_ik^(h)
#'   Step 12 w_kp^(h) <- w_kp^(h) + eta psi_ik x_ip
#'
#' The sign convention is the one printed: delta carries (y - yhat), not
#' (yhat - y), and the updates ADD eta times the gradient term.  The two sign
#' flips cancel, so this is ordinary gradient descent on E, but it is implemented
#' in the printed form so that the hand computation of Section 10.8.2 reproduces
#' digit for digit.
#'
#' Step 10 also fixes the intercept handling that is easy to get wrong: the sum
#' runs over the output weights EXCLUDING the intercept column, which p.416
#' states explicitly -- "where w_1^(l) is w^(l) without the weight of the
#' intercept, that is, without the first element".
#'
#' MULTI-TRAIT LOSS.  Section 12.4.1 weights each head, so the joint loss is
#' E(w) = (1/(2 n T)) sum_i sum_t w_t (yhat_it - y_it)^2 and the head weight w_t
#' multiplies delta_it.  At T = 1 with w = 1 this collapses exactly onto the
#' Chapter 10 loss of Step 8, which is what the anchor exploits.
#'
#' DEFAULT HEAD WEIGHTS.  p.493 prints a recipe rather than a formula: "(1) first
#' we calculated the median of each trait, (2) then we calculated the 0.25 and
#' 0.75 quantiles for each trait, (3) then we calculated the maximum distance in
#' terms of absolute value between the median and both quantiles, (4) then we
#' used as the weight for the first trait (GY) its calculated distance, and (5)
#' then we used as weight for the second trait the value obtained by dividing the
#' distance of the first trait by the distance of the second trait".  Implemented
#' as printed: d_t = max(|median_t - q25_t|, |q75_t - median_t|), w_1 = d_1,
#' w_t = d_1 / d_t for t >= 2.
#'
#' Noted, not silently corrected: step (4) makes w_1 the raw distance while every
#' other weight is a ratio, so the overall scale of the loss depends on the units
#' of trait 1.  The book itself calls these steps "only suggestions that can work
#' for some data sets".  Pass heads explicitly to override.
#'
#' DEPARTURES FROM SECTION 12.4.1, stated rather than papered over.  DROPOUT IS
#' NOT IMPLEMENTED: dropout is stochastic by definition and this package requires
#' both language arms to land on the same numbers; the Chapter 12 grid search
#' itself includes dropout1 = 0 as one of its two settings, so the dropout-free
#' model is inside the book grid.  THE OPTIMISER IS PLAIN FULL-BATCH GRADIENT
#' DESCENT, the Chapter 10 Steps 11-12 update, not the Adam of the Chapter 12
#' code; Adam is never given as equations anywhere in the book, only as the Keras
#' call optimizer_adam(lr = ...), so implementing it would mean writing from
#' memory rather than from the source.  There is no validation split and no early
#' stopping on a held-out set; training stops on the Chapter 10 Step 14
#' criterion, E(w) <= tol.
#'
#' DETERMINISM.  Chapter 10 Step 1 says "initialize the weights to small random
#' values".  Weights are instead laid down by a linear congruential generator,
#' seeded by seed, walked in a fixed order over the layers.  An LCG is used
#' rather than the low-discrepancy van der Corput draws available in the s03
#' helpers on purpose: a van der Corput stream strided across several parameter
#' blocks makes those blocks correlated, which has already produced a silently
#' wrong module on this shelf, and a correlated weight initialisation breaks the
#' symmetry-breaking that a hidden layer needs.  Pass init to supply weights
#' directly.
#'
#' @param X n-by-p matrix of inputs (markers, or any predictors).
#' @param Y n-by-T matrix of trait values; T is the number of output heads.
#' @param layers widths of the shared hidden layers, e.g. c(33, 33, 33) for the
#'   three-hidden-layer stack of Section 12.4.1.
#' @param heads per-head loss weights w_t; NULL applies the p.493 recipe.
#' @param activation hidden activation, one of "relu", "sigmoid", "tanh",
#'   "linear"; Section 12.4.1 uses relu.
#' @param out_activation head activation; Section 12.4.1 uses linear for
#'   continuous traits.
#' @param eta learning rate of Steps 11-12.
#' @param epochs maximum number of full-batch epochs.
#' @param tol Step 14 stopping tolerance on E(w); zero means run all epochs.
#' @param seed LCG seed for the weight initialisation.
#' @param init explicit starting weights, a list with one matrix per layer
#'   including the output layer; layer k has (fan_in + 1) rows by fan_out
#'   columns with the intercept in row 1.
#' @return list: estimate, Y_hat, loss, head_weights, weights, epochs_run, n,
#'   method.
#' @keywords internal
#' @examples
#' X <- matrix(c(0.33, 0.95, 0.27, 1.3), 4, 1)
#' Y <- matrix(c(0.9, 0.6, 0.95, 0.7), 4, 1)
#' Dnnmt(X, Y, 1L, heads = 1, activation = "sigmoid", out_activation = "sigmoid",
#'       eta = 0.1, epochs = 1L,
#'       init = list(matrix(c(1.86, -3.3), 2, 1), matrix(c(-1.5, 4.4), 2, 1)))$loss
#' @export
Dnnmt <- function(X, Y, layers, heads = NULL, activation = "relu",
                  out_activation = "linear", eta = 0.1, epochs = 200, tol = 0,
                  seed = 1, init = NULL) {
  Xm <- .s03mat(X)
  n <- nrow(Xm)
  if (n == 0L) stop("dnn_multitrait: X is empty")
  p <- ncol(Xm)
  if (p == 0L) stop("dnn_multitrait: X has no columns")
  Yc <- .s03mat(Y)
  if (nrow(Yc) != n) {
    stop("dnn_multitrait: X and Y disagree on the number of observations")
  }
  nt <- ncol(Yc)
  if (nt == 0L) stop("dnn_multitrait: Y has no traits")
  hid <- as.integer(layers)
  if (any(hid < 1L)) {
    stop("dnn_multitrait: every hidden layer must have at least one unit")
  }
  eta <- as.numeric(eta)
  if (!(eta > 0)) stop("dnn_multitrait: eta must be positive")
  epochs <- as.integer(epochs)
  if (epochs < 1L) stop("dnn_multitrait: epochs must be at least 1")

  if (is.null(heads)) {
    wt <- .dnnheadweights(Yc, nt, n)
  } else {
    wt <- .s03vec(heads)
    if (length(wt) != nt) {
      stop("dnn_multitrait: heads must give one loss weight per column of Y")
    }
    if (any(wt < 0)) stop("dnn_multitrait: head loss weights must be non-negative")
  }

  dims <- c(p, hid, nt)
  nlay <- length(dims) - 1L
  if (!is.null(init)) {
    W <- lapply(init, .s03mat)
    if (length(W) != nlay) {
      stop("dnn_multitrait: init must give one weight matrix per layer")
    }
    for (k in seq_len(nlay)) {
      if (nrow(W[[k]]) != dims[k] + 1L || ncol(W[[k]]) != dims[k + 1L]) {
        stop(sprintf("dnn_multitrait: init layer %d has the wrong shape", k))
      }
    }
  } else {
    s <- as.numeric(as.integer(seed)) %% 2147483648
    W <- vector("list", nlay)
    for (k in seq_len(nlay)) {
      Mk <- matrix(0, dims[k] + 1L, dims[k + 1L])
      for (q in seq_len(dims[k] + 1L)) {
        for (j in seq_len(dims[k + 1L])) {
          s <- .dnnlcg(s)
          Mk[q, j] <- 0.2 * (s / 2147483648) - 0.1
        }
      }
      W[[k]] <- Mk
    }
  }

  acts <- c(rep(activation, length(hid)), out_activation)
  loss <- NaN
  Yhat <- matrix(0, n, nt)
  ran <- 0L
  for (ep in seq_len(epochs)) {
    Z <- vector("list", nlay)
    A <- vector("list", nlay)
    A[[1L]] <- cbind(rep(1, n), Xm)
    for (k in seq_len(nlay)) {
      zk <- matrix(0, n, dims[k + 1L])
      for (i in seq_len(n)) {
        for (j in seq_len(dims[k + 1L])) zk[i, j] <- sum(A[[k]][i, ] * W[[k]][, j])
      }
      gk <- matrix(0, n, dims[k + 1L])
      for (i in seq_len(n)) {
        for (j in seq_len(dims[k + 1L])) gk[i, j] <- .dnnact(acts[k], zk[i, j])
      }
      Z[[k]] <- zk
      if (k < nlay) A[[k + 1L]] <- cbind(rep(1, n), gk) else Yhat <- gk
    }
    acc <- 0
    for (i in seq_len(n)) {
      for (t in seq_len(nt)) acc <- acc + wt[t] * (Yhat[i, t] - Yc[i, t])^2
    }
    loss <- acc / (2 * n * nt)
    ran <- ep
    if (tol > 0 && loss <= tol) break
    Dk <- matrix(0, n, nt)
    for (i in seq_len(n)) {
      for (t in seq_len(nt)) {
        Dk[i, t] <- wt[t] * (Yc[i, t] - Yhat[i, t]) *
          .dnndact(acts[nlay], Z[[nlay]][i, t], Yhat[i, t])
      }
    }
    newW <- vector("list", nlay)
    for (k in seq(nlay, 1L)) {
      nw <- matrix(0, dims[k] + 1L, dims[k + 1L])
      for (q in seq_len(dims[k] + 1L)) {
        for (j in seq_len(dims[k + 1L])) {
          nw[q, j] <- W[[k]][q, j] + eta * sum(A[[k]][, q] * Dk[, j])
        }
      }
      newW[[k]] <- nw
      if (k > 1L) {
        nd <- matrix(0, n, dims[k])
        for (i in seq_len(n)) {
          for (q in seq_len(dims[k])) {
            nd[i, q] <- .dnndact(acts[k - 1L], Z[[k - 1L]][i, q], A[[k]][i, q + 1L]) *
              sum(Dk[i, ] * W[[k]][q + 1L, ])
          }
        }
        Dk <- nd
      }
    }
    W <- newW
  }

  list(estimate = loss, Y_hat = Yhat, loss = loss, head_weights = wt,
       weights = W, epochs_run = ran, n = n,
       method = paste0("shared hidden stack with one linear head per trait ",
                       "(Ch 12 Sec 12.4.1, pp.490-493), trained by the Ch 10 ",
                       "Sec 10.8.1 backpropagation Steps 4-12 (pp.412-413); ",
                       "joint loss sum_t w_t L_t; Montesinos Lopez et al. (2022)"))
}

#' .dnnact
#'
#' A step of the dnnmt implementation. Called by \code{Dnnmt}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param name See Usage.
#' @param z Passed to \code{.s03sigmoid}.
#' @return Nothing; this branch always raises.
#' @export
.dnnact <- function(name, z) {
  if (identical(name, "linear")) return(z)
  if (identical(name, "relu")) return(if (z > 0) z else 0)
  if (identical(name, "sigmoid")) return(.s03sigmoid(z))
  if (identical(name, "tanh")) return(tanh(z))
  stop(sprintf("dnn_multitrait: unknown activation '%s'", name), call. = FALSE)
}

# Derivative of the activation at z, given its value g.
#' Derivative of the activation at z, given its value g
#'
#' A step of the dnnmt implementation. Called by \code{Dnnmt}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param name See Usage.
#' @param z See Usage.
#' @param g Numeric; combined arithmetically in the body.
#' @return Nothing; this branch always raises.
#' @export
.dnndact <- function(name, z, g) {
  if (identical(name, "linear")) return(1)
  if (identical(name, "relu")) return(if (z > 0) 1 else 0)
  if (identical(name, "sigmoid")) return(g * (1 - g))
  if (identical(name, "tanh")) return(1 - g * g)
  stop(sprintf("dnn_multitrait: unknown activation '%s'", name), call. = FALSE)
}

# The p.493 median/quantile recipe.
#' The p.493 median/quantile recipe
#'
#' A step of the dnnmt implementation. Called by \code{Dnnmt}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param Yc A matrix; indexed by row and column.
#' @param nt A count; the body uses it as \code{seq_len(...)}.
#' @param n See Usage.
#' @return A vector, from \code{vapply}.
#' @export
.dnnheadweights <- function(Yc, nt, n) {
  d <- numeric(nt)
  for (t in seq_len(nt)) {
    col <- Yc[, t]
    med <- .s03median(col)
    q1 <- .s03quantile7(col, 0.25)
    q3 <- .s03quantile7(col, 0.75)
    dt <- max(abs(med - q1), abs(q3 - med))
    if (!(dt > 0)) {
      stop(sprintf(paste0("dnn_multitrait: trait %d has a zero interquartile ",
                          "spread, so the p.493 head-weight recipe divides by ",
                          "zero; pass heads explicitly"), t), call. = FALSE)
    }
    d[t] <- dt
  }
  vapply(seq_len(nt), function(t) if (t == 1L) d[1L] else d[1L] / d[t], 0)
}

# One LCG step, s <- (1103515245 s + 12345) mod 2^31, split so that no
# intermediate product exceeds 2^53 and R doubles stay exact.
#' One LCG step, s <- (1103515245 s + 12345) mod 2^31, split so that no
#'
#' intermediate product exceeds 2^53 and R doubles stay exact.
#'
#' @param s Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.dnnlcg <- function(s) {
  hi <- floor(s / 65536)
  lo <- s %% 65536
  t1 <- (1103515245 * lo) %% 2147483648
  t2 <- ((1103515245 * hi) %% 32768) * 65536
  (t1 + t2 + 12345) %% 2147483648
}
