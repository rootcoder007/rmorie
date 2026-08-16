# SPDX-License-Identifier: AGPL-3.0-or-later

.k03_lcg_m <- 2147483647
.k03_lcg_a <- 16807

## One MINSTD step. Exact in double precision (a * m < 2^53), so both
## language arms produce bit-identical initial weights.
#' # One MINSTD step. Exact in double precision (a * m < 2^53), so both
#'
#' # language arms produce bit-identical initial weights.
#'
#' @param state Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.k03_lcg <- function(state) (.k03_lcg_a * state) %% .k03_lcg_m

#' Wide & Deep jointly trained classifier
#'
#' The wide part is a generalised linear model on the raw wide features
#' and their cross-product transformations; the deep part is a
#' feed-forward network on the dense features. Their output log-odds are
#' summed and fed to one logistic loss, so the two parts are trained
#' jointly rather than ensembled: every parameter, in both parts and in
#' the shared bias, sees the same gradient step.
#'
#' In the numbering of Cheng et al. (2016): eq. (1) is the cross-product
#' transformation \eqn{\phi_k(x) = \prod_i x_i^{c_{ki}}}, which lets the
#' linear wide part express interactions; eq. (2) is the hidden-layer
#' computation \eqn{a^{(l+1)} = f(W^{(l)} a^{(l)} + b^{(l)})} with
#' \eqn{f} the rectifier; and eq. (3) is the prediction
#' \deqn{P(Y = 1 | x) = \sigma(w_{wide}^T [x, \phi(x)]
#'   + w_{deep}^T a^{(l_f)} + b).}
#'
#' The paper trains with mini-batch stochastic gradient descent, FTRL
#' with L1 for the wide part and AdaGrad for the deep part. This
#' implementation instead uses full-batch gradient descent with a fixed
#' step size for a fixed number of epochs, and initialises from a
#' reproducible linear congruential generator. The model, eq. (1) to (3),
#' and the joint logistic objective are the paper's; only the optimiser
#' is not, and it is replaced so that the fit is a deterministic function
#' of the inputs. Do not read the result as reproducing the paper's
#' benchmark numbers, which depend on that optimiser and on embeddings
#' this function does not fit.
#'
#' Mirrors \code{morie.fn.wideD} on the Python side.
#'
#' @param X_wide Numeric matrix of wide features, \eqn{n} rows.
#' @param X_deep Numeric matrix of dense features for the network.
#' @param y Binary labels in \{0, 1\}, length \eqn{n}.
#' @param hidden Integer vector of hidden layer widths. Default 8.
#' @param epochs Number of full-batch gradient steps.
#' @param lr Step size.
#' @param seed Seed of the deterministic generator used to initialise
#'   weights.
#' @param crosses List of length-2 integer vectors giving 0-based index
#'   pairs into the columns of \code{X_wide}; each adds the eq. (1)
#'   product of those columns to the wide design.
#' @param l2 Ridge penalty on every weight (not on biases). Default 0.
#' @return Named list with \code{coef_wide}, \code{coef_deep},
#'   \code{bias}, \code{hidden_weights}, \code{hidden_bias},
#'   \code{fitted}, \code{loss}, \code{n}, \code{n_wide}, \code{n_deep},
#'   \code{epochs}, \code{method}.
#' @references Cheng H-T et al. (2016). Wide & Deep learning for
#'   recommender systems. \emph{Proceedings of the 1st Workshop on Deep
#'   Learning for Recommender Systems (DLRS 2016)}, 7--10.
#'   arXiv:1606.07792, equations (1)--(3).
#' @examples
#' set.seed(1)
#' xw <- cbind(c(0, 1, 0, 1, 1, 0), c(1, 1, 0, 0, 1, 1))
#' xd <- cbind(c(0.2, 0.8, 0.1, 0.9, 0.7, 0.3))
#' WideD(xw, xd, c(0, 1, 0, 1, 1, 0), epochs = 50)$loss
#' @export
WideD <- function(X_wide, X_deep, y, hidden = 8L, epochs = 300L, lr = 0.05,
                  seed = 1L, crosses = NULL, l2 = 0) {
  as_mat <- function(a, nm) {
    m <- if (is.null(dim(a))) matrix(as.numeric(a), ncol = 1L) else
      matrix(as.numeric(as.matrix(a)), nrow = nrow(a))
    if (nrow(m) == 0L) stop(nm, " must be non-empty", call. = FALSE)
    m
  }
  xw <- as_mat(X_wide, "X_wide")
  xd <- as_mat(X_deep, "X_deep")
  yv <- as.numeric(y)
  n <- length(yv)
  if (nrow(xw) != n || nrow(xd) != n) {
    stop("X_wide, X_deep and y must have the same number of rows",
         call. = FALSE)
  }
  if (n < 2L) stop("need at least two observations", call. = FALSE)
  if (any(yv != 0 & yv != 1)) {
    stop("y must be binary, coded 0 and 1", call. = FALSE)
  }

  ## eq. (1): append the requested cross-product transformations.
  if (length(crosses)) {
    pw0 <- ncol(xw)
    for (pr in crosses) {
      a <- as.integer(pr[1L]); b <- as.integer(pr[2L])
      if (a < 0L || a >= pw0 || b < 0L || b >= pw0) {
        stop("cross indices must be columns of X_wide", call. = FALSE)
      }
      xw <- cbind(xw, xw[, a + 1L] * xw[, b + 1L])
    }
  }
  pw <- ncol(xw)
  pd <- ncol(xd)
  widths <- c(pd, as.integer(hidden))
  if (any(widths < 1L)) {
    stop("hidden layer widths must be positive", call. = FALSE)
  }
  epochs <- as.integer(epochs)
  if (epochs < 1L) stop("epochs must be at least 1", call. = FALSE)
  lr <- as.numeric(lr)[1L]
  l2 <- as.numeric(l2)[1L]

  state <- as.numeric(as.integer(seed) %% 2147483646L) + 1
  ## MINSTD emits a run of near-zero values from a small seed, which
  ## would make every initial weight about -scale and leave the whole
  ## ReLU stack dead on arrival. Discard a fixed warm-up so the seed only
  ## picks the stream, not its first magnitudes.
  for (i in seq_len(100)) state <- .k03_lcg(state)

  rnd <- function(scale) {
    state <<- .k03_lcg(state)
    (state / .k03_lcg_m - 0.5) * 2 * scale
  }

  nlayer <- length(widths) - 1L
  W <- vector("list", nlayer)
  B <- vector("list", nlayer)
  for (l in seq_len(nlayer)) {
    fan_in <- widths[l]
    scale <- sqrt(6 / (fan_in + widths[l + 1L]))
    wl <- vector("list", widths[l + 1L])
    for (u in seq_len(widths[l + 1L])) {
      wl[[u]] <- vapply(seq_len(fan_in), function(k) rnd(scale), 0)
    }
    W[[l]] <- wl
    B[[l]] <- rep(0, widths[l + 1L])
  }
  w_wide <- rep(0, pw)
  w_deep <- rep(0, widths[nlayer + 1L])
  bias <- 0

  forward <- function(row_d) {
    acts <- vector("list", nlayer + 1L)
    acts[[1L]] <- row_d
    a <- row_d
    for (l in seq_len(nlayer)) {
      nxt <- numeric(widths[l + 1L])
      for (u in seq_len(widths[l + 1L])) {
        s <- B[[l]][u] + sum(W[[l]][[u]] * a)   # eq. (2)
        nxt[u] <- if (s > 0) s else 0           # ReLU
      }
      acts[[l + 1L]] <- nxt
      a <- nxt
    }
    acts
  }

  loss <- NA_real_
  ntop <- widths[nlayer + 1L]
  for (ep in seq_len(epochs)) {
    g_wide <- rep(0, pw)
    g_deep <- rep(0, ntop)
    g_bias <- 0
    gW <- lapply(seq_len(nlayer), function(l)
      lapply(seq_len(widths[l + 1L]), function(u) rep(0, widths[l])))
    gB <- lapply(seq_len(nlayer), function(l) rep(0, widths[l + 1L]))
    loss <- 0
    for (i in seq_len(n)) {
      acts <- forward(xd[i, ])
      top <- acts[[nlayer + 1L]]
      z <- bias + sum(w_wide * xw[i, ]) + sum(w_deep * top)   # eq. (3)
      if (z >= 0) {
        e <- exp(-z); p <- 1 / (1 + e)
        loss <- loss + log1p(e) + (1 - yv[i]) * z
      } else {
        e <- exp(z); p <- e / (1 + e)
        loss <- loss + log1p(e) - yv[i] * z
      }
      r <- p - yv[i]
      g_bias <- g_bias + r
      g_wide <- g_wide + r * xw[i, ]
      g_deep <- g_deep + r * top
      delta <- r * w_deep
      for (l in seq.int(nlayer, 1L)) {
        a_prev <- acts[[l]]
        a_cur <- acts[[l + 1L]]
        dpre <- ifelse(a_cur > 0, delta, 0)
        for (u in seq_len(widths[l + 1L])) {
          du <- dpre[u]
          if (du == 0) next
          gB[[l]][u] <- gB[[l]][u] + du
          gW[[l]][[u]] <- gW[[l]][[u]] + du * a_prev
        }
        if (l > 1L) {
          nd <- rep(0, widths[l])
          for (u in seq_len(widths[l + 1L])) {
            du <- dpre[u]
            if (du == 0) next
            nd <- nd + du * W[[l]][[u]]
          }
          delta <- nd
        }
      }
    }
    loss <- loss / n
    step <- lr / n
    bias <- bias - step * g_bias
    w_wide <- w_wide - step * (g_wide + l2 * w_wide * n)
    w_deep <- w_deep - step * (g_deep + l2 * w_deep * n)
    for (l in seq_len(nlayer)) {
      for (u in seq_len(widths[l + 1L])) {
        B[[l]][u] <- B[[l]][u] - step * gB[[l]][u]
        W[[l]][[u]] <- W[[l]][[u]] -
          step * (gW[[l]][[u]] + l2 * W[[l]][[u]] * n)
      }
    }
  }

  fitted <- numeric(n)
  for (i in seq_len(n)) {
    top <- forward(xd[i, ])[[nlayer + 1L]]
    z <- bias + sum(w_wide * xw[i, ]) + sum(w_deep * top)
    fitted[i] <- if (z >= 0) 1 / (1 + exp(-z)) else exp(z) / (1 + exp(z))
  }

  list(coef_wide = w_wide,
       coef_deep = w_deep,
       bias = bias,
       hidden_weights = W,
       hidden_bias = B,
       fitted = fitted,
       loss = loss,
       n = n,
       n_wide = pw,
       n_deep = pd,
       epochs = epochs,
       method = "Wide & Deep (Cheng et al. 2016, eqs. 1-3)")
}
