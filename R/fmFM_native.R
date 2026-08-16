# fmFM_native.R
# Factorization machines: interactions under sparsity.
# Rendle (2010) "Factorization Machines", ICDM 2010, 995-1000,
# doi:10.1109/ICDM.2010.127.
#
#   hat y(x) = w0 + sum_i w_i x_i + sum_{i<j} <v_i, v_j> x_i x_j
#
# Lemma 3.1: the double sum equals (1/2) sum_f [ (sum_i v_{i,f} x_i)^2
# - sum_i v_{i,f}^2 x_i^2 ], giving O(kn). Both forms are implemented
# and must agree. FMs subsume MF, SVD++, PITF, FPMC under particular
# input encodings.

.fmFM_EPS <- 1e-12

# --- eq. (1) as written -- the O(kn^2) double sum -------------------
#' Eq. (1) as written -- the O(kn^2) double sum -------------------
#'
#' A step of the fmFM_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param w0 Coerced to numeric by the body, with \code{as.numeric}.
#' @param w Numeric; combined arithmetically in the body.
#' @param V A vector; indexed elementwise.
#' @return The value of \code{s}, as built in the body.
#' @export
predict_naive <- function(x, w0, w, V) {
  xs <- as.numeric(x)
  n <- length(xs)
  s <- as.numeric(w0) + sum(w * xs)
  kk <- length(V[[1]])
  for (i in 1:(n - 1L)) {
    if (xs[i] == 0) next
    for (j in (i + 1L):n) {
      if (xs[j] == 0) next
      s <- s + sum(V[[i]][seq_len(kk)] * V[[j]][seq_len(kk)]) *
              xs[i] * xs[j]
    }
  }
  s
}

# --- the same value in O(kn) by Lemma 3.1 ---------------------------
#' The same value in O(kn) by Lemma 3.1 ---------------------------
#'
#' A step of the fmFM_native implementation. Called by \code{fit_fm}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param w0 Coerced to numeric by the body, with \code{as.numeric}.
#' @param w Numeric; combined arithmetically in the body.
#' @param V A vector; indexed elementwise.
#' @return The value of \code{s}, as built in the body.
#' @export
.fmFM_predict <- function(x, w0, w, V) {
  xs <- as.numeric(x)
  n <- length(xs); kk <- length(V[[1]])
  s <- as.numeric(w0) + sum(w * xs)
  for (f in seq_len(kk)) {
    a <- sum(vapply(seq_len(n), function(i) V[[i]][f] * xs[i],
                    numeric(1)))
    b <- sum(vapply(seq_len(n), function(i) (V[[i]][f] * xs[i])^2,
                    numeric(1)))
    s <- s + 0.5 * (a * a - b)
  }
  s
}

# --- d/d v_{i,f} of hat y, used in the SGD step --------------------
#' D/d v_{i,f} of hat y, used in the SGD step --------------------
#'
#' A step of the fmFM_native implementation. Called by \code{fit_fm}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param V A vector; indexed elementwise.
#' @param f See Usage.
#' @param i See Usage.
#' @return A numeric value.
#' @export
gradient <- function(x, V, f, i) {
  xs <- as.numeric(x)
  a <- sum(vapply(seq_along(xs), function(j) V[[j]][f] * xs[j],
                  numeric(1)))
  xs[i] * a - V[[i]][f] * xs[i]^2
}

# --- encoding under which an FM IS matrix factorisation -------------
# One indicator for the user, one for the item. The only non-zero
# interaction is then <v_u, v_i>.
#' Encoding under which an FM IS matrix factorisation -------------
#'
#' One indicator for the user, one for the item. The only non-zero
#' interaction is then <v_u, v_i>.
#'
#' @param u Coerced to integer by the body, with \code{as.integer}.
#' @param i Coerced to integer by the body, with \code{as.integer}.
#' @param n_users Coerced to integer by the body, with \code{as.integer}.
#' @param n_items Coerced to integer by the body, with \code{as.integer}.
#' @return The value of \code{x}, as built in the body.
#' @export
design_mf <- function(u, i, n_users, n_items) {
  x <- rep(0, as.integer(n_users) + as.integer(n_items))
  x[as.integer(u) + 1L] <- 1
  x[as.integer(n_users) + as.integer(i) + 1L] <- 1
  x
}

# --- least-squares FM by stochastic gradient descent ----------------
#' Least-squares FM by stochastic gradient descent ----------------
#'
#' A step of the fmFM_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; passed to \code{as.matrix}.
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @param k_dim Coerced to integer by the body, with \code{as.integer}. Defaults to \code{4}.
#' @param iters A count; the body uses it as \code{seq_len(...)}. Defaults to \code{300}.
#' @param alpha Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.02}.
#' @param lam Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.01}.
#' @param seed Defaults to \code{0}.
#' @return A list with \code{estimate}, \code{w0}, \code{w}, \code{V}, \code{mse_history}, \code{final_mse}, \code{k}, \code{n_features}, \code{method}.
#' @export
fit_fm <- function(X, y, k_dim = 4, iters = 300, alpha = 0.02,
                   lam = 0.01, seed = 0) {
  rows <- as.matrix(X); storage.mode(rows) <- "double"
  t <- as.numeric(y)
  if (nrow(rows) != length(t)) {
    stop(sprintf("fmFM: %d rows but %d targets", nrow(rows), length(t)))
  }
  if (nrow(rows) == 0L) stop("fmFM: no data")
  n <- ncol(rows); kk <- as.integer(k_dim)
  if (kk < 1L) stop("fmFM: k must be at least 1")
  set.seed(seed)
  w0 <- 0
  w <- rep(0, n)
  V <- replicate(kk, runif(n) - 0.5) * 0.1
  a <- as.numeric(alpha); lm <- as.numeric(lam)
  hist <- numeric(iters)
  for (it in seq_len(iters)) {
    for (r in seq_len(nrow(rows))) {
      e <- .fmFM_predict(rows[r, ], w0, w, V) - t[r]
      w0 <- w0 - a * e
      for (i in seq_len(n)) {
        if (rows[r, i] == 0) next
        w[i] <- w[i] - a * (e * rows[r, i] + lm * w[i])
        for (f in seq_len(kk)) {
          g <- gradient(rows[r, ], V, f, i)
          V[i, f] <- V[i, f] - a * (e * g + lm * V[i, f])
        }
      }
    }
    sse <- 0
    for (r in seq_len(nrow(rows))) {
      sse <- sse + (.fmFM_predict(rows[r, ], w0, w, V) - t[r])^2
    }
    hist[it] <- sse / nrow(rows)
  }
  list(estimate = list(w0 = w0, w = w, V = V), w0 = w0, w = w, V = V,
       mse_history = hist, final_mse = hist[length(hist)],
       k = kk, n_features = n,
       method = paste0("factorization machine, SGD; Rendle (2010) ",
                       "eq. (1) with the linear-time reformulation"))
}

# --- cheatsheet -----------------------------------------------------
#' Cheatsheet -----------------------------------------------------
#'
#' A step of the fmFM_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.fmFM_cheatsheet <- function() {
  paste0("fmFM: y = w0 + sum w_i x_i + sum_{i<j} <v_i,v_j> x_i x_j. ",
         "Factorising the interaction parameter COUPLES pairs that ",
         "an SVM treats independently, which is why FMs estimate ",
         "interactions under sparsity where SVMs fail -- a free ",
         "w_ij needs both features non-zero in the same row, and ",
         "almost none are. Lemma 3.1 turns the double sum into ",
         "O(kn). Because the model equation is direct, parameters ",
         "are learned in the PRIMAL with no support vectors. MF, ",
         "SVD++, PITF and FPMC are FMs with a particular input ",
         "encoding.")
}

# compact alias per ledger/NAMING.md
factorizationmachine <- fit_fm

# public names resolved by fn/_lazy_map.json
factorization_machines <- fit_fm

# house entry point: the package exports one morie_<module>
morie_fmFM <- fit_fm
