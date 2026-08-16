# morie.fn -- function file (rootcoder007/morie)
# Implicit feedback: preference and confidence are different things.
#
# References
# ----------
# Hu, Y., Koren, Y. & Volinsky, C. (2008) "Collaborative Filtering for
# Implicit Feedback Datasets", Proceedings of the Eighth IEEE
# International Conference on Data Mining (ICDM 2008), 263-272,
# doi:10.1109/ICDM.2008.22. Sec. 2 (the four properties of implicit
# feedback, in particular that its numerical value indicates confidence
# rather than preference). Sec. 4 (the binarised p_ui, the confidence
# c_ui = 1 + alpha r_ui with alpha = 40, the cost function of eq. (3)
# summed over ALL m*n pairs, the alternating least squares update
# eq. (4), and the Y'C^u Y = Y'Y + Y'(C^u - I)Y decomposition giving
# O(f^2 N + f^3 m) total time). Sec. 5 (explaining recommendations
# through W^u).
#
# Koren, Y., Bell, R. & Volinsky, C. (2009) "Matrix Factorization
# Techniques for Recommender Systems", Computer 42(8), 30-37,
# doi:10.1109/MC.2009.263. The explicit-feedback factorisation this is
# adapted from.

.impFB_eps <- 1e-12

.impFB_preference <- function(r) {
  r <- as.matrix(r)
  storage.mode(r) <- "double"
  result <- matrix(0, nrow = nrow(r), ncol = ncol(r))
  result[r > 0] <- 1.0
  return(result)
}

.impFB_confidence <- function(r, alpha = 40.0) {
  a <- as.numeric(alpha)
  if (a < 0.0) stop("impFB: alpha must be non-negative")
  r <- as.matrix(r)
  storage.mode(r) <- "double"
  return(1.0 + a * r)
}

.impFB_solve <- function(A, b) {
  n <- length(b)
  M <- matrix(0, nrow = n, ncol = n + 1)
  for (i in 1:n) {
    M[i, 1:n] <- A[i, ]
    M[i, n + 1] <- b[i]
  }
  for (c in 1:n) {
    p <- c
    max_val <- abs(M[c, c])
    if (c < n) {
      for (i in (c + 1):n) {
        if (abs(M[i, c]) > max_val) {
          max_val <- abs(M[i, c])
          p <- i
        }
      }
    }
    if (abs(M[p, c]) < 1e-14) {
      stop("impFB: the normal equations are singular; increase lambda")
    }
    if (p != c) {
      tmp <- M[c, ]
      M[c, ] <- M[p, ]
      M[p, ] <- tmp
    }
    d <- M[c, c]
    M[c, ] <- M[c, ] / d
    for (i in 1:n) {
      if (i != c && M[i, c] != 0.0) {
        f <- M[i, c]
        M[i, ] <- M[i, ] - f * M[c, ]
      }
    }
  }
  return(M[, n + 1])
}

.impFB_als_step <- function(Y, C_row, p_row, lam, fast = TRUE) {
  n <- nrow(Y)
  f <- ncol(Y)
  lm <- as.numeric(lam)
  A <- matrix(0, nrow = f, ncol = f)
  if (fast) {
    A <- crossprod(Y)
    for (i in 1:n) {
      if (C_row[i] != 1.0) {
        w <- C_row[i] - 1.0
        A <- A + w * t(Y[i, , drop = FALSE]) %*% Y[i, , drop = FALSE]
      }
    }
  } else {
    for (i in 1:n) {
      A <- A + C_row[i] * t(Y[i, , drop = FALSE]) %*% Y[i, , drop = FALSE]
    }
  }
  diag(A) <- diag(A) + lm
  rhs <- crossprod(Y, C_row * p_row)
  return(.impFB_solve(A, rhs))
}

.impFB_cost <- function(R, X, Y, alpha = 40.0, lam = 0.1) {
  P <- .impFB_preference(R)
  C <- .impFB_confidence(R, alpha)
  pred <- X %*% t(Y)
  err <- P - pred
  tot <- sum(C * err * err)
  reg <- sum(X * X) + sum(Y * Y)
  return(tot + as.numeric(lam) * reg)
}

#' morie_impFB
#'
#' Part of the impFB_native implementation; see the file header for the
#' source it follows.
#'
#' @param R See Usage.
#' @param f Defaults to \code{8}.
#' @param alpha Defaults to \code{40}.
#' @param lam Defaults to \code{0.1}.
#' @param iters Defaults to \code{15}.
#' @param seed Defaults to \code{0}.
#' @param fast Defaults to \code{TRUE}.
#' @return The value of \code{result}, as built in the body.
#' @export
morie_impFB <- function(R, f = 8, alpha = 40.0, lam = 0.1, iters = 15,
                       seed = 0, fast = TRUE) {
  M <- as.matrix(R)
  storage.mode(M) <- "double"
  m <- nrow(M)
  n <- ncol(M)

  if (as.integer(f) < 1) {
    stop("impFB: f must be at least 1")
  }
  if (any(M < 0.0)) {
    stop("impFB: implicit counts cannot be negative")
  }

  P <- .impFB_preference(M)
  C <- .impFB_confidence(M, alpha)

  rng <- .ghc_rng(seed)
  xrand <- .ghc_unif(rng, m * as.integer(f))
  X <- matrix(xrand * 0.1, nrow = m, ncol = as.integer(f), byrow = TRUE)

  yrand <- .ghc_unif(rng, n * as.integer(f))
  Y <- matrix(yrand * 0.1, nrow = n, ncol = as.integer(f), byrow = TRUE)

  hist <- numeric(as.integer(iters))

  for (iter in 1:as.integer(iters)) {
    for (u in 1:m) {
      X[u, ] <- .impFB_als_step(Y, C[u, ], P[u, ], lam, fast)
    }
    for (i in 1:n) {
      Y[i, ] <- .impFB_als_step(X, C[, i], P[, i], lam, fast)
    }
    hist[iter] <- .impFB_cost(M, X, Y, alpha, lam)
  }

  result <- list(
    estimate = list(X, Y),
    X = X,
    Y = Y,
    cost_history = hist,
    final_cost = if (length(hist) > 0) hist[length(hist)] else NaN,
    f = as.integer(f),
    alpha = as.numeric(alpha),
    lambda = as.numeric(lam),
    method = "weighted ALS over all m*n pairs; Hu, Koren & Volinsky (2008) eqs. (3)-(4)",
    note = "the numerical value of implicit feedback is CONFIDENCE, not preference"
  )
  return(result)
}

.impFB_explain <- function(Y, C_row, p_row, i, lam = 0.1) {
  n <- nrow(Y)
  f <- ncol(Y)

  A <- matrix(0, nrow = f, ncol = f)
  for (t in 1:n) {
    A <- A + C_row[t] * t(Y[t, , drop = FALSE]) %*% Y[t, , drop = FALSE]
  }
  diag(A) <- diag(A) + as.numeric(lam)

  W <- matrix(0, nrow = f, ncol = f)
  for (a in 1:f) {
    e_a <- numeric(f)
    e_a[a] <- 1.0
    W[, a] <- .impFB_solve(A, e_a)
  }

  yi <- Y[as.integer(i), ]
  v <- as.numeric(W %*% yi)

  terms <- list()
  for (j in 1:n) {
    if (p_row[j] > 0.0) {
      terms[[as.character(j)]] <- C_row[j] * p_row[j] * sum(v * Y[j, ])
    }
  }

  prediction <- sum(unlist(terms))

  return(list(
    contributions = terms,
    prediction = prediction,
    note = "each past item's share of the predicted preference"
  ))
}

.impFB_cheatsheet <- function() {
  return("impFB: implicit feedback measures CONFIDENCE, not preference -- the favourite film is watched once, the merely-liked series weekly. Split into binary p_ui and c_ui = 1 + alpha r_ui (alpha = 40). The cost sums over ALL m*n pairs, because zeros are missing evidence rather than negatives, which rules out SGD and forces ALS. Y'C^u Y = Y'Y + Y'(C^u - I)Y makes each update O(f^2 n_u + f^3), linear in the input. Substituting the update into the prediction yields per-item explanations.")
}

# compact alias per ledger/NAMING.md
implicitfeedback <- morie_impFB

# public names resolved by fn/_lazy_map.json
implicit_feedback_loss <- morie_impFB
