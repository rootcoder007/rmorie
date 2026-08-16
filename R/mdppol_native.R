# SPDX-License-Identifier: AGPL-3.0-or-later
# Policy iteration for a finite MDP (Howard's algorithm, Sutton-Barto 2018 Sec 4.3).

.mdppol_method <- "Policy iteration (iterative policy evaluation + greedy improvement)"

#' .mdppol_args
#'
#' A step of the mdppol_native implementation. Called by \code{morie_mdppol}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param P Iterated over elementwise, with \code{lapply}.
#' @param R A vector; its length is taken and its elements indexed.
#' @return A list with \code{Pm}, \code{R}, \code{S}, \code{A}.
#' @export
.mdppol_args <- function(P, R) {
  Pm <- lapply(P, function(p) {
    if (is.matrix(p)) {
      p
    } else {
      p <- as.numeric(p)
      n <- as.integer(sqrt(length(p)))
      if (n * n != length(p)) stop("P element is not square")
      matrix(p, nrow = n)
    }
  })
  A <- length(Pm)
  S <- nrow(Pm[[1]])
  if (is.list(R) && !is.matrix(R) && length(R) == A) {
    Rmat <- matrix(0, S, A)
    for (a in seq_len(A)) {
      Pa <- Pm[[a]]
      Ra <- if (is.matrix(R[[a]])) R[[a]] else matrix(as.numeric(R[[a]]), S, S)
      for (s in seq_len(S)) {
        Rmat[s, a] <- sum(Pa[s, ] * Ra[s, ])
      }
    }
    R <- Rmat
  } else {
    if (!is.matrix(R)) {
      R <- matrix(as.numeric(R), S, A)
    }
  }
  list(Pm = Pm, R = R, S = S, A = A)
}

#' morie_mdppol
#'
#' A step of the mdppol_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param P Passed to \code{.mdppol_args}.
#' @param R A matrix; indexed by row and column.
#' @param gamma Numeric; combined arithmetically in the body.
#' @param tol Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1e-12}.
#' @param max_eval A count; the body uses it as \code{seq_len(...)}. Defaults to \code{1e+05}.
#' @param max_improve A count; the body uses it as \code{seq_len(...)}. Defaults to \code{1000}.
#' @param pi0 Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{estimate}, \code{policy}, \code{q}, \code{n_improve}, \code{n_eval}, \code{policy_stable}, \code{method}.
#' @export
morie_mdppol <- function(P, R, gamma, tol = 1e-12, max_eval = 100000,
                         max_improve = 1000, pi0 = NULL) {
  args <- .mdppol_args(P, R)
  Pm <- args$Pm
  R <- args$R
  S <- args$S
  A <- args$A
  gamma <- as.numeric(gamma)
  tol <- as.numeric(tol)
  max_eval <- as.integer(max_eval)
  max_improve <- as.integer(max_improve)
  
  pol <- rep(1L, S)
  if (!is.null(pi0)) {
    pi0v <- as.numeric(pi0)
    for (s in seq_len(S)) {
      a <- as.integer(pi0v[s])
      if (a < 0L || a >= A) stop(sprintf("pi0[%d] out of range", s))
      pol[s] <- a + 1L
    }
  }
  V <- rep(0, S)
  n_eval <- 0L
  stable <- FALSE
  rounds <- 0L
  
  for (rounds in seq_len(max_improve)) {
    # 2. iterative policy evaluation (sweep order s = 1..S)
    for (i in seq_len(max_eval)) {
      n_eval <- n_eval + 1L
      delta <- 0
      for (s in seq_len(S)) {
        v <- V[s]
        a <- pol[s]
        Vs <- R[s, a] + gamma * sum(Pm[[a]][s, ] * V)
        V[s] <- Vs
        d <- abs(v - Vs)
        if (d > delta) delta <- d
      }
      if (delta < tol) break
    }
    # 3. policy improvement
    stable <- TRUE
    for (s in seq_len(S)) {
      old <- pol[s]
      qs <- numeric(A)
      for (a in seq_len(A)) {
        qs[a] <- R[s, a] + gamma * sum(Pm[[a]][s, ] * V)
      }
      best <- 1L
      best_val <- qs[1]
      for (a in seq_len(A)) {
        if (qs[a] > best_val) {
          best_val <- qs[a]
          best <- a
        }
      }
      if (best_val > qs[old] + 1e-12) {
        pol[s] <- best
        stable <- FALSE
      }
    }
    if (stable) break
  }
  
  Q <- matrix(0, S, A)
  for (s in seq_len(S)) {
    for (a in seq_len(A)) {
      Q[s, a] <- R[s, a] + gamma * sum(Pm[[a]][s, ] * V)
    }
  }
  
  list(
    estimate = V,
    policy = as.numeric(pol - 1L),
    q = Q,
    n_improve = as.integer(rounds),
    n_eval = as.integer(n_eval),
    policy_stable = stable,
    method = .mdppol_method
  )
}

mdppol <- morie_mdppol

#' .mdppol_cheatsheet
#'
#' A step of the mdppol_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.mdppol_cheatsheet <- function() {
  "mdppol(P, R, gamma) -> optimal policy/V by Howard policy iteration (Sutton-Barto 2018 Sec 4.3)."
}
