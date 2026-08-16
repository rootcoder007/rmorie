# Nash Q-learning for general-sum stochastic games.
# Sources: Hu, J. & Wellman, M. P. (2003) "Nash Q-Learning for
# General-Sum Stochastic Games", *Journal of Machine Learning
# Research* 4, 1039-1069: Definitions 5-7 and 12-13, eqs. 5-7,
# Table 2.
#
# Native implementation mirroring Python morie.fn.nashq exactly:
# the joint-action Q-function of Definition 5, the eqs. 6-7 update
# with the same selected stage-game equilibrium driving both
# agents' updates, exact support enumeration of the two-player
# stage game, and the same Definition 12 (global optimal) and
# Definition 13 (saddle) classifications of the final stage games.

.NASHQ_SELECTIONS <- c("global_optimal", "saddle", "first", "best_for_agent")

#' .nashq_mat
#'
#' A step of the nashq_native implementation. Called by \code{nash_equilibria_bimatrix}, \code{stage_game_type}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param M A matrix; passed to \code{nrow}.
#' @param name See Usage.
#' @return The value of \code{M}, as built in the body.
#' @export
.nashq_mat <- function(M, name) {
  M <- as.matrix(M)
  if (!is.numeric(M)) {
    M <- apply(M, c(1, 2), as.numeric)
  }
  storage.mode(M) <- "double"
  if (nrow(M) < 1L || ncol(M) < 1L) {
    stop(sprintf("nashq: %s must be a non-empty matrix", name))
  }
  M
}

#' .nashq_solve
#'
#' A step of the nashq_native implementation. Called by \code{.nashq_indifference}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A A matrix; passed to \code{nrow}.
#' @param b See Usage.
#' @return The value of \code{[}.
#' @export
.nashq_solve <- function(A, b) {
  n <- nrow(A)
  M <- cbind(A, b)
  for (c in seq_len(n) - 1L) {
    rng <- seq.int(c + 1L, n)
    p <- rng[which.max(abs(M[rng, c + 1L]))]
    if (abs(M[p, c + 1L]) < 1e-12) return(NULL)
    if (p != c) {
      tmp <- M[c + 1L, ]
      M[c + 1L, ] <- M[p, ]
      M[p, ] <- tmp
    }
    pv <- M[c + 1L, c + 1L]
    M[c + 1L, ] <- M[c + 1L, ] / pv
    for (r in seq_len(n)) {
      if (r == c + 1L) next
      f <- M[r, c + 1L]
      if (f == 0) next
      M[r, ] <- M[r, ] - f * M[c + 1L, ]
    }
  }
  M[, n + 1L]
}

#' Unknowns x_0..x_{k-1}, v
#'
#' A step of the nashq_native implementation. Called by \code{nash_equilibria_bimatrix}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param payoff A matrix; indexed by row and column.
#' @param k A count; the body uses it as \code{seq_len(...)}.
#' @return The value of \code{[}.
#' @export
.nashq_indifference <- function(payoff, k) {
  # unknowns x_0..x_{k-1}, v
  Aeq <- matrix(0, nrow = k + 1L, ncol = k + 1L)
  beq <- numeric(k + 1L)
  for (i in seq_len(k)) {
    Aeq[i, seq_len(k)] <- payoff[i, ]
    Aeq[i, k + 1L] <- -1
  }
  Aeq[k + 1L, seq_len(k)] <- 1
  beq[k + 1L] <- 1
  sol <- .nashq_solve(Aeq, beq)
  if (is.null(sol)) return(NULL)
  sol[seq_len(k)]
}

#' .nashq_payoff
#'
#' A step of the nashq_native implementation. Called by \code{.nashq_is_equilibrium}, \code{.nashq_is_saddle}, \code{.nashq_select} and 2 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param M A matrix; indexed by row and column.
#' @param p A vector; its length is taken and its elements indexed.
#' @param q A vector; its length is taken and its elements indexed.
#' @return The value of \code{tot}, as built in the body.
#' @export
.nashq_payoff <- function(M, p, q) {
  tot <- 0
  for (i in seq_along(p)) {
    if (p[i] == 0) next
    for (j in seq_along(q)) {
      if (q[j] != 0) tot <- tot + p[i] * q[j] * M[i, j]
    }
  }
  tot
}

#' .nashq_is_equilibrium
#'
#' A step of the nashq_native implementation. Called by \code{nash_equilibria_bimatrix}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A A matrix; indexed by row and column.
#' @param B A matrix; indexed by row and column.
#' @param p Numeric; combined arithmetically in the body.
#' @param q Numeric; combined arithmetically in the body.
#' @param tol Numeric; combined arithmetically in the body.
#' @return A logical value.
#' @export
.nashq_is_equilibrium <- function(A, B, p, q, tol) {
  va <- .nashq_payoff(A, p, q)
  vb <- .nashq_payoff(B, p, q)
  for (i in seq_len(nrow(A))) {
    dev <- sum(q * A[i, ])
    if (dev > va + tol) return(FALSE)
  }
  for (j in seq_len(ncol(A))) {
    dev <- sum(p * B[, j])
    if (dev > vb + tol) return(FALSE)
  }
  TRUE
}

#' nash_equilibria_bimatrix
#'
#' A step of the nashq_native implementation. Called by \code{.nashq_select}, \code{stage_game_type}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A A matrix; indexed by row and column.
#' @param B A matrix; indexed by row and column.
#' @param tol Numeric; combined arithmetically in the body. Defaults to \code{1e-09}.
#' @return The value of \code{out}, as built in the body.
#' @export
nash_equilibria_bimatrix <- function(A, B, tol = 1e-9) {
  A <- .nashq_mat(A, "A")
  B <- .nashq_mat(B, "B")
  m <- nrow(A); n <- ncol(A)
  if (nrow(B) != m || ncol(B) != n) {
    stop("nashq: A and B must have the same shape")
  }

  out <- vector("list", 0L)
  seen <- list()
  for (k in seq_len(min(m, n))) {
    if (k > m || k > n) break
    Icombs <- combn(seq_len(m), k, simplify = FALSE)
    Jcombs <- combn(seq_len(n), k, simplify = FALSE)
    for (I in Icombs) {
      for (J in Jcombs) {
        subA <- A[I, J, drop = FALSE]
        subBt <- B[I, J, drop = FALSE]
        q <- .nashq_indifference(subA, k)
        p <- .nashq_indifference(subBt, k)
        if (is.null(q) || is.null(p)) next
        if (min(q) < -tol || min(p) < -tol) next
        P <- numeric(m)
        Q <- numeric(n)
        for (a in seq_along(I)) P[I[a]] <- max(0, p[a])
        for (a in seq_along(J)) Q[J[a]] <- max(0, q[a])
        sp <- sum(P); sq <- sum(Q)
        if (sp <= tol || sq <= tol) next
        P <- P / sp; Q <- Q / sq
        if (!.nashq_is_equilibrium(A, B, P, Q, tol)) next
        key <- paste0(paste(round(P, 9), collapse = ","), "|",
                      paste(round(Q, 9), collapse = ","))
        if (!is.null(seen[[key]])) next
        seen[[key]] <- TRUE
        out[[length(out) + 1L]] <- list(p = P, q = Q)
      }
    }
  }
  out
}

#' Definition 13: each agent gains when the OTHER deviates
#'
#' A step of the nashq_native implementation. Called by \code{.nashq_select}, \code{stage_game_type}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A A matrix; passed to \code{nrow}.
#' @param B Passed to \code{.nashq_payoff}.
#' @param p Passed to \code{.nashq_payoff}.
#' @param q Passed to \code{.nashq_payoff}.
#' @param tol Numeric; combined arithmetically in the body.
#' @return A logical value.
#' @export
.nashq_is_saddle <- function(A, B, p, q, tol) {
  # Definition 13: each agent gains when the OTHER deviates.
  for (j in seq_len(ncol(A))) {
    pure <- numeric(ncol(A))
    pure[j] <- 1
    if (.nashq_payoff(A, p, pure) < .nashq_payoff(A, p, q) - tol) {
      return(FALSE)
    }
  }
  for (i in seq_len(nrow(A))) {
    pure <- numeric(nrow(A))
    pure[i] <- 1
    if (.nashq_payoff(B, pure, q) < .nashq_payoff(B, p, q) - tol) {
      return(FALSE)
    }
  }
  TRUE
}

#' stage_game_type
#'
#' A step of the nashq_native implementation. Called by \code{morie_nashq}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A Numeric; passed to \code{max}.
#' @param B Numeric; passed to \code{max}.
#' @param tol Numeric; combined arithmetically in the body. Defaults to \code{1e-09}.
#' @return A list with \code{estimate}, \code{equilibria}, \code{n_equilibria}, \code{has_global_optimal}, \code{has_saddle}, \code{global_optimal}, \code{saddle}, \code{method}.
#' @export
stage_game_type <- function(A, B, tol = 1e-9) {
  A <- .nashq_mat(A, "A")
  B <- .nashq_mat(B, "B")
  eqs <- nash_equilibria_bimatrix(A, B, tol)
  best_a <- max(A)
  best_b <- max(B)
  glob <- list()
  sad <- list()
  for (e in eqs) {
    p <- e$p; q <- e$q
    va <- .nashq_payoff(A, p, q)
    vb <- .nashq_payoff(B, p, q)
    if (va >= best_a - tol && vb >= best_b - tol) {
      glob[[length(glob) + 1L]] <- list(p = p, q = q)
    }
    if (.nashq_is_saddle(A, B, p, q, tol)) {
      sad[[length(sad) + 1L]] <- list(p = p, q = q)
    }
  }
  list(estimate = length(eqs),
       equilibria = eqs,
       n_equilibria = length(eqs),
       has_global_optimal = length(glob) > 0L,
       has_saddle = length(sad) > 0L,
       global_optimal = glob,
       saddle = sad,
       method = paste0("stage game classification (Hu & Wellman 2003 ",
                       "Defs 12-13)"))
}

#' .nashq_select
#'
#' A step of the nashq_native implementation. Called by \code{morie_nashq}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A Numeric; passed to \code{max}.
#' @param B Numeric; passed to \code{max}.
#' @param selection One of \code{"best_for_agent"}, \code{"first"}, \code{"global_optimal"}.
#' @param agent See Usage.
#' @param tol Numeric; combined arithmetically in the body.
#' @return The value of \code{[[}.
#' @export
.nashq_select <- function(A, B, selection, agent, tol) {
  eqs <- nash_equilibria_bimatrix(A, B, tol)
  if (length(eqs) == 0L) return(NULL)
  if (selection == "first") return(eqs[[1]])
  if (selection == "best_for_agent") {
    M <- if (agent == 0) A else B
    best <- eqs[[1]]
    best_v <- .nashq_payoff(M, best$p, best$q)
    for (k in seq.int(2L, length(eqs))) {
      e <- eqs[[k]]
      v <- .nashq_payoff(M, e$p, e$q)
      if (v > best_v) {
        best <- e
        best_v <- v
      }
    }
    return(best)
  }
  if (selection == "global_optimal") {
    ba <- max(A)
    bb <- max(B)
    for (e in eqs) {
      p <- e$p; q <- e$q
      if (.nashq_payoff(A, p, q) >= ba - tol &&
          .nashq_payoff(B, p, q) >= bb - tol) {
        return(e)
      }
    }
    return(eqs[[1]])
  }
  for (e in eqs) {
    if (.nashq_is_saddle(A, B, e$p, e$q, tol)) return(e)
  }
  eqs[[1]]
}

#' .nashq_pick
#'
#' A step of the nashq_native implementation. Called by \code{morie_nashq}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param M A matrix; passed to \code{nrow}.
#' @param A A vector; its length is taken.
#' @param who See Usage.
#' @param epsilon See Usage.
#' @param rng_e Passed to \code{.ghc_unif}.
#' @return One of two values, depending on the branch taken.
#' @export
.nashq_pick <- function(M, A, who, epsilon, rng_e) {
  if (.ghc_unif(rng_e, 1L) < epsilon) {
    return(as.integer(.ghc_unif(rng_e, 1L) * length(A)) + 1L)
  }
  if (who == 0) {
    vals <- rowSums(M) / ncol(M)
  } else {
    vals <- colSums(M) / nrow(M)
  }
  bv <- max(vals)
  best <- which(vals >= bv - 1e-15)
  if (length(best) > 1L) {
    best[as.integer(.ghc_unif(rng_e, 1L) * length(best)) + 1L]
  } else {
    best[1L]
  }
}

#' morie_nashq
#'
#' A step of the nashq_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param states Coerced to list by the body, with \code{as.list}.
#' @param actions A vector; its length is taken and its elements indexed.
#' @param step A function; the body checks with \code{is.function}.
#' @param rewards A function; the body checks with \code{is.function}.
#' @param gamma Numeric; combined arithmetically in the body. Defaults to \code{0.9}.
#' @param alpha Numeric; combined arithmetically in the body. Defaults to \code{0.5}.
#' @param epsilon Passed to \code{.nashq_pick}. Defaults to \code{0.1}.
#' @param episodes Coerced to integer by the body, with \code{as.integer}. Defaults to \code{500}.
#' @param horizon Coerced to integer by the body, with \code{as.integer}. Defaults to \code{50}.
#' @param start Optional; may be \code{NULL}. A function; the body checks with \code{is.function}.
#' @param selection Passed to \code{.nashq_select}. Defaults to \code{"global_optimal"}.
#' @param terminal Optional; may be \code{NULL}. Coerced to list by the body, with \code{as.list}.
#' @param seed Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.
#' @param agent Coerced to integer by the body, with \code{as.integer}. Defaults to \code{0L}.
#' @param tol Passed to \code{.nashq_select}. Defaults to \code{1e-09}.
#' @return A list with \code{estimate}, \code{q}, \code{policy}, \code{nash_values}, \code{stage_game_types}, \code{returns}, \code{mean_return_last}, \code{selection}, \code{method}.
#' @export
morie_nashq <- function(states, actions, step, rewards,
                        gamma = 0.9, alpha = 0.5, epsilon = 0.1,
                        episodes = 500, horizon = 50, start = NULL,
                        selection = "global_optimal", terminal = NULL,
                        seed = 0, agent = 0L, tol = 1e-9) {
  if (!(selection %in% .NASHQ_SELECTIONS)) {
    stop(sprintf("nashq: selection must be one of %s, got %s",
                 paste(sQuote(.NASHQ_SELECTIONS), collapse = ", "),
                 selection))
  }
  S <- as.list(states)
  if (length(actions) != 2L) {
    stop("nashq: this implementation covers two players; ",
         "pass actions as (A1, A2)")
  }
  A1 <- as.list(actions[[1]])
  A2 <- as.list(actions[[2]])
  if (length(S) == 0L || length(A1) == 0L || length(A2) == 0L) {
    stop("nashq: states and both action sets must be non-empty")
  }
  if (!is.function(step) || !is.function(rewards)) {
    stop("nashq: step and rewards must be callable")
  }
  if (is.null(terminal)) term <- list() else term <- as.list(terminal)
  if (is.null(start)) {
    s0_fn <- function() S[[1]]
  } else if (is.function(start)) {
    s0_fn <- start
  } else {
    s0_fn <- function() start
  }
  rng_e <- .ghc_rng(as.numeric(seed))

  # Q[[paste(player, s)]] is a |A1| x |A2| payoff matrix.
  Q <- list()
  for (pl in c(0L, 1L)) {
    for (s in S) {
      key <- paste0(pl, "\r", deparse(s, control = "keepInteger"))
      Q[[key]] <- matrix(0, nrow = length(A1), ncol = length(A2))
    }
  }

  .nashq_key <- function(pl, s) {
    paste0(pl, "\r", deparse(s, control = "keepInteger"))
  }

  returns <- vector("list", as.integer(episodes))
  for (ep in seq_len(as.integer(episodes))) {
    s <- s0_fn()
    tot <- c(0, 0)
    for (t in seq_len(as.integer(horizon))) {
      if (any(vapply(term, function(x) identical(x, s), logical(1L)))) break
      M0 <- Q[[.nashq_key(0L, s)]]
      M1 <- Q[[.nashq_key(1L, s)]]
      i <- .nashq_pick(M0, A1, 0L, epsilon, rng_e)
      j <- .nashq_pick(M1, A2, 1L, epsilon, rng_e)
      s1 <- step(s, A1[[i]], A2[[j]])
      rr <- rewards(s, A1[[i]], A2[[j]], s1)
      r1 <- rr[[1]]; r2 <- rr[[2]]
      tot[1] <- tot[1] + r1
      tot[2] <- tot[2] + r2
      is_term <- any(vapply(term, function(x) identical(x, s1), logical(1L)))
      if (is_term) {
        nv <- c(0, 0)
      } else {
        M0_s1 <- Q[[.nashq_key(0L, s1)]]
        M1_s1 <- Q[[.nashq_key(1L, s1)]]
        eq <- .nashq_select(M0_s1, M1_s1, selection, as.integer(agent), tol)
        if (is.null(eq)) {
          nv <- c(0, 0)
        } else {
          nv <- c(.nashq_payoff(M0_s1, eq$p, eq$q),
                  .nashq_payoff(M1_s1, eq$p, eq$q))
        }
      }
      for (pl in c(0L, 1L)) {
        cur <- Q[[.nashq_key(pl, s)]][i, j]
        r <- if (pl == 0L) r1 else r2
        Q[[.nashq_key(pl, s)]][i, j] <-
          (1 - alpha) * cur + alpha * (r + gamma * nv[pl + 1L])
      }
      s <- s1
    }
    returns[[ep]] <- tot
  }

  policy <- list()
  nash_values <- list()
  types <- list()
  for (s in S) {
    M0 <- Q[[.nashq_key(0L, s)]]
    M1 <- Q[[.nashq_key(1L, s)]]
    cls <- stage_game_type(M0, M1, tol)
    type_str <- if (isTRUE(cls$has_global_optimal)) "global_optimal"
      else if (isTRUE(cls$has_saddle)) "saddle"
      else if (cls$n_equilibria > 0L) "neither"
      else "none_found"
    types[[deparse(s, control = "keepInteger")]] <- type_str
    eq <- .nashq_select(M0, M1, selection, as.integer(agent), tol)
    if (is.null(eq)) next
    policy[[deparse(s, control = "keepInteger")]] <- list(p = eq$p, q = eq$q)
    nash_values[[deparse(s, control = "keepInteger")]] <-
      c(.nashq_payoff(M0, eq$p, eq$q),
        .nashq_payoff(M1, eq$p, eq$q))
  }

  tenth <- max(1L, as.integer(episodes) %/% 10L)
  mean_last <- c(
    sum(vapply(returns[(length(returns) - tenth + 1L):length(returns)],
               function(r) r[1], numeric(1))) / tenth,
    sum(vapply(returns[(length(returns) - tenth + 1L):length(returns)],
               function(r) r[2], numeric(1))) / tenth
  )
  list(estimate = Q,
       q = Q,
       policy = policy,
       nash_values = nash_values,
       stage_game_types = types,
       returns = returns,
       mean_return_last = mean_last,
       selection = selection,
       method = "Nash Q-learning (Hu & Wellman 2003, Table 2)")
}

# compact aliases per ledger/NAMING.md
nashq <- morie_nashq
nash_q_learning <- morie_nashq
nashqlearning <- morie_nashq

#' .nashq_cheatsheet
#'
#' A step of the nashq_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.nashq_cheatsheet <- function() {
  paste0("nashq: Q^i over JOINT actions; update with the stage-game ",
         "Nash payoff instead of a max -- Q^i <- (1-a)Q^i + ",
         "a[r^i + beta pi^1...pi^n Q^i(s')] (Hu & Wellman 2003 ",
         "eqs. 6-7). Needs every agent's reward. Equilibrium ",
         "selection changes the update: convergence is proved only ",
         "for global optimal (Def 12) or saddle (Def 13) stage ",
         "games. stage_game_type() reports which you have.")
}

