# Batch-Constrained Q-learning, discrete form.
# Sources: Fujimoto, S., Conti, E., Ghavamzadeh, M., & Pineau, J.
# (2019) "Benchmarking Batch Deep Reinforcement Learning Algorithms",
# arXiv:1910.01708, section 4 (discrete BCQ), equations 17 and 18:
# pi(s) = argmax_{a: G(a|s)/max_a G(a|s) > tau} Q(s,a), and the same
# constrained argmax replacing the max inside the temporal-difference
# backup. Fujimoto, S., van Hoof, H., & Meger, D. (2019) "Off-Policy
# Deep Reinforcement Learning without Exploration" (the original
# continuous method). The Huber loss in the DQN lineage is the choice
# of Mnih, V. et al. (2015) "Human-level control through deep
# reinforcement learning", *Nature* 518(7540), 529-533.

#' bcq
#'
#' A step of the bcq_native implementation. Called by \code{morie_bcq}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param dataset See Usage.
#' @param states Defaults to \code{NULL}.
#' @param actions Defaults to \code{NULL}.
#' @param tau Defaults to \code{0.3}.
#' @param gamma Numeric; combined arithmetically in the body. Defaults to \code{0.99}.
#' @param lr Numeric; combined arithmetically in the body. Defaults to \code{0.5}.
#' @param iters Defaults to \code{2000}.
#' @param loss One of \code{"huber"}, \code{"squared"}. Defaults to \code{"huber"}.
#' @param huber_c Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @param behavior Optional; may be \code{NULL}. A vector; indexed elementwise.
#' @param tol Defaults to \code{1e-12}.
#' @return A list with \code{estimate}, \code{q}, \code{policy}, \code{allowed}, \code{behavior}, \code{value}, \code{n_eliminated}, \code{bellman_error}, \code{tau}, \code{gamma}, \code{n_transitions}, \code{method}.
#' @export
bcq <- function(dataset, states = NULL, actions = NULL, tau = 0.3,
                gamma = 0.99, lr = 0.5, iters = 2000, loss = "huber",
                huber_c = 1.0, behavior = NULL, tol = 1e-12) {
  tau <- as.numeric(tau)
  if (is.na(tau) || tau < 0.0 || tau > 1.0)
    stop(sprintf("bcq: tau must lie in [0, 1], got %r", tau))
  if (!(loss %in% c("huber", "squared")))
    stop(sprintf("bcq: loss must be 'huber' or 'squared', got %r", loss))
  huber_c <- as.numeric(huber_c)
  if (huber_c <= 0.0)
    stop("bcq: huber_c must be > 0")

  D <- list()
  for (t in dataset) {
    if (length(t) == 4L) {
      s <- t[[1]]; a <- t[[2]]; r <- as.numeric(t[[3]]); s1 <- t[[4]]
      done <- FALSE
    } else if (length(t) == 5L) {
      s <- t[[1]]; a <- t[[2]]; r <- as.numeric(t[[3]]); s1 <- t[[4]]
      done <- as.logical(t[[5]])
    } else {
      stop("bcq: each transition must be (s, a, r, s_next) or (s, a, r, s_next, done)")
    }
    D[[length(D) + 1L]] <- list(s, a, r, s1, done)
  }
  if (length(D) == 0L) stop("bcq: dataset must be non-empty")

  if (is.null(states)) {
    all_states <- unlist(lapply(D, function(d) c(d[[1]], d[[4]])))
    S <- as.list(sort(unique(all_states)))
  } else {
    S <- as.list(states)
  }
  if (is.null(actions)) {
    A <- as.list(sort(unique(unlist(lapply(D, function(d) d[[2]])))))
  } else {
    A <- as.list(actions)
  }
  if (length(S) == 0L)
    stop("bcq: states and actions must be non-empty")
  if (length(A) == 0L)
    stop("bcq: states and actions must be non-empty")

  G <- new.env(hash = TRUE, parent = emptyenv())
  if (is.null(behavior)) {
    n_sa <- new.env(hash = TRUE, parent = emptyenv())
    n_s <- new.env(hash = TRUE, parent = emptyenv())
    for (d in D) {
      s <- d[[1]]; a <- d[[2]]
      key <- paste0(s, "|", a)
      n_sa[[key]] <- (n_sa[[key]] %||% 0) + 1
      n_s[[as.character(s)]] <- (n_s[[as.character(s)]] %||% 0) + 1
    }
    for (s in S) {
      for (a in A) {
        key <- paste0(s, "|", a)
        ns <- n_s[[as.character(s)]] %||% 0
        G[[key]] <- if (ns > 0) (n_sa[[key]] %||% 0) / ns else 1.0 / length(A)
      }
    }
  } else if (is.function(behavior)) {
    for (s in S) for (a in A) G[[paste0(s, "|", a)]] <- as.numeric(behavior(s, a))
  } else {
    for (s in S) for (a in A) G[[paste0(s, "|", a)]] <- as.numeric(behavior[[paste0(s, "|", a)]])
  }
  for (s in S) {
    tot <- 0.0
    for (a in A) tot <- tot + G[[paste0(s, "|", a)]]
    if (tot <= 0.0)
      stop(sprintf("bcq: G(.|%r) is all zero", s))
  }

  allowed <- new.env(hash = TRUE, parent = emptyenv())
  for (s in S) {
    gvals <- vapply(A, function(a) G[[paste0(s, "|", a)]], numeric(1))
    mx <- max(gvals)
    if (mx > 0) {
      keep_idx <- which(gvals / mx > tau)
    } else {
      keep_idx <- integer(0)
    }
    if (length(keep_idx) == 0L) {
      keep <- list(A[[which.max(gvals)]])
    } else {
      keep <- A[keep_idx]
    }
    allowed[[as.character(s)]] <- keep
  }

  Q <- new.env(hash = TRUE, parent = emptyenv())
  for (s in S) for (a in A) Q[[paste0(s, "|", a)]] <- 0.0
  for (iter in seq_len(as.integer(iters))) {
    target <- new.env(hash = TRUE, parent = emptyenv())
    cnt <- new.env(hash = TRUE, parent = emptyenv())
    for (d in D) {
      s <- d[[1]]; a <- d[[2]]; r <- d[[3]]; s1 <- d[[4]]; done <- d[[5]]
      key <- paste0(s, "|", a)
      if (done) {
        t_val <- r
      } else {
        al1 <- allowed[[as.character(s1)]]
        if (is.null(al1) || length(al1) == 0L) {
          t_val <- r
        } else {
          qvals <- vapply(al1, function(b) Q[[paste0(s1, "|", b)]], numeric(1))
          t_val <- r + gamma * max(qvals)
        }
      }
      target[[key]] <- (target[[key]] %||% 0.0) + t_val
      cnt[[key]] <- (cnt[[key]] %||% 0) + 1
    }
    delta <- 0.0
    keys <- ls(target)
    for (k in keys) {
      tgt <- target[[k]] / cnt[[k]]
      err <- tgt - Q[[k]]
      if (loss == "huber" && abs(err) > huber_c)
        err <- huber_c * (if (err > 0) 1.0 else -1.0)
      step <- lr * err
      Q[[k]] <- Q[[k]] + step
      if (abs(step) > delta) delta <- abs(step)
    }
    if (delta < tol) break
  }

  policy <- new.env(hash = TRUE, parent = emptyenv())
  value <- new.env(hash = TRUE, parent = emptyenv())
  for (s in S) {
    al <- allowed[[as.character(s)]]
    best <- al[[1]]; best_q <- Q[[paste0(s, "|", al[[1]])]]
    for (a in al) {
      v <- Q[[paste0(s, "|", a)]]
      if (v > best_q) { best_q <- v; best <- a }
    }
    policy[[as.character(s)]] <- best
    value[[as.character(s)]] <- best_q
  }

  berr <- 0.0
  for (d in D) {
    s <- d[[1]]; a <- d[[2]]; r <- d[[3]]; s1 <- d[[4]]; done <- d[[5]]
    if (done) {
      t_val <- r
    } else {
      al1 <- allowed[[as.character(s1)]]
      qvals <- vapply(al1, function(b) Q[[paste0(s1, "|", b)]], numeric(1))
      t_val <- r + gamma * max(qvals)
    }
    e <- t_val - Q[[paste0(s, "|", a)]]
    if (loss == "squared" || abs(e) <= huber_c)
      berr <- berr + 0.5 * e * e
    else
      berr <- berr + huber_c * (abs(e) - 0.5 * huber_c)
  }
  berr <- berr / length(D)

  n_elim <- 0L
  for (s in S) n_elim <- n_elim + (length(A) - length(allowed[[as.character(s)]]))

  Qr <- as.list(Q)
  Gr <- as.list(G)
  polr <- as.list(policy)
  vlr <- as.list(value)
  alr <- as.list(allowed)

  list(
    estimate = Qr,
    q = Qr,
    policy = polr,
    allowed = alr,
    behavior = Gr,
    value = vlr,
    n_eliminated = n_elim,
    bellman_error = as.numeric(berr),
    tau = tau,
    gamma = as.numeric(gamma),
    n_transitions = length(D),
    method = "discrete BCQ (Fujimoto et al. 2019, eqs. 17-18)"
  )
}

batch_constrained_q <- bcq

#' morie_bcq
#'
#' A step of the bcq_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param ... Passed through.
#' @return The value of \code{bcq}.
#' @export
morie_bcq <- function(...) bcq(...)
