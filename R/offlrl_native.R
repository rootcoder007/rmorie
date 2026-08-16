# Conservative Q-Learning for offline RL.
# Sources: Kumar, A., Zhou, A., Tucker, G. & Levine, S. (2020)
# "Conservative Q-Learning for Offline Reinforcement Learning",
# NeurIPS, arXiv:2006.04779. Eq. 2 (the CQL objective with the
# push-down-on-mu, push-up-on-pi_beta asymmetry), Eq. 3 (the
# min_Q max_mu family regularised by R(mu)), Eq. 4 (the CQL(H)
# variant with rho = Unif, whose closed form is the logsumexp
# penalty), Theorems 3.1 and 3.2 (pointwise lower bound and the
# weaker expected-value lower bound under pi), the variant rho
# (mu proportional to pi^{k-1}) and the backup-mode distinction
# between B* (Q-learning) and B^pi (policy evaluation).

# Base R only, faithful translation of offlrl_python_reference.py.

.OFFLRL_VARIANTS <- c("H", "rho", "mu")
.OFFLRL_BACKUPS <- c("max", "pi")

#' .offlrl_logsumexp
#'
#' A step of the offlrl_native implementation. Called by \code{offlrl}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v Numeric; passed to \code{max}.
#' @return A numeric value.
#' @export
.offlrl_logsumexp <- function(v) {
  m <- max(v)
  m + log(sum(exp(v - m)))
}

#' .offlrl_softmax
#'
#' A step of the offlrl_native implementation. Called by \code{offlrl}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v Numeric; passed to \code{max}.
#' @return A numeric value.
#' @export
.offlrl_softmax <- function(v) {
  m <- max(v)
  e <- exp(v - m)
  s <- sum(e)
  e / s
}

#' .offlrl_as_dist
#'
#' A step of the offlrl_native implementation. Called by \code{offlrl}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param d Optional; may be \code{NULL}. A vector; indexed elementwise.
#' @param S See Usage.
#' @param A See Usage.
#' @param name See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.offlrl_as_dist <- function(d, S, A, name) {
  if (is.null(d)) return(NULL)
  if (is.function(d)) {
    out <- list()
    for (s in S) for (a in A) {
      key <- paste0(s, "\r", a)
      out[[key]] <- as.numeric(d(s, a))
    }
  } else if (is.list(d)) {
    out <- list()
    for (s in S) for (a in A) {
      key <- paste0(s, "\r", a)
      v <- d[[key]]
      out[[key]] <- if (is.null(v)) 0.0 else as.numeric(v)
    }
  } else {
    stop("offlrl: ", name, " must be a function or a list")
  }
  for (s in S) {
    tot <- 0.0
    for (a in A) tot <- tot + out[[paste0(s, "\r", a)]]
    if (abs(tot - 1.0) > 1e-6)
      stop("offlrl: ", name, "(.|", s, ") sums to ", format(tot),
           ", not 1")
  }
  out
}

#' .offlrl_key
#'
#' A step of the offlrl_native implementation. Called by \code{offlrl}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param s See Usage.
#' @param a See Usage.
#' @return A character value.
#' @export
.offlrl_key <- function(s, a) paste0(s, "\r", a)

#' offlrl
#'
#' A step of the offlrl_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param dataset See Usage.
#' @param states Optional; may be \code{NULL}. Coerced to list by the body, with \code{as.list}.
#' @param actions Optional; may be \code{NULL}. Coerced to list by the body, with \code{as.list}.
#' @param alpha Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @param gamma Numeric; combined arithmetically in the body. Defaults to \code{0.99}.
#' @param variant One of \code{"H"}, \code{"mu"}, \code{"rho"}. Defaults to \code{"H"}.
#' @param backup One of \code{"max"}, \code{"pi"}. Defaults to \code{"max"}.
#' @param policy Passed to \code{.offlrl_as_dist}.
#' @param mu Passed to \code{.offlrl_as_dist}.
#' @param lr Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.5}.
#' @param iters Coerced to integer by the body, with \code{as.integer}. Defaults to \code{2000}.
#' @param tol Defaults to \code{1e-12}.
#' @return A list with \code{estimate}, \code{q}, \code{value}, \code{greedy}, \code{behavior}, \code{counts}, \code{penalty}, \code{bellman_error}, \code{objective}, \code{alpha}, \code{variant}, \code{backup}, \code{n_transitions}, \code{method}.
#' @export
offlrl <- function(dataset, states = NULL, actions = NULL, alpha = 1.0,
                   gamma = 0.99, variant = "H", backup = "max",
                   policy = NULL, mu = NULL, lr = 0.5, iters = 2000,
                   tol = 1e-12) {
  if (!(variant %in% .OFFLRL_VARIANTS))
    stop("offlrl: variant must be one of ",
         paste(sprintf("%r", .OFFLRL_VARIANTS), collapse = ", "),
         ", got ", sprintf("%r", variant))
  if (!(backup %in% .OFFLRL_BACKUPS))
    stop("offlrl: backup must be 'max' or 'pi', got ",
         sprintf("%r", backup))
  alpha <- as.numeric(alpha)
  if (alpha < 0.0)
    stop("offlrl: alpha must be >= 0, got ", format(alpha))

  D <- list()
  for (t in dataset) {
    if (length(t) == 4L) {
      s <- t[[1L]]; a <- t[[2L]]; r <- as.numeric(t[[3L]])
      s1 <- t[[4L]]; done <- FALSE
    } else if (length(t) == 5L) {
      s <- t[[1L]]; a <- t[[2L]]; r <- as.numeric(t[[3L]])
      s1 <- t[[4L]]; done <- as.logical(t[[5L]])
    } else {
      stop("offlrl: each transition must be (s, a, r, s_next) or (s, a, r, s_next, done)")
    }
    D[[length(D) + 1L]] <- list(s = s, a = a, r = r, s1 = s1,
                                 done = done)
  }
  if (length(D) == 0L)
    stop("offlrl: dataset must be non-empty")

  if (!is.null(states)) {
    S <- as.list(states)
  } else {
    all_states <- unique(c(vapply(D, function(x) x$s, character(1L)),
                           vapply(D, function(x) x$s1, character(1L))))
    S <- as.list(all_states)
  }
  if (!is.null(actions)) {
    A <- as.list(actions)
  } else {
    all_a <- unique(vapply(D, function(x) x$a, character(1L)))
    A <- as.list(all_a)
  }
  if (length(S) == 0L || length(A) == 0L)
    stop("offlrl: states and actions must be non-empty")

  n_sa <- list()
  n_s <- list()
  for (tr in D) {
    sk <- .offlrl_key(tr$s, tr$a)
    n_sa[[sk]] <- if (is.null(n_sa[[sk]])) 1L
                  else n_sa[[sk]] + 1L
    n_s[[tr$s]] <- if (is.null(n_s[[tr$s]])) 1L
                   else n_s[[tr$s]] + 1L
  }

  behavior <- list()
  for (s in S) {
    if (is.null(n_s[[s]])) next
    for (a in A) {
      k <- .offlrl_key(s, a)
      cnt <- if (is.null(n_sa[[k]])) 0L else n_sa[[k]]
      behavior[[k]] <- cnt / as.numeric(n_s[[s]])
    }
  }

  pol <- .offlrl_as_dist(policy, S, A, "policy")
  muu <- .offlrl_as_dist(mu, S, A, "mu")
  if (variant == "mu" && is.null(muu))
    stop("offlrl: variant='mu' needs mu(a|s)")
  if (backup == "pi" && is.null(pol))
    stop("offlrl: backup='pi' needs policy(a|s)")
  if (variant == "rho" && is.null(pol))
    stop("offlrl: variant='rho' needs policy(a|s) to play the role of pi^{k-1}")

  Q <- list()
  for (s in S) for (a in A) {
    Q[[.offlrl_key(s, a)]] <- 0.0
  }

  data_states <- S[vapply(S, function(s) !is.null(n_s[[s]]),
                          logical(1L))]

  for (iter in seq_len(as.integer(iters))) {
    target <- list()
    cnt <- list()
    for (tr in D) {
      if (tr$done) {
        tt <- tr$r
      } else if (backup == "max") {
        bvals <- vapply(A, function(b) Q[[.offlrl_key(tr$s1, b)]],
                        numeric(1L))
        tt <- tr$r + gamma * max(bvals)
      } else {
        acc <- 0.0
        for (b in A) {
          acc <- acc + pol[[.offlrl_key(tr$s1, b)]] *
            Q[[.offlrl_key(tr$s1, b)]]
        }
        tt <- tr$r + gamma * acc
      }
      k <- .offlrl_key(tr$s, tr$a)
      target[[k]] <- if (is.null(target[[k]])) tt
                     else target[[k]] + tt
      cnt[[k]] <- if (is.null(cnt[[k]])) 1L else cnt[[k]] + 1L
    }
    for (k in names(target)) target[[k]] <- target[[k]] / cnt[[k]]

    grad <- list()
    for (s in S) for (a in A)
      grad[[.offlrl_key(s, a)]] <- 0.0

    for (s in data_states) {
      w <- n_s[[s]] / as.numeric(length(D))
      qs <- vapply(A, function(b) Q[[.offlrl_key(s, b)]], numeric(1L))
      if (variant == "H") {
        push <- .offlrl_softmax(qs)
      } else if (variant == "rho") {
        m <- max(qs)
        e <- vapply(seq_along(A), function(i) {
          pol[[.offlrl_key(s, A[[i]])]] * exp(qs[i] - m)
        }, numeric(1L))
        z <- sum(e)
        push <- if (z > 0) e / z else rep(1.0 / length(A), length(A))
      } else {
        push <- vapply(A, function(b) muu[[.offlrl_key(s, b)]],
                       numeric(1L))
      }
      for (i in seq_along(A)) {
        b <- A[[i]]
        k <- .offlrl_key(s, b)
        grad[[k]] <- grad[[k]] + alpha * w *
          (push[i] - behavior[[k]])
      }
    }
    for (k in names(target)) {
      grad[[k]] <- grad[[k]] +
        (cnt[[k]] / as.numeric(length(D))) * (Q[[k]] - target[[k]])
    }

    delta <- 0.0
    for (k in names(Q)) {
      step <- as.numeric(lr) * grad[[k]]
      Q[[k]] <- Q[[k]] - step
      if (abs(step) > delta) delta <- abs(step)
    }
    if (delta < tol) break
  }

  value <- list()
  for (s in S) {
    bvals <- vapply(A, function(b) Q[[.offlrl_key(s, b)]], numeric(1L))
    value[[paste0(s)]] <- max(bvals)
  }
  greedy <- list()
  for (s in S) {
    bvals <- vapply(A, function(b) Q[[.offlrl_key(s, b)]], numeric(1L))
    greedy[[paste0(s)]] <- A[[which.max(bvals)]]
  }

  pen <- 0.0
  for (s in data_states) {
    w <- n_s[[s]] / as.numeric(length(D))
    qs <- vapply(A, function(b) Q[[.offlrl_key(s, b)]], numeric(1L))
    if (variant == "H") {
      first <- .offlrl_logsumexp(qs)
    } else if (variant == "rho") {
      m <- max(qs)
      acc <- 0.0
      for (i in seq_along(A)) {
        acc <- acc + pol[[.offlrl_key(s, A[[i]])]] * exp(qs[i] - m)
      }
      first <- m + log(acc)
    } else {
      acc <- 0.0
      for (b in A) acc <- acc + muu[[.offlrl_key(s, b)]] *
        Q[[.offlrl_key(s, b)]]
      first <- acc
    }
    bsum <- 0.0
    for (b in A) bsum <- bsum + behavior[[.offlrl_key(s, b)]] *
      Q[[.offlrl_key(s, b)]]
    pen <- pen + w * (first - bsum)
  }

  berr <- 0.0
  for (tr in D) {
    if (tr$done) {
      tt <- tr$r
    } else if (backup == "max") {
      bvals <- vapply(A, function(b) Q[[.offlrl_key(tr$s1, b)]],
                      numeric(1L))
      tt <- tr$r + gamma * max(bvals)
    } else {
      acc <- 0.0
      for (b in A) acc <- acc + pol[[.offlrl_key(tr$s1, b)]] *
        Q[[.offlrl_key(tr$s1, b)]]
      tt <- tr$r + gamma * acc
    }
    berr <- berr + 0.5 * (Q[[.offlrl_key(tr$s, tr$a)]] - tt)^2
  }
  berr <- berr / as.numeric(length(D))

  list(
    estimate = Q,
    q = Q,
    value = value,
    greedy = greedy,
    behavior = behavior,
    counts = n_sa,
    penalty = as.numeric(pen),
    bellman_error = as.numeric(berr),
    objective = as.numeric(alpha * pen + berr),
    alpha = alpha,
    variant = variant,
    backup = backup,
    n_transitions = length(D),
    method = paste0("CQL (Kumar et al. 2020, eq. ",
                    if (variant %in% c("H", "rho")) "4" else "2", ")")
  )
}

# compact aliases per ledger/NAMING.md
offline_rl_cql <- offlrl
offlinerlcql <- offlrl
conservative_q_learning <- offlrl

#' .offlrl_cheatsheet
#'
#' A step of the offlrl_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.offlrl_cheatsheet <- function() {
  paste("offlrl: CQL (Kumar 2020). Fitted Q plus alpha*(push DOWN ",
        "E_mu[Q] - push UP E_pi_beta[Q]) so the Q-function LOWER ",
        "BOUNDS the truth and OOD actions stop being over-estimated. ",
        "variant='H' is eq. 4's logsumexp (rho=Unif); 'rho' uses ",
        "pi^{k-1}; 'mu' is eq. 2 directly. Thm 3.2 bounds the ",
        "EXPECTED value under pi, not pointwise. alpha=0 is plain ",
        "fitted Q.", sep = "")
}

morie_offlrl <- offlrl
