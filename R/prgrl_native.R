# Curriculum learning: easy examples first.
#
# Bengio, Y., Louradour, J., Collobert, R., & Weston, J. (2009)
# "Curriculum Learning", *ICML*, 41-48.
#
# The paper gives the idea a definition precise enough to check. Let
# :math:`0 \le W_\lambda(z) \le 1` be the weight on example :math:`z` at
# step :math:`\lambda` of the sequence, with :math:`W_1(z) = 1`. The
# training distribution at that step is
#
# .. math::
#
#    Q_\lambda(z) \propto W_\lambda(z) P(z),
#    \qquad Q_1(z) = P(z)
#    \tag{1, 2}
#
# and the sequence is a **curriculum** when two things hold:
#
# .. math::
#
#    H(Q_\lambda) < H(Q_{\lambda + \epsilon})
#    \quad\text{and}\quad
#    W_{\lambda+\epsilon}(z) \ge W_\lambda(z)
#    \qquad \forall \epsilon > 0
#    \tag{3, 4}
#
# -- the entropy of the training distribution increases, and no example's
# weight ever falls. The second condition is what makes it a curriculum
# rather than a schedule: examples are only ever *added*.
#
# **How the paper actually tests this.** Section 4 uses *convex* criteria
# and still finds an effect, which is worth being precise about, because
# on a convex objective run to convergence the ordering cannot move the
# optimum. The effect is elsewhere:
#
# * **Section 4.1** trains a linear classifier on the easy examples only
#   -- those on the correct side of the Bayes boundary -- and measures
#   *generalization*: 16.3% error against 17.1% for the full set. That is
#   a different training set, not a reordering.
# * **Section 4.2** trains a Perceptron **online with a fixed budget** of
#   200 updates and measures generalization at the end. Because training
#   stops well short of convergence, the order the updates arrive in
#   decides where it stops. The paper's two easiness criteria are the
#   number of irrelevant inputs zeroed out, and the margin
#   :math:`y\,w'x`.
#
# So the quantity to compare is held-out error under a fixed update
# budget, not training loss at convergence -- comparing the latter on a
# convex objective can only ever return "no difference", which says
# nothing about curricula.
#
# **What reproduces here, and what does not.** Section 4.1 does, closely:
# on its two-Gaussian setup, training on the clean examples only gives
# 0.1604 against 0.1635 for the full set, where the paper prints 0.163
# against 0.171 -- same direction, same size, and the absolute levels
# agree to under a point. :func:`easy_only_fit` is that experiment.
#
# Section 4.2 does not. On a reconstruction of its generator the
# curriculum is *worse* than shuffling: 0.213 sorted easiest-first and
# 0.175 sampling from :math:`Q_\lambda`, against 0.140 shuffled, over 300
# restarts. Two things in that section are underdetermined -- the number
# of irrelevant inputs is never given, and "ordered by easiness" does not
# say whether examples are sorted once or sampled from the widening
# support -- so both orderings are offered through ``order=`` and neither
# is claimed to reproduce the paper. This is recorded rather than tuned
# away; a fixture adjusted until it agreed would be evidence of nothing.
#
# :func:`curriculum_schedule` builds the sequence from a difficulty score
# and checks both conditions; :func:`is_curriculum` checks an arbitrary
# sequence of weights someone else built. :func:`prgrl` runs the paper's
# comparison -- the same learner trained on the curriculum and on the
# shuffled data -- and reports both loss curves, because the claim the
# paper makes is about the speed of convergence and the quality of the
# minimum reached, not about a formula.

#' .prgrl_to_rows
#'
#' A step of the prgrl_native implementation. Called by \code{easy_only_fit}, \code{prgrl}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; indexed by row and column.
#' @return One of two values, depending on the branch taken.
#' @export
.prgrl_to_rows <- function(X) {
  if (is.matrix(X)) {
    lapply(seq_len(nrow(X)), function(i) as.numeric(X[i, ]))
  } else if (is.list(X)) {
    lapply(X, as.numeric)
  } else {
    M <- as.matrix(X)
    lapply(seq_len(nrow(M)), function(i) as.numeric(M[i, ]))
  }
}

#' .prgrl_to_vec
#'
#' A step of the prgrl_native implementation. Called by \code{curriculum_schedule}, \code{easy_only_fit}, \code{prgrl}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x See Usage.
#' @return A vector, from \code{as.numeric}.
#' @export
.prgrl_to_vec <- function(x) {
  as.numeric(as.vector(x))
}

#' .prgrl_rng
#'
#' A step of the prgrl_native implementation. Called by \code{easy_only_fit}, \code{prgrl}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param seed See Usage.
#' @return The value of \code{function}.
#' @export
.prgrl_rng <- function(seed) {
  st <- as.numeric(seed) %% 2147483648
  if (st == 0L) st <- 1L
  e <- new.env()
  e$st <- st
  function() {
    e$st <- .ghc_lcg31(e$st)
    e$st / 2^31
  }
}

#' .prgrl_gauss
#'
#' A step of the prgrl_native implementation. Called by \code{easy_only_fit}, \code{prgrl}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param r See Usage.
#' @return A numeric value.
#' @export
.prgrl_gauss <- function(r) {
  u <- max(r(), 1e-12)
  v <- r()
  sqrt(-2.0 * log(u)) * cos(2.0 * pi * v)
}

#' entropy
#'
#' A step of the prgrl_native implementation. Called by \code{is_curriculum}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param q Numeric; passed to \code{sum}.
#' @return A numeric value.
#' @export
entropy <- function(q) {
  q <- as.numeric(q)
  tot <- sum(q)
  if (tot <= 0) stop("prgrl: the distribution has no mass")
  p <- q / tot
  p <- p[p > 0]
  -sum(p * log(p))
}

#' curriculum_schedule
#'
#' A step of the prgrl_native implementation. Called by \code{prgrl}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param difficulty Passed to \code{.prgrl_to_vec}.
#' @param n_steps A count; the body uses it as \code{seq_len(...)}. Defaults to \code{5}.
#' @param hard_first A flag; the body branches on it. Defaults to \code{FALSE}.
#' @return A list with \code{lambdas}, \code{weights}, \code{dists}.
#' @export
curriculum_schedule <- function(difficulty, n_steps = 5, hard_first = FALSE) {
  d <- .prgrl_to_vec(difficulty)
  n <- length(d)
  if (n < 2) stop("prgrl: need at least two examples")
  n_steps <- as.integer(n_steps)
  if (n_steps < 2) stop("prgrl: need at least two curriculum steps")
  if (hard_first) {
    order_idx <- order(-d)
  } else {
    order_idx <- order(d)
  }
  lambdas <- numeric(n_steps)
  weights <- vector("list", n_steps)
  dists <- vector("list", n_steps)
  for (s in seq_len(n_steps) - 1L) {
    lam <- (s + 1) / n_steps
    take <- max(1L, round(lam * n))
    if (s == n_steps - 1L) take <- n
    keep <- order_idx[seq_len(take)]
    w <- numeric(n)
    w[keep] <- 1.0
    weights[[s + 1L]] <- w
    lambdas[s + 1L] <- lam
    dists[[s + 1L]] <- w / take
  }
  list(lambdas = lambdas, weights = weights, dists = dists)
}

#' is_curriculum
#'
#' A step of the prgrl_native implementation. Called by \code{prgrl}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param weights A vector; its length is taken and its elements indexed.
#' @param p Optional; may be \code{NULL}. A vector; indexed elementwise.
#' @param tol Numeric; combined arithmetically in the body. Defaults to \code{1e-12}.
#' @return A list with \code{is_curriculum}, \code{entropy_increasing}, \code{strictly_increasing}, \code{weights_monotone}, \code{final_step_is_p}, \code{entropies}.
#' @export
is_curriculum <- function(weights, p = NULL, tol = 1e-12) {
  if (length(weights) < 2) stop("prgrl: need at least two steps to check")
  n <- length(weights[[1]])
  for (w in weights) {
    if (length(w) != n) stop("prgrl: the weight vectors differ in length")
    for (v in w) {
      if (!(-tol <= v && v <= 1.0 + tol)) stop("prgrl: weights must lie in [0, 1]")
    }
  }
  if (is.null(p)) p <- rep(1.0 / n, n)
  ents <- numeric(length(weights))
  monotone <- TRUE
  final_ones <- TRUE
  for (k in seq_along(weights)) {
    w <- weights[[k]]
    q <- numeric(n)
    for (i in seq_len(n)) q[i] <- w[i] * p[i]
    if (sum(q) <= 0) stop(sprintf("prgrl: step %d has no mass", k - 1L))
    ents[k] <- entropy(q)
    if (k > 1L) {
      prev <- weights[[k - 1L]]
      for (i in seq_len(n)) {
        if (w[i] < prev[i] - tol) {
          monotone <- FALSE
          break
        }
      }
    }
  }
  if (length(ents) >= 2L) {
    diffs <- ents[2:length(ents)] - ents[1:(length(ents) - 1L)]
    increasing <- all(diffs > -tol)
    strictly <- all(diffs > 1e-12)
  } else {
    increasing <- TRUE
    strictly <- TRUE
  }
  for (v in weights[[length(weights)]]) {
    if (abs(v - 1.0) > tol) {
      final_ones <- FALSE
      break
    }
  }
  list(
    is_curriculum = increasing && monotone,
    entropy_increasing = increasing,
    strictly_increasing = strictly,
    weights_monotone = monotone,
    final_step_is_p = final_ones,
    entropies = ents
  )
}

#' .prgrl_perceptron
#'
#' A step of the prgrl_native implementation. Called by \code{easy_only_fit}, \code{prgrl}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A vector; indexed elementwise.
#' @param y A vector; indexed elementwise.
#' @param order A vector; its length is taken and its elements indexed.
#' @param updates See Usage.
#' @param w0 See Usage.
#' @return The value of \code{w}, as built in the body.
#' @export
.prgrl_perceptron <- function(X, y, order, updates, w0) {
  p <- length(X[[1]])
  w <- as.numeric(w0)
  n <- length(order)
  updates_int <- as.integer(updates)
  for (step in seq_len(updates_int) - 1L) {
    i <- order[(step %% n) + 1L]
    s <- 0
    for (k in seq_len(p)) {
      s <- s + w[k] * X[[i]][k]
    }
    if (y[i] * s <= 0) {
      for (k in seq_len(p)) {
        w[k] <- w[k] + y[i] * X[[i]][k]
      }
    }
  }
  w
}

#' .prgrl_error
#'
#' A step of the prgrl_native implementation. Called by \code{easy_only_fit}, \code{prgrl}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A vector; its length is taken and its elements indexed.
#' @param y A vector; indexed elementwise.
#' @param w A vector; indexed elementwise.
#' @return A numeric value.
#' @export
.prgrl_error <- function(X, y, w) {
  p <- length(X[[1]])
  bad <- 0
  for (i in seq_along(X)) {
    s <- 0
    for (k in seq_len(p)) {
      s <- s + w[k] * X[[i]][k]
    }
    val <- if (s != 0) s else -1.0
    if (y[i] * val <= 0) bad <- bad + 1
  }
  bad / length(X)
}

#' prgrl
#'
#' A step of the prgrl_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X Passed to \code{.prgrl_to_rows}.
#' @param y Passed to \code{.prgrl_to_vec}.
#' @param difficulty Passed to \code{.prgrl_to_vec}.
#' @param X_test Optional; may be \code{NULL}. Passed to \code{.prgrl_to_rows}.
#' @param y_test Passed to \code{.prgrl_to_vec}.
#' @param updates Passed to \code{.prgrl_perceptron}. Defaults to \code{200}.
#' @param n_steps Defaults to \code{5}.
#' @param seed Passed to \code{.prgrl_rng}. Defaults to \code{0}.
#' @param n_repeats Defaults to \code{50}.
#' @param order One of \code{"sampled"}, \code{"sorted"}. Defaults to \code{"sampled"}.
#' @return A list with \code{estimate}, \code{curriculum_error}, \code{baseline_error}, \code{improvement}, \code{curriculum_errors}, \code{baseline_errors}, \code{held_out}, \code{updates}, \code{order}, \code{n_repeats}, \code{lambdas}, \code{weights}, \code{entropies}, \code{is_curriculum}, \code{n}, \code{method}, \code{note}.
#' @export
prgrl <- function(X, y, difficulty, X_test = NULL, y_test = NULL,
                  updates = 200, n_steps = 5, seed = 0, n_repeats = 50,
                  order = "sampled") {
  Xr <- .prgrl_to_rows(X)
  yv <- .prgrl_to_vec(y)
  n <- length(Xr)
  if (length(yv) != n) stop("prgrl: X and y must have the same length")
  if (n < 2) stop("prgrl: need at least two examples")
  if (as.integer(updates) < 1) stop("prgrl: updates must be at least 1")
  for (v in yv) {
    if (!(v %in% c(-1.0, 1.0))) {
      stop("prgrl: y must be -1/+1 for the Perceptron of Section 4.2")
    }
  }
  d <- .prgrl_to_vec(difficulty)
  if (length(d) != n) stop("prgrl: difficulty must have one score per example")

  if (is.null(X_test)) {
    Xe <- Xr
    ye <- yv
    held_out <- FALSE
  } else {
    Xe <- .prgrl_to_rows(X_test)
    ye <- .prgrl_to_vec(y_test)
    if (length(Xe) != length(ye)) stop("prgrl: X_test and y_test must match")
    held_out <- TRUE
  }

  if (!(order %in% c("sampled", "sorted"))) {
    stop("prgrl: order must be 'sampled' or 'sorted'")
  }

  easy <- order(d)
  cs <- curriculum_schedule(d, n_steps)
  lam <- cs$lambdas
  weights <- cs$weights
  chk <- is_curriculum(weights)

  updates_int <- as.integer(updates)
  n_repeats_int <- as.integer(n_repeats)
  p_dim <- length(Xr[[1]])

  curriculum_order_fn <- function(rnd) {
    if (order == "sorted") {
      return(easy)
    }
    seq <- integer(updates_int)
    for (step in seq_len(updates_int) - 1L) {
      lam_s <- (step + 1) / updates_int
      take <- max(1L, round(lam_s * n))
      seq[step + 1L] <- easy[floor(rnd() * take) + 1L]
    }
    seq
  }

  rnd <- .prgrl_rng(seed)
  cur_errs <- numeric(n_repeats_int)
  base_errs <- numeric(n_repeats_int)

  for (rep in seq_len(n_repeats_int)) {
    w0 <- numeric(p_dim)
    for (i in seq_len(p_dim)) w0[i] <- .prgrl_gauss(rnd)

    shuffled <- seq_len(n)
    for (i in n:2) {
      j <- floor(rnd() * i) + 1L
      tmp <- shuffled[i]
      shuffled[i] <- shuffled[j]
      shuffled[j] <- tmp
    }

    cur_w <- .prgrl_perceptron(Xr, yv, curriculum_order_fn(rnd), updates, w0)
    cur_errs[rep] <- .prgrl_error(Xe, ye, cur_w)

    base_w <- .prgrl_perceptron(Xr, yv, shuffled, updates, w0)
    base_errs[rep] <- .prgrl_error(Xe, ye, base_w)
  }

  cur <- mean(cur_errs)
  base <- mean(base_errs)

  note <- if (held_out) {
    "the comparison is held-out error under a FIXED update budget, which is what the paper measures. Training loss at convergence on a convex criterion cannot differ between orderings and says nothing"
  } else {
    "no test set was given, so this scored on the training set; pass X_test and y_test for the paper's comparison"
  }

  list(
    estimate = cur,
    curriculum_error = cur,
    baseline_error = base,
    improvement = base - cur,
    curriculum_errors = cur_errs,
    baseline_errors = base_errs,
    held_out = held_out,
    updates = updates_int,
    order = order,
    n_repeats = n_repeats_int,
    lambdas = lam,
    weights = weights,
    entropies = chk$entropies,
    is_curriculum = chk$is_curriculum,
    n = n,
    method = "curriculum learning (Bengio, Louradour, Collobert & Weston 2009), Section 4.2: online Perceptron, fixed update budget, generalization error",
    note = note
  )
}

#' easy_only_fit
#'
#' A step of the prgrl_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X Passed to \code{.prgrl_to_rows}.
#' @param y Passed to \code{.prgrl_to_vec}.
#' @param difficulty Passed to \code{.prgrl_to_vec}.
#' @param X_test Passed to \code{.prgrl_to_rows}.
#' @param y_test Passed to \code{.prgrl_to_vec}.
#' @param quantile Numeric; combined arithmetically in the body. Defaults to \code{0.5}.
#' @param updates Defaults to \code{200}.
#' @param seed Passed to \code{.prgrl_rng}. Defaults to \code{0}.
#' @param n_repeats Defaults to \code{50}.
#' @return A list with \code{estimate}, \code{easy_only_error}, \code{all_examples_error}, \code{improvement}, \code{n_kept}, \code{n}, \code{method}, \code{note}.
#' @export
easy_only_fit <- function(X, y, difficulty, X_test, y_test, quantile = 0.5,
                          updates = 200, seed = 0, n_repeats = 50) {
  Xr <- .prgrl_to_rows(X)
  yv <- .prgrl_to_vec(y)
  d <- .prgrl_to_vec(difficulty)
  n <- length(Xr)
  if (!(0.0 < quantile && quantile <= 1.0)) {
    stop("prgrl: quantile must lie in (0, 1]")
  }
  Xe <- .prgrl_to_rows(X_test)
  ye <- .prgrl_to_vec(y_test)
  order_idx <- order(d)
  keep <- order_idx[seq_len(max(1L, round(quantile * n)))]

  rnd <- .prgrl_rng(seed)
  easy_errs <- numeric(as.integer(n_repeats))
  all_errs <- numeric(as.integer(n_repeats))
  p_dim <- length(Xr[[1]])
  updates_int <- as.integer(updates)
  n_repeats_int <- as.integer(n_repeats)

  for (rep in seq_len(n_repeats_int)) {
    w0 <- numeric(p_dim)
    for (i in seq_len(p_dim)) w0[i] <- .prgrl_gauss(rnd)

    easy_errs[rep] <- .prgrl_error(Xe, ye,
                                   .prgrl_perceptron(Xr, yv, keep, updates_int, w0))

    allo <- seq_len(n)
    for (i in n:2) {
      j <- floor(rnd() * i) + 1L
      tmp <- allo[i]
      allo[i] <- allo[j]
      allo[j] <- tmp
    }
    all_errs[rep] <- .prgrl_error(Xe, ye,
                                  .prgrl_perceptron(Xr, yv, allo, updates_int, w0))
  }

  e_easy <- mean(easy_errs)
  e_all <- mean(all_errs)

  list(
    estimate = e_easy,
    easy_only_error = e_easy,
    all_examples_error = e_all,
    improvement = e_all - e_easy,
    n_kept = length(keep),
    n = n,
    method = "curriculum learning (Bengio et al. 2009), Section 4.1: train on the clean examples only",
    note = "the paper reports 16.3% against 17.1% for a linear SVM on two Gaussians; the direction is the claim, and noisy examples are the ones on the wrong side of the Bayes boundary"
  )
}

#' .prgrl_cheatsheet
#'
#' A step of the prgrl_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.prgrl_cheatsheet <- function() {
  "prgrl: curriculum learning (Bengio et al. 2009). Q_lambda(z) proportional to W_lambda(z) P(z) with W_1 = 1; it is a curriculum only if H(Q_lambda) increases and W_lambda(z) never falls as lambda grows (eqns 3-4), which is checkable and is checked. prgrl trains the same learner on the schedule and on the shuffled data and compares HELD-OUT error under a fixed update budget, which is Section 4.2's experiment; easy_only_fit is Section 4.1's."
}

prog_rl <- entropy
progrl <- entropy

morie_prgrl <- prgrl
