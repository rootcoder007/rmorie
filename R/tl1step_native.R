# morie.fn -- function file (rootcoder007/morie)
# R translation of tl1step: One-step TMLE by a universal least favorable submodel.
#
# The ordinary TMLE fluctuates along a local least favorable submodel
# and iterates: update, recompute the clever covariate, update again,
# until the efficient score equation is solved. Each iteration is a
# fresh maximum likelihood step, and when the data carry sparse
# information about the target, that iteration is where the estimator
# becomes unstable.
#
# The fix is a submodel that is least favorable everywhere, not just
# locally. A submodel {P_eps} is universal least favorable when its
# score at every eps (not only at zero) equals the canonical gradient
# at the current point. Then a single move along it, of the length
# that solves the score equation, is enough: no iteration, and the
# path taken is the shortest one achieving the required bias reduction.
# The estimator is psi_n* = Psi(P_n^1), one step.
#
# Why it is more stable, concretely: the iterative TMLE recomputes
# its direction after each update and can overshoot in the sparse case;
# the universal submodel's direction is defined by the gradient at
# wherever it currently is, so following it is an integral rather than
# a sequence of jumps. The construction is a differential equation,
# solved here by small steps whose limit is the path, and build_ulfm
# reports the step count so the discretisation is visible rather than
# implied.
#
# It generalises without changing shape: the same construction handles
# a multivariate target parameter, and even an infinite-dimensional one
# such as a complete treatment-specific survival curve, because the
# submodel is characterised by the canonical gradient rather than by a
# finite parametrisation.
#
# References
# ----------
# van der Laan, M. J. & Rose, S. (2018) *Targeted Learning in Data
# Science*, Springer, doi:10.1007/978-3-319-65304-4. Chap. 5
# (one-dimensional universal least favorable parametric submodels for
# univariate, multivariate and infinite-dimensional target parameters;
# the definition by which the score at every epsilon equals the
# canonical gradient; the resulting one-step TMLE solving the efficient
# influence curve equation without iteration; the reading of the
# universal submodel as a shortest path achieving the desired bias
# reduction; the argument that the iterative TMLE can be unstable when
# the data provide sparse information about the target; and the worked
# treatment-specific survival example).
#
# van der Laan, M. J. & Gruber, S. (2016) "One-step targeted minimum
# loss-based estimation based on universal least favorable
# one-dimensional submodels", *International Journal of Biostatistics*
# 12(1), 351-378, doi:10.1515/ijb-2015-0054. The construction this
# chapter relies on.

# ---- private helpers (prefixed .tl1step_ to avoid collisions) --------------

#' .tl1step_logit
#'
#' A step of the tl1step_native implementation. Called by \code{.tl1step_build_ulfm},
#' \code{.tl1step_is_universal}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param p Coerced to numeric by the body, with \code{as.numeric}.
#' @return A numeric value.
#' @export
#' @examples
#' res <- .tl1step_logit(p = 0.5)
#' res
.tl1step_logit <- function(p) {
  q <- min(max(as.numeric(p), 1e-9), 1 - 1e-9)
  log(q / (1 - q))
}

#' .tl1step_expit
#'
#' A step of the tl1step_native implementation. Called by \code{.tl1step_build_ulfm},
#' \code{.tl1step_is_universal}, \code{.tl1step_iterative_tmle}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @return One of two values, depending on the branch taken.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .tl1step_expit(x = x)
#' res
.tl1step_expit <- function(x) {
  # vectorised clamp: the scalar if() errors on any vector input
  xc <- pmax(x, -700)
  1 / (1 + exp(-xc))
}

#' .tl1step_as_numvec
#'
#' A step of the tl1step_native implementation. Called by \code{.tl1step_build_ulfm},
#' \code{.tl1step_is_universal}, \code{.tl1step_iterative_tmle} and 1 others in the
#' module.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x Passed to \code{unlist}.
#' @return A vector, from \code{as.numeric}.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .tl1step_as_numvec(x = x)
#' res
.tl1step_as_numvec <- function(x) {
  as.numeric(unlist(x))
}

# ---- build_ulfm ------------------------------------------------------------

# Integrate the universal least favorable path. The direction is
# recomputed continuously from the current point (H_fn(Q_current))
# rather than fixed at the start, which is what makes the submodel
# universal rather than local.
#' Integrate the universal least favorable path. The direction is
#'
#' recomputed continuously from the current point (H_fn(Q_current))
#' rather than fixed at the start, which is what makes the submodel
#' universal rather than local.
#'
#' @param Q Passed to \code{.tl1step_as_numvec}.
#' @param H_fn Accepted by the signature and not used anywhere in the body.
#' @param Y Passed to \code{.tl1step_as_numvec}.
#' @param eps_max Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{2}.
#' @param steps Coerced to integer by the body, with \code{as.integer}. Defaults to \code{400}.
#' @return A list with \code{path}, \code{steps}, \code{d_epsilon}, \code{note}.
#' @export
.tl1step_build_ulfm <- function(Q, H_fn, Y, eps_max = 2.0, steps = 400) {
  q <- .tl1step_as_numvec(Q)
  y <- .tl1step_as_numvec(Y)
  n <- length(q)
  if (length(y) != n) {
    stop(sprintf("tl1step: %d fits but %d outcomes", n, length(y)))
  }
  steps_i <- as.integer(steps)
  if (steps_i < 1L) stop("tl1step: steps must be at least 1")
  de <- as.numeric(eps_max) / steps_i

  # integrate in BOTH directions: the score at epsilon = 0 may have
  # either sign, and a forward-only path cannot reach the solution
  # when it is negative.
  total <- 2L * steps_i + 1L
  path <- vector("list", total)
  path[[1L]] <- list(eps = 0.0, q = q)
  idx <- 2L

  for (sgn in c(1.0, -1.0)) {
    cur <- q
    for (s in seq_len(steps_i)) {
      h <- .tl1step_as_numvec(H_fn(cur))
      cur <- vapply(seq_len(n), function(i) {
        .tl1step_expit(.tl1step_logit(cur[i]) + sgn * de * h[i])
      }, numeric(1))
      path[[idx]] <- list(eps = sgn * s * de, q = cur)
      idx <- idx + 1L
    }
  }

  # sort by eps
  eps_vals <- vapply(path, function(p) p$eps, numeric(1))
  ord <- order(eps_vals)
  path <- path[ord]

  list(
    path = path,
    steps = steps_i,
    d_epsilon = de,
    note = "the direction is recomputed at every point, so the submodel is least favorable EVERYWHERE, not only at epsilon = 0"
  )
}

# ---- is_universal ----------------------------------------------------------

# Check the defining property: score = gradient at every epsilon.
# Evaluated away from zero, because at zero a local least favorable
# submodel satisfies it too, so testing only there cannot distinguish
# the two.
#' Check the defining property: score = gradient at every epsilon
#'
#' Evaluated away from zero, because at zero a local least favorable
#' submodel satisfies it too, so testing only there cannot distinguish
#' the two.
#'
#' @param Q Passed to \code{.tl1step_as_numvec}.
#' @param H_fn Accepted by the signature and not used anywhere in the body.
#' @param eps Numeric; combined arithmetically in the body. Defaults to \code{0.3}.
#' @param h Numeric; combined arithmetically in the body. Defaults to \code{1e-05}.
#' @return A list with \code{max_deviation}, \code{universal}, \code{epsilon},
#' \code{local_submodel_direction_drift}, \code{note}.
#' @export
.tl1step_is_universal <- function(Q, H_fn, eps = 0.3, h = 1e-5) {
  q <- .tl1step_as_numvec(Q)
  n <- length(q)

  move <- function(e, direction_at_start = FALSE) {
    cur <- q
    steps_n <- 2000L
    de <- e / steps_n
    for (k_ in seq_len(steps_n)) {
      d <- if (direction_at_start) {
        .tl1step_as_numvec(H_fn(q))
      } else {
        .tl1step_as_numvec(H_fn(cur))
      }
      cur <- vapply(seq_len(n), function(i) {
        .tl1step_expit(.tl1step_logit(cur[i]) + de * d[i])
      }, numeric(1))
    }
    cur
  }

  at <- move(eps)
  fwd <- move(eps + h)
  num <- (fwd - at) / h
  grad <- .tl1step_as_numvec(H_fn(at))
  ana <- grad * at * (1 - at)
  dev <- max(abs(num - ana))

  local <- move(eps, TRUE)
  lg <- .tl1step_as_numvec(H_fn(local))
  ldev <- max(abs(lg - grad))

  list(
    max_deviation = dev,
    universal = dev < 1e-3,
    epsilon = eps,
    local_submodel_direction_drift = ldev,
    note = "a LOCAL submodel keeps the direction it had at epsilon = 0, so its score no longer equals the gradient once it has moved"
  )
}

# ---- one_step_tmle ---------------------------------------------------------

# Move once along the universal path to where the score vanishes.
#' Move once along the universal path to where the score vanishes
#'
#' A step of the tl1step_native implementation. Called by \code{morie_tl1step}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param Q Passed to \code{.tl1step_build_ulfm}.
#' @param H_fn Passed to \code{.tl1step_build_ulfm}.
#' @param Y Passed to \code{.tl1step_build_ulfm}.
#' @param eps_max Passed to \code{.tl1step_build_ulfm}. Defaults to \code{3}.
#' @param steps Passed to \code{.tl1step_build_ulfm}. Defaults to \code{600}.
#' @return A list with \code{estimate}, \code{psi}, \code{epsilon}, \code{Q_star},
#' \code{abs_score}, \code{iterations}, \code{path_steps}, \code{method}, \code{note}.
#' @export
.tl1step_one_step_tmle <- function(Q, H_fn, Y, eps_max = 3.0, steps = 600) {
  b <- .tl1step_build_ulfm(Q, H_fn, Y, eps_max, steps)
  y <- .tl1step_as_numvec(Y)
  n <- length(y)

  best <- Inf
  chosen_eps <- b$path[[1L]]$eps
  chosen_q <- b$path[[1L]]$q

  for (i in seq_along(b$path)) {
    entry <- b$path[[i]]
    cur <- entry$q
    e_val <- entry$eps
    h <- .tl1step_as_numvec(H_fn(cur))
    sc <- abs(sum(h * (y - cur)) / n)
    if (sc < best) {
      best <- sc
      chosen_eps <- e_val
      chosen_q <- cur
    }
  }

  list(
    estimate = sum(chosen_q) / n,
    psi = sum(chosen_q) / n,
    epsilon = chosen_eps,
    Q_star = chosen_q,
    abs_score = best,
    iterations = 1,
    path_steps = b$steps,
    method = "one-step TMLE along a universal least favorable submodel; van der Laan & Rose (2018) Chap. 5",
    note = "no iteration: one move along the shortest path that achieves the required bias reduction"
  )
}

# ---- iterative_tmle --------------------------------------------------------

# The ordinary iterative TMLE, for comparison. The direction is frozen
# within each iteration and recomputed between them -- a sequence of
# jumps rather than a path.
#' The ordinary iterative TMLE, for comparison. The direction is frozen
#'
#' within each iteration and recomputed between them -- a sequence of
#' jumps rather than a path.
#'
#' @param Q Passed to \code{.tl1step_as_numvec}.
#' @param H_fn Accepted by the signature and not used anywhere in the body.
#' @param Y Passed to \code{.tl1step_as_numvec}.
#' @param max_iter Coerced to integer by the body, with \code{as.integer}. Defaults to \code{25}.
#' @param tol Passed to \code{<}. Defaults to \code{1e-08}.
#' @return A list with \code{estimate}, \code{psi}, \code{iterations}, \code{Q_star},
#' \code{abs_score}, \code{method}.
#' @export
.tl1step_iterative_tmle <- function(Q, H_fn, Y, max_iter = 25, tol = 1e-8) {
  q <- .tl1step_as_numvec(Q)
  y <- .tl1step_as_numvec(Y)
  n <- length(q)
  cur <- q
  it <- 0L
  for (it in seq_len(as.integer(max_iter))) {
    h <- .tl1step_as_numvec(H_fn(cur))
    off <- vapply(cur, .tl1step_logit, numeric(1))
    e <- 0.0
    for (k_ in seq_len(50L)) {
      p <- vapply(seq_len(n), function(i) {
        .tl1step_expit(off[i] + e * h[i])
      }, numeric(1))
      gr <- sum(h * (y - p))
      he <- sum(h * h * p * (1 - p))
      if (he < 1e-12) break
      e <- e + gr / he
    }
    cur <- vapply(seq_len(n), function(i) {
      .tl1step_expit(off[i] + e * h[i])
    }, numeric(1))
    sc <- abs(sum(h * (y - cur)) / n)
    if (sc < tol) break
  }
  h <- .tl1step_as_numvec(H_fn(cur))
  list(
    estimate = sum(cur) / n,
    psi = sum(cur) / n,
    iterations = as.integer(it),
    Q_star = cur,
    abs_score = abs(sum(h * (y - cur)) / n),
    method = "iterative TMLE along a local least favorable submodel"
  )
}

# ---- cheatsheet ------------------------------------------------------------

#' .tl1step_cheatsheet
#'
#' A step of the tl1step_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @return A character value.
#' @export
#' @examples
#' res <- .tl1step_cheatsheet()
#' res
.tl1step_cheatsheet <- function() {
  paste0("tl1step: an ordinary TMLE fluctuates along a LOCAL least ",
         "favorable submodel and ITERATES, which is where it becomes ",
         "unstable when the data are sparse for the target. A UNIVERSAL ",
         "least favorable submodel has score = canonical gradient at ",
         "EVERY epsilon, not only at 0, so one move solves the efficient ",
         "score equation -- an integral rather than a sequence of jumps, ",
         "and the shortest path achieving the required bias reduction. The ",
         "construction is characterised by the gradient, so it extends to ",
         "multivariate and infinite-dimensional targets.")
}

# ---- compact alias (per ledger/NAMING.md) ----------------------------------

.tl1step_onesteptmle <- .tl1step_one_step_tmle

# ---- main entry point: morie_<module> -------------------------------------

#' morie_tl1step
#'
#' A step of the tl1step_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param Q Passed to \code{.tl1step_one_step_tmle}.
#' @param H_fn Passed to \code{.tl1step_one_step_tmle}.
#' @param Y Passed to \code{.tl1step_one_step_tmle}.
#' @param eps_max Passed to \code{.tl1step_one_step_tmle}. Defaults to \code{3}.
#' @param steps Passed to \code{.tl1step_one_step_tmle}. Defaults to \code{600}.
#' @return The value of \code{.tl1step_one_step_tmle}.
#' @export
morie_tl1step <- function(Q, H_fn, Y, eps_max = 3.0, steps = 600) {
  .tl1step_one_step_tmle(Q, H_fn, Y, eps_max, steps)
}
