# Simulated annealing.
#
# Kirkpatrick, S., Gelatt, C. D., & Vecchi, M. P. (1983) "Optimization by
# Simulated Annealing", Science 220(4598), 671-680.
#
# The Metropolis criterion (their Sec. "Simulated Annealing", after
# Metropolis et al. 1953) accepts a proposed move by
#
#   P(accept) = 1                       if Delta E <= 0
#               exp(-Delta E / T)       if Delta E > 0
#
# so uphill moves are taken with a probability that falls as the
# temperature does. The whole method is that schedule: at high T the
# walk is nearly free and explores; as T -> 0 it becomes greedy descent
# and settles. Kirkpatrick's point is that cooling slowly enough leaves
# the system in a near-ground state rather than the first local minimum
# found, which is what plain descent gives.
#
# Routes
# ------
# schedule selects the cooling law, all three in common use and the
# first two named in the paper's discussion:
#
#   "geometric":    T_k = T_0 * alpha^k, the standard choice; alpha near 1
#                   cools slowly.
#   "linear":       T_k = T_0 * (1 - k/K).
#   "logarithmic":  T_k = T_0 / ln(k + e), the schedule for which
#                   convergence in probability to the global optimum can
#                   be proved (Geman & Geman 1984); it is impractically
#                   slow, and is offered for exactly that reason -- it
#                   is the one with the guarantee.
#
# Determinism: proposals and acceptances come from the shared RNG, so a
# given seed reproduces the whole trajectory in both language arms.

.sa_opt_schedules <- c("geometric", "linear", "logarithmic")

#' .sa_opt_temperature
#'
#' A step of the sa_opt_native implementation. Called by \code{morie_sa_opt}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param schedule One of \code{"geometric"}, \code{"linear"}.
#' @param T0 Numeric; combined arithmetically in the body.
#' @param k Numeric; combined arithmetically in the body.
#' @param n_iter Coerced to numeric by the body, with \code{as.numeric}.
#' @param alpha Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.sa_opt_temperature <- function(schedule, T0, k, n_iter, alpha) {
  if (schedule == "geometric") {
    return(T0 * (alpha ^ k))
  }
  if (schedule == "linear") {
    frac <- 1.0 - (k / as.numeric(n_iter))
    if (frac > 0.0) return(T0 * frac) else return(0.0)
  }
  return(T0 / log(k + exp(1)))
}

#' morie_sa_opt
#'
#' A step of the sa_opt_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param fun Accepted by the signature and not used anywhere in the body.
#' @param x0 Coerced to numeric by the body, with \code{as.numeric}.
#' @param step Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @param T0 Passed to \code{.sa_opt_temperature}. Defaults to \code{1}.
#' @param n_iter A count; the body uses it as \code{seq_len(...)}. Defaults to \code{1000}.
#' @param schedule Coerced to character by the body, with \code{as.character}. Defaults to \code{"geometric"}.
#' @param alpha Passed to \code{.sa_opt_temperature}. Defaults to \code{0.99}.
#' @param lower Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param upper Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param seed Passed to \code{.ghc_rng}. Defaults to \code{0}.
#' @return The value of \code{result}, as built in the body.
#' @export
morie_sa_opt <- function(fun, x0, step=1.0, T0=1.0, n_iter=1000,
                         schedule="geometric", alpha=0.99, lower=NULL,
                         upper=NULL, seed=0) {
  x <- as.numeric(x0)
  if (length(x) == 0L) {
    stop("simulated_annealing: x0 must be non-empty")
  }
  n <- length(x)

  sched <- tolower(as.character(schedule))
  if (!(sched %in% .sa_opt_schedules)) {
    stop(sprintf("simulated_annealing: schedule must be one of %s, got %s",
                 paste(.sa_opt_schedules, collapse=", "), schedule))
  }

  T0 <- as.numeric(T0)
  if (T0 <= 0.0) {
    stop(sprintf("simulated_annealing: T0 must be positive, got %s", T0))
  }

  n_iter <- as.integer(n_iter)
  if (n_iter < 1L) {
    stop("simulated_annealing: n_iter must be at least 1")
  }

  alpha <- as.numeric(alpha)
  if (alpha <= 0.0 || alpha > 1.0) {
    stop(sprintf("simulated_annealing: alpha must lie in (0, 1], got %s", alpha))
  }

  lo <- if (is.null(lower)) NULL else as.numeric(lower)
  hi <- if (is.null(upper)) NULL else as.numeric(upper)

  rng <- .ghc_rng(seed)
  f <- as.numeric(fun(x))
  best_x <- x
  best_f <- f
  n_acc <- 0L
  n_up <- 0L
  temps <- numeric(n_iter)
  trace <- numeric(n_iter + 1L)
  trace[1L] <- f

  for (k in seq_len(n_iter)) {
    T <- .sa_opt_temperature(sched, T0, k, n_iter, alpha)
    temps[k] <- T

    u_gauss <- .ghc_unif(rng, n)
    gauss <- qnorm(u_gauss)
    prop <- x + as.numeric(step) * gauss

    if (!is.null(lo)) prop <- pmax(prop, lo)
    if (!is.null(hi)) prop <- pmin(prop, hi)

    fp <- as.numeric(fun(prop))
    dE <- fp - f

    accept <- FALSE
    if (dE <= 0.0) {
      accept <- TRUE
    } else if (T <= 0.0) {
      accept <- FALSE
    } else {
      u_accept <- .ghc_unif(rng, 1L)
      if (u_accept < exp(-dE / T)) {
        accept <- TRUE
        n_up <- n_up + 1L
      }
    }

    if (accept) {
      x <- prop
      f <- fp
      n_acc <- n_acc + 1L
      if (f < best_f) {
        best_x <- x
        best_f <- f
      }
    }
    trace[k + 1L] <- f
  }

  result <- list(
    estimate = best_x,
    x = best_x,
    fun = as.numeric(best_f),
    final_x = x,
    final_fun = as.numeric(f),
    n_accepted = as.integer(n_acc),
    n_uphill_accepted = as.integer(n_up),
    acceptance_rate = as.numeric(n_acc) / as.numeric(n_iter),
    temperatures = temps,
    trace = trace,
    schedule = sched,
    T0 = T0,
    n_iter = as.integer(n_iter),
    method = "Simulated annealing, Metropolis acceptance (Kirkpatrick, Gelatt & Vecchi 1983)"
  )

  result
}

#' .sa_opt_cheatsheet
#'
#' A step of the sa_opt_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.sa_opt_cheatsheet <- function() {
  paste("sa_opt: Metropolis accept exp(-dE/T) for dE>0, always for dE<=0;",
        "schedules geometric T0 a^k, linear, logarithmic T0/ln(k+e);",
        "returns the BEST point visited, not the last.")
}
