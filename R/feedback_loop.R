# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Research P4: predictive-policing feedback loops (two-region model).
#
# A department keeps discovered-crime counts (cA, cB), sends its patrol to
# region A with share x = cA / (cA + cB), and discovers crime only where the
# patrol is. Feeding the discovered counts back into the allocation makes
# the allocation a function of the initial condition, not of the true
# rates (Ensign, Friedler, Neville, Scheidegger, Venkatasubramanian 2018).
#
# Every limit these functions report is a machine-checked theorem in
# research/lean/P4Feedback.lean and research/lean/P4Limit.lean (Lean 4 +
# Mathlib, 0 sorry, axioms propext / Classical.choice / Quot.sound only):
#
#   Research.P4.naive_step_drift        x' - x = x(1-x)(lamA-lamB)/(c + lamA x + lamB(1-x))
#   Research.P4.naive_share_increasing  lamA > lamB and 0 < x < 1  =>  x < x'
#   Research.P4.naiveShare_tendsto_one  lamA > lamB  =>  x_n -> 1 for every start
#   Research.P4.corrected_share_tendsto corrected update  =>  x_n -> lamA/(lamA+lamB)
#   Research.P4.naiveShare_rate_bound   x_N - x_0 <= (lamA-lamB)/4 * sum_{k<N} 1/(c0 + k min(lamA,lamB))
#   Research.P4.rho_cap                 (P4Mitigation.lean) with reporting share rho in [0,1],
#                                       x_n <= max(x_0, lamA / (lamA + rho lamB)) for every n
#   Research.P4.cap_zero, cap_one       the cap is 1 at rho = 0 and lamA/(lamA+lamB) at rho = 1
#   Research.P4.Urn.urn_step_martingale (P4Urn.lean) one discovery per step: the share is a martingale
#   Research.P4.Urn.polya_uniform       equal rates, one count each: A-discoveries after n draws are
#                                       uniform on 0..n, so the share is uniform on its grid
#   Research.P4.Urn.polya_no_concentration
#                                       P(1/4 <= share_n <= 3/4) >= 1/4 for every n >= 2
#
# What the theorems do NOT say: that a real department's discovery process
# is this model. The mean-field recursion is the expected-count dynamics;
# the urn simulator is its stochastic counterpart, and for equal rates its
# exact law is proved (P4Urn.lean): the share is a martingale that
# converges to a random limit, not to the mean-field constant.

#' Mean-field feedback-loop recursion for two regions
#'
#' Iterates the expected discovered-crime counts of two regions under a
#' patrol allocation proportional to those counts. Under the naive update
#' the region with the higher true rate absorbs the whole patrol whatever
#' the ratio of the rates; under the corrected update the allocation
#' converges to the true-rate proportion.
#'
#' The runaway is a limit statement, and the limit is approached
#' logarithmically: the one-step drift is
#' \eqn{x(1-x)(\lambda_A-\lambda_B)/c_n} with \eqn{c_n} growing linearly,
#' so the share moves like a harmonic sum. From a 10/10 start with rates
#' 0.3 and 0.2 the share reaches 0.89 after 20,000 steps and 0.97 after two
#' million; from a 1/99 start with rates 0.21 and 0.20 it moves from 0.010
#' to 0.015 in two million steps. A claim that a feedback loop "will"
#' concentrate patrols therefore has to state its horizon; the theorem
#' gives the destination, the recursion gives the speed.
#'
#' @param lam_a,lam_b True crime rates (per patrol visit) of regions A and
#'   B; positive.
#' @param c_a0,c_b0 Initial discovered-crime counts; positive.
#' @param n_steps Number of steps to iterate.
#' @param update \code{"naive"} (discovered counts fed back as they are) or
#'   \code{"corrected"} (each discovery discounted by the presence that
#'   produced it, so the increment is the true rate).
#' @param rho Share of crimes that reach the record through public reports
#'   independent of patrol presence, in \code{[0, 1]}. With \code{rho > 0}
#'   the naive share is proved never to exceed
#'   \eqn{\max(x_0, \lambda_A/(\lambda_A + \rho\lambda_B))}
#'   (\code{Research.P4.rho_cap}): reports cap the loop strictly below 1
#'   but strictly above the true-rate proportion unless \code{rho = 1}.
#' @return A data frame with \code{step}, \code{share_a} (share of patrol
#'   sent to A), \code{c_a}, \code{c_b}, and the attribute
#'   \code{"limit"} giving the proved limit of the share (see
#'   \code{\link{morie_feedback_loop_limit}}).
#' @references Ensign D, Friedler SA, Neville S, Scheidegger C,
#'   Venkatasubramanian S (2018). Runaway feedback loops in predictive
#'   policing. Proceedings of Machine Learning Research 81, 160-171.
#' @seealso \code{\link{morie_feedback_loop_sim}} for the stochastic urn,
#'   \code{\link{morie_feedback_loop_limit}} for the proved limits.
#' @examples
#' mf <- morie_feedback_loop_meanfield(0.3, 0.2, 10, 10, n_steps = 200)
#' tail(mf$share_a, 1)          # climbing towards 1, not towards 0.6
#' attr(mf, "limit")
#' cf <- morie_feedback_loop_meanfield(0.3, 0.2, 10, 10, n_steps = 200,
#'                                     update = "corrected")
#' tail(cf$share_a, 1)          # approaching 0.3 / 0.5 = 0.6
#' @export
morie_feedback_loop_meanfield <- function(lam_a, lam_b, c_a0, c_b0,
                                          n_steps = 100L,
                                          update = c("naive", "corrected"),
                                          rho = 0) {
  update <- match.arg(update)
  .morie_feedback_check(lam_a, lam_b, c_a0, c_b0, rho)
  n_steps <- as.integer(n_steps)
  if (is.na(n_steps) || n_steps < 0L) stop("n_steps must be a non-negative integer", call. = FALSE)
  c_a <- numeric(n_steps + 1L)
  c_b <- numeric(n_steps + 1L)
  c_a[1L] <- c_a0
  c_b[1L] <- c_b0
  p_report <- lam_a / (lam_a + lam_b)
  for (t in seq_len(n_steps)) {
    x <- c_a[t] / (c_a[t] + c_b[t])
    if (update == "naive") {
      inc_a <- (1 - rho) * lam_a * x
      inc_b <- (1 - rho) * lam_b * (1 - x)
    } else {
      inc_a <- (1 - rho) * lam_a
      inc_b <- (1 - rho) * lam_b
    }
    # public reports: expected count per step split by the true rates
    inc_a <- inc_a + rho * (lam_a + lam_b) * p_report
    inc_b <- inc_b + rho * (lam_a + lam_b) * (1 - p_report)
    c_a[t + 1L] <- c_a[t] + inc_a
    c_b[t + 1L] <- c_b[t] + inc_b
  }
  out <- data.frame(
    step = 0:n_steps,
    share_a = c_a / (c_a + c_b),
    c_a = c_a,
    c_b = c_b
  )
  attr(out, "limit") <- morie_feedback_loop_limit(lam_a, lam_b, c_a0, c_b0, update, rho)
  out
}

#' Proved limit of the two-region feedback loop
#'
#' Returns the long-run share of patrol sent to region A under the
#' mean-field recursion, with the name of the Lean theorem that proves it.
#' With \code{rho > 0} under the naive update the limit is not proved, but
#' the share is proved never to exceed the cap
#' \eqn{\max(x_0, \lambda_A/(\lambda_A + \rho\lambda_B))}
#' (\code{Research.P4.rho_cap}); \code{share_a} is then \code{NA} and
#' \code{cap} carries the bound.
#'
#' @inheritParams morie_feedback_loop_meanfield
#' @return A list with \code{share_a} (the limit, or \code{NA}),
#'   \code{theorem} (the Lean name, or \code{NA}), \code{note}, and for
#'   \code{rho > 0} also \code{cap} and \code{cap_theorem}.
#' @examples
#' morie_feedback_loop_limit(0.3, 0.2, 10, 10)                 # 1
#' morie_feedback_loop_limit(0.3, 0.2, 10, 10, "corrected")    # 0.6
#' morie_feedback_loop_limit(0.3, 0.3, 10, 10)                 # stays at 0.5
#' morie_feedback_loop_limit(0.3, 0.2, 10, 10, rho = 0.5)$cap # 0.3 / 0.4 = 0.75
#' @export
morie_feedback_loop_limit <- function(lam_a, lam_b, c_a0, c_b0,
                                      update = c("naive", "corrected"),
                                      rho = 0) {
  update <- match.arg(update)
  .morie_feedback_check(lam_a, lam_b, c_a0, c_b0, rho)
  if (rho > 0) {
    if (update == "corrected") {
      # the corrected increments are (1 - rho) lam + rho lam = lam: the
      # reporting share cancels and the closed form applies unchanged
      return(list(share_a = lam_a / (lam_a + lam_b),
                  theorem = "Research.P4.corrected_share_tendsto",
                  note = "corrected update: reports do not change the increments"))
    }
    return(list(share_a = NA_real_, theorem = NA_character_,
                note = "no limit theorem for rho > 0; the cap is proved",
                cap = max(c_a0 / (c_a0 + c_b0), lam_a / (lam_a + rho * lam_b)),
                cap_theorem = "Research.P4.rho_cap"))
  }
  if (update == "corrected") {
    return(list(share_a = lam_a / (lam_a + lam_b),
                theorem = "Research.P4.corrected_share_tendsto",
                note = "limit is the true-rate proportion, for every start"))
  }
  if (lam_a > lam_b) {
    list(share_a = 1, theorem = "Research.P4.naiveShare_tendsto_one",
         note = "runaway: the higher-rate region absorbs the whole patrol")
  } else if (lam_a < lam_b) {
    list(share_a = 0, theorem = "Research.P4.naiveShare_tendsto_one",
         note = "runaway (regions swapped): the higher-rate region absorbs the whole patrol")
  } else {
    list(share_a = c_a0 / (c_a0 + c_b0), theorem = "Research.P4.naive_step_drift",
         note = paste("equal rates: the mean-field drift is zero and the share never moves;",
                      "the stochastic urn instead converges to a random limit",
                      "(Research.P4.Urn.polya_uniform, see morie_feedback_loop_urn_law)"),
         stochastic_theorem = "Research.P4.Urn.polya_no_concentration")
  }
}

#' Exact law of the stochastic two-region urn with equal rates
#'
#' One discovery per step, credited to region A with probability equal to
#' the current share of A and adding one count there. The share is a
#' martingale for any reinforcement (\code{Research.P4.Urn.urn_step_martingale}).
#' Started from one count each, the number of discoveries credited to A
#' after \code{n_steps} draws is exactly uniform on \code{0:n_steps}
#' (\code{Research.P4.Urn.polya_uniform}), so the share
#' \eqn{(1 + j)/(n + 2)} is uniform on its grid and the probability that
#' it lies in \eqn{[1/4, 3/4]} is at least \eqn{1/4} at every horizon
#' \eqn{n \ge 2} (\code{Research.P4.Urn.polya_no_concentration}).
#'
#' The mean-field recursion with equal rates keeps the share at one half
#' forever; the stochastic process locks in early luck and converges to a
#' random limit that is uniform on (0, 1). "Equal rates, therefore no
#' runaway" is false for the process a department actually runs.
#'
#' @param n_steps Number of draws (non-negative integer).
#' @return A data frame with \code{j} (discoveries credited to A),
#'   \code{share_a} and \code{prob}, with attributes \code{"prob_middle"}
#'   (probability that the share lies in \eqn{[1/4, 3/4]}) and
#'   \code{"theorems"}.
#' @seealso \code{\link{morie_feedback_loop_sim}} for the simulated urn with
#'   unequal rates.
#' @examples
#' law <- morie_feedback_loop_urn_law(10)
#' law$prob                       # all 1/11
#' attr(law, "prob_middle")       # at least 1/4
#' @export
morie_feedback_loop_urn_law <- function(n_steps) {
  n_steps <- as.integer(n_steps)
  if (length(n_steps) != 1L || is.na(n_steps) || n_steps < 0L) stop("n_steps must be a non-negative integer", call. = FALSE)
  j <- 0:n_steps
  out <- data.frame(j = j, share_a = (1 + j) / (n_steps + 2), prob = rep(1 / (n_steps + 1), n_steps + 1L))
  attr(out, "prob_middle") <- sum(out$prob[out$share_a >= 0.25 & out$share_a <= 0.75])
  attr(out, "theorems") <- c("Research.P4.Urn.urn_step_martingale", "Research.P4.Urn.polya_uniform",
                             "Research.P4.Urn.polya_no_concentration")
  out
}

#' Stochastic urn simulation of the two-region feedback loop
#'
#' Simulates the patrol-and-discovery process step by step (C++ kernel).
#' Each step the patrol visits A with probability equal to the current
#' share and discovers a crime there with probability equal to that
#' region's true rate; the naive update adds the discovery to the count,
#' the corrected update adds it weighted by one over the presence that
#' produced it. Public reports, when \code{rho > 0}, arrive from a region
#' in proportion to the true rates regardless of presence.
#'
#' @inheritParams morie_feedback_loop_meanfield
#' @param n_sims Number of independent runs.
#' @param seed Philox seed (non-negative integer). Run \code{i} draws its
#'   uniforms from stream \code{i - 1} of \code{.morie_random_uniform}, three
#'   per step when \code{rho > 0} and two otherwise, so the R and Python arms
#'   produce identical paths for the same seed.
#' @return A list with \code{share_a} (an \code{n_sims} by
#'   \code{n_steps + 1} matrix of share paths), \code{final} (the final
#'   shares) and \code{limit} (the proved mean-field limit).
#' @examples
#' s <- morie_feedback_loop_sim(0.3, 0.2, 10, 10, n_steps = 2000, n_sims = 20, seed = 1)
#' stats::median(s$final)     # near 1
#' s$limit$theorem
#' @export
morie_feedback_loop_sim <- function(lam_a, lam_b, c_a0, c_b0, n_steps = 1000L,
                                    update = c("naive", "corrected"), rho = 0,
                                    n_sims = 100L, seed = 0) {
  update <- match.arg(update)
  .morie_feedback_check(lam_a, lam_b, c_a0, c_b0, rho)
  if (!(lam_a <= 1 && lam_b <= 1)) {
    stop("the urn simulator needs per-visit discovery probabilities: lam_a, lam_b <= 1",
         call. = FALSE)
  }
  n_steps <- as.integer(n_steps)
  n_sims <- as.integer(n_sims)
  if (is.na(n_sims) || n_sims < 1L) stop("n_sims must be a positive integer", call. = FALSE)
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) || seed < 0) {
    stop("seed must be a single non-negative number", call. = FALSE)
  }
  k <- if (rho > 0) 3L else 2L
  paths <- matrix(NA_real_, nrow = n_sims, ncol = n_steps + 1L)
  for (i in seq_len(n_sims)) {
    u <- .morie_random_uniform(k * n_steps, seed = seed, stream = i - 1L)
    r <- .morie_feedback_urn_cpp(lam_a, lam_b, c_a0, c_b0, n_steps,
                                 if (update == "corrected") 1L else 0L, rho, u)
    paths[i, ] <- r$share
  }
  list(
    share_a = paths,
    final = paths[, n_steps + 1L],
    limit = morie_feedback_loop_limit(lam_a, lam_b, c_a0, c_b0, update, rho)
  )
}

#' Proved upper bound on how far the naive loop can move in N steps
#'
#' The one-step gain of the naive recursion is at most
#' \eqn{(\lambda_A-\lambda_B)/4} divided by the current total count, and
#' the total count grows by at least \eqn{\min(\lambda_A,\lambda_B)} per
#' step, so after \eqn{N} steps the share has moved by at most
#' \deqn{\frac{\lambda_A-\lambda_B}{4} \sum_{k<N} \frac{1}{c_0 + k\min(\lambda_A,\lambda_B)},}
#' a harmonic sum that grows like \eqn{\log N}
#' (\code{Research.P4.naiveShare_rate_bound}). Use it to state the
#' horizon over which a feedback loop can matter: if the bound is small,
#' the loop cannot have moved the allocation, whatever the limit says.
#'
#' @inheritParams morie_feedback_loop_meanfield
#' @param n_steps Horizon \eqn{N}.
#' @return A list with \code{bound} (the maximum possible increase of the
#'   share to A over \code{n_steps} steps, for \code{lam_a > lam_b}),
#'   \code{share_a_max} (\code{c_a0/(c_a0+c_b0) + bound}, capped at 1) and
#'   \code{theorem}.
#' @examples
#' morie_feedback_loop_bound(0.21, 0.20, 1, 99, n_steps = 2e6)$bound   # about 0.10
#' morie_feedback_loop_bound(0.3, 0.2, 10, 10, n_steps = 2e4)$bound
#' @export
morie_feedback_loop_bound <- function(lam_a, lam_b, c_a0, c_b0, n_steps = 1000L) {
  .morie_feedback_check(lam_a, lam_b, c_a0, c_b0, 0)
  if (lam_a <= lam_b) {
    stop("the bound is stated for lam_a > lam_b; swap the regions otherwise", call. = FALSE)
  }
  n_steps <- as.integer(n_steps)
  if (is.na(n_steps) || n_steps < 0L) stop("n_steps must be a non-negative integer", call. = FALSE)
  k <- seq_len(n_steps) - 1
  bound <- (lam_a - lam_b) / 4 * sum(1 / (c_a0 + c_b0 + k * min(lam_a, lam_b)))
  x0 <- c_a0 / (c_a0 + c_b0)
  list(bound = bound, share_a_max = min(1, x0 + bound),
       theorem = "Research.P4.naiveShare_rate_bound")
}

#' Internal: argument checks shared by the feedback-loop functions
#' @noRd
.morie_feedback_check <- function(lam_a, lam_b, c_a0, c_b0, rho) {
  for (nm in c("lam_a", "lam_b", "c_a0", "c_b0", "rho")) {
    v <- get(nm)
    if (!is.numeric(v) || length(v) != 1L || is.na(v)) {
      stop(nm, " must be a single non-missing number", call. = FALSE)
    }
  }
  if (lam_a <= 0 || lam_b <= 0) stop("lam_a and lam_b must be positive", call. = FALSE)
  if (c_a0 <= 0 || c_b0 <= 0) stop("c_a0 and c_b0 must be positive", call. = FALSE)
  if (rho < 0 || rho > 1) stop("rho must lie in [0, 1]", call. = FALSE)
  invisible(TRUE)
}
