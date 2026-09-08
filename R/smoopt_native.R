# morie.fn -- function file (rootcoder007/morie)
# R arm of smoopt (error_cache, violates_kkt, outer_loop_schedule,
# second_choice, compute_threshold, smo_platt).
# Sources:
#   Platt, J. C. (1998) "Sequential Minimal Optimization: A Fast
#   Algorithm for Training Support Vector Machines", Microsoft
#   Research Technical Report MSR-TR-98-14. The decomposition to the
#   smallest possible optimisation problem (two Lagrange multipliers,
#   because the linear equality constraint forces them to move
#   together), solved analytically so that no numerical QP
#   optimisation is required; the outer loop alternating between
#   single passes over the entire training set and repeated passes
#   over the non-bound examples until all of them obey the KKT
#   conditions within tolerance; the second-choice heuristic
#   maximising |E_1 - E_2|; and the recomputation of b from the
#   resulting non-bound multipliers, taking the midpoint when both
#   are at a bound.
#   Chang, C.-C. & Lin, C.-J. (2011) "LIBSVM: A Library for Support
#   Vector Machines", ACM TIST 2(3), Article 27,
#   doi:10.1145/1961189.1961199. The maximal-violating-pair
#   selection kept as the alternative route; implemented in svmopt.
#   Cortes, C. & Vapnik, V. (1995) "Support-Vector Networks",
#   Machine Learning 20(3), 273-297, doi:10.1007/BF00994018. The dual
#   being solved.

.SMOOPT_EPS <- 1e-12

#' .smoopt_kvec
#'
#' A step of the smoopt_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x A matrix; passed to \code{dim}.
#' @return One of two values, depending on the branch taken.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .smoopt_kvec(x = x)
#' res
.smoopt_kvec <- function(x) if (is.null(dim(x))) as.numeric(x) else
  as.numeric(x)

#' .smoopt_make_rng
#'
#' A step of the smoopt_native implementation. Called by \code{smo_platt}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param seed Passed to \code{.ghc_rng}.
#' @return A list with \code{uniform}.
#' @export
#' @examples
#' res <- .smoopt_make_rng(seed = 1L)
#' res
.smoopt_make_rng <- function(seed) {
  e <- .ghc_rng(seed)
  list(uniform = function() .ghc_unif(e, 1L))
}

#' error_cache
#'
#' A step of the smoopt_native implementation. Called by \code{smo_platt}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param alpha Coerced to numeric by the body, with \code{as.numeric}.
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @param K A matrix; indexed by row and column.
#' @param b Coerced to numeric by the body, with \code{as.numeric}.
#' @return The value of \code{out}, as built in the body.
#' @export
error_cache <- function(alpha, y, K, b) {
  a <- as.numeric(alpha)
  yy <- as.numeric(y)
  n <- length(a)
  out <- numeric(n)
  for (i in seq_len(n)) {
    f <- sum(a * yy * K[i, ]) - as.numeric(b)
    out[i] <- f - yy[i]
  }
  out
}

#' violates_kkt
#'
#' A step of the smoopt_native implementation. Called by \code{smo_platt}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param i See Usage.
#' @param alpha A vector; indexed elementwise.
#' @param y A vector; indexed elementwise.
#' @param E A vector; indexed elementwise.
#' @param C Coerced to numeric by the body, with \code{as.numeric}.
#' @param tol Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.001}.
#' @return A logical value.
#' @export
violates_kkt <- function(i, alpha, y, E, C, tol = 1e-3) {
  a <- as.numeric(alpha[i])
  r <- as.numeric(y[i]) * as.numeric(E[i])
  (r < -as.numeric(tol) && a < as.numeric(C) - .SMOOPT_EPS) ||
    (r > as.numeric(tol) && a > .SMOOPT_EPS)
}

#' outer_loop_schedule
#'
#' A step of the smoopt_native implementation. Called by \code{smo_platt}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param alpha Coerced to numeric by the body, with \code{as.numeric}.
#' @param C Coerced to numeric by the body, with \code{as.numeric}.
#' @param examine_all A flag; the body branches on it.
#' @return A list with \code{indices}, \code{kind}, \code{n_non_bound}, \code{note}.
#' @export
outer_loop_schedule <- function(alpha, C, examine_all) {
  a <- as.numeric(alpha)
  if (isTRUE(examine_all)) {
    return(list(indices = seq_along(a), kind = "all",
                note = "a full sweep catches violators at a bound"))
  }
  nb <- which(a > .SMOOPT_EPS & a < as.numeric(C) - .SMOOPT_EPS)
  list(indices = as.integer(nb), kind = "non_bound",
       n_non_bound = length(nb),
       note = "the non-bound set is where the action is")
}

#' second_choice
#'
#' A step of the smoopt_native implementation. Called by \code{smo_platt}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param i1 Coerced to integer by the body, with \code{as.integer}.
#' @param alpha Coerced to numeric by the body, with \code{as.numeric}.
#' @param y Accepted by the signature and not used anywhere in the body.
#' @param E A vector; indexed elementwise.
#' @param C Coerced to numeric by the body, with \code{as.numeric}.
#' @param rng A list; the body reads \code{$uniform} from it.
#' @param tol Accepted by the signature and not used anywhere in the body. Defaults to \code{0.001}.
#' @return A list with \code{index}, \code{level}, \code{note}.
#' @export
second_choice <- function(i1, alpha, y, E, C, rng, tol = 1e-3) {
  a <- as.numeric(alpha)
  n <- length(a)
  nb <- which(a > .SMOOPT_EPS & a < as.numeric(C) - .SMOOPT_EPS)
  nb <- nb[nb != as.integer(i1)]
  if (length(nb) > 1L) {
    E1 <- as.numeric(E[as.integer(i1)])
    j <- nb[which.max(abs(E1 - as.numeric(E[nb])))]
    return(list(index = as.integer(j), level = 1L,
                gap = abs(E1 - as.numeric(E[j])),
                note = paste0("the analytic step is proportional to ",
                              "|E1 - E2|, so this maximises progress")))
  }
  start <- as.integer(rng$uniform() * max(n, 1L)) %% max(n, 1L)
  if (length(nb) > 0L) {
    j <- nb[(start %% length(nb)) + 1L]
    return(list(index = as.integer(j), level = 2L,
                note = "non-bound examples from a random position"))
  }
  for (t in seq_len(n)) {
    j <- (start + t - 1L) %% n + 1L
    if (j != as.integer(i1))
      return(list(index = as.integer(j), level = 3L,
                  note = "all examples from a random position"))
  }
  list(index = NULL, level = 4L,
       note = "no second index available; abandon this i1")
}

#' compute_threshold
#'
#' A step of the smoopt_native implementation. Called by \code{smo_platt}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param i1 Coerced to integer by the body, with \code{as.integer}.
#' @param i2 Coerced to integer by the body, with \code{as.integer}.
#' @param a1_new Coerced to numeric by the body, with \code{as.numeric}.
#' @param a2_new Coerced to numeric by the body, with \code{as.numeric}.
#' @param alpha Coerced to numeric by the body, with \code{as.numeric}.
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @param E A vector; indexed elementwise.
#' @param K A matrix; indexed by row and column.
#' @param b Coerced to numeric by the body, with \code{as.numeric}.
#' @param C Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{b}, \code{from}, \code{b1}, \code{b2}, \code{note}.
#' @export
compute_threshold <- function(i1, i2, a1_new, a2_new, alpha, y, E, K, b,
                              C) {
  yy <- as.numeric(y)
  a <- as.numeric(alpha)
  i <- as.integer(i1)
  j <- as.integer(i2)
  d1 <- as.numeric(a1_new) - a[i]
  d2 <- as.numeric(a2_new) - a[j]
  b1 <- as.numeric(b) + as.numeric(E[i]) +
    yy[i] * d1 * K[i, i] + yy[j] * d2 * K[i, j]
  b2 <- as.numeric(b) + as.numeric(E[j]) +
    yy[i] * d1 * K[i, j] + yy[j] * d2 * K[j, j]
  free1 <- as.numeric(a1_new) > .SMOOPT_EPS &&
    as.numeric(a1_new) < as.numeric(C) - .SMOOPT_EPS
  free2 <- as.numeric(a2_new) > .SMOOPT_EPS &&
    as.numeric(a2_new) < as.numeric(C) - .SMOOPT_EPS
  if (free1) return(list(b = b1, from = "i1", b1 = b1, b2 = b2))
  if (free2) return(list(b = b2, from = "i2", b1 = b1, b2 = b2))
  list(b = 0.5 * (b1 + b2), from = "midpoint", b1 = b1, b2 = b2,
       note = paste0("both at a bound, so any value between b1 and b2 ",
                     "satisfies KKT"))
}

#' smo_platt
#'
#' A step of the smoopt_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @param K A matrix; indexed by row and column.
#' @param C Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @param tol Passed to \code{violates_kkt}. Defaults to \code{0.001}.
#' @param eps Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1e-05}.
#' @param max_passes Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200}.
#' @param seed Passed to \code{.smoopt_make_rng}. Defaults to \code{0}.
#' @return A list with \code{estimate}, \code{alpha}, \code{b}, \code{passes},
#' \code{full_passes}, \code{non_bound_passes}, \code{steps}, \code{support_vectors},
#' \code{n_sv}, \code{equality_residual}, \code{kkt_violations}, \code{objective},
#' \code{method}, \code{note}.
#' @export
smo_platt <- function(y, K, C = 1.0, tol = 1e-3, eps = 1e-5,
                      max_passes = 200, seed = 0) {
  yy <- as.numeric(y)
  n <- length(yy)
  if (any(!(yy %in% c(-1.0, 1.0))))
    stop("smoopt: labels must be -1 or +1")
  if (as.numeric(C) <= 0.0)
    stop("smoopt: C must be positive")
  rng <- .smoopt_make_rng(seed)
  a <- rep(0.0, n)
  b <- 0.0
  examine_all <- TRUE
  passes <- 0L
  changed_total <- 0L
  full_passes <- 0L
  nb_passes <- 0L
  while (passes < as.integer(max_passes)) {
    passes <- passes + 1L
    E <- error_cache(a, yy, K, b)
    sched <- outer_loop_schedule(a, C, examine_all)
    if (sched$kind == "all") full_passes <- full_passes + 1L
    else nb_passes <- nb_passes + 1L
    changed <- 0L
    for (i1 in sched$indices) {
      if (!violates_kkt(i1, a, yy, E, C, tol)) next
      pick <- second_choice(i1, a, yy, E, C, rng, tol)
      i2 <- pick$index
      if (is.null(i2)) next
      bounds <- morie_svmopt$.svmopt_bounds(i1, i2, a, yy, C)
      L <- bounds$L
      H <- bounds$H
      if (H <= L + .SMOOPT_EPS) next
      eta <- K[i1, i1] + K[i2, i2] - 2.0 * K[i1, i2]
      if (eta <= .SMOOPT_EPS) next
      a2_new <- a[i2] + yy[i2] * (E[i1] - E[i2]) / eta
      a2_new <- min(max(a2_new, L), H)
      if (abs(a2_new - a[i2]) < as.numeric(eps) *
          (a2_new + a[i2] + as.numeric(eps))) next
      a1_new <- a[i1] - yy[i1] * yy[i2] * (a2_new - a[i2])
      th <- compute_threshold(i1, i2, a1_new, a2_new, a, yy, E, K, b, C)
      a[i1] <- a1_new
      a[i2] <- a2_new
      b <- th$b
      E <- error_cache(a, yy, K, b)
      changed <- changed + 1L
    }
    changed_total <- changed_total + changed
    if (isTRUE(examine_all)) {
      examine_all <- FALSE
    } else if (changed == 0L) {
      examine_all <- TRUE
      if (passes > 1L) {
        E <- error_cache(a, yy, K, b)
        viol <- vapply(seq_len(n), function(i)
          violates_kkt(i, a, yy, E, C, tol), logical(1))
        if (!any(viol)) break
      }
    }
  }
  E <- error_cache(a, yy, K, b)
  sv <- which(a > .SMOOPT_EPS)
  list(estimate = a, alpha = a, b = b, passes = passes,
       full_passes = full_passes, non_bound_passes = nb_passes,
       steps = changed_total, support_vectors = as.integer(sv),
       n_sv = length(sv),
       equality_residual = sum(a * yy),
       kkt_violations = sum(vapply(seq_len(n), function(i)
         violates_kkt(i, a, yy, E, C, tol), logical(1))),
       objective = morie_svmopt$dual_objective(a, yy, K),
       method = "SMO with Platt's heuristics; Platt (1998)",
       note = paste0("same dual as svmopt, different working-set rule ",
                     "-- Platt's needs only the non-bound errors; ",
                     "note b follows Platt's f = sum(a y K) - b, the ",
                     "NEGATIVE of the LIBSVM convention used in svmopt"))
}

#' .smoopt_cheatsheet
#'
#' A step of the smoopt_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
#' @examples
#' res <- .smoopt_cheatsheet()
#' res
.smoopt_cheatsheet <- function() {
  paste0("smoopt: same SVM dual as svmopt, different CHOICE of ",
         "pair. Two multipliers because the equality constraint ",
         "forces them to move together, and at two the QP is ",
         "analytic -- SMO calls NO inner QP solver. Outer loop ",
         "ALTERNATES: one full sweep, then repeated sweeps over the ",
         "NON-BOUND examples until they all satisfy KKT, then a ",
         "full sweep again -- bound examples rarely move, but ",
         "skipping them forever hides a violator sitting at a ",
         "bound. Inner heuristic maximises |E1 - E2|, since the ",
         "analytic step is proportional to it, with a fallback ",
         "hierarchy. b is RECOMPUTED each step, not accumulated.")
}

# ledger/NAMING.md compact alias
sequential_minimal_optimization <- smo_platt
smo_solver <- smo_platt
smosolver <- smo_platt

morie_smoopt <- list(error_cache = error_cache,
                     violates_kkt = violates_kkt,
                     outer_loop_schedule = outer_loop_schedule,
                     second_choice = second_choice,
                     compute_threshold = compute_threshold,
                     smo_platt = smo_platt,
                     cheatsheet = .smoopt_cheatsheet,
                     sequential_minimal_optimization =
                       sequential_minimal_optimization,
                     smo_solver = smo_solver,
                     smosolver = smosolver)
