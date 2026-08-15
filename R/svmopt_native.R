# morie.fn -- function file (rootcoder007/morie)
# R arm of svmopt (kernel_matrix, dual_objective, solve_pair,
# kkt_violation, smo, recover_bias).
# Sources:
#   Chang, C.-C. & Lin, C.-J. (2011) "LIBSVM: A Library for Support
#   Vector Machines", ACM TIST 2(3), Article 27,
#   doi:10.1145/1961189.1961199. The dual formulation with the box
#   and equality constraints; the decomposition method working on
#   two variables at a time because the density of the Hessian makes
#   the full problem infeasible in memory; working-set selection by
#   the maximal violating pair from the sets I_up and I_low; the
#   stopping condition expressed as the gap between the two extreme
#   gradient terms; and the recovery of b from the free support
#   vectors.
#   Cortes, C. & Vapnik, V. (1995) "Support-Vector Networks",
#   Machine Learning 20(3), 273-297, doi:10.1007/BF00994018. The
#   soft-margin formulation, its dual, and the support vectors.
#   Platt, J. C. (1998) "Sequential Minimal Optimization: A Fast
#   Algorithm for Training Support Vector Machines", Microsoft
#   Research Technical Report MSR-TR-98-14. The two-variable
#   decomposition and the analytic solution that lets SMO skip the
#   inner QP solver.
#   Boyd, S. & Vandenberghe, L. (2004) Convex Optimization,
#   Cambridge University Press, doi:10.1017/CBO9780511804441. The
#   Lagrange dual and the KKT conditions.

.SVMOPT_EPS <- 1e-12
.SVMOPT_TAU <- 1e-12

kernel_matrix <- function(X, kernel = "linear", gamma = 1.0, degree = 3,
                          coef0 = 0.0) {
  M <- as.matrix(X)
  n <- nrow(M)
  kf <- function(a, b) {
    d <- sum(a * b)
    if (kernel == "linear") return(d)
    if (kernel == "poly")
      return((as.numeric(gamma) * d + as.numeric(coef0)) ^
               as.integer(degree))
    if (kernel == "rbf") {
      s <- sum((a - b) ^ 2)
      return(exp(-as.numeric(gamma) * s))
    }
    stop(sprintf("svmopt: kernel must be linear, poly or rbf, got %r",
                 kernel))
  }
  K <- matrix(0, n, n)
  for (i in seq_len(n))
    for (j in seq_len(n))
      K[i, j] <- kf(M[i, ], M[j, ])
  K
}

dual_objective <- function(alpha, y, K) {
  a <- as.numeric(alpha); yy <- as.numeric(y)
  n <- length(a)
  q <- 0.0
  for (i in seq_len(n))
    if (a[i] != 0.0)
      for (j in seq_len(n))
        q <- q + a[i] * a[j] * yy[i] * yy[j] * K[i, j]
  sum(a) - 0.5 * q
}

.bounds <- function(i, j, a, y, C) {
  if (y[i] != y[j]) {
    L <- max(0.0, a[j] - a[i])
    Hh <- min(C, C + a[j] - a[i])
  } else {
    L <- max(0.0, a[i] + a[j] - C)
    Hh <- min(C, a[i] + a[j])
  }
  list(L = L, H = Hh)
}

solve_pair <- function(i, j, alpha, y, K, grad, C) {
  a <- as.numeric(alpha)
  if (i == j)
    stop("svmopt: the working set must contain two DIFFERENT indices")
  bnd <- .bounds(i, j, a, as.numeric(y), C)
  L <- bnd$L; Hh <- bnd$H
  if (Hh <= L + .SVMOPT_EPS)
    return(list(alpha = a, moved = 0.0, clipped = TRUE,
                L = L, H = Hh,
                note = "the box leaves no room for this pair"))
  eta <- K[i, i] + K[j, j] - 2.0 * as.numeric(y[i]) *
    as.numeric(y[j]) * K[i, j]
  if (eta <= .SVMOPT_TAU) eta <- .SVMOPT_TAU
  step <- ((-as.numeric(y[i]) * grad[i]) -
             (-as.numeric(y[j]) * grad[j])) / eta
  aj_new <- a[j] - as.numeric(y[j]) * step
  aj_cl <- min(max(aj_new, L), Hh)
  delta <- aj_cl - a[j]
  out <- a
  out[j] <- aj_cl
  out[i] <- a[i] - as.numeric(y[i]) * as.numeric(y[j]) * delta
  list(alpha = out, moved = abs(delta),
       clipped = abs(aj_cl - aj_new) > .SVMOPT_EPS,
       L = L, H = Hh, eta = eta, step = step)
}

kkt_violation <- function(alpha, y, grad, C) {
  a <- as.numeric(alpha); yy <- as.numeric(y)
  up <- integer(0); low <- integer(0)
  for (t in seq_along(a)) {
    if ((yy[t] > 0 && a[t] < C - .SVMOPT_EPS) ||
        (yy[t] < 0 && a[t] > .SVMOPT_EPS)) up <- c(up, t)
    if ((yy[t] > 0 && a[t] > .SVMOPT_EPS) ||
        (yy[t] < 0 && a[t] < C - .SVMOPT_EPS)) low <- c(low, t)
  }
  if (length(up) == 0L || length(low) == 0L)
    return(list(gap = 0.0, i = NULL, j = NULL,
                note = "no violating pair exists"))
  i <- up[which.max(-yy[up] * grad[up])]
  j <- low[which.min(-yy[low] * grad[low])]
  list(gap = (-yy[i] * grad[i]) - (-yy[j] * grad[j]),
       i = i, j = j, n_up = length(up), n_low = length(low))
}

recover_bias <- function(alpha, y, grad, C) {
  a <- as.numeric(alpha); yy <- as.numeric(y)
  free <- which(a > .SVMOPT_EPS & a < C - .SVMOPT_EPS)
  if (length(free) > 0L) {
    vals <- -yy[free] * grad[free]
    return(list(b = mean(vals), n_free = length(free),
                bracketed = FALSE,
                spread = max(vals) - min(vals)))
  }
  v <- kkt_violation(a, yy, grad, C)
  lo <- if (!is.null(v$j)) -yy[v$j] * grad[v$j] else 0.0
  hi <- if (!is.null(v$i)) -yy[v$i] * grad[v$i] else 0.0
  list(b = 0.5 * (lo + hi), n_free = 0L, bracketed = TRUE,
       note = "no free support vector, so b is only bracketed")
}

smo <- function(y, K, C = 1.0, tol = 1e-8, max_iter = 20000) {
  yy <- as.numeric(y)
  n <- length(yy)
  if (any(!(yy %in% c(-1.0, 1.0))))
    stop("svmopt: labels must be -1 or +1")
  if (nrow(K) != n || ncol(K) != n)
    stop(sprintf("svmopt: the kernel matrix is %dx%d for %d labels",
                 nrow(K), ncol(K), n))
  if (as.numeric(C) <= 0.0)
    stop("svmopt: C must be positive")
  a <- rep(0.0, n)
  grad <- rep(-1.0, n)
  it <- 0L; gap <- Inf
  for (it in seq_len(as.integer(max_iter))) {
    v <- kkt_violation(a, yy, grad, C)
    gap <- v$gap
    if (is.null(v$i) || gap <= as.numeric(tol)) break
    r <- solve_pair(v$i, v$j, a, yy, K, grad, C)
    if (r$moved <= .SVMOPT_EPS) break
    di <- r$alpha[v$i] - a[v$i]
    dj <- r$alpha[v$j] - a[v$j]
    a <- r$alpha
    for (t in seq_len(n)) {
      grad[t] <- grad[t] + (yy[t] * yy[v$i] * K[t, v$i] * di +
                              yy[t] * yy[v$j] * K[t, v$j] * dj)
    }
  }
  b <- recover_bias(a, yy, grad, C)
  sv <- which(a > .SVMOPT_EPS)
  list(estimate = a, alpha = a, b = b$b, gap = gap,
       iterations = it, converged = gap <= as.numeric(tol),
       support_vectors = as.integer(sv), n_sv = length(sv),
       n_free = b$n_free,
       equality_residual = sum(a * yy),
       objective = dual_objective(a, yy, K),
       method = paste0("two-variable decomposition on the maximal ",
                        "violating pair; Chang & Lin (2011)"),
       note = paste0("the KKT gap is both the working-set rule and ",
                     "the stopping criterion"))
}

.svmopt_cheatsheet <- function() {
  paste0("svmopt: the SVM DUAL is where the kernel enters and where ",
         "the structure is exploitable -- max sum(a) - 0.5 a'Qa ",
         "subject to 0 <= a <= C and sum(y a) = 0. The Hessian is ",
         "dense and l x l, so decompose; the EQUALITY constraint ",
         "means one variable cannot move alone, so TWO is the ",
         "smallest workable set -- and at two the subproblem is ",
         "closed form. Choose the pair by MAXIMAL KKT VIOLATION, ",
         "which is also the stopping criterion, so convergence is ",
         "measured not assumed. Clip to [L,H], whose branch depends ",
         "on whether the labels agree -- get it wrong and the ",
         "solver still converges, to the wrong answer. b comes from ",
         "the FREE support vectors.")
}

# compact alias
svm_dual_qp <- smo
svm_dual <- smo
svmdual <- smo

morie_svmopt <- list(kernel_matrix = kernel_matrix,
                     dual_objective = dual_objective,
                     solve_pair = solve_pair,
                     kkt_violation = kkt_violation,
                     smo = smo,
                     recover_bias = recover_bias,
                     cheatsheet = .svmopt_cheatsheet,
                     .bounds = .bounds,
                     svm_dual_qp = svm_dual_qp,
                     svm_dual = svm_dual,
                     svmdual = svmdual)
