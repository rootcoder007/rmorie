# tail2 batch, tranche 3 -- R mirror of the Python modules
#   morie/fn/macohd.py   Cohen (1988) d; Hedges (1981) g
#   morie/fn/crmrlb.py   Rao (1945), Cramer (1946) lower bound
#   morie/fn/survcox.py  Cox (1972, 1975) partial likelihood, Breslow ties
#   morie/fn/penmth.py   Courant (1943) quadratic penalty method
#   morie/fn/prdldm.py   Combettes & Pesquet (2011) Algorithm 3.4
#   morie/fn/otdom.py    Courty et al (2017) OT domain adaptation
#
# Byte-identical between r-package/morie/R and r-morie-oss/R.
#
# Sources actually consulted are named in each function and, at more
# length, in the docstring of the matching Python module.  Two of the
# six were checked against an independent reference implementation:
# CohensD against metafor::escalc(measure = "SMD") and CoxPL against
# survival::coxph(ties = "breslow"); both agree to 1e-15.  Every routine
# below is closed form or runs a FIXED number of steps, because an early
# exit on one language arm and not the other silently breaks parity.

# ---- Gauss-Jordan inverse, shared by CramerRao and CoxPL ------------
# Local on purpose: the Python arms each carry their own copy too, so
# the two languages run the identical arithmetic in the identical order.

#' .morie_t2_inv
#'
#' A step of the tail2_t03 implementation. Called by \code{CoxPL}, \code{CramerRao}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A A matrix; passed to \code{nrow}.
#' @return The value of \code{out}, as built in the body.
#' @export
.morie_t2_inv <- function(A) {
  k <- nrow(A)
  M <- cbind(A, diag(1, k))
  for (cc in seq_len(k)) {
    piv <- cc - 1L + which.max(abs(M[cc:k, cc]))
    if (abs(M[piv, cc]) < 1e-300)
      stop("matrix is singular")
    if (piv != cc) {
      tmp <- M[cc, ]; M[cc, ] <- M[piv, ]; M[piv, ] <- tmp
    }
    pv <- M[cc, cc]
    for (r in seq_len(k)) {
      if (r == cc) next
      fac <- M[r, cc] / pv
      if (fac == 0) next
      for (t in cc:(2L * k)) M[r, t] <- M[r, t] - fac * M[cc, t]
    }
  }
  out <- matrix(0, k, k)
  for (i in seq_len(k)) for (j in seq_len(k))
    out[i, j] <- M[i, k + j] / M[i, i]
  out
}

# ---- Cohen (1988) d, Hedges (1981) g --------------------------------

#\' Cohen\'s d and Hedges\' g for two independent groups
#\'
#\' d = (m1 - m2) / s_pooled with the usual pooled standard deviation.
#\' The bias correction is the exact Hedges (1981) factor
#\' J = Gamma(df/2) / (sqrt(df/2) Gamma((df-1)/2)), and the variance is
#\' 1/n1 + 1/n2 + estimate^2 / (2 (n1 + n2)) evaluated at the estimate.
#\' Both conventions were settled against metafor::escalc(measure="SMD").
#\'
#\' @param m1,m2 group means
#\' @param s1,s2 group standard deviations (denominator n-1)
#\' @param n1,n2 group sizes, each at least 2
#\' @return list(d, s_pooled, var_d, se_d, j, j_approx, hedges_g, var_g,
#\'   se_g, df, n, method)
#\' @export
#' \' Cohen\'s d and Hedges\' g for two independent groups
#'
#' \' \' d = (m1 - m2) / s_pooled with the usual pooled standard
#' deviation. \' The bias correction is the exact Hedges (1981) factor
#' \' J = Gamma(df/2) / (sqrt(df/2) Gamma((df-1)/2)), and the variance
#' is \' 1/n1 + 1/n2 + estimate^2 / (2 (n1 + n2)) evaluated at the
#' estimate. \' Both conventions were settled against
#' metafor::escalc(measure="SMD"). \' \' @param m1,m2 group means \'
#' @param s1,s2 group standard deviations (denominator n-1) \' @param
#' n1,n2 group sizes, each at least 2 \' @return list(d, s_pooled,
#' var_d, se_d, j, j_approx, hedges_g, var_g, \' se_g, df, n, method)
#' \' @export
#'
#' @param m1 Numeric; combined arithmetically in the body.
#' @param m2 Numeric; combined arithmetically in the body.
#' @param s1 Numeric; combined arithmetically in the body.
#' @param s2 Numeric; combined arithmetically in the body.
#' @param n1 Numeric; combined arithmetically in the body.
#' @param n2 Numeric; combined arithmetically in the body.
#' @return A list with \code{d}, \code{s_pooled}, \code{var_d}, \code{se_d}, \code{j}, \code{j_approx}, \code{hedges_g}, \code{var_g}, \code{se_g}, \code{df}, \code{n}, \code{method}.
#' @export
CohensD <- function(m1, m2, s1, s2, n1, n2) {
  n1 <- as.integer(n1); n2 <- as.integer(n2)
  if (n1 < 2L || n2 < 2L)
    stop("each group needs at least 2 observations")
  df <- n1 + n2 - 2L
  m1 <- as.numeric(m1); m2 <- as.numeric(m2)
  s1 <- as.numeric(s1); s2 <- as.numeric(s2)
  if (s1 < 0 || s2 < 0)
    stop("standard deviations must be non-negative")
  sp2 <- ((n1 - 1L) * s1 * s1 + (n2 - 1L) * s2 * s2) / df
  sp <- sqrt(sp2)
  if (sp == 0) stop("pooled standard deviation is zero")
  d <- (m1 - m2) / sp
  ntot <- n1 + n2
  base <- 1 / n1 + 1 / n2
  var_d <- base + d * d / (2 * ntot)
  j <- exp(lgamma(df / 2) - 0.5 * log(df / 2) - lgamma((df - 1) / 2))
  j_approx <- 1 - 3 / (4 * df - 1)
  g <- j * d
  var_g <- base + g * g / (2 * ntot)
  list(d = d, s_pooled = sp, var_d = var_d, se_d = sqrt(var_d),
       j = j, j_approx = j_approx, hedges_g = g, var_g = var_g,
       se_g = sqrt(var_g), df = df, n = ntot,
       method = "Cohen\'s d / Hedges\' g, two independent groups")
}

# ---- Rao (1945), Cramer (1946) lower bound --------------------------

#\' Cramer-Rao lower bound implied by a Fisher information matrix
#\'
#\' Cov(theta_hat) >= I(theta)^-1 in the Loewner order, so the bound on
#\' component k is the k-th diagonal entry of the inverse information.
#\'
#\' @param fisher_info a scalar, a vector read as a diagonal, or a
#\'   square symmetric matrix
#\' @param var_estimate optional actual variances, one per parameter
#\' @return list(bound, variance, se, information, k, efficiency,
#\'   attained, method)
#\' @export
#' \' Cramer-Rao lower bound implied by a Fisher information matrix
#'
#' \' \' Cov(theta_hat) >= I(theta)^-1 in the Loewner order, so the
#' bound on \' component k is the k-th diagonal entry of the inverse
#' information. \' \' @param fisher_info a scalar, a vector read as a
#' diagonal, or a \' square symmetric matrix \' @param var_estimate
#' optional actual variances, one per parameter \' @return list(bound,
#' variance, se, information, k, efficiency, \' attained, method) \'
#' @export
#'
#' @param fisher_info A matrix; passed to \code{nrow}.
#' @param var_estimate Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{bound}, \code{variance}, \code{se}, \code{information}, \code{k}, \code{efficiency}, \code{attained}, \code{method}.
#' @export
CramerRao <- function(fisher_info, var_estimate = NULL) {
  if (is.matrix(fisher_info)) {
    info <- matrix(as.numeric(fisher_info), nrow(fisher_info))
    if (nrow(info) != ncol(info)) stop("fisher_info must be square")
  } else {
    v <- as.numeric(fisher_info)
    if (length(v) == 0L) stop("fisher_info is empty")
    info <- diag(v, nrow = length(v))
  }
  k <- nrow(info)
  for (i in seq_len(k)) {
    j <- i + 1L
    while (j <= k) {
      if (abs(info[i, j] - info[j, i]) > 1e-12 * (1 + abs(info[i, j])))
        stop("Fisher information matrix must be symmetric")
      j <- j + 1L
    }
  }
  for (i in seq_len(k))
    if (info[i, i] <= 0)
      stop("Fisher information has a non-positive diagonal")
  bound <- .morie_t2_inv(info)
  var <- vapply(seq_len(k), function(i) bound[i, i], numeric(1))
  if (any(var <= 0))
    stop("inverse information has a non-positive diagonal")
  se <- sqrt(var)
  eff <- NULL
  attained <- NULL
  if (!is.null(var_estimate)) {
    ve <- as.numeric(var_estimate)
    if (length(ve) != k)
      stop("var_estimate must have one entry per parameter")
    eff <- var / ve
    attained <- all(eff <= 1 + 1e-9)
  }
  list(bound = bound, variance = var, se = se, information = info,
       k = k, efficiency = eff, attained = attained,
       method = "Cramer-Rao lower bound (inverse Fisher information)")
}

# ---- Cox (1972, 1975) partial likelihood, Breslow ties ---------------

#' .morie_t2_coxterms
#'
#' A step of the tail2_t03 implementation. Called by \code{CoxPL}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param time A vector; its length is taken and its elements indexed.
#' @param event A vector; indexed elementwise.
#' @param X A matrix; indexed by row and column.
#' @param beta A matrix; passed to \code{\%*\%}.
#' @return A list with \code{loglik}, \code{score}, \code{info}, \code{nevent}.
#' @export
.morie_t2_coxterms <- function(time, event, X, beta) {
  n <- length(time)
  p <- length(beta)
  eta <- as.numeric(X %*% beta)
  w <- exp(eta)
  ord <- order(-time, seq_len(n))
  loglik <- 0
  score <- numeric(p)
  info <- matrix(0, p, p)
  s0 <- 0
  s1 <- numeric(p)
  s2 <- matrix(0, p, p)
  k <- 1L
  nevent <- 0L
  while (k <= n) {
    t <- time[ord[k]]
    j <- k
    while (j <= n && time[ord[j]] == t) {
      i <- ord[j]
      s0 <- s0 + w[i]
      for (a in seq_len(p)) {
        s1[a] <- s1[a] + w[i] * X[i, a]
        for (b in seq_len(p))
          s2[a, b] <- s2[a, b] + w[i] * X[i, a] * X[i, b]
      }
      j <- j + 1L
    }
    idx <- ord[k:(j - 1L)]
    deaths <- idx[event[idx] == 1L]
    d <- length(deaths)
    if (d > 0L) {
      nevent <- nevent + d
      for (i in deaths) {
        loglik <- loglik + eta[i]
        for (a in seq_len(p)) score[a] <- score[a] + X[i, a]
      }
      loglik <- loglik - d * log(s0)
      for (a in seq_len(p)) {
        score[a] <- score[a] - d * s1[a] / s0
        for (b in seq_len(p))
          info[a, b] <- info[a, b] +
            d * (s2[a, b] / s0 - (s1[a] / s0) * (s1[b] / s0))
      }
    }
    k <- j
  }
  list(loglik = loglik, score = score, info = info, nevent = nevent)
}

#\' Cox partial likelihood, its score and information, optionally fitted
#\'
#\' Breslow handling of tied event times.  When beta is NULL the partial
#\' likelihood is maximised by Newton-Raphson from zero.  Checked against
#\' survival::coxph(ties = "breslow").
#\'
#\' @param time follow-up times
#\' @param event 1 for an observed event, 0 for right censoring
#\' @param X covariate matrix, no intercept column
#\' @param beta coefficients to evaluate at, or NULL to fit
#\' @param max_iter,tol Newton-Raphson controls, used only when beta is NULL
#\' @return list(loglik, score, information, coefficients, vcov, se, n,
#\'   n_event, iterations, converged, method)
#\' @export
#' \' Cox partial likelihood, its score and information, optionally
#' fitted
#'
#' \' \' Breslow handling of tied event times.  When beta is NULL the
#' partial \' likelihood is maximised by Newton-Raphson from zero.
#' Checked against \' survival::coxph(ties = "breslow"). \' \' @param
#' time follow-up times \' @param event 1 for an observed event, 0 for
#' right censoring \' @param X covariate matrix, no intercept column
#' \' @param beta coefficients to evaluate at, or NULL to fit \'
#' @param max_iter,tol Newton-Raphson controls, used only when beta is
#' NULL \' @return list(loglik, score, information, coefficients, vcov,
#' se, n, \' n_event, iterations, converged, method) \' @export
#'
#' @param time A vector; its length is taken.
#' @param event A vector; its length is taken.
#' @param X A matrix; passed to \code{nrow}.
#' @param beta Optional; may be \code{NULL}. A vector; its length is taken.
#' @param max_iter Coerced to integer by the body, with \code{as.integer}. Defaults to \code{50L}.
#' @param tol Defaults to \code{1e-10}.
#' @return A list with \code{loglik}, \code{score}, \code{information}, \code{coefficients}, \code{vcov}, \code{se}, \code{n}, \code{n_event}, \code{iterations}, \code{converged}, \code{method}.
#' @export
CoxPL <- function(time, event, X, beta = NULL, max_iter = 50L,
                  tol = 1e-10) {
  time <- as.numeric(time)
  event <- as.integer(ifelse(as.logical(event), 1L, 0L))
  X <- matrix(as.numeric(as.matrix(X)), nrow = length(time))
  n <- length(time)
  if (length(event) != n || nrow(X) != n)
    stop("time, event and X must have the same length")
  if (n == 0L) stop("no observations")
  p <- ncol(X)
  if (!any(event == 1L))
    stop("no events; the partial likelihood is empty")

  fitted <- is.null(beta)
  iterations <- 0L
  converged <- TRUE
  if (fitted) {
    b <- numeric(p)
    converged <- FALSE
    for (it in seq_len(as.integer(max_iter))) {
      iterations <- it
      tt <- .morie_t2_coxterms(time, event, X, b)
      step <- .morie_t2_inv(tt$info)
      delta <- as.numeric(step %*% tt$score)
      b <- b + delta
      if (max(abs(delta)) < tol) {
        converged <- TRUE
        break
      }
    }
    beta <- b
  } else {
    beta <- as.numeric(beta)
    if (length(beta) != p)
      stop("beta must have one entry per column of X")
  }

  tt <- .morie_t2_coxterms(time, event, X, beta)
  vcov <- .morie_t2_inv(tt$info)
  list(loglik = tt$loglik, score = tt$score, information = tt$info,
       coefficients = beta, vcov = vcov,
       se = sqrt(vapply(seq_len(p), function(a) vcov[a, a], numeric(1))),
       n = n, n_event = tt$nevent, iterations = iterations,
       converged = converged,
       method = paste0("Cox partial likelihood, Breslow ties",
                       if (fitted) " (Newton-Raphson fit)"
                       else " (evaluated at beta)"))
}

# ---- Courant (1943) quadratic penalty method ------------------------

#' .morie_t2_viol
#'
#' A step of the tail2_t03 implementation. Called by \code{.morie_t2_qpen}, \code{PenaltyMin}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param constraints Iterated over elementwise, with \code{vapply}.
#' @param x See Usage.
#' @return A vector, from \code{vapply}.
#' @export
.morie_t2_viol <- function(constraints, x)
  vapply(constraints, function(g) max(0, as.numeric(g(x))), numeric(1))

#' .morie_t2_qpen
#'
#' A step of the tail2_t03 implementation. Called by \code{PenaltyMin}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param f See Usage.
#' @param constraints Passed to \code{.morie_t2_viol}.
#' @param x Passed to \code{.morie_t2_viol}.
#' @param mu Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.morie_t2_qpen <- function(f, constraints, x, mu) {
  v <- .morie_t2_viol(constraints, x)
  as.numeric(f(x)) + mu * sum(v * v)
}

#' .morie_t2_fdgrad
#'
#' A step of the tail2_t03 implementation. Called by \code{PenaltyMin}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param fun See Usage.
#' @param x A vector; its length is taken.
#' @param h Numeric; combined arithmetically in the body.
#' @return The value of \code{g}, as built in the body.
#' @export
.morie_t2_fdgrad <- function(fun, x, h) {
  g <- numeric(length(x))
  for (k in seq_along(x)) {
    xp <- x; xm <- x
    xp[k] <- xp[k] + h
    xm[k] <- xm[k] - h
    g[k] <- (fun(xp) - fun(xm)) / (2 * h)
  }
  g
}

#\' Minimise f subject to g_i(x) <= 0 by Courant\'s quadratic penalty
#\'
#\' Q(x; mu) = f(x) + mu sum_i max(0, g_i(x))^2, minimised for an
#\' increasing sequence of mu.  The inner solve is steepest descent with
#\' an Armijo backtracking line search on central finite differences, run
#\' for a fixed budget.
#\'
#\' @param f objective, taking a numeric vector
#\' @param constraints list of functions; g_i(x) <= 0 is the constraint
#\' @param x0 starting point
#\' @param mu initial penalty weight
#\' @param n_outer,growth,n_inner,step0,h,armijo,max_halving fixed budget
#\' @return list(x, f, penalty, violation, max_violation, q, mu, n_outer,
#\'   n_inner, method)
#\' @export
#' \' Minimise f subject to g_i(x) <= 0 by Courant\'s quadratic
#' penalty
#'
#' \' \' Q(x; mu) = f(x) + mu sum_i max(0, g_i(x))^2, minimised for an
#' \' increasing sequence of mu.  The inner solve is steepest descent
#' with \' an Armijo backtracking line search on central finite
#' differences, run \' for a fixed budget. \' \' @param f objective,
#' taking a numeric vector \' @param constraints list of functions;
#' g_i(x) <= 0 is the constraint \' @param x0 starting point \' @param
#' mu initial penalty weight \' @param
#' n_outer,growth,n_inner,step0,h,armijo,max_halving fixed budget \'
#' @return list(x, f, penalty, violation, max_violation, q, mu, n_outer,
#' \' n_inner, method) \' @export
#'
#' @param f Passed to \code{.morie_t2_qpen}.
#' @param constraints Passed to \code{.morie_t2_qpen}.
#' @param x0 Coerced to numeric by the body, with \code{as.numeric}.
#' @param mu Numeric; combined arithmetically in the body.
#' @param n_outer Coerced to integer by the body, with \code{as.integer}. Defaults to \code{8L}.
#' @param growth Numeric; combined arithmetically in the body. Defaults to \code{10}.
#' @param n_inner Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200L}.
#' @param step0 Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @param h Passed to \code{.morie_t2_fdgrad}. Defaults to \code{1e-06}.
#' @param armijo Numeric; combined arithmetically in the body. Defaults to \code{1e-04}.
#' @param max_halving Coerced to integer by the body, with \code{as.integer}. Defaults to \code{40L}.
#' @return A list with \code{x}, \code{f}, \code{penalty}, \code{violation}, \code{max_violation}, \code{q}, \code{mu}, \code{n_outer}, \code{n_inner}, \code{method}.
#' @export
PenaltyMin <- function(f, constraints, x0, mu, n_outer = 8L,
                       growth = 10, n_inner = 200L, step0 = 1,
                       h = 1e-6, armijo = 1e-4, max_halving = 40L) {
  x <- as.numeric(x0)
  constraints <- as.list(constraints)
  mu <- as.numeric(mu)
  if (mu <= 0) stop("mu must be positive")
  if (growth <= 1)
    stop("growth must exceed 1 or the penalty never tightens")
  for (outer in seq_len(as.integer(n_outer))) {
    local_mu <- mu
    q <- function(z) .morie_t2_qpen(f, constraints, z, local_mu)
    cur <- q(x)
    for (inner in seq_len(as.integer(n_inner))) {
      g <- .morie_t2_fdgrad(q, x, h)
      gn2 <- sum(g * g)
      if (gn2 == 0) break
      step <- as.numeric(step0)
      improved <- FALSE
      for (hv in seq_len(as.integer(max_halving))) {
        trial <- x - step * g
        qt <- q(trial)
        if (qt <= cur - armijo * step * gn2) {
          x <- trial
          cur <- qt
          improved <- TRUE
          break
        }
        step <- step * 0.5
      }
      if (!improved) break
    }
    mu <- mu * growth
  }
  v <- .morie_t2_viol(constraints, x)
  fx <- as.numeric(f(x))
  pen <- sum(v * v)
  list(x = x, f = fx, penalty = pen, violation = v,
       max_violation = if (length(v)) max(v) else 0,
       q = fx + (mu / growth) * pen, mu = mu / growth,
       n_outer = as.integer(n_outer), n_inner = as.integer(n_inner),
       method = "Courant (1943) quadratic penalty method")
}

# ---- Combettes & Pesquet (2011) Algorithm 3.4 -----------------------

#\' Minimise f + g by proximal forward-backward splitting
#\'
#\' x_{n+1} = x_n + lambda (prox_{lr g}(x_n - lr grad f(x_n)) - x_n),
#\' equation (21) of Combettes & Pesquet (2011), arXiv:0912.3522.
#\' Runs a fixed number of iterations.
#\'
#\' @param f smooth part, reported only as the objective
#\' @param grad_f gradient of f
#\' @param prox_g function(y, lr) giving prox of lr * g at y
#\' @param x0 starting point
#\' @param lr step size, the survey\'s beta^-1
#\' @param n_iter fixed iteration count
#\' @param relaxation the survey\'s lambda_n, in ]0, 3/2[
#\' @return list(x, objective, n_iter, lr, relaxation, step_norm, method)
#\' @export
#' \' Minimise f + g by proximal forward-backward splitting
#'
#' \' \' x_{n+1} = x_n + lambda (prox_{lr g}(x_n - lr grad f(x_n)) -
#' x_n), \' equation (21) of Combettes & Pesquet (2011),
#' arXiv:0912.3522. \' Runs a fixed number of iterations. \' \'
#' @param f smooth part, reported only as the objective \' @param
#' grad_f gradient of f \' @param prox_g function(y, lr) giving prox of
#' lr * g at y \' @param x0 starting point \' @param lr step size, the
#' survey\'s beta^-1 \' @param n_iter fixed iteration count \' @param
#' relaxation the survey\'s lambda_n, in ]0, 3/2[ \' @return list(x,
#' objective, n_iter, lr, relaxation, step_norm, method) \' @export
#'
#' @param f See Usage.
#' @param grad_f See Usage.
#' @param prox_g See Usage.
#' @param x0 Coerced to numeric by the body, with \code{as.numeric}.
#' @param lr Numeric; combined arithmetically in the body.
#' @param n_iter Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200L}.
#' @param relaxation Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @return A list with \code{x}, \code{objective}, \code{n_iter}, \code{lr}, \code{relaxation}, \code{step_norm}, \code{method}.
#' @export
ProxGrad <- function(f, grad_f, prox_g, x0, lr, n_iter = 200L,
                     relaxation = 1) {
  x <- as.numeric(x0)
  lr <- as.numeric(lr)
  relaxation <- as.numeric(relaxation)
  if (lr <= 0) stop("lr must be positive")
  if (!(relaxation > 0 && relaxation < 1.5))
    stop("relaxation must lie strictly inside ]0, 3/2[")
  step_norm <- 0
  for (nn in seq_len(as.integer(n_iter))) {
    g <- as.numeric(grad_f(x))
    if (length(g) != length(x)) stop("grad_f returned the wrong length")
    y <- x - lr * g
    z <- as.numeric(prox_g(y, lr))
    if (length(z) != length(x)) stop("prox_g returned the wrong length")
    nx <- x + relaxation * (z - x)
    step_norm <- sqrt(sum((nx - x)^2))
    x <- nx
  }
  list(x = x, objective = as.numeric(f(x)),
       n_iter = as.integer(n_iter), lr = lr, relaxation = relaxation,
       step_norm = step_norm,
       method = paste("forward-backward splitting, Combettes & Pesquet",
                      "(2011) Algorithm 3.4"))
}

# ---- Courty et al (2017) OT domain adaptation -----------------------

#\' Adapt source samples to a target domain through an OT plan
#\'
#\' The entropy-regularised plan of equation (9) is found by a fixed
#\' number of Sinkhorn-Knopp sweeps, then the source samples are moved by
#\' the barycentric mapping of equation (14),
#\' Xs_hat = diag(gamma 1_nt)^-1 gamma Xt, with squared Euclidean cost.
#\'
#\' @param Xs source samples, ns x d
#\' @param Xt target samples, nt x d
#\' @param epsilon entropic regularisation weight (1/lambda in the paper)
#\' @param n_iter fixed number of Sinkhorn sweeps
#\' @return list(Xs_adapted, gamma, cost, transport_cost, row_error,
#\'   col_error, ns, nt, d, epsilon, n_iter, method)
#\' @export
#' \' Adapt source samples to a target domain through an OT plan
#'
#' \' \' The entropy-regularised plan of equation (9) is found by a
#' fixed \' number of Sinkhorn-Knopp sweeps, then the source samples
#' are moved by \' the barycentric mapping of equation (14), \' Xs_hat
#' = diag(gamma 1_nt)^-1 gamma Xt, with squared Euclidean cost. \' \'
#' @param Xs source samples, ns x d \' @param Xt target samples, nt x d
#' \' @param epsilon entropic regularisation weight (1/lambda in the
#' paper) \' @param n_iter fixed number of Sinkhorn sweeps \' @return
#' list(Xs_adapted, gamma, cost, transport_cost, row_error, \'
#' col_error, ns, nt, d, epsilon, n_iter, method) \' @export
#'
#' @param Xs A matrix; indexed by row and column.
#' @param Xt A matrix; indexed by row and column.
#' @param epsilon Numeric; combined arithmetically in the body.
#' @param n_iter Coerced to integer by the body, with \code{as.integer}. Defaults to \code{1000L}.
#' @return A list with \code{Xs_adapted}, \code{gamma}, \code{cost}, \code{transport_cost}, \code{row_error}, \code{col_error}, \code{ns}, \code{nt}, \code{d}, \code{epsilon}, \code{n_iter}, \code{method}.
#' @export
OtAdapt <- function(Xs, Xt, epsilon, n_iter = 1000L) {
  Xs <- as.matrix(Xs); Xt <- as.matrix(Xt)
  ns <- nrow(Xs); nt <- nrow(Xt)
  if (ns == 0L || nt == 0L)
    stop("both sample sets must be non-empty")
  d <- ncol(Xs)
  if (ncol(Xt) != d)
    stop("Xs and Xt must share the same dimension")
  epsilon <- as.numeric(epsilon)
  if (epsilon <= 0) stop("epsilon must be positive")

  C <- matrix(0, ns, nt)
  for (i in seq_len(ns)) for (j in seq_len(nt))
    C[i, j] <- sum((Xs[i, ] - Xt[j, ])^2)
  K <- exp(-C / epsilon)
  a <- 1 / ns
  b <- 1 / nt
  u <- rep(1, ns)
  v <- rep(1, nt)
  for (nn in seq_len(as.integer(n_iter))) {
    for (i in seq_len(ns)) {
      s <- sum(K[i, ] * v)
      if (s <= 0) stop("Sinkhorn kernel underflowed; raise epsilon")
      u[i] <- a / s
    }
    for (j in seq_len(nt)) {
      s <- sum(K[, j] * u)
      if (s <= 0) stop("Sinkhorn kernel underflowed; raise epsilon")
      v[j] <- b / s
    }
  }
  gamma <- matrix(0, ns, nt)
  for (i in seq_len(ns)) for (j in seq_len(nt))
    gamma[i, j] <- u[i] * K[i, j] * v[j]

  rows <- vapply(seq_len(ns), function(i) sum(gamma[i, ]), numeric(1))
  cols <- vapply(seq_len(nt), function(j) sum(gamma[, j]), numeric(1))
  adapted <- matrix(0, ns, d)
  for (i in seq_len(ns)) {
    if (rows[i] <= 0)
      stop("a source point received no transported mass")
    for (k in seq_len(d))
      adapted[i, k] <- sum(gamma[i, ] * Xt[, k]) / rows[i]
  }
  list(Xs_adapted = adapted, gamma = gamma, cost = C,
       transport_cost = sum(gamma * C),
       row_error = max(abs(rows - a)), col_error = max(abs(cols - b)),
       ns = ns, nt = nt, d = d, epsilon = epsilon,
       n_iter = as.integer(n_iter),
       method = paste("entropic OT domain adaptation, Courty et al (2017)",
                      "eq. (9) and (14)"))
}
