# SPDX-License-Identifier: AGPL-3.0-or-later

## First-order optimiser update rules -- R parity for morie.fn's optimiser
## family (adamopt, adgrad, rmsoptm, sgdmom, nesterv, gdupd, sgdup, adwopt,
## larspec, lambopt).
##
## Each rule keeps its accumulators in a `state` list that the caller threads
## between steps, matching the Python signatures exactly so results agree to
## machine precision.

#' .morie_opt_vec
#'
#' Part of the optim_native implementation; see the file header for the
#' source it follows.
#'
#' @param g See Usage.
#' @param name Defaults to \code{"g"}.
#' @return The value of \code{a}, as built in the body.
#' @export
.morie_opt_vec <- function(g, name = "g") {
  a <- as.numeric(g)
  if (length(a) == 0L) stop(sprintf("%s must be non-empty", name), call. = FALSE)
  if (any(!is.finite(a))) stop(sprintf("%s contains non-finite values", name), call. = FALSE)
  a
}

#' .morie_opt_state
#'
#' Part of the optim_native implementation; see the file header for the
#' source it follows.
#'
#' @param state See Usage.
#' @param n See Usage.
#' @param keys Defaults to \code{c("m", "v")}.
#' @return The value of \code{out}, as built in the body.
#' @export
.morie_opt_state <- function(state, n, keys = c("m", "v")) {
  out <- list(t = 0L)
  if (!is.null(state)) {
    for (k in keys) if (!is.null(state[[k]])) out[[k]] <- as.numeric(state[[k]])
    if (!is.null(state$t)) out$t <- as.integer(state$t)
  }
  for (k in keys) if (is.null(out[[k]]) || length(out[[k]]) != n) out[[k]] <- numeric(n)
  out$t <- out$t + 1L
  out
}

#' Adam optimiser step
#'
#' Bias-corrected first and second moment estimates (Kingma & Ba, 2015).
#' The bias correction is what makes the first step approximately `lr` in
#' size whatever the gradient's scale.
#'
#' @param g Gradient at the current parameters.
#' @param beta1,beta2 Moment decay rates in \[0, 1).
#' @param lr Step size.
#' @param eps Denominator floor.
#' @param state List returned by the previous call, or `NULL` to start fresh.
#' @return List with `update`, `state`, `m`, `v`, `t`, `step_norm`.
#' @references Kingma, D. P., & Ba, J. (2015). Adam: A method for stochastic
#'   optimization. ICLR 2015.
#' @examples
#' x <- 0; st <- NULL
#' for (i in 1:4000) {
#'   r <- morie_adam(2 * (x - 3), lr = 0.05, state = st)
#'   x <- x + r$update; st <- r$state
#' }
#' abs(x - 3) < 1e-3
#' @export
morie_adam <- function(g, beta1 = 0.9, beta2 = 0.999, lr = 1e-3, eps = 1e-8,
                       state = NULL) {
  if (beta1 < 0 || beta1 >= 1) stop("beta1 must be in [0, 1)", call. = FALSE)
  if (beta2 < 0 || beta2 >= 1) stop("beta2 must be in [0, 1)", call. = FALSE)
  g <- .morie_opt_vec(g)
  st <- .morie_opt_state(state, length(g), c("m", "v"))
  st$m <- beta1 * st$m + (1 - beta1) * g
  st$v <- beta2 * st$v + (1 - beta2) * g^2
  m_hat <- st$m / (1 - beta1^st$t)
  v_hat <- st$v / (1 - beta2^st$t)
  upd <- -lr * m_hat / (sqrt(v_hat) + eps)
  list(update = upd, state = st, m = st$m, v = st$v, m_hat = m_hat,
       v_hat = v_hat, t = st$t, step_norm = sqrt(sum(upd^2)), method = "Adam")
}

#' AdaGrad optimiser step
#'
#' Accumulates the undecayed sum of squared gradients, so the effective step
#' size is monotonically non-increasing and eventually stalls -- the
#' behaviour RMSProp's decay was introduced to fix.
#'
#' @param g Gradient at the current parameters.
#' @param lr Base step size.
#' @param eps Denominator floor.
#' @param state List from the previous call, or `NULL`.
#' @return List with `update`, `state`, `G`, `t`, `step_norm`.
#' @references Duchi, J., Hazan, E., & Singer, Y. (2011). Adaptive subgradient
#'   methods for online learning and stochastic optimization. JMLR 12,
#'   2121-2159.
#' @examples
#' r1 <- morie_adagrad(1, lr = 0.1)
#' r2 <- morie_adagrad(1, lr = 0.1, state = r1$state)
#' abs(r2$update) < abs(r1$update)
#' @export
morie_adagrad <- function(g, lr = 0.01, eps = 1e-8, state = NULL) {
  g <- .morie_opt_vec(g)
  st <- .morie_opt_state(state, length(g), "G")
  st$G <- st$G + g^2
  upd <- -lr * g / (sqrt(st$G) + eps)
  list(update = upd, state = st, G = st$G, t = st$t,
       step_norm = sqrt(sum(upd^2)), method = "AdaGrad")
}

#' RMSProp optimiser step
#'
#' Exponentially decayed average of squared gradients, so the effective step
#' size settles rather than decaying to zero as AdaGrad's does. No momentum
#' and no bias correction, so the first steps are larger than the asymptotic
#' ones.
#'
#' @param g Gradient at the current parameters.
#' @param rho Decay rate in \[0, 1).
#' @param lr Step size.
#' @param eps Denominator floor.
#' @param state List from the previous call, or `NULL`.
#' @return List with `update`, `state`, `v`, `t`, `step_norm`.
#' @references Tieleman, T., & Hinton, G. (2012). Lecture 6.5 -- RMSProp.
#' @examples
#' st <- NULL
#' for (i in 1:500) { r <- morie_rmsprop(1, lr = 0.1, state = st); st <- r$state }
#' abs(abs(r$update) - 0.1) < 1e-3
#' @export
morie_rmsprop <- function(g, rho = 0.9, lr = 0.001, eps = 1e-8, state = NULL) {
  if (rho < 0 || rho >= 1) stop("rho must be in [0, 1)", call. = FALSE)
  g <- .morie_opt_vec(g)
  st <- .morie_opt_state(state, length(g), "v")
  st$v <- rho * st$v + (1 - rho) * g^2
  upd <- -lr * g / (sqrt(st$v) + eps)
  list(update = upd, state = st, v = st$v, t = st$t,
       step_norm = sqrt(sum(upd^2)), method = "RMSProp")
}

#' SGD with heavy-ball momentum
#'
#' On a constant gradient the step approaches `-lr * g / (1 - mu)`, so raising
#' `mu` without lowering `lr` amplifies the effective learning rate.
#'
#' @param g Gradient at the current parameters.
#' @param mu Momentum coefficient in \[0, 1).
#' @param lr Step size.
#' @param state List from the previous call, or `NULL`.
#' @return List with `update`, `state`, `v`, `t`, `step_norm`.
#' @references Polyak, B. T. (1964). Some methods of speeding up the
#'   convergence of iteration methods. USSR Comp. Math. 4(5), 1-17.
#' @examples
#' st <- NULL
#' for (i in 1:400) { r <- morie_sgd_momentum(1, mu = 0.9, lr = 0.01, state = st); st <- r$state }
#' abs(r$update + 0.1) < 1e-6
#' @export
morie_sgd_momentum <- function(g, mu = 0.9, lr = 0.01, state = NULL) {
  if (mu < 0 || mu >= 1) stop("mu must be in [0, 1)", call. = FALSE)
  g <- .morie_opt_vec(g)
  st <- .morie_opt_state(state, length(g), "v")
  st$v <- mu * st$v - lr * g
  list(update = st$v, state = st, v = st$v, t = st$t,
       step_norm = sqrt(sum(st$v^2)), method = "SGD with momentum")
}

#' Nesterov accelerated gradient step
#'
#' The look-ahead form of Sutskever et al. (2013): the velocity updates as for
#' heavy-ball but the parameter step is `mu * v - lr * g`. For the exact
#' method `g` should be evaluated at the look-ahead point `theta + mu * v`.
#'
#' @param g Gradient (see note on where to evaluate it).
#' @param mu Momentum coefficient in \[0, 1).
#' @param lr Step size.
#' @param state List from the previous call, or `NULL`.
#' @return List with `update`, `state`, `v`, `t`, `step_norm`.
#' @references Nesterov, Y. (1983). Soviet Mathematics Doklady 27(2), 372-376.
#'   Sutskever, I., et al. (2013). ICML 2013, 1139-1147.
#' @examples
#' round(morie_nesterov(1, mu = 0.9, lr = 0.1)$update, 6)
#' @export
morie_nesterov <- function(g, mu = 0.9, lr = 0.01, state = NULL) {
  if (mu < 0 || mu >= 1) stop("mu must be in [0, 1)", call. = FALSE)
  g <- .morie_opt_vec(g)
  st <- .morie_opt_state(state, length(g), "v")
  st$v <- mu * st$v - lr * g
  upd <- mu * st$v - lr * g
  list(update = upd, state = st, v = st$v, t = st$t,
       step_norm = sqrt(sum(upd^2)), method = "Nesterov accelerated gradient")
}

#' AdamW step with decoupled weight decay
#'
#' Adam applies the decay outside the adaptive `1/sqrt(v)` scaling, so
#' parameters with a large gradient history are not decayed less. Supply
#' `theta` or the step reduces to plain Adam.
#'
#' @param g Gradient of the unregularised loss.
#' @param beta1,beta2 Moment decay rates in \[0, 1).
#' @param lr Step size.
#' @param wd Decoupled weight-decay coefficient.
#' @param eps Denominator floor.
#' @param theta Current parameters, needed for the decay term.
#' @param state List from the previous call, or `NULL`.
#' @return List with `update`, `state`, `m`, `v`, `decay`, `t`, `step_norm`.
#' @references Loshchilov, I., & Hutter, F. (2019). Decoupled weight decay
#'   regularization. ICLR 2019.
#' @examples
#' round(morie_adamw_step(0, lr = 0.1, wd = 0.5, theta = 2)$update, 6)
#' @export
morie_adamw_step <- function(g, beta1 = 0.9, beta2 = 0.999, lr = 1e-3,
                             wd = 0.01, eps = 1e-8, theta = NULL, state = NULL) {
  if (beta1 < 0 || beta1 >= 1) stop("beta1 must be in [0, 1)", call. = FALSE)
  if (beta2 < 0 || beta2 >= 1) stop("beta2 must be in [0, 1)", call. = FALSE)
  g <- .morie_opt_vec(g)
  st <- .morie_opt_state(state, length(g), c("m", "v"))
  st$m <- beta1 * st$m + (1 - beta1) * g
  st$v <- beta2 * st$v + (1 - beta2) * g^2
  m_hat <- st$m / (1 - beta1^st$t)
  v_hat <- st$v / (1 - beta2^st$t)
  adaptive <- -lr * m_hat / (sqrt(v_hat) + eps)
  if (is.null(theta)) {
    decay <- numeric(length(g))
  } else {
    th <- as.numeric(theta)
    if (length(th) != length(g)) {
      stop(sprintf("theta has %d entries but g has %d", length(th), length(g)),
           call. = FALSE)
    }
    decay <- -lr * wd * th
  }
  upd <- adaptive + decay
  list(update = upd, state = st, m = st$m, v = st$v, decay = decay,
       t = st$t, step_norm = sqrt(sum(upd^2)), method = "AdamW")
}

#' Gradient-descent parameter update
#'
#' @param beta Current parameters.
#' @param grad Gradient at `beta`, same length.
#' @param alpha Learning rate, positive.
#' @return List with `beta` (updated), `update`, `step_norm`, `alpha`.
#' @references Cauchy, A. (1847). Comptes Rendus 25, 536-538.
#' @examples
#' morie_gradient_descent_update(c(0, 0), c(1, -2), 0.1)$beta
#' @export
morie_gradient_descent_update <- function(beta, grad, alpha = 0.01) {
  if (alpha <= 0) stop("alpha must be positive", call. = FALSE)
  beta <- as.numeric(beta)
  grad <- as.numeric(grad)
  if (length(beta) != length(grad)) {
    stop(sprintf("beta (%d) and grad (%d) must have the same length",
                 length(beta), length(grad)), call. = FALSE)
  }
  upd <- -alpha * grad
  list(beta = beta + upd, update = upd, step_norm = sqrt(sum(upd^2)),
       alpha = alpha, method = "gradient_descent_update")
}

#' Stochastic-gradient update on a mini-batch
#'
#' Averages the per-example gradients, which is what makes the step size
#' independent of the batch size. `grad_se` reports the standard error of that
#' mean across the batch.
#'
#' @param beta Current parameters, length p.
#' @param batch_grads Per-example gradients, a B-by-p matrix.
#' @param eta Learning rate, positive.
#' @return List with `beta`, `update`, `grad_mean`, `grad_se`, `batch_size`.
#' @references Robbins, H., & Monro, S. (1951). Ann. Math. Statist. 22(3),
#'   400-407.
#' @examples
#' morie_sgd_update(c(0, 0), rbind(c(1, 2), c(3, 4)), eta = 0.1)$grad_mean
#' @export
morie_sgd_update <- function(beta, batch_grads, eta = 0.01) {
  if (eta <= 0) stop("eta must be positive", call. = FALSE)
  beta <- as.numeric(beta)
  G <- if (is.matrix(batch_grads)) batch_grads else matrix(batch_grads, nrow = 1L)
  if (ncol(G) != length(beta)) {
    stop(sprintf("batch_grads has %d columns but beta has %d entries",
                 ncol(G), length(beta)), call. = FALSE)
  }
  B <- nrow(G)
  gbar <- colMeans(G)
  se <- if (B > 1L) apply(G, 2L, stats::sd) / sqrt(B) else rep(NA_real_, length(beta))
  upd <- -eta * gbar
  list(beta = beta + upd, update = upd, grad_mean = gbar, grad_se = se,
       batch_size = B, eta = eta, method = "sgd_update")
}

#' LARS layer-wise adaptive rate scaling
#'
#' Rescales each layer's step by the trust ratio
#' `eta * ||w|| / (||g|| + wd * ||w||)`, so the step length tracks the weight
#' norm. A zero weight norm falls back to an unscaled step.
#'
#' @param g Gradient for this layer.
#' @param w Current weights for this layer, same length as `g`.
#' @param lr Global learning rate.
#' @param mu Momentum coefficient in \[0, 1).
#' @param wd Weight decay, folded into the gradient (not decoupled).
#' @param eta Trust coefficient.
#' @param eps Denominator floor.
#' @param state List from the previous call, or `NULL`.
#' @return List with `update`, `state`, `trust_ratio`, `w_norm`, `g_norm`, `v`.
#' @references You, Y., Gitman, I., & Ginsburg, B. (2017). arXiv:1708.03888.
#' @examples
#' morie_lars(c(1, 1), c(0, 0), lr = 0.1, mu = 0)$trust_ratio
#' @export
morie_lars <- function(g, w, lr = 0.1, mu = 0.9, wd = 0, eta = 0.001,
                       eps = 1e-8, state = NULL) {
  if (mu < 0 || mu >= 1) stop("mu must be in [0, 1)", call. = FALSE)
  g <- .morie_opt_vec(g)
  w <- as.numeric(w)
  if (length(w) != length(g)) {
    stop(sprintf("w has %d entries but g has %d", length(w), length(g)),
         call. = FALSE)
  }
  wn <- sqrt(sum(w^2)); gn <- sqrt(sum(g^2))
  trust <- if (wn > 0 && gn > 0) eta * wn / (gn + wd * wn + eps) else 1
  st <- .morie_opt_state(state, length(g), "v")
  st$v <- mu * st$v + trust * (g + wd * w)
  upd <- -lr * st$v
  list(update = upd, state = st, trust_ratio = trust, w_norm = wn,
       g_norm = gn, v = st$v, t = st$t, step_norm = sqrt(sum(upd^2)),
       method = "LARS")
}

#' LAMB layer-wise adaptive moments
#'
#' The LARS trust ratio applied to Adam's direction rather than to the raw
#' gradient. A zero weight or direction norm falls back to a trust ratio of 1.
#'
#' @param g Gradient for this layer.
#' @param w Current weights for this layer, same length as `g`.
#' @param lr Global learning rate.
#' @param beta1,beta2 Moment decay rates in \[0, 1).
#' @param wd Weight decay applied to the Adam direction.
#' @param eps Denominator floor.
#' @param state List from the previous call, or `NULL`.
#' @return List with `update`, `state`, `trust_ratio`, `m`, `v`, `direction`.
#' @references You, Y., Li, J., Reddi, S., et al. (2020). ICLR 2020.
#' @examples
#' morie_lamb(c(1, 1), c(0, 0), lr = 0.1, wd = 0)$trust_ratio
#' @export
morie_lamb <- function(g, w, lr = 0.001, beta1 = 0.9, beta2 = 0.999,
                       wd = 0.01, eps = 1e-6, state = NULL) {
  if (beta1 < 0 || beta1 >= 1) stop("beta1 must be in [0, 1)", call. = FALSE)
  if (beta2 < 0 || beta2 >= 1) stop("beta2 must be in [0, 1)", call. = FALSE)
  g <- .morie_opt_vec(g)
  w <- as.numeric(w)
  if (length(w) != length(g)) {
    stop(sprintf("w has %d entries but g has %d", length(w), length(g)),
         call. = FALSE)
  }
  st <- .morie_opt_state(state, length(g), c("m", "v"))
  st$m <- beta1 * st$m + (1 - beta1) * g
  st$v <- beta2 * st$v + (1 - beta2) * g^2
  m_hat <- st$m / (1 - beta1^st$t)
  v_hat <- st$v / (1 - beta2^st$t)
  r <- m_hat / (sqrt(v_hat) + eps) + wd * w
  wn <- sqrt(sum(w^2)); rn <- sqrt(sum(r^2))
  trust <- if (wn > 0 && rn > 0) wn / rn else 1
  upd <- -lr * trust * r
  list(update = upd, state = st, trust_ratio = trust, w_norm = wn,
       r_norm = rn, m = st$m, v = st$v, direction = r, t = st$t,
       step_norm = sqrt(sum(upd^2)), method = "LAMB")
}

#' Newton step for unconstrained minimisation
#'
#' Solved as a linear system rather than by forming the inverse. Reports
#' whether the Hessian was positive definite, since away from a minimum the
#' Newton direction need not be a descent direction.
#'
#' @param grad Gradient, length n.
#' @param hess Hessian, n-by-n and symmetric.
#' @param ridge Optional shift added to the diagonal.
#' @return List with `step`, `decrement`, `is_descent`, `pd`.
#' @references Boyd, S., & Vandenberghe, L. (2004). Convex Optimization.
#'   Cambridge University Press, Sec 9.5.
#' @examples
#' A <- matrix(c(4, 1, 1, 3), 2)
#' x <- c(5, -2)
#' round(x + morie_boyd_newton(A %*% x, A)$step, 12)
#' @export
morie_boyd_newton <- function(grad, hess, ridge = 0) {
  g <- as.numeric(grad)
  H <- as.matrix(hess)
  n <- length(g)
  if (!all(dim(H) == c(n, n))) {
    stop(sprintf("hess must be (%d, %d) to match grad", n, n), call. = FALSE)
  }
  if (max(abs(H - t(H))) > 1e-10) stop("hess must be symmetric", call. = FALSE)
  if (ridge != 0) H <- H + ridge * diag(n)
  pd <- tryCatch({ chol(H); TRUE }, error = function(e) FALSE)
  step <- tryCatch(-solve(H, g),
                   error = function(e) -qr.solve(H, g, tol = 1e-12))
  list(step = step, decrement = sqrt(max(sum(g * -step), 0)),
       is_descent = sum(g * step) < 0, pd = pd, method = "boyd_newton")
}

#' Newton decrement
#'
#' `lambda^2 / 2` estimates `f(x) - p*` and is affine-invariant, unlike the
#' gradient norm -- which is why it is the standard stopping rule for Newton's
#' method.
#'
#' @param grad Gradient, length n.
#' @param hess Hessian, n-by-n, symmetric positive definite.
#' @return List with `decrement`, `suboptimality`, `grad_norm`.
#' @references Boyd, S., & Vandenberghe, L. (2004). Convex Optimization,
#'   Sec 9.5.1.
#' @examples
#' A <- matrix(c(4, 1, 1, 3), 2)
#' x <- c(5, -2)
#' morie_boyd_newton_decrement(A %*% x, A)$suboptimality
#' @export
morie_boyd_newton_decrement <- function(grad, hess) {
  g <- as.numeric(grad)
  H <- as.matrix(hess)
  n <- length(g)
  if (!all(dim(H) == c(n, n))) {
    stop(sprintf("hess must be (%d, %d) to match grad", n, n), call. = FALSE)
  }
  quad <- tryCatch(sum(g * solve(H, g)),
                   error = function(e)
                     stop("hess is singular; the Newton decrement is undefined",
                          call. = FALSE))
  if (quad < 0) {
    stop("grad' H^-1 grad is negative, so hess is not positive definite",
         call. = FALSE)
  }
  lam <- sqrt(quad)
  list(decrement = lam, suboptimality = lam^2 / 2,
       grad_norm = sqrt(sum(g^2)), method = "boyd_newton_decrement")
}

#' Backtracking line search
#'
#' Shrinks the step until the Armijo sufficient-decrease condition holds.
#' A non-descent direction is rejected rather than searched.
#'
#' @param f Objective, `f(x)` returning a scalar.
#' @param grad Gradient at `x`.
#' @param x Current point.
#' @param dx Search direction; must be a descent direction.
#' @param alpha Sufficient-decrease fraction in (0, 0.5).
#' @param beta Shrink factor in (0, 1).
#' @param max_iter Cap on backtracking steps.
#' @return List with `t`, `x_new`, `f_new`, `n_backtracks`, `converged`.
#' @references Boyd, S., & Vandenberghe, L. (2004). Convex Optimization,
#'   Algorithm 9.2.
#' @examples
#' f <- function(z) sum(z^2)
#' morie_boyd_backtracking(f, 2, 1, -2)$t
#' @export
morie_boyd_backtracking <- function(f, grad, x, dx, alpha = 0.25, beta = 0.5,
                                    max_iter = 100L) {
  if (alpha <= 0 || alpha >= 0.5) stop("alpha must be in (0, 0.5)", call. = FALSE)
  if (beta <= 0 || beta >= 1) stop("beta must be in (0, 1)", call. = FALSE)
  x <- as.numeric(x); g <- as.numeric(grad); d <- as.numeric(dx)
  if (length(x) != length(g) || length(x) != length(d)) {
    stop("x, grad and dx must all have the same length", call. = FALSE)
  }
  slope <- sum(g * d)
  if (slope >= 0) {
    stop(sprintf("dx is not a descent direction (grad %%*%% dx = %g)", slope),
         call. = FALSE)
  }
  f0 <- as.numeric(f(x))
  tt <- 1
  for (k in seq_len(max_iter)) {
    fx <- as.numeric(f(x + tt * d))
    if (is.finite(fx) && fx <= f0 + alpha * tt * slope) {
      return(list(t = tt, x_new = x + tt * d, f_new = fx, f_old = f0,
                  slope = slope, n_backtracks = k - 1L, converged = TRUE,
                  method = "boyd_backtracking"))
    }
    tt <- tt * beta
  }
  list(t = tt, x_new = x + tt * d, f_new = as.numeric(f(x + tt * d)),
       f_old = f0, slope = slope, n_backtracks = as.integer(max_iter),
       converged = FALSE, method = "boyd_backtracking")
}

#' Proximal operator
#'
#' Closed forms for the standard penalties. Note that `"l1"` soft-thresholds
#' each coordinate (giving sparsity) while `"l2"` shrinks the whole vector as
#' a block -- choosing the wrong one is the usual cause of a group lasso that
#' will not select groups.
#'
#' @param h One of "l1", "l2", "l2sq", "nonneg", "box", "zero".
#' @param v Point at which to evaluate.
#' @param t Step size, positive.
#' @param lo,hi Box bounds, required when `h = "box"`.
#' @return List with `prox`, `moreau`, `h_value`, `h_name`.
#' @references Parikh, N., & Boyd, S. (2014). Proximal algorithms.
#'   Foundations and Trends in Optimization 1(3), 127-239.
#' @examples
#' morie_boyd_proximal("l1", c(3, -0.5, 0.2), t = 1)$prox
#' @export
morie_boyd_proximal <- function(h, v, t = 1, lo = NULL, hi = NULL) {
  if (t <= 0) stop("t must be positive", call. = FALSE)
  v <- as.numeric(v)
  x <- switch(h,
    l1     = sign(v) * pmax(abs(v) - t, 0),
    l2     = { nv <- sqrt(sum(v^2)); if (nv <= t) numeric(length(v)) else (1 - t / nv) * v },
    l2sq   = v / (1 + 2 * t),
    nonneg = pmax(v, 0),
    box    = {
      if (is.null(lo) || is.null(hi)) stop('h = "box" requires both lo and hi', call. = FALSE)
      if (lo > hi) stop("lo must not exceed hi", call. = FALSE)
      pmin(pmax(v, lo), hi)
    },
    zero   = v,
    stop(sprintf("unknown h '%s'", h), call. = FALSE)
  )
  hx <- switch(h,
    l1 = sum(abs(x)), l2 = sqrt(sum(x^2)), l2sq = sum(x^2),
    nonneg = if (all(x >= 0)) 0 else Inf,
    box = if (all(x >= lo & x <= hi)) 0 else Inf,
    0
  )
  list(prox = x, moreau = hx + sum((x - v)^2) / (2 * t), h_value = hx,
       h_name = h, t = t, method = "boyd_proximal")
}
