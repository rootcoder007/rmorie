# poissp.R -- function file (rootcoder007/morie)
# Poisson areal regression with a conditionally autoregressive effect.
#
# Y_i | mu_i ~ Poisson(E_i mu_i), log mu_i = x_i' beta + u_i, with E a
# known offset entering at coefficient one, so exp(eta) is a relative
# risk. The spatial term is the proper CAR with precision
# Q = tau (D_w - rho W); rho = 1 is the intrinsic CAR, whose precision is
# singular (Q 1 = 0) and which therefore needs sum(u) = 0.
#
# The fixed effects are unpenalised, so X'(y - m) = 0 at the mode: with
# an intercept column that forces sum(y) = sum(fitted) exactly, which is
# the anchor this arm is checked against.
#
# References:
# Banerjee, S., Carlin, B. P. and Gelfand, A. E. (2014) Hierarchical
# Modeling and Analysis for Spatial Data, 2nd edn, Monographs on
# Statistics and Applied Probability 135, Chapman & Hall/CRC, Boca Raton,
# ISBN 978-1-4398-1917-3 -- Ch. 4 (proper and intrinsic CAR) and Ch. 6
# (Poisson-CAR disease mapping).
# Besag, J., York, J. and Mollie, A. (1991) "Bayesian image restoration,
# with two applications in spatial statistics", Annals of the Institute
# of Statistical Mathematics 43(1), 1-20, doi:10.1007/BF00116466.

#' .poissp_adjacency
#'
#' A step of the poissp_native implementation. Called by \code{morie_poissp_car_precision}, \code{morie_poissp_rho_bounds}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param W A matrix; passed to \code{as.matrix}.
#' @return The value of \code{A}, as built in the body.
#' @export
.poissp_adjacency <- function(W) {
  A <- as.matrix(W)
  storage.mode(A) <- "double"
  n <- nrow(A)
  if (n == 0L || ncol(A) != n) {
    stop("poissp: W must be a square weight matrix")
  }
  for (i in seq_len(n)) {
    if (A[i, i] != 0) {
      stop(sprintf("poissp: W must have a zero diagonal; area %d is its own neighbour",
                   i - 1L))
    }
    for (j in seq_len(n)) {
      if (A[i, j] < 0) stop("poissp: weights must be non-negative")
      if (abs(A[i, j] - A[j, i]) > 1e-12) {
        stop(sprintf("poissp: W must be symmetric; w[%d][%d] and w[%d][%d] differ",
                     i - 1L, j - 1L, j - 1L, i - 1L))
      }
    }
  }
  A
}

#' The proper-CAR precision Q = tau (D_w - rho W).
#' @export
morie_poissp_car_precision <- function(W, tau = 1.0, rho = 1.0) {
  A <- .poissp_adjacency(W)
  n <- nrow(A)
  d <- rowSums(A)
  t <- as.numeric(tau)
  r <- as.numeric(rho)
  if (t <= 0) stop("poissp: tau must be positive")
  t * (diag(d, n, n) - r * A)
}

#' The propriety interval for rho.
#' @export
morie_poissp_rho_bounds <- function(W) {
  A <- .poissp_adjacency(W)
  n <- nrow(A)
  d <- rowSums(A)
  if (any(d <= 0)) stop("poissp: every area needs at least one neighbour")
  S <- A / sqrt(outer(d, d))
  ev <- sort(eigen(S, symmetric = TRUE)$values)
  lo <- min(ev)
  hi <- max(ev)
  list(lower = if (lo < 0) 1 / lo else -Inf,
       upper = if (hi > 0) 1 / hi else Inf,
       eigenvalues = ev)
}

# weight of the sum(u) = 0 penalty
#' Weight of the sum(u) = 0 penalty
#'
#' A step of the poissp_native implementation. Called by \code{.poissp_fit_mode}, \code{.poissp_joint_hessian}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param m A vector; its length is taken.
#' @return A numeric value.
#' @export
.poissp_constraint_weight <- function(m) {
  1e8 * max(1.0, if (length(m)) max(m) else 1.0)
}

# negative joint Hessian [[X'MX, X'M], [MX, M + Q]] (+ constraint block)
#' Negative joint Hessian [[X\'MX, X\'M], [MX, M + Q]] (+ constraint
#' block)
#'
#' A step of the poissp_native implementation. Called by \code{.poissp_fit_mode}, \code{morie_poissp}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; passed to \code{ncol}.
#' @param m A matrix; passed to \code{diag}.
#' @param Q Numeric; combined arithmetically in the body.
#' @param constrain A flag; the body branches on it.
#' @return The value of \code{H}, as built in the body.
#' @export
.poissp_joint_hessian <- function(X, m, Q, constrain) {
  n <- length(m)
  p <- ncol(X)
  H <- matrix(0.0, p + n, p + n)
  H[1:p, 1:p] <- crossprod(X, X * m)
  XM <- t(X * m)                      # p x n
  H[1:p, (p + 1L):(p + n)] <- XM
  H[(p + 1L):(p + n), 1:p] <- t(XM)
  H[(p + 1L):(p + n), (p + 1L):(p + n)] <- Q + diag(m, n, n)
  if (isTRUE(constrain)) {
    H[(p + 1L):(p + n), (p + 1L):(p + n)] <-
      H[(p + 1L):(p + n), (p + 1L):(p + n)] + .poissp_constraint_weight(m)
  }
  H
}

#' .poissp_ridgesolve
#'
#' A step of the poissp_native implementation. Called by \code{.poissp_fit_mode}, \code{morie_poissp}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A A matrix; passed to \code{nrow}.
#' @param b A matrix; passed to \code{solve}.
#' @param ridge Numeric; combined arithmetically in the body. Defaults to \code{1e-10}.
#' @return A vector, from \code{as.numeric}.
#' @export
.poissp_ridgesolve <- function(A, b, ridge = 1e-10) {
  as.numeric(solve(A + ridge * diag(nrow(A)), b))
}

# joint Newton-Raphson for (beta, u) at fixed (tau, rho)
#' Joint Newton-Raphson for (beta, u) at fixed (tau, rho)
#'
#' A step of the poissp_native implementation. Called by \code{morie_poissp}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y A vector; its length is taken.
#' @param X A matrix; indexed by row and column.
#' @param off Numeric; passed to \code{sum}.
#' @param Q A matrix; passed to \code{\%*\%}.
#' @param constrain A flag; the body branches on it.
#' @param iters Coerced to integer by the body, with \code{as.integer}.
#' @param tol See Usage.
#' @param ridge Passed to \code{.poissp_ridgesolve}.
#' @return A list with \code{beta}, \code{u}, \code{m}, \code{eta}.
#' @export
.poissp_fit_mode <- function(y, X, off, Q, constrain, iters, tol, ridge) {
  n <- length(y)
  p <- ncol(X)
  beta <- numeric(p)
  # start the intercept at its exact offset-only MLE
  tot_y <- sum(y)
  tot_e <- sum(off)
  if (p > 0L && all(abs(X[, 1] - 1) < 1e-12)) {
    beta[1] <- if (tot_y > 0 && tot_e > 0) log(tot_y / tot_e) else 0.0
  }
  u <- numeric(n)
  for (it in seq_len(as.integer(iters))) {
    eta <- as.numeric(X %*% beta) + u
    m <- off * exp(eta)
    r <- y - m
    g <- c(as.numeric(crossprod(X, r)), r - as.numeric(Q %*% u))
    H <- .poissp_joint_hessian(X, m, Q, constrain)
    if (isTRUE(constrain)) {
      big <- .poissp_constraint_weight(m)
      g[(p + 1L):(p + n)] <- g[(p + 1L):(p + n)] - big * sum(u)
    }
    step <- .poissp_ridgesolve(H, g, ridge)
    beta <- beta + step[1:p]
    u <- u + step[(p + 1L):(p + n)]
    if (max(abs(step)) < tol) break
  }
  eta <- as.numeric(X %*% beta) + u
  m <- off * exp(eta)
  list(beta = beta, u = u, m = m, eta = eta)
}

#' .poissp_loglik
#'
#' A step of the poissp_native implementation. Called by \code{.poissp_laplace}, \code{morie_poissp}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Numeric; combined arithmetically in the body.
#' @param m Numeric; passed to \code{log}.
#' @return A numeric value.
#' @export
.poissp_loglik <- function(y, m) {
  sum(y * log(m) - m - lgamma(y + 1))
}

#' .poissp_logdet_pd
#'
#' A step of the poissp_native implementation. Called by \code{.poissp_laplace}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A A matrix; passed to \code{nrow}.
#' @param ridge Numeric; combined arithmetically in the body. Defaults to \code{0}.
#' @return A numeric value.
#' @export
.poissp_logdet_pd <- function(A, ridge = 0.0) {
  L <- chol(A + ridge * diag(nrow(A)))
  2 * sum(log(diag(L)))
}

# generalised log-determinant: drop the rank_deficit smallest |eigenvalues|
#' Generalised log-determinant: drop the rank_deficit smallest
#' |eigenvalues|
#'
#' A step of the poissp_native implementation. Called by \code{.poissp_laplace}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A See Usage.
#' @param rank_deficit Numeric; combined arithmetically in the body. Defaults to \code{0L}.
#' @return A numeric value.
#' @export
.poissp_logdet_gen <- function(A, rank_deficit = 0L) {
  ev <- sort(abs(eigen(A, symmetric = TRUE)$values), decreasing = TRUE)
  keep <- if (rank_deficit > 0L) ev[seq_len(length(ev) - rank_deficit)] else ev
  sum(log(keep[keep > 1e-300]))
}

#' .poissp_laplace
#'
#' A step of the poissp_native implementation. Called by \code{morie_poissp}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y A vector; its length is taken.
#' @param u A matrix; passed to \code{\%*\%}.
#' @param m A matrix; passed to \code{diag}.
#' @param Q A matrix; passed to \code{\%*\%}.
#' @param constrain A flag; the body branches on it.
#' @return A numeric value.
#' @export
.poissp_laplace <- function(y, u, m, Q, constrain) {
  n <- length(y)
  quad <- sum(u * as.numeric(Q %*% u))
  deficit <- if (isTRUE(constrain)) 1L else 0L
  ldQ <- .poissp_logdet_gen(Q, deficit)
  ldH <- .poissp_logdet_pd(Q + diag(m, n, n), 1e-12)
  .poissp_loglik(y, m) - 0.5 * quad + 0.5 * ldQ - 0.5 * ldH
}

#' Fit the Poisson-CAR areal model.
#'
#' @references
#' Banerjee, S., Carlin, B. P. and Gelfand, A. E. (2014) Hierarchical
#' Modeling and Analysis for Spatial Data, 2nd edn, Chapman and Hall/CRC.
#' @export
morie_poissp <- function(counts, X = NULL, offset = NULL, W = NULL,
                         rho = 1.0, tau = NULL, constrain = NULL,
                         iters = 100L, tol = 1e-11, ridge = 1e-10,
                         tau_grid = NULL, level = 0.95) {
  y <- as.numeric(counts)
  n <- length(y)
  if (n == 0L) stop("poissp: no observations")
  if (any(y < 0)) stop("poissp: counts must be non-negative")
  if (any(abs(y - round(y)) > 1e-9)) stop("poissp: counts must be integers")
  off <- if (is.null(offset)) rep(1.0, n) else as.numeric(offset)
  if (length(off) != n) {
    stop(sprintf("poissp: %d counts but %d offsets", n, length(off)))
  }
  if (any(off <= 0)) stop("poissp: offsets must be positive")
  Xd <- if (is.null(X)) matrix(1.0, n, 1L) else cbind(1.0, as.matrix(X))
  if (nrow(Xd) != n) {
    stop(sprintf("poissp: %d counts but %d covariate rows", n, nrow(Xd)))
  }
  storage.mode(Xd) <- "double"

  if (is.null(W)) {
    Q <- diag(1e12, n, n)          # pin u at zero
    rho_used <- NULL
    tau_used <- NULL
    spatial <- FALSE
    constrain <- FALSE
  } else {
    spatial <- TRUE
    rho_used <- as.numeric(rho)
    if (is.null(constrain)) constrain <- abs(rho_used - 1) < 1e-12
    if (abs(rho_used - 1) > 1e-12) {
      b <- morie_poissp_rho_bounds(W)
      if (!(b$lower < rho_used && rho_used < b$upper)) {
        stop(sprintf(paste0("poissp: rho = %g is outside the propriety ",
                            "interval (%g, %g); the CAR prior would be improper"),
                     rho_used, b$lower, b$upper))
      }
    }
    if (is.null(tau)) {
      grid <- if (!is.null(tau_grid)) tau_grid else
        c(0.05, 0.1, 0.25, 0.5, 1.0, 2.0, 5.0, 10.0, 25.0, 50.0, 100.0)
      best <- NULL
      for (t in grid) {
        Qt <- morie_poissp_car_precision(W, t, rho_used)
        fit <- .poissp_fit_mode(y, Xd, off, Qt, constrain, iters, tol, ridge)
        lap <- .poissp_laplace(y, fit$u, fit$m, Qt, constrain)
        if (is.null(best) || lap > best$lap) best <- list(lap = lap, tau = t)
      }
      tau_used <- best$tau
    } else {
      tau_used <- as.numeric(tau)
      if (tau_used <= 0) stop("poissp: tau must be positive")
    }
    Q <- morie_poissp_car_precision(W, tau_used, rho_used)
  }

  fit <- .poissp_fit_mode(y, Xd, off, Q, constrain, iters, tol, ridge)
  beta <- fit$beta; u <- fit$u; m <- fit$m; eta <- fit$eta
  p <- ncol(Xd)
  score <- as.numeric(crossprod(Xd, y - m))

  # (beta, beta) block of the INVERSE joint Hessian, by solving H z = e_a.
  # Forming the Schur complement explicitly loses every digit when the CAR
  # precision is large (the tau -> infinity anchor turns it indefinite).
  H <- .poissp_joint_hessian(Xd, m, Q, constrain)
  cov <- matrix(0.0, p, p)
  for (a in seq_len(p)) {
    e <- numeric(p + n)
    e[a] <- 1.0
    z <- .poissp_ridgesolve(H, e, ridge)
    cov[a, ] <- z[1:p]
  }
  se <- sqrt(ifelse(diag(cov) > 0, diag(cov), NA_real_))
  z <- stats::qnorm(0.5 + 0.5 * as.numeric(level))
  lo <- beta - z * se
  hi <- beta + z * se

  ll <- .poissp_loglik(y, m)
  dev <- 2 * sum(ifelse(y > 0, y * log(y / m), 0) - (y - m))

  list(
    estimate = beta,
    beta = beta,
    se = se,
    lower = lo,
    upper = hi,
    u = u,
    eta = eta,
    fitted = m,
    relative_risk = exp(eta),
    score_beta = score,
    loglik = ll,
    deviance = dev,
    tau = tau_used,
    rho = rho_used,
    spatial = spatial,
    constrained = isTRUE(constrain),
    n = n,
    p = p,
    level = as.numeric(level),
    method = sprintf(paste0("Poisson areal regression with a %s CAR effect, ",
                            "Banerjee, Carlin & Gelfand (2014) Ch. 4 and 6"),
                     if (spatial && abs((if (is.null(rho_used)) 0 else rho_used) - 1) < 1e-12)
                       "intrinsic" else if (spatial) "proper" else "no"),
    note = paste0("the offset enters with coefficient fixed at one, so ",
                  "exp(eta) is a relative risk; score_beta is zero at the ",
                  "mode because the fixed effects are unpenalised")
  )
}

#' @rdname morie_poissp
#' @export
morie_poisson_spatial_glm <- morie_poissp

#' morie_poissp_cheatsheet
#'
#' A step of the poissp_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
morie_poissp_cheatsheet <- function() {
  paste0("poissp: Poisson areal regression, log mu = X beta + u with a ",
         "known offset E entering at coefficient one, and u ~ CAR with ",
         "precision tau(D_w - rho W). rho=1 is the INTRINSIC CAR: Q1=0, ",
         "improper, needs sum(u)=0. The fixed-effect score X'(y-m) is ",
         "zero at the mode, so an intercept forces sum(y)=sum(fitted). ",
         "tau by Laplace marginal likelihood. BCG (2014) Ch. 4, 6.")
}
