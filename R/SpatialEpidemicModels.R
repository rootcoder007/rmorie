.sem_rhs <- function(S, I, N, C, beta, gamma) {
  force <- as.vector(C %*% (I / N))
  list(-beta * S * force, beta * S * force - gamma * I, gamma * I)
}

#' Spatial epidemic and cluster models
#'
#' `spatial_sir` integrates the metapopulation SIR model with coupling
#' `C = (1 - kappa) I + kappa W~` (row-standardised weights) by classical
#' fourth-order Runge-Kutta (Keeling and Rohani 2008, sec. 7.3); the
#' statistic is the final attack rate.
#'
#' @param data Initially infected counts per patch.
#' @param W Adjacency matrix (NULL: isolated patches).
#' @param N Patch populations (default 1000 each).
#' @param beta,gamma Transmission and recovery rates.
#' @param kappa Share of the infection pressure from neighbours.
#' @param t_max,dt Horizon and step.
#' @param method Only "rk4".
#' @return List with `statistic` (attack rate), `times`, `S`, `I`, `R`
#'   (lists of patch vectors), `attack_rate`, `R0_local`.
#' @references Keeling, M. J. and Rohani, P. (2008). Modeling Infectious
#'   Diseases in Humans and Animals. Princeton University Press.
#' @examples
#' W <- rbind(c(0, 1, 0), c(1, 0, 1), c(0, 1, 0))
#' spatial_sir(c(10, 0, 0), W, rep(1000, 3), beta = 0.6, gamma = 0.2, t_max = 60, dt = 0.5)$statistic
#' @export
spatial_sir <- function(data, W = NULL, N = NULL, beta = 0.5, gamma = 0.2, kappa = 0.1, t_max = 100, dt = 0.1,
                        method = "rk4") {
  if (method != "rk4") stop("method must be 'rk4'")
  I <- as.numeric(data)
  n <- length(I)
  N <- if (is.null(N)) rep(1000, n) else as.numeric(N)
  A <- if (is.null(W)) matrix(0, n, n) else unname(as.matrix(W)) * 1
  rs <- rowSums(A)
  Wt <- A / ifelse(rs > 0, rs, 1)
  C <- (1 - kappa) * diag(n) + kappa * Wt
  S <- N - I
  R <- numeric(n)
  steps <- round(t_max / dt)
  Ss <- list(S)
  Is <- list(I)
  Rs <- list(R)
  for (k in seq_len(steps)) {
    k1 <- .sem_rhs(S, I, N, C, beta, gamma)
    k2 <- .sem_rhs(S + 0.5 * dt * k1[[1]], I + 0.5 * dt * k1[[2]], N, C, beta, gamma)
    k3 <- .sem_rhs(S + 0.5 * dt * k2[[1]], I + 0.5 * dt * k2[[2]], N, C, beta, gamma)
    k4 <- .sem_rhs(S + dt * k3[[1]], I + dt * k3[[2]], N, C, beta, gamma)
    S <- S + dt / 6 * (k1[[1]] + 2 * k2[[1]] + 2 * k3[[1]] + k4[[1]])
    I <- I + dt / 6 * (k1[[2]] + 2 * k2[[2]] + 2 * k3[[2]] + k4[[2]])
    R <- R + dt / 6 * (k1[[3]] + 2 * k2[[3]] + 2 * k3[[3]] + k4[[3]])
    Ss[[k + 1]] <- S
    Is[[k + 1]] <- I
    Rs[[k + 1]] <- R
  }
  list(statistic = sum(R) / sum(N), times = (0:steps) * dt, S = Ss, I = Is, R = Rs, attack_rate = sum(R) / sum(N),
       R0_local = beta / gamma)
}

.sem_newton <- function(grad, obj, x0, tol = 1e-10, maxit = 200) {
  x <- x0
  k <- length(x)
  f <- obj(x)
  for (it in seq_len(maxit)) {
    g <- grad(x)
    H <- matrix(0, k, k)
    for (j in seq_len(k)) {
      h <- 1e-5 * max(1, abs(x[j]))
      xp <- x
      xm <- x
      xp[j] <- xp[j] + h
      xm[j] <- xm[j] - h
      H[j, ] <- (grad(xp) - grad(xm)) / (2 * h)
    }
    H <- (H + t(H)) / 2
    step <- solve(-H, g)
    s <- 1
    repeat {
      xn <- x + s * step
      fn <- obj(xn)
      if (!is.nan(fn) && fn >= f - 1e-12 * abs(f)) break
      s <- s / 2
      if (s < 1e-12) {
        xn <- x
        fn <- f
        break
      }
    }
    done <- max(abs(s * step)) < tol
    x <- xn
    f <- fn
    if (done) break
  }
  list(x = x, f = f, H = H, it = it)
}

.sem_prep <- function(time, event, X, region, W) {
  A <- unname(as.matrix(W)) * 1
  list(t = as.numeric(time), d = as.numeric(event),
       X = if (is.null(X)) matrix(0, length(time), 0) else unname(as.matrix(X)) * 1,
       reg = as.integer(region) + 1L, Q = diag(rowSums(A), nrow(A)) - A)
}

#' @rdname spatial_sir
#' @param time,event Survival times and event indicators.
#' @param X Covariate matrix.
#' @param region 0-based region of each subject.
#' @param tau ICAR precision of the regional effects.
#' @param tol Newton convergence tolerance.
#' @details `spatial_frailty` fits a Weibull proportional-hazards model with
#'   intrinsic-CAR regional log-frailties by penalised likelihood, and
#'   `spatial_cure_rate` the promotion-time cure model with Weibull promotion
#'   times and ICAR regional effects, both by Newton-Raphson on the analytic
#'   gradient with a central-difference Hessian (Banerjee, Wall and Carlin
#'   2003; Chen, Ibrahim and Sinha 1999).
#' @export
spatial_frailty <- function(time, event, X, region, W, tau = 1, tol = 1e-10) {
  s <- .sem_prep(time, event, X, region, W)
  p <- ncol(s$X)
  R <- nrow(s$Q)
  lt <- log(s$t)
  parts <- function(th) {
    u <- th[1 + p + seq_len(R)]
    eta <- as.vector(s$X %*% th[1 + seq_len(p)]) + u[s$reg]
    list(a = th[1], rho = exp(th[1]), u = u, eta = eta, Hc = exp(exp(th[1]) * lt + eta))
  }
  obj <- function(th) {
    q <- parts(th)
    sum(s$d * (q$a + (q$rho - 1) * lt + q$eta) - q$Hc) - 0.5 * tau * sum(q$u * (s$Q %*% q$u))
  }
  grad <- function(th) {
    q <- parts(th)
    e <- s$d - q$Hc
    c(sum(s$d * (1 + q$rho * lt) - q$Hc * q$rho * lt), as.vector(crossprod(s$X, e)),
      vapply(seq_len(R), function(r) sum(e[s$reg == r]), 0) - tau * as.vector(s$Q %*% q$u))
  }
  o <- .sem_newton(grad, obj, rep(0, 1 + p + R), tol = tol)
  V <- solve(-o$H)
  list(statistic = o$f, shape = exp(o$x[1]), coefficients = o$x[1 + seq_len(p)], frailties = o$x[1 + p + seq_len(R)],
       se = ifelse(diag(V) > 0, sqrt(pmax(diag(V), 0)), NaN), iterations = o$it, tau = tau)
}

#' @rdname spatial_sir
#' @export
spatial_cure_rate <- function(time, event, X, region, W, tau = 1, tol = 1e-10) {
  s <- .sem_prep(time, event, X, region, W)
  p <- ncol(s$X)
  R <- nrow(s$Q)
  lt <- log(s$t)
  parts <- function(th) {
    u <- th[2 + p + seq_len(R)]
    eta <- as.vector(s$X %*% th[2 + seq_len(p)]) + u[s$reg]
    list(a = th[1], c = th[2], rho = exp(th[1]), u = u, eta = eta, G = exp(th[2] + exp(th[1]) * lt))
  }
  obj <- function(th) {
    q <- parts(th)
    sum(s$d * (q$eta + q$a + q$c + (q$rho - 1) * lt - q$G) - exp(q$eta) * (1 - exp(-q$G))) -
      0.5 * tau * sum(q$u * (s$Q %*% q$u))
  }
  grad <- function(th) {
    q <- parts(th)
    thv <- exp(q$eta)
    dF <- exp(-q$G) * q$G
    e <- s$d - thv * (1 - exp(-q$G))
    c(sum(s$d * (1 + q$rho * lt - q$G * q$rho * lt) - thv * dF * q$rho * lt), sum(s$d * (1 - q$G) - thv * dF),
      as.vector(crossprod(s$X, e)), vapply(seq_len(R), function(r) sum(e[s$reg == r]), 0) - tau * as.vector(s$Q %*% q$u))
  }
  o <- .sem_newton(grad, obj, rep(0, 2 + p + R), tol = tol)
  V <- solve(-o$H)
  b <- o$x[2 + seq_len(p)]
  u <- o$x[2 + p + seq_len(R)]
  list(statistic = o$f, shape = exp(o$x[1]), log_scale = o$x[2], coefficients = b, regional_effects = u,
       se = ifelse(diag(V) > 0, sqrt(pmax(diag(V), 0)), NaN),
       cure_fraction = exp(-exp(as.vector(s$X %*% b) + u[s$reg])), iterations = o$it, tau = tau)
}
