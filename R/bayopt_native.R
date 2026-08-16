# bayopt -- Bayesian optimisation of an expensive black-box function
# Mockus (1975); Snoek, Larochelle & Adams (2012) "Practical Bayesian
# Optimization of Machine Learning Algorithms", NIPS 25.
# Base R only.

# --------------------------------------------------------------------------
# helpers
# --------------------------------------------------------------------------

#' .bayopt_phi
#'
#' A step of the bayopt_native implementation. Called by \code{acquisition_gradient}, \code{expected_improvement}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param z Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.bayopt_phi <- function(z) exp(-0.5 * z * z) / sqrt(2 * pi)

#' .Phi
#'
#' A step of the bayopt_native implementation. Called by \code{acquisition_gradient}, \code{expected_improvement}, \code{probability_of_improvement}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param z See Usage.
#' @return The value of \code{pnorm}.
#' @export
.Phi <- function(z) pnorm(z)

#' .lengths
#'
#' A step of the bayopt_native implementation. Called by \code{gp_posterior_gradient}, \code{matern52}, \code{squared_exponential}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param ls A vector; its length is taken.
#' @param d A count; the body uses it as \code{rep(...)}.
#' @return The value of \code{out}, as built in the body.
#' @export
.lengths <- function(ls, d) {
  if (is.numeric(ls) && length(ls) == 1L) {
    out <- rep(as.numeric(ls), d)
  } else {
    out <- as.numeric(ls)
    if (length(out) != d) {
      stop("bayopt: length_scale must be a scalar or one value per dimension")
    }
  }
  if (any(out <= 0)) stop("bayopt: length scales must be positive")
  out
}

#' .r2
#'
#' A step of the bayopt_native implementation. Called by \code{gp_posterior_gradient}, \code{matern52}, \code{squared_exponential}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a A vector; its length is taken and its elements indexed.
#' @param b A vector; indexed elementwise.
#' @param ls A vector; indexed elementwise.
#' @return The value of \code{s}, as built in the body.
#' @export
.r2 <- function(a, b, ls) {
  d <- length(a)
  s <- 0
  for (i in 1:d) s <- s + (a[i] - b[i])^2 / (ls[i]^2)
  s
}

# --------------------------------------------------------------------------
# kernels
# --------------------------------------------------------------------------

#' matern52
#'
#' A step of the bayopt_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a A vector; its length is taken.
#' @param b Passed to \code{.r2}.
#' @param amplitude Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @param length_scale Passed to \code{.lengths}. Defaults to \code{1}.
#' @return A numeric value.
#' @export
matern52 <- function(a, b, amplitude = 1, length_scale = 1) {
  d <- length(a)
  ls <- .lengths(length_scale, d)
  r2 <- .r2(a, b, ls)
  s <- sqrt(5 * r2)
  amplitude * (1 + s + (5/3) * r2) * exp(-s)
}

#' squared_exponential
#'
#' A step of the bayopt_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a A vector; its length is taken.
#' @param b Passed to \code{.r2}.
#' @param amplitude Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @param length_scale Passed to \code{.lengths}. Defaults to \code{1}.
#' @return A numeric value.
#' @export
squared_exponential <- function(a, b, amplitude = 1, length_scale = 1) {
  d <- length(a)
  ls <- .lengths(length_scale, d)
  amplitude * exp(-0.5 * .r2(a, b, ls))
}

#' .dkernel_dr2
#'
#' A step of the bayopt_native implementation. Called by \code{gp_posterior_gradient}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param name Compared against \code{"se"}.
#' @param amplitude Numeric; combined arithmetically in the body.
#' @param r2 Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.dkernel_dr2 <- function(name, amplitude, r2) {
  if (name == "se") return(-0.5 * amplitude * exp(-0.5 * r2))
  s <- sqrt(5 * r2)
  -(5/6) * amplitude * (1 + s) * exp(-s)
}

#' .kernel
#'
#' A step of the bayopt_native implementation. Called by \code{gp_posterior}, \code{gp_posterior_gradient}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param name One of \code{"matern52"}, \code{"se"}.
#' @return One of two values, depending on the branch taken.
#' @export
.kernel <- function(name) {
  if (!(name %in% c("matern52", "se")))
    stop("bayopt: kernel must be one of matern52, se")
  if (name == "matern52") matern52 else squared_exponential
}

# --------------------------------------------------------------------------
# Cholesky
# --------------------------------------------------------------------------

#' .chol_r
#'
#' A step of the bayopt_native implementation. Called by \code{gp_posterior}, \code{gp_posterior_gradient}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A A matrix; indexed by row and column.
#' @return The value of \code{L}, as built in the body.
#' @export
.chol_r <- function(A) {
  A <- as.matrix(A)
  n <- nrow(A)
  L <- matrix(0, n, n)
  for (i in 1:n) {
    for (j in 1:i) {
      s <- A[i, j] - sum(L[i, 1:(j - 1)] * L[j, 1:(j - 1)])
      if (i == j) {
        if (s <= 0) {
          stop(paste("bayopt: the covariance matrix is not positive",
                     "definite; add noise or spread the design points"))
        }
        L[i, j] <- sqrt(s)
      } else {
        L[i, j] <- s / L[j, j]
      }
    }
  }
  L
}

#' .chol_solve
#'
#' A step of the bayopt_native implementation. Called by \code{gp_posterior}, \code{gp_posterior_gradient}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param L A matrix; indexed by row and column.
#' @param b A vector; indexed elementwise.
#' @return The value of \code{x}, as built in the body.
#' @export
.chol_solve <- function(L, b) {
  n <- nrow(L)
  y <- numeric(n)
  # seq_len, and a guard on the back-substitution: 1:(i - 1) is c(1, 0)
  # at i = 1, and (i + 1):n counts DOWN at i = n, so both ends of this
  # solve read the wrong entries.
  for (i in seq_len(n))
    y[i] <- (b[i] - sum(L[i, seq_len(i - 1L)] * y[seq_len(i - 1L)])) / L[i, i]
  x <- numeric(n)
  for (i in n:1L) {
    s <- 0.0
    if (i < n) s <- sum(L[(i + 1L):n, i] * x[(i + 1L):n])
    x[i] <- (y[i] - s) / L[i, i]
  }
  x
}

# --------------------------------------------------------------------------
# GP posterior
# --------------------------------------------------------------------------

#' gp_posterior
#'
#' A step of the bayopt_native implementation. Called by \code{bayopt}, \code{maximise_acquisition}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; passed to \code{as.matrix}.
#' @param y See Usage.
#' @param Xs A matrix; passed to \code{as.matrix}.
#' @param kernel Passed to \code{.kernel}. Defaults to \code{"matern52"}.
#' @param amplitude Defaults to \code{1}.
#' @param length_scale Defaults to \code{1}.
#' @param noise Defaults to \code{1e-08}.
#' @param mean Defaults to \code{NULL}.
#' @return A list with \code{mean}, \code{variance}, \code{sd}.
#' @export
gp_posterior <- function(X, y, Xs, kernel = "matern52", amplitude = 1,
                         length_scale = 1, noise = 1e-8, mean = NULL) {
  rows <- as.matrix(X)
  storage.mode(rows) <- "double"
  if (nrow(rows) == 0) stop("bayopt: no observations")
  d <- ncol(rows)
  ys <- as.numeric(y)
  if (length(ys) != nrow(rows))
    stop("bayopt: one observation per design point")
  if (noise < 0) stop("bayopt: noise must be non-negative")
  k <- .kernel(kernel)
  m <- if (is.null(mean)) mean(ys) else as.numeric(mean)
  n <- nrow(rows)
  K <- matrix(0, n, n)
  for (i in 1:n) for (j in 1:n) {
    K[i, j] <- k(rows[i, ], rows[j, ], amplitude, length_scale) +
      (if (i == j) noise else 0)
  }
  L <- .chol_r(K)
  alpha <- .chol_solve(L, ys - m)
  Xsm <- as.matrix(Xs)
  storage.mode(Xsm) <- "double"
  if (ncol(Xsm) != d)
    stop("bayopt: a query point has the wrong dimension")
  ns <- nrow(Xsm)
  out_m <- numeric(ns); out_v <- numeric(ns); out_sd <- numeric(ns)
  for (s_ in 1:ns) {
    q <- Xsm[s_, ]
    ks <- numeric(n)
    for (i in 1:n) ks[i] <- k(q, rows[i, ], amplitude, length_scale)
    mu_s <- m + sum(ks * alpha)
    v <- .chol_solve(L, ks)
    var_s <- k(q, q, amplitude, length_scale) - sum(ks * v)
    var_s <- max(var_s, 0)
    out_m[s_] <- mu_s
    out_v[s_] <- var_s
    out_sd[s_] <- sqrt(var_s)
  }
  list(mean = out_m, variance = out_v, sd = out_sd)
}

# --------------------------------------------------------------------------
# gradients
# --------------------------------------------------------------------------

#' gp_posterior_gradient
#'
#' A step of the bayopt_native implementation. Called by \code{maximise_acquisition}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; passed to \code{as.matrix}.
#' @param y See Usage.
#' @param xs See Usage.
#' @param kernel Passed to \code{.kernel}. Defaults to \code{"matern52"}.
#' @param amplitude Passed to \code{.dkernel_dr2}. Defaults to \code{1}.
#' @param length_scale Passed to \code{.lengths}. Defaults to \code{1}.
#' @param noise Defaults to \code{1e-08}.
#' @param mean Defaults to \code{NULL}.
#' @return A list with \code{grad_mu}, \code{grad_sd}, \code{mu}, \code{sd}.
#' @export
gp_posterior_gradient <- function(X, y, xs, kernel = "matern52",
                                  amplitude = 1, length_scale = 1,
                                  noise = 1e-8, mean = NULL) {
  rows <- as.matrix(X); storage.mode(rows) <- "double"
  ys <- as.numeric(y)
  q <- as.numeric(xs)
  if (nrow(rows) == 0) stop("bayopt: no observations")
  d <- ncol(rows)
  if (length(q) != d)
    stop("bayopt: the query point has the wrong dimension")
  if (length(ys) != nrow(rows))
    stop("bayopt: one observation per design point")
  k <- .kernel(kernel)
  ls <- .lengths(length_scale, d)
  m <- if (is.null(mean)) mean(ys) else as.numeric(mean)
  n <- nrow(rows)
  K <- matrix(0, n, n)
  for (i in 1:n) for (j in 1:n) {
    K[i, j] <- k(rows[i, ], rows[j, ], amplitude, length_scale) +
      (if (i == j) noise else 0)
  }
  L <- .chol_r(K)
  alpha <- .chol_solve(L, ys - m)
  ks <- numeric(n)
  for (i in 1:n) ks[i] <- k(q, rows[i, ], amplitude, length_scale)
  v <- .chol_solve(L, ks)
  mu_s <- m + sum(ks * alpha)
  var_s <- max(k(q, q, amplitude, length_scale) - sum(ks * v), 0)
  sd_s <- sqrt(var_s)
  gmu <- numeric(d); gsd <- numeric(d)
  for (dd in 1:d) {
    dk <- numeric(n)
    for (i in 1:n) {
      r2v <- .r2(q, rows[i, ], ls)
      dr2 <- 2 * (q[dd] - rows[i, dd]) / (ls[dd]^2)
      dk[i] <- .dkernel_dr2(kernel, amplitude, r2v) * dr2
    }
    gmu[dd] <- sum(dk * alpha)
    dvar <- -2 * sum(dk * v)
    gsd[dd] <- if (sd_s > 1e-12) dvar / (2 * sd_s) else 0
  }
  list(grad_mu = gmu, grad_sd = gsd, mu = mu_s, sd = sd_s)
}

# --------------------------------------------------------------------------
# acquisition functions
# --------------------------------------------------------------------------

#' probability_of_improvement
#'
#' A step of the bayopt_native implementation. Called by \code{acquire}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param mu Numeric; combined arithmetically in the body.
#' @param sd Numeric; combined arithmetically in the body.
#' @param best Numeric; combined arithmetically in the body.
#' @param xi Numeric; combined arithmetically in the body. Defaults to \code{0}.
#' @return The value of \code{.Phi}.
#' @export
probability_of_improvement <- function(mu, sd, best, xi = 0) {
  if (sd <= 0) return(0)
  .Phi((best - xi - mu) / sd)
}

#' expected_improvement
#'
#' A step of the bayopt_native implementation. Called by \code{acquire}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param mu Numeric; combined arithmetically in the body.
#' @param sd Numeric; combined arithmetically in the body.
#' @param best Numeric; combined arithmetically in the body.
#' @param xi Numeric; combined arithmetically in the body. Defaults to \code{0}.
#' @return A numeric value.
#' @export
expected_improvement <- function(mu, sd, best, xi = 0) {
  if (sd <= 0) return(0)
  g <- (best - xi - mu) / sd
  sd * (g * .Phi(g) + .bayopt_phi(g))
}

#' lower_confidence_bound
#'
#' A step of the bayopt_native implementation. Called by \code{acquire}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param mu Numeric; combined arithmetically in the body.
#' @param sd Numeric; combined arithmetically in the body.
#' @param kappa Numeric; combined arithmetically in the body. Defaults to \code{2}.
#' @return A numeric value.
#' @export
lower_confidence_bound <- function(mu, sd, kappa = 2) {
  mu - kappa * sd
}

#' acquire
#'
#' A step of the bayopt_native implementation. Called by \code{bayopt}, \code{maximise_acquisition}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param mu See Usage.
#' @param sd See Usage.
#' @param best See Usage.
#' @param acq One of \code{"ei"}, \code{"lcb"}, \code{"pi"}. Defaults to \code{"ei"}.
#' @param kappa Defaults to \code{2}.
#' @param xi Defaults to \code{0}.
#' @return A numeric value.
#' @export
acquire <- function(mu, sd, best, acq = "ei", kappa = 2, xi = 0) {
  if (!(acq %in% c("ei", "pi", "lcb")))
    stop("bayopt: acq must be one of ei, pi, lcb")
  if (acq == "ei") return(expected_improvement(mu, sd, best, xi))
  if (acq == "pi") return(probability_of_improvement(mu, sd, best, xi))
  -lower_confidence_bound(mu, sd, kappa)
}

#' acquisition_gradient
#'
#' A step of the bayopt_native implementation. Called by \code{maximise_acquisition}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param gmu A vector; its length is taken.
#' @param gsd Numeric; combined arithmetically in the body.
#' @param mu Numeric; combined arithmetically in the body.
#' @param sd Numeric; combined arithmetically in the body.
#' @param best Numeric; combined arithmetically in the body.
#' @param acq One of \code{"ei"}, \code{"lcb"}, \code{"pi"}. Defaults to \code{"ei"}.
#' @param kappa Numeric; combined arithmetically in the body. Defaults to \code{2}.
#' @param xi Numeric; combined arithmetically in the body. Defaults to \code{0}.
#' @return A numeric value.
#' @export
acquisition_gradient <- function(gmu, gsd, mu, sd, best, acq = "ei",
                                 kappa = 2, xi = 0) {
  if (!(acq %in% c("ei", "pi", "lcb")))
    stop("bayopt: acq must be one of ei, pi, lcb")
  d <- length(gmu)
  if (acq == "lcb") return(-gmu + kappa * gsd)
  if (sd <= 1e-12) return(rep(0, d))
  g <- (best - xi - mu) / sd
  if (acq == "ei") return(.bayopt_phi(g) * gsd - .Phi(g) * gmu)
  dg <- (-gmu - g * gsd) / sd
  .bayopt_phi(g) * dg
}

# --------------------------------------------------------------------------
# multi-start acquisition maximisation
# --------------------------------------------------------------------------

#' maximise_acquisition
#'
#' A step of the bayopt_native implementation. Called by \code{bayopt}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X See Usage.
#' @param y See Usage.
#' @param best See Usage.
#' @param box A vector; its length is taken and its elements indexed.
#' @param acq Defaults to \code{"ei"}.
#' @param kernel Defaults to \code{"matern52"}.
#' @param amplitude Defaults to \code{1}.
#' @param length_scale Defaults to \code{1}.
#' @param noise Defaults to \code{1e-08}.
#' @param kappa Defaults to \code{2}.
#' @param xi Defaults to \code{0}.
#' @param starts Optional; may be \code{NULL}. A vector; its length is taken.
#' @param n_starts Defaults to \code{8}.
#' @param max_iter Defaults to \code{60}.
#' @param tol Defaults to \code{1e-08}.
#' @param seed Defaults to \code{0}.
#' @return A list with \code{x}, \code{acq}, \code{n_starts}, \code{evaluations}.
#' @export
maximise_acquisition <- function(X, y, best, box, acq = "ei",
                                 kernel = "matern52", amplitude = 1,
                                 length_scale = 1, noise = 1e-8,
                                 kappa = 2, xi = 0, starts = NULL,
                                 n_starts = 8, max_iter = 60,
                                 tol = 1e-8, seed = 0) {
  d <- length(box)
  st <- as.integer(seed)
  if (st <= 0) st <- 1L
  rnd <- function() {
    st <<- .ghc_lcg31(st)
    st / 2147483648
  }

  if (is.null(starts)) {
    starts <- lapply(seq_len(as.integer(n_starts)), function(i) {
      vapply(1:d, function(j) box[[j]][1] + rnd() * (box[[j]][2] - box[[j]][1]),
             numeric(1))
    })
  }
  if (length(starts) == 0) stop("bayopt: no starting points")

  score_pt <- function(pt) {
    p <- gp_posterior(X, y, rbind(pt), kernel, amplitude, length_scale,
                      noise)
    acquire(p$mean[1], p$sd[1], best, acq, kappa, xi)
  }
  clip <- function(pt) {
    vapply(1:d, function(i) min(max(pt[i], box[[i]][1]), box[[i]][2]),
           numeric(1))
  }

  best_pt <- NULL; best_val <- -Inf; evals <- 0
  span <- max(vapply(1:d, function(i) box[[i]][2] - box[[i]][1], numeric(1)))
  step <- span * 0.1
  for (s0 in starts) {
    pt <- clip(as.numeric(s0))
    val <- score_pt(pt); evals <- evals + 1
    for (it in seq_len(as.integer(max_iter))) {
      g <- gp_posterior_gradient(X, y, pt, kernel, amplitude, length_scale,
                                 noise)
      g_ <- acquisition_gradient(g$grad_mu, g$grad_sd, g$mu, g$sd,
                                 best, acq, kappa, xi)
      gn <- sqrt(sum(g_^2))
      if (gn < tol) break
      moved <- FALSE
      t <- step
      for (bl in 1:30) {
        cand <- clip(pt + t * g_ / gn)
        cval <- score_pt(cand); evals <- evals + 1
        if (cval > val + 1e-15) {
          pt <- cand; val <- cval; step <- t * 1.3
          moved <- TRUE
          break
        }
        t <- t * 0.5
      }
      if (!moved) break
    }
    if (val > best_val) {
      best_pt <- pt; best_val <- val
    }
  }
  list(x = best_pt, acq = best_val, n_starts = length(starts),
       evaluations = evals)
}

# --------------------------------------------------------------------------
# top-level
# --------------------------------------------------------------------------

#' bayopt
#'
#' A step of the bayopt_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param f See Usage.
#' @param bounds See Usage.
#' @param n_iter Defaults to \code{20}.
#' @param n_init Defaults to \code{5}.
#' @param acq One of \code{"ei"}, \code{"lcb"}, \code{"pi"}. Defaults to \code{"ei"}.
#' @param kernel Defaults to \code{"matern52"}.
#' @param amplitude Defaults to \code{1}.
#' @param length_scale Defaults to \code{1}.
#' @param noise Defaults to \code{1e-08}.
#' @param kappa Defaults to \code{2}.
#' @param xi Defaults to \code{0}.
#' @param n_candidates Defaults to \code{200}.
#' @param seed Defaults to \code{0}.
#' @param X0 Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.
#' @param y0 Defaults to \code{NULL}.
#' @param inner One of \code{"gradient"}, \code{"random"}. Defaults to \code{"gradient"}.
#' @param n_starts Defaults to \code{8}.
#' @return A list with \code{estimate}, \code{x_best}, \code{y_best}, \code{X}, \code{y}, \code{trace}, \code{acq}, \code{kernel}, \code{inner}, \code{n_eval}, \code{method}, \code{note}.
#' @export
bayopt <- function(f, bounds, n_iter = 20, n_init = 5, acq = "ei",
                   kernel = "matern52", amplitude = 1, length_scale = 1,
                   noise = 1e-8, kappa = 2, xi = 0, n_candidates = 200,
                   seed = 0, X0 = NULL, y0 = NULL, inner = "gradient",
                   n_starts = 8) {
  if (!(inner %in% c("gradient", "random")))
    stop("bayopt: inner must be 'gradient' or 'random'")
  if (n_starts < 1) stop("bayopt: n_starts must be positive")
  if (!(acq %in% c("ei", "pi", "lcb")))
    stop("bayopt: acq must be one of ei, pi, lcb")
  box <- lapply(bounds, function(b) c(as.numeric(b[1]), as.numeric(b[2])))
  if (length(box) == 0) stop("bayopt: bounds are empty")
  if (any(vapply(box, function(b) b[1] >= b[2], logical(1))))
    stop("bayopt: each bound must have lo < hi")
  if (n_iter < 1 || n_candidates < 1)
    stop("bayopt: n_iter and n_candidates must be positive")
  if (is.null(X0) && n_init < 2)
    stop("bayopt: at least two initial points are needed")
  d <- length(box)
  st <- as.integer(seed); if (st <= 0) st <- 1L
  rnd <- function() {
    st <<- .ghc_lcg31(st)
    st / 2147483648
  }
  draw <- function() {
    vapply(1:d, function(i) box[[i]][1] + rnd() * (box[[i]][2] - box[[i]][1]),
           numeric(1))
  }

  if (!is.null(X0)) {
    X <- as.matrix(X0); storage.mode(X) <- "double"
    if (is.null(y0)) {
      Y <- vapply(seq_len(nrow(X)), function(i) as.numeric(f(X[i, ])),
                  numeric(1))
    } else {
      Y <- as.numeric(y0)
    }
    if (length(Y) != nrow(X))
      stop("bayopt: X0 and y0 have different lengths")
  } else {
    # t(sapply(...)) collapses to a 1 x n matrix when d == 1, i.e. the
    # transpose of the design, so ncol was read as the sample size.
    X <- matrix(unlist(lapply(seq_len(as.integer(n_init)),
                              function(i) draw())),
                ncol = d, byrow = TRUE)
    Y <- vapply(seq_len(nrow(X)), function(i) as.numeric(f(X[i, ])),
                numeric(1))
  }
  if (is.null(rownames(X))) rownames(X) <- NULL

  trace <- vector("list", as.integer(n_iter))
  for (it in seq_len(as.integer(n_iter))) {
    best <- min(Y)
    if (inner == "gradient") {
      got <- maximise_acquisition(X, Y, best, box, acq, kernel,
                                  amplitude, length_scale, noise, kappa,
                                  xi, n_starts = n_starts, seed = st)
      x_new <- got$x; a_val <- got$acq
    } else {
      cand <- t(sapply(seq_len(as.integer(n_candidates)), function(i) draw()))
      post <- gp_posterior(X, Y, cand, kernel, amplitude, length_scale,
                           noise)
      scores <- vapply(seq_len(nrow(cand)),
                       function(i) acquire(post$mean[i], post$sd[i], best,
                                           acq, kappa, xi),
                       numeric(1))
      k <- which.max(scores)
      x_new <- cand[k, ]; a_val <- scores[k]
    }
    X <- rbind(X, x_new)
    Y <- c(Y, as.numeric(f(x_new)))
    trace[[it]] <- list(x = x_new, y = Y[length(Y)], acq = a_val,
                        best = min(Y))
  }
  best_idx <- which.min(Y)
  list(estimate = X[best_idx, ], x_best = X[best_idx, ], y_best = Y[best_idx],
       X = X, y = Y, trace = trace, acq = acq, kernel = kernel,
       inner = inner, n_eval = length(Y),
       method = sprintf("Bayesian optimisation (Mockus 1975; Snoek, %s
                        Larochelle & Adams 2012) with a %s kernel and the
                        %s acquisition", " ", kernel, acq),
       note = paste("minimisation throughout; acquisition is maximised by",
                    "multi-start projected gradient ascent on the",
                    "closed-form gradients; inner='random' is the",
                    "gradient-free baseline"))
}

bayesian_optimization <- bayopt

# house entry point: the package exports one morie_<module>
morie_bayopt <- bayopt
