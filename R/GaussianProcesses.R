.gp_k <- function(a, b, kind, p) {
  s2 <- if (is.null(p$variance)) 1 else p$variance
  if (kind == "linear") return(s2 * sum(a * b) + (if (is.null(p$bias)) 0 else p$bias))
  ls <- if (is.null(p$lengthscale)) 1 else p$lengthscale
  L <- if (length(ls) == 1) rep(ls, length(a)) else ls
  r2 <- sum(((a - b) / L)^2)
  r <- sqrt(r2)
  switch(kind,
    se = , ard = s2 * exp(-0.5 * r2),
    matern32 = s2 * (1 + sqrt(3) * r) * exp(-sqrt(3) * r),
    matern52 = s2 * (1 + sqrt(5) * r + 5 * r2 / 3) * exp(-sqrt(5) * r),
    rq = {
      al <- if (is.null(p$alpha)) 1 else p$alpha
      s2 * (1 + r2 / (2 * al))^(-al)
    },
    periodic = {
      per <- if (is.null(p$period)) 1 else p$period
      s2 * exp(-2 * sin(pi * sqrt(sum((a - b)^2)) / per)^2 / L[1]^2)
    },
    stop("kernel must be se, ard, linear, periodic, matern32, matern52 or rq")
  )
}

.gp_mat <- function(A, B, kind, p) {
  A <- as.matrix(A)
  B <- as.matrix(B)
  if (!nrow(A) || !nrow(B)) return(matrix(0, nrow(A), nrow(B)))
  outer(seq_len(nrow(A)), seq_len(nrow(B)), Vectorize(function(i, j) .gp_k(A[i, ], B[j, ], kind, p)))
}

#' Gaussian process regression, sampling and fitting
#'
#' \code{GpCovariance}: kernel matrices (SE/ARD, Matern 3/2 and 5/2, rational
#' quadratic, periodic, linear). \code{GpPredict}: exact regression
#' (Rasmussen and Williams 2006, Algorithm 2.1) with the log marginal
#' likelihood. \code{GpSample}: prior or posterior sample paths.
#' \code{GpFit}: type-II maximum likelihood by Nelder-Mead and kernel
#' selection. \code{GpLoo}: closed-form leave-one-out. Identical to the Python
#' arm \code{morie.fn.gproc}.
#'
#' @param X1,X2,X,Xnew Input matrices.
#' @param y Targets.
#' @param kernel Kernel name.
#' @param noise Noise variance.
#' @param mean Constant mean.
#' @param ... Kernel parameters (\code{variance}, \code{lengthscale},
#'   \code{period}, \code{alpha}, \code{bias}).
#' @param nsim Number of paths.
#' @param seed Philox seed.
#' @param data \code{list(X, y)} for posterior sampling.
#' @param jitter Diagonal jitter.
#' @param kernels Candidate kernels.
#' @param init Initial log parameters.
#' @return Matrix or list.
#' @references Rasmussen, C. E. and Williams, C. K. I. (2006). Gaussian
#'   Processes for Machine Learning. MIT Press.
#' @examples
#' GpCovariance(matrix(0), matrix(c(0, 1)), "matern32")
#' GpPredict(matrix(c(0, 1)), c(0, 1), matrix(0.5), noise = 0.01)$mean
#' @export
GpCovariance <- function(X1, X2, kernel = "se", ...) .gp_mat(X1, X2, kernel, list(...))

#' @rdname GpCovariance
#' @export
GpPredict <- function(X, y, Xnew, kernel = "se", noise = 1e-6, mean = 0, ...) {
  p <- list(...)
  P <- as.matrix(X)
  Q <- as.matrix(Xnew)
  yv <- y - mean
  n <- nrow(P)
  K <- .gp_mat(P, P, kernel, p) + diag(noise, n)
  L <- t(chol(K))
  alpha <- backsolve(t(L), forwardsolve(L, yv))
  Ks <- .gp_mat(P, Q, kernel, p)
  mu <- mean + as.vector(t(Ks) %*% alpha)
  V <- if (nrow(Q)) forwardsolve(L, Ks) else matrix(0, n, 0)
  var <- vapply(seq_len(nrow(Q)), function(k) .gp_k(Q[k, ], Q[k, ], kernel, p) - sum(V[, k]^2), 0)
  lml <- -0.5 * sum(yv * alpha) - sum(log(diag(L))) - n / 2 * log(2 * pi)
  list(mean = mu, variance = var, log_marginal_likelihood = lml, alpha = alpha)
}

#' @rdname GpCovariance
#' @export
GpSample <- function(X, kernel = "se", nsim = 1, seed = 1, data = NULL, noise = 1e-6, jitter = 1e-10, ...) {
  p <- list(...)
  Q <- as.matrix(X)
  m <- nrow(Q)
  Kss <- .gp_mat(Q, Q, kernel, p)
  mu <- numeric(m)
  if (!is.null(data)) {
    P <- as.matrix(data[[1]])
    K <- .gp_mat(P, P, kernel, p) + diag(noise, nrow(P))
    L <- t(chol(K))
    Ks <- .gp_mat(P, Q, kernel, p)
    mu <- as.vector(t(Ks) %*% backsolve(t(L), forwardsolve(L, data[[2]])))
    V <- forwardsolve(L, Ks)
    Kss <- Kss - crossprod(V)
  }
  L2 <- t(chol(Kss + diag(jitter, m)))
  out <- t(vapply(seq_len(nsim) - 1, function(s) mu + as.vector(L2 %*% .morie_random_normal(m, seed = seed, stream = s)),
                  numeric(m)))
  list(samples = matrix(out, nsim), mean = mu, cov = Kss)
}

.gp_nm <- function(f, x0, step = 0.5, tol = 1e-10, maxit = 2000) {
  n <- length(x0)
  pts <- c(list(x0), lapply(seq_len(n), function(i) {
    x <- x0
    x[i] <- x[i] + step
    x
  }))
  vals <- vapply(pts, f, 0)
  for (it in seq_len(maxit)) {
    o <- order(vals, seq_along(vals))
    pts <- pts[o]
    vals <- vals[o]
    if (abs(vals[n + 1] - vals[1]) <= tol * (abs(vals[1]) + tol)) break
    cc <- Reduce(`+`, pts[1:n]) / n
    xr <- cc + (cc - pts[[n + 1]])
    fr <- f(xr)
    if (fr < vals[1]) {
      xe <- cc + 2 * (cc - pts[[n + 1]])
      fe <- f(xe)
      if (fe < fr) {
        pts[[n + 1]] <- xe
        vals[n + 1] <- fe
      } else {
        pts[[n + 1]] <- xr
        vals[n + 1] <- fr
      }
    } else if (fr < vals[n]) {
      pts[[n + 1]] <- xr
      vals[n + 1] <- fr
    } else {
      xc <- if (fr < vals[n + 1]) cc + 0.5 * (xr - cc) else cc + 0.5 * (pts[[n + 1]] - cc)
      fc <- f(xc)
      if (fc < min(fr, vals[n + 1])) {
        pts[[n + 1]] <- xc
        vals[n + 1] <- fc
      } else {
        for (i in 2:(n + 1)) {
          pts[[i]] <- pts[[1]] + 0.5 * (pts[[i]] - pts[[1]])
          vals[i] <- f(pts[[i]])
        }
      }
    }
  }
  k <- order(vals, seq_along(vals))[1]
  list(par = pts[[k]], value = vals[k])
}

#' @rdname GpCovariance
#' @export
GpFit <- function(X, y, kernels = "se", mean = 0, init = NULL) {
  fits <- list()
  for (kern in kernels) {
    extra <- if (kern %in% c("periodic", "rq")) 1 else 0
    x0 <- if (is.null(init)) c(0, 0, log(0.1), rep(0, extra)) else init
    nll <- function(th) {
      p <- list(variance = exp(th[1]), lengthscale = exp(th[2]))
      if (kern == "periodic") p$period <- exp(th[4])
      if (kern == "rq") p$alpha <- exp(th[4])
      v <- tryCatch(-do.call(GpPredict, c(list(X, y, matrix(0, 0, ncol(as.matrix(X))), kern, exp(th[3]), mean), p))$
                      log_marginal_likelihood, error = function(e) Inf)
      if (is.finite(v)) v else Inf
    }
    r <- .gp_nm(nll, x0)
    par <- list(variance = exp(r$par[1]), lengthscale = exp(r$par[2]), noise = exp(r$par[3]))
    if (kern == "periodic") par$period <- exp(r$par[4])
    if (kern == "rq") par$alpha <- exp(r$par[4])
    fits[[kern]] <- list(params = par, log_marginal_likelihood = -r$value)
  }
  best <- names(fits)[which.max(vapply(fits, function(f) f$log_marginal_likelihood, 0))]
  list(fits = fits, best = best, params = fits[[best]]$params)
}

#' @rdname GpCovariance
#' @export
GpLoo <- function(X, y, kernel = "se", noise = 1e-6, ...) {
  P <- as.matrix(X)
  K <- .gp_mat(P, P, kernel, list(...)) + diag(noise, nrow(P))
  Ki <- chol2inv(chol(K))
  a <- as.vector(Ki %*% y)
  mu <- y - a / diag(Ki)
  var <- 1 / diag(Ki)
  list(mean = mu, variance = var, log_predictive = sum(-0.5 * log(2 * pi * var) - (y - mu)^2 / (2 * var)))
}

.gp_sig <- function(z) ifelse(z >= 0, 1 / (1 + exp(-z)), exp(z) / (1 + exp(z)))

#' GP classification, sparse GPs, warping and deep-GP samples
#'
#' \code{GpClassify}: Laplace-approximation binary classification with the
#' logistic likelihood (Rasmussen and Williams 2006, Algorithms 3.1-3.2) and
#' MacKay's probit-averaged probabilities. \code{GpSparse}: DTC predictions
#' with the DTC evidence and the Titsias (2009) VFE bound.
#' \code{KumaraswamyWarp}: input warping. \code{DeepGpSample}: deep-GP prior
#' samples.
#'
#' @param X,Xnew Input matrices.
#' @param y Labels in \code{-1, 1} or targets.
#' @param Z Inducing inputs.
#' @param kernel Kernel name.
#' @param tol,maxit Newton controls.
#' @param method \code{"vfe"} or \code{"dtc"}.
#' @param noise Noise variance.
#' @param ... Kernel parameters.
#' @param x Values in the unit interval.
#' @param a,b Kumaraswamy shapes.
#' @param layers List of \code{list(kernel, params)}.
#' @param seed Philox seed.
#' @param jitter Diagonal jitter.
#' @return List or vector.
#' @references Titsias, M. K. (2009). Variational learning of inducing
#'   variables in sparse Gaussian processes. Proceedings of AISTATS, 567-574.
#'
#'   Snoek, J., Swersky, K., Zemel, R. and Adams, R. P. (2014). Input warping
#'   for Bayesian optimization of non-stationary functions. Proceedings of
#'   ICML, 1674-1682.
#'
#'   Damianou, A. and Lawrence, N. D. (2013). Deep Gaussian processes.
#'   Proceedings of AISTATS, 207-215.
#' @examples
#' GpClassify(matrix(c(-1, -0.5, 0.5, 1)), c(-1, -1, 1, 1), matrix(0.8), variance = 4)$probability
#' KumaraswamyWarp(c(0, 0.5, 1), 2, 1)
#' @export
GpClassify <- function(X, y, Xnew, kernel = "se", tol = 1e-10, maxit = 100, ...) {
  p <- list(...)
  P <- as.matrix(X)
  Q <- as.matrix(Xnew)
  n <- nrow(P)
  K <- .gp_mat(P, P, kernel, p)
  f <- numeric(n)
  obj_old <- -Inf
  for (it in seq_len(maxit)) {
    pi_ <- .gp_sig(f)
    grad <- (y + 1) / 2 - pi_
    W <- pi_ * (1 - pi_)
    sw <- sqrt(W)
    L <- t(chol(diag(n) + outer(sw, sw) * K))
    b <- W * f + grad
    a <- b - sw * backsolve(t(L), forwardsolve(L, sw * as.vector(K %*% b)))
    f <- as.vector(K %*% a)
    obj <- -0.5 * sum(a * f) + sum(-log1p(exp(-y * f)))
    if (abs(obj - obj_old) < tol) break
    obj_old <- obj
  }
  pi_ <- .gp_sig(f)
  grad <- (y + 1) / 2 - pi_
  sw <- sqrt(pi_ * (1 - pi_))
  L <- t(chol(diag(n) + outer(sw, sw) * K))
  Ks <- .gp_mat(P, Q, kernel, p)
  mu <- as.vector(t(Ks) %*% grad)
  V <- forwardsolve(L, sw * Ks)
  s2 <- vapply(seq_len(nrow(Q)), function(k) .gp_k(Q[k, ], Q[k, ], kernel, p) - sum(V[, k]^2), 0)
  list(latent_mean = mu, latent_variance = s2, probability = .gp_sig(mu / sqrt(1 + pi * s2 / 8)), f_hat = f,
       log_marginal_likelihood = obj - sum(log(diag(L))))
}

#' @rdname GpClassify
#' @export
GpSparse <- function(X, y, Z, Xnew, method = "vfe", kernel = "se", noise = 0.1, ...) {
  if (!method %in% c("vfe", "dtc")) stop("method must be vfe or dtc")
  p <- list(...)
  P <- as.matrix(X)
  Zp <- as.matrix(Z)
  Q <- as.matrix(Xnew)
  n <- nrow(P)
  m <- nrow(Zp)
  Kmm <- .gp_mat(Zp, Zp, kernel, p) + diag(1e-10, m)
  Kmn <- .gp_mat(Zp, P, kernel, p)
  Lm <- t(chol(Kmm))
  V <- forwardsolve(Lm, Kmn)
  S <- Kmm + tcrossprod(Kmn) / noise
  Ls <- t(chol(S))
  w <- backsolve(t(Ls), forwardsolve(Ls, as.vector(Kmn %*% y) / noise))
  Ksm <- .gp_mat(Q, Zp, kernel, p)
  mu <- as.vector(Ksm %*% w)
  vq <- forwardsolve(Lm, t(Ksm))
  vs <- forwardsolve(Ls, t(Ksm))
  var <- vapply(seq_len(nrow(Q)), function(k) .gp_k(Q[k, ], Q[k, ], kernel, p) - sum(vq[, k]^2) + sum(vs[, k]^2), 0)
  C <- crossprod(V) + diag(noise, n)
  Lc <- t(chol(C))
  al <- backsolve(t(Lc), forwardsolve(Lc, y))
  dtc <- -0.5 * sum(y * al) - sum(log(diag(Lc))) - n / 2 * log(2 * pi)
  tr <- sum(vapply(seq_len(n), function(i) .gp_k(P[i, ], P[i, ], kernel, p), 0)) - sum(V^2)
  bound <- dtc - tr / (2 * noise)
  list(mean = mu, variance = var, log_evidence = if (method == "vfe") bound else dtc, dtc_log_evidence = dtc,
       vfe_bound = bound)
}

#' @rdname GpClassify
#' @export
KumaraswamyWarp <- function(x, a, b) 1 - (1 - x^a)^b

#' @rdname GpClassify
#' @export
DeepGpSample <- function(X, layers, seed = 1, jitter = 1e-9) {
  cur <- as.matrix(X)
  outs <- list()
  for (l in seq_along(layers)) {
    K <- .gp_mat(cur, cur, layers[[l]][[1]], layers[[l]][[2]]) + diag(jitter, nrow(cur))
    f <- as.vector(t(chol(K)) %*% .morie_random_normal(nrow(cur), seed = seed, stream = l - 1))
    outs[[l]] <- f
    cur <- matrix(f)
  }
  list(layers = outs, output = outs[[length(outs)]])
}
