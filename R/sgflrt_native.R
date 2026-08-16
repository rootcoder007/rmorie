# R arm of sgflrt -- spatial GLMM by the Laplace approximation, with the
# random effect carried as u = Lv so Sigma^-1 is never formed.
# Diggle, P. J., Tawn, J. A. & Moyeed, R. A. (1998) JRSS-C 47(3), 299-350;
# Breslow, N. E. & Clayton, D. G. (1993) JASA 88(421), 9-25;
# Tierney, L. & Kadane, J. B. (1986) JASA 81(393), 82-86.
# Mirrors src/morie/fn/sgflrt.py.

.sgflrt_EPS <- 1e-12
.sgflrt_INVPHI <- 0.6180339887498949

.sgflrt_rows <- function(x) {
  if (is.matrix(x)) m <- x
  else if (is.data.frame(x)) m <- as.matrix(x)
  else if (is.list(x)) m <- do.call(rbind, lapply(x, as.numeric))
  else m <- matrix(as.numeric(x), ncol = 1L)
  storage.mode(m) <- "double"
  m
}

.sgflrt_chol <- function(A, rel_jitter = 1e-10) {
  n <- nrow(A)
  L <- matrix(0.0, n, n)
  jit <- rel_jitter * max(abs(sum(diag(A)) / n), 1.0)
  for (i in seq_len(n)) {
    for (j in seq_len(i)) {
      s <- A[i, j]
      if (j > 1L) s <- s - sum(L[i, seq_len(j - 1L)] * L[j, seq_len(j - 1L)])
      if (i == j) {
        s <- s + jit
        if (s <= 0.0) return(NULL)
        L[i, i] <- sqrt(s)
      } else {
        L[i, j] <- s / L[j, j]
      }
    }
  }
  L
}

.sgflrt_solve <- function(L, b) {
  n <- nrow(L)
  z <- numeric(n)
  for (i in seq_len(n)) {
    s <- b[i]
    if (i > 1L) s <- s - sum(L[i, seq_len(i - 1L)] * z[seq_len(i - 1L)])
    z[i] <- s / L[i, i]
  }
  x <- numeric(n)
  for (i in seq.int(n, 1L)) {
    s <- z[i]
    if (i < n) s <- s - sum(L[seq.int(i + 1L, n), i] * x[seq.int(i + 1L, n)])
    x[i] <- s / L[i, i]
  }
  x
}

.sgflrt_inv <- function(L) {
  n <- nrow(L)
  M <- matrix(0.0, n, n)
  for (a in seq_len(n)) {
    e <- numeric(n); e[a] <- 1.0
    M[, a] <- .sgflrt_solve(L, e)
  }
  M
}

.sgflrt_logdet <- function(L) 2.0 * sum(log(diag(L)))

.sgflrt_corr <- function(h, model, phi, kappa) {
  if (h <= 0.0) return(1.0)
  if (model == "exponential") return(exp(-h / phi))
  if (model == "gaussian") return(exp(-(h / phi) ^ 2))
  if (model == "spherical") {
    if (h >= phi) return(0.0)
    r <- h / phi
    return(1.0 - 1.5 * r + 0.5 * r ^ 3)
  }
  if (model == "matern") {
    z <- sqrt(2.0 * kappa) * h / phi
    if (z <= 0.0) return(1.0)
    val <- (2.0 ^ (1.0 - kappa)) / exp(.s03lgamma(kappa)) *
      (z ^ kappa) * .s03besselk(kappa, z)
    return(max(min(val, 1.0), 0.0))
  }
  stop(sprintf(paste0("sgflrt: model must be exponential, gaussian, ",
                      "spherical or matern, got '%s'"), model))
}

# Return (linkinv, V, W, loglik). V is the variance function and W = V/phi
# the IRLS weight. They coincide for the canonical links with a fixed
# dispersion, which is why writing the working response as
# eta + (y - mu) / W looks correct and is -- right up until a family with a
# real dispersion parameter arrives, where it is off by exactly that factor.
# The working response is eta + (y - mu) / V(mu) and nothing else.
.sgflrt_family <- function(family, disp = 1.0) {
  if (family == "poisson")
    return(list(inv = function(e) exp(pmax(-500.0, pmin(500.0, e))),
                vf = function(m) pmax(m, 1e-10),
                wf = function(m) pmax(m, 1e-10),
                ll = function(y, m) y * log(pmax(m, 1e-300)) - m -
                  lgamma(y + 1.0)))
  if (family == "binomial") {
    vf <- function(m) pmax(m * (1.0 - m), 1e-10)
    return(list(inv = function(e) 1.0 /
                  (1.0 + exp(-pmax(-500.0, pmin(500.0, e)))),
                vf = vf, wf = vf,
                ll = function(y, m) {
                  mm <- pmin(pmax(m, 1e-12), 1 - 1e-12)
                  y * log(mm) + (1.0 - y) * log(1.0 - mm)
                }))
  }
  if (family == "gaussian") {
    d <- max(as.numeric(disp), 1e-300)
    return(list(inv = function(e) e,
                vf = function(m) rep(1.0, length(m)),
                wf = function(m) rep(1.0 / d, length(m)),
                ll = function(y, m) -0.5 * (log(2.0 * pi * d) +
                                              (y - m) ^ 2 / d)))
  }
  stop(sprintf(paste0("sgflrt: family must be poisson, binomial or ",
                      "gaussian, got '%s'"), family))
}

# Inner Laplace mode and the approximated log likelihood. The random effect
# is carried as u = Lv with LL' = Sigma and v ~ N(0, I), so the penalty is
# v'v/2 and Sigma^-1 is never formed. That is not a nicety: a spatial
# correlation matrix with any two nearby locations is close to singular, an
# explicitly inverted one is dominated by the jitter that made the inversion
# possible, and the fitted coefficients then miss the exact Gaussian answer
# by tenths rather than by 1e-10.
.sgflrt_laplace <- function(y, X, Sig, family, inner_iter, tol, disp = 1.0) {
  n <- length(y); p <- ncol(X)
  fam <- .sgflrt_family(family, disp)
  L <- .sgflrt_chol(Sig)
  if (is.null(L)) return(NULL)
  beta <- numeric(p); v <- numeric(n)
  for (itr in seq_len(as.integer(inner_iter))) {
    u <- as.numeric(L %*% v)
    eta <- as.numeric(X %*% beta) + u
    mu <- fam$inv(eta)
    w <- fam$wf(mu)
    z <- eta + (y - mu) / fam$vf(mu)
    q <- p + n
    A <- matrix(0.0, q, q)
    rhs <- numeric(q)
    WL <- w * L
    A[seq_len(p), seq_len(p)] <- crossprod(X * w, X)
    A[seq_len(p), p + seq_len(n)] <- crossprod(X, WL)
    A[p + seq_len(n), seq_len(p)] <- t(A[seq_len(p), p + seq_len(n),
                                        drop = FALSE])
    A[p + seq_len(n), p + seq_len(n)] <- crossprod(L, WL) + diag(n)
    rhs[seq_len(p)] <- as.numeric(crossprod(X, w * z))
    rhs[p + seq_len(n)] <- as.numeric(crossprod(L, w * z))
    LA <- .sgflrt_chol(A)
    if (is.null(LA)) return(NULL)
    sol <- .sgflrt_solve(LA, rhs)
    nb <- sol[seq_len(p)]; nv <- sol[p + seq_len(n)]
    shift <- max(max(abs(nb - beta)), max(abs(nv - v)))
    beta <- nb; v <- nv
    if (shift < tol) break
  }
  u <- as.numeric(L %*% v)
  eta <- as.numeric(X %*% beta) + u
  mu <- fam$inv(eta)
  w <- fam$wf(mu)
  loglik <- sum(fam$ll(y, mu))
  pen <- 0.5 * sum(v * v)
  H <- crossprod(L, w * L) + diag(n)
  LH <- .sgflrt_chol(H)
  if (is.null(LH)) return(NULL)
  list(lap = loglik - pen - 0.5 * .sgflrt_logdet(LH), beta = beta, u = u,
       mu = mu, eta = eta, loglik = loglik, w = w, L = L, v = v)
}

.sgflrt_golden <- function(f, lo, hi, iters = 16L) {
  a <- lo; b <- hi
  c0 <- b - (b - a) * .sgflrt_INVPHI
  d0 <- a + (b - a) * .sgflrt_INVPHI
  fc <- f(c0); fd <- f(d0)
  for (i in seq_len(iters)) {
    if (b - a < 1e-8) break
    if (fc > fd) {
      b <- d0; d0 <- c0; fd <- fc
      c0 <- b - (b - a) * .sgflrt_INVPHI
      fc <- f(c0)
    } else {
      a <- c0; c0 <- d0; fc <- fd
      d0 <- a + (b - a) * .sgflrt_INVPHI
      fd <- f(d0)
    }
  }
  0.5 * (a + b)
}

#' morie_sgflrt_spatial_glmm_fit
#'
#' Part of the sgflrt_native implementation; see the file header for the
#' source it follows.
#'
#' @param y See Usage.
#' @param X See Usage.
#' @param coords See Usage.
#' @param family Defaults to \code{"poisson"}.
#' @param model Defaults to \code{"exponential"}.
#' @param sigma2 Defaults to \code{NULL}.
#' @param phi Defaults to \code{NULL}.
#' @param kappa Defaults to \code{1.5}.
#' @param nugget Defaults to \code{0}.
#' @param dispersion Defaults to \code{NULL}.
#' @param inner_iter Defaults to \code{50L}.
#' @param outer_cycles Defaults to \code{3L}.
#' @param tol Defaults to \code{1e-10}.
#' @return A list with \code{estimate}, \code{coefficients}, \code{std_error}, \code{z}, \code{spatial_effect}, \code{fitted}, \code{linear_predictor}, \code{sigma2}, \code{phi}, \code{dispersion}, \code{sigma2_at_lower_bound}, \code{spatial_signal}, \code{kappa}, \code{nugget}, \code{loglik}, \code{laplace_loglik}, \code{gls_identity_gap}, \code{covariance}, \code{family}, \code{model}, \code{n}, \code{p}, \code{d}, \code{min_distance}, \code{max_distance}, \code{method}, \code{note}.
#' @export
morie_sgflrt_spatial_glmm_fit <- function(y, X, coords, family = "poisson",
                                          model = "exponential",
                                          sigma2 = NULL, phi = NULL,
                                          kappa = 1.5, nugget = 0.0,
                                          dispersion = NULL,
                                          inner_iter = 50L,
                                          outer_cycles = 3L, tol = 1e-10) {
  yv <- as.numeric(y)
  n <- length(yv)
  if (n == 0L) stop("sgflrt: no observations")
  Xm <- .sgflrt_rows(X)
  if (nrow(Xm) != n)
    stop(sprintf("sgflrt: %d responses but %d design rows", n, nrow(Xm)))
  p <- ncol(Xm)
  C <- .sgflrt_rows(coords)
  if (nrow(C) != n)
    stop(sprintf("sgflrt: %d responses but %d coordinate rows", n, nrow(C)))
  d <- ncol(C)
  invisible(.sgflrt_family(family))
  if (!is.null(dispersion) && family != "gaussian")
    stop(paste0("sgflrt: only the gaussian family has a dispersion ",
                "parameter; poisson and binomial fix it at one"))
  disp <- if (is.null(dispersion)) 1.0 else as.numeric(dispersion)
  if (disp <= 0.0) stop("sgflrt: the dispersion must be positive")
  fit_disp <- family == "gaussian" && is.null(dispersion)
  nug <- as.numeric(nugget)
  if (nug < 0.0) stop("sgflrt: the nugget cannot be negative")
  if (family == "binomial" && any(!(yv %in% c(0.0, 1.0))))
    stop("sgflrt: the binomial family here takes 0/1 responses")
  if (family == "poisson" && any(yv < 0.0 | yv != floor(yv)))
    stop("sgflrt: the Poisson family takes non-negative counts")

  # Euclidean distances computed here rather than via stats::dist: this
  # package implements its own numerics, and this mirrors the Python arm
  # term for term so the summation order matches in both languages.
  D <- matrix(0.0, n, n)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      s <- 0.0
      for (a in seq_len(ncol(C))) s <- s + (C[i, a] - C[j, a])^2
      D[i, j] <- sqrt(s)
    }
  }
  dmax <- max(D)
  if (dmax <= .sgflrt_EPS)
    stop(paste0("sgflrt: every location is the same point, so there is no ",
                "spatial structure to fit"))
  dmin <- min(D[row(D) != col(D)])

  corrmat <- function(ph) {
    R <- matrix(0.0, n, n)
    for (i in seq_len(n)) for (j in seq_len(n))
      R[i, j] <- .sgflrt_corr(D[i, j], model, ph, as.numeric(kappa))
    diag(R) <- diag(R) + nug
    R
  }
  scaled <- function(R, s2) s2 * R

  if (!is.null(sigma2) && as.numeric(sigma2) <= .sgflrt_EPS) {
    s2h <- 0.0
    phh <- if (!is.null(phi)) as.numeric(phi) else dmax / 3.0
    Sig <- diag(1e-10, n)
    res <- .sgflrt_laplace(yv, Xm, Sig, family, inner_iter, tol, disp)
    at_bound <- FALSE
  } else {
    lo_s <- log(1e-6); hi_s <- log(1e3)
    lo_p <- log(max(dmin, 1e-6) / 4.0); hi_p <- log(dmax * 4.0)
    s2h <- if (is.null(sigma2)) 1.0 else as.numeric(sigma2)
    phh <- if (is.null(phi)) dmax / 3.0 else as.numeric(phi)
    for (cyc in seq_len(as.integer(outer_cycles))) {
      if (is.null(sigma2)) {
        # the correlation matrix does not depend on sigma2, so it is built
        # once and reused across the whole variance search -- rebuilding it
        # per step is what made the Matern model unusable, since every
        # entry costs a Bessel evaluation
        R <- corrmat(phh)
        fs <- function(ls) {
          r <- .sgflrt_laplace(yv, Xm, scaled(R, exp(ls)), family,
                               inner_iter, tol, disp)
          if (is.null(r)) -1e300 else r$lap
        }
        s2h <- exp(.sgflrt_golden(fs, lo_s, hi_s))
      }
      if (is.null(phi)) {
        fp <- function(lp) {
          r <- .sgflrt_laplace(yv, Xm, scaled(corrmat(exp(lp)), s2h),
                               family, inner_iter, tol, disp)
          if (is.null(r)) -1e300 else r$lap
        }
        phh <- exp(.sgflrt_golden(fp, lo_p, hi_p))
      }
      if (fit_disp) {
        Sd <- scaled(corrmat(phh), s2h)
        fdp <- function(ld) {
          r <- .sgflrt_laplace(yv, Xm, Sd, family, inner_iter, tol, exp(ld))
          if (is.null(r)) -1e300 else r$lap
        }
        disp <- exp(.sgflrt_golden(fdp, log(1e-8), log(1e4)))
      }
      if (!is.null(sigma2) && !is.null(phi) && !fit_disp) break
    }
    # a variance pinned against its lower bound means the data carry no
    # spatial signal, and the range is then not identified at all --
    # reported rather than left as a number that looks like an estimate
    at_bound <- is.null(sigma2) && s2h < exp(lo_s) * 1.01
    Sig <- scaled(corrmat(phh), s2h)
    res <- .sgflrt_laplace(yv, Xm, Sig, family, inner_iter, tol, disp)
  }
  if (is.null(res))
    stop(paste0("sgflrt: the penalised system is not positive definite -- ",
                "try a positive nugget, or a shorter range"))
  lap <- res$lap; beta <- res$beta; u <- res$u; mu <- res$mu
  eta <- res$eta; loglik <- res$loglik; w <- res$w
  Lsig <- res$L

  # standard errors for beta: the curvature after profiling out v, in the
  # same parameterisation the fit used
  WL <- w * Lsig
  H <- crossprod(Lsig, WL) + diag(n)
  LH <- .sgflrt_chol(H)
  XtWX <- crossprod(Xm * w, Xm)
  B <- crossprod(Xm, WL)
  HiB <- vapply(seq_len(p), function(a) .sgflrt_solve(LH, B[a, ]),
                numeric(n))
  if (!is.matrix(HiB)) HiB <- matrix(HiB, nrow = n)
  Ib <- XtWX - B %*% HiB
  LIb <- .sgflrt_chol(Ib)
  covb <- if (!is.null(LIb)) .sgflrt_inv(LIb) else
    matrix(NaN, p, p)
  se <- vapply(seq_len(p), function(a)
    if (!is.nan(covb[a, a])) sqrt(max(covb[a, a], 0.0)) else NaN, 0)

  # the Gaussian identity-link case has a closed form; compute it so the
  # approximation can be compared with the answer rather than trusted
  gls_gap <- NaN
  if (family == "gaussian") {
    V <- Sig
    diag(V) <- diag(V) + disp
    Lv <- .sgflrt_chol(V)
    if (!is.null(Lv)) {
      Viy <- .sgflrt_solve(Lv, yv)
      ViX <- vapply(seq_len(p), function(a) .sgflrt_solve(Lv, Xm[, a]),
                    numeric(n))
      if (!is.matrix(ViX)) ViX <- matrix(ViX, nrow = n)
      A <- crossprod(Xm, ViX)
      LA <- .sgflrt_chol(A)
      if (!is.null(LA)) {
        bg <- .sgflrt_solve(LA, as.numeric(crossprod(Xm, Viy)))
        gls_gap <- max(abs(bg - beta))
      }
    }
  }

  list(estimate = beta, coefficients = beta, std_error = se,
       z = vapply(seq_len(p), function(a)
         if (se[a] > .sgflrt_EPS) beta[a] / se[a] else NaN, 0),
       spatial_effect = u, fitted = mu, linear_predictor = eta,
       sigma2 = s2h, phi = phh, dispersion = disp,
       sigma2_at_lower_bound = at_bound, spatial_signal = !at_bound,
       kappa = as.numeric(kappa), nugget = nug,
       loglik = loglik, laplace_loglik = lap,
       gls_identity_gap = gls_gap, covariance = covb,
       family = family, model = model,
       n = as.integer(n), p = as.integer(p), d = as.integer(d),
       min_distance = dmin, max_distance = dmax,
       method = paste0("spatial GLMM by the Laplace approximation: ",
                       "penalised IRLS for the joint mode of (beta, u), ",
                       "the variance and range by cycling golden-section ",
                       "searches on the approximated marginal likelihood ",
                       "(Diggle, Tawn & Moyeed 1998; Breslow & Clayton ",
                       "1993)"),
       note = paste0("the Laplace approximation is exact for the Gaussian ",
                     "identity-link case, and gls_identity_gap is how far ",
                     "the fit sits from the closed-form GLS answer there; ",
                     "for binary data with few observations per correlated ",
                     "unit the variance component is biased downward ",
                     "(Breslow & Clayton 1993) and n is reported so that ",
                     "can be judged; sigma2_at_lower_bound means the data ",
                     "carry no spatial signal, and phi is then not ",
                     "identified whatever value it was left at"))
}

.sgflrt_cheatsheet <- function() {
  paste0("sgflrt: morie_sgflrt_spatial_glmm_fit(y, X, coords, family) -> ",
         "spatial GLMM by Laplace, with the spatial random effect returned ",
         "(Diggle, Tawn & Moyeed 1998; Breslow & Clayton 1993)")
}

morie_sgflrt <- morie_sgflrt_spatial_glmm_fit
