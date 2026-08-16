# R arm of rrblpr -- ridge-regression BLUP through Henderson's mixed model
# equations, the variance ratio estimated by profile REML when not supplied.
# Whittaker, J. C., Thompson, R. & Denham, M. C. (2000) Genetical Research
# 75(2), 249-252; Meuwissen, T. H. E. et al. (2001) Genetics 157(4),
# 1819-1829; Henderson, C. R. (1975) Biometrics 31(2), 423-447.
# Mirrors src/morie/fn/rrblpr.py.

.rrblpr_EPS <- 1e-12


# Maximise f over [lo, hi] by a staged fixed-grid argmax.
#
# A golden-section search is PATH-DEPENDENT. Each arm walks its own sequence
# of brackets, and near a flat maximum the fc > fd branch is decided by the
# last bits of two nearly equal likelihoods, so the two languages take
# different paths and land on different answers. Quantising the result
# afterwards hides that only when the answer does not fall near a cell
# boundary, which is a coincidence rather than a guarantee: the measured
# failure was two arms landing on ADJACENT points of a 1e-6 grid.
#
# Here both arms evaluate the SAME list of points -- a + (i - 1) * step is
# the same double in both languages -- and take the argmax BY INDEX, ties to
# the lowest index. The winning index is therefore the same by construction,
# and the value returned is an exact grid point rather than a bracket
# midpoint, so the two arms return bit-identical doubles. R indexes from one
# and Python from zero; the indexing stays native in each because the index
# is internal and never reported.
#
# Refinement stops while adjacent grid values still differ by far more than
# floating-point noise. Going finer would push the comparison back below the
# noise floor and reintroduce exactly the disagreement this exists to remove.
#' Refinement stops while adjacent grid values still differ by far more
#' than
#'
#' floating-point noise. Going finer would push the comparison back
#' below the noise floor and reintroduce exactly the disagreement this
#' exists to remove.
#'
#' @param f Accepted by the signature and not used anywhere in the body.
#' @param lo Coerced to numeric by the body, with \code{as.numeric}.
#' @param hi Coerced to numeric by the body, with \code{as.numeric}.
#' @param points Coerced to integer by the body, with \code{as.integer}. Defaults to \code{201L}.
#' @param stages Coerced to integer by the body, with \code{as.integer}. Defaults to \code{4L}.
#' @return The value of \code{a}, as built in the body.
#' @export
.rrblpr_gridmax <- function(f, lo, hi, points = 201L, stages = 4L) {
  a <- as.numeric(lo); b <- as.numeric(hi)
  npt <- as.integer(points)
  nst <- as.integer(stages)
  for (s in seq_len(nst)) {
    step <- (b - a) / (npt - 1L)
    vals <- vapply(seq_len(npt),
                   function(i) f(a + (i - 1L) * step), numeric(1))
    best <- 1L
    for (i in seq_len(npt)) if (vals[i] > vals[best]) best <- i
    if (s == nst) return(a + (best - 1L) * step)
    lo_i <- if (best > 1L) best - 1L else 1L
    hi_i <- if (best < npt) best + 1L else npt
    a2 <- a + (lo_i - 1L) * step
    b <- a + (hi_i - 1L) * step
    a <- a2
    npt <- 21L
  }
  a
}

#' .rrblpr_rows
#'
#' A step of the rrblpr_native implementation. Called by \code{morie_rrblpr_rr_blup}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x A matrix; passed to \code{as.matrix}.
#' @return The value of \code{m}, as built in the body.
#' @export
.rrblpr_rows <- function(x) {
  if (is.matrix(x)) m <- x
  else if (is.data.frame(x)) m <- as.matrix(x)
  else if (is.list(x)) m <- do.call(rbind, lapply(x, as.numeric))
  else m <- matrix(as.numeric(x), ncol = 1L)
  storage.mode(m) <- "double"
  m
}

# Cholesky factor, lower triangular, with a scaled jitter.
#' Cholesky factor, lower triangular, with a scaled jitter
#'
#' A step of the rrblpr_native implementation. Called by \code{.rrblpr_reml_at}, \code{morie_rrblpr_rr_blup}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A A matrix; indexed by row and column.
#' @return The value of \code{L}, as built in the body.
#' @export
.rrblpr_chol <- function(A) {
  n <- nrow(A)
  L <- matrix(0.0, n, n)
  jit <- 1e-12 * max(abs(sum(diag(A)) / n), 1.0)
  for (i in seq_len(n)) {
    for (j in seq_len(i)) {
      s <- A[i, j]
      if (j > 1L) s <- s - sum(L[i, seq_len(j - 1L)] * L[j, seq_len(j - 1L)])
      if (i == j) {
        s <- s + jit
        if (s <= 0.0)
          stop("rrblpr: the covariance matrix is not positive definite")
        L[i, i] <- sqrt(s)
      } else {
        L[i, j] <- s / L[j, j]
      }
    }
  }
  L
}

#' .rrblpr_chol_solve
#'
#' A step of the rrblpr_native implementation. Called by \code{.rrblpr_reml_at}, \code{morie_rrblpr_rr_blup}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param L A matrix; indexed by row and column.
#' @param b A vector; indexed elementwise.
#' @return The value of \code{x}, as built in the body.
#' @export
.rrblpr_chol_solve <- function(L, b) {
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

#' .rrblpr_logdet
#'
#' A step of the rrblpr_native implementation. Called by \code{.rrblpr_reml_at}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param L A matrix; passed to \code{diag}.
#' @return A numeric value.
#' @export
.rrblpr_logdet <- function(L) 2.0 * sum(log(diag(L)))

# Restricted log likelihood at lambda, profiled over sigma_e^2.
#' Restricted log likelihood at lambda, profiled over sigma_e^2
#'
#' A step of the rrblpr_native implementation. Called by \code{morie_rrblpr_rr_blup}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param loglam Numeric; passed to \code{exp}.
#' @param G Numeric; combined arithmetically in the body.
#' @param y A vector; its length is taken.
#' @param X A matrix; indexed by row and column.
#' @return A list with \code{ll}, \code{lam}, \code{beta}, \code{s2e}, \code{L}.
#' @export
.rrblpr_reml_at <- function(loglam, G, y, X) {
  n <- length(y); p <- ncol(X)
  lam <- exp(loglam)
  V <- G / lam
  diag(V) <- diag(V) + 1.0
  L <- .rrblpr_chol(V)
  Viy <- .rrblpr_chol_solve(L, y)
  ViX <- vapply(seq_len(p), function(a) .rrblpr_chol_solve(L, X[, a]),
                numeric(n))
  if (!is.matrix(ViX)) ViX <- matrix(ViX, nrow = n)
  XtViX <- crossprod(X, ViX)
  XtViy <- as.numeric(crossprod(X, Viy))
  Lx <- .rrblpr_chol(XtViX)
  beta <- .rrblpr_chol_solve(Lx, XtViy)
  r <- y - as.numeric(X %*% beta)
  Vir <- .rrblpr_chol_solve(L, r)
  dfr <- n - p
  s2e <- sum(r * Vir) / dfr
  ll <- -0.5 * (dfr * log(max(s2e, 1e-300)) + .rrblpr_logdet(L) +
                  .rrblpr_logdet(Lx) + dfr)
  list(ll = ll, lam = lam, beta = beta, s2e = s2e, L = L)
}

#' morie_rrblpr_rr_blup
#'
#' A step of the rrblpr_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @param M Passed to \code{.rrblpr_rows}.
#' @param lam Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param X Optional; may be \code{NULL}. Passed to \code{.rrblpr_rows}.
#' @param M_new Optional; may be \code{NULL}. Passed to \code{.rrblpr_rows}.
#' @param log_lam_lo Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{-12}.
#' @param log_lam_hi Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{12}.
#' @param max_iter Accepted by the signature and not used anywhere in the body. Defaults to \code{200L}.
#' @param tol Accepted by the signature and not used anywhere in the body. Defaults to \code{1e-09}.
#' @return A list with \code{estimate}, \code{marker_effects}, \code{coefficients}, \code{breeding_values}, \code{breeding_values_kernel}, \code{kernel_identity_gap}, \code{fitted}, \code{residuals}, \code{lambda}, \code{lambda_estimated}, \code{sigma2_e}, \code{sigma2_u}, \code{sigma2_g}, \code{h2}, \code{reml_loglik}, \code{reml_profile}, \code{prediction_new}, \code{n}, \code{m}, \code{p}, \code{method}, \code{note}.
#' @export
morie_rrblpr_rr_blup <- function(y, M, lam = NULL, X = NULL, M_new = NULL,
                                 log_lam_lo = -12.0, log_lam_hi = 12.0,
                                 max_iter = 200L, tol = 1e-9) {
  yv <- as.numeric(y)
  Mm <- .rrblpr_rows(M)
  n <- length(yv)
  if (n == 0L) stop("rrblpr: no observations")
  if (nrow(Mm) != n)
    stop(sprintf("rrblpr: %d phenotypes but %d marker rows", n, nrow(Mm)))
  m <- ncol(Mm)
  Xm <- if (is.null(X)) matrix(1.0, n, 1L) else .rrblpr_rows(X)
  if (nrow(Xm) != n)
    stop(sprintf("rrblpr: X has %d rows, y has %d", nrow(Xm), n))
  p <- ncol(Xm)
  if (n - p < 1L)
    stop(sprintf(paste0("rrblpr: %d observations and %d fixed effects leave ",
                        "no residual degrees of freedom"), n, p))

  # the kernel MM' -- the object both forms of the predictor share
  G <- tcrossprod(Mm)

  profile <- list()
  ll <- NULL; s2e <- NULL; estimated <- FALSE
  if (is.null(lam)) {
    # max_iter is accepted and ignored: the grid schedule fixes the
    # evaluation count, and dropping the argument would break callers.
    loglam <- .rrblpr_gridmax(function(t) .rrblpr_reml_at(t, G, yv, Xm)$ll,
                              as.numeric(log_lam_lo), as.numeric(log_lam_hi))
    fit <- .rrblpr_reml_at(loglam, G, yv, Xm)
    lam_hat <- fit$lam; ll <- fit$ll; s2e <- fit$s2e
    for (t in 0:20) {
      lt <- as.numeric(log_lam_lo) +
        (as.numeric(log_lam_hi) - as.numeric(log_lam_lo)) * t / 20.0
      profile[[length(profile) + 1L]] <-
        c(lt, .rrblpr_reml_at(lt, G, yv, Xm)$ll)
    }
    estimated <- TRUE
  } else {
    lam_hat <- as.numeric(lam)
    if (lam_hat < 0.0)
      stop("rrblpr: lambda is a variance ratio and cannot be negative")
    if (lam_hat <= .rrblpr_EPS) {
      if (m > n - p)
        stop(sprintf(paste0("rrblpr: lambda = 0 with %d markers and %d ",
                            "residual degrees of freedom -- the least ",
                            "squares problem is not identified"), m, n - p))
      lam_hat <- 0.0
    }
  }

  # ---- Henderson's mixed model equations, solved as written
  q <- p + m
  A <- matrix(0.0, q, q)
  rhs <- numeric(q)
  A[seq_len(p), seq_len(p)] <- crossprod(Xm)
  A[seq_len(p), p + seq_len(m)] <- crossprod(Xm, Mm)
  A[p + seq_len(m), seq_len(p)] <- crossprod(Mm, Xm)
  A[p + seq_len(m), p + seq_len(m)] <- crossprod(Mm)
  for (a in seq_len(m)) A[p + a, p + a] <- A[p + a, p + a] + lam_hat
  rhs[seq_len(p)] <- as.numeric(crossprod(Xm, yv))
  rhs[p + seq_len(m)] <- as.numeric(crossprod(Mm, yv))
  sol <- .rrblpr_chol_solve(.rrblpr_chol(A), rhs)
  beta_h <- sol[seq_len(p)]
  u <- sol[p + seq_len(m)]

  gv <- as.numeric(Mm %*% u)
  fitted <- as.numeric(Xm %*% beta_h) + gv

  # ---- the kernel form of the same predictor, computed independently
  r <- yv - as.numeric(Xm %*% beta_h)
  if (lam_hat > .rrblpr_EPS) {
    Vk <- G
    diag(Vk) <- diag(Vk) + lam_hat
    w <- .rrblpr_chol_solve(.rrblpr_chol(Vk), r)
    gv_kernel <- as.numeric(G %*% w)
  } else {
    gv_kernel <- gv
  }
  kernel_gap <- max(abs(gv - gv_kernel))

  resid <- yv - fitted
  s2e_h <- if (is.null(s2e)) sum(resid ^ 2) / max(n - p, 1L) else s2e
  s2u <- if (lam_hat > .rrblpr_EPS) s2e_h / lam_hat else Inf
  trG <- sum(diag(G)) / n
  s2g <- if (lam_hat > .rrblpr_EPS) s2u * trG else Inf
  h2 <- if (lam_hat > .rrblpr_EPS) s2g / (s2g + s2e_h) else NaN

  pred_new <- NULL
  if (!is.null(M_new)) {
    Mn <- .rrblpr_rows(M_new)
    if (ncol(Mn) != m)
      stop(sprintf("rrblpr: M_new must have %d markers", m))
    pred_new <- as.numeric(Mn %*% u)
  }

  list(estimate = u, marker_effects = u, coefficients = beta_h,
       breeding_values = gv, breeding_values_kernel = gv_kernel,
       kernel_identity_gap = kernel_gap,
       fitted = fitted, residuals = resid,
       lambda = lam_hat, lambda_estimated = estimated,
       sigma2_e = s2e_h, sigma2_u = s2u, sigma2_g = s2g, h2 = h2,
       reml_loglik = ll,
       reml_profile = if (length(profile)) do.call(rbind, profile) else
         list(),
       prediction_new = pred_new,
       n = as.integer(n), m = as.integer(m), p = as.integer(p),
       method = paste0("RR-BLUP: Henderson's mixed model equations with a ",
                       "single variance ratio, the ratio estimated by ",
                       "profile REML when it is not supplied (Whittaker et ",
                       "al. 2000; Meuwissen et al. 2001; Henderson 1975)"),
       note = paste0("breeding_values and breeding_values_kernel are the ",
                     "marker-effect and GBLUP forms of the same predictor; ",
                     "kernel_identity_gap is how far apart they came out, ",
                     "and it is the check that the implementation is right ",
                     "rather than merely plausible"))
}

#' .rrblpr_cheatsheet
#'
#' A step of the rrblpr_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.rrblpr_cheatsheet <- function() {
  paste0("rrblpr: morie_rrblpr_rr_blup(y, M, lam) -> marker effects and ",
         "breeding values from the mixed model equations, lambda by REML ",
         "when NULL (Whittaker, Thompson & Denham 2000)")
}

morie_rrblpr <- morie_rrblpr_rr_blup
