# SPDX-License-Identifier: AGPL-3.0-or-later
# Native generalized additive models (penalized regression splines with
# automatic smoothness selection), reproducing mgcv::gam's default fits.
#
# Sources: Wood, S. N. (2017) Generalized Additive Models: An Introduction
# with R, 2nd ed., CRC: sections 4.2 (penalized regression splines), 5.3.1
# (cubic regression splines), 5.5 (thin plate regression splines), 5.8
# (identifiability constraints), 6.2 (smoothness selection: GCV, UBRE/Cp,
# Laplace REML); Wood, S. N. (2003) Thin plate regression splines, JRSS-B
# 65, 95-114; Wood, S. N. (2011) Fast stable restricted maximum likelihood
# and marginal likelihood estimation of semiparametric generalized linear
# models, JRSS-B 73, 3-36; Wood, S. N. (2013) On p-values for smooth
# components of an extended generalized additive model, Biometrika 100,
# 221-228.
#
# The construction follows mgcv 1.9-1 step by step so that the fitted model
# is the same model: the thin plate basis is the Lanczos-truncated
# eigenbasis of the thin plate penalty matrix with the polynomial null space
# absorbed by Householder rotations (mgcv's tprs.c), the columns rescaled to
# unit root mean square, the penalty rescaled by the infinity norm of the
# model matrix (smoothCon), and the sum-to-zero constraint absorbed by the
# QR decomposition of the constraint vector.  The cubic regression spline
# is the cardinal (value-at-knot) basis with knots at quantiles of the
# unique covariate values (mgcv's crspl).  Smoothing parameters are
# optimised by a full Newton method on log(lambda) (and log(scale) for
# Gaussian REML) with exact first and second derivatives of the criterion,
# with P-IRLS at each trial value for binomial and Poisson responses.
#
# The signs of the thin plate basis columns are those of the Ritz vectors
# of mgcv's Lanczos iteration, which take them from LAPACK's DSTEDC.  To
# return the same coefficients (not only the same fitted model), the
# Lanczos recurrence is reproduced operation by operation and DSTEDC
# (DSTEQR below 26 rows, the DLAED0-3 divide and conquer above) is
# translated from LAPACK 3.12.  p-values for smooth terms use Wood (2013)
# with the Davies (1980) algorithm for weighted sums of chi-squared
# variables, both translated from mgcv 1.9-1 (GPL >= 2; Davies, R. B.
# (1980) The distribution of a linear combination of chi-squared random
# variables, Applied Statistics 29, 323-333).


# ---------------------------------------------------------------------------
# Formula handling
# ---------------------------------------------------------------------------

# Read one s() call: its covariate expressions and the k / bs settings.
.gamn_read_s <- function(cl) {
  args <- as.list(cl)[-1L]
  nm <- names(args)
  if (is.null(nm)) nm <- rep("", length(args))
  covs <- args[nm == ""]
  opt <- args[nm != ""]
  if (!length(covs)) stop("s() needs at least one covariate")
  if (length(covs) > 2L)
    stop("s() supports one or two covariates (thin plate 2-D at most)")
  okn <- c("k", "bs", "fx")
  bad <- setdiff(names(opt), okn)
  if (length(bad))
    stop(sprintf("s() argument(s) not supported: %s", paste(bad, collapse = ", ")))
  k <- -1L
  if (!is.null(opt$k)) {
    kk <- opt$k
    if (is.call(kk) && identical(kk[[1L]], as.name("-")) && length(kk) == 2L &&
        is.numeric(kk[[2L]])) kk <- -kk[[2L]]
    if (!is.numeric(kk) || length(kk) != 1L)
      stop("k in s() must be a number written in the formula")
    k <- as.integer(round(kk))
  }
  bs <- "tp"
  if (!is.null(opt$bs)) {
    if (!is.character(opt$bs) || length(opt$bs) != 1L)
      stop("bs in s() must be a quoted string, \"tp\" or \"cr\"")
    bs <- opt$bs
  }
  if (!bs %in% c("tp", "cr")) stop(sprintf("bs = \"%s\" is not supported (use \"tp\" or \"cr\")", bs))
  fx <- FALSE
  if (!is.null(opt$fx)) {
    if (!is.logical(opt$fx) || length(opt$fx) != 1L)
      stop("fx in s() must be TRUE or FALSE")
    fx <- opt$fx
  }
  if (bs == "cr" && length(covs) != 1L) stop("bs = \"cr\" handles one covariate only")
  terms <- vapply(covs, function(e) paste(format(e), collapse = ""), "")
  label <- sprintf("s(%s)", paste(terms, collapse = ","))
  list(exprs = covs, term = terms, k = k, bs = bs, fixed = fx, label = label,
       dim = length(covs))
}

# Split a formula into its parametric part (a terms object) and its s() terms.
.gamn_formula_parts <- function(formula) {
  if (!inherits(formula, "formula") || length(formula) != 3L)
    stop("formula must be a two-sided formula such as y ~ s(x) + z")
  tt <- stats::terms(formula, specials = "s")
  vars <- as.list(attr(tt, "variables"))[-1L]
  sidx <- attr(tt, "specials")$s
  fac <- attr(tt, "factors")
  labs <- attr(tt, "term.labels")
  smooths <- list()
  sterm <- integer(0)
  if (length(sidx)) {
    for (v in sidx) {
      if (identical(v, attr(tt, "response"))) stop("s() cannot be the response")
      cols <- which(fac[v, ] != 0)
      if (length(cols) != 1L || sum(fac[, cols] != 0) != 1L)
        stop("s() terms cannot appear inside interactions")
      sterm <- c(sterm, cols)
      smooths[[length(smooths) + 1L]] <- .gamn_read_s(vars[[v]])
    }
    ord <- order(sterm)
    smooths <- smooths[ord]
    sterm <- sterm[ord]
  }
  ptt <- if (length(sterm)) {
    if (length(sterm) == length(labs)) {
      # only smooths: keep the response and the intercept choice
      pt <- stats::terms(stats::update.formula(formula, . ~ 1))
      if (attr(tt, "intercept") == 0L) pt <- stats::terms(stats::update.formula(formula, . ~ 0))
      pt
    } else stats::drop.terms(tt, dropx = sterm, keep.response = TRUE)
  } else tt
  offs <- attr(tt, "offset")
  if (length(sterm) && length(offs)) {
    # drop.terms / update lose offset() terms: put them back
    fo <- stats::formula(ptt)
    rhs <- fo[[3L]]
    for (o in offs) rhs <- call("+", rhs, vars[[o]])
    fo[[3L]] <- rhs
    ptt <- stats::terms(fo)
  }
  labels <- vapply(smooths, `[[`, "", "label")
  if (anyDuplicated(labels)) stop("the same smooth appears twice in the formula")
  list(pterms = ptt, smooths = smooths, response = vars[[attr(tt, "response")]],
       intercept = attr(tt, "intercept"))
}


# ---------------------------------------------------------------------------
# Thin plate regression spline basis (mgcv tprs.c, Wood 2003)
# ---------------------------------------------------------------------------

# Null-space dimension and the default penalty order m (smallest m with
# 2m > d + 1 when the supplied order is not valid).
.gamn_tp_m <- function(d, m = 0L) {
  if (2L * m <= d) { m <- 1L; while (2L * m < d + 2L) m <- m + 1L }
  m
}
.gamn_tp_M <- function(d, m) choose(m + d - 1L, d)

# Thin plate radial basis constant (mgcv eta_const).
.gamn_eta_const <- function(m, d) {
  if (d %% 2L == 0L) {
    d2 <- d %/% 2L
    f <- if ((m + 1L + d2) %% 2L) -1 else 1
    f <- f / 2^(2L * m - 1L) / pi^d2
    if (m > 2L) f <- f / factorial(m - 1L)
    if (m - d2 >= 2L) f <- f / factorial(m - d2)
  } else {
    f <- sqrt(pi)
    kk <- m - (d - 1L) %/% 2L
    for (i in seq_len(kk) - 1L) f <- f / (-0.5 - i)
    f <- f / 4^m / pi^(d %/% 2L) / sqrt(pi)
    if (m > 2L) f <- f / factorial(m - 1L)
  }
  f
}

# eta(r) from squared distances r2 (mgcv fast_eta), vectorised.
.gamn_eta <- function(r2, m, d, f) {
  out <- numeric(length(r2))
  pos <- r2 > 0
  r <- r2[pos]
  if (d %% 2L == 0L) {
    out[pos] <- f * 0.5 * log(r) * r^(m - d %/% 2L)
  } else {
    out[pos] <- f * r^(m - d %/% 2L - 1L) * sqrt(r)
  }
  out
}

# Polynomial powers spanning the penalty null space (mgcv
# gen_tps_poly_powers): an M x d integer matrix.
.gamn_poly_powers <- function(M, m, d) {
  P <- matrix(0L, M, d)
  idx <- integer(d)
  for (i in seq_len(M)) {
    P[i, ] <- idx
    s <- sum(idx)
    if (s < m - 1L) idx[1L] <- idx[1L] + 1L else {
      s <- s - idx[1L]
      idx[1L] <- 0L
      if (d > 1L) for (j in 2:d) {
        idx[j] <- idx[j] + 1L
        s <- s + 1L
        if (s == m) { s <- s - idx[j]; idx[j] <- 0L } else break
      }
    }
  }
  P
}

.gamn_tps_T <- function(X, P) {
  T <- matrix(1, nrow(X), nrow(P))
  for (j in seq_len(nrow(P))) for (k in seq_len(ncol(P)))
    if (P[j, k] > 0L) T[, j] <- T[, j] * X[, k]^P[j, k]
  T
}

# Radial basis matrix between the rows of A (n x d) and B (nk x d).
.gamn_tps_E <- function(A, B, m, d, f) {
  r2 <- 0
  for (j in seq_len(d)) { dx <- outer(A[, j], B[, j], "-"); r2 <- r2 + dx * dx }
  if (d == 1L && m == 2L) {
    E <- f * r2 * sqrt(r2)              # (f r^2) sqrt(r^2), as mgcv's fast_eta
  } else if (d == 2L && m == 2L) {
    E <- f * (log(r2) * 0.5) * r2
    E[r2 <= 0] <- 0
  } else {
    E <- .gamn_eta(r2, m, d, f)
  }
  dim(E) <- c(nrow(A), nrow(B))
  E
}

# Householder factorisation A Q = [0, T] (mgcv QT with fullQ = 0): returns
# the rows u_i defining Q = H_1 H_2 ... with H_i = I - u_i u_i'.
.gamn_QT <- function(A) {
  Ar <- nrow(A); Ac <- ncol(A)
  U <- matrix(0, Ar, Ac)
  for (i in seq_len(Ar)) {
    len <- Ac - i + 1L
    p <- A[i, seq_len(len)]
    m <- max(abs(p))
    if (m > 0) p <- p / m
    lsq <- sqrt(sum(crossprod(p, p)))
    if (p[len] < 0) lsq <- -lsq
    p[len] <- p[len] + lsq
    g <- if (lsq != 0) 1 / (lsq * p[len]) else 0
    if (i < Ar) for (j in (i + 1L):Ar) {
      x <- sum(crossprod(p, A[j, seq_len(len)])) * g
      A[j, seq_len(len)] <- A[j, seq_len(len)] - x * p
    }
    U[i, seq_len(len)] <- p * sqrt(g)
  }
  U
}

# C Q (postmultiplication by the Householder product, mgcv HQmult(C,U,0,0)).
.gamn_HQ_right <- function(C, U) {
  for (k in seq_len(nrow(U))) {
    u <- U[k, ]
    C <- C - tcrossprod(drop(C %*% u), u)
  }
  C
}

# Lanczos iteration for the k largest-magnitude eigenpairs of symmetric A,
# reproducing mgcv's Rlanczos operation by operation: the same start
# vector, the same order of floating point operations (sequential dot
# products, modified Gram-Schmidt re-orthogonalisation applied twice; the
# reference BLAS symmetric and general matrix-vector products agree bit for
# bit), the same convergence tests, and LAPACK DSTEDC's eigenvector signs
# for the tridiagonal matrix.  The Ritz vectors therefore come out as mgcv's,
# signs included, which makes the thin plate coefficients comparable.
.gamn_lanczos <- function(A, k, tol = .Machine$double.eps^0.7) {
  n <- nrow(A)
  dot <- function(u, v) sum(crossprod(u, v))
  f_check <- max(k %/% 2L, 10L)
  kk <- max(n %/% 10L, 1L)
  if (kk < f_check) f_check <- kk
  jran <- 1; q0 <- numeric(n)
  for (i in seq_len(n)) {
    jran <- (jran * 106 + 1283) %% 6075
    q0[i] <- jran / 6075 - 0.5
  }
  Q <- matrix(0, n, min(n, 256L))
  Q[, 1L] <- q0 / sqrt(dot(q0, q0))
  a <- numeric(n); b <- numeric(n)
  res <- NULL
  for (j in 0:(n - 1L)) {
    if (j + 2L > ncol(Q)) Q <- cbind(Q, matrix(0, n, ncol(Q)))
    qj <- Q[, j + 1L]
    z <- drop(A %*% qj)
    a[j + 1L] <- xx <- dot(qj, z)
    if (j == 0L) z <- z - xx * qj else {
      z <- z - (xx * qj + b[j] * Q[, j])
      for (rep in 1:2) for (i in seq_len(j + 1L)) {
        qi <- Q[, i]
        z <- z + (-dot(z, qi)) * qi
      }
    }
    b[j + 1L] <- sqrt(dot(z, z))
    if (j < n - 1L) Q[, j + 2L] <- z / b[j + 1L]
    if ((j >= k && j %% f_check == 0L) || j == n - 1L) {
      ev <- .gamn_tridiag_eigen(a[seq_len(j + 1L)], b[seq_len(j)])
      d <- ev$values; v <- ev$vectors
      normTj <- max(abs(d[1L]), abs(d[j + 1L]))
      err <- abs(b[j + 1L] * v[j + 1L, ])
      if (j >= k) {
        max_err <- normTj * tol
        pi_ <- 0L; ni <- 0L; conv <- TRUE
        while (pi_ + ni < k) {
          if (abs(d[pi_ + 1L]) >= abs(d[j + 1L - ni])) {
            if (err[pi_ + 1L] > max_err) { conv <- FALSE; break } else pi_ <- pi_ + 1L
          } else {
            if (err[ni + 1L] > max_err) { conv <- FALSE; break } else ni <- ni + 1L
          }
        }
        if (conv || j == n - 1L) {
          if (!conv) { pi_ <- k; ni <- 0L }
          jj <- j + 1L
          # final decomposition with LAPACK's DSTEDC conventions, descending
          ev <- .gamn_dstedc(a[seq_len(jj)], b[seq_len(j)])
          d <- rev(ev$values); v <- ev$vectors[, jj:1L, drop = FALSE]
          idx <- c(seq_len(pi_), if (ni > 0L) (jj - ni + 1L):jj)
          U <- Q[, seq_len(jj), drop = FALSE] %*% v[, idx, drop = FALSE]
          res <- list(values = d[idx], vectors = U, iter = jj)
          break
        }
      }
    }
  }
  res
}

# Eigen-decomposition of a symmetric tridiagonal matrix (diagonal a,
# off-diagonal b), eigenvalues in descending order.
.gamn_tridiag_eigen <- function(a, b) {
  n <- length(a)
  if (n == 1L) return(list(values = a, vectors = matrix(1, 1, 1)))
  Tm <- diag(a, n)
  Tm[cbind(2:n, 1:(n - 1L))] <- b
  Tm[cbind(1:(n - 1L), 2:n)] <- b
  eigen(Tm, symmetric = TRUE)
}

# The thin plate regression spline basis of one smooth.
.gamn_tp_construct <- function(Xd, k, max_knots = 2000L, seed = 1L) {
  Xd <- as.matrix(Xd)
  n <- nrow(Xd); d <- ncol(Xd)
  shift <- colMeans(Xd)
  Xd <- sweep(Xd, 2L, shift)
  m <- .gamn_tp_m(d, 0L)
  M <- as.integer(.gamn_tp_M(d, m))
  if (k < 0L) k <- M + c(8L, 27L, 100L)[min(d, 3L)]
  if (k < M + 1L) {
    k <- M + 1L
    warning("basis dimension, k, increased to minimum possible")
  }
  knots <- NULL
  if (n > max_knots) {
    if (d == 1L) {
      xu <- matrix(sort(unique(Xd[, 1L])), ncol = 1L)
    } else {
      txt <- as.character(Xd[, 1L])
      for (j in 2:d) txt <- paste0(txt, "*", as.character(Xd[, j]))
      dup <- duplicated(txt)
      xt <- txt[!dup]
      xu <- Xd[!dup, , drop = FALSE]
      o <- order(xt, method = "radix")
      xu <- xu[o, , drop = FALSE]
    }
    if (nrow(xu) > max_knots) {
      knots <- .gamn_with_seed(seed, xu[sample(seq_len(nrow(xu)), max_knots, replace = FALSE), , drop = FALSE])
    }
  }
  # unique (sorted) locations used to build E
  base <- if (is.null(knots)) Xd else knots
  o <- do.call(order, c(lapply(seq_len(d), function(j) base[, j]), list(seq_len(nrow(base)))))
  bs <- base[o, , drop = FALSE]
  keep <- c(TRUE, rowSums(bs[-1L, , drop = FALSE] != bs[-nrow(bs), , drop = FALSE]) > 0)
  Xu <- bs[keep, , drop = FALSE]
  nu <- nrow(Xu)
  if (nu < k) stop("A term has fewer unique covariate combinations than specified maximum degrees of freedom")
  if (nu == k) stop("k equals the number of unique covariate values; reduce k")
  f <- .gamn_eta_const(m, d)
  E <- .gamn_tps_E(Xu, Xu, m, d, f)
  P <- .gamn_poly_powers(M, m, d)
  T <- .gamn_tps_T(Xu, P)
  lz <- .gamn_lanczos(E, k)
  U <- lz$vectors; v <- lz$values
  TU <- crossprod(T, U)
  Hh <- .gamn_QT(TU)
  UQ <- .gamn_HQ_right(U, Hh)
  UZ <- rbind(cbind(UQ[, seq_len(k - M), drop = FALSE], matrix(0, nu, M)),
              cbind(matrix(0, M, k - M), diag(M)))
  S <- .gamn_HQ_right(diag(v, k), Hh)
  S <- t(.gamn_HQ_right(t(S), Hh))
  S[, (k - M + 1L):k] <- 0; S[(k - M + 1L):k, ] <- 0
  if (is.null(knots)) {
    X1 <- .gamn_HQ_right(sweep(U, 2L, v, "*"), Hh)
    X1[, (k - M + 1L):k] <- T
    rowkey <- do.call(paste, c(lapply(seq_len(d), function(j) Xd[, j]), sep = "\r"))
    ukey <- do.call(paste, c(lapply(seq_len(d), function(j) Xu[, j]), sep = "\r"))
    X <- X1[match(rowkey, ukey), , drop = FALSE]
  } else {
    X <- .gamn_tp_predict_raw(Xd, Xu, UZ, m, d, M, f)
  }
  w <- sqrt(vapply(seq_len(k), function(i) sum(crossprod(X[, i], X[, i])), 0) / n)
  X <- sweep(X, 2L, w, "/")
  UZ <- sweep(UZ, 2L, w, "/")
  # mgcv divides row i and then column i by w[i], i = 1, ..., k
  ri <- row(S); ci <- col(S)
  S <- S / w[pmin(ri, ci)] / w[pmax(ri, ci)]
  S <- (S + t(S)) / 2
  list(X = X, S = S, UZ = UZ, Xu = Xu, shift = shift, m = m, M = M, k = k,
       null.space.dim = M, rank = k - M, eta_f = f, lanczos_iter = lz$iter)
}

# b'UZ for each row of X (already shifted): the knot-based evaluation used
# for prediction and for large data sets.
.gamn_tp_predict_raw <- function(Xd, Xu, UZ, m, d, M, f) {
  n <- nrow(Xd)
  P <- .gamn_poly_powers(M, m, d)
  nu <- nrow(Xu)
  out <- matrix(0, n, ncol(UZ))
  UZr <- UZ[seq_len(nu), , drop = FALSE]
  UZp <- UZ[nu + seq_len(M), , drop = FALSE]
  if (d == 1L && m == 2L && !is.unsorted(Xu[, 1L], strictly = TRUE)) {
    # eta(r) = f |r|^3: on each knot interval sum_j c_j |x - k_j|^3 is the
    # cubic sum_q choose(3, q) x^(3-q) (-1)^q sum_j s_j c_j k_j^q with
    # s_j = sign(x - k_j), evaluated from cumulative sums over the knots.
    kn <- Xu[, 1L]; x <- Xd[, 1L]
    pidx <- findInterval(x, kn) + 1L
    cf <- c(1, -3, 3, -1)
    for (q in 0:3) {
      W <- kn^q * UZr
      Cw <- rbind(0, apply(W, 2L, cumsum))
      Tq <- Cw[nu + 1L, ]
      Sq <- 2 * Cw[pidx, , drop = FALSE] - rep(Tq, each = n)
      out <- out + (cf[q + 1L] * x^(3 - q)) * Sq
    }
    return(f * out + .gamn_tps_T(Xd, P) %*% UZp)
  }
  chunk <- max(1L, floor(4e6 / nu))
  for (s in seq(1L, n, by = chunk)) {
    i <- s:min(n, s + chunk - 1L)
    Ei <- .gamn_tps_E(Xd[i, , drop = FALSE], Xu, m, d, f)
    out[i, ] <- Ei %*% UZr + .gamn_tps_T(Xd[i, , drop = FALSE], P) %*% UZp
  }
  out
}

# Run expr with the RNG set to a fixed seed (as mgcv's temp.seed), leaving
# the caller's random number stream untouched.
.gamn_with_seed <- function(seed, expr) {
  genv <- globalenv()
  had <- exists(".Random.seed", envir = genv, inherits = FALSE)
  if (had) old <- get(".Random.seed", envir = genv, inherits = FALSE)
  kind <- RNGkind()
  on.exit({
    RNGkind(kind[1L], kind[2L], kind[3L])
    if (had) assign(".Random.seed", old, envir = genv) else
      if (exists(".Random.seed", envir = genv, inherits = FALSE))
        rm(".Random.seed", envir = genv)
  })
  RNGkind("default", "default")
  set.seed(seed)
  force(expr)
}


# ---------------------------------------------------------------------------
# Cubic regression spline basis (mgcv crspl, Wood 2017 section 5.3.1)
# ---------------------------------------------------------------------------

.gamn_cr_FS <- function(xk) {
  n <- length(xk)
  h <- diff(xk)
  n2 <- n - 2L
  D <- matrix(0, n2, n)
  for (i in seq_len(n2)) {
    D[i, i] <- 1 / h[i]
    D[i, i + 2L] <- 1 / h[i + 1L]
    D[i, i + 1L] <- -1 / h[i] - 1 / h[i + 1L]
  }
  B <- diag((h[-n + 1L][seq_len(n2)] + h[-1L][seq_len(n2)]) / 3, n2)
  if (n2 > 1L) {
    B[cbind(2:n2, 1:(n2 - 1L))] <- h[2:n2] / 6
    B[cbind(1:(n2 - 1L), 2:n2)] <- h[2:n2] / 6
  }
  BiD <- solve(B, D)
  list(S = crossprod(D, BiD), F = rbind(0, BiD, 0))  # F: second derivs at knots
}

.gamn_cr_X <- function(x, xk, Fm) {
  nk <- length(xk); n <- length(x)
  X <- matrix(0, n, nk)
  kmin <- xk[1L]; kmax <- xk[nk]
  lo <- x < kmin; hi <- x > kmax; mid <- !(lo | hi)
  if (any(mid)) {
    xi <- x[mid]
    j <- findInterval(xi, xk, rightmost.closed = TRUE, left.open = TRUE)
    j[j < 1L] <- 1L; j[j > nk - 1L] <- nk - 1L
    xj <- xk[j]; xj1 <- xk[j + 1L]; h <- xj1 - xj
    ajm <- xj1 - xi; ajp <- xi - xj
    cjm <- ajm * (ajm * ajm / h - h) / 6
    cjp <- ajp * (ajp * ajp / h - h) / 6
    Xm <- cjm * Fm[j, , drop = FALSE] + cjp * Fm[j + 1L, , drop = FALSE]
    rr <- seq_along(xi)
    Xm[cbind(rr, j)] <- Xm[cbind(rr, j)] + ajm / h
    Xm[cbind(rr, j + 1L)] <- Xm[cbind(rr, j + 1L)] + ajp / h
    X[mid, ] <- Xm
  }
  if (any(lo)) {
    h <- xk[2L] - kmin; xik <- x[lo] - kmin
    Xl <- outer(-xik * h / 3, Fm[1L, ]) + outer(-xik * h / 6, Fm[2L, ])
    Xl[, 1L] <- Xl[, 1L] + 1 - xik / h
    Xl[, 2L] <- Xl[, 2L] + xik / h
    X[lo, ] <- Xl
  }
  if (any(hi)) {
    h <- kmax - xk[nk - 1L]; xik <- x[hi] - kmax
    Xh <- outer(xik * h / 6, Fm[nk - 1L, ]) + outer(xik * h / 3, Fm[nk, ])
    Xh[, nk - 1L] <- Xh[, nk - 1L] - xik / h
    Xh[, nk] <- Xh[, nk] + 1 + xik / h
    X[hi, ] <- Xh
  }
  X
}

.gamn_cr_construct <- function(x, k) {
  if (k < 0L) k <- 10L
  if (k < 3L) { k <- 3L; warning("basis dimension, k, increased to minimum possible") }
  xu <- unique(x)
  if (length(xu) < k) stop("the covariate has fewer unique values than k: reduce k")
  xk <- as.numeric(stats::quantile(xu, seq(0, 1, length.out = k)))
  fs <- .gamn_cr_FS(xk)
  S <- (fs$S + t(fs$S)) / 2
  list(X = .gamn_cr_X(x, xk, fs$F), S = S, xk = xk, F = fs$F, k = k,
       null.space.dim = 2L, rank = k - 2L)
}


# ---------------------------------------------------------------------------
# smoothCon: penalty scaling and the sum-to-zero constraint (Wood 2017, 5.8)
# ---------------------------------------------------------------------------

.gamn_smooth_con <- function(spec, cov) {
  if (spec$bs == "tp") {
    b <- .gamn_tp_construct(cov, spec$k)
  } else {
    b <- .gamn_cr_construct(cov[, 1L], spec$k)
  }
  X <- b$X; S <- b$S
  k <- ncol(X)
  S.scale <- 1
  if (!spec$fixed) {
    maXX <- norm(X, type = "I")^2
    S.scale <- norm(S) / maXX
    S <- S / S.scale
  }
  C <- matrix(colMeans(X), 1L, k)
  indi <- which(colSums(C) != 0)
  nx <- length(indi)
  if (nx < k) {
    nz <- nx - 1L
    qrc <- qr(t(C[, indi, drop = FALSE]))
    ZSZ <- S
    if (nz > 0L) ZSZ[indi[seq_len(nz)], ] <- qr.qty(qrc, S[indi, , drop = FALSE])[2:nx, ]
    ZSZ <- ZSZ[-indi[nx], , drop = FALSE]
    if (nz > 0L) ZSZ[, indi[seq_len(nz)]] <- t(qr.qty(qrc, t(ZSZ[, indi, drop = FALSE]))[2:nx, ])
    S <- ZSZ[, -indi[nx], drop = FALSE]
    if (nz > 0L) X[, indi[seq_len(nz)]] <- t(qr.qty(qrc, t(X[, indi, drop = FALSE]))[2:nx, ])
    X <- X[, -indi[nx], drop = FALSE]
    con <- list(type = "partial", qrc = qrc, indi = indi)
  } else {
    qrc <- qr(t(C))
    ZSZ <- qr.qty(qrc, S)[2:k, , drop = FALSE]
    S <- t(qr.qty(qrc, t(ZSZ))[2:k, , drop = FALSE])
    X <- t(qr.qty(qrc, t(X))[2:k, , drop = FALSE])
    con <- list(type = "full", qrc = qrc)
  }
  S <- (S + t(S)) / 2
  brank <- b$rank; bnull <- b$null.space.dim
  b$X <- NULL; b$S <- NULL; b$rank <- NULL; b$null.space.dim <- NULL
  c(list(label = spec$label, term = spec$term, exprs = spec$exprs, bs = spec$bs,
         dim = spec$dim, fixed = spec$fixed, X = X, S = S, S.scale = S.scale,
         con = con, rank = min(brank, k - 1L),
         null.space.dim = max(0L, bnull - 1L), bs.dim = k), b)
}

# Apply the stored constraint to a raw (unconstrained) basis matrix.
.gamn_apply_con <- function(X, con) {
  k <- ncol(X)
  if (con$type == "full") {
    t(qr.qty(con$qrc, t(X))[2:k, , drop = FALSE])
  } else {
    indi <- con$indi; nx <- length(indi); nz <- nx - 1L
    if (nz > 0L) X[, indi[seq_len(nz)]] <- t(qr.qty(con$qrc, t(X[, indi, drop = FALSE]))[2:nx, ])
    X[, -indi[nx], drop = FALSE]
  }
}

# Unconstrained basis of a fitted smooth at new covariate values.
.gamn_smooth_predict_raw <- function(sm, cov) {
  cov <- as.matrix(cov)
  if (sm$bs == "tp") {
    Xd <- sweep(cov, 2L, sm$shift)
    .gamn_tp_predict_raw(Xd, sm$Xu, sm$UZ, sm$m, sm$dim, sm$M, sm$eta_f)
  } else {
    .gamn_cr_X(cov[, 1L], sm$xk, sm$F)
  }
}


# ---------------------------------------------------------------------------
# Families (canonical links only: identity, logit, log)
# ---------------------------------------------------------------------------

.gamn_family <- function(name) {
  switch(name,
    gaussian = list(
      family = "gaussian", link = "identity",
      linkinv = function(eta) eta, linkfun = function(mu) mu,
      variance = function(mu) rep(1, length(mu)),
      # derivatives of the working weight w = V(mu) w.r.t. eta
      dw = function(mu) rep(0, length(mu)), d2w = function(mu) rep(0, length(mu)),
      dev.resids = function(y, mu, wt) wt * (y - mu)^2,
      mustart = function(y, wt) y,
      valid = function(y) all(is.finite(y))),
    binomial = list(
      family = "binomial", link = "logit",
      linkinv = function(eta) stats::plogis(eta),
      linkfun = function(mu) stats::qlogis(mu),
      variance = function(mu) mu * (1 - mu),
      dw = function(mu) mu * (1 - mu) * (1 - 2 * mu),
      d2w = function(mu) mu * (1 - mu) * ((1 - 2 * mu)^2 - 2 * mu * (1 - mu)),
      dev.resids = function(y, mu, wt) {
        r <- numeric(length(y))
        a <- y > 0; b <- y < 1
        r[a] <- r[a] + y[a] * log(y[a] / mu[a])
        r[b] <- r[b] + (1 - y[b]) * log((1 - y[b]) / (1 - mu[b]))
        2 * wt * r
      },
      mustart = function(y, wt) (wt * y + 0.5) / (wt + 1),
      valid = function(y) all(is.finite(y)) && all(y >= 0 & y <= 1)),
    poisson = list(
      family = "poisson", link = "log",
      linkinv = function(eta) pmax(exp(eta), .Machine$double.eps),
      linkfun = function(mu) log(mu),
      variance = function(mu) mu,
      dw = function(mu) mu, d2w = function(mu) mu,
      dev.resids = function(y, mu, wt) {
        r <- mu * wt
        p <- y > 0
        r[p] <- (wt * (y * log(y / mu) - (y - mu)))[p]
        2 * r
      },
      mustart = function(y, wt) y + 0.1,
      valid = function(y) all(is.finite(y)) && all(y >= 0)),
    stop("family must be gaussian, binomial or poisson"))
}

# Log saturated likelihood (mgcv fix.family.ls), used by REML.
.gamn_ls <- function(fam, y, wt, scale) {
  if (fam$family == "gaussian") {
    nobs <- sum(wt > 0)
    -nobs * log(2 * pi * scale) / 2 + sum(log(wt[wt > 0])) / 2
  } else if (fam$family == "poisson") {
    sum(stats::dpois(y, y, log = TRUE) * wt)
  } else {
    m <- wt
    -(-2 * sum(ifelse(m > 0, wt / m, 0) *
                 stats::dbinom(round(m * y), round(m), y, log = TRUE))) / 2
  }
}


# ---------------------------------------------------------------------------
# Penalized fit and criterion derivatives at given log smoothing parameters
# ---------------------------------------------------------------------------

# Total penalty sum_k lambda_k S_k (p x p).
.gamn_Slambda <- function(G, lambda) {
  St <- matrix(0, G$p, G$p)
  for (k in seq_along(G$S)) {
    ii <- G$Sind[[k]]
    St[ii, ii] <- St[ii, ii] + lambda[k] * G$S[[k]]
  }
  St
}

# Full-size S_k (p x p).
.gamn_Sfull <- function(G, k, lambda = 1) {
  St <- matrix(0, G$p, G$p)
  ii <- G$Sind[[k]]
  St[ii, ii] <- lambda * G$S[[k]]
  St
}

# Inverse and log determinant of a symmetric positive (semi)definite H.
.gamn_inv <- function(H) {
  R <- tryCatch(chol(H), error = function(e) NULL)
  if (!is.null(R)) {
    return(list(A = chol2inv(R), ldet = 2 * sum(log(diag(R))), ok = TRUE))
  }
  ev <- eigen(H, symmetric = TRUE)
  tol <- max(abs(ev$values)) * .Machine$double.eps^0.8
  vals <- ev$values
  inv <- ifelse(vals > tol, 1 / vals, 0)
  list(A = ev$vectors %*% (inv * t(ev$vectors)),
       ldet = sum(log(vals[vals > tol])), ok = FALSE)
}

# P-IRLS for the penalized likelihood at fixed lambda (canonical links).
.gamn_pirls <- function(G, St, beta, tol = 1e-13, maxit = 200L) {
  fam <- G$fam; X <- G$X; y <- G$y; wt <- G$wt
  eta <- if (is.null(beta)) fam$linkfun(fam$mustart(y, wt)) else drop(X %*% beta) + G$offset
  if (is.null(beta)) eta <- eta
  mu <- fam$linkinv(eta)
  pdev_old <- Inf
  if (!is.null(beta)) pdev_old <- sum(fam$dev.resids(y, mu, wt)) + sum(beta * (St %*% beta))
  conv <- FALSE
  for (it in seq_len(maxit)) {
    v <- fam$variance(mu)
    w <- wt * v
    z <- (eta - G$offset) + (y - mu) / v
    sw <- sqrt(w)
    Xw <- X * sw
    H <- crossprod(Xw) + St
    bnew <- drop(.gamn_inv(H)$A %*% crossprod(Xw, sw * z))
    etan <- drop(X %*% bnew) + G$offset
    mun <- fam$linkinv(etan)
    pdev <- sum(fam$dev.resids(y, mun, wt)) + sum(bnew * (St %*% bnew))
    hh <- 0L
    while ((!is.finite(pdev) || pdev > pdev_old + 1e-12 * abs(pdev_old)) &&
           !is.null(beta) && hh < 40L) {
      bnew <- (bnew + beta) / 2
      etan <- drop(X %*% bnew) + G$offset
      mun <- fam$linkinv(etan)
      pdev <- sum(fam$dev.resids(y, mun, wt)) + sum(bnew * (St %*% bnew))
      hh <- hh + 1L
    }
    dchange <- abs(pdev - pdev_old)
    bchange <- if (is.null(beta)) Inf else max(abs(bnew - beta)) / (max(abs(bnew)) + 1e-300)
    beta <- bnew; eta <- etan; mu <- mun
    if (dchange <= tol * (abs(pdev) + 0.1) || bchange < 1e-12) { conv <- TRUE; break }
    pdev_old <- pdev
  }
  list(beta = beta, eta = eta, mu = mu, converged = conv, iter = it)
}

# Fit at log smoothing parameters rho (and log scale for Gaussian REML) and
# return the criterion with its gradient and Hessian w.r.t. the parameters.
.gamn_fit_score <- function(par, G, deriv = 2L, beta = NULL) {
  fam <- G$fam; X <- G$X; nS <- length(G$S)
  rho <- par[seq_len(nS)]
  lambda <- exp(rho)
  St <- .gamn_Slambda(G, lambda)
  gaussian <- fam$family == "gaussian"
  if (gaussian) {
    XWX <- G$XWX
    inv <- .gamn_inv(XWX + St)
    A <- inv$A
    beta <- drop(A %*% G$XWy)
    eta <- drop(X %*% beta) + G$offset
    mu <- eta
    w <- G$wt
    fit_conv <- TRUE
  } else {
    pf <- .gamn_pirls(G, St, beta)
    beta <- pf$beta; eta <- pf$eta; mu <- pf$mu
    w <- G$wt * fam$variance(mu)
    XWX <- crossprod(X * sqrt(w))
    inv <- .gamn_inv(XWX + St)
    A <- inv$A
    fit_conv <- pf$converged
  }
  dev <- sum(fam$dev.resids(G$y, mu, G$wt))
  bSb <- sum(beta * (St %*% beta))
  F <- A %*% XWX
  edf <- diag(F)
  trA <- sum(edf)
  n <- G$nobs
  out <- list(beta = beta, eta = eta, mu = mu, w = w, A = A, F = F, edf = edf,
              trA = trA, dev = dev, bSb = bSb, ldetH = inv$ldet, St = St,
              lambda = lambda, XWX = XWX, fit_conv = fit_conv)
  crit <- G$criterion
  if (crit == "REML") {
    ldS <- sum(G$Srank * rho) + G$ldS0
    Dp <- dev + bSb
    if (G$scale_known) {
      phi <- 1
    } else {
      phi <- exp(par[nS + 1L])
    }
    score <- Dp / (2 * phi) - .gamn_ls(fam, G$y, G$wt, phi) + inv$ldet / 2 - ldS / 2 -
      G$Mp / 2 * log(2 * pi * phi)
  } else if (crit == "GCV") {
    delta <- n - G$gamma * trA
    score <- n * dev / delta^2
  } else {
    score <- dev / n - 2 * (n - G$gamma * trA) / n + 1
  }
  out$score <- score
  if (deriv == 0L || nS == 0L) return(out)

  # ---- first derivatives of beta and of the working weights
  Sk <- lapply(seq_len(nS), function(k) .gamn_Sfull(G, k, lambda[k]))
  Skb <- lapply(Sk, function(S) drop(S %*% beta))
  bk <- vapply(seq_len(nS), function(k) -drop(A %*% Skb[[k]]), numeric(G$p))
  bk <- matrix(bk, G$p, nS)
  glm <- !gaussian
  if (glm) {
    etak <- X %*% bk
    dw <- G$wt * fam$dw(mu)
    wk <- dw * etak
  }
  Hk <- lapply(seq_len(nS), function(k) {
    H <- Sk[[k]]
    if (glm) H <- H + crossprod(X, wk[, k] * X)
    H
  })
  ASt <- A %*% St
  Bk <- lapply(Hk, function(H) A %*% H)
  r <- G$wt * (G$y - mu)          # -dD/deta / 2
  # D_k = -2 r' X b_k
  Xr <- drop(crossprod(X, r))
  Dk <- -2 * drop(crossprod(bk, Xr))
  trAk <- vapply(seq_len(nS), function(k)
    sum(Bk[[k]] * t(ASt)) - sum(A * Sk[[k]]), 0)
  Lk <- vapply(seq_len(nS), function(k) sum(diag(Bk[[k]])), 0)
  Dpk <- vapply(seq_len(nS), function(k) sum(beta * Skb[[k]]), 0)
  if (deriv >= 2L) {
    bkj <- array(0, c(G$p, nS, nS))
    for (k in seq_len(nS)) for (j in k:nS) {
      v <- Hk[[j]] %*% bk[, k] + Sk[[k]] %*% bk[, j]
      if (j == k) v <- v + Skb[[k]]
      bkj[, k, j] <- bkj[, j, k] <- -drop(A %*% v)
    }
    Hkj <- vector("list", nS * nS)
    if (glm) d2w <- G$wt * fam$d2w(mu)
    for (k in seq_len(nS)) for (j in k:nS) {
      H <- if (j == k) Sk[[k]] else matrix(0, G$p, G$p)
      if (glm) {
        wkj <- d2w * etak[, k] * etak[, j] + dw * drop(X %*% bkj[, k, j])
        H <- H + crossprod(X, wkj * X)
      }
      Hkj[[(j - 1L) * nS + k]] <- Hkj[[(k - 1L) * nS + j]] <- H
    }
    Dkj <- trAkj <- Lkj <- Dpkj <- matrix(0, nS, nS)
    Xb <- X %*% bk
    for (k in seq_len(nS)) for (j in k:nS) {
      H2 <- Hkj[[(j - 1L) * nS + k]]
      BkBj <- Bk[[k]] %*% Bk[[j]]
      BjBk <- Bk[[j]] %*% Bk[[k]]
      AH2 <- A %*% H2
      t1 <- -sum(BjBk * t(ASt)) + sum(AH2 * t(ASt)) - sum(BkBj * t(ASt)) +
        sum(Bk[[k]] * t(A %*% Sk[[j]]))
      t2 <- -(if (j == k) sum(A * Sk[[k]]) else 0) + sum(Bk[[j]] * t(A %*% Sk[[k]]))
      trAkj[k, j] <- trAkj[j, k] <- t1 + t2
      Lkj[k, j] <- Lkj[j, k] <- -sum(Bk[[j]] * t(Bk[[k]])) + sum(diag(AH2))
      Dkj[k, j] <- Dkj[j, k] <- 2 * sum(w * Xb[, k] * Xb[, j]) -
        2 * sum(Xr * bkj[, k, j])
      Dpkj[k, j] <- Dpkj[j, k] <- (if (j == k) Dpk[k] else 0) + 2 * sum(Skb[[k]] * bk[, j])
    }
  }
  if (crit == "REML") {
    g <- Dpk / (2 * phi) + Lk / 2 - G$Srank / 2
    if (deriv >= 2L) Hs <- Dpkj / (2 * phi) + Lkj / 2
    if (!G$scale_known) {
      Dp <- dev + bSb
      gphi <- -Dp / (2 * phi) + n / 2 - G$Mp / 2
      g <- c(g, gphi)
      if (deriv >= 2L) {
        cross <- -Dpk / (2 * phi)
        Hs <- rbind(cbind(Hs, cross), c(cross, Dp / (2 * phi)))
      }
    }
  } else if (crit == "GCV") {
    gm <- G$gamma
    g <- n * Dk / delta^2 + 2 * n * dev * gm * trAk / delta^3
    if (deriv >= 2L) {
      Hs <- n * Dkj / delta^2 + 2 * n * gm * (outer(Dk, trAk) + outer(trAk, Dk)) / delta^3 +
        6 * n * dev * gm^2 * outer(trAk, trAk) / delta^4 + 2 * n * dev * gm * trAkj / delta^3
    }
  } else {
    g <- Dk / n + 2 * G$gamma * trAk / n
    if (deriv >= 2L) Hs <- Dkj / n + 2 * G$gamma * trAkj / n
  }
  out$grad <- g
  if (deriv >= 2L) out$hess <- (Hs + t(Hs)) / 2
  out$db <- bk
  out
}


# ---------------------------------------------------------------------------
# Smoothing parameter selection: Newton's method on log(lambda)
# ---------------------------------------------------------------------------

# mgcv initial.sp: balance the diagonals of X'WX and the penalties.
.gamn_initial_sp <- function(X, G) {
  nS <- length(G$S)
  ldxx <- colSums(X * X)
  ldss <- ldxx * 0
  pen <- rep(FALSE, length(ldxx))
  sp <- numeric(nS)
  for (i in seq_len(nS)) {
    S <- G$S[[i]]
    maS <- max(abs(S))
    rsS <- rowMeans(abs(S)); csS <- colMeans(abs(S)); dS <- diag(abs(S))
    thresh <- .Machine$double.eps^0.8 * maS
    ind <- rsS > thresh & csS > thresh & dS > thresh
    ii <- G$Sind[[i]]
    xx <- ldxx[ii][ind]
    pen[ii] <- pen[ii] | ind
    sp[i] <- mean(xx) / mean(diag(S)[ind])
    ldss[ii] <- ldss[ii] + sp[i] * diag(S)
  }
  ind <- ldss > 0 & pen & ldxx > 0
  ldxx <- ldxx[ind]; ldss <- ldss[ind]
  while (mean(ldxx / (ldxx + ldss)) > 0.4) { sp <- sp * 10; ldss <- ldss * 10 }
  while (mean(ldxx / (ldxx + ldss)) < 0.4) { sp <- sp / 10; ldss <- ldss / 10 }
  sp
}

.gamn_newton <- function(par, G, tol = 1e-10, maxit = 200L, max_step = 5) {
  b <- .gamn_fit_score(par, G, 2L)
  hist <- b$score
  conv <- "iteration limit reached"
  iter <- 0L
  for (it in seq_len(maxit)) {
    iter <- it
    g <- b$grad
    scale_ref <- abs(b$score) + (if (G$criterion == "REML") 1 else abs(b$dev / G$nobs)) 
    if (max(abs(g)) <= tol * scale_ref) { conv <- "full convergence"; break }
    eh <- eigen(b$hess, symmetric = TRUE)
    ev <- abs(eh$values)
    ev[ev < max(ev) * 1e-7] <- max(ev) * 1e-7
    step <- -drop(eh$vectors %*% (drop(crossprod(eh$vectors, g)) / ev))
    ms <- max(abs(step))
    if (ms > max_step) step <- step * max_step / ms
    ok <- FALSE
    for (h in 0:40) {
      trial <- par + step
      bt <- tryCatch(.gamn_fit_score(trial, G, 2L, b$beta), error = function(e) NULL)
      if (!is.null(bt) && is.finite(bt$score) && bt$score <= b$score) { ok <- TRUE; break }
      step <- step / 2
    }
    if (!ok) {
      # steepest descent as a fall-back
      step <- -g / max(abs(g)) * 0.1
      for (h in 0:40) {
        trial <- par + step
        bt <- tryCatch(.gamn_fit_score(trial, G, 2L, b$beta), error = function(e) NULL)
        if (!is.null(bt) && is.finite(bt$score) && bt$score <= b$score) { ok <- TRUE; break }
        step <- step / 2
      }
    }
    if (!ok) { conv <- "step failed"; break }
    dscore <- b$score - bt$score
    par <- trial; b <- bt
    hist <- c(hist, b$score)
    if (max(abs(step)) < 1e-12 && dscore <= 1e-15 * abs(b$score)) {
      conv <- "step length small"; break
    }
  }
  list(par = par, fit = b, conv = conv, iter = iter, score.hist = hist)
}


# ---------------------------------------------------------------------------
# Model set-up
# ---------------------------------------------------------------------------

# Evaluate a covariate expression from an s() term in the data, through a
# one-sided formula and model.frame (no evaluation of text).
.gamn_cov <- function(expr, data) {
  fo <- stats::as.formula(call("~", expr), env = baseenv())
  if (is.environment(data)) stop("data must be a data frame or list")
  mf <- stats::model.frame(fo, data = data, na.action = stats::na.pass)
  v <- mf[[1L]]
  if (!is.numeric(v)) stop(sprintf("smooth covariate %s must be numeric",
                                   paste(format(expr), collapse = "")))
  as.numeric(v)
}

.gamn_setup <- function(formula, data, family, weights) {
  pr <- .gamn_formula_parts(formula)
  data <- as.data.frame(data)
  mfp <- stats::model.frame(pr$pterms, data = data, na.action = stats::na.pass,
                            drop.unused.levels = TRUE)
  ok <- stats::complete.cases(mfp)
  covs <- lapply(pr$smooths, function(sp) {
    m <- vapply(sp$exprs, .gamn_cov, numeric(nrow(data)), data = data)
    matrix(m, nrow(data), length(sp$exprs), dimnames = list(NULL, sp$term))
  })
  for (cv in covs) ok <- ok & stats::complete.cases(cv)
  if (!is.null(weights)) {
    if (length(weights) != nrow(data)) stop("weights must have one value per row of data")
    ok <- ok & !is.na(weights)
  }
  if (sum(ok) < 2L) stop("too few complete observations")
  data2 <- data[ok, , drop = FALSE]
  mfp <- stats::model.frame(pr$pterms, data = data2, drop.unused.levels = TRUE)
  ptt <- attr(mfp, "terms")
  y <- stats::model.response(mfp)
  fam <- .gamn_family(family)
  if (fam$family == "binomial") {
    if (is.factor(y)) y <- as.numeric(y != levels(y)[1L])
    if (is.logical(y)) y <- as.numeric(y)
    if (is.matrix(y)) stop("give a 0/1 binomial response (two-column responses are not supported)")
  }
  if (is.logical(y)) y <- as.numeric(y)
  if (!is.numeric(y) || !is.null(dim(y)) && ncol(as.matrix(y)) != 1L)
    stop("the response must be a numeric vector")
  y <- as.numeric(y)
  if (!fam$valid(y)) stop(sprintf("response values are not valid for the %s family", fam$family))
  wt <- if (is.null(weights)) rep(1, length(y)) else as.numeric(weights[ok])
  if (any(wt < 0)) stop("weights must be non-negative")
  Xp <- stats::model.matrix(ptt, mfp)
  contr <- attr(Xp, "contrasts")
  assign <- attr(Xp, "assign")
  xlev <- stats::.getXlevels(ptt, mfp)
  covs <- lapply(covs, function(cv) cv[ok, , drop = FALSE])
  sms <- lapply(seq_along(pr$smooths), function(i) .gamn_smooth_con(pr$smooths[[i]], covs[[i]]))
  X <- Xp
  nms <- colnames(Xp)
  S <- list(); Sind <- list(); Srank <- numeric(0); ldS0 <- 0; Sowner <- integer(0)
  for (i in seq_along(sms)) {
    sm <- sms[[i]]
    first <- ncol(X) + 1L
    X <- cbind(X, sm$X)
    sms[[i]]$first.para <- first
    sms[[i]]$last.para <- ncol(X)
    nms <- c(nms, paste0(sm$label, ".", seq_len(ncol(sm$X))))
    sms[[i]]$X <- NULL
    if (!sm$fixed) {
      S[[length(S) + 1L]] <- sm$S
      Sind[[length(Sind) + 1L]] <- first:ncol(X)
      ev <- eigen(sm$S, symmetric = TRUE, only.values = TRUE)$values
      Srank <- c(Srank, sm$rank)
      ldS0 <- ldS0 + sum(log(ev[seq_len(sm$rank)]))
      Sowner <- c(Sowner, i)
    }
  }
  colnames(X) <- nms
  if (qr(X)$rank < ncol(X)) warning("the model matrix is rank deficient; results may be unreliable")
  off <- stats::model.offset(mfp)
  off <- if (is.null(off)) rep(0, length(y)) else as.numeric(off)
  list(X = X, y = y, wt = wt, offset = off, fam = fam, S = S, Sind = Sind,
       Srank = Srank, ldS0 = ldS0, Sowner = Sowner, p = ncol(X), nobs = sum(wt > 0),
       Mp = ncol(X) - sum(Srank), smooths = sms, nsdf = ncol(Xp), pterms = ptt, assign = assign,
       contrasts = contr, xlevels = xlev, intercept = attr(ptt, "intercept") > 0,
       rows = which(ok), n.data = nrow(data), formula = formula, gamma = 1)
}


# ---------------------------------------------------------------------------
# Main fitting function
# ---------------------------------------------------------------------------

#' Generalized additive model fitted natively (mgcv-compatible)
#'
#' Fits a generalized additive model with penalized regression spline
#' smooths and automatic smoothness selection, reproducing the default fits
#' of \code{mgcv::gam}: the same thin plate regression spline and cubic
#' regression spline bases, the same identifiability constraints and penalty
#' scaling, and smoothing parameters chosen by GCV/UBRE (\code{"GCV.Cp"}) or
#' by Laplace-approximate REML (\code{"REML"}) with Newton's method on the
#' log smoothing parameters.  Binomial (logit) and Poisson (log) responses
#' are fitted by penalized iteratively re-weighted least squares (P-IRLS).
#'
#' The right-hand side may contain ordinary parametric terms (anything
#' \code{model.matrix} understands) and smooth terms written \code{s(x)},
#' \code{s(x, k = 20)}, \code{s(x, bs = "cr")} or the two-dimensional
#' isotropic thin plate smooth \code{s(x, z)}.  The \code{s()} terms are read
#' from the formula as data; no function \code{s} is called.  For more than
#' 2000 unique covariate values a thin plate basis is built from 2000 knots
#' sampled with the fixed seed 1, as mgcv does, without disturbing the
#' caller's random number stream.
#'
#' @param formula A model formula such as \code{y ~ s(x) + s(z, bs = "cr") + w}.
#' @param data A data frame containing the variables in the formula.
#' @param family One of \code{"gaussian"} (identity link), \code{"binomial"}
#'   (logit link, 0/1 or logical or two-level factor response) or
#'   \code{"poisson"} (log link).
#' @param method Smoothness selection criterion: \code{"GCV.Cp"} (GCV when
#'   the scale is unknown, UBRE/Mallows' Cp for binomial and Poisson) or
#'   \code{"REML"}.
#' @param weights Optional non-negative prior weights, one per row of
#'   \code{data}.
#' @param sp Optional fixed smoothing parameters, one per penalized smooth
#'   in formula order (as \code{mgcv::gam}'s \code{sp}); \code{NULL}
#'   (default) estimates them.  With \code{method = "REML"} and a Gaussian
#'   response the scale is still estimated.
#' @param ... Unused; present for compatibility with the bridge calling
#'   convention.
#' @return An object of class \code{"morie_gam"}: a list with
#'   \code{coefficients}, \code{Vp} (Bayesian posterior covariance),
#'   \code{Ve} (frequentist covariance), \code{edf} (per coefficient),
#'   \code{edf_smooth}, \code{edf_total}, \code{sp} (smoothing parameters),
#'   \code{score} (the minimised GCV/UBRE or REML criterion), \code{scale},
#'   \code{fitted.values}, \code{linear.predictors}, \code{residuals},
#'   \code{deviance}, \code{null.deviance}, \code{dev.expl}, \code{r.sq},
#'   \code{aic}, \code{smooths} (basis information used by \code{predict})
#'   and optimiser diagnostics.
#' @references Wood, S. N. (2017) \emph{Generalized Additive Models: An
#'   Introduction with R}, 2nd ed., CRC Press.  Wood, S. N. (2003) Thin
#'   plate regression splines, \emph{JRSS B} 65, 95-114.  Wood, S. N. (2011)
#'   Fast stable restricted maximum likelihood and marginal likelihood
#'   estimation of semiparametric generalized linear models, \emph{JRSS B}
#'   73, 3-36.  Wood, S. N. (2013) On p-values for smooth components of an
#'   extended generalized additive model, \emph{Biometrika} 100, 221-228.
#' @examples
#' set.seed(1)
#' d <- data.frame(x = runif(200), z = runif(200))
#' d$y <- sin(2 * pi * d$x) + d$z + rnorm(200, 0, 0.3)
#' fit <- morie_gam(y ~ s(x) + z, data = d)
#' fit
#' summary(fit)
#' head(predict(fit, newdata = data.frame(x = c(0.1, 0.5), z = 0.5)))
#' @export
morie_gam <- function(formula, data, family = c("gaussian", "binomial", "poisson"),
                      method = c("GCV.Cp", "REML"), weights = NULL, sp = NULL, ...) {
  family <- match.arg(family)
  method <- match.arg(method)
  G <- .gamn_setup(formula, data, family, weights)
  fam <- G$fam
  nS <- length(G$S)
  G$scale_known <- fam$family != "gaussian"
  G$criterion <- if (method == "REML") "REML" else if (G$scale_known) "UBRE" else "GCV"
  if (fam$family == "gaussian") {
    G$XWX <- crossprod(G$X * sqrt(G$wt))
    G$XWy <- drop(crossprod(G$X, G$wt * (G$y - G$offset)))
  }
  opt <- NULL
  if (!is.null(sp)) {
    if (length(sp) != nS || any(!is.finite(sp)) || any(sp < 0))
      stop(sprintf("sp must hold %d non-negative smoothing parameter(s)", nS))
    par <- log(sp)
    if (G$criterion == "REML" && !G$scale_known) {
      b <- .gamn_fit_score(c(par, 0), G, 0L)
      par <- c(par, log((b$dev + b$bSb) / (G$nobs - G$Mp)))
    }
    b <- .gamn_fit_score(par, G, if (nS > 0L) 1L else 0L)
  } else if (nS > 0L) {
    if (G$criterion == "GCV") {
      sp0 <- .gamn_initial_sp(G$X, G)
    } else {
      mus <- fam$mustart(G$y, G$wt)
      sp0 <- .gamn_initial_sp(sqrt(G$wt * fam$variance(mus)) * G$X, G)
    }
    par <- log(sp0)
    if (G$criterion == "REML" && !G$scale_known) {
      null.scale <- sum(fam$dev.resids(G$y, rep(mean(G$y), length(G$y)), G$wt)) / nrow(G$X)
      par <- c(par, log(null.scale / 10))
    }
    opt <- .gamn_newton(par, G)
    b <- opt$fit
    par <- opt$par
  } else {
    par <- if (G$criterion == "REML" && !G$scale_known) 0 else numeric(0)
    b <- .gamn_fit_score(par, G, 0L)
    if (G$criterion == "REML" && !G$scale_known) {
      # profile the scale: phi = Dp / (n - Mp)
      par <- log((b$dev + b$bSb) / (G$nobs - G$Mp))
      b <- .gamn_fit_score(par, G, 0L)
    }
  }
  .gamn_finish(G, b, par, opt, method)
}

.gamn_finish <- function(G, b, par, opt, method) {
  fam <- G$fam; nS <- length(G$S); n <- G$nobs
  y <- G$y; wt <- G$wt
  beta <- b$beta; names(beta) <- colnames(G$X)
  mu <- b$mu
  trA <- b$trA
  if (G$scale_known) {
    scale <- 1
  } else if (G$criterion == "REML") {
    scale <- exp(par[nS + 1L])
  } else {
    scale <- b$dev / (n - trA)
  }
  Vp <- b$A * scale
  F <- b$F
  Ve <- F %*% Vp
  dimnames(Vp) <- dimnames(Ve) <- list(names(beta), names(beta))
  edf <- b$edf; names(edf) <- names(beta)
  edf1 <- 2 * edf - rowSums(t(F) * F); names(edf1) <- names(beta)
  sm_lab <- vapply(G$smooths, `[[`, "", "label")
  edf_s <- vapply(G$smooths, function(s) sum(edf[s$first.para:s$last.para]), 0)
  names(edf_s) <- sm_lab
  sp <- b$lambda; names(sp) <- sm_lab[G$Sowner]
  wtdmu <- if (G$intercept) sum(wt * y) / sum(wt) else fam$linkinv(G$offset)
  nulldev <- sum(fam$dev.resids(y, rep_len(wtdmu, length(y)), wt))
  if (G$intercept && any(G$offset != 0)) nulldev <- .gamn_null_dev_offset(fam, y, wt, G$offset)
  dev <- b$dev
  residual.df <- length(y) - sum(edf)
  sw <- sqrt(wt)
  mean.y <- sum(wt * y) / sum(wt)
  r.sq <- 1 - stats::var(sw * (y - mu)) * (length(y) - 1) /
    (stats::var(sw * (y - mean.y)) * residual.df)
  dev1 <- if (G$scale_known) sum(wt) else dev
  fam_aic <- switch(fam$family,
    gaussian = stats::gaussian()$aic(y, NULL, mu, wt, dev1),
    binomial = stats::binomial()$aic(y, wt, mu, wt, dev1),
    poisson = stats::poisson()$aic(y, NULL, mu, wt, dev1))
  aic <- fam_aic + 2 * sum(edf)
  score <- b$score
  names(score) <- if (method == "REML") "REML" else "GCV.Cp"
  dres <- sign(y - mu) * sqrt(pmax(fam$dev.resids(y, mu, wt), 0))
  out <- list(
    coefficients = beta, Vp = Vp, Ve = Ve, edf = edf, edf1 = edf1,
    edf_smooth = edf_s, edf_total = sum(edf), sp = sp, score = score,
    scale = scale, scale.estimated = !G$scale_known,
    fitted.values = mu, linear.predictors = b$eta,
    residuals = dres, y = y, prior.weights = wt, working.weights = b$w,
    deviance = dev, null.deviance = nulldev, dev.expl = (nulldev - dev) / nulldev,
    r.sq = r.sq, residual.df = residual.df, aic = aic,
    family = fam$family, link = fam$link, method = method,
    criterion = G$criterion, nobs = length(y), nsdf = G$nsdf,
    smooths = G$smooths, pterms = G$pterms, contrasts = G$contrasts, assign = G$assign,
    xlevels = G$xlevels, formula = G$formula,
    R = .gamn_Rfactor(G$X * sqrt(b$w)),
    outer = if (is.null(opt)) NULL else list(conv = opt$conv, iter = opt$iter,
                                             grad = b$grad, hess = b$hess,
                                             par = opt$par),
    fit_converged = b$fit_conv, rows = G$rows)
  class(out) <- "morie_gam"
  out
}

# Deviance of the intercept-plus-offset model (mgcv refits glm(y ~
# offset(offset)) for the null deviance when an offset is present).
.gamn_null_dev_offset <- function(fam, y, wt, offset) {
  a <- 0
  if (fam$family == "gaussian") {
    a <- sum(wt * (y - offset)) / sum(wt)
  } else {
    for (it in 1:100) {
      mu <- fam$linkinv(a + offset)
      v <- fam$variance(mu)
      step <- sum(wt * (y - mu)) / sum(wt * v)
      a <- a + step
      if (abs(step) < 1e-12 * (abs(a) + 1)) break
    }
  }
  sum(fam$dev.resids(y, fam$linkinv(a + offset), wt))
}

# R factor of a QR decomposition with the columns in their original order.
.gamn_Rfactor <- function(WX) {
  q <- qr(WX)
  R <- qr.R(q)
  R[, q$pivot] <- R
  R
}


# ---------------------------------------------------------------------------
# Symmetric tridiagonal eigensolver following LAPACK 3.12 DSTEDC (with
# DSTEQR for blocks of size <= 25 and the DLAED0/1/2/3 divide and conquer
# merges above that), so that each eigenvector carries the sign LAPACK
# gives it.  mgcv's Lanczos routine obtains its Ritz vectors through
# DSTEDC, and the sign of each Ritz vector fixes the sign of the matching
# thin plate basis column, hence of its coefficient.
# ---------------------------------------------------------------------------

.gamn_lartg <- function(f, g) {
  if (g == 0) return(c(1, 0, f))
  if (f == 0) return(c(0, sign(g), abs(g)))
  d <- sqrt(f * f + g * g)
  c1 <- abs(f) / d
  r <- if (f >= 0) d else -d
  c(c1, g / r, r)
}

.gamn_laev2 <- function(a, b, c) {
  sm <- a + c; df <- a - c; adf <- abs(df); tb <- b + b; ab <- abs(tb)
  if (abs(a) > abs(c)) { acmx <- a; acmn <- c } else { acmx <- c; acmn <- a }
  rt <- if (adf > ab) adf * sqrt(1 + (ab / adf)^2) else
    if (adf < ab) ab * sqrt(1 + (adf / ab)^2) else ab * sqrt(2)
  if (sm < 0) {
    rt1 <- 0.5 * (sm - rt); sgn1 <- -1; rt2 <- (acmx / rt1) * acmn - (b / rt1) * b
  } else if (sm > 0) {
    rt1 <- 0.5 * (sm + rt); sgn1 <- 1; rt2 <- (acmx / rt1) * acmn - (b / rt1) * b
  } else { rt1 <- 0.5 * rt; rt2 <- -0.5 * rt; sgn1 <- 1 }
  if (df >= 0) { cs <- df + rt; sgn2 <- 1 } else { cs <- df - rt; sgn2 <- -1 }
  if (abs(cs) > ab) {
    ct <- -tb / cs; sn1 <- 1 / sqrt(1 + ct * ct); cs1 <- ct * sn1
  } else if (ab == 0) { cs1 <- 1; sn1 <- 0 } else {
    tn <- -cs / tb; cs1 <- 1 / sqrt(1 + tn * tn); sn1 <- tn * cs1
  }
  if (sgn1 == sgn2) { tn <- cs1; cs1 <- -sn1; sn1 <- tn }
  c(rt1, rt2, cs1, sn1)
}

# Apply plane rotations from the right to columns l..l+mm-1 of Z
# (DLASR with SIDE = 'R', PIVOT = 'V').
.gamn_lasr_rv <- function(Z, cs, sn, l, mm, forward) {
  js <- if (forward) seq_len(mm - 1L) else rev(seq_len(mm - 1L))
  for (j in js) {
    ct <- cs[j]; st <- sn[j]
    if (ct != 1 || st != 0) {
      a <- l + j - 1L
      tmp <- Z[, a + 1L]
      Z[, a + 1L] <- ct * tmp - st * Z[, a]
      Z[, a] <- st * tmp + ct * Z[, a]
    }
  }
  Z
}

# DSTEQR with COMPZ = 'I': implicit QL/QR on a tridiagonal matrix.
.gamn_dsteqr <- function(d, e) {
  n <- length(d)
  Z <- diag(n)
  if (n == 1L) return(list(values = d, vectors = Z))
  e <- c(e, 0)
  eps <- .Machine$double.eps / 2
  eps2 <- eps^2
  safmin <- .Machine$double.xmin
  ssfmax <- sqrt(1 / safmin) / 3
  ssfmin <- sqrt(safmin) / eps2
  nmaxit <- n * 30L; jtot <- 0L
  l1 <- 1L
  wc <- numeric(n); ws <- numeric(n)
  repeat {
    if (l1 > n) break
    if (l1 > 1L) e[l1 - 1L] <- 0
    m <- n
    if (l1 <= n - 1L) for (mm_ in l1:(n - 1L)) {
      tst <- abs(e[mm_])
      if (tst == 0) { m <- mm_; break }
      if (tst <= sqrt(abs(d[mm_])) * sqrt(abs(d[mm_ + 1L])) * eps) {
        e[mm_] <- 0; m <- mm_; break
      }
    }
    l <- l1; lsv <- l; lend <- m; lendsv <- lend; l1 <- m + 1L
    if (lend == l) next
    idx <- l:lend
    anorm <- max(abs(d[idx]), if (lend > l) abs(e[l:(lend - 1L)]) else 0)
    iscale <- 0L
    if (anorm == 0) next
    if (anorm > ssfmax) {
      iscale <- 1L; d[idx] <- d[idx] * (ssfmax / anorm)
      if (lend > l) e[l:(lend - 1L)] <- e[l:(lend - 1L)] * (ssfmax / anorm)
    } else if (anorm < ssfmin) {
      iscale <- 2L; d[idx] <- d[idx] * (ssfmin / anorm)
      if (lend > l) e[l:(lend - 1L)] <- e[l:(lend - 1L)] * (ssfmin / anorm)
    }
    if (abs(d[lend]) < abs(d[l])) { lend <- lsv; l <- lendsv }
    if (lend > l) {
      # QL iteration
      repeat {
        m <- lend
        if (l != lend) for (mm_ in l:(lend - 1L)) {
          tst <- abs(e[mm_])^2
          if (tst <= (eps2 * abs(d[mm_])) * abs(d[mm_ + 1L]) + safmin) { m <- mm_; break }
        }
        if (m < lend) e[m] <- 0
        p <- d[l]
        if (m == l) {
          d[l] <- p; l <- l + 1L
          if (l <= lend) next
          break
        }
        if (m == l + 1L) {
          r2 <- .gamn_laev2(d[l], e[l], d[l + 1L])
          Z <- .gamn_lasr_rv(Z, r2[3L], r2[4L], l, 2L, FALSE)
          d[l] <- r2[1L]; d[l + 1L] <- r2[2L]; e[l] <- 0
          l <- l + 2L
          if (l <= lend) next
          break
        }
        if (jtot == nmaxit) break
        jtot <- jtot + 1L
        g <- (d[l + 1L] - p) / (2 * e[l])
        r <- sqrt(g * g + 1)
        g <- d[m] - p + (e[l] / (g + (if (g >= 0) abs(r) else -abs(r))))
        s <- 1; cc <- 1; p <- 0
        for (i in (m - 1L):l) {
          f <- s * e[i]; b <- cc * e[i]
          rot <- .gamn_lartg(g, f); cc <- rot[1L]; s <- rot[2L]; r <- rot[3L]
          if (i != m - 1L) e[i + 1L] <- r
          g <- d[i + 1L] - p
          r <- (d[i] - g) * s + 2 * cc * b
          p <- s * r
          d[i + 1L] <- g + p
          g <- cc * r - b
          wc[i] <- cc; ws[i] <- -s
        }
        mm <- m - l + 1L
        Z <- .gamn_lasr_rv(Z, wc[l:(m - 1L)], ws[l:(m - 1L)], l, mm, FALSE)
        d[l] <- d[l] - p
        e[l] <- g
      }
    } else {
      # QR iteration
      repeat {
        m <- lend
        if (l != lend) for (mm_ in l:(lend + 1L)) {
          tst <- abs(e[mm_ - 1L])^2
          if (tst <= (eps2 * abs(d[mm_])) * abs(d[mm_ - 1L]) + safmin) { m <- mm_; break }
        }
        if (m > lend) e[m - 1L] <- 0
        p <- d[l]
        if (m == l) {
          d[l] <- p; l <- l - 1L
          if (l >= lend) next
          break
        }
        if (m == l - 1L) {
          r2 <- .gamn_laev2(d[l - 1L], e[l - 1L], d[l])
          Z <- .gamn_lasr_rv(Z, r2[3L], r2[4L], m, 2L, TRUE)
          d[l - 1L] <- r2[1L]; d[l] <- r2[2L]; e[l - 1L] <- 0
          l <- l - 2L
          if (l >= lend) next
          break
        }
        if (jtot == nmaxit) break
        jtot <- jtot + 1L
        g <- (d[l - 1L] - p) / (2 * e[l - 1L])
        r <- sqrt(g * g + 1)
        g <- d[m] - p + (e[l - 1L] / (g + (if (g >= 0) abs(r) else -abs(r))))
        s <- 1; cc <- 1; p <- 0
        for (i in m:(l - 1L)) {
          f <- s * e[i]; b <- cc * e[i]
          rot <- .gamn_lartg(g, f); cc <- rot[1L]; s <- rot[2L]; r <- rot[3L]
          if (i != m) e[i - 1L] <- r
          g <- d[i] - p
          r <- (d[i + 1L] - g) * s + 2 * cc * b
          p <- s * r
          d[i] <- g + p
          g <- cc * r - b
          wc[i] <- cc; ws[i] <- s
        }
        mm <- l - m + 1L
        Z <- .gamn_lasr_rv(Z, wc[m:(l - 1L)], ws[m:(l - 1L)], m, mm, TRUE)
        d[l] <- d[l] - p
        e[l - 1L] <- g
      }
    }
    if (iscale == 1L) {
      d[lsv:lendsv] <- d[lsv:lendsv] * (anorm / ssfmax)
      if (lendsv > lsv) e[lsv:(lendsv - 1L)] <- e[lsv:(lendsv - 1L)] * (anorm / ssfmax)
    } else if (iscale == 2L) {
      d[lsv:lendsv] <- d[lsv:lendsv] * (anorm / ssfmin)
      if (lendsv > lsv) e[lsv:(lendsv - 1L)] <- e[lsv:(lendsv - 1L)] * (anorm / ssfmin)
    }
    if (jtot >= nmaxit) break
  }
  # selection sort into ascending order
  for (ii in 2:n) {
    i <- ii - 1L; k <- i; p <- d[i]
    for (j in ii:n) if (d[j] < p) { k <- j; p <- d[j] }
    if (k != i) {
      d[k] <- d[i]; d[i] <- p
      tmp <- Z[, i]; Z[, i] <- Z[, k]; Z[, k] <- tmp
    }
  }
  list(values = d, vectors = Z)
}

# Secular equation roots for D + rho w w' (rho > 0, d ascending), returning
# the eigenvalues and DELTA[i, j] = d_i - lambda_j computed relative to the
# nearer pole (as DLAED4 does).
.gamn_secular <- function(d, w, rho) {
  K <- length(d)
  lam <- numeric(K); DEL <- matrix(0, K, K)
  w2 <- w * w
  for (j in seq_len(K)) {
    if (j < K) {
      mid <- (d[j + 1L] - d[j]) / 2
      fmid <- 1 + rho * sum(w2 / ((d - d[j]) - mid))
      if (fmid > 0) { o <- j; lo <- 0; hi <- mid } else { o <- j + 1L; lo <- -mid; hi <- 0 }
    } else {
      o <- K; lo <- 0; hi <- rho * sum(w2)
    }
    dd <- d - d[o]
    for (it in 1:200) {
      tau <- (lo + hi) / 2
      if (tau == lo || tau == hi) break
      fv <- 1 + rho * sum(w2 / (dd - tau))
      if (fv > 0) hi <- tau else lo <- tau
    }
    lam[j] <- d[o] + tau
    DEL[, j] <- dd - tau
  }
  list(values = lam, delta = DEL)
}

# DLAED1 (with DLAED2 deflation and DLAED3 eigenvectors): merge the
# eigensystems of the two halves of a torn tridiagonal matrix.
.gamn_dlaed1 <- function(D, Q, indxq, rho, cutpnt) {
  n <- length(D); n1 <- cutpnt; n2 <- n - n1
  z <- c(Q[cutpnt, seq_len(n1)], Q[cutpnt + 1L, n1 + seq_len(n2)])
  # ---- DLAED2
  if (rho < 0) z[n1 + seq_len(n2)] <- -z[n1 + seq_len(n2)]
  z <- z / sqrt(2)
  rho <- abs(2 * rho)
  indxq[n1 + seq_len(n2)] <- indxq[n1 + seq_len(n2)] + n1
  dlam <- D[indxq]
  indxc <- .gamn_dlamrg(dlam, n1, n2, 1L, 1L)
  indx <- indxq[indxc]
  eps <- .Machine$double.eps / 2
  tol <- 8 * eps * max(abs(D[which.max(abs(D))]), abs(z[which.max(abs(z))]))
  if (rho * max(abs(z)) <= tol) {
    Q <- Q[, indx, drop = FALSE]
    D <- D[indx]
    return(list(D = D, Q = Q, indxq = seq_len(n)))
  }
  coltyp <- c(rep(1L, n1), rep(3L, n2))
  K <- 0L; k2 <- n + 1L
  indxp <- integer(n); W <- numeric(n); dlambda <- numeric(n)
  j <- 1L; pj <- NA_integer_
  while (j <= n) {
    nj <- indx[j]
    if (rho * abs(z[nj]) <= tol) {
      k2 <- k2 - 1L; coltyp[nj] <- 4L; indxp[k2] <- nj
      j <- j + 1L
    } else { pj <- nj; break }
  }
  if (!is.na(pj)) {
    repeat {
      j <- j + 1L
      if (j > n) break
      nj <- indx[j]
      if (rho * abs(z[nj]) <= tol) {
        k2 <- k2 - 1L; coltyp[nj] <- 4L; indxp[k2] <- nj
      } else {
        s <- z[pj]; cc <- z[nj]
        tau <- sqrt(cc * cc + s * s)
        tt <- D[nj] - D[pj]
        cc <- cc / tau; s <- -s / tau
        if (abs(tt * cc * s) <= tol) {
          z[nj] <- tau; z[pj] <- 0
          if (coltyp[nj] != coltyp[pj]) coltyp[nj] <- 2L
          coltyp[pj] <- 4L
          qp <- Q[, pj]; qn <- Q[, nj]
          Q[, pj] <- cc * qp + s * qn
          Q[, nj] <- cc * qn - s * qp
          tt <- D[pj] * cc^2 + D[nj] * s^2
          D[nj] <- D[pj] * s^2 + D[nj] * cc^2
          D[pj] <- tt
          k2 <- k2 - 1L
          i <- 1L
          repeat {
            if (k2 + i <= n) {
              if (D[pj] < D[indxp[k2 + i]]) {
                indxp[k2 + i - 1L] <- indxp[k2 + i]
                indxp[k2 + i] <- pj
                i <- i + 1L
              } else { indxp[k2 + i - 1L] <- pj; break }
            } else { indxp[k2 + i - 1L] <- pj; break }
          }
          pj <- nj
        } else {
          K <- K + 1L
          dlambda[K] <- D[pj]; W[K] <- z[pj]; indxp[K] <- pj
          pj <- nj
        }
      }
    }
    K <- K + 1L
    dlambda[K] <- D[pj]; W[K] <- z[pj]; indxp[K] <- pj
  }
  ctot <- tabulate(coltyp, 4L)
  psm <- c(1L, 1L + ctot[1L], 1L + ctot[1L] + ctot[2L], 1L + ctot[1L] + ctot[2L] + ctot[3L])
  K <- n - ctot[4L]
  indx2 <- integer(n); indxc2 <- integer(n)
  for (jj in seq_len(n)) {
    js <- indxp[jj]; ct <- coltyp[js]
    indx2[psm[ct]] <- js; indxc2[psm[ct]] <- jj
    psm[ct] <- psm[ct] + 1L
  }
  # Q2 blocks and the deflated part
  c1 <- indx2[seq_len(ctot[1L])]
  c2 <- indx2[ctot[1L] + seq_len(ctot[2L])]
  c3 <- indx2[ctot[1L] + ctot[2L] + seq_len(ctot[3L])]
  c4 <- indx2[ctot[1L] + ctot[2L] + ctot[3L] + seq_len(ctot[4L])]
  Q2top <- Q[seq_len(n1), c(c1, c2), drop = FALSE]        # n1 x (ct1 + ct2)
  Q2bot <- Q[n1 + seq_len(n2), c(c2, c3), drop = FALSE]   # n2 x (ct2 + ct3)
  Qdef <- Q[, c4, drop = FALSE]
  Ddef <- D[c4]
  Dnew <- numeric(n); Qnew <- matrix(0, n, n)
  if (K < n) { Qnew[, (K + 1L):n] <- Qdef; Dnew[(K + 1L):n] <- Ddef }
  # ---- DLAED3
  dl <- dlambda[seq_len(K)]; w <- W[seq_len(K)]
  ix <- indxc2[seq_len(K)]
  if (K == 1L) {
    Dnew[1L] <- dl[1L] + rho * w[1L]^2
    Sv <- matrix(1, 1, 1)
  } else if (K == 2L) {
    Sv <- matrix(0, 2, 2)
    for (jj in 1:2) {
      r5 <- .gamn_dlaed5(jj, dl, w, rho)
      Dnew[jj] <- r5$dlam
      Sv[, jj] <- r5$delta[ix]
    }
  } else {
    sec <- .gamn_secular(dl, w, rho)
    Dnew[seq_len(K)] <- sec$values
    Qd <- sec$delta
    Wr <- diag(Qd)
    for (jj in seq_len(K)) {
      ii <- setdiff(seq_len(K), jj)
      Wr[ii] <- Wr[ii] * (Qd[ii, jj] / (dl[ii] - dl[jj]))
    }
    Wr <- ifelse(w >= 0, 1, -1) * sqrt(-Wr)
    Sv <- matrix(0, K, K)
    for (jj in seq_len(K)) {
      s <- Wr / Qd[, jj]
      s <- s / sqrt(sum(s * s))
      Sv[, jj] <- s[ix]
    }
  }
  n12 <- ctot[1L] + ctot[2L]; n23 <- ctot[2L] + ctot[3L]
  top <- if (n12 > 0L) Q2top %*% Sv[seq_len(n12), , drop = FALSE] else matrix(0, n1, K)
  bot <- if (n23 > 0L) Q2bot %*% Sv[ctot[1L] + seq_len(n23), , drop = FALSE] else matrix(0, n2, K)
  Qnew[seq_len(n1), seq_len(K)] <- top
  Qnew[n1 + seq_len(n2), seq_len(K)] <- bot
  indxq <- .gamn_dlamrg(Dnew, K, n - K, 1L, -1L)
  list(D = Dnew, Q = Qnew, indxq = indxq)
}

.gamn_dlaed5 <- function(i, d, z, rho) {
  del <- d[2L] - d[1L]
  if (i == 1L) {
    w <- 1 + 2 * rho * (z[2L]^2 - z[1L]^2) / del
    if (w > 0) {
      b <- del + rho * (z[1L]^2 + z[2L]^2); cc <- rho * z[1L]^2 * del
      tau <- 2 * cc / (b + sqrt(abs(b * b - 4 * cc)))
      dlam <- d[1L] + tau
      delta <- c(-z[1L] / tau, z[2L] / (del - tau))
    } else {
      b <- -del + rho * (z[1L]^2 + z[2L]^2); cc <- rho * z[2L]^2 * del
      tau <- if (b > 0) -2 * cc / (b + sqrt(b * b + 4 * cc)) else (b - sqrt(b * b + 4 * cc)) / 2
      dlam <- d[2L] + tau
      delta <- c(-z[1L] / (del + tau), -z[2L] / tau)
    }
  } else {
    b <- -del + rho * (z[1L]^2 + z[2L]^2); cc <- rho * z[2L]^2 * del
    tau <- if (b > 0) (b + sqrt(b * b + 4 * cc)) / 2 else 2 * cc / (-b + sqrt(b * b + 4 * cc))
    dlam <- d[2L] + tau
    delta <- c(-z[1L] / (del + tau), -z[2L] / tau)
  }
  list(dlam = dlam, delta = delta / sqrt(sum(delta^2)))
}

.gamn_dlamrg <- function(a, n1, n2, s1, s2) {
  ind1 <- if (s1 > 0) 1L else n1
  ind2 <- if (s2 > 0) 1L + n1 else n1 + n2
  out <- integer(n1 + n2); i <- 1L
  while (n1 > 0L && n2 > 0L) {
    if (a[ind1] <= a[ind2]) { out[i] <- ind1; ind1 <- ind1 + s1; n1 <- n1 - 1L } else {
      out[i] <- ind2; ind2 <- ind2 + s2; n2 <- n2 - 1L }
    i <- i + 1L
  }
  while (n2 > 0L) { out[i] <- ind2; ind2 <- ind2 + s2; n2 <- n2 - 1L; i <- i + 1L }
  while (n1 > 0L) { out[i] <- ind1; ind1 <- ind1 + s1; n1 <- n1 - 1L; i <- i + 1L }
  out
}

# DLAED0 with ICOMPQ = 2 on an unreduced tridiagonal block.
.gamn_dlaed0 <- function(d, e) {
  n <- length(d); smlsiz <- 25L
  sizes <- n
  while (sizes[length(sizes)] > smlsiz) {
    sizes <- as.vector(rbind(sizes %/% 2L, (sizes + 1L) %/% 2L))
  }
  subpbs <- length(sizes)
  cum <- cumsum(sizes)
  if (subpbs > 1L) for (i in seq_len(subpbs - 1L)) {
    sm1 <- cum[i]
    d[sm1] <- d[sm1] - abs(e[sm1])
    d[sm1 + 1L] <- d[sm1 + 1L] - abs(e[sm1])
  }
  Q <- matrix(0, n, n)
  indxq <- integer(n)
  starts <- c(1L, cum[-subpbs] + 1L)
  for (i in seq_len(subpbs)) {
    ii <- starts[i]:cum[i]
    st <- .gamn_dsteqr(d[ii], e[ii[-length(ii)]])
    d[ii] <- st$values
    Q[ii, ii] <- st$vectors
    indxq[ii] <- seq_along(ii)
  }
  bounds <- cum
  while (subpbs > 1L) {
    nb <- integer(0)
    for (i in seq(0L, subpbs - 2L, by = 2L)) {
      if (i == 0L) {
        submat <- 1L; matsiz <- bounds[2L]; msd2 <- bounds[1L]
      } else {
        submat <- bounds[i] + 1L; matsiz <- bounds[i + 2L] - bounds[i]; msd2 <- matsiz %/% 2L
      }
      ii <- submat:(submat + matsiz - 1L)
      r <- .gamn_dlaed1(d[ii], Q[ii, ii, drop = FALSE], indxq[ii], e[submat + msd2 - 1L], msd2)
      d[ii] <- r$D; Q[ii, ii] <- r$Q; indxq[ii] <- r$indxq
      nb <- c(nb, bounds[i + 2L])
    }
    bounds <- nb
    subpbs <- subpbs %/% 2L
  }
  list(values = d[indxq], vectors = Q[, indxq, drop = FALSE])
}

# DSTEDC with COMPZ = 'I'; eigenvalues ascending.
.gamn_dstedc <- function(d, e) {
  n <- length(d)
  if (n <= 25L) return(.gamn_dsteqr(d, e))
  Z <- diag(n)
  eps <- .Machine$double.eps / 2
  start <- 1L
  while (start <= n) {
    finish <- start
    while (finish < n) {
      tiny <- eps * sqrt(abs(d[finish])) * sqrt(abs(d[finish + 1L]))
      if (abs(e[finish]) > tiny) finish <- finish + 1L else break
    }
    m <- finish - start + 1L
    if (m > 1L) {
      ii <- start:finish
      ei <- if (m > 1L) e[start:(finish - 1L)] else numeric(0)
      if (m > 25L) {
        orgnrm <- max(abs(d[ii]), abs(ei))
        mul <- 1 / orgnrm   # DLASCL multiplies by cto / cfrom
        r <- .gamn_dlaed0(d[ii] * mul, ei * mul)
        d[ii] <- r$values * orgnrm
        Z[ii, ii] <- r$vectors
      } else {
        r <- .gamn_dsteqr(d[ii], ei)
        d[ii] <- r$values
        Z[ii, ii] <- r$vectors
      }
    }
    start <- finish + 1L
  }
  for (ii in 2:n) {
    i <- ii - 1L; k <- i; p <- d[i]
    for (j in ii:n) if (d[j] < p) { k <- j; p <- d[j] }
    if (k != i) {
      d[k] <- d[i]; d[i] <- p
      tmp <- Z[, i]; Z[, i] <- Z[, k]; Z[, k] <- tmp
    }
  }
  list(values = d, vectors = Z)
}


# ---------------------------------------------------------------------------
# Davies (1980) algorithm for the distribution of a linear combination of
# chi-squared variables (as used by mgcv's psum.chisq), Algol 60 / mgcv C
# code translated to R.
# ---------------------------------------------------------------------------

.gamn_log1pmx <- function(x) {
  if (abs(x) < 1e-2) {
    # series log(1 + x) - x = -x^2/2 + x^3/3 - ...
    s <- 0; term <- x
    for (k in 2:40) {
      term <- -term * x
      s <- s + term / k
      if (abs(term / k) < 1e-20 * max(abs(s), 1e-300)) break
    }
    s
  } else log1p(x) - x
}

.gamn_davies <- function(lb, nc, n, sigma, cc, lim = 100000L, acc = 2e-5) {
  r <- length(lb)
  ln1 <- function(x, first) if (first) log1p(x) else .gamn_log1pmx(x)
  errbd <- function(u, sigsq) {
    cx <- u * sigsq
    sum1 <- u * cx
    u <- u * 2
    for (j in r:1) {
      nj <- n[j]; lj <- lb[j]; ncj <- nc[j]; x <- u * lj
      y <- 1 - x; cx <- cx + lj * (ncj / y + nj) / y
      xy <- x / y
      sum1 <- sum1 + ncj * xy * xy + nj * (x * xy + ln1(-x, FALSE))
    }
    list(p = exp(-0.5 * sum1), cx = cx)
  }
  ctff <- function(accx, upn, mean, lmin, lmax, sigsq) {
    u2 <- upn; u1 <- 0; c1 <- mean
    rb <- if (u2 > 0) 2 * lmax else 2 * lmin
    repeat {
      eb <- errbd(u2 / (1 + u2 * rb), sigsq)
      c2 <- eb$cx
      if (eb$p <= accx) break
      u1 <- u2; c1 <- c2; u2 <- u2 * 2
    }
    repeat {
      if ((c1 - mean) / (c2 - mean) >= 0.9) break
      u <- (u1 + u2) * 0.5
      eb <- errbd(u / (1 + u * rb), sigsq)
      if (eb$p > accx) { u1 <- u; c1 <- eb$cx } else { u2 <- u; c2 <- eb$cx }
    }
    list(c = c2, up = u2)
  }
  truncation <- function(u, tausq, sigsq) {
    sum1 <- 0; prod2 <- 0; prod3 <- 0; s <- 0
    sum2 <- (sigsq + tausq) * u * u
    prod1 <- 2 * sum2; u <- 2 * u
    for (j in seq_len(r)) {
      lj <- lb[j]; ncj <- nc[j]; nj <- n[j]
      x <- u * lj; x <- x * x
      sum1 <- sum1 + ncj * x / (1 + x)
      if (x > 1) {
        prod2 <- prod2 + nj * log(x)
        prod3 <- prod3 + nj * ln1(x, TRUE)
        s <- s + nj
      } else prod1 <- prod1 + nj * ln1(x, TRUE)
    }
    sum1 <- sum1 * 0.5; prod2 <- prod2 + prod1; prod3 <- prod3 + prod1
    x <- exp(-sum1 - 0.25 * prod2) / pi
    y <- exp(-sum1 - 0.25 * prod3) / pi
    err1 <- if (s == 0) 1 else 2 * x / s
    err2 <- if (prod3 > 1) 2.5 * y else 1
    if (err2 < err1) err1 <- err2
    x <- 0.5 * sum2
    err2 <- if (x <= y) 1 else y / x
    if (err1 < err2) err1 else err2
  }
  findu <- function(utx, accx, sigsq) {
    ut <- utx; u <- ut * 0.25
    if (truncation(u, 0, sigsq) > accx) {
      while (truncation(ut, 0, sigsq) > accx) ut <- ut * 4
    } else {
      ut <- u; u <- u / 4
      while (truncation(u, 0, sigsq) <= accx) { ut <- u; u <- u / 4 }
    }
    for (a in c(2, 1.4, 1.2, 1.1)) {
      u <- ut / a
      if (truncation(u, 0, sigsq) <= accx) ut <- u
    }
    ut
  }
  integ <- function(nterm, interv, tausq, main, sigsq, st) {
    inpi <- interv / pi
    for (k in nterm:0) {
      u <- (k + 0.5) * interv; sum1 <- -2 * u * cc
      sum2 <- abs(sum1); sum3 <- -0.5 * sigsq * u * u
      for (j in r:1) {
        nj <- n[j]; x <- 2 * lb[j] * u; y <- x * x
        sum3 <- sum3 - 0.25 * nj * ln1(y, TRUE)
        y <- nc[j] * x / (1 + y); z <- nj * atan(x) + y
        sum1 <- sum1 + z; sum2 <- sum2 + abs(z)
        sum3 <- sum3 - 0.5 * x * y
      }
      x <- inpi * exp(sum3) / u
      if (!main) x <- x * (1 - exp(-0.5 * tausq * u * u))
      st$intl <- st$intl + sin(0.5 * sum1) * x
      st$ersm <- st$ersm + 0.5 * sum2 * x
    }
    st
  }
  th <- order(abs(lb), decreasing = TRUE)
  ln28 <- log(2) / 8
  cfe <- function(x) {
    axl <- abs(x); sxl <- if (x < 0) -1 else 1
    sum1 <- 0
    for (j in r:1) {
      t <- th[j]
      if (lb[t] * sxl > 0) {
        lj <- abs(lb[t]); axl1 <- axl - lj * (n[t] + nc[t])
        axl2 <- lj / ln28
        if (axl1 > axl2) axl <- axl1 else {
          if (axl > axl2) axl <- axl2
          sum1 <- (axl - axl1) / lj
          if (j > 1L) for (kk in (j - 1L):1) sum1 <- sum1 + n[th[kk]] + nc[th[kk]]
          break
        }
      }
    }
    if (sum1 > 100) list(v = 1, fail = TRUE) else
      list(v = 2^(sum1 * 0.25) / (pi * axl * axl), fail = FALSE)
  }
  st <- list(intl = 0, ersm = 0)
  acc1 <- acc
  sigsq <- sigma * sigma
  sd <- sigsq
  lmax <- 0; lmin <- 0; mean <- 0
  for (j in seq_len(r)) {
    nj <- n[j]; lj <- lb[j]; ncj <- nc[j]
    sd <- sd + lj * lj * (2 * nj + 4 * ncj)
    mean <- mean + lj * (nj + ncj)
    if (lmax < lj) lmax <- lj else if (lmin > lj) lmin <- lj
  }
  if (sd == 0) return(list(p = if (cc > 0) 1 else 0, ifault = 0L))
  sd <- sqrt(sd)
  almx <- if (lmax < -lmin) -lmin else lmax
  utx <- 16 / sd; up <- 4.5 / sd; un <- -up
  utx <- findu(utx, 0.5 * acc1, sigsq)
  if (cc != 0 && almx > 0.07 * sd) {
    cf <- cfe(cc)
    tausq <- 0.25 * acc1 / cf$v
    if (!cf$fail) {
      if (truncation(utx, tausq, sigsq) < 0.2 * acc1) {
        sigsq <- sigsq + tausq
        utx <- findu(utx, 0.25 * acc1, sigsq)
      }
    }
  }
  acc1 <- 0.5 * acc1
  repeat {
    ct <- ctff(acc1, up, mean, lmin, lmax, sigsq); up <- ct$up
    d1 <- ct$c - cc
    if (d1 < 0) return(list(p = 1, ifault = 0L))
    ct <- ctff(acc1, un, mean, lmin, lmax, sigsq); un <- ct$up
    d2 <- cc - ct$c
    if (d2 < 0) return(list(p = 0, ifault = 0L))
    intv <- if (d1 > d2) 2 * pi / d1 else 2 * pi / d2
    x <- utx / intv; nt <- floor(x); if (x - nt > 0.5) nt <- nt + 1
    x <- 3 / sqrt(acc1); ntm <- floor(x); if (x - ntm > 0.5) ntm <- ntm + 1
    if (nt > ntm * 1.5) {
      intv1 <- utx / ntm; x <- 2 * pi / intv1
      if (x <= abs(cc)) break
      c1 <- cfe(cc - x); c2 <- cfe(cc + x)
      tausq <- 0.33 * acc1 / (1.1 * (c1$v + c2$v))
      if (c2$fail) break
      acc1 <- acc1 * 0.67
      if (ntm > lim) return(list(p = -1, ifault = 1L))
      st <- integ(ntm, intv1, tausq, FALSE, sigsq, st)
      lim <- lim - ntm; sigsq <- sigsq + tausq
      utx <- findu(utx, 0.25 * acc1, sigsq)
      acc1 <- 0.75 * acc1
    } else break
  }
  if (nt > lim) return(list(p = -1, ifault = 1L))
  st <- integ(nt, intv, 0, TRUE, sigsq, st)
  p <- 0.5 - st$intl
  x <- st$ersm + acc / 10
  ifault <- 0L
  jj <- 1
  for (i in 1:4) { if (jj * x == jj * st$ersm) ifault <- 2L; jj <- jj * 2 }
  list(p = p, ifault = ifault)
}

# Pr(sum_j lb_j X_j > q), X_j ~ chi^2_df_j (mgcv psum.chisq, upper tail).
.gamn_psum_chisq <- function(q, lb, df = rep(1, length(lb))) {
  df <- round(df)
  r <- .gamn_davies(lb, rep(0, length(lb)), df, 0, q)
  if (r$ifault != 0L && r$ifault != 2L) {
    # Liu et al. / Pearson approximation, as mgcv falls back on
    return(.gamn_liu2(q, lb, df))
  }
  1 - r$p
}

.gamn_liu2 <- function(x, lambda, h = rep(1, length(lambda))) {
  lh <- lambda * h
  muQ <- sum(lh)
  lh <- lh * lambda; c2 <- sum(lh)
  lh <- lh * lambda; c3 <- sum(lh)
  if (x <= 0 || c2 <= 0) return(1)
  s1 <- c3 / c2^1.5
  s2 <- sum(lh * lambda) / c2^2
  sigQ <- sqrt(2 * c2)
  t <- (x - muQ) / sigQ
  if (s1^2 > s2) {
    a <- 1 / (s1 - sqrt(s1^2 - s2)); delta <- s1 * a^3 - a^2; l <- a^2 - 2 * delta
  } else {
    a <- 1 / s1; delta <- 0
    if (c3 == 0) return(1)
    l <- c2^3 / c3^2
  }
  stats::pchisq(t * sqrt(2) * a + l + delta, df = l, ncp = delta, lower.tail = FALSE)
}

# Wood (2013) test statistic and p-value for one smooth (mgcv testStat).
.gamn_test_stat <- function(p, X, V, rank, res.df = -1) {
  qrx <- qr(X, tol = 0)
  R <- qr.R(qrx)
  V <- R %*% V[qrx$pivot, qrx$pivot, drop = FALSE] %*% t(R)
  V <- (V + t(V)) / 2
  ed <- eigen(V, symmetric = TRUE)
  siv <- sign(ed$vectors[1L, ]); siv[siv == 0] <- 1
  ed$vectors <- sweep(ed$vectors, 2L, siv, "*")
  k <- max(0, floor(rank))
  nu <- abs(rank - k)
  k1 <- if (nu > 0) k + 1 else k
  r.est <- sum(ed$values > max(ed$values) * .Machine$double.eps^0.9)
  if (r.est < k1) { k1 <- k <- r.est; nu <- 0; rank <- r.est }
  vec <- ed$vectors
  if (k1 < ncol(vec)) vec <- vec[, seq_len(k1), drop = FALSE]
  if (nu > 0 && k > 0) {
    if (k > 1) vec[, 1:(k - 1)] <- t(t(vec[, 1:(k - 1), drop = FALSE]) / sqrt(ed$values[1:(k - 1)]))
    b12 <- 0.5 * nu * (1 - nu)
    if (b12 < 0) b12 <- 0
    b12 <- sqrt(b12)
    B <- matrix(c(1, b12, b12, nu), 2, 2)
    ev <- diag(ed$values[k:k1]^-0.5, nrow = k1 - k + 1)
    B <- ev %*% B %*% ev
    eb <- eigen(B, symmetric = TRUE)
    rB <- eb$vectors %*% diag(sqrt(eb$values)) %*% t(eb$vectors)
    vec1 <- vec
    vec1[, k:k1] <- t(rB %*% diag(c(-1, 1)) %*% t(vec[, k:k1]))
    vec[, k:k1] <- t(rB %*% t(vec[, k:k1]))
  } else {
    vec1 <- vec <- if (k == 0) t(t(vec) * sqrt(1 / ed$values[1])) else
      t(t(vec) / sqrt(ed$values[1:k]))
    if (k == 1) rank <- 1
  }
  d <- sum((t(vec) %*% (R %*% p))^2)
  d1 <- sum((t(vec1) %*% (R %*% p))^2)
  rank1 <- rank
  if (nu > 0) {
    if (k1 == 1) { rank1 <- val <- 1 } else {
      val <- rep(1, k1)
      rp <- nu + 1
      val[k] <- (rp + sqrt(rp * (2 - rp))) / 2
      val[k1] <- (rp - val[k])
    }
    if (res.df <= 0) {
      pval <- (.gamn_psum_chisq(d, val) + .gamn_psum_chisq(d1, val)) / 2
    } else {
      k0 <- max(1, round(res.df))
      pval <- (.gamn_psum_chisq(0, c(val, -d / k0), df = c(rep(1, length(val)), k0)) +
                 .gamn_psum_chisq(0, c(val, -d1 / k0), df = c(rep(1, length(val)), k0))) / 2
    }
  } else pval <- 2
  if (pval > 1) {
    if (res.df <= 0) {
      pval <- (stats::pchisq(d, df = rank1, lower.tail = FALSE) +
                 stats::pchisq(d1, df = rank1, lower.tail = FALSE)) / 2
    } else {
      pval <- (stats::pf(d / rank1, rank1, res.df, lower.tail = FALSE) +
                 stats::pf(d1 / rank1, rank1, res.df, lower.tail = FALSE)) / 2
    }
  }
  list(stat = d, pval = min(1, pval), rank = rank)
}


# ---------------------------------------------------------------------------
# Methods
# ---------------------------------------------------------------------------

# Model matrix of a fitted morie_gam at new data.
.gamn_predict_matrix <- function(object, newdata) {
  newdata <- as.data.frame(newdata)
  tt <- stats::delete.response(object$pterms)
  mf <- stats::model.frame(tt, newdata, xlev = object$xlevels, na.action = stats::na.pass)
  Xp <- stats::model.matrix(tt, mf, contrasts.arg = object$contrasts)
  off <- stats::model.offset(mf)
  X <- Xp
  for (sm in object$smooths) {
    cov <- vapply(sm$exprs, .gamn_cov, numeric(nrow(newdata)), data = newdata)
    cov <- matrix(cov, nrow(newdata), length(sm$exprs))
    ok <- stats::complete.cases(cov)
    Xs <- matrix(NA_real_, nrow(newdata), sm$last.para - sm$first.para + 1L)
    if (any(ok)) {
      raw <- .gamn_smooth_predict_raw(sm, cov[ok, , drop = FALSE])
      Xs[ok, ] <- .gamn_apply_con(raw, sm$con)
    }
    X <- cbind(X, Xs)
  }
  colnames(X) <- names(object$coefficients)
  attr(X, "offset") <- if (is.null(off)) rep(0, nrow(X)) else as.numeric(off)
  X
}

#' Predictions from a native generalized additive model
#'
#' @param object A \code{"morie_gam"} fit.
#' @param newdata Optional data frame; the fitted values are returned when
#'   it is missing.
#' @param type \code{"link"} (linear predictor), \code{"response"},
#'   \code{"terms"} (one column per parametric term and smooth, centred as
#'   in the fit) or \code{"lpmatrix"} (the model matrix at \code{newdata}).
#' @param se.fit Logical; also return standard errors from the Bayesian
#'   covariance \code{Vp} (delta method for \code{"response"}).
#' @param ... Unused.
#' @return A vector (or matrix for \code{"terms"} and \code{"lpmatrix"}),
#'   or a list with \code{fit} and \code{se.fit}.
#' @export
predict.morie_gam <- function(object, newdata, type = c("link", "response", "terms", "lpmatrix"),
                              se.fit = FALSE, ...) {
  type <- match.arg(type)
  X <- if (missing(newdata) || is.null(newdata)) NULL else .gamn_predict_matrix(object, newdata)
  if (is.null(X)) {
    if (type == "lpmatrix" || type == "terms" || se.fit) {
      stop("supply newdata for this prediction type (the fitted model matrix is not stored)")
    }
    return(if (type == "link") object$linear.predictors else object$fitted.values)
  }
  if (type == "lpmatrix") return(X)
  b <- object$coefficients
  if (type == "terms") {
    labs <- attr(object$pterms, "term.labels")
    cols <- list()
    for (i in seq_along(labs)) cols[[labs[i]]] <- which(object$assign == i)
    for (sm in object$smooths) cols[[sm$label]] <- sm$first.para:sm$last.para
    fit <- vapply(cols, function(ii) drop(X[, ii, drop = FALSE] %*% b[ii]), numeric(nrow(X)))
    fit <- matrix(fit, nrow(X), length(cols), dimnames = list(NULL, names(cols)))
    if (!se.fit) return(fit)
    se <- vapply(cols, function(ii) {
      Xi <- X[, ii, drop = FALSE]
      sqrt(pmax(rowSums((Xi %*% object$Vp[ii, ii, drop = FALSE]) * Xi), 0))
    }, numeric(nrow(X)))
    se <- matrix(se, nrow(X), length(cols), dimnames = list(NULL, names(cols)))
    return(list(fit = fit, se.fit = se))
  }
  eta <- drop(X %*% b) + attr(X, "offset")
  fam <- .gamn_family(object$family)
  fit <- if (type == "link") eta else fam$linkinv(eta)
  if (!se.fit) return(fit)
  se <- sqrt(pmax(rowSums((X %*% object$Vp) * X), 0))
  if (type == "response") {
    dmu <- switch(object$family, gaussian = rep(1, length(eta)),
                  binomial = fit * (1 - fit), poisson = fit)
    se <- se * abs(dmu)
  }
  list(fit = fit, se.fit = se)
}

#' Print a native generalized additive model
#'
#' @param x A \code{"morie_gam"} fit.
#' @param ... Unused.
#' @return \code{x}, invisibly.
#' @keywords internal
#' @export
print.morie_gam <- function(x, ...) {
  cat("\nFamily:", x$family, "\nLink function:", x$link, "\n\n")
  cat("Formula:\n"); print(x$formula)
  if (length(x$edf_smooth)) {
    cat("\nEstimated degrees of freedom:\n")
    print(round(x$edf_smooth, 4))
    cat(sprintf(" total = %.4f\n", x$edf_total))
  }
  cat(sprintf("\n%s score: %.8g", if (x$method == "REML") "REML" else
              if (x$criterion == "UBRE") "UBRE" else "GCV", unname(x$score)))
  if (x$scale.estimated) cat(sprintf("   scale est. = %.6g", x$scale))
  cat(sprintf("\nn = %d\n", x$nobs))
  invisible(x)
}

#' Summary of a native generalized additive model
#'
#' Parametric coefficient table and approximate significance of smooth
#' terms by the test of Wood (2013), as \code{summary.gam} reports them.
#'
#' @param object A \code{"morie_gam"} fit.
#' @param ... Unused.
#' @return An object of class \code{"summary.morie_gam"} with \code{p.table},
#'   \code{pTerms.table}, \code{s.table}, \code{r.sq}, \code{dev.expl},
#'   \code{score}, \code{scale}, \code{n} and \code{residual.df}.
#' @export
summary.morie_gam <- function(object, ...) {
  est.disp <- object$scale.estimated
  Vp <- object$Vp
  se <- sqrt(diag(Vp))
  residual.df <- object$residual.df
  p.table <- NULL
  if (object$nsdf > 0L) {
    ind <- seq_len(object$nsdf)
    cf <- object$coefficients[ind]
    tval <- cf / se[ind]
    if (est.disp) {
      pv <- 2 * stats::pt(abs(tval), df = residual.df, lower.tail = FALSE)
      p.table <- cbind(cf, se[ind], tval, pv)
      colnames(p.table) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
    } else {
      pv <- 2 * stats::pnorm(abs(tval), lower.tail = FALSE)
      p.table <- cbind(cf, se[ind], tval, pv)
      colnames(p.table) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
    }
    rownames(p.table) <- names(cf)
  }
  labs <- attr(object$pterms, "term.labels")
  pTerms.table <- NULL
  if (length(labs)) {
    tab <- t(vapply(seq_along(labs), function(i) {
      ii <- which(object$assign == i)
      b <- object$coefficients[ii]; V <- Vp[ii, ii, drop = FALSE]
      if (length(b) == 1L) { nb <- 1; chi <- b * b / V[1L, 1L] } else {
        D <- eigen(V, symmetric = TRUE)
        keep <- D$values > .Machine$double.eps^0.5 * D$values[1L]
        nb <- sum(keep)
        Vi <- D$vectors[, keep, drop = FALSE] %*% (t(D$vectors[, keep, drop = FALSE]) / D$values[keep])
        chi <- drop(t(b) %*% Vi %*% b)
      }
      pv <- if (est.disp) stats::pf(chi / nb, nb, residual.df, lower.tail = FALSE) else
        stats::pchisq(chi, nb, lower.tail = FALSE)
      c(nb, if (est.disp) chi / nb else chi, pv)
    }, numeric(3)))
    dimnames(tab) <- list(labs, c("df", if (est.disp) "F" else "Chi.sq", "p-value"))
    pTerms.table <- tab
  }
  s.table <- NULL
  if (length(object$smooths)) {
    s.table <- t(vapply(object$smooths, function(sm) {
      ii <- sm$first.para:sm$last.para
      V <- Vp[ii, ii, drop = FALSE]
      edfi <- sum(object$edf[ii]); edf1i <- sum(object$edf1[ii])
      Xt <- object$R[, ii, drop = FALSE]
      res <- .gamn_test_stat(object$coefficients[ii], Xt, V, min(ncol(Xt), edf1i),
                             res.df = if (est.disp) residual.df else -1)
      c(edfi, res$rank, if (est.disp) res$stat / res$rank else res$stat, res$pval)
    }, numeric(4)))
    dimnames(s.table) <- list(vapply(object$smooths, `[[`, "", "label"),
                              c("edf", "Ref.df", if (est.disp) "F" else "Chi.sq", "p-value"))
  }
  out <- list(p.table = p.table, pTerms.table = pTerms.table, s.table = s.table,
              r.sq = object$r.sq, dev.expl = object$dev.expl, score = object$score,
              method = object$method, criterion = object$criterion, scale = object$scale,
              n = object$nobs, residual.df = residual.df, family = object$family,
              link = object$link, formula = object$formula)
  class(out) <- "summary.morie_gam"
  out
}

#' Print the summary of a native generalized additive model
#'
#' @param x A \code{"summary.morie_gam"} object.
#' @param ... Unused.
#' @return \code{x}, invisibly.
#' @keywords internal
#' @export
print.summary.morie_gam <- function(x, ...) {
  cat("\nFamily:", x$family, "\nLink function:", x$link, "\n\nFormula:\n")
  print(x$formula)
  if (!is.null(x$p.table)) {
    cat("\nParametric coefficients:\n")
    stats::printCoefmat(x$p.table, signif.stars = TRUE)
  }
  if (!is.null(x$s.table)) {
    cat("\nApproximate significance of smooth terms:\n")
    stats::printCoefmat(x$s.table, has.Pvalue = TRUE, P.values = TRUE, cs.ind = 1L,
                        signif.stars = TRUE)
  }
  cat(sprintf("\nR-sq.(adj) = %.3g   Deviance explained = %.1f%%\n", x$r.sq, 100 * x$dev.expl))
  cat(sprintf("%s = %.6g   Scale est. = %.6g   n = %d\n",
              if (x$method == "REML") "-REML" else if (x$criterion == "UBRE") "UBRE" else "GCV",
              unname(x$score), x$scale, x$n))
  invisible(x)
}

#' Coefficients of a native generalized additive model
#'
#' @param object A \code{"morie_gam"} fit.
#' @param ... Unused.
#' @return The named coefficient vector.
#' @keywords internal
#' @export
coef.morie_gam <- function(object, ...) object$coefficients

#' Bayesian posterior covariance of a native generalized additive model
#'
#' @param object A \code{"morie_gam"} fit.
#' @param ... Unused.
#' @return The matrix \code{Vp}.
#' @keywords internal
#' @export
vcov.morie_gam <- function(object, ...) object$Vp

#' Fitted values of a native generalized additive model
#'
#' @param object A \code{"morie_gam"} fit.
#' @param ... Unused.
#' @return Fitted means.
#' @keywords internal
#' @export
fitted.morie_gam <- function(object, ...) object$fitted.values

#' Residuals of a native generalized additive model
#'
#' @param object A \code{"morie_gam"} fit.
#' @param type \code{"deviance"} (default), \code{"pearson"},
#'   \code{"working"} or \code{"response"}.
#' @param ... Unused.
#' @return A residual vector.
#' @keywords internal
#' @export
residuals.morie_gam <- function(object, type = c("deviance", "pearson", "working", "response"), ...) {
  type <- match.arg(type)
  fam <- .gamn_family(object$family)
  y <- object$y; mu <- object$fitted.values; wt <- object$prior.weights
  switch(type,
    deviance = sign(y - mu) * sqrt(pmax(fam$dev.resids(y, mu, wt), 0)),
    pearson = (y - mu) * sqrt(wt) / sqrt(fam$variance(mu)),
    working = (y - mu) / switch(object$family, gaussian = 1, binomial = mu * (1 - mu), poisson = mu),
    response = y - mu)
}

#' Log-likelihood of a native generalized additive model
#'
#' As \code{logLik.gam}: the value is \code{edf + (scale estimated) -
#' AIC / 2}; the degrees of freedom are the total EDF plus one for an
#' estimated scale.
#'
#' @param object A \code{"morie_gam"} fit.
#' @param ... Unused.
#' @return An object of class \code{"logLik"}.
#' @keywords internal
#' @export
logLik.morie_gam <- function(object, ...) {
  sc.p <- as.numeric(object$scale.estimated)
  p <- object$edf_total + sc.p
  val <- p - object$aic / 2
  attr(val, "df") <- min(p, length(object$coefficients) + sc.p)
  attr(val, "nobs") <- object$nobs
  class(val) <- "logLik"
  val
}
