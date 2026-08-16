# R arm of crkbsg -- ordinary cokriging under a linear model of
# coregionalisation. Wackernagel, H. (2003) Multivariate Geostatistics,
# 3rd ed., Springer, Ch. 24-25; Goovaerts, P. (1997) Geostatistics for
# Natural Resources Evaluation, OUP, Sec. 6.2.
# Mirrors src/morie/fn/crkbsg.py.

.crkbsg_EPS <- 1e-12

.crkbsg_DEFAULT <- list(model = "exponential", range = 1.0, b11 = 1.0,
                        b22 = 1.0, b12 = 0.0, nugget11 = 0.0,
                        nugget22 = 0.0, nugget12 = 0.0)

.crkbsg_rows <- function(x) {
  if (is.matrix(x)) {
    m <- x
  } else if (is.data.frame(x)) {
    m <- as.matrix(x)
  } else if (is.list(x)) {
    m <- do.call(rbind, lapply(x, as.numeric))
  } else {
    m <- matrix(as.numeric(x), nrow = 1L)
  }
  storage.mode(m) <- "double"
  m
}

# Correlogram of the basic structure; rho(0) = 1.
.crkbsg_rho <- function(h, model, rng) {
  if (h <= 0.0) return(1.0)
  if (rng <= .crkbsg_EPS) return(0.0)
  if (model == "spherical") {
    if (h >= rng) return(0.0)
    r <- h / rng
    return(1.0 - (1.5 * r - 0.5 * r ^ 3))
  }
  if (model == "exponential") return(exp(-3.0 * h / rng))
  if (model == "gaussian") return(exp(-3.0 * (h / rng) ^ 2))
  stop(sprintf(paste0("crkbsg: model must be spherical, exponential or ",
                      "gaussian, got '%s'"), model))
}

.crkbsg_dist <- function(a, b) sqrt(sum((a - b) ^ 2))

# Gaussian elimination with partial pivoting. The cokriging matrix is
# symmetric but NOT positive definite -- the Lagrange rows see to that --
# so a Cholesky solve is not available.
.crkbsg_solve <- function(A, b) {
  n <- length(b)
  M <- cbind(A, b)
  for (col in seq_len(n)) {
    piv <- col
    for (r in seq.int(col, n)) if (abs(M[r, col]) > abs(M[piv, col])) piv <- r
    if (abs(M[piv, col]) < 1e-300)
      stop(paste0("crkbsg: the cokriging system is singular -- duplicated ",
                  "sample locations, or a coregionalisation matrix of ",
                  "deficient rank"))
    if (piv != col) {
      tmp <- M[col, ]; M[col, ] <- M[piv, ]; M[piv, ] <- tmp
    }
    d <- M[col, col]
    if (col < n) for (r in seq.int(col + 1L, n)) {
      f <- M[r, col] / d
      if (f != 0.0)
        M[r, seq.int(col, n + 1L)] <- M[r, seq.int(col, n + 1L)] -
          f * M[col, seq.int(col, n + 1L)]
    }
  }
  x <- numeric(n)
  for (r in seq.int(n, 1L)) {
    s <- M[r, n + 1L]
    if (r < n) s <- s - sum(M[r, seq.int(r + 1L, n)] * x[seq.int(r + 1L, n)])
    x[r] <- s / M[r, r]
  }
  x
}

#' morie_crkbsg_cokriging
#'
#' Part of the crkbsg_native implementation; see the file header for the
#' source it follows.
#'
#' @param coords See Usage.
#' @param y See Usage.
#' @param z See Usage.
#' @param s_predict See Usage.
#' @param cross_variogram Defaults to \code{NULL}.
#' @param coords_z Defaults to \code{NULL}.
#' @return A list with \code{estimate}, \code{prediction}, \code{variance}, \code{std_error}, \code{kriging_prediction}, \code{kriging_variance}, \code{variance_reduction}, \code{weights_primary}, \code{weights_secondary}, \code{lagrange}, \code{targets}, \code{coregionalisation}, \code{nugget_matrix}, \code{model}, \code{range}, \code{n_primary}, \code{n_secondary}, \code{method}, \code{note}.
#' @export
morie_crkbsg_cokriging <- function(coords, y, z, s_predict,
                                   cross_variogram = NULL, coords_z = NULL) {
  C1 <- .crkbsg_rows(coords)
  yv <- as.numeric(y)
  zv <- as.numeric(z)
  C2 <- if (is.null(coords_z)) C1 else .crkbsg_rows(coords_z)
  n1 <- nrow(C1); n2 <- nrow(C2)
  if (n1 == 0L) stop("crkbsg: no primary observations")
  if (length(yv) != n1)
    stop(sprintf("crkbsg: %d primary locations but %d values", n1, length(yv)))
  if (length(zv) != n2)
    stop(sprintf("crkbsg: %d secondary locations but %d values", n2,
                 length(zv)))
  d <- ncol(C1)
  if (ncol(C2) != d)
    stop("crkbsg: all coordinates must have the same dimension")

  par <- .crkbsg_DEFAULT
  if (!is.null(cross_variogram) && length(cross_variogram) > 0L) {
    for (key in names(cross_variogram)) {
      if (!(key %in% names(.crkbsg_DEFAULT)))
        stop(sprintf("crkbsg: unknown cross_variogram key '%s'", key))
      par[[key]] <- cross_variogram[[key]]
    }
  }
  model <- as.character(par$model)
  rng <- as.numeric(par$range)
  b11 <- as.numeric(par$b11); b22 <- as.numeric(par$b22)
  b12 <- as.numeric(par$b12)
  n11 <- as.numeric(par$nugget11); n22 <- as.numeric(par$nugget22)
  n12 <- as.numeric(par$nugget12)
  if (rng <= 0.0) stop("crkbsg: the range must be positive")
  # permissibility: an indefinite B gives negative prediction variances
  if (b11 < 0.0 || b22 < 0.0 || b11 * b22 < b12 * b12 - 1e-12)
    stop(sprintf(paste0("crkbsg: the coregionalisation matrix is not ",
                        "positive semidefinite (b11*b22 = %.6g < b12^2 = ",
                        "%.6g)"), b11 * b22, b12 * b12))
  if (n11 < 0.0 || n22 < 0.0 || n11 * n22 < n12 * n12 - 1e-12)
    stop("crkbsg: the nugget matrix is not positive semidefinite")

  targets <- .crkbsg_rows(s_predict)
  if (ncol(targets) != d)
    stop(sprintf("crkbsg: targets must have dimension %d", d))

  cov2 <- function(a, b_, bij, nij) {
    h <- .crkbsg_dist(a, b_)
    bij * .crkbsg_rho(h, model, rng) + (if (h <= .crkbsg_EPS) nij else 0.0)
  }

  m <- n1 + n2 + 2L
  A <- matrix(0.0, m, m)
  for (i in seq_len(n1)) {
    for (j in seq_len(n1)) A[i, j] <- cov2(C1[i, ], C1[j, ], b11, n11)
    for (j in seq_len(n2)) A[i, n1 + j] <- cov2(C1[i, ], C2[j, ], b12, n12)
    A[i, n1 + n2 + 1L] <- 1.0
  }
  for (i in seq_len(n2)) {
    for (j in seq_len(n1)) A[n1 + i, j] <- cov2(C2[i, ], C1[j, ], b12, n12)
    for (j in seq_len(n2))
      A[n1 + i, n1 + j] <- cov2(C2[i, ], C2[j, ], b22, n22)
    A[n1 + i, n1 + n2 + 2L] <- 1.0
  }
  for (j in seq_len(n1)) A[n1 + n2 + 1L, j] <- 1.0
  for (j in seq_len(n2)) A[n1 + n2 + 2L, n1 + j] <- 1.0

  # primary-only ordinary kriging, for the comparison the run is for
  mk <- n1 + 1L
  Ak <- matrix(0.0, mk, mk)
  Ak[seq_len(n1), seq_len(n1)] <- A[seq_len(n1), seq_len(n1)]
  Ak[seq_len(n1), n1 + 1L] <- 1.0
  Ak[n1 + 1L, seq_len(n1)] <- 1.0

  c11_0 <- b11 + n11
  nt <- nrow(targets)
  pred <- numeric(nt); vv <- numeric(nt)
  kpred <- numeric(nt); kvar <- numeric(nt)
  lam <- NULL; mu <- NULL; lagr <- c(0.0, 0.0)
  for (ti in seq_len(nt)) {
    t0 <- targets[ti, ]
    rhs <- c(vapply(seq_len(n1), function(i) cov2(C1[i, ], t0, b11, n11), 0),
             vapply(seq_len(n2), function(i) cov2(C2[i, ], t0, b12, n12), 0),
             1.0, 0.0)
    sol <- .crkbsg_solve(A, rhs)
    lam <- sol[seq_len(n1)]
    mu <- sol[n1 + seq_len(n2)]
    lagr <- c(sol[n1 + n2 + 1L], sol[n1 + n2 + 2L])
    pred[ti] <- sum(lam * yv) + sum(mu * zv)
    v <- c11_0 - (sum(lam * rhs[seq_len(n1)]) +
                    sum(mu * rhs[n1 + seq_len(n2)]) + lagr[1])
    vv[ti] <- max(v, 0.0)

    rk <- c(vapply(seq_len(n1), function(i) cov2(C1[i, ], t0, b11, n11), 0),
            1.0)
    sk <- .crkbsg_solve(Ak, rk)
    wk <- sk[seq_len(n1)]
    kpred[ti] <- sum(wk * yv)
    vk <- c11_0 - (sum(wk * rk[seq_len(n1)]) + sk[n1 + 1L])
    kvar[ti] <- max(vk, 0.0)
  }

  list(estimate = pred, prediction = pred,
       variance = vv, std_error = sqrt(vv),
       kriging_prediction = kpred, kriging_variance = kvar,
       variance_reduction = kvar - vv,
       weights_primary = lam, weights_secondary = mu, lagrange = lagr,
       targets = targets,
       coregionalisation = matrix(c(b11, b12, b12, b22), 2L, 2L, byrow = TRUE),
       nugget_matrix = matrix(c(n11, n12, n12, n22), 2L, 2L, byrow = TRUE),
       model = model, range = rng,
       n_primary = as.integer(n1), n_secondary = as.integer(n2),
       method = paste0("ordinary cokriging under a linear model of ",
                       "coregionalisation, with the two-constraint ",
                       "unbiasedness system (Wackernagel 2003 Ch. 24-25; ",
                       "Goovaerts 1997 Sec. 6.2)"),
       note = paste0("the cokriging variance can never exceed the ",
                     "primary-only kriging variance; variance_reduction is ",
                     "what the secondary variable bought, and it is exactly ",
                     "zero when b12 = 0"))
}

.crkbsg_cheatsheet <- function() {
  paste0("crkbsg: morie_crkbsg_cokriging(coords, y, z, s_predict, ",
         "cross_variogram) -> ordinary cokriging prediction and variance ",
         "under a linear model of coregionalisation (Wackernagel 2003)")
}

morie_crkbsg <- morie_crkbsg_cokriging
