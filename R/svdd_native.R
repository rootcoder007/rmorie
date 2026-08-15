# Support Vector Data Description: the smallest sphere containing the data.
# Tax, D. M. J., & Duin, R. P. W. (2004) "Support Vector Data
# Description", Machine Learning 54(1), 45-66.

.morie_svdd_mat <- function(X, name) {
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  if (nrow(X) == 0 || ncol(X) == 0) {
    stop(sprintf("svdd: %s must be a non-empty (n, p) matrix", name))
  }
  X
}

.morie_svdd_kernel_matrix <- function(X, Y = NULL, kernel = "rbf",
                                       gamma = NULL, degree = 3,
                                       coef0 = 1.0) {
  if (is.null(Y)) {
    Y <- X
  }
  p <- ncol(X)
  if (is.null(gamma)) {
    gamma <- 1.0 / p
  }
  n <- nrow(X)
  m <- nrow(Y)
  K <- matrix(0, nrow = n, ncol = m)

  if (kernel == "linear") {
    for (i in seq_len(n)) {
      for (j in seq_len(m)) {
        K[i, j] <- sum(X[i, ] * Y[j, ])
      }
    }
  } else if (kernel == "rbf") {
    for (i in seq_len(n)) {
      for (j in seq_len(m)) {
        diff <- X[i, ] - Y[j, ]
        K[i, j] <- exp(-gamma * sum(diff * diff))
      }
    }
  } else if (kernel == "poly") {
    for (i in seq_len(n)) {
      for (j in seq_len(m)) {
        K[i, j] <- (gamma * sum(X[i, ] * Y[j, ]) + coef0)^degree
      }
    }
  } else {
    stop(sprintf("svdd: kernel must be rbf, linear or poly, got %s", kernel))
  }

  K
}

.morie_svdd_solve_dual <- function(K, C, n, tol, max_iter) {
  alpha <- rep(1.0 / n, n)
  if (C < 1.0 / n) {
    stop("svdd: infeasible C")
  }

  Ka <- rep(0, n)
  for (i in seq_len(n)) {
    s <- 0
    for (j in seq_len(n)) {
      s <- s + alpha[j] * K[i, j]
    }
    Ka[i] <- s
  }

  for (iter in seq_len(as.integer(max_iter))) {
    g <- rep(0, n)
    for (i in seq_len(n)) {
      g[i] <- K[i, i] - 2.0 * Ka[i]
    }

    up <- NA_integer_
    dn <- NA_integer_
    for (i in seq_len(n)) {
      if (alpha[i] < C - 1e-15 && (is.na(up) || g[i] > g[up])) {
        up <- i
      }
      if (alpha[i] > 1e-15 && (is.na(dn) || g[i] < g[dn])) {
        dn <- i
      }
    }
    if (is.na(up) || is.na(dn) || g[up] - g[dn] <= tol) {
      break
    }

    i <- up
    j <- dn
    denom <- 2.0 * (K[i, i] - 2.0 * K[i, j] + K[j, j])
    if (denom <= 1e-15) {
      d <- if (g[i] > g[j]) alpha[j] else 0.0
    } else {
      d <- (g[i] - g[j]) / denom
    }
    d <- min(d, C - alpha[i], alpha[j])
    if (d <= 1e-15) {
      break
    }
    alpha[i] <- alpha[i] + d
    alpha[j] <- alpha[j] - d
    for (t in seq_len(n)) {
      Ka[t] <- Ka[t] + d * (K[t, i] - K[t, j])
    }
  }

  for (i in seq_len(n)) {
    if (alpha[i] < 1e-12) {
      alpha[i] <- 0.0
    } else if (alpha[i] > C - 1e-12) {
      alpha[i] <- C
    }
  }
  s <- sum(alpha)
  if (s > 0) {
    alpha <- alpha / s
  }
  alpha
}

morie_svdd <- function(X, C = NULL, nu = NULL, kernel = "rbf",
                        gamma = NULL, degree = 3, coef0 = 1.0,
                        tol = 1e-10, max_iter = 20000) {
  rows <- .morie_svdd_mat(X, "X")
  n <- nrow(rows)
  p <- ncol(rows)

  if (!is.null(C) && !is.null(nu)) {
    stop("svdd: pass C or nu, not both")
  }
  if (!is.null(nu)) {
    nu <- as.numeric(nu)
    if (!(nu > 0.0 && nu <= 1.0)) {
      stop(sprintf("svdd: nu must lie in (0, 1], got %g", nu))
    }
    C <- 1.0 / (nu * n)
  }
  if (is.null(C)) {
    C <- 1.0
  }
  C <- as.numeric(C)
  if (C <= 0.0) {
    stop(sprintf("svdd: C must be > 0, got %g", C))
  }
  if (C * n < 1.0) {
    stop(sprintf("svdd: C = %g with n = %d makes sum(alpha) = 1 infeasible under alpha_i <= C; need C >= 1/n", C, n))
  }
  if (!(kernel %in% c("rbf", "linear", "poly"))) {
    stop(sprintf("svdd: kernel must be rbf, linear or poly, got %s", kernel))
  }
  if (is.null(gamma)) {
    gamma <- 1.0 / p
  }

  K <- .morie_svdd_kernel_matrix(rows, kernel = kernel, gamma = gamma,
                                  degree = degree, coef0 = coef0)

  alpha <- .morie_svdd_solve_dual(K, C, n, tol, max_iter)

  aKa <- 0.0
  for (i in seq_len(n)) {
    if (alpha[i] == 0.0) next
    ai <- alpha[i]
    for (j in seq_len(n)) {
      if (alpha[j] != 0.0) {
        aKa <- aKa + ai * alpha[j] * K[i, j]
      }
    }
  }

  dist2_row <- function(krow, kxx) {
    s <- 0.0
    for (i in seq_len(n)) {
      if (alpha[i] != 0.0) {
        s <- s + alpha[i] * krow[i]
      }
    }
    kxx - 2.0 * s + aKa
  }

  d2 <- rep(0, n)
  for (i in seq_len(n)) {
    d2[i] <- dist2_row(K[i, ], K[i, i])
  }

  eps <- 1e-8
  support <- which(alpha > eps)
  bounded <- which(alpha >= C - eps)
  boundary <- support[!(support %in% bounded)]

  degenerate <- length(boundary) == 0 && length(support) > 0
  if (length(boundary) > 0) {
    R2 <- mean(d2[boundary])
  } else if (length(support) > 0) {
    R2 <- max(d2[support])
  } else {
    R2 <- 0.0
  }
  R2 <- max(0.0, R2)

  center <- NULL
  if (kernel == "linear") {
    center <- rep(0.0, p)
    for (i in seq_len(n)) {
      if (alpha[i] == 0.0) next
      for (t in seq_len(p)) {
        center[t] <- center[t] + alpha[i] * rows[i, t]
      }
    }
  }

  decision <- function(Z) {
    zr <- .morie_svdd_mat(Z, "Z")
    if (ncol(zr) != p) {
      stop(sprintf("svdd: test data has %d columns, training had %d", ncol(zr), p))
    }
    Kz <- .morie_svdd_kernel_matrix(zr, rows, kernel = kernel, gamma = gamma,
                                     degree = degree, coef0 = coef0)
    Kzz <- .morie_svdd_kernel_matrix(zr, kernel = kernel, gamma = gamma,
                                     degree = degree, coef0 = coef0)
    result <- rep(0, nrow(zr))
    for (t in seq_len(nrow(zr))) {
      result[t] <- dist2_row(Kz[t, ], Kzz[t, t]) - R2
    }
    result
  }

  predict <- function(Z) {
    v <- decision(Z)
    v <= 0.0
  }

  n_out <- sum(d2 > R2 + 1e-8)

  list(
    estimate = alpha,
    alpha = alpha,
    R2 = as.numeric(R2),
    radius = as.numeric(sqrt(R2)),
    center = center,
    support_ = support,
    boundary_ = boundary,
    bounded_ = bounded,
    n_support = length(support),
    degenerate = as.logical(degenerate),
    distance2 = d2,
    outlier_fraction = as.numeric(n_out) / n,
    outlier_bound = min(1.0, 1.0 / (C * n)),
    decision = decision,
    predict = predict,
    C = C,
    kernel = kernel,
    gamma = gamma,
    n = n,
    method = "SVDD (Tax & Duin 2004)"
  )
}

.morie_svdd_cheatsheet <- function() {
  paste0("svdd: smallest enclosing sphere, min R^2 + C sum xi ",
         "(Tax & Duin 2004 eqs. 3-4). Dual: max sum a_i K_ii - ",
         "sum a_i a_j K_ij, sum a = 1, 0 <= a_i <= C (eqs. 9-10). ",
         "KKT eqs. 11-13 label interior / boundary / bounded; R^2 ",
         "comes from an UNBOUNDED support vector (eq. 15). At most ",
         "1/(CN) of the data can be rejected, so C = 1/(nu N) ",
         "makes nu the outlier fraction. C >= 1 gives the exact MEB.")
}

# compact alias per ledger/NAMING.md
morie_support_vector_data_description <- morie_svdd
