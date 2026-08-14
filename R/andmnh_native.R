# andmnh -- VAR prewhitened kernel HAC covariance matrix estimation
# Andrews, D. W. K., & Monahan, J. C. (1992) "An Improved
# Heteroskedasticity and Autocorrelation Consistent Covariance Matrix
# Estimator", Econometrica 60(4), 953-966.
# Base R only.

# --------------------------------------------------------------------------
# Kernels
# --------------------------------------------------------------------------

bartlett_kernel <- function(x) {
  ax <- abs(as.numeric(x))
  if (ax <= 1) 1 - ax else 0
}

parzen_kernel <- function(x) {
  ax <- abs(as.numeric(x))
  if (ax <= 0.5) {
    return(1 - 6 * ax^2 + 6 * ax^3)
  } else if (ax <= 1) {
    return(2 * (1 - ax)^3)
  }
  0
}

quadratic_spectral_kernel <- function(x) {
  x <- as.numeric(x)
  if (x == 0) return(1)
  z <- 6 * pi * x / 5
  (25 / (12 * pi^2 * x^2)) * (sin(z)/z - cos(z))
}

tukey_hanning_kernel <- function(x) {
  ax <- abs(as.numeric(x))
  if (ax <= 1) 0.5 * (1 + cos(pi * ax)) else 0
}

# name -> (q, k_q, integral of k^2, has bounded support)
.KERNEL_CONSTANTS <- list(
  bartlett      = c(1, 1, 2/3, TRUE),
  parzen        = c(2, 6, 0.539285, TRUE),
  qs            = c(2, 1.421223, 1, FALSE),
  `tukey-hanning` = c(2, pi^2/4, 0.75, TRUE)
)

.KERNELS <- list(
  bartlett      = bartlett_kernel,
  parzen        = parzen_kernel,
  qs            = quadratic_spectral_kernel,
  `tukey-hanning` = tukey_hanning_kernel
)

.check_kernel <- function(kernel) {
  if (!(kernel %in% names(.KERNELS))) {
    stop(sprintf("andmnh: kernel must be one of %s, got %s",
                 paste(names(.KERNELS), collapse=", "), kernel))
  }
  list(fun = .KERNELS[[kernel]], const = .KERNEL_CONSTANTS[[kernel]],
       name = kernel)
}

# --------------------------------------------------------------------------
# moment vectors
# --------------------------------------------------------------------------

moment_vectors <- function(e, X) {
  e <- as.numeric(e)
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  if (nrow(X) != length(e)) {
    stop(sprintf("andmnh: %d residuals but %d regressor rows",
                 length(e), nrow(X)))
  }
  if (nrow(X) == 0) stop("andmnh: no observations")
  V <- X * e
  storage.mode(V) <- "double"
  V
}

# --------------------------------------------------------------------------
# matrix utilities (base R)
# --------------------------------------------------------------------------

.svd_r <- function(a) {
  # returns U, s, V (V transposed, like np.linalg.svd with full matrices)
  a <- as.matrix(a)
  storage.mode(a) <- "double"
  s <- svd(a)
  list(u = s$u, s = s$d, v = s$v)
}

.singular_value_adjust <- function(a, cap = 0.97) {
  cap <- as.numeric(cap)
  if (!(cap > 0 && cap < 1)) {
    stop("andmnh: cap must lie strictly between 0 and 1")
  }
  sv <- .svd_r(a)
  s2 <- pmin(pmax(sv$s, 0), cap)   # singular values are >= 0
  # build diag
  p <- length(s2)
  Smat <- matrix(0, p, p)
  diag(Smat) <- s2
  sv$u %*% Smat %*% t(sv$v)
}

# solve a linear system
.solve_safe <- function(A, b) {
  A <- as.matrix(A)
  storage.mode(A) <- "double"
  b <- as.matrix(b)
  storage.mode(b) <- "double"
  solve(A, b)
}

# --------------------------------------------------------------------------
# prewhitening VAR
# --------------------------------------------------------------------------

prewhiten_var <- function(v, order = 1, cap = 0.97, adjust = TRUE) {
  rows <- as.matrix(v)
  storage.mode(rows) <- "double"
  n <- nrow(rows)
  if (n == 0) stop("andmnh: no observations")
  p <- ncol(rows)
  order <- as.integer(order)
  if (order < 0) stop("andmnh: VAR order must be non-negative")
  if (order == 0) {
    return(list(A = list(), residuals = rows, D = diag(p)))
  }
  if (n <= order * p + 1) {
    stop(sprintf("andmnh: %d observations cannot fit a VAR(%d) in %d variables",
                 n, order, p))
  }

  # Y: rows t = order,...,n-1; Z: rows t = order,...,n-1, columns
  # v_{t-1}, ..., v_{t-order} stacked.
  Y <- rows[(order + 1):n, , drop = FALSE]
  Z <- matrix(0, nrow = n - order, ncol = order * p)
  for (t_idx in (order + 1):n) {
    cols <- c()
    for (r in 1:order) {
      cols <- c(cols, rows[t_idx - r, ])
    }
    Z[t_idx - order, ] <- cols
  }
  storage.mode(Z) <- "double"
  storage.mode(Y) <- "double"

  coef <- .solve_safe(crossprod(Z), crossprod(Z, Y))   # (order*p) x p
  rownames(coef) <- NULL
  colnames(coef) <- NULL

  a_list <- vector("list", order)
  for (r in 1:order) {
    block <- matrix(coef[((r - 1) * p + 1):(r * p), , drop = FALSE],
                    nrow = p, ncol = p)
    # coef currently has rows indexed by [var][lag]; we want A[i,j] such
    # that pred_i += A[i,j] * v_{t-r-1}[j].  Build a_list[[r]] with the
    # entry [i, j] = coef[r*p_block + j, i] of the LS coefficient matrix.
    blk <- matrix(0, p, p)
    for (i in 1:p) {
      for (j in 1:p) {
        blk[i, j] <- coef[(r - 1) * p + j, i]
      }
    }
    a_list[[r]] <- blk
  }

  if (adjust) {
    a_list <- lapply(a_list, .singular_value_adjust, cap = cap)
    if (order > 1) {
      for (iter in 1:200) {
        tot <- a_list[[1]]
        if (length(a_list) > 1) {
          for (k in 2:length(a_list)) tot <- tot + a_list[[k]]
        }
        smax <- max(svd(tot)$d)
        if (smax <= cap) break
        a_list <- lapply(a_list, function(A) A * (cap / smax))
      }
    }
  }

  resid <- matrix(0, nrow = n - order, ncol = p)
  for (idx in 1:(n - order)) {
    t <- idx + order
    pred <- rep(0, p)
    for (r in 1:order) {
      ar <- a_list[[r]]
      for (i in 1:p) {
        for (j in 1:p) {
          pred[i] <- pred[i] + ar[i, j] * rows[t - r, j]
        }
      }
    }
    for (i in 1:p) {
      resid[idx, i] <- rows[t, i] - pred[i]
    }
  }
  storage.mode(resid) <- "double"

  tot <- a_list[[1]]
  if (length(a_list) > 1) {
    for (k in 2:length(a_list)) tot <- tot + a_list[[k]]
  }
  D <- solve(diag(p) - tot)
  list(A = a_list, residuals = resid, D = D)
}

# --------------------------------------------------------------------------
# AR(1) and the alpha(q) plug-in
# --------------------------------------------------------------------------

ar1_fit <- function(x) {
  x <- as.numeric(x)
  n <- length(x)
  if (n < 3) stop("andmnh: an AR(1) needs at least 3 observations")
  num <- sum(x[2:n] * x[1:(n - 1)])
  den <- sum(x[1:(n - 1)]^2)
  rho <- if (den > 0) num/den else 0
  s2 <- sum((x[2:n] - rho * x[1:(n - 1)])^2) / (n - 1)
  c(rho = rho, sigma2 = s2)
}

alpha_ar1 <- function(v, q = 2, weights = NULL) {
  rows <- as.matrix(v)
  storage.mode(rows) <- "double"
  if (nrow(rows) == 0) stop("andmnh: no observations")
  p <- ncol(rows)
  if (is.null(weights)) {
    w <- rep(1, p)
  } else if (is.character(weights) && length(weights) == 1 &&
             weights == "drop_first") {
    w <- c(0, rep(1, p - 1))
  } else {
    w <- as.numeric(weights)
    if (length(w) != p) {
      stop(sprintf("andmnh: %d weights for %d series", length(w), p))
    }
  }
  if (any(w < 0) || sum(w) <= 0) {
    stop("andmnh: weights must be non-negative and not all zero")
  }
  q <- as.integer(q)
  if (!(q %in% c(1, 2))) stop("andmnh: alpha(q) is given for q = 1 or 2")

  num <- 0
  den <- 0
  fits <- list()
  for (a in 1:p) {
    fit <- ar1_fit(rows[, a])
    rho <- fit["rho"]; s2 <- fit["sigma2"]
    fits[[a]] <- list(rho = unname(rho), sigma2 = unname(s2))
    if (w[a] == 0) next
    s4 <- s2^2
    if (q == 2) {
      num <- num + w[a] * 4 * rho^2 * s4 / (1 - rho)^8
    } else {
      num <- num + w[a] * 4 * rho^2 * s4 /
                   ((1 - rho)^6 * (1 + rho)^2)
    }
    den <- den + w[a] * s4 / (1 - rho)^4
  }
  if (den <= 0) stop("andmnh: the alpha(q) denominator vanished")
  list(alpha = num/den, fits = fits)
}

automatic_bandwidth <- function(v, kernel = "qs", weights = NULL, n = NULL) {
  ck <- .check_kernel(kernel)
  q <- ck$const[1]; kq <- ck$const[2]; ik2 <- ck$const[3]
  rows <- as.matrix(v)
  storage.mode(rows) <- "double"
  Tn <- if (is.null(n)) nrow(rows) else as.integer(n)
  aout <- alpha_ar1(rows, q = q, weights = weights)
  s <- (q * kq^2 * aout$alpha * Tn / ik2)^(1/(2*q + 1))
  list(bandwidth = s, alpha = aout$alpha, fits = aout$fits)
}

# --------------------------------------------------------------------------
# kernel HAC
# --------------------------------------------------------------------------

kernel_hac <- function(v, bandwidth, kernel = "qs", n_params = 0, n = NULL) {
  ck <- .check_kernel(kernel)
  kfun <- ck$fun
  bounded <- isTRUE(ck$const[4])
  rows <- as.matrix(v)
  storage.mode(rows) <- "double"
  m <- nrow(rows)
  if (m == 0) stop("andmnh: no observations")
  p <- ncol(rows)
  Tn <- if (is.null(n)) m else as.integer(n)
  if (Tn <= n_params) {
    stop(sprintf("andmnh: T = %d is not larger than the %d estimated parameters",
                 Tn, n_params))
  }
  s <- as.numeric(bandwidth)
  if (s <= 0) stop("andmnh: bandwidth must be positive")

  jmax <- m - 1
  if (bounded) jmax <- min(jmax, floor(s))

  out <- matrix(0, p, p)
  for (j in 0:jmax) {
    kj <- kfun(j/s)
    if (kj == 0) next
    gam <- matrix(0, p, p)
    for (tt in (j + 1):m) {
      a <- rows[tt, ]
      b <- rows[tt - j, ]
      # outer product
      for (i in 1:p) {
        ai <- a[i]
        if (ai == 0) next
        for (k_ in 1:p) {
          gam[i, k_] <- gam[i, k_] + ai * b[k_]
        }
      }
    }
    gam <- gam / Tn
    if (j == 0) {
      out <- out + kj * gam
    } else {
      out <- out + kj * (gam + t(gam))
    }
  }
  dof <- Tn / (Tn - n_params)
  dof * out
}

# --------------------------------------------------------------------------
# top-level estimator
# --------------------------------------------------------------------------

andrews_monahan_hac <- function(e, X = NULL, prewhiten = TRUE,
                                var_order = 1, kernel = "qs",
                                bandwidth = NULL, weights = NULL,
                                n_params = NULL, cap = 0.97,
                                adjust = TRUE) {
  if (!is.null(X)) {
    V <- moment_vectors(e, X)
    if (is.null(n_params)) n_params <- ncol(V)
  } else {
    V <- as.matrix(e)
    storage.mode(V) <- "double"
    if (is.null(n_params)) n_params <- 0
  }
  n <- nrow(V)
  if (n == 0) stop("andmnh: no observations")
  p <- ncol(V)
  order <- if (prewhiten) as.integer(var_order) else 0L

  pw <- prewhiten_var(V, order = order, cap = cap, adjust = adjust)

  if (is.null(bandwidth)) {
    ab <- automatic_bandwidth(pw$residuals, kernel = kernel,
                              weights = weights, n = n)
    s <- ab$bandwidth
    alpha <- ab$alpha
    fits <- ab$fits
    auto <- TRUE
  } else {
    s <- as.numeric(bandwidth)
    alpha <- NULL
    fits <- NULL
    auto <- FALSE
  }

  Jstar <- kernel_hac(pw$residuals, bandwidth = s, kernel = kernel,
                      n_params = n_params, n = n)
  J <- pw$D %*% Jstar %*% t(pw$D)

  structure(
    list(
      J = J,
      J_star = Jstar,
      D = pw$D,
      A = pw$A,
      bandwidth = s,
      bandwidth_automatic = auto,
      alpha = alpha,
      ar1_fits = fits,
      kernel = kernel,
      var_order = order,
      n = n,
      p = p,
      n_params = as.integer(n_params),
      prewhitened = as.logical(order > 0),
      method = paste("Andrews & Monahan (1992) VAR prewhitened kernel HAC,",
                     "eq. 2.2-2.4, with the Andrews (1991) eq. 6.1",
                     "automatic bandwidth"),
      note = sprintf(paste("the VAR is a filter, not a model; its",
                           "coefficients are capped through their SVD at",
                           "%.2f so that I - sum(A_r) stays %.2f away from",
                           "singular (footnote 4)"),
                     cap, 1 - cap)
    ),
    class = "andmnh"
  )
}

andmnh <- andrews_monahan_hac

print.andmnh <- function(x, ...) {
  cat(sprintf("Andrews-Monahan VAR prewhitened kernel HAC\n"))
  cat(sprintf("  kernel        : %s\n", x$kernel))
  cat(sprintf("  bandwidth     : %.6f (automatic = %s)\n",
              x$bandwidth, x$bandwidth_automatic))
  cat(sprintf("  var_order     : %d (prewhitened = %s)\n",
              x$var_order, x$prewhitened))
  cat(sprintf("  n / p / l     : %d / %d / %d\n", x$n, x$p, x$n_params))
  if (!is.null(x$alpha))
    cat(sprintf("  alpha(2)      : %.6f\n", x$alpha))
  cat("\nJ (recoloured):\n")
  print(x$J)
  invisible(x)
}

# house entry point: the package exports one morie_<module>
morie_andmnh <- andrews_monahan_hac
