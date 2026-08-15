# Unscented Kalman filter (Julier & Uhlmann 1997).
#
# Julier, S. J. & Uhlmann, J. K. (1997). A new extension of the
# Kalman filter to nonlinear systems. *Proc. SPIE 3068, Signal
# Processing, Sensor Fusion, and Target Recognition VI*, Eqs.
# 12-14 and the transformation procedure (local copy
# fetched-wave3/julier-uhlmann-1997-ukf.pdf).

.ukfF_chol <- function(a) {
  k <- nrow(a)
  l <- matrix(0, k, k)
  for (i in 1:k) {
    for (j in 1:i) {
      s <- 0
      if (j > 1) {
        for (t in 1:(j - 1)) {
          s <- s + l[i, t] * l[j, t]
        }
      }
      if (i == j) {
        v <- a[i, i] - s
        if (v < -1e-10) {
          stop("covariance not positive semidefinite")
        }
        l[i, j] <- sqrt(max(v, 0))
      } else {
        if (l[j, j] > 0) {
          l[i, j] <- (a[i, j] - s) / l[j, j]
        } else {
          l[i, j] <- 0
        }
      }
    }
  }
  l
}

.ukfF_solve_mat <- function(a, b_cols) {
  # solve A X = B for X (small systems, partial pivoting, Gauss-Jordan)
  k <- nrow(a)
  nb <- ncol(b_cols)
  m <- cbind(a, b_cols)
  for (c_ in 1:k) {
    sub <- abs(m[c_:k, c_])
    piv_idx <- which.max(sub)
    piv <- piv_idx + c_ - 1
    if (abs(m[piv, c_]) < 1e-300) {
      stop("singular innovation covariance")
    }
    if (piv != c_) {
      m[c_, ] <- m[piv, ]
    }
    d <- m[c_, c_]
    m[c_, ] <- m[c_, ] / d
    for (r_ in 1:k) {
      if (r_ != c_ && m[r_, c_] != 0) {
        f_ <- m[r_, c_]
        m[r_, ] <- m[r_, ] - f_ * m[c_, ]
      }
    }
  }
  m[, (k + 1):(k + nb), drop = FALSE]
}

.ukfF_sigma_points <- function(x, P, kappa) {
  n <- length(x)
  scale <- n + kappa
  a <- scale * P
  l <- .ukfF_chol(a)
  pts <- matrix(0, 2 * n + 1, n)
  pts[1, ] <- x
  w <- numeric(2 * n + 1)
  w[1] <- kappa / scale
  for (i in 1:n) {
    pts[i + 1, ] <- x + l[, i]
    pts[n + i + 1, ] <- x - l[, i]
    w[i + 1] <- 1 / (2 * scale)
    w[n + i + 1] <- 1 / (2 * scale)
  }
  list(pts = pts, w = w)
}

.ukfF_ut <- function(pts, w, fun) {
  np <- nrow(pts)
  # size the output by what fun RETURNS, not by the input dimension --
  # a state->measurement map can change dimension
  y1 <- as.numeric(fun(pts[1, ]))
  m_out <- length(y1)
  ys <- matrix(0, np, m_out)
  ys[1, ] <- y1
  if (np > 1L) for (k in 2:np) {
    ys[k, ] <- as.numeric(fun(pts[k, ]))
  }
  mean_y <- as.numeric(crossprod(ys, w))
  diff <- ys - matrix(mean_y, np, m_out, byrow = TRUE)
  cov_y <- crossprod(diff * w, diff)
  list(ys = ys, mean = mean_y, cov = cov_y)
}

morie_ukfF <- function(f, h, Q, R, x0, P0, measurements, kappa = NULL) {
  x <- as.numeric(x0)
  n <- length(x)
  P <- matrix(as.numeric(P0), n, n)
  Qm <- matrix(as.numeric(Q), n, n)
  if (is.matrix(R)) {
    Rm <- R
  } else {
    Rm <- as.matrix(R)
  }
  storage.mode(Rm) <- "double"
  if (is.null(kappa)) {
    kappa <- 3 - n
    if (n + kappa <= 0) {
      kappa <- 1e-6 - n + 1
    }
  }
  kappa <- as.numeric(kappa)
  if (n + kappa <= 0) {
    stop("need n + kappa > 0")
  }
  states <- list()
  covs <- list()
  innovs <- list()
  for (z in measurements) {
    z <- as.numeric(z)
    # predict
    sp <- .ukfF_sigma_points(x, P, kappa)
    ut_p <- .ukfF_ut(sp$pts, sp$w, f)
    xp <- ut_p$mean
    Pp <- ut_p$cov + Qm
    # update: fresh sigma points from the predicted distribution
    sp2 <- .ukfF_sigma_points(xp, Pp, kappa)
    ut_m <- .ukfF_ut(sp2$pts, sp2$w, h)
    ys <- ut_m$ys
    zp <- ut_m$mean
    Pzz <- ut_m$cov
    m <- length(zp)
    Pzz <- Pzz + Rm
    # Pxz[a, b] = sum_i w[i] * (p[i, a] - xp[a]) * (y[i, b] - zp[b])
    diff_p <- sp2$pts - matrix(xp, nrow(sp2$pts), n, byrow = TRUE)
    diff_y <- ys - matrix(zp, nrow(ys), m, byrow = TRUE)
    Pxz <- matrix(0, n, m)
    for (a in 1:n) {
      for (b in 1:m) {
        Pxz[a, b] <- sum(sp2$w * diff_p[, a] * diff_y[, b])
      }
    }
    # K = Pxz Pzz^{-1}  <=>  solve Pzz K' = Pxz'
    Pxz_t <- t(Pxz)
    kt <- .ukfF_solve_mat(Pzz, Pxz_t)
    K <- t(kt)
    innov <- z - zp
    x <- xp + as.numeric(K %*% innov)
    # P = Pp - K Pzz K'
    P <- Pp - K %*% Pzz %*% t(K)
    # symmetrize (guard n=1 to avoid 1:0 = c(1,0))
    for (a in seq_len(n - 1)) {
      for (b in (a + 1):n) {
        v <- 0.5 * (P[a, b] + P[b, a])
        P[a, b] <- v
        P[b, a] <- v
      }
    }
    states[[length(states) + 1]] <- x
    covs[[length(covs) + 1]] <- P
    innovs[[length(innovs) + 1]] <- innov
  }
  list(
    states = states,
    covariances = covs,
    innovations = innovs,
    kappa = kappa,
    method = "unscented Kalman filter (Julier & Uhlmann 1997)"
  )
}

# long descriptive alias
morie_unscented_kalman_filter <- morie_ukfF
morie_unscented_kalman <- morie_ukfF

morie_ukfF_cheatsheet <- function() {
  "ukfF: 2n+1 sigma points, UT predict + Kalman gain update"
}
