# Anchors for the unscented Kalman filter (Julier & Uhlmann 1997).
#
# The unscented transform is defined by the property that its sigma set
# reproduces the mean and covariance it was built from, and that it is
# exact for a linear map. Together those force the filter to reduce to the
# ordinary Kalman filter on a linear Gaussian system, which is the
# strongest check available here and the one this file gets right to
# machine precision. It sat at 13.4% coverage with no test naming any of
# its functions.

Pmat <- function() matrix(c(4, 1, 0.5, 1, 3, 0.8, 0.5, 0.8, 2), 3, 3)

test_that("the Cholesky factor is lower triangular and reproduces P", {
  P <- Pmat()
  L <- .ukfF_chol(P)
  expect_equal(L %*% t(L), P, ignore_attr = TRUE)
  # lower triangular, so the strict upper part is zero
  expect_equal(L[upper.tri(L)], rep(0, sum(upper.tri(L))), tolerance = 1e-12)
  # the diagonal of a positive definite factor is positive
  expect_true(all(diag(L) > 0))
  # a diagonal matrix factors to the square roots
  expect_equal(diag(.ukfF_chol(diag(c(4, 9)))), c(2, 3), tolerance = 1e-12)
})

test_that("the linear solve satisfies its own equation", {
  set.seed(1)
  A <- crossprod(matrix(rnorm(9), 3, 3)) + diag(3)
  B <- matrix(rnorm(6), 3, 2)
  X <- .ukfF_solve_mat(A, B)
  expect_equal(A %*% X, B, ignore_attr = TRUE, tolerance = 1e-10)
})

sigma_parts <- function(sp) {
  nm <- names(sp)
  pts <- sp[[grep("pts|points", nm)[1]]]
  w <- as.numeric(unlist(sp[[grep("^w", nm)[1]]]))
  M <- if (is.matrix(pts)) pts else do.call(rbind, lapply(pts, as.numeric))
  list(M = M, w = w)
}

test_that("the sigma set reproduces the mean and covariance it came from", {
  # Julier & Uhlmann's defining property, equations 12 to 14
  P <- Pmat()
  x <- c(1, -2, 0.5)
  for (kappa in c(0, 1, 2, 5)) {
    s <- sigma_parts(.ukfF_sigma_points(x, P, kappa))
    # 2n + 1 points for an n-dimensional state
    expect_identical(nrow(s$M), 7L)
    expect_length(s$w, 7L)
    expect_equal(sum(s$w), 1, tolerance = 1e-12)
    mu <- as.numeric(colSums(s$M * s$w))
    expect_equal(mu, x, tolerance = 1e-10)
    C <- matrix(0, 3, 3)
    for (i in seq_len(nrow(s$M))) {
      d <- s$M[i, ] - mu
      C <- C + s$w[i] * (d %o% d)
    }
    expect_equal(C, P, ignore_attr = TRUE, tolerance = 1e-9)
  }
  # the first point is the mean itself
  s0 <- sigma_parts(.ukfF_sigma_points(x, P, 0))
  expect_equal(s0$M[1, ], x, tolerance = 1e-12)
})

test_that("the unscented transform is exact for a linear map", {
  # this is what forces the filter to match the Kalman filter below
  P <- Pmat()
  x <- c(1, -2, 0.5)
  A <- matrix(c(1, 0.5, 0, 0, 1, 0.25, 0.1, 0, 1), 3, 3, byrow = TRUE)
  sp <- .ukfF_sigma_points(x, P, 0)
  ut <- .ukfF_ut(sp[[grep("pts|points", names(sp))[1]]],
                 sp[[grep("^w", names(sp))[1]]],
                 function(v) as.numeric(A %*% v))
  expect_equal(as.numeric(ut$mean), as.numeric(A %*% x), tolerance = 1e-9)
  expect_equal(matrix(unlist(ut$cov), 3, 3), A %*% P %*% t(A),
               ignore_attr = TRUE, tolerance = 1e-8)
  # the identity map leaves both alone
  id <- .ukfF_ut(sp[[grep("pts|points", names(sp))[1]]],
                 sp[[grep("^w", names(sp))[1]]], function(v) v)
  expect_equal(as.numeric(id$mean), x, tolerance = 1e-10)
  expect_equal(matrix(unlist(id$cov), 3, 3), P, ignore_attr = TRUE,
               tolerance = 1e-9)
})

linear_problem <- function(K = 12, seed = 1) {
  set.seed(seed)
  A <- matrix(c(1, 0.5, 0, 1), 2, 2, byrow = TRUE)
  H <- matrix(c(1, 0), 1, 2)
  Q <- diag(c(0.05, 0.02)); R <- matrix(0.3, 1, 1)
  x0 <- c(0, 1); P0 <- diag(c(1, 1))
  xt <- x0; z <- numeric(K)
  for (k in seq_len(K)) {
    xt <- as.numeric(A %*% xt) + c(rnorm(1, 0, sqrt(0.05)),
                                   rnorm(1, 0, sqrt(0.02)))
    z[k] <- as.numeric(H %*% xt) + rnorm(1, 0, sqrt(0.3))
  }
  list(A = A, H = H, Q = Q, R = R, x0 = x0, P0 = P0, z = z, K = K)
}

kalman <- function(p) {
  x <- p$x0; P <- p$P0
  out <- matrix(0, p$K, 2); Ps <- vector("list", p$K)
  for (k in seq_len(p$K)) {
    xp <- as.numeric(p$A %*% x)
    Pp <- p$A %*% P %*% t(p$A) + p$Q
    S <- p$H %*% Pp %*% t(p$H) + p$R
    Kg <- Pp %*% t(p$H) %*% solve(S)
    x <- xp + as.numeric(Kg %*% (p$z[k] - as.numeric(p$H %*% xp)))
    P <- (diag(2) - Kg %*% p$H) %*% Pp
    out[k, ] <- x; Ps[[k]] <- P
  }
  list(x = out, P = Ps)
}

test_that("on a linear system the filter is the Kalman filter", {
  p <- linear_problem()
  ref <- kalman(p)
  u <- morie_ukfF(function(v) as.numeric(p$A %*% v),
                  function(v) as.numeric(p$H %*% v),
                  p$Q, p$R, p$x0, p$P0, as.list(p$z))
  M <- do.call(rbind, lapply(u$states, as.numeric))
  expect_identical(dim(M), c(as.integer(p$K), 2L))
  # to machine precision, not merely closely
  expect_lt(max(abs(M - ref$x)), 1e-12)
  # and so are the covariances
  for (k in seq_len(p$K)) {
    Ck <- matrix(unlist(u$covariances[[k]]), 2, 2)
    expect_equal(Ck, ref$P[[k]], ignore_attr = TRUE, tolerance = 1e-10)
  }
  expect_length(u$innovations, p$K)
  expect_match(u$method, "unscented|Julier")
})

test_that("every reported covariance is symmetric and positive definite", {
  p <- linear_problem()
  u <- morie_ukfF(function(v) as.numeric(p$A %*% v),
                  function(v) as.numeric(p$H %*% v),
                  p$Q, p$R, p$x0, p$P0, as.list(p$z))
  for (Ck in u$covariances) {
    C <- matrix(unlist(Ck), 2, 2)
    expect_equal(C, t(C), tolerance = 1e-10)
    expect_gt(min(eigen(C, only.values = TRUE)$values), 0)
  }
})

test_that("more informative measurements shrink the covariance", {
  p <- linear_problem()
  sharp <- morie_ukfF(function(v) as.numeric(p$A %*% v),
                      function(v) as.numeric(p$H %*% v),
                      p$Q, matrix(0.01, 1, 1), p$x0, p$P0, as.list(p$z))
  vague <- morie_ukfF(function(v) as.numeric(p$A %*% v),
                      function(v) as.numeric(p$H %*% v),
                      p$Q, matrix(10, 1, 1), p$x0, p$P0, as.list(p$z))
  tr <- function(u) sum(diag(matrix(unlist(u$covariances[[p$K]]), 2, 2)))
  expect_lt(tr(sharp), tr(vague))
})

test_that("a nonlinear system is tracked", {
  # the case the unscented transform exists for
  set.seed(3)
  K <- 40
  f <- function(v) c(v[1] + 0.1 * v[2], 0.99 * v[2])
  h <- function(v) sqrt(v[1]^2 + 1)
  Q <- diag(c(1e-4, 1e-4)); R <- matrix(0.01, 1, 1)
  xt <- c(1, 1); z <- numeric(K)
  for (k in seq_len(K)) {
    xt <- f(xt)
    z[k] <- h(xt) + rnorm(1, 0, 0.1)
  }
  u <- morie_ukfF(f, h, Q, R, c(1, 1), diag(c(0.1, 0.1)), as.list(z))
  M <- do.call(rbind, lapply(u$states, as.numeric))
  expect_true(all(is.finite(M)))
  # the filtered first component stays near the trajectory it is tracking
  expect_lt(abs(M[K, 1] - xt[1]), 1)
})
