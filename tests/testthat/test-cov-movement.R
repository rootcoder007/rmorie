# Coverage for the movement-ecology shelf: Brownian, correlated and
# lattice random walks, the CRW state-space Kalman fit, the gamma/von
# Mises movement HMM and movement networks; recomputed from the Philox
# streams and a generic Kalman recursion in the test body.

test_that("BrownianMotion and LatticeRandomWalk cumulate the Philox draws", {
  b <- BrownianMotion(5, 0.1, sigma = 2, nwalk = 2, seed = 3)
  for (w in 0:1) {
    z <- matrix(.morie_random_normal(10, seed = 3, stream = w), 5, 2, byrow = TRUE)
    expect_equal(b$paths[[w + 1]], rbind(c(0, 0), apply(2 * sqrt(0.1) * z, 2, cumsum)), tolerance = 1e-12)
  }
  expect_equal(b$msd, (rowSums(b$paths[[1]]^2) + rowSums(b$paths[[2]]^2)) / 2, tolerance = 1e-12)
  l <- LatticeRandomWalk(6, nwalk = 1, seed = 2)
  u <- .morie_random_uniform(6, seed = 2, stream = 0)
  mv <- rbind(c(1, 0), c(0, 1), c(-1, 0), c(0, -1))[pmin(floor(u * 4), 3) + 1, ]
  expect_equal(l$paths[[1]], rbind(c(0, 0), apply(mv, 2, cumsum)))
  expect_true(all(rowSums(abs(diff(l$paths[[1]]))) == 1))
})

test_that("CorrelatedRandomWalk takes unit steps and reports the Kareiva-Shigesada MSD", {
  r <- CorrelatedRandomWalk(20, step_length = 1.5, kappa = 3, nwalk = 3, seed = 4)
  for (p in r$paths) expect_equal(sqrt(rowSums(diff(p)^2)), rep(1.5, 20), tolerance = 1e-12)
  c1 <- besselI(3, 1) / besselI(3, 0)
  n <- 0:20
  expect_equal(r$theory, n * 2.25 + 2 * 2.25 * c1 / (1 - c1) * (n - (1 - c1^n) / (1 - c1)), tolerance = 1e-9)
  expect_equal(r$msd, rowMeans(vapply(r$paths, function(p) rowSums(p^2), numeric(21))), tolerance = 1e-12)
})

test_that("CrwKalman evaluates the CRW state-space likelihood and smoother", {
  set.seed(5)
  n <- 25
  v <- numeric(n)
  x <- numeric(n)
  for (t in 2:n) {
    v[t] <- 0.7 * v[t - 1] + rnorm(1, 0, 0.3)
    x[t] <- x[t - 1] + v[t]
  }
  xo <- x + rnorm(n, 0, 0.1)
  yo <- cumsum(rnorm(n, 0, 0.3)) + rnorm(n, 0, 0.1)
  kf <- function(y, g, s2, t2) {
    F <- rbind(c(1, g), c(0, g))
    Q <- s2 * matrix(1, 2, 2)
    m <- c(y[1], 0)
    P <- diag(c(t2 + 1e6, s2 / (1 - g^2)))
    ll <- 0
    ms <- mp <- matrix(0, length(y), 2)
    Ps <- Pp <- vector("list", length(y))
    for (t in seq_along(y)) {
      if (t > 1) {
        a <- as.numeric(F %*% m)
        R <- F %*% P %*% t(F) + Q
      } else {
        a <- m
        R <- P
      }
      f <- R[1, 1] + t2
      e <- y[t] - a[1]
      if (t > 1) ll <- ll - 0.5 * (log(2 * pi * f) + e^2 / f)
      m <- a + R[, 1] / f * e
      P <- R - tcrossprod(R[, 1]) / f
      ms[t, ] <- m
      mp[t, ] <- a
      Ps[[t]] <- P
      Pp[[t]] <- R
    }
    sm <- ms
    for (t in (length(y) - 1):1) {
      J <- Ps[[t]] %*% t(F) %*% solve(Pp[[t + 1]])
      sm[t, ] <- ms[t, ] + as.numeric(J %*% (sm[t + 1, ] - mp[t + 1, ]))
    }
    list(ll = ll, smooth = sm[, 1])
  }
  r <- CrwKalman(xo, yo, gamma = 0.6, sigma = 0.3, tau = 0.1)
  kx <- kf(xo, 0.6, 0.09, 0.01)
  ky <- kf(yo, 0.6, 0.09, 0.01)
  expect_equal(r$loglik, kx$ll + ky$ll, tolerance = 1e-9)
  # the diffuse 1e6 prior variance costs about six digits in the smoother gain
  expect_equal(r$x_smooth, kx$smooth, tolerance = 1e-6)
  fit <- CrwKalman(xo, yo, cycles = 5)
  expect_gte(fit$loglik, r$loglik - 1e-9)
  expect_equal(fit$loglik, kf(xo, fit$gamma, fit$sigma^2, fit$tau^2)$ll + kf(yo, fit$gamma, fit$sigma^2, fit$tau^2)$ll,
               tolerance = 1e-9)
})

test_that("MovementHmm's EM never lowers the likelihood and decodes separated regimes", {
  set.seed(6)
  st <- rep(c(1, 2, 1, 2), each = 25)
  step <- ifelse(st == 1, rgamma(100, 2, 1 / 0.2), rgamma(100, 3, 1 / 2))
  ang <- ifelse(st == 1, runif(100, -pi, pi), rnorm(100, 0, 0.2))
  r <- MovementHmm(step, ang, maxit = 60)
  expect_true(all(diff(r$loglik) > -1e-8))
  expect_equal(rowSums(r$transition), c(1, 1), tolerance = 1e-12)
  dec <- r$states + 1
  agree <- max(mean(dec == st), mean(dec == 3 - st))
  expect_gt(agree, 0.9)
  sl <- order(r$shape * r$scale)
  expect_lt((r$shape * r$scale)[sl[1]], (r$shape * r$scale)[sl[2]])
})

test_that("MovementNetwork counts grid-cell transitions", {
  x <- c(0.2, 0.8, 1.5, 1.7, 0.3, 2.4)
  y <- c(0.1, 0.4, 0.2, 1.3, 0.2, 0.1)
  r <- MovementNetwork(x, y, resolution = 1)
  cells <- paste(floor(x), floor(y))
  nodes <- unique(cells)
  id <- match(cells, nodes)
  tr <- cbind(id[-6], id[-1])
  tr <- tr[tr[, 1] != tr[, 2], , drop = FALSE]
  expect_equal(nrow(r$nodes), length(nodes))
  expect_equal(r$visits, tabulate(id, length(nodes)))
  expect_equal(sum(r$edges[, 3]), nrow(tr))
  expect_equal(r$out_degree, vapply(seq_along(nodes), function(k) length(unique(tr[tr[, 1] == k, 2])), 0L))
})
