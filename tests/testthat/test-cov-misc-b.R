# Coverage for betbnm.R, bfac.R, bcdblk.R, besagl.R, blupr.R, blups.R,
# bprop.R, brcls.R, brrpf.R, bvtrA.R and bezout.R: conjugate updates,
# BLUPs (checked through the Woodbury form), back-propagated gradients
# (against finite differences of the network loss) and number theory.

test_that("Betabinom updates the Beta prior", {
  r <- Betabinom(7, 20, alpha = 2, beta = 3, m = 10)
  a <- 9
  b <- 16
  expect_equal(c(r$postmean, r$postvar), c(a / 25, a * b / (25^2 * 26)), tolerance = 1e-12)
  expect_equal(r$postmode, 8 / 23, tolerance = 1e-12)
  expect_equal(r$logmarglik, log(choose(20, 7)) + lbeta(9, 16) - lbeta(2, 3), tolerance = 1e-12)
  expect_equal(r$predvar, 10 * (9 / 25) * (16 / 25) * 35 / 26, tolerance = 1e-12)
  expect_true(is.na(Betabinom(0, 1, 0.5, 0.5)$postmode))
  expect_error(Betabinom(5, 3), "0 <= y <= n")
  expect_error(Betabinom(1, 3, alpha = 0), "strictly positive")
  expect_error(Betabinom(1, 3, m = -1), "non-negative")
})

test_that("Bfac is Bayfac on log likelihoods", {
  expect_equal(Bfac(-3, -5)$bf, exp(2), tolerance = 1e-12)
})

test_that("Bcdblk minimises the quadratic by exact block updates", {
  Q <- rbind(c(4, 1, 0.5), c(1, 3, 0.2), c(0.5, 0.2, 2))
  b <- c(1, -1, 2)
  r <- Bcdblk(Q, b, blocks = list(c(0, 1), 2), n_iter = 60)
  expect_equal(r$x, as.numeric(solve(Q, b)), tolerance = 1e-10)
  expect_equal(r$estimate, -0.5 * sum(b * solve(Q, b)), tolerance = 1e-10)
  expect_true(all(diff(r$obj_trace) <= 1e-15))
  x <- c(0, 0, 0)
  x[1:2] <- solve(Q[1:2, 1:2], b[1:2])
  x[3] <- (b[3] - sum(Q[3, 1:2] * x[1:2])) / Q[3, 3]
  expect_equal(Bcdblk(Q, b, list(c(0, 1), 2), n_iter = 1)$x, x, tolerance = 1e-12)
})

test_that("Bymfit evaluates the BYM log-posterior kernel", {
  A <- rbind(c(0, 1, 0, 1), c(1, 0, 1, 0), c(0, 1, 0, 1), c(1, 0, 1, 0))
  y <- c(3, 7, 2, 5)
  E <- c(4, 5, 3, 4.5)
  u <- c(0.1, -0.2, 0.05, 0.05)
  v <- c(0.02, 0.1, -0.1, 0)
  X <- cbind(c(1, 0.5, -0.3, 0.2))
  r <- Bymfit(y, E, A, u, v, taus = 2, tauv = 3, X = X, beta = 0.4)
  mu <- E * exp(0.4 * X[, 1] + u + v)
  q <- (u[1] - u[2])^2 + (u[1] - u[4])^2 + (u[2] - u[3])^2 + (u[3] - u[4])^2
  expect_equal(r$loglik, sum(dpois(y, mu, log = TRUE)), tolerance = 1e-12)
  expect_equal(r$logpu, 1.5 * log(2) - q, tolerance = 1e-12)
  expect_equal(r$logpv, sum(dnorm(v, 0, 1 / sqrt(3), log = TRUE)), tolerance = 1e-12)
  expect_equal(r$npair, 4L)
  expect_error(Bymfit(y, E[-1], A, u, v), "same length")
  expect_error(Bymfit(y, -E, A, u, v), "strictly positive")
  expect_error(Bymfit(-y, E, A, u, v), "non-negative")
  expect_error(Bymfit(y, E, A[-1, ], u, v), "n by n")
  expect_error(Bymfit(y, E, A, u, v, taus = 0), "strictly positive")
  expect_error(Bymfit(y, E, A, u, v, X = X[-1, , drop = FALSE], beta = 1), "one row per area")
  expect_error(Bymfit(y, E, A, u, v, X = X, beta = c(1, 2)), "one entry per column")
})

test_that("Blupint and Blupslope are the random-effect BLUPs", {
  y <- c(2.1, 2.5, 1.9, 3.4, 3.8, 3.1, 3.6, 1.2, 1.5)
  g <- c("a", "a", "a", "b", "b", "b", "b", "c", "c")
  r <- Blupint(y, g, s2u = 0.5, s2e = 0.3)
  nj <- c(3, 4, 2)
  gm <- as.numeric(tapply(y, g, mean))
  k <- 0.5 / (0.5 + 0.3 / nj)
  expect_equal(r$u, k * gm, tolerance = 1e-12)
  expect_equal(r$vpc, 0.5 / 0.8, tolerance = 1e-12)
  xb <- Blupint(y, g, 0.5, 0.3, X = matrix(1, 9, 1), beta = 2)
  expect_equal(xb$groupmean, gm - 2, tolerance = 1e-12)
  expect_error(Blupint(y, g[-1], 1, 1), "one label")
  expect_error(Blupint(y, g, -1, 1), "non-negative")
  expect_error(Blupint(y, g, 1, 0), "strictly positive")
  expect_error(Blupint(y, g, 1, 1, X = matrix(1, 2, 1), beta = 1), "one row")
  expect_error(Blupint(y, g, 1, 1, X = matrix(1, 9, 1), beta = 1:2), "one entry per column")
  Z <- cbind(1, c(0, 1, 2, 0, 1, 2, 3, 0, 1))
  D <- rbind(c(0.5, 0.1), c(0.1, 0.2))
  s <- Blupslope(y, g, Z, D, s2e = 0.3)
  vb <- lapply(c("a", "b", "c"), function(L) {
    Zj <- Z[g == L, ]
    as.numeric(solve(crossprod(Zj) / 0.3 + solve(D), crossprod(Zj, y[g == L]) / 0.3))
  })
  expect_equal(s$v, vb, tolerance = 1e-10)
  expect_error(Blupslope(y, g, Z[-1, ], D, 1), "one row per observation")
  expect_error(Blupslope(y, g, Z, diag(3), 1), "q by q")
  expect_error(Blupslope(y, g, Z, D, 0), "strictly positive")
  expect_error(Blupslope(y, g[-1], Z, D, 1), "one label")
})

test_that("Bprop back-propagates the loss gradient (finite-difference check)", {
  sig <- function(z) 1 / (1 + exp(-z))
  X <- rbind(c(0.5, -0.2), c(0.1, 0.8), c(-0.6, 0.3))
  Tt <- rbind(c(0.2), c(0.9), c(0.4))
  W1 <- rbind(c(0.1, 0.4, -0.3), c(-0.2, 0.2, 0.5))
  W2 <- rbind(c(0.3, -0.6, 0.8))
  fwd <- function(W1, W2) {
    H <- sig(cbind(1, X) %*% t(W1))
    O <- sig(cbind(1, H) %*% t(W2))
    list(H = H, O = O)
  }
  f <- fwd(W1, W2)
  loss <- function(W1, W2) 0.5 * sum((fwd(W1, W2)$O - Tt)^2)
  r <- Bprop(list(W1, W2), list(X, f$H, f$O), f$O - Tt)
  num <- function(which, i, j) {
    h <- 1e-6
    if (which == 1) {
      Wp <- W1
      Wp[i, j] <- Wp[i, j] + h
      Wm <- W1
      Wm[i, j] <- Wm[i, j] - h
      (loss(Wp, W2) - loss(Wm, W2)) / (2 * h)
    } else {
      Wp <- W2
      Wp[i, j] <- Wp[i, j] + h
      Wm <- W2
      Wm[i, j] <- Wm[i, j] - h
      (loss(W1, Wp) - loss(W1, Wm)) / (2 * h)
    }
  }
  g1 <- outer(1:2, 1:3, Vectorize(function(i, j) num(1, i, j)))
  g2 <- outer(1, 1:3, Vectorize(function(i, j) num(2, i, j)))
  # central differences with h = 1e-6 are accurate to about 1e-10
  expect_equal(r$gradients[[1]], g1, tolerance = 1e-8)
  expect_equal(r$gradients[[2]], g2, tolerance = 1e-8)
  expect_equal(r$loss, sum((f$O - Tt)^2) / 6, tolerance = 1e-12)
  lin <- Bprop(list(W2), list(f$H, f$O), f$O - Tt, act_fun = "linear")
  expect_equal(lin$gradients[[1]], t(f$O - Tt) %*% cbind(1, f$H), tolerance = 1e-12)
  expect_error(Bprop(list(), list(X), Tt), "no layers")
  expect_error(Bprop(list(W1), list(X), Tt), "L\\+1")
  expect_error(Bprop(list(W2), list(f$H, f$O[1:2, , drop = FALSE]), Tt), "disagree")
  expect_error(Bprop(list(W2), list(f$H, f$O), Tt[1:2, , drop = FALSE]), "does not match")
  expect_error(Bprop(list(W2), list(f$H, f$O), f$O - Tt, act_fun = c("a", "b")), "one activation")
  expect_error(Bprop(list(W1), list(X, f$O), f$O - Tt), "wrong number of rows")
  expect_error(Bprop(list(W2[, 1:2, drop = FALSE]), list(f$H, f$O), f$O - Tt), "wrong number of columns")
  expect_error(Bprop(list(W2), list(f$H, f$O), f$O - Tt, act_fun = "gelu"), "unknown activation")
})

test_that("Brierscore, Brrhyper, Biasvardec and Bezout", {
  P <- rbind(c(0.7, 0.2, 0.1), c(0.1, 0.6, 0.3), c(0.3, 0.3, 0.4))
  y <- c(1, 3, 3)
  D <- diag(3)[y, ]
  expect_equal(Brierscore(P, y)$brier, sum((P - D)^2) / 3, tolerance = 1e-12)
  expect_error(Brierscore(P[0, ], integer(0)), "at least one row")
  expect_error(Brierscore(P, 1:2), "one entry per row")
  expect_error(Brierscore(P, c(1, 4, 1)), "1-based")
  yy <- c(3, 5, 2, 8, 6, 4)
  h <- Brrhyper(yy, R2 = 0.4, nu = 6, nu_beta = 4)
  expect_equal(h$S, var(yy) * 0.6 * 8, tolerance = 1e-12)
  expect_equal(h$S_beta, var(yy) * 0.4 * 6, tolerance = 1e-12)
  Fm <- rbind(c(1.1, 2.2, 2.9), c(0.9, 1.8, 3.2), c(1.0, 2.1, 3.1))
  f <- c(1, 2, 3)
  bv <- Biasvardec(Fm, f, 0.25)
  m <- colMeans(Fm)
  expect_equal(bv$bias2, mean((m - f)^2), tolerance = 1e-12)
  expect_equal(bv$variance, mean(colMeans(sweep(Fm, 2, m)^2)), tolerance = 1e-12)
  expect_equal(bv$total, 0.25 + bv$bias2 + bv$variance, tolerance = 1e-12)
  expect_error(Biasvardec(Fm[0, ], f, 1), "at least one replicate")
  expect_error(Biasvardec(Fm, f[-1], 1), "one column per entry")
  expect_error(Biasvardec(Fm, f, -1), "non-negative")
  z <- Bezout(240, 46)
  expect_equal(z$gcd, 2)
  expect_equal(240 * z$x + 46 * z$y, 2)
  n <- Bezout(-12, 18)
  expect_equal(n$gcd, 6)
  expect_equal(-12 * n$x + 18 * n$y, 6)
})
