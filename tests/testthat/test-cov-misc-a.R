# Coverage for assorted capitalised helpers (CoalitionGames.R,
# CompositionalCircular.R, CoxProcesses.R, DemAnalysis.R,
# ElectoralSystems.R, EnvironmentalStatistics.R, Doctide.R, Dssoot.R,
# Dyntmt.R, Effmod.R, Frdbnd.R, Frwol2.R, Fwlfwd.R), each recomputed from
# its definition in base R.

test_that("MedianLines and SpatialHeart for weighted majority games", {
  tri <- rbind(c(0, 0), c(4, 0), c(1, 3))
  L <- MedianLines(tri, c(1, 1, 1), quota = 2)
  expect_equal(L, rbind(c(0L, 1L), c(0L, 2L), c(1L, 2L)))
  P4 <- rbind(c(0, 0), c(2, 0), c(0, 2), c(2, 2))
  side <- function(p, a, b) (b[1] - a[1]) * (p[2] - a[2]) - (b[2] - a[2]) * (p[1] - a[1])
  ml <- matrix(integer(0), 0, 2)
  for (i in 1:3) for (j in (i + 1):4) {
    s <- apply(P4, 1, side, a = P4[i, ], b = P4[j, ])
    w <- c(1, 2, 1, 1)
    if (sum(w[s >= 0]) >= 3 && sum(w[s <= 0]) >= 3) ml <- rbind(ml, c(i - 1L, j - 1L))
  }
  expect_equal(MedianLines(P4, c(1, 2, 1, 1), 3), ml)
  h <- SpatialHeart(tri, c(1, 1, 1), 2)
  expect_false(h$core)
  expect_equal(h$vertices[order(h$vertices[, 1], h$vertices[, 2]), ], tri[c(1, 3, 2), ],
               tolerance = 1e-12)
  line <- rbind(c(0, 0), c(1, 0), c(3, 0))
  hc <- SpatialHeart(line, c(1, 1, 1), 2)
  expect_true(hc$core)
  expect_equal(hc$vertices, line[2, , drop = FALSE])
})

test_that("CompositionalQuantileDist is the Aitchison distance of the quartiles", {
  X <- rbind(c(0.2, 0.3, 0.5), c(0.1, 0.6, 0.3), c(0.4, 0.4, 0.2), c(0.3, 0.2, 0.5),
             c(0.25, 0.35, 0.4))
  q1 <- apply(X, 2, quantile, 0.25)
  q3 <- apply(X, 2, quantile, 0.75)
  clr <- function(v) log(v / sum(v)) - mean(log(v / sum(v)))
  expect_equal(CompositionalQuantileDist(X), sqrt(sum((clr(q3) - clr(q1))^2)), tolerance = 1e-12)
})

test_that("LgcpMoments and ThomasSimulate", {
  m <- LgcpMoments(0.5, 0.8, area = 3)
  expect_equal(m$intensity, exp(0.9), tolerance = 1e-12)
  expect_equal(m$expected_count, 3 * exp(0.9), tolerance = 1e-12)
  mm <- LgcpMoments(0, 1, 1, r = c(0, 1, 2), model = list(model = "Exp", psill = 0.5, range = 2))
  expect_equal(mm$pcf, exp(0.5 * exp(-c(0, 1, 2) / 2)), tolerance = 1e-12)
  th <- ThomasSimulate(kappa = 0.5, scale = 0.2, mu = 3, window = c(0, 2, 0, 2), seed = 3)
  ub <- .morie_random_uniform(4096, seed = 3, stream = 0)
  zb <- .morie_random_normal(4096, seed = 3, stream = 5000)
  ui <- 0
  zi <- 0
  U <- function() {
    ui <<- ui + 1
    ub[ui]
  }
  Z <- function() {
    zi <<- zi + 1
    zb[zi]
  }
  pois <- function(m) {
    u <- U()
    k <- 0
    p <- exp(-m)
    F <- p
    while (u > F && p > 0) {
      k <- k + 1
      p <- p * m / k
      F <- F + p
    }
    k
  }
  np <- pois(0.5 * 3.6^2)
  par <- t(vapply(seq_len(np), function(i) c(-0.8 + 3.6 * U(), -0.8 + 3.6 * U()), numeric(2)))
  pts <- matrix(0, 0, 2)
  for (i in seq_len(np)) {
    k <- pois(3)
    for (t in seq_len(k)) {
      p <- par[i, ] + 0.2 * c(Z(), Z())
      if (all(p >= 0 & p <= 2)) pts <- rbind(pts, p)
    }
  }
  expect_equal(th$parents, matrix(par, ncol = 2), tolerance = 1e-12)
  expect_equal(unname(th$points), unname(pts), tolerance = 1e-12)
})

test_that("StreamPowerIndex and BudykoOlr", {
  expect_equal(StreamPowerIndex(c(10, 50), c(0.1, 0.3), res = 2), c(10, 50) * 2 * tan(c(0.1, 0.3)),
               tolerance = 1e-12)
  b <- BudykoOlr(c(-10, 15))
  expect_equal(b$olr, 203.3 + 2.09 * c(-10, 15), tolerance = 1e-12)
  expect_equal(b$equilibrium_temperature, (0.7 * 1361 / 4 - 203.3) / 2.09, tolerance = 1e-12)
  expect_equal(b$sensitivity, 1 / 2.09, tolerance = 1e-12)
})

test_that("NetworkPolarization and RunoffWinner", {
  V <- rbind(c(1, 1, 0, NA), c(1, 1, 0, 1), c(0, 0, 1, 0), c(0, 1, 1, 0))
  r <- NetworkPolarization(V, c(1, 1, 2, 2))
  A <- matrix(0, 4, 4)
  for (i in 1:3) for (j in (i + 1):4) {
    ok <- !is.na(V[i, ]) & !is.na(V[j, ])
    A[i, j] <- A[j, i] <- mean(V[i, ok] == V[j, ok])
  }
  m <- sum(A) / 2
  g <- c(1, 1, 2, 2)
  Q <- sum(vapply(1:2, function(cc) {
    s <- g == cc
    sum(A[s, s]) / 2 / m - (sum(A[s, ]) / (2 * m))^2
  }, 0))
  expect_equal(r$agreement, A, tolerance = 1e-12)
  expect_equal(r$modularity, Q, tolerance = 1e-12)
  ballots <- list(c(0, 1, 2), c(0, 2, 1), c(1, 2, 0), c(1, 0, 2), c(2, 1, 0), c(2, 1, 0), c(1, 2, 0))
  w <- RunoffWinner(ballots)
  expect_equal(w$first_round, c(2L, 3L, 2L))
  expect_equal(unname(w$second_round), c(5, 2))
  expect_equal(w$winner, 1)
  maj <- RunoffWinner(list(c(0, 1), c(0, 1), c(1, 0)))
  expect_equal(maj$winner, 0)
  expect_null(maj$second_round)
})

test_that("Doctide is de Chaisemartin-D'Haultfoeuille DID_M", {
  unit <- rep(c("a", "b", "c", "d"), each = 3)
  time <- rep(1:3, 4)
  D <- c(0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0)
  y <- c(1, 3, 4, 2, 2.5, 5, 1.5, 2, 2.2, 4, 4.5, 3)
  r <- Doctide(y, D, unit, time)
  Y <- matrix(y, 4, byrow = TRUE)
  Dm <- matrix(D, 4, byrow = TRUE)
  dp <- dm <- n10 <- n01 <- numeric(2)
  for (t in 2:3) {
    dy <- Y[, t] - Y[, t - 1]
    sw <- Dm[, t] == 1 & Dm[, t - 1] == 0
    s0 <- Dm[, t] == 0 & Dm[, t - 1] == 0
    s1 <- Dm[, t] == 1 & Dm[, t - 1] == 1
    so <- Dm[, t] == 0 & Dm[, t - 1] == 1
    dp[t - 1] <- if (any(sw) && any(s0)) mean(dy[sw]) - mean(dy[s0]) else 0
    dm[t - 1] <- if (any(s1) && any(so)) mean(dy[s1]) - mean(dy[so]) else 0
    n10[t - 1] <- sum(sw)
    n01[t - 1] <- sum(so)
  }
  ns <- sum(n10 + n01)
  expect_equal(r$estimate, sum(n10 / ns * dp + n01 / ns * dm), tolerance = 1e-12)
  expect_equal(c(r$did_plus, r$did_minus), c(dp, dm), tolerance = 1e-12)
  expect_error(Doctide(numeric(0), 0, "a", 1), "empty")
  expect_error(Doctide(y, D[-1], unit, time), "same length")
  expect_error(Doctide(y, D + 2, unit, time), "binary")
  expect_error(Doctide(y, D, unit, rep(1, 12)), "two periods")
  expect_error(Doctide(y, rep(0, 12), unit, time), "no group switches")
})

test_that("Dssoot bootstraps the indirect effect with the documented LCG", {
  X <- c(0.2, 1.1, 0.5, 1.8, 0.9, 1.4, 0.3, 1.7, 0.8, 1.2)
  M <- 0.5 + 0.7 * X + c(0.1, -0.1, 0.05, 0.2, -0.15, 0.1, -0.05, -0.1, 0.15, 0.02)
  Y <- 1 + 0.4 * X + 0.9 * M + c(-0.05, 0.1, 0.02, -0.1, 0.08, -0.02, 0.05, 0.03, -0.07, 0.01)
  r <- Dssoot(Y, X, M, n_boot = 5, alpha = 0.2, seed = 11)
  a <- coef(lm(M ~ X))[2]
  b <- coef(lm(Y ~ X + M))[3]
  # .s03lstsq adds a 1e-10 ridge, which moves the OLS slopes at ~1e-9
  expect_equal(r$estimate, unname(a * b), tolerance = 1e-8)
  st <- 11
  dr <- vapply(1:5, function(k) {
    idx <- vapply(1:10, function(j) {
      st <<- (1664525 * st + 1013904223) %% 4294967296
      floor(st / 4294967296 * 10) %% 10 + 1
    }, 0)
    coef(lm(M[idx] ~ X[idx]))[2] * coef(lm(Y[idx] ~ X[idx] + M[idx]))[3]
  }, 0)
  expect_equal(r$se_boot, sd(dr), tolerance = 1e-8)
  expect_equal(r$ci_lo, unname(quantile(dr, 0.1)), tolerance = 1e-8)
  expect_error(Dssoot(numeric(0), numeric(0), numeric(0)), "empty")
  expect_error(Dssoot(Y, X[-1], M), "same length")
  expect_error(Dssoot(Y, X, M, n_boot = 0), "at least 1")
  expect_error(Dssoot(Y, X, M, alpha = 1), "strictly")
  expect_error(Dssoot(Y[1:2], X[1:2], M[1:2]), "three observations")
})

test_that("Dyntmt weights regime-followers by inverse treatment probabilities", {
  H <- cbind(c(0.2, 1.5, -0.3, 0.8, 1.1, -0.9, 0.4, 1.9), c(1, -0.5, 0.3, 1.2, -1, 0.6, 0.9, -0.2))
  D <- cbind(c(1, 1, 0, 1, 0, 0, 1, 1), c(1, 0, 1, 1, 0, 1, 0, 0))
  y <- c(3, 2, 1.5, 4, 1, 2.5, 3.2, 2.8)
  r <- Dyntmt(y, D, H)
  pres <- (H > 0) * 1
  fol <- rowSums(D != pres) == 0
  w <- rep(1, 8)
  for (t in 1:2) {
    g <- glm.fit(cbind(1, H[, t]), D[, t], family = binomial(), control = list(epsilon = 1e-14, maxit = 100))
    p <- g$fitted.values
    mg <- mean(D[, t])
    w <- w * ifelse(pres[, t] == 1, mg / p, (1 - mg) / (1 - p))
  }
  ww <- fol * w
  expect_equal(r$estimate, sum(ww * y) / sum(ww), tolerance = 1e-8)
  expect_equal(r$n_follow, sum(fol))
  fnr <- Dyntmt(y, D, H, regime_fn = function(v) v > 0.5)
  expect_equal(fnr$n_follow, sum(rowSums(D != (H > 0.5)) == 0))
  expect_error(Dyntmt(numeric(0), D, H), "empty")
  expect_error(Dyntmt(y, D[-1, ], H), "one row per subject")
  expect_error(Dyntmt(y, D, H[, 1]), "ragged")
  expect_error(Dyntmt(y, D + 1, H), "binary")
  expect_error(Dyntmt(y, D, H, regime_fn = matrix(1, 2, 2)), "n x T")
  expect_error(Dyntmt(y, D, H, regime_fn = 1 - D), "no subject follows")
})

test_that("Effmod reports RERI and multiplicative interaction", {
  A <- c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0)
  V <- c(0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0)
  y <- c(0.1, 0.3, 0.2, 0.8, 0.15, 0.25, 0.3, 0.7, 0.25, 0.9, 0.35, 0.05)
  r <- Effmod(y, A, V)
  p <- tapply(y, list(A, V), mean)
  reri <- (p["1", "1"] - p["1", "0"] - p["0", "1"] + p["0", "0"]) / p["0", "0"]
  expect_equal(r$reri, reri, tolerance = 1e-12)
  expect_equal(r$mult, p["1", "1"] * p["0", "0"] / (p["1", "0"] * p["0", "1"]), tolerance = 1e-12)
  h <- c(0.5, -0.2, 0.1, 0.3, -0.4, 0.6, 0, -0.1, 0.2, -0.3, 0.4, -0.5)
  rh <- Effmod(y, A, V, H = h)
  cell <- factor(paste(A, V))
  f <- lm(y ~ 0 + cell + I(h - mean(h)))
  expect_equal(c(rh$p00, rh$p10, rh$p01, rh$p11), unname(coef(f)[c(1, 3, 2, 4)]), tolerance = 1e-8)
  expect_error(Effmod(numeric(0), numeric(0), numeric(0)), "empty")
  expect_error(Effmod(y, A[-1], V), "same length")
  expect_error(Effmod(y, A + 1, V), "A must be binary")
  expect_error(Effmod(y, A, V + 1), "V must be binary")
  expect_error(Effmod(y[1:3], c(0, 1, 0), c(0, 0, 1)), "every A x V cell")
  expect_error(Effmod(y, A, V, H = 1:3), "one row per observation")
  expect_error(Effmod(-y, A, V), "strictly positive")
})

test_that("Frdbnd gives the Frechet-Hoeffding copula bounds", {
  u <- c(0.2, 0.5, 0.9)
  v <- c(0.6, 0.7, 0.3)
  r <- Frdbnd(u, v, joint = c(0.1, 0.6, 0.3))
  expect_equal(r$lower, pmax(u + v - 1, 0), tolerance = 1e-12)
  expect_equal(r$upper, pmin(u, v), tolerance = 1e-12)
  expect_equal(r$n_violations, 1L)
  expect_equal(Frdbnd(u, v)$respects_bounds, 1L)
  expect_error(Frdbnd(numeric(0), numeric(0)), "empty")
  expect_error(Frdbnd(u, v[-1]), "different lengths")
  expect_error(Frdbnd(u + 1, v), "\\[0, 1\\]")
  expect_error(Frdbnd(u, v, joint = 1), "different lengths")
})

test_that("Frank-Wolfe and fully-corrective Frank-Wolfe on the simplex", {
  cc <- c(0.2, 0.5, 0.3)
  f <- function(x) sum((x - cc)^2)
  gf <- function(x) 2 * (x - cc)
  V <- diag(3)
  r <- Frwol2(f, gf, V, c(1, 0, 0), steps = 8)
  x <- c(1, 0, 0)
  path <- f(x)
  for (t in 0:7) {
    g <- gf(x)
    i <- which.min(V %*% g)
    x <- (1 - 2 / (t + 2)) * x + 2 / (t + 2) * V[i, ]
    path <- c(path, f(x))
  }
  expect_equal(r$x, x, tolerance = 1e-12)
  expect_equal(r$f_path, path, tolerance = 1e-12)
  fc <- Fwlfwd(f, gf, V, c(1, 0, 0), steps = 8, rounds = 5)
  expect_true(all(diff(fc$f_path) <= 1e-15))
  expect_lt(fc$estimate, r$estimate)
  expect_equal(sum(fc$x), 1, tolerance = 1e-12)
  expect_error(Frwol2(f, gf, matrix(numeric(0), 0, 3), cc), "no vertices")
  expect_error(Frwol2(f, gf, V, c(1, 0)), "different dimensions")
  expect_error(Frwol2(1, gf, V, cc), "callable")
  expect_error(Frwol2(f, gf, V, cc, steps = 0), "at least 1")
  expect_error(Frwol2(f, function(x) 1, V, cc), "wrong length")
  expect_error(Fwlfwd(f, gf, matrix(numeric(0), 0, 3), cc), "no vertices")
  expect_error(Fwlfwd(f, gf, V, c(1, 0)), "different dimensions")
  expect_error(Fwlfwd(f, 1, V, cc), "callable")
  expect_error(Fwlfwd(f, gf, V, cc, steps = 0), "at least 1")
})
