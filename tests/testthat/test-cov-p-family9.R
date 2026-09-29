# Coverage for propme .. Ptotal exports. Every expectation is recomputed in
# the test body.

test_that("Propme is the MacKinnon proportion mediated with delta-method SEs", {
  r <- Propme(0.5, 0.8, 0.6, se_a = 0.1, se_b = 0.2, se_c_prime = 0.15)
  ab <- 0.4
  tot <- 1
  expect_equal(r$estimate, ab / tot, tolerance = 1e-12)
  expect_equal(r$ratio, ab / 0.6, tolerance = 1e-12)
  # gradient of ab / (c + ab) with respect to (a, b, c)
  g <- c(0.8 * 0.6, 0.5 * 0.6, -ab) / tot^2
  expect_equal(r$se, sqrt(sum(g^2 * c(0.01, 0.04, 0.0225))), tolerance = 1e-12)
  gr <- c(0.8 / 0.6, 0.5 / 0.6, -ab / 0.36)
  expect_equal(r$se_ratio, sqrt(sum(gr^2 * c(0.01, 0.04, 0.0225))), tolerance = 1e-12)
  expect_null(Propme(0.5, 0.8, 0.6)$se)
  expect_true(is.nan(Propme(0.5, 0.8, 0, 1, 1, 1)$se_ratio))
  expect_error(Propme(1, -0.5, 0.5), "total effect")
})

test_that("Prevratio is the prevalence ratio with a log-scale interval", {
  r <- Prevratio(0.3, 0.2, n_exposed = 150, n_unexposed = 200, alpha = 0.1)
  se <- sqrt(0.7 / (0.3 * 150) + 0.8 / (0.2 * 200))
  expect_equal(r$pr, 1.5, tolerance = 1e-12)
  expect_equal(r$se_log, se, tolerance = 1e-12)
  expect_equal(c(r$ci_lower, r$ci_upper), exp(log(1.5) + c(-1, 1) * stats::qnorm(0.95) * se),
               tolerance = 1e-12)
  expect_true(is.na(Prevratio(0.3, 0.2)$se_log))
  expect_error(Prevratio(0, 0.2), "strictly between")
})

test_that("Prtdid is the Goodman-Bacon decomposition of the TWFE coefficient", {
  units <- rep(1:6, each = 5)
  tt <- rep(1:5, 6)
  g <- c(3, 3, 4, 4, Inf, Inf)[units]
  D <- as.integer(tt >= g)
  y <- 1 + 0.3 * units + 0.2 * tt + ifelse(D == 1, 1 + 0.5 * (tt - g), 0) +
    c(0.1, -0.2, 0.05, 0, 0.15, -0.1, 0.2, 0, -0.05, 0.1, 0, 0.1, -0.1, 0.05, 0.2,
      -0.15, 0, 0.1, -0.05, 0, 0.05, -0.1, 0.15, 0, -0.2, 0.1, 0, -0.05, 0.1, 0)
  df <- data.frame(y = y, D = D, id = units, t = tt)
  r <- Prtdid(df, "y", "D", "id", "t")
  twfe <- unname(stats::coef(stats::lm(y ~ D + factor(id) + factor(t), data = df))["D"])
  expect_equal(r$overall_estimate, twfe, tolerance = 1e-10)
  expect_equal(sum(r$components$weight), 1, tolerance = 1e-12)
  skip_if_not_installed("bacondecomp")
  b <- suppressMessages(bacondecomp::bacon(y ~ D, data = df, id_var = "id", time_var = "t", quietly = TRUE))
  expect_equal(sort(r$components$weight), sort(b$weight), tolerance = 1e-10)
  expect_equal(sort(r$components$estimate), sort(b$estimate), tolerance = 1e-10)
})

test_that("Pseudo sums products of path coefficients", {
  B <- matrix(0, 4, 4)
  B[1, 2] <- 0.5
  B[1, 3] <- 0.4
  B[2, 3] <- 0.7
  B[2, 4] <- -0.3
  B[3, 4] <- 0.6
  r <- Pseudo(B, 1, 4)
  paths <- c(0.5 * -0.3, 0.5 * 0.7 * 0.6, 0.4 * 0.6)
  expect_equal(r$total, sum(paths), tolerance = 1e-12)
  expect_equal(r$total, solve(diag(4) - B)[1, 4], tolerance = 1e-12)
  expect_equal(r$direct, 0)
  expect_equal(r$indirect, sum(paths), tolerance = 1e-12)
  # only the edges through node 3
  rs <- Pseudo(B, 1, 4, edges = list(c(1, 3), c(3, 4), c(1, 2), c(2, 3)))
  expect_equal(rs$estimate, 0.4 * 0.6 + 0.5 * 0.7 * 0.6, tolerance = 1e-12)
  expect_equal(rs$n_edges_used, 4L)
  C <- B
  C[4, 1] <- 0.1
  expect_error(Pseudo(C, 1, 4), "not acyclic")
  expect_error(Pseudo(B, 1, 1), "distinct indices")
  expect_error(Pseudo(B[1:3, ], 1, 2), "square")
  expect_error(Pseudo(B, 1, 4, edges = list(c(1, 5))), "out of range")
})

test_that("Pseudor2 gives McFadden, Cox-Snell and Nagelkerke R2", {
  set <- data.frame(x = c(0.2, 1.1, -0.5, 0.8, 1.9, -1.0, 0.3, 0.6, 1.4, -0.2),
                    y = c(0, 1, 0, 1, 1, 0, 0, 1, 1, 0))
  set$y[3] <- 1
  f <- stats::glm(y ~ x, family = stats::binomial(), data = set)
  f0 <- stats::glm(y ~ 1, family = stats::binomial(), data = set)
  l1 <- as.numeric(stats::logLik(f))
  l0 <- as.numeric(stats::logLik(f0))
  r <- Pseudor2(l1, l0, 10)
  expect_equal(r$mcfadden, 1 - l1 / l0, tolerance = 1e-12)
  expect_equal(r$coxsnell, 1 - (exp(l0) / exp(l1))^(2 / 10), tolerance = 1e-12)
  expect_equal(r$nagelkerke, r$coxsnell / (1 - exp(l0)^(2 / 10)), tolerance = 1e-12)
  expect_error(Pseudor2(-1, 0, 10), "strictly negative")
  expect_error(Pseudor2(-1, -2, 0), "at least 1")
  expect_error(Pseudor2(Inf, -2, 5), "finite")
})

test_that("Psoop runs the van der Corput particle swarm", {
  vdc <- function(i, b) {
    k <- i + 1
    f <- 1
    r <- 0
    while (k > 0) {
      f <- f / b
      r <- r + f * (k %% b)
      k <- k %/% b
    }
    r
  }
  fn <- function(x) (x[1] - 0.3)^2 + 2 * (x[2] + 0.4)^2
  bnd <- list(c(-1, 1), c(-2, 1))
  np <- 4
  pos <- t(vapply(1:np, function(i) c(-1 + 2 * vdc(i, 2), -2 + 3 * vdc(i, 3)), numeric(2)))
  vel <- matrix(0, np, 2)
  pb <- pos
  pv <- apply(pos, 1, fn)
  gb <- pos[which.min(pv), ]
  gv <- min(pv)
  k <- 1
  for (it in 1:15) for (i in 1:np) {
    for (j in 1:2) {
      vel[i, j] <- 0.7 * vel[i, j] + 1.5 * vdc(k, 2) * (pb[i, j] - pos[i, j]) + 1.5 * vdc(k, 3) * (gb[j] - pos[i, j])
      k <- k + 1
      pos[i, j] <- min(max(pos[i, j] + vel[i, j], bnd[[j]][1]), bnd[[j]][2])
    }
    v <- fn(pos[i, ])
    if (v < pv[i]) {
      pv[i] <- v
      pb[i, ] <- pos[i, ]
      if (v < gv) {
        gv <- v
        gb <- pos[i, ]
      }
    }
  }
  r <- Psoop(fn, bnd, n_particles = np, maxiter = 15)
  expect_equal(r$value, gv, tolerance = 1e-12)
  expect_equal(r$x, gb, tolerance = 1e-12)
  expect_equal(r$n_eval, np + 15 * np)
  expect_error(Psoop(1, bnd), "callable")
  expect_error(Psoop(fn, list()), "bounds is empty")
  expect_error(Psoop(fn, list(c(1, 0))), "hi > lo")
  expect_error(Psoop(fn, bnd, n_particles = 0), "at least 1")
  expect_error(Psoop(fn, bnd, w = -1), "non-negative")
})

test_that("morie_ptmcmc replays its Metropolis moves and replica swaps", {
  lp <- function(x) -0.5 * (x - 1)^2 / 0.5
  temps <- c(1, 2, 4)
  e <- .ghc_rng(11)
  x <- rep(0, 3)
  l <- vapply(x, lp, 0)
  acc <- integer(3)
  sacc <- integer(2)
  cold <- numeric(25)
  for (s in 1:25) {
    for (k in 1:3) {
      pr <- x[k] + 0.8 * sqrt(temps[k]) * .ghc_norm(e, 1L)
      lpp <- lp(pr)
      if (log(.ghc_unif(e, 1L)) < (lpp - l[k]) / temps[k]) {
        x[k] <- pr
        l[k] <- lpp
        acc[k] <- acc[k] + 1L
      }
    }
    if (s %% 2 == 0) {
      for (k in 1:2) {
        dl <- (1 / temps[k] - 1 / temps[k + 1]) * (l[k + 1] - l[k])
        if (log(.ghc_unif(e, 1L)) < min(0, dl)) {
          x[c(k, k + 1)] <- x[c(k + 1, k)]
          l[c(k, k + 1)] <- l[c(k + 1, k)]
          sacc[k] <- sacc[k] + 1L
        }
      }
    }
    cold[s] <- x[1]
  }
  r <- morie_ptmcmc(lp, temps, 0, n_iter = 25, step = 0.8, seed = 11, swap_every = 2)
  expect_equal(r$chain, cold, tolerance = 1e-12)
  expect_equal(r$chains_last, x, tolerance = 1e-12)
  expect_equal(r$accept_rate, acc / 25)
  expect_equal(r$swap_accept_rate, sacc / 12)
  expect_error(morie_ptmcmc(lp, 1, 0), "two temperatures")
  expect_error(morie_ptmcmc(lp, c(2, 1), 0), "ascending")
})

test_that("Ptotal is the law of total probability", {
  r <- Ptotal(c(0.2, 0.5, 0.3), c(0.9, 0.4, 0.1))
  expect_equal(r$p_total, 0.2 * 0.9 + 0.5 * 0.4 + 0.3 * 0.1, tolerance = 1e-12)
  expect_equal(r$p_event, r$p_total)
  expect_error(Ptotal(c(0.5, 0.4), c(0.1, 0.2)), "sum to 1")
  expect_error(Ptotal(c(0.5, 0.5), c(0.1, 1.2)), "in \\[0, 1\\]")
  expect_error(Ptotal(1, c(0.1, 0.2)), "equal-length")
})
