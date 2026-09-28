# SPDX-License-Identifier: AGPL-3.0-or-later
# Cross-validation: Wildlife.R vs adehabitatHR, mrds, unmarked and vegan.

test_that("McpHomeRange equals adehabitatHR::mcp areas", {
  skip_if_not_installed("adehabitatHR")
  skip_if_not_installed("sp")
  U <- .morie_random_uniform(120, seed = 3, stream = 0)
  xy <- cbind(1000 * U[1:60], 800 * U[61:120])
  for (pc in c(50, 80, 95, 100)) {
    ref <- suppressWarnings(adehabitatHR::mcp(sp::SpatialPoints(xy), percent = pc, unin = "m", unout = "m2"))
    expect_equal(McpHomeRange(xy, pc)$area, ref$area, tolerance = 1e-10, info = pc)
  }
})

test_that("DistanceSampling equals mrds::ddf (half-normal and hazard rate, line and point)", {
  skip_if_not_installed("mrds")
  U <- .morie_random_uniform(400, seed = 5, stream = 0)
  x <- abs(qnorm(0.5 + 0.49 * U[1:120])) * 12
  for (tr in c("line", "point")) for (key in c("hn", "hr")) {
    d <- data.frame(distance = x, object = seq_along(x), observer = 1, detected = 1)
    ref <- suppressMessages(suppressWarnings(mrds::ddf(dsmodel = as.formula(paste0("~cds(key = \"", key, "\")")),
      data = d, method = "ds", meta.data = list(width = 30, point = tr == "point"))))
    m <- DistanceSampling(x, width = 30, key = key, transect = tr)
    # mrds stops at optim's default tolerance: our maximum is at least as high
    expect_gte(m$loglik, as.numeric(logLik(ref)) - 1e-9)
    par <- if (key == "hr") unname(ref$par[c(2, 1)]) else unname(ref$par)  # mrds orders (shape, scale)
    tol <- if (key == "hn") 1e-5 else 2e-3
    expect_equal(m$coefficients, par, tolerance = tol, info = paste(tr, key))
    expect_equal(m$p, unname(summary(ref)$average.p), tolerance = tol, info = paste(tr, key))
  }
})

test_that("OccupancyModel and NmixtureModel equal unmarked::occu / pcount", {
  skip_if_not_installed("unmarked")
  U <- .morie_random_uniform(2000, seed = 7, stream = 0)
  n <- 60
  xs <- 2 * U[1:n] - 1
  z <- U[n + 1:n] < plogis(0.3 + 1.2 * xs)
  Y <- matrix(as.numeric(z & (U[2 * n + seq_len(4 * n)] < 0.45)), n, 4)
  Y[3, 4] <- NA
  um <- unmarked::unmarkedFrameOccu(y = Y, siteCovs = data.frame(xs = xs))
  ref <- unmarked::occu(~1 ~ xs, um)
  m <- OccupancyModel(Y, psi_covariates = cbind(1, xs))
  # unmarked stops at optim's default reltol; our maximum is at least as high and the estimates agree
  expect_gte(m$loglik, -ref@negLogLike - 1e-10)
  expect_equal(c(m$psi_coefficients, m$p_coefficients), unname(unmarked::coef(ref)[c(1, 2, 3)]), tolerance = 1e-3)
  expect_equal(m$se, unname(sqrt(diag(unmarked::vcov(ref)))), tolerance = 1e-3)
  N <- qpois(U[500 + 1:n], exp(1 + 0.5 * xs))
  C <- matrix(qbinom(U[700 + seq_len(3 * n)], rep(N, 3), 0.5), n, 3)
  up <- unmarked::unmarkedFramePCount(y = C, siteCovs = data.frame(xs = xs))
  ref2 <- suppressWarnings(unmarked::pcount(~1 ~ xs, up))
  m2 <- NmixtureModel(C, lambda_covariates = cbind(1, xs))
  expect_gte(m2$loglik, -ref2@negLogLike - 1e-10)
  expect_equal(c(m2$lambda_coefficients, m2$p_coefficients), unname(unmarked::coef(ref2)), tolerance = 5e-3)
})

test_that("PartialMantel equals vegan::mantel.partial; CircuitResistance equals the Laplacian pseudo-inverse", {
  skip_if_not_installed("vegan")
  U <- .morie_random_uniform(300, seed = 9, stream = 0)
  P <- cbind(U[1:12], U[13:24])
  A <- as.matrix(dist(P)) + as.matrix(dist(U[25:36]))
  B <- as.matrix(dist(cbind(P, U[37:48])))
  C <- as.matrix(dist(U[49:60]))
  ref <- vegan::mantel.partial(as.dist(A), as.dist(B), as.dist(C), permutations = 0)
  expect_equal(PartialMantel(A, B, C, nsim = 0)$statistic, unname(ref$statistic), tolerance = 1e-12)
  R <- matrix(1 + 4 * U[100 + 1:20], 4, 5)
  cr <- CircuitResistance(R, rbind(c(1, 1), c(4, 5), c(2, 3)), directions = 8)
  n <- length(R)
  L <- matrix(0, n, n)
  ix <- function(i, j) (i - 1) * 5 + j
  for (i in 1:4) for (j in 1:5) for (d in list(c(0, 1), c(1, 0), c(1, 1), c(1, -1))) {
    x <- i + d[1]
    y <- j + d[2]
    if (x <= 4 && y >= 1 && y <= 5) {
      w <- 1 / ((if (all(d != 0)) sqrt(2) else 1) * (R[i, j] + R[x, y]) / 2)
      a <- ix(i, j)
      b <- ix(x, y)
      L[a, a] <- L[a, a] + w
      L[b, b] <- L[b, b] + w
      L[a, b] <- L[a, b] - w
      L[b, a] <- L[b, a] - w
    }
  }
  Lp <- MASS::ginv(L)
  e <- function(a, b) {
    v <- numeric(n)
    v[a] <- 1
    v[b] <- -1
    v
  }
  expect_equal(cr$resistance[1, 2], drop(t(e(ix(1, 1), ix(4, 5))) %*% Lp %*% e(ix(1, 1), ix(4, 5))), tolerance = 1e-10)
  expect_equal(cr$resistance[2, 3], drop(t(e(ix(4, 5), ix(2, 3))) %*% Lp %*% e(ix(4, 5), ix(2, 3))), tolerance = 1e-10)
})
