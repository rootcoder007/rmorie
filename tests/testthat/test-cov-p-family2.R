# Coverage for PandInd .. pcm exports. Every expectation is recomputed in
# the test body.

test_that("PandInd multiplies independent probabilities", {
  r <- PandInd(c(0.3, 0.5))
  expect_equal(r$p_and, 0.15)
  expect_equal(c(r$p_a, r$p_b), c(0.3, 0.5))
  r3 <- PandInd(c(0.9, 0.8, 0.5))
  expect_equal(r3$p_and, 0.9 * 0.8 * 0.5, tolerance = 1e-12)
  expect_null(r3[["p_a"]])
  expect_error(PandInd(c(0.2, 1.2)), "probabilities")
  expect_error(PandInd(numeric(0)), "probabilities")
})

test_that("Pareff is Levin's attributable fraction with a delta-method interval", {
  r <- Pareff(0.3, 2.5, se_RR = 0.4, alpha = 0.1)
  paf <- 0.3 * 1.5 / (0.3 * 1.5 + 1)
  # d PAF / d RR = p / (p (RR - 1) + 1)^2, checked by a central difference
  h <- 1e-6
  lev <- function(rr) 0.3 * (rr - 1) / (0.3 * (rr - 1) + 1)
  dn <- (lev(2.5 + h) - lev(2.5 - h)) / (2 * h)
  expect_equal(r$estimate, paf, tolerance = 1e-12)
  expect_equal(r$se, 0.3 / (0.3 * 1.5 + 1)^2 * 0.4, tolerance = 1e-12)
  expect_equal(r$se, abs(dn) * 0.4, tolerance = 1e-8)
  z <- stats::qnorm(0.95)
  expect_equal(c(r$ci_lower, r$ci_upper), paf + c(-1, 1) * z * r$se, tolerance = 1e-12)
  expect_true(is.nan(Pareff(0.3, 2.5)$se))
  expect_error(Pareff(0.3, 0), "RR must be positive")
  expect_error(Pareff(1.2, 2), "pe must lie")
})

cov_pc_shares <- function(P, V) {
  P <- as.matrix(P)
  V <- as.matrix(V)
  sh <- numeric(nrow(P))
  for (i in seq_len(nrow(V))) {
    d <- rowSums((P - matrix(V[i, ], nrow(P), ncol(P), byrow = TRUE))^2)
    win <- d == min(d)
    sh <- sh + win / sum(win)
  }
  sh / nrow(V)
}

test_that("EntryGame finds incumbents that anticipate the best entrant", {
  voters <- cbind(c(0, 1, 1, 2, 3, 4, 4, 5, 6))
  grid <- cbind(0:6)
  r <- EntryGame(voters, grid, n_incumbents = 2)
  entrant <- function(inc) {
    s <- vapply(0:6, function(g) cov_pc_shares(grid[c(inc, g) + 1, , drop = FALSE], voters)[3], 0)
    (0:6)[which(s > max(s) - 1e-12)[1]]
  }
  pay <- function(inc, j) cov_pc_shares(grid[c(inc, entrant(inc)) + 1, , drop = FALSE], voters)[j]
  inc <- c(max(0, 3 - 1 - 0), min(6, 3 + 1))
  repeat {
    moved <- FALSE
    for (j in 1:2) {
      best <- inc[j]
      bs <- pay(inc, j)
      for (g in 0:6) {
        tr <- inc
        tr[j] <- g
        s <- pay(tr, j)
        if (s > bs + 1e-12) {
          best <- g
          bs <- s
        }
      }
      if (best != inc[j]) {
        inc[j] <- best
        moved <- TRUE
      }
    }
    if (!moved) break
  }
  e <- entrant(inc)
  expect_equal(as.numeric(r$incumbents), inc)
  expect_equal(as.numeric(r$entrant), e)
  expect_equal(r$shares, cov_pc_shares(grid[c(inc, e) + 1, , drop = FALSE], voters), tolerance = 1e-12)
})

test_that("MergeSplitParties recomputes proximity shares after a merge or split", {
  voters <- cbind(c(0, 0.5, 1, 2, 3, 3.5, 4, 6))
  P <- cbind(c(0.5, 2.5, 5))
  old <- cov_pc_shares(P, voters)
  m <- MergeSplitParties(P, voters, merge = c(0, 1))
  mp <- (old[1] * 0.5 + old[2] * 2.5) / (old[1] + old[2])
  nP <- rbind(P[3, , drop = FALSE], mp)
  expect_equal(m$old, old, tolerance = 1e-12)
  expect_equal(m$positions, unname(nP), tolerance = 1e-12)
  expect_equal(m$new, unname(cov_pc_shares(nP, voters)), tolerance = 1e-12)
  expect_equal(m$gain, m$new[2] - (old[1] + old[2]), tolerance = 1e-12)
  s <- MergeSplitParties(P, voters, split = c(1, 0.75))
  sP <- rbind(P[c(1, 3), , drop = FALSE], 2.5 - 0.75, 2.5 + 0.75)
  expect_equal(s$positions, unname(sP), tolerance = 1e-12)
  ns <- cov_pc_shares(sP, voters)
  expect_equal(s$gain, ns[3] + ns[4] - old[2], tolerance = 1e-12)
})

test_that("LaverDynamics moves aggregators to their voters and predators toward the leader", {
  V <- rbind(c(0, 0), c(1, 0), c(0, 1), c(5, 5), c(6, 5), c(5, 6), c(6, 6))
  P <- rbind(c(0.5, 0.5), c(5.5, 5.0), c(3, 3))
  r <- LaverDynamics(V, P, rules = c("aggregator", "sticker", "predator"), n_steps = 2, speed = 0.4)
  near <- apply(V, 1, function(v) which.min(colSums((v - t(P))^2)))
  sh <- cov_pc_shares(P, V)
  big <- which.max(sh)
  P1 <- P
  P1[1, ] <- colMeans(V[near == 1, , drop = FALSE])
  dxy <- P[big, ] - P[3, ]
  P1[3, ] <- P[3, ] + min(0.4, sqrt(sum(dxy^2))) * dxy / sqrt(sum(dxy^2))
  expect_equal(r$path[[2]], P1, tolerance = 1e-12)
  expect_equal(r$shares[[2]], cov_pc_shares(P1, V), tolerance = 1e-12)
  expect_length(r$path, 3)
  # a hunter always moves exactly `speed` per step
  h <- LaverDynamics(V, P, rules = c("hunter", "sticker", "sticker"), n_steps = 3, speed = 0.3, seed = 4)
  for (s in 1:3) {
    expect_equal(sqrt(sum((h$path[[s + 1]][1, ] - h$path[[s]][1, ])^2)), 0.3, tolerance = 1e-12)
  }
})

test_that("PascalId verifies Pascal's rule", {
  r <- PascalId(9, 4)
  expect_equal(r$lhs, choose(9, 4))
  expect_equal(c(r$term_left, r$term_right), c(choose(8, 3), choose(8, 4)))
  expect_equal(r$rhs, r$lhs)
  expect_error(PascalId(4, 0), "1 <= k <= n - 1")
  expect_error(PascalId(-1, 1), "non-negative")
})

test_that("morie_find_project_root walks up to a pyproject with libexec or docs", {
  root <- file.path(tempdir(), "cov_proj_root")
  deep <- file.path(root, "a", "b")
  dir.create(deep, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "libexec", "config"), recursive = TRUE, showWarnings = FALSE)
  writeLines("[project]", file.path(root, "pyproject.toml"))
  expect_equal(morie_find_project_root(deep), normalizePath(root, winslash = "/"))
  # too few levels allowed
  expect_error(morie_find_project_root(deep, max_up = 2), "Unable to detect project root")
  unlink(file.path(root, "libexec"), recursive = TRUE)
  dir.create(file.path(root, "docs", "source"), recursive = TRUE, showWarnings = FALSE)
  expect_equal(morie_find_project_root(deep), normalizePath(root, winslash = "/"))
  unlink(root, recursive = TRUE)
})

test_that("Pcadim is standardised PCA with a sign-pinned basis", {
  X <- cbind(c(1.2, 2.3, 3.1, 4.8, 5.0, 6.7, 7.2),
             c(2.0, 1.5, 3.9, 3.1, 5.5, 4.2, 6.8),
             c(0.3, -0.2, 0.9, 0.1, 1.4, 0.2, 1.1))
  r <- Pcadim(X, k = 2)
  pc <- stats::prcomp(X, scale. = TRUE)
  expect_equal(r$eigenvalues, pc$sdev^2, tolerance = 1e-10)
  for (j in 1:3) {
    v <- pc$rotation[, j]
    v <- v * sign(v[which.max(abs(v))])
    expect_equal(r$loadings[, j], unname(v), tolerance = 1e-10)
    expect_equal(r$scores[, j], as.numeric(scale(X) %*% v), tolerance = 1e-10)
  }
  expect_equal(r$compressed, r$scores[, 1:2], tolerance = 1e-12)
  expect_equal(r$cum_variance, cumsum(pc$sdev^2) / 3, tolerance = 1e-10)
  expect_error(Pcadim(X, k = 4), "between 1 and the number of columns")
})

test_that("Pcasnps is EIGENSTRAT PCA with the Tracy-Widom statistic", {
  G <- rbind(c(0, 1, 2, 1, 0, 2), c(1, 1, 0, 2, 0, 1), c(2, 0, 1, 1, 1, 0),
             c(0, 2, 2, 0, 1, 1), c(1, 1, 1, 1, 1, 1), c(2, 0, 0, 2, 1, 2))
  G <- cbind(G, 2)  # a monomorphic (fixed homozygous) marker is dropped
  r <- Pcasnps(G, n_components = 2)
  mu <- colMeans(G)
  p <- mu / 2
  keep <- p * (1 - p) > 0
  M <- sweep(sweep(G[, keep], 2, mu[keep]), 2, sqrt(p[keep] * (1 - p[keep])), "/")
  X <- tcrossprod(M) / sum(keep)
  e <- eigen(X, symmetric = TRUE)
  expect_equal(r$eigenvalues, e$values, tolerance = 1e-10)
  expect_equal(r$n_dropped, 1L)
  # components agree up to sign
  expect_equal(abs(colSums(r$pcs * e$vectors[, 1:2])), c(1, 1), tolerance = 1e-10)
  n <- 6
  lam <- e$values
  neff <- (n + 1) * sum(lam)^2 / ((n - 1) * sum(lam^2) - sum(lam)^2)
  a <- sqrt(n - 1) + sqrt(neff)
  tw <- (n * lam[1] / sum(lam) - a^2 / neff) / ((a / neff) * (1 / sqrt(n - 1) + 1 / sqrt(neff))^(1 / 3))
  expect_equal(r$n_eff, neff, tolerance = 1e-9)
  expect_equal(r$tw_statistic, tw, tolerance = 1e-9)
  expect_equal(r$variance_explained, lam / sum(lam), tolerance = 1e-10)
  expect_error(Pcasnps(G[1, , drop = FALSE]), "two individuals")
  expect_error(Pcasnps(G, n_components = 7), "1 <= k <= n")
  expect_error(Pcasnps(matrix(0, 3, 2)), "monomorphic")
})

test_that("Pcfunc differentiates the border-corrected K function", {
  P <- cbind(c(0.12, 0.35, 0.8, 0.52, 0.9, 0.22, 0.61, 0.44, 0.7, 0.3),
             c(0.2, 0.75, 0.4, 0.5, 0.95, 0.44, 0.13, 0.28, 0.66, 0.9))
  rs <- c(0.1, 0.15, 0.2)
  kb <- function(h) {
    D <- as.matrix(stats::dist(P))
    diag(D) <- Inf
    b <- pmin(P[, 1], 1 - P[, 1], P[, 2], 1 - P[, 2])
    keep <- b > h
    sum(D[keep, ] <= h) / sum(keep) / 10
  }
  r <- Pcfunc(P, c(0, 0, 1, 1), rs, h = 0.02)
  g <- vapply(rs, function(x) (kb(x + 0.02) - kb(x - 0.02)) / 0.04 / (2 * pi * x), 0)
  expect_equal(r$g, g, tolerance = 1e-12)
  expect_equal(r$K, vapply(rs, kb, 0), tolerance = 1e-12)
  expect_equal(Pcfunc(P, c(0, 0, 1, 1), rs)$h, 0.1 / 4)
  expect_error(Pcfunc(P, c(0, 0, 1, 1), 0), "strictly positive")
  expect_error(Pcfunc(P, c(0, 0, 1, 1), numeric(0)), "r is empty")
  expect_error(Pcfunc(P, c(0, 0, 1, 1), 0.1, h = -1), "h must be positive")
  expect_error(Pcfunc(P[1, , drop = FALSE], c(0, 0, 1, 1), 0.1), "two points")
})

test_that("Pcgxe reduces a GxE table by its leading principal components", {
  X <- rbind(c(3.1, 2.8, 4.0, 3.5), c(2.2, 2.9, 3.1, 2.0), c(4.5, 4.1, 5.2, 4.8),
             c(1.9, 2.5, 2.2, 1.6), c(3.8, 3.0, 4.6, 4.1))
  for (sc in c(FALSE, TRUE)) {
    r <- Pcgxe(X, 2, scale = sc)
    pc <- stats::prcomp(X, scale. = sc)
    expect_equal(r$eigenvalues, pc$sdev^2, tolerance = 1e-10)
    Cc <- scale(X, scale = if (sc) apply(X, 2, stats::sd) else FALSE)
    V <- pc$rotation[, 1:2]
    # the rank-2 reconstruction is sign-invariant
    expect_equal(r$GxE_approx, unname(Cc %*% V %*% t(V)), tolerance = 1e-10)
    expect_equal(r$estimate, sum(pc$sdev[1:2]^2) / sum(pc$sdev^2), tolerance = 1e-10)
  }
  expect_equal(Pcgxe(X, 1)$center, colMeans(X), tolerance = 1e-12)
  expect_error(Pcgxe(X, 5), "k must lie")
  expect_error(Pcgxe(X[1, , drop = FALSE], 1), "two rows")
  expect_error(Pcgxe(cbind(X, 1), 1, scale = TRUE), "zero variance")
})

test_that("morie_pcm gives Masters' partial credit probabilities", {
  b <- c(-0.5, 0.3, 1.2)
  r <- morie_pcm(0.4, b, a = 1.3, D = 1.7)
  num <- exp(c(0, cumsum(1.7 * 1.3 * (0.4 - b))))
  p <- num / sum(num)
  expect_equal(r$probabilities, p, tolerance = 1e-12)
  expect_equal(r$expected_score, sum(0:3 * p), tolerance = 1e-12)
  expect_equal(r$n_categories, 4L)
  # one step: the Rasch / 2PL logistic
  expect_equal(morie_pcm(1, 0.2)$probabilities[2], stats::plogis(0.8), tolerance = 1e-12)
  expect_error(morie_pcm(0, numeric(0)), "at least one step")
})
