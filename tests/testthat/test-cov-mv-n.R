# Coverage for the minimum-volume ellipsoid and its regression, MWEM,
# the multi-environment GxE BLUP, de Haan spectral max-stable fields,
# Nadaraya-Watson and local-linear smoothers, NB2 dispersion, NDCG, the
# linear-SEM natural direct effect and negative-control outcomes;
# recomputed with combn(), mahalanobis(), lm(), MASS and replayed
# random streams in the test body.

test_that("Mvedet searches all (p+1)-subsets for the smallest inflated ellipsoid", {
  set.seed(3)
  X <- cbind(rnorm(8), rnorm(8))
  X[8, ] <- c(6, -5)
  h <- (8 + 2 + 1) %/% 2
  cand <- combn(8, 3, simplify = FALSE)
  fit1 <- function(J) {
    mu <- colMeans(X[J, ])
    S <- cov(X[J, ])
    d2 <- mahalanobis(X, mu, S)
    m2 <- sort(d2)[h]
    list(obj = m2^2 * det(S), mu = mu, S = S, m2 = m2, J = J, cov = sort(order(d2)[1:h]))
  }
  fits <- lapply(cand, fit1)
  best <- fits[[which.min(vapply(fits, `[[`, 0, "obj"))]]
  r <- Mvedet(X)
  expect_equal(r$estimate, best$obj, tolerance = 1e-9)
  expect_equal(r$center, best$mu, tolerance = 1e-12)
  expect_equal(r$cov, best$S * best$m2, tolerance = 1e-9)
  expect_equal(r$subset, best$J - 1)
  expect_equal(r$covered, best$cov - 1)
  expect_equal(r$h, h)
  sub <- Mvedet(X, n_starts = 10)
  pick <- fits[seq(1, 46, by = 5)]
  expect_equal(sub$estimate, min(vapply(pick, `[[`, 0, "obj")), tolerance = 1e-9)
  expect_error(Mvedet(X, h = 2), "h must exceed p")
  expect_error(Mvedet(X, h = 9), "cannot exceed")
  expect_error(Mvedet(matrix(numeric(0), 0, 2)), "X is empty")
})

test_that("Mvecv regresses y on X through the MVE scatter", {
  set.seed(4)
  x <- rnorm(9)
  y <- 1 + 2 * x + rnorm(9, sd = 0.3)
  y[9] <- 30
  r <- Mvecv(y, x)
  m <- Mvedet(cbind(x, y))
  expect_equal(r$coef, m$cov[1, 2] / m$cov[1, 1], tolerance = 1e-10)
  expect_equal(r$intercept, m$center[2] - r$coef * m$center[1], tolerance = 1e-10)
  expect_equal(r$estimate, m$estimate, tolerance = 1e-12)
  expect_error(Mvecv(y, x[-1]), "one row per response")
  expect_error(Mvecv(numeric(0), matrix(numeric(0), 0, 1)), "y is empty")
})

test_that("mwem replays the multiplicative-weights updates", {
  B <- c(10, 4, 0, 6)
  Q <- rbind(c(1, 1, 0, 0), c(0, 1, 1, 0), c(1, 0, 0, 1))
  replay <- function(Tn, eps, gum = NULL, lap = NULL) {
    n <- sum(B)
    qb <- as.numeric(Q %*% B)
    A <- rep(n / 4, 4)
    acc <- 0
    sel <- integer(0)
    for (i in seq_len(Tn)) {
      qa <- as.numeric(Q %*% A)
      u <- eps / (2 * Tn) * abs(qa - qb) / 2
      if (!is.null(gum)) u <- u + gum[i, ]
      k <- which.max(u)
      sel <- c(sel, k - 1L)
      m <- qb[k] + if (is.null(lap)) 0 else 2 * Tn / eps * lap[i]
      A <- A * exp(Q[k, ] * (m - qa[k]) / (2 * n))
      A <- A * n / sum(A)
      acc <- acc + A
    }
    out <- if (Tn > 0) acc / Tn else A
    list(A = out, sel = sel, err = abs(as.numeric(Q %*% out) - qb))
  }
  r <- mwem(B, Q, eps = 1, T = 5)
  e <- replay(5, 1)
  expect_equal(r$A, e$A, tolerance = 1e-12)
  expect_equal(r$selected, e$sel)
  expect_equal(c(r$maxerr, r$meanerr), c(max(e$err), mean(e$err)), tolerance = 1e-12)
  gum <- matrix(c(0.3, -0.1, 0.5, 0.2, 0.9, -0.4, 0, 0.1, 0.2), 3, byrow = TRUE)
  lap <- c(0.2, -0.5, 1)
  rn <- morie_mwem(B, Q, eps = 2, T = 3, gumbel = gum, lap = lap)
  en <- replay(3, 2, gum, lap)
  expect_equal(rn$A, en$A, tolerance = 1e-12)
  expect_equal(rn$selected, en$sel)
  expect_equal(mwem(B, Q, T = 0)$A, rep(5, 4))
  expect_same_function(morie_mwem, mwem)
})

test_that("Gxeblup is the GLS BLUE/BLUP of the GxE model", {
  set.seed(5)
  y <- rnorm(6, 10)
  xe <- rep(0:1, each = 3)
  ZL <- kronecker(matrix(1, 2, 1), diag(3))
  ZEL <- diag(6)
  G <- matrix(c(1, 0.3, 0.1, 0.3, 1, 0.2, 0.1, 0.2, 1), 3)
  SE <- matrix(c(0.8, 0.2, 0.2, 0.5), 2)
  gls <- function(X) {
    Sg <- rbind(cbind(0.7 * G, matrix(0, 3, 6)), cbind(matrix(0, 6, 3), kronecker(SE, G)))
    Z <- cbind(ZL, ZEL)
    Vi <- solve(Z %*% Sg %*% t(Z) + diag(0.5, 6))
    b <- solve(t(X) %*% Vi %*% X, t(X) %*% Vi %*% y)
    list(b = as.numeric(b), u = as.numeric(Sg %*% t(Z) %*% Vi %*% (y - X %*% b)))
  }
  r <- Gxeblup(y, xe, ZL, ZEL, G, 0.7, SE, 0.5)
  e <- gls(cbind(1, xe))
  expect_equal(r$beta, e$b, tolerance = 1e-9)
  expect_equal(c(r$b_lines, r$b_gxe), e$u, tolerance = 1e-9)
  r0 <- Gxeblup(y, NULL, ZL, ZEL, G, 0.7, SE, 0.5)
  expect_equal(r0$beta, gls(matrix(1, 6, 1))$b, tolerance = 1e-9)
})

test_that("morie_mxetA replays the de Haan spectral construction", {
  F <- rbind(c(1, 0.2, 0.5, 0), c(0.3, 1, 0.1, 0.6), c(0, 0.4, 1, 0.2))
  r <- morie_mxetA(F, n_sim = 2, seed = 7)
  u <- .ghc_unif(.ghc_rng(7), 20000)
  k <- 0
  nxt <- function() {
    k <<- k + 1
    u[k]
  }
  for (s in 1:2) {
    y <- numeric(3)
    g <- 0
    np <- 0
    repeat {
      v <- nxt()
      while (v <= 0) v <- nxt()
      g <- g - log(v)
      if (min(y) > 0 && max(F) / g <= min(y)) break
      site <- min(floor(nxt() * 4), 3)
      y <- pmax(y, F[, site + 1] / g)
      np <- np + 1
    }
    expect_equal(r$fields[s, ], y, tolerance = 1e-12)
    expect_equal(r$n_points[s], np)
  }
  expect_equal(r$scales, rowMeans(F), tolerance = 1e-12)
  expect_equal(r$frechet_uniform, exp(-t(rowMeans(F) / t(r$fields))), tolerance = 1e-12)
  one <- morie_mxetA(matrix(c(1, 0.5), 1), n_sim = 3, seed = 2)
  expect_equal(dim(one$frechet_uniform), c(3, 1))
  expect_error(morie_mxetA(-F), "non-negative")
  expect_error(morie_mxetA(0 * F), "identically zero")
  expect_error(morie_mxetA(matrix(numeric(0), 0, 2)), "rectangular")
})

test_that("Naday and Nadwat are Gaussian-kernel local averages", {
  set.seed(6)
  x <- runif(25, 0, 3)
  y <- sin(x) + rnorm(25, sd = 0.2)
  g <- c(0.5, 1.5, 2.5)
  r <- Naday(x, y, h = 0.4, grid = g)
  W <- dnorm(outer(g, x, "-") / 0.4)
  m <- as.numeric(W %*% y / rowSums(W))
  expect_equal(r$estimate, m, tolerance = 1e-12)
  s2 <- rowSums(W * (outer(rep(1, 3), y) - m)^2) / rowSums(W)
  f <- rowSums(W) / (25 * 0.4)
  expect_equal(r$se, sqrt(s2 / (2 * sqrt(pi)) / (25 * 0.4 * f)), tolerance = 1e-10)
  hs <- 1.06 * min(sd(x), IQR(x) / 1.349) * 25^(-1 / 5)
  expect_equal(Naday(x, y)$bandwidth, hs, tolerance = 1e-12)
  expect_true(is.na(Naday(1, 2)$estimate))
  X2 <- cbind(x, y^2)
  z <- c(1.2, 0.5)
  w <- exp(-rowSums(sweep(X2, 2, z)^2) / (2 * 0.3)) / 0.3
  n0 <- Nadwat(X2, y, z, 0.3)
  expect_equal(n0$fit, sum(w * y) / sum(w), tolerance = 1e-12)
  expect_equal(n0$weights, w, tolerance = 1e-12)
  n1 <- Nadwat(X2, y, z, 0.3, degree = 1)
  ll <- lm(y ~ I(x - 1.2) + I(y^2 - 0.5), weights = w)
  expect_equal(n1$coef, unname(coef(ll)), tolerance = 1e-8)
  expect_error(Nadwat(X2, y, z, 0), "lambda must be positive")
  expect_error(Nadwat(X2, y, z, 1, degree = 2), "degree must be 0 or 1")
  expect_error(Nadwat(X2, y, 1, 1), "one entry per column")
  expect_error(Nadwat(X2, y[-1], z, 1), "same number of rows")
})

test_that("Nbdsp reaches the NB2 score equation at its moment dispersion", {
  set.seed(7)
  x <- rnorm(60)
  mu <- exp(0.5 + 0.6 * x)
  y <- rnbinom(60, size = 2, mu = mu)
  X <- cbind(1, x)
  r <- Nbdsp(y, X)
  mh <- exp(as.numeric(X %*% r$beta))
  expect_equal(r$mu_hat, mh, tolerance = 1e-12)
  expect_equal(r$r_hat, sum(mh^2) / sum((y - mh)^2 - mh), tolerance = 1e-10)
  f <- glm(y ~ x, family = MASS::negative.binomial(r$r_hat), control = glm.control(epsilon = 1e-14, maxit = 100))
  expect_equal(r$beta, unname(coef(f)), tolerance = 1e-8)
  yu <- c(2, 2, 3, 3, 2, 3)
  u <- Nbdsp(yu, matrix(1, 6, 1))
  expect_equal(u$r_hat, Inf)
  expect_equal(u$beta, log(mean(yu)), tolerance = 1e-10)
  expect_error(Nbdsp(c(1, 2.5), cbind(1, 1:2)), "non-negative counts")
  expect_error(Nbdsp(y, X, link = "identity"), "only the log link")
  expect_error(Nbdsp(y, X[-1, ]), "different number of rows")
})

test_that("Ndcg is DCG over ideal DCG", {
  dcg <- function(g, k) {
    g <- g[seq_len(min(k, length(g)))]
    sum((2^g - 1) / log2(seq_along(g) + 1))
  }
  pr <- c("a", "b", "c", "d", "e")
  r <- Ndcg(pr, c("c", "a", "z"), 3)
  g <- c(1, 0, 1, 0, 0)
  expect_equal(r$estimate, dcg(g, 3) / dcg(sort(g, TRUE), 3), tolerance = 1e-12)
  gr <- Ndcg(pr, c(a = 3, c = 1, e = 2), 10)
  g2 <- c(3, 0, 1, 0, 2)
  expect_equal(gr$dcg, dcg(g2, 10), tolerance = 1e-12)
  expect_equal(gr$estimate, dcg(g2, 10) / dcg(sort(g2, TRUE), 10), tolerance = 1e-12)
  expect_error(Ndcg(pr, "z", 3), "IDCG is 0")
  expect_error(Ndcg(pr, "a", 0), "k must be positive")
  expect_error(Ndcg(pr, c(a = -1), 2), "non-negative")
  expect_error(Ndcg(character(0), "a", 2), "empty")
})

test_that("Nde and Negctc follow their regressions", {
  set.seed(8)
  n <- 40
  x <- rbinom(n, 1, 0.5)
  m <- 0.5 + 0.8 * x + rnorm(n)
  y <- 1 + 0.3 * x + 0.6 * m + 0.2 * x * m + rnorm(n)
  r <- Nde(x, m, y)
  b <- coef(lm(m ~ x))
  cf <- coef(lm(y ~ x + m + x:m))
  expect_equal(r$nde, cf[["x"]] + cf[["x:m"]] * b[[1]], tolerance = 1e-10)
  expect_equal(r$nie, b[["x"]] * (cf[["m"]] + cf[["x:m"]]), tolerance = 1e-10)
  expect_equal(r$total, r$nde + r$nie, tolerance = 1e-12)
  expect_true(is.nan(r$se))
  expect_error(Nde(1:3, 1:3, 1:3), "at least 4")
  expect_error(Nde(x, m[-1], y), "same length")
  expect_error(Nde(numeric(0), numeric(0), numeric(0)), "empty")
  cv <- cbind(rnorm(n), rnorm(n))
  yn <- 0.4 * cv[, 1] + 0.5 * x + rnorm(n)
  ng <- Negctc(yn, x, cv, alpha = 0.1)
  sm <- summary(lm(yn ~ x + cv))$coefficients
  expect_equal(c(ng$estimate, ng$se), unname(sm["x", 1:2]), tolerance = 1e-10)
  expect_equal(ng$p_value, 2 * pnorm(-abs(sm["x", 1] / sm["x", 2])), tolerance = 1e-10)
  expect_equal(ng$confounding_suspected, as.numeric(ng$p_value < 0.1))
  expect_equal(c(ng$ci_lower, ng$ci_upper), sm["x", 1] + c(-1, 1) * qnorm(0.95) * sm["x", 2], tolerance = 1e-10)
  n0 <- Negctc(yn, x)
  expect_equal(n0$estimate, coef(lm(yn ~ x))[["x"]], tolerance = 1e-10)
  expect_equal(Negctc(yn, x, t(cv))$estimate, ng$estimate, tolerance = 1e-12)
  expect_error(Negctc(yn, x, alpha = 1), "alpha must lie")
  expect_error(Negctc(yn, x[-1]), "same length")
  expect_error(Negctc(yn[1:2], x[1:2]), "more observations")
  expect_error(Negctc(yn, x, cv[-1, ]), "one row per observation")
})
