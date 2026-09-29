# Coverage for Sstrlf .. SumInts exports. Every expectation is recomputed in
# the test body.

test_that("Sstrlf is the delayed-entry Kaplan-Meier estimate", {
  skip_if_not_installed("survival")
  en <- c(0, 1, 0, 2, 1, 0, 3, 0)
  tm <- c(4, 5, 3, 7, 6, 2, 8, 9)
  ev <- c(1, 0, 1, 1, 1, 0, 1, 1)
  r <- Sstrlf(en, tm, ev)
  f <- survival::survfit(survival::Surv(en, tm, ev) ~ 1)
  ev_t <- f$time[f$n.event > 0]
  expect_equal(r$times, ev_t)
  expect_equal(r$survival, f$surv[f$n.event > 0], tolerance = 1e-12)
  expect_equal(r$estimate, utils::tail(f$surv[f$n.event > 0], 1), tolerance = 1e-12)
  expect_true(is.nan(Sstrlf(0, 1, 0)$estimate))
})

test_that("morie_stahdo weights by Stahel-Donoho outlyingness", {
  X <- cbind(c(0.1, 1.2, -0.4, 0.8, 2.9, 0.3), c(0.5, -0.2, 0.9, 1.1, -2.5, 0.0))
  r <- morie_stahdo(X)
  dirs <- list()
  for (i in 1:5) for (j in (i + 1):6) {
    v <- X[j, ] - X[i, ]
    dirs[[length(dirs) + 1]] <- c(-v[2], v[1]) / sqrt(sum(v^2))
  }
  o <- rep(0, 6)
  for (a in dirs) {
    pr <- as.numeric(X %*% a)
    s <- stats::mad(pr)
    if (s > 1e-12) o <- pmax(o, abs(pr - stats::median(pr)) / s)
  }
  expect_equal(r$outlyingness, o, tolerance = 1e-10)
  expect_true(r$exhaustive)
  cc <- sqrt(stats::qchisq(0.5, 2))
  expect_equal(r$cutoff, cc, tolerance = 1e-10)
  w <- ifelse(o <= cc, 1, (cc / o)^2)
  expect_equal(r$weights, w, tolerance = 1e-10)
  loc <- colSums(w * X) / sum(w)
  expect_equal(r$location, loc, tolerance = 1e-10)
  Xc <- sweep(X, 2, loc)
  expect_equal(r$scatter, crossprod(Xc * w, Xc) / sum(w), tolerance = 1e-10)
  u <- morie_stahdo(X[, 1, drop = FALSE], cutoff = 1.5)
  o1 <- abs(X[, 1] - stats::median(X[, 1])) / stats::mad(X[, 1])
  expect_equal(u$outlyingness, o1, tolerance = 1e-12)
  expect_equal(u$weights, ifelse(o1 <= 1.5, 1, (1.5 / o1)^2), tolerance = 1e-12)
  rd <- morie_stahdo(X, directions = "random", n_directions = 4, seed = 3)
  e <- .ghc_rng(3L)
  o2 <- rep(0, 6)
  k <- 0
  while (k < 4) {
    v <- .ghc_unif(e, 2) * 2 - 1
    if (sqrt(sum(v^2)) > 1e-9) {
      k <- k + 1
      pr <- as.numeric(X %*% (v / sqrt(sum(v^2))))
      o2 <- pmax(o2, abs(pr - stats::median(pr)) / stats::mad(pr))
    }
  }
  expect_equal(rd$outlyingness, o2, tolerance = 1e-10)
  expect_error(morie_stahdo(X, directions = "grid"), "directions must be one of")
  expect_error(morie_stahdo(X[1:2, ]), "at least three")
  expect_error(morie_stahdo(X, cutoff = -1), "cutoff must be positive")
})

test_that("stars and bars counts and the triangular sums", {
  s <- StarBars(4, 3)
  expect_equal(s$count, choose(6, 2))
  expect_equal(s$count_alt, choose(6, 4))
  b <- StarBRec(4, 3)
  expect_equal(b$recursion_sum, sum(choose(0:4 + 1, 1)))
  expect_equal(b$closed_form, choose(6, 2))
  expect_equal(b$n_terms, 5)
  expect_error(StarBRec(3, 1), "N >= 2")
  expect_error(StarBRec(-1, 3), "non-negative")
  si <- SumInts(10)
  expect_equal(si$explicit_sum, sum(1:10))
  expect_equal(si$next_closed_form, sum(1:11))
  expect_equal(SumInts(0)$explicit_sum, 0)
  expect_error(SumInts(-1), "non-negative")
})

test_that("jarque_bera, kruskal_wallis and shapiro_wilk", {
  x <- c(2.1, 3.4, 1.8, 5.9, 2.2, 3.1, 4.4, 2.8, 9.5, 3.0)
  j <- jarque_bera(x)
  m <- mean(x)
  s <- stats::sd(x)
  sk <- mean((x - m)^3) / s^3
  ku <- mean((x - m)^4) / s^4 - 3
  jb <- 10 / 6 * (sk^2 + ku^2 / 4)
  expect_equal(j$test_statistic, jb, tolerance = 1e-12)
  expect_equal(j$p_value, stats::pchisq(jb, 2, lower.tail = FALSE), tolerance = 1e-12)
  expect_equal(jarque_bera(rep(1, 4))$p_value, 1)
  g1 <- c(1.2, 3.4, 2.2, NA)
  g2 <- c(4.5, 5.1, 3.9, 6.0)
  g3 <- c(2.0, 2.8, 1.1)
  k <- kruskal_wallis(g1, g2, g3)
  kt <- stats::kruskal.test(list(g1[1:3], g2, g3))
  expect_equal(k$test_statistic, unname(kt$statistic), tolerance = 1e-12)
  expect_equal(k$p_value, kt$p.value, tolerance = 1e-12)
  expect_equal(k$effect_size, max((unname(kt$statistic) - 2) / (10 - 3), 0), tolerance = 1e-12)
  expect_equal(k$n, 10L)
  sw <- shapiro_wilk(x)
  st <- stats::shapiro.test(x)
  expect_equal(sw$test_statistic, unname(st$statistic), tolerance = 1e-12)
  expect_equal(sw$p_value, st$p.value, tolerance = 1e-12)
  expect_equal(shapiro_wilk(c(x, NA, Inf))$n, 10L)
})

test_that("Stefanbz uses the exact SI Stefan-Boltzmann constant", {
  r <- Stefanbz(c(300, 5772), emissivity = 0.9)
  # CODATA 2018 value of sigma, exact since the 2019 SI redefinition
  expect_equal(r$sigma, 5.670374419e-8, tolerance = 1e-9)
  expect_equal(r$exitance, 0.9 * r$sigma * c(300, 5772)^4, tolerance = 1e-12)
  expect_equal(r$total, sum(r$exitance), tolerance = 1e-12)
  expect_true(is.nan(Stefanbz(numeric(0))$estimate))
})

test_that("morie_stl_decompose matches stats::stl", {
  x <- 10 + 0.3 * (1:36) + 2 * sin(2 * pi * (1:36) / 6) + c(0.4, -0.2, 0.1, 0.5, -0.6, 0.3, -0.1, 0.2, -0.4, 0.6, 0.0, -0.3)
  # s_window below the 6 cycles per subseries: for q > n the Fortran widens
  # the span by (q - n) / 2 where Cleveland et al. (1990) scale it by q / n
  r <- morie_stl_decompose(x, 6, s_window = 5)
  s <- stats::stl(stats::ts(x, frequency = 6), s.window = 5, t.window = r$t_window, l.window = r$l_window,
                  s.degree = 1, s.jump = 1, t.jump = 1, l.jump = 1, inner = 2, outer = 0)
  expect_equal(r$seasonal, as.numeric(s$time.series[, "seasonal"]), tolerance = 1e-10)
  expect_equal(r$trend, as.numeric(s$time.series[, "trend"]), tolerance = 1e-10)
  expect_equal(r$seasonal + r$trend + r$remainder, x, tolerance = 1e-12)
  a <- morie_stlAn(replace(x, 20, x[20] + 8), 6, k = 3)
  R <- a$remainder
  sig <- 1.4826 * stats::median(abs(R - stats::median(R)))
  expect_equal(a$sigma_hat, sig, tolerance = 1e-12)
  expect_equal(a$outliers, as.numeric(which(abs(R - stats::median(R)) > 3 * sig)))
  expect_true(20 %in% a$outliers)
  expect_error(morie_stl_decompose(x[1:10], 6), "two full cycles")
})

test_that("Stratmean and Stratdes follow Cochran's stratified formulas", {
  y <- c(3, 5, 4, 10, 12, 11, 13, 7, 8)
  h <- c(1, 1, 1, 2, 2, 2, 2, 3, 3)
  Nh <- c(30, 50, 20)
  r <- Stratmean(y, h, Nh, level = 0.9)
  W <- Nh / 100
  mh <- tapply(y, h, mean)
  vh <- (Nh - c(3, 4, 2)) / Nh * tapply(y, h, stats::var) / c(3, 4, 2)
  est <- sum(W * mh)
  se <- sqrt(sum(W^2 * vh))
  expect_equal(r$estimate, est, tolerance = 1e-12)
  expect_equal(r$se, se, tolerance = 1e-12)
  expect_equal(r$ci_upper, est + stats::qnorm(0.95) * se, tolerance = 1e-12)
  expect_error(Stratmean(y, h, c(30, 50)), "one-based stratum labels")
  expect_error(Stratmean(y[-9], h[-9], Nh), "at least two")
  d <- Stratdes(c(100, 200, 50), c(4, 2, 10), n = 20)
  w <- c(400, 400, 500) / 1300
  expect_equal(d$nh_exact, 20 * w, tolerance = 1e-12)
  expect_equal(sum(d$nh), 20L)
  # largest-remainder rounding of 6.15, 6.15, 7.69
  expect_equal(d$nh, c(6L, 6L, 8L))
  Wh <- c(100, 200, 50) / 350
  expect_equal(d$variance, sum(Wh^2 * (1 - d$nh / c(100, 200, 50)) * c(16, 4, 100) / d$nh), tolerance = 1e-12)
  p <- Stratdes(c(100, 200, 50), c(4, 2, 10), n = 7, kind = "prop")
  expect_equal(p$nh, c(2L, 4L, 1L))
  cst <- Stratdes(c(100, 200, 50), c(4, 2, 10), n = 20, Ch = c(1, 4, 25), kind = "cost")
  expect_equal(cst$weights, c(400, 200, 100) / 700, tolerance = 1e-12)
  expect_error(Stratdes(c(100, 200), c(1, 1), n = 5, kind = "cost"), "needs the per-unit costs")
  expect_error(Stratdes(c(100, 200), c(1, 1), n = 1), "at least the number of strata")
})

test_that("Strdis is the DeltaCon-0 distance", {
  A <- rbind(c(0, 1, 1, 0), c(1, 0, 1, 0), c(1, 1, 0, 1), c(0, 0, 1, 0))
  B <- A
  B[1, 2] <- B[2, 1] <- 0
  r <- Strdis(A, B)
  eps <- 1 / (1 + 3)
  S <- function(G) solve(diag(4) + eps^2 * diag(rowSums(G)) - eps * G)
  d <- sqrt(sum((sqrt(abs(S(A))) - sqrt(abs(S(B))))^2))
  expect_equal(r$distance, d, tolerance = 1e-12)
  expect_equal(r$similarity, 1 / (1 + d), tolerance = 1e-12)
  expect_equal(Strdis(A, A, eps = 0.1)$distance, 0)
  expect_same_function(structural_distance, Strdis)
  expect_error(Strdis(A, B[1:3, 1:3]), "equal size")
})

test_that("StripMean, SumDens and SumDensP", {
  s <- StripMean(0.6, 2, 4, 3)
  expect_equal(s$slope, 0.6 * 2 / 4, tolerance = 1e-12)
  expect_equal(s$x, 0.3 * 3, tolerance = 1e-12)
  expect_error(StripMean(0.6, 2, 4, c(1, 2)), "single value")
  g <- seq(-6, 6, by = 0.01)
  r <- SumDens(g, stats::dnorm(g), g, stats::dnorm(g), 0.7)
  vals <- stats::dnorm(g) * stats::approx(g, stats::dnorm(g), xout = 0.7 - g, yleft = 0, yright = 0)$y
  expect_equal(r$density, sum(diff(g) * (vals[-1] + vals[-length(vals)]) / 2), tolerance = 1e-12)
  # the grid resolves the N(0, 2) convolution to O(step^2)
  expect_equal(r$density, stats::dnorm(0.7, sd = sqrt(2)), tolerance = 1e-4)
  p <- SumDensP(g, stats::dnorm(g), g, stats::dnorm(g), 0.7, 0.01)
  expect_equal(p$probability, 0.01 * r$density, tolerance = 1e-12)
  expect_error(SumDens(1, 1, g, g, 0), "n >= 2")
  expect_error(SumDensP(g, g, g, g, 0, NA), "dz must be")
})

test_that("Strtwt forms stabilised inverse probability weights", {
  a <- c(1, 0, 1, 1, 0, 0, 1, 0, 1, 0)
  S <- c(0.2, 1.1, -0.5, 0.8, 0.3, -1.2, 1.5, 0.1, -0.3, 0.6)
  H <- c(1, 0, 0, 1, 1, 0, 1, 0, 1, 1)
  r <- Strtwt(a, H = H, S = S)
  pn <- stats::fitted(stats::glm(a ~ S, family = stats::binomial(), control = list(epsilon = 1e-14)))
  pd <- stats::fitted(stats::glm(a ~ S + H, family = stats::binomial(), control = list(epsilon = 1e-14)))
  num <- ifelse(a == 1, pn, 1 - pn)
  den <- ifelse(a == 1, pd, 1 - pd)
  expect_equal(r$weights, unname(num / den), tolerance = 1e-9)
  expect_equal(r$unstabilized, unname(1 / den), tolerance = 1e-9)
  expect_equal(r$estimate, mean(num / den), tolerance = 1e-9)
  expect_equal(r$sd, stats::sd(num / den), tolerance = 1e-9)
  m <- Strtwt(a)
  expect_equal(m$weights, rep(1, 10), tolerance = 1e-12)
  expect_error(Strtwt(c(0, 2)), "coded 0/1")
  expect_error(Strtwt(a, H = 1:3), "wrong number of rows")
})

test_that("Studrs gives externally studentised residuals", {
  X <- cbind(1, c(0.5, 1.2, 2.2, 3.1, 4.0, 5.3, 6.1))
  y <- c(1.1, 2.0, 2.4, 4.5, 4.1, 5.9, 9.0)
  r <- Studrs(y, X)
  f <- stats::lm(y ~ X[, 2])
  t_ <- unname(stats::rstudent(f))
  expect_equal(r$t, t_, tolerance = 1e-10)
  expect_equal(r$leverage, unname(stats::hatvalues(f)), tolerance = 1e-10)
  expect_equal(r$estimate, t_[which.max(abs(t_))], tolerance = 1e-10)
  expect_equal(r$df, 4)
})
