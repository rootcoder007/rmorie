# Coverage for the extreme-value files (ev*.R, esd.R, evt_coles.R):
# expected values come from evd / goftest where a reference exists and
# from Coles (2001) formulas otherwise.

ev_x <- c(2.3, 0.4, 5.1, 1.8, 3.9, 0.9, 7.2, 2.6, 1.1, 4.4, 0.7, 6.3, 3.1,
          1.5, 9.8, 2.2, 0.3, 4.9, 1.9, 3.6, 12.4, 2.8, 0.6, 5.7)

test_that("GEV and GPD cdfs and Anderson-Darling statistics", {
  skip_if_not_installed("evd")
  skip_if_not_installed("goftest")
  z <- c(-1, 0.2, 1.5, 4)
  expect_equal(Gevcdf(z, 0.5, 1.3, 0.2), evd::pgev(z, 0.5, 1.3, 0.2), tolerance = 1e-12)
  expect_equal(Gevcdf(z, 0.5, 1.3, 0), evd::pgev(z, 0.5, 1.3, 0), tolerance = 1e-12)
  expect_equal(Gevcdf(z, 0.5, 1.3, -0.4), evd::pgev(z, 0.5, 1.3, -0.4), tolerance = 1e-12)
  expect_error(Gevcdf(1, 0, 0), "positive")
  y <- c(0.2, 1, 3.5, 7)
  expect_equal(Gpdcdf(y, 2, 0.3), evd::pgpd(y, 0, 2, 0.3), tolerance = 1e-12)
  expect_equal(Gpdcdf(y, 2, 0), evd::pgpd(y, 0, 2, 0), tolerance = 1e-12)
  expect_equal(Gpdcdf(c(-1, y), 2, -0.5), c(0, evd::pgpd(y, 0, 2, -0.5)), tolerance = 1e-12)
  expect_error(Gpdcdf(1, 0), "positive")
  a <- Adgev(ev_x, 2, 2, 0.2)
  ref <- goftest::ad.test(ev_x, null = evd::pgev, loc = 2, scale = 2, shape = 0.2,
                          estimated = FALSE)
  expect_equal(a$statistic, unname(ref$statistic), tolerance = 1e-10)
  g <- Adgpd(ev_x, 3, 0.1)
  refg <- goftest::ad.test(ev_x, null = evd::pgpd, loc = 0, scale = 3, shape = 0.1,
                           estimated = FALSE)
  expect_equal(g$statistic, unname(refg$statistic), tolerance = 1e-10)
  expect_error(Adgpd(c(1, -1)), "positive")
  expect_equal(morie_evt_gev_logpdf(z, 0.5, 1.3, 0.2), evd::dgev(z, 0.5, 1.3, 0.2, log = TRUE),
               tolerance = 1e-12)
  expect_equal(morie_evt_gev_logpdf(z, 0.5, 1.3, 0), evd::dgev(z, 0.5, 1.3, 0, log = TRUE),
               tolerance = 1e-12)
  expect_equal(morie_evt_gev_logpdf(z, 0, -1, 0), rep(-Inf, 4))
})

test_that("MASS_ginv_fallback is the Moore-Penrose inverse of a PSD matrix", {
  v <- c(1, 2, -1)
  H <- outer(v, v) + outer(c(0, 1, 1), c(0, 1, 1))
  G <- MASS_ginv_fallback(H)
  expect_equal(H %*% G %*% H, H, tolerance = 1e-10)
  expect_equal(G %*% H %*% G, G, tolerance = 1e-10)
  skip_if_not_installed("MASS")
  expect_equal(G, MASS::ginv(H), tolerance = 1e-10)
})

test_that("Evpot maximises the GPD likelihood", {
  r <- Evpot(ev_x, u = 1.5)
  y <- ev_x[ev_x > 1.5] - 1.5
  nll <- function(p) {
    s <- p[1]
    xi <- p[2]
    if (s <= 0 || any(1 + xi * y / s <= 0)) return(1e10)
    length(y) * log(s) + (1 / xi + 1) * sum(log(1 + xi * y / s))
  }
  expect_equal(r$nll, nll(c(r$sigma, r$xi)), tolerance = 1e-12)
  o <- stats::optim(c(r$sigma, r$xi), nll, control = list(reltol = 1e-15, maxit = 5000))
  # the golden-section profile search leaves < 1e-9 on the table
  expect_lt(r$nll - o$value, 1e-9)
  expect_equal(r$zeta_u, mean(ev_x > 1.5), tolerance = 1e-12)
  expect_equal(r$modified_scale, r$sigma - 1.5 * r$xi, tolerance = 1e-12)
  expect_error(Evpot(numeric(0), 0), "empty")
  expect_error(Evpot(ev_x, 20), "fewer than two")
})

test_that("Evtsthr scores windows of GPD fits", {
  grid <- c(0.5, 1, 1.5, 2, 2.5)
  r <- Evtsthr(ev_x, u_grid = grid, window = 3)
  fits <- lapply(grid, function(u) Evpot(ev_x, u))
  xi <- vapply(fits, `[[`, 0, "xi")
  ms <- vapply(fits, `[[`, 0, "modified_scale")
  sc <- vapply(1:3, function(i) {
    w <- i:(i + 2)
    if (any(ms[w] <= 0)) Inf else var(log(ms[w])) + var(xi[w])
  }, 0)
  expect_equal(r$scores, sc, tolerance = 1e-12)
  expect_equal(r$u_star, grid[which.min(sc)])
  expect_error(Evtsthr(1:5), "ten")
  expect_error(Evtsthr(ev_x, window = 1), "at least 2")
  expect_error(Evtsthr(ev_x, u_grid = 1, window = 2), "shorter")
  expect_error(Evtsthr(ev_x, u_grid = c(9, 10, 11)), "too few")
})

test_that("Evespot and Evvarpot use the GPD tail", {
  e <- Evespot(u = 2, sigma = 1.5, xi = 0.2, VaR = 6)
  expect_equal(e$ES, (6 + 1.5 - 0.4) / 0.8, tolerance = 1e-12)
  expect_error(Evespot(2, 0, 0.2, 6), "positive")
  expect_error(Evespot(2, 1, 1, 6), "infinite")
  v <- Evvarpot(u = 2, sigma = 1.5, xi = 0.2, zeta_u = 0.1, p = 0.99)
  var <- 2 + 1.5 / 0.2 * ((0.01 / 0.1)^-0.2 - 1)
  expect_equal(v$VaR, var, tolerance = 1e-12)
  expect_equal(v$tail_prob, 0.01, tolerance = 1e-12)
  v0 <- Evvarpot(2, 1.5, 0, 0.1, 0.99)
  expect_equal(v0$VaR, 2 + 1.5 * log(10), tolerance = 1e-12)
  expect_equal(v0$tail_prob, 0.01, tolerance = 1e-12)
  expect_error(Evvarpot(2, 0, 0, 0.1, 0.99), "positive")
  expect_error(Evvarpot(2, 1, 0, 0, 0.99), "zeta_u")
  expect_error(Evvarpot(2, 1, 0, 0.1, 1), "strictly")
  expect_error(Evvarpot(2, 1, 0, 0.1, 0.5), "below the threshold")
})

test_that("Evdec, Evmrlp and Evtrecords", {
  x <- c(1, 5, 6, 1, 1, 7, 1, 1, 1, 8, 2, 9)
  r <- Evdec(x, u = 4, r = 2)
  expect_equal(r$cluster_id, c(0, 1, 1, 0, 0, 1, 0, 0, 0, 2, 0, 2))
  expect_equal(r$cluster_max, c(7, 9))
  expect_equal(r$theta, 2 / 5)
  expect_equal(Evdec(x, 4, 1)$n_clusters, 3L)
  expect_true(is.nan(Evdec(x, 100, 1)$theta))
  expect_error(Evdec(numeric(0), 1, 1), "empty")
  expect_error(Evdec(x, 1, 0), "at least 1")
  m <- Evmrlp(ev_x, u_grid = c(0.5, 1, 2, 3, 4))
  eu <- vapply(c(0.5, 1, 2, 3, 4), function(u) mean(ev_x[ev_x > u] - u), 0)
  expect_equal(m$e_u, eu, tolerance = 1e-12)
  ex <- ev_x[ev_x > 2] - 2
  expect_equal(m$se[3], sd(ex) / sqrt(length(ex)), tolerance = 1e-12)
  expect_equal(m$estimate, unname(coef(lm(eu ~ c(0.5, 1, 2, 3, 4)))[2]), tolerance = 1e-12)
  expect_length(Evmrlp(ev_x)$u, 20L)
  expect_error(Evmrlp(1), "two")
  expect_error(Evmrlp(ev_x, u_grid = numeric(0)), "empty")
  expect_error(Evmrlp(ev_x, u_grid = c(100, 200)), "grid too high")
  rc <- Evtrecords(c(3, 1, 4, 1, 5, 9, 2, 6))
  expect_equal(rc$times, c(0L, 2L, 4L, 5L))
  expect_equal(rc$expected, sum(1 / 1:8), tolerance = 1e-12)
  expect_equal(rc$z, (4 - sum(1 / 1:8)) / sqrt(sum(1 / 1:8 - 1 / (1:8)^2)), tolerance = 1e-12)
  expect_true(is.na(Evtrecords(1)$z))
  expect_error(Evtrecords(numeric(0)), "at least one")
})

test_that("bivariate max-stable models match evd", {
  skip_if_not_installed("evd")
  xs <- c(0.5, 1.2, 3)
  ys <- c(2, 0.8, 1.5)
  h <- Evhrid(xs, ys, lam = 0.7)
  expect_equal(h$F, vapply(1:3, function(i) evd::pbvevd(c(1 / xs[i], 1 / ys[i]),
    dep = 1 / 0.7, model = "hr", mar1 = c(1, 1, 1), mar2 = c(1, 1, 1)), 0), tolerance = 1e-10)
  expect_equal(h$chi, 2 - 2 * pnorm(0.7), tolerance = 1e-12)
  expect_error(Evhrid(numeric(0), 1, 1), "empty")
  expect_error(Evhrid(1:2, 1, 1), "same length")
  expect_error(Evhrid(1, 1, 0), "strictly positive")
  expect_error(Evhrid(-1, 1, 1), "strictly positive")
  lg <- Evmsexp(xs, ys, alpha = 0.6)
  expect_equal(lg$F, vapply(1:3, function(i) evd::pbvevd(c(xs[i], ys[i]), dep = 0.6,
    model = "log", mar1 = c(1, 1, 1), mar2 = c(1, 1, 1)), 0), tolerance = 1e-10)
  expect_equal(lg$chi, 2 - 2^0.6, tolerance = 1e-12)
  expect_error(Evmsexp(numeric(0), 1, 1), "empty")
  expect_error(Evmsexp(1:2, 1, 1), "same length")
  expect_error(Evmsexp(1, 1, 0), "\\(0, 1\\]")
  expect_error(Evmsexp(-1, 1, 1), "Frechet")
})

test_that("empirical dependence: Evangia, Evpdfn", {
  X <- cbind(c(1, 5, 3, 9, 2, 7, 4, 8), c(2, 6, 1, 8, 3, 9, 5, 4))
  r <- Evangia(X, k = 3)
  f0 <- 9 / (9 - rank(X[, 1]))
  f1 <- 9 / (9 - rank(X[, 2]))
  rad <- f0 + f1
  top <- order(-rad)[1:3]
  expect_equal(r$atoms, sort(f0[top] / rad[top]), tolerance = 1e-12)
  expect_error(Evangia(X[0, ], 1), "no rows")
  expect_error(Evangia(cbind(X, 1), 1), "two columns")
  expect_error(Evangia(X, 9), "between 1")
  p <- Evpdfn(X[, 1], X[, 2], t_grid = c(0, 0.3, 0.5, 1), u = 0.5)
  ux <- rank(X[, 1]) / 9
  uy <- rank(X[, 2]) / 9
  A <- vapply(c(0, 0.3, 0.5, 1), function(t) {
    pr <- mean(ux <= 0.5^(1 - t) & uy <= 0.5^t)
    if (t <= 0 || t >= 1 || pr <= 0) 1 else min(max(-log(pr) / log(2), max(t, 1 - t)), 1)
  }, 0)
  expect_equal(p$A, A, tolerance = 1e-12)
  expect_equal(p$estimate, A[3], tolerance = 1e-12)
  expect_equal(Evpdfn(X[, 1], X[, 2], t_grid = c(0.2, 0.4))$estimate,
               mean(Evpdfn(X[, 1], X[, 2], t_grid = c(0.2, 0.4))$A), tolerance = 1e-12)
  expect_error(Evpdfn(numeric(0), numeric(0)), "empty")
  expect_error(Evpdfn(1:2, 1), "same length")
  expect_error(Evpdfn(1:3, 1:3, t_grid = 2), "\\[0, 1\\]")
  expect_error(Evpdfn(1:3, 1:3, u = 1), "strictly")
})

test_that("Evhpvr fits the Heffernan-Tawn profile likelihood", {
  X <- cbind(c(1.2, 2.5, 3.1, 0.4, 4.2, 5.5, 2.2, 6.1, 3.7, 7.3),
             c(0.8, 2.1, 2.2, 0.9, 3.5, 4.1, 1.5, 5.2, 3.1, 5.9))
  r <- Evhpvr(X, u = 1)
  keep <- X[, 1] > 1
  xv <- X[keep, 1]
  yv <- X[keep, 2]
  prof <- function(b) {
    u <- yv / xv^b
    v <- xv / xv^b
    a <- if (var(v) > 0) cov(u, v) / var(v) else 0
    m <- mean(u) - a * mean(v)
    s2 <- mean((u - a * v)^2) - m^2
    list(f = 0.5 * length(xv) * log(s2) + b * sum(log(xv)), a = a, m = m, s = sqrt(s2))
  }
  pr <- prof(r$b)
  expect_equal(r$a, pr$a, tolerance = 1e-10)
  expect_equal(c(r$mu_z, r$sigma_z), c(pr$m, pr$s), tolerance = 1e-10)
  expect_equal(r$nll, pr$f, tolerance = 1e-10)
  bs <- seq(0, 0.999, length.out = 2001)
  # the bisected stationary point is the grid minimiser to grid resolution
  expect_lt(r$nll, min(vapply(bs, function(b) prof(b)$f, 0)) + 1e-6)
  expect_error(Evhpvr(X[0, ], 1), "no rows")
  expect_error(Evhpvr(cbind(X, 1), 1), "two columns")
  expect_error(Evhpvr(X, 7), "fewer than three")
})

test_that("Evtlmom trimmed L-moments", {
  x <- ev_x[1:10]
  xs <- sort(x)
  n <- 10
  tl <- function(r, s, t) {
    m <- r + s + t
    sum(vapply(0:(r - 1), function(k) {
      (-1)^k * choose(r - 1, k) * sum(choose(0:(n - 1), r + s - k - 1) *
        choose((n - 1):0, t + k) * xs) / choose(n, m)
    }, 0)) / r
  }
  r <- Evtlmom(x, order = 3)
  expect_equal(r$lambda, c(tl(1, 0, 0), tl(2, 0, 0), tl(3, 0, 0)), tolerance = 1e-12)
  expect_equal(r$lambda[1], mean(x), tolerance = 1e-12)
  expect_equal(r$lambda[2], mean(abs(outer(x, x, "-"))[upper.tri(diag(n))]) / 2,
               tolerance = 1e-12)
  expect_equal(r$tau[3], r$lambda[3] / r$lambda[2], tolerance = 1e-12)
  t11 <- Evtlmom(x, s = 1, t = 1, order = 2)
  expect_equal(t11$lambda, c(tl(1, 1, 1), tl(2, 1, 1)), tolerance = 1e-12)
  expect_error(Evtlmom(numeric(0)), "empty")
  expect_error(Evtlmom(x, s = -1), "non-negative")
  expect_error(Evtlmom(x, order = 0), "at least 1")
  expect_error(Evtlmom(1:3, s = 2, t = 2), "too small")
})

test_that("Gesd applies Rosner's generalised ESD test", {
  x <- c(ev_x[1:20], 40, 35)
  r <- Gesd(x, alpha = 0.05, r = 4)
  n <- 22
  keep <- x
  R <- numeric(4)
  for (i in 1:4) {
    d <- abs(keep - mean(keep))
    R[i] <- max(d) / sd(keep)
    keep <- keep[-which.max(d)]
  }
  lam <- vapply(1:4, function(i) {
    tq <- qt(1 - 0.05 / (2 * (n - i + 1)), n - i - 1)
    (n - i) * tq / sqrt((n - i - 1 + tq^2) * (n - i + 1))
  }, 0)
  expect_equal(r$R, R, tolerance = 1e-12)
  expect_equal(r$lam, lam, tolerance = 1e-12)
  expect_equal(r$n_outliers, max(c(0L, which(R > lam))))
  expect_equal(r$outlier_index[1:2], c(21L, 22L))
  expect_equal(Gesd(x)$r, 2L)
  expect_error(Gesd(1:2), "n>=3")
})

test_that("Evstud matches the two-way fixed-effects event-study OLS", {
  unit <- rep(c("a", "b", "c", "d"), each = 5)
  time <- rep(1:5, 4)
  cohort <- rep(c(3, 4, Inf, 2), each = 5)
  y <- c(1.1, 1.3, 2.9, 3.2, 3.6, 0.8, 1, 1.2, 2.8, 3.1,
         0.5, 0.7, 0.6, 0.9, 1, 1.9, 3.4, 3.7, 3.9, 4.2)
  r <- Evstud(y, D = numeric(20), unit, time, cohort, max_lead = 1, max_lag = 1)
  ev <- ifelse(is.finite(cohort), time - cohort, NA)
  d0 <- as.numeric(!is.na(ev) & ev == 0)
  d1 <- as.numeric(!is.na(ev) & ev == 1)
  fit <- lm(y ~ factor(unit) + factor(time) + d0 + d1)
  cf <- summary(fit)$coefficients
  expect_equal(r$event_times, c(0L, 1L))
  expect_equal(r$coef, unname(cf[c("d0", "d1"), 1]), tolerance = 1e-9)
  expect_equal(r$se, unname(cf[c("d0", "d1"), 2]), tolerance = 1e-9)
  expect_equal(r$estimate, unname(cf["d0", 1]), tolerance = 1e-9)
  expect_error(Evstud(numeric(0), 0, "a", 1, 1), "empty")
  expect_error(Evstud(y, numeric(20), unit[-1], time, cohort), "equal length")
  expect_error(Evstud(y, numeric(20), rep("a", 20), time, cohort), "two units")
  expect_error(Evstud(y, numeric(20), unit, time, rep(Inf, 20)), "no event-time")
  expect_error(Evstud(y[1:8], numeric(8), unit[1:8], time[1:8], cohort[1:8]), "more parameters")
})
