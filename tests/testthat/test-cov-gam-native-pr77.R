# R/gam_native.R, the lines PR #77 added that no other test reached: the thin-plate constants for
# other orders and dimensions, Lanczos run to the end, the knot thinning of a large 2-d basis, the
# cubic basis outside its knots, a constraint on a basis whose linear column has mean zero, the
# fit's step-halving and fall-back steps, and the eigensolver, Davies and Liu tails, each against
# eigen(), mgcv or the closed form.

.gp <- function(name) get(name, envir = environment(morie_gam))

.tri <- function(d, e) {
  n <- length(d)
  T <- diag(d, n)
  if (n > 1L) {
    T[cbind(seq_len(n - 1L), 2:n)] <- e
    T[cbind(2:n, seq_len(n - 1L))] <- e
  }
  T
}

.eig_ok <- function(d, e) {
  r <- .gp(".gamn_dstedc")(d, e)
  T <- .tri(d, e)
  ev <- sort(eigen(T, symmetric = TRUE)$values)
  s <- max(abs(ev))
  if (s == 0) s <- 1
  expect_lt(max(abs(sort(r$values) - ev)) / s, 1e-12)
  expect_lt(max(abs(T %*% r$vectors - r$vectors %*% diag(r$values, length(d)))) / s, 1e-12)
  expect_lt(max(abs(crossprod(r$vectors) - diag(length(d)))), 1e-12)
  expect_false(is.unsorted(r$values))
}

test_that("thin-plate constants and radial functions match their closed forms for every order and dimension", {
  ec <- .gp(".gamn_eta_const")
  et <- .gp(".gamn_eta")
  for (md in list(c(3, 1), c(3, 2), c(4, 2), c(4, 3), c(5, 2), c(5, 4))) {
    m <- md[[1]]
    d <- md[[2]]
    want <- if (d %% 2 == 0) {
      (-1)^(m + 1 + d / 2) / (2^(2 * m - 1) * pi^(d / 2) * factorial(m - 1) * factorial(m - d / 2))
    } else {
      gamma(d / 2 - m) / (2^(2 * m) * pi^(d / 2) * factorial(m - 1))
    }
    expect_equal(ec(m, d), want, tolerance = 1e-13)
    r <- c(0, 0.3, 1.7, 4)
    got <- et(r^2, m, d, 1)
    ref <- if (d %% 2 == 0) ifelse(r > 0, r^(2 * m - d) * log(r), 0) else r^(2 * m - d)
    expect_equal(got, ref, tolerance = 1e-13)
  }
  # the general branch of the radial matrix: three covariates
  A <- matrix(c(0, 1, 0, 0, 1, 1, 0.5, 0, 2), 3, 3)
  E <- .gp(".gamn_tps_E")(A, A, 3L, 3L, ec(3, 3))
  r2 <- as.matrix(stats::dist(A))^2
  expect_equal(E, et(r2, 3L, 3L, ec(3, 3)), ignore_attr = TRUE)
})

test_that("Lanczos run to the full dimension returns the leading eigenpairs", {
  set.seed(11)
  M <- matrix(stats::rnorm(100), 10)
  A <- crossprod(M) - 3 * diag(10)
  lz <- .gp(".gamn_lanczos")(A, 9L)
  ev <- eigen(A, symmetric = TRUE)$values
  lz0 <- .gp(".gamn_lanczos")(A, 5L, tol = 0)              # no tolerance met: run to the last step
  expect_equal(sort(lz0$values), sort(ev[order(-abs(ev))[1:5]]), tolerance = 1e-10)
  expect_error(.gp(".gamn_lanczos")(A, 10L), "needs k below the matrix dimension (10)", fixed = TRUE)
  ord <- order(-abs(ev))[1:9]
  expect_equal(sort(lz$values), sort(ev[ord]), tolerance = 1e-10)
  expect_equal(A %*% lz$vectors, lz$vectors %*% diag(lz$values), tolerance = 1e-9)
})

test_that("the thin-plate basis thins a large 2-d design to max_knots, warns on a small k and refuses k = unique", {
  tp <- .gp(".gamn_tp_construct")
  set.seed(3)
  Xd <- cbind(stats::runif(80), stats::runif(80))
  Xd <- rbind(Xd, Xd[1:10, ])                       # duplicated rows are dropped before thinning
  b1 <- tp(Xd, 10L, max_knots = 40L, seed = 5L)
  b2 <- tp(Xd, 10L, max_knots = 40L, seed = 5L)
  expect_identical(b1$X, b2$X)                         # the same seed draws the same knots
  expect_equal(dim(b1$X), c(90L, 10L))
  expect_warning(b3 <- tp(Xd, 2L), "increased to minimum possible")
  expect_equal(ncol(b3$X), 4L)
  expect_error(tp(cbind(1:5, c(2, 1, 4, 3, 5)), 5L), "k equals the number of unique covariate values")
})

test_that("with_seed restores the generator, and leaves no seed behind when there was none", {
  ws <- .gp(".gamn_with_seed")
  genv <- globalenv()
  if (exists(".Random.seed", envir = genv, inherits = FALSE)) {
    keep <- get(".Random.seed", envir = genv)
    on.exit(assign(".Random.seed", keep, envir = genv), add = TRUE)
    rm(".Random.seed", envir = genv)
  }
  v <- ws(9L, stats::runif(2))
  expect_false(exists(".Random.seed", envir = genv, inherits = FALSE))
  set.seed(9L)
  expect_equal(v, stats::runif(2))
})

test_that("the cubic regression basis continues linearly beyond its end knots", {
  cr <- .gp(".gamn_cr_construct")
  b <- cr(seq(0, 1, length.out = 50), 6L)
  crX <- .gp(".gamn_cr_X")
  xk <- b$xk
  Fm <- b$F
  lo <- crX(xk[1] - c(0, 0.1, 0.2), xk, Fm)
  hi <- crX(xk[6] + c(0, 0.1, 0.2), xk, Fm)
  expect_equal(lo[3, ] - lo[2, ], lo[2, ] - lo[1, ], tolerance = 1e-12)
  expect_equal(hi[3, ] - hi[2, ], hi[2, ] - hi[1, ], tolerance = 1e-12)
  inside <- crX(c(xk[1], xk[6]), xk, Fm)
  expect_equal(lo[1, ], inside[1, ])
  expect_equal(hi[1, ], inside[2, ])
  # the slope at the end knot is the basis derivative there
  eps <- 1e-6
  d_in <- (crX(xk[6], xk, Fm) - crX(xk[6] - eps, xk, Fm)) / eps
  expect_equal(drop(hi[2, ] - hi[1, ]) / 0.1, drop(d_in), tolerance = 1e-4)
})

test_that("a thin-plate basis whose linear column has mean zero takes the partial constraint, as mgcv does", {
  skip_if_not_installed("mgcv")
  x <- -20:20                                            # centred exactly: the linear column sums to 0
  set.seed(4)
  y <- sin(x / 6) + stats::rnorm(41, sd = 0.1)
  d <- data.frame(x = x, y = y)
  f <- morie_gam(y ~ s(x, k = 8), data = d, method = "REML")
  m <- mgcv::gam(y ~ s(x, k = 8), data = d, method = "REML")
  expect_equal(unname(fitted(f)), unname(fitted(m)), tolerance = 1e-6)
  nd <- data.frame(x = c(-25, 0.5, 13))
  expect_equal(as.numeric(predict(f, nd)), as.numeric(predict(m, nd)), tolerance = 1e-6)
})

test_that("a logical response to a gaussian fit is its 0/1 numbers, and a rank-deficient model matrix is said", {
  set.seed(8)
  d <- data.frame(x = stats::runif(80))
  d$yl <- d$x + stats::rnorm(80, sd = 0.3) > 0.5
  d$yn <- as.numeric(d$yl)
  expect_equal(fitted(morie_gam(yl ~ s(x, k = 5), data = d)), fitted(morie_gam(yn ~ s(x, k = 5), data = d)))
  d$x2 <- 2 * d$x
  expect_warning(morie_gam(yn ~ x + x2, data = d), "rank deficient")
})

test_that("PIRLS halves a step that raises the penalised deviance", {
  set.seed(2)
  d <- data.frame(x = stats::runif(120))
  d$y <- stats::rpois(120, exp(1 + 2 * d$x))
  G <- .gp(".gamn_setup")(y ~ x, d, "poisson", NULL)
  St <- matrix(0, ncol(G$X), ncol(G$X))
  ref <- stats::glm(y ~ x, family = stats::poisson(), data = d)
  # start far out: a full IRLS step overshoots (or overflows) and is halved back
  for (b0 in list(c(-20, 0), c(8, -8), c(30, 30), c(-5, 20))) {
    pf <- .gp(".gamn_pirls")(G, St, b0)
    expect_equal(unname(pf$beta), unname(stats::coef(ref)), tolerance = 1e-8)
  }
})

test_that("Newton falls back to steepest descent and then stops with 'step failed' when no step helps", {
  set.seed(6)
  d <- data.frame(x = stats::runif(60))
  d$y <- sin(3 * d$x) + stats::rnorm(60, sd = 0.2)
  real <- .gp(".gamn_fit_score")
  n_calls <- 0L
  testthat::local_mocked_bindings(.gamn_fit_score = function(par, G, deriv = 2L, beta = NULL) {
    n_calls <<- n_calls + 1L
    if (n_calls == 1L) return(real(par, G, deriv, beta))
    list(score = Inf)                                    # every trial step is worse
  }, .package = "rmorie")
  f <- morie_gam(y ~ s(x, k = 6), data = d)
  expect_identical(f$outer$conv, "step failed")
  expect_equal(n_calls, 1L + 41L + 41L)                  # the first score, 41 Newton halvings, 41 steepest descents
})

test_that("the Givens and 2x2 eigen kernels match their LAPACK definitions", {
  lr <- .gp(".gamn_lartg")
  expect_equal(lr(3, 0), c(1, 0, 3))
  expect_equal(lr(0, -2), c(0, -1, 2))
  la <- .gp(".gamn_laev2")
  r <- la(2, 0, 2)                                           # a = c, b = 0: any unit vector is an eigenvector
  expect_equal(r[1:2], c(2, 2))
  expect_equal(sum(r[3:4]^2), 1)
  for (abc in list(c(1, 0, 3), c(2, 1, 2), c(-1, 2, 5), c(4, -3, -4), c(0, 1, 0))) {
    r <- la(abc[1], abc[2], abc[3])
    M <- matrix(c(abc[1], abc[2], abc[2], abc[3]), 2)
    expect_equal(sort(r[1:2]), sort(eigen(M, symmetric = TRUE)$values), tolerance = 1e-14)
    v <- r[3:4]
    expect_equal(drop(M %*% v), r[1] * v, tolerance = 1e-13)    # (cs1, sn1) is the vector of rt1
  }
  d5 <- .gp(".gamn_dlaed5")
  for (cfg in list(list(1L, c(0.6, 0.8), 0.5), list(1L, c(0.1, 0.99), 3), list(1L, c(0.99, 0.1), 3),
                   list(1L, c(0.99, 0.14), 0.6), list(1L, c(0.9, 0.1), -0.4),
                   list(2L, c(0.6, 0.8), 0.5), list(2L, c(0.6, 0.8), -0.1))) {
    i <- cfg[[1]]
    z <- cfg[[2]]
    rho <- cfg[[3]]
    dd <- c(1, 2)
    r <- d5(i, dd, z, rho)
    M <- diag(dd) + rho * tcrossprod(z)
    expect_equal(r$dlam, sort(eigen(M, symmetric = TRUE)$values)[i], tolerance = 1e-13)
    v <- (z) / (dd - r$dlam)
    v <- v / sqrt(sum(v^2))
    expect_equal(abs(sum(v * r$delta)), 1, tolerance = 1e-10)
  }
})

test_that("the tridiagonal eigensolver agrees with eigen() on split, clustered, scaled, zero and unsorted matrices", {
  set.seed(1)
  .eig_ok(stats::rnorm(60), stats::rnorm(59))                                   # divide and conquer
  .eig_ok(rep(1, 60), rep(1e-9, 59))                                            # clustered: deflation
  .eig_ok(rep(c(1, 2), 30), rep(1e-12, 59))                                     # pairs of equal eigenvalues
  .eig_ok(c(stats::rnorm(30), stats::rnorm(30)), c(stats::rnorm(29), 0, stats::rnorm(29)))   # split
  .eig_ok(stats::rnorm(40) * 1e200, stats::rnorm(39) * 1e200)                   # scaled down
  .eig_ok(stats::rnorm(40) * 1e-200, stats::rnorm(39) * 1e-200)                 # scaled up
  .eig_ok(c(rep(0, 5), stats::rnorm(55)), c(rep(0, 5), stats::rnorm(54)))       # a zero block
  .eig_ok(c(3, 1, 2, 7, 5), c(0, 0, 0, 0))                                      # diagonal, unsorted
  .eig_ok(c(5, 4, 3, 2, 1), c(1e-3, 1e-3, 1e-3, 1e-3))
  .eig_ok(c(1, 2, 3, 4, 5), c(1e-3, 1e-3, 1e-3, 1e-3))
  .eig_ok(c(stats::rnorm(10), stats::rnorm(40) * 1e3), c(stats::rnorm(9), 0, stats::rnorm(39)))
  st <- .gp(".gamn_dsteqr")
  for (de in list(list(c(4, 1), 0), list(c(1, 4), 0.5), list(c(9, 1, 5), c(0, 2)), list(c(1, 1e-300), 1e-300),
                  list(c(1e250, 2e250, 3e250), c(1e250, 1e250)), list(c(1e-250, 2e-250, 3e-250), c(1e-250, 1e-250)),
                  list(c(1, 2, 3, 4), c(1, 0, 1)), list(c(4, 3, 2, 1), c(1, 1, 1)))) {
    T <- .tri(de[[1]], de[[2]])
    r <- st(de[[1]], de[[2]])
    s <- max(abs(eigen(T, symmetric = TRUE)$values), .Machine$double.xmin)
    expect_lt(max(abs(sort(r$values) - sort(eigen(T, symmetric = TRUE)$values))) / s, 1e-12)
  }
})

test_that("the chi-square tail sums match mgcv's psum.chisq, testStat and liu2", {
  skip_if_not_installed("mgcv")
  ps <- .gp(".gamn_psum_chisq")
  for (case in list(list(3, c(1, 0.5)), list(0.01, c(2, 1, 0.5)), list(50, c(1, 1, 1)), list(-1, c(1, 2)),
                    list(2, c(1, -0.5)), list(1e4, c(1, 2)), list(5, c(10, 1e-3, 1e-3, 1e-3)),
                    list(0.5, c(1, 0.2, 0.1, 0.05, 0.01)), list(30, seq(0.1, 5, length.out = 25)))) {
    expect_equal(ps(case[[1]], case[[2]]), mgcv::psum.chisq(case[[1]], case[[2]]), tolerance = 1e-4)
  }
  dv <- .gp(".gamn_davies")
  expect_equal(dv(0, 0, 1, 0, 1)$p, 1)
  expect_equal(dv(0, 0, 1, 0, -1)$p, 0)
  expect_equal(dv(c(1, 2, 3), c(0, 0, 0), c(1, 1, 1), 0, 5, lim = 1L)$ifault, 1L)
  li <- .gp(".gamn_liu2")
  for (lam in list(c(1, 0.5), c(3, 0.2, 0.1), c(1, 1), c(5, 1e-3))) {
    expect_equal(li(3, lam), mgcv:::liu2(3, lam), tolerance = 1e-12)
  }
  expect_equal(li(3, c(1, -1)), mgcv:::liu2(3, c(1, -1)))
  ts <- .gp(".gamn_test_stat")
  set.seed(5)
  X <- matrix(stats::rnorm(60), 20, 3)
  p <- c(0.3, -0.2, 0.5)
  V <- crossprod(matrix(stats::rnorm(9), 3)) / 10
  Vd <- V
  Vd[3, ] <- Vd[, 3] <- 0                                          # rank 2
  for (cfg in list(list(V, 2.4), list(V, 0.6), list(Vd, 2.5), list(V, 3))) {
    a <- ts(p, X, cfg[[1]], cfg[[2]])
    b <- mgcv:::testStat(p, X, cfg[[1]], cfg[[2]])
    expect_equal(a$stat, b$stat, tolerance = 1e-10)
    expect_equal(a$pval, b$pval, tolerance = 1e-6)
    expect_equal(a$rank, b$rank)
  }
})

test_that("the eigensolver stops with an error at its iteration cap instead of returning unconverged values", {
  st <- .gp(".gamn_dsteqr")
  set.seed(9)
  expect_error(st(c(1, 2, 3, 4, 5), stats::rnorm(4), nmaxit = 1L), "did not converge in 1 iterations")
  expect_error(st(c(5, 4, 3, 2, 1), stats::rnorm(4), nmaxit = 1L), "did not converge in 1 iterations")
  expect_error(st(c(1, 9, 1, 9, 1, 9), rep(3, 5), nmaxit = 2L), "did not converge")
  r <- st(c(1, 2, 3), c(0.5, 0.5))                          # the default cap: 30 n
  expect_equal(r$values, sort(eigen(.tri(c(1, 2, 3), c(0.5, 0.5)), symmetric = TRUE)$values), tolerance = 1e-14)
})

test_that("divide and conquer deflates near-zero couplings and near-equal eigenvalues the way eigen() resolves them", {
  set.seed(21)
  for (cut in c(1e-15, 5e-16, 2e-16, 1e-14)) {
    e <- stats::rnorm(59)
    e[30] <- cut
    .eig_ok(stats::rnorm(60), e)
  }
  .eig_ok(c(rep(1, 29), 2, rep(1, 30)), rep(1e-10, 59))
  .eig_ok(c(rep(1, 30), rep(1 + 1e-14, 30)), c(rep(1e-3, 29), 1, rep(1e-3, 29)))
  .eig_ok(rev(seq_len(60)) / 60, rep(1e-12, 59))
  .eig_ok(rep(c(3, 1, 2), 20), c(rep(1e-13, 29), 0.5, rep(1e-13, 29)))
  .eig_ok(c(1, rep(2, 58), 3), c(0.5, rep(1e-15, 57), 0.5))
  # halves joined by one coupling: equal ends merge by a Givens rotation, or one root survives
  .eig_ok(rep(seq(0.1, 3, length.out = 30), 2), c(rep(1e-15, 29), 1, rep(1e-15, 29)))
  .eig_ok(rep(1, 60), c(rep(1e-15, 29), 1, rep(1e-15, 29)))
  .eig_ok(c(1:30 / 10, 3 + 1:30 / 10), c(rep(1e-15, 29), 1e-3, rep(1e-15, 29)))
  .eig_ok(c(rep(2, 59), 5), c(rep(1e-15, 58), 1))
  for (i in 1:60) {
    n <- sample(26:80, 1)
    d <- round(stats::rnorm(n), sample(0:1, 1))
    e <- stats::rnorm(n - 1) * 10^stats::runif(n - 1, -16, 0)
    .eig_ok(d, e)
  }
})

test_that("Davies' algorithm reports its iteration cap, and the tail falls back on Liu's approximation there", {
  dv <- .gp(".gamn_davies")
  expect_equal(dv(1, 0, 1, 0, 2, lim = 1L)$ifault, 1L)
  expect_equal(dv(c(1, 1e-4), c(0, 0), c(1, 1), 0, 2, lim = 3L)$ifault, 1L)
  ps <- .gp(".gamn_psum_chisq")
  lb <- c(1, 1e-6, 1e-6)
  r <- dv(lb, rep(0, 3), rep(1, 3), 0, 1e-3)
  if (r$ifault == 1L) expect_equal(ps(1e-3, lb), .gp(".gamn_liu2")(1e-3, lb))
  for (case in list(list(c(100, 1), 1e3), list(c(1, -1), 0.001), list(c(1e3, 1e-3), 50), list(c(-1, -2), -3))) {
    r <- dv(case[[1]], rep(0, length(case[[1]])), rep(1, length(case[[1]])), 0, case[[2]])
    expect_true(r$ifault %in% c(0L, 1L, 2L))
    if (r$ifault == 0L && requireNamespace("mgcv", quietly = TRUE)) {
      expect_equal(1 - r$p, mgcv::psum.chisq(case[[2]], case[[1]]), tolerance = 1e-4)
    }
  }
})

test_that("Davies' algorithm stays within its contract over weights, quantiles, limits and accuracies", {
  dv <- .gp(".gamn_davies")
  for (lb in list(1, c(1, 1e-4), c(1, 1e-6, 1e-6), c(100, 1), c(1e3, 1e-3), c(1, -1), c(-1, -2),
                  c(5, 4, 3, 2, 1), c(1, 0.999, 0.998), seq(1, 50), c(1e4, rep(1, 30)), c(1, rep(1e-3, 40)))) {
    for (q in c(1e-6, 1e-3, 0.5, 2, 10, 100, 1e4, -2)) for (lim in c(1L, 3L, 10L)) for (acc in c(2e-5, 0.01)) {
      r <- dv(lb, rep(0, length(lb)), rep(1, length(lb)), 0, q, lim = lim, acc = acc)
      expect_true(r$ifault %in% 0:2)
      if (r$ifault == 1L) expect_identical(r$p, -1) else expect_true(r$p >= -acc && r$p <= 1 + acc)
    }
  }
  # where it answers at the default accuracy it is the distribution function survey's routine gives
  skip_if_not_installed("mgcv")
  for (lb in list(c(5, 4, 3, 2, 1), c(100, 1), seq(1, 50))) for (q in c(2, 10, 100)) {
    r <- dv(lb, rep(0, length(lb)), rep(1, length(lb)), 0, q)
    if (r$ifault == 0L) expect_equal(1 - r$p, mgcv::psum.chisq(q, lb), tolerance = 1e-4)
  }
})

test_that("Davies' convergence factor gives up past 100 degrees of freedom, and the tail takes Liu's value past nlim", {
  dv <- .gp(".gamn_davies")
  # many degrees of freedom: the convergence factor's bound passes 100 and the factor is skipped
  for (df in list(c(150, 150), c(60, 60), c(500, 1), c(1, 1000), c(5000, 5000))) for (q in c(1, 100, 300, 1000, 1e4, 1e5))
    for (lb in list(c(1, 0.5), c(1, 1), c(1, 1e-3))) {
    rr <- dv(lb, c(0, 0), df, 0, q)
    expect_true(rr$ifault %in% 0:2)
  }
  r <- dv(c(1, 0.5), c(0, 0), c(150, 150), 0, 225)
  expect_identical(r$ifault, 0L)
  expect_equal(r$p, 1 - mgcv::psum.chisq(225, c(1, 0.5), df = c(150, 150)), tolerance = 1e-4)
  # one integration term allowed: Davies refuses, and the tail is Liu's approximation, as mgcv's is
  ps <- .gp(".gamn_psum_chisq")
  expect_equal(ps(3, c(1, 0.5), lim = 1L), .gp(".gamn_liu2")(3, c(1, 0.5)))
  expect_warning(ref <- mgcv::psum.chisq(3, c(1, 0.5), nlim = 1), "failure of Davies method")   # mgcv says it too
  expect_equal(ps(3, c(1, 0.5), lim = 1L), ref, tolerance = 1e-10)
  # a scaled chi-square on one degree of freedom, at a coarse accuracy, against the exact value
  expect_equal(dv(27.7, 0, 1, 0, 50, acc = 0.01)$p, stats::pchisq(50 / 27.7, 1), tolerance = 0.01)
})
