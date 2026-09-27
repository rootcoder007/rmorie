test_that("GHSecant reduces to sech and logistic and is standardised", {
  xs <- c(-2, -0.3, 0, 1.1)
  expect_equal(GHSecant(xs, t = -pi / 2)$pdf, 0.5 / cosh(pi * xs / 2), tolerance = 1e-14)
  s <- sqrt(3) / pi
  expect_equal(GHSecant(xs, t = 0)$pdf, dlogis(xs, 0, s), tolerance = 1e-14)
  for (t in c(-2.5, -1, 0, 0.7, 3)) {
    f <- function(v) GHSecant(v, t = t)$pdf
    m <- vapply(0:2, function(k) integrate(function(v) v^k * f(v), -Inf, Inf, rel.tol = 1e-12)$value, 0)
    expect_equal(m, c(1, 0, 1), tolerance = 1e-9)
    xs <- c(-1.3, 0.2, 2.1)
    r <- GHSecant(xs, t = t, loc = 0.5, scale = 2)
    num <- vapply(xs, function(x) integrate(function(v) GHSecant(v, t = t, loc = 0.5, scale = 2)$pdf, -Inf, x, rel.tol = 1e-12)$value, 0)
    expect_equal(r$cdf, num, tolerance = 1e-9)
    expect_equal(GHSecant(p = r$cdf, t = t, loc = 0.5, scale = 2)$quantile, xs, tolerance = 1e-11)
  }
})

test_that("GHSecant matches the Python arm", {
  xs <- c(-2.2, -0.4, 0.3, 1.9)
  r <- GHSecant(xs, t = -2.5, loc = 0.4, scale = 1.3)
  expect_equal(r$pdf, c(0.02694960625408328, 0.23188339487486, 0.4996082829426766, 0.0906663762630237), tolerance = 1e-12)
  expect_equal(r$cdf, c(0.0291581653477348, 0.18985533700056711, 0.44944298630982954, 0.914301276811783), tolerance = 1e-12)
  r <- GHSecant(xs, t = -0.5, loc = 0.4, scale = 1.3)
  expect_equal(r$pdf, c(0.035028589685646594, 0.2591039271392708, 0.3499487736787849, 0.1350979274356413), tolerance = 1e-12)
  expect_equal(r$cdf, c(0.026054301926301537, 0.2454023220580126, 0.46488713985249497, 0.8908018037655583), tolerance = 1e-12)
  r <- GHSecant(xs, t = 0.7, loc = 0.4, scale = 1.3)
  expect_equal(r$pdf, c(0.035482682360713995, 0.2596104316587597, 0.3418988258768606, 0.13877582571270963), tolerance = 1e-12)
  expect_equal(r$cdf, c(0.02557487771749023, 0.24917814533003046, 0.4657067398556496, 0.8890852303807263), tolerance = 1e-12)
  r <- GHSecant(xs, t = 3.0, loc = 0.4, scale = 1.3)
  expect_equal(r$pdf, c(0.03768734752145766, 0.2573851237703502, 0.29054540782720145, 0.16841147463571185), tolerance = 1e-12)
  expect_equal(r$cdf, c(0.02078553651573445, 0.2759204751362695, 0.4709127808044994, 0.8758494902410078), tolerance = 1e-12)
  expect_equal(GHSecant(t = 1.1, n = 3, seed = 5)$random, c(0.6714564047525892, -1.4356729899412328, 2.232557010463369), tolerance = 1e-12)
})

test_that("BinghamDens is normalised and matches the Python arm", {
  A3 <- rbind(c(2, 0.5, 0.1), c(0.5, -1, 0.3), c(0.1, 0.3, 0.4))
  A2 <- rbind(c(1.5, 0.4), c(0.4, -0.7))
  expect_equal(BinghamDens(c(1, 0, 0), matrix(0, 3, 3))$pdf, 1 / (4 * pi), tolerance = 1e-15)
  th <- 2 * pi * (0:399) / 400
  expect_equal(sum(BinghamDens(cbind(cos(th), sin(th)), A2)$pdf) * 2 * pi / 400, 1, tolerance = 1e-13)
  g <- function(z) vapply(z, function(zz) {
    r <- sqrt(1 - zz^2)
    ph <- 2 * pi * (0:63) / 64
    sum(BinghamDens(cbind(r * cos(ph), r * sin(ph), zz), A3)$pdf) * 2 * pi / 64
  }, 0)
  expect_equal(integrate(g, -1, 1, rel.tol = 1e-12)$value, 1, tolerance = 1e-9)
  expect_equal(BinghamDens(c(1, 0, 0), A3 + 0.7 * diag(3))$log_normalizer - BinghamDens(c(1, 0, 0), A3)$log_normalizer, 0.7, tolerance = 1e-12)
  expect_equal(BinghamDens(rbind(c(0.6, 0, 0.8), c(0, -1, 0)), A3)$logpdf, c(-2.259381723397146, -4.331381723397146), tolerance = 1e-12)
  expect_equal(BinghamDens(c(0.6, 0.8), A2)$logpdf, c(-2.0788510281267074), tolerance = 1e-12)
})

test_that("CopulaDens bb1 is the mixed partial of the BB1 copula", {
  th <- 0.8
  de <- 1.7
  C <- function(u, v) (1 + ((u^-th - 1)^de + (v^-th - 1)^de)^(1 / de))^(-1 / th)
  h <- 1e-4
  for (uv in list(c(0.3, 0.7), c(0.1, 0.15), c(0.9, 0.6), c(0.5, 0.5))) {
    u <- uv[1]
    v <- uv[2]
    fd <- (C(u + h, v + h) - C(u + h, v - h) - C(u - h, v + h) + C(u - h, v - h)) / (4 * h^2)
    expect_equal(CopulaDens(u, v, "bb1", th, delta = de)$density, fd, tolerance = 1e-6)
  }
  expect_equal(CopulaDens(0.3, 0.7, "bb1", 1.3, delta = 1)$density, CopulaDens(0.3, 0.7, "clayton", 1.3)$density, tolerance = 1e-14)
  got <- c(CopulaDens(0.3, 0.7, "bb1", 0.8, delta = 1.7)$density, CopulaDens(0.1, 0.15, "bb1", 0.8, delta = 1.7)$density,
           CopulaDens(0.9, 0.6, "bb1", 0.8, delta = 1.7)$density)
  expect_equal(got, c(0.5630155340857571, 3.243167350850808, 0.754921952483426), tolerance = 1e-12)
})
