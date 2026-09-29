# Coverage for the classical toolkit: ranking metrics from their counting
# definitions, number theory from brute force (Farey enumeration, Euclid,
# known continued fractions of pi and rationals), the resultant against
# base::det of the Sylvester matrix and the product-over-roots formula,
# and preprocessing / kernels / entropies from their closed forms.

test_that("precision, recall, hit rate and MAP at k", {
  pred <- c("d", "a", "x", "b", "y", "c")
  rel <- c("a", "b", "c", "a")
  expect_equal(PrecK(pred, rel, 4)$estimate, 2 / 4)
  expect_equal(ColMet(pred, rel, 4)$estimate, 2 / 3)
  expect_equal(ColMet(pred, rel, 6)$estimate, 1)
  expect_identical(HitsR(pred, rel, 1)$estimate, 0)
  expect_identical(HitsR(pred, rel, 2)$estimate, 1)
  m <- MapMet(pred, rel, 6)
  expect_equal(m$ap, (1 / 2 + 2 / 4 + 3 / 6) / 3, tolerance = 1e-12)
  m2 <- MapMet(list(pred, c("c", "q")), list(rel, "q"), 2)
  expect_equal(m2$ap, c((1 / 2) / 2, (1 / 2) / 1), tolerance = 1e-12)
  expect_equal(m2$ap_over_nrel, c((1 / 2) / 3, 1 / 2), tolerance = 1e-12)
  expect_equal(m2$estimate, mean(m2$ap), tolerance = 1e-12)
  expect_error(PrecK(pred, rel, 0), "k must be positive")
  expect_error(ColMet(pred, character(0), 3), "no relevant items")
  expect_error(MapMet(list(pred), list(rel, rel), 2), "one relevant set per ranking")
})

test_that("linear Diophantine equations by extended Euclid", {
  gcd <- function(a, b) if (b == 0) abs(a) else gcd(b, a %% b)
  for (abc in list(c(12, 18, 30), c(-7, 5, 3), c(21, -14, 35), c(0, 6, 12))) {
    r <- Diophs(abc[1], abc[2], abc[3])
    expect_true(r$solvable)
    expect_equal(r$gcd, gcd(abc[1], abc[2]))
    expect_equal(abc[1] * r$x + abc[2] * r$y, abc[3])
    expect_equal(abc[1] * (r$x + 3 * r$x_step) + abc[2] * (r$y + 3 * r$y_step), abc[3])
  }
  u <- Diophs(4, 6, 5)
  expect_false(u$solvable)
  expect_null(u$x)
  expect_error(Diophs(0, 0, 1), "cannot both be zero")
})

test_that("DiopT is the Farey sequence of order n", {
  for (n in 1:7) {
    fr <- unique(do.call(rbind, lapply(1:n, function(q) cbind(0:q, q))))
    fr <- fr[vapply(seq_len(nrow(fr)), function(i) {
      a <- fr[i, 1]
      b <- fr[i, 2]
      while (b) {
        t <- b
        b <- a %% b
        a <- t
      }
      a == 1
    }, TRUE), , drop = FALSE]
    fr <- fr[order(fr[, 1] / fr[, 2]), , drop = FALSE]
    r <- DiopT(n)
    expect_equal(unname(r$terms), unname(fr))
    expect_identical(r$estimate, nrow(fr))
  }
  expect_error(DiopT(0), "at least 1")
})

test_that("continued fractions of rationals, sqrt(2) and pi", {
  cf <- ContFr(415 / 93, 10)
  expect_identical(cf$terms, c(4L, 2L, 6L, 7L))
  expect_equal(unname(cf$convergents[4, ]), c(415, 93))
  expect_equal(cf$residual, 415 / 93 - 415 / 93)
  s2 <- ContFr(sqrt(2), 8)
  expect_identical(s2$terms, c(1L, rep(2L, 7)))
  pell <- c(1, 3, 7, 17, 41, 99, 239, 577)
  expect_equal(unname(s2$convergents[, 1]), pell)
  expect_equal(s2$estimate, 577 / 408, tolerance = 1e-15)
  expect_error(ContFr(1.5, 21), "at most 20")
  p <- Conti(4)
  expect_identical(p$terms, c(3, 7, 15, 1))
  expect_equal(c(p$numerator, p$denominator), c(355, 113))
  expect_equal(p$error, 355 / 113 - pi, tolerance = 1e-12)
  expect_equal(unname(Conti(3)$convergents[, 1] / Conti(3)$convergents[, 2]), c(3, 22 / 7, 333 / 106))
  expect_error(Conti(16), "between 1 and 15")
})

test_that("formal derivative and the Sylvester resultant", {
  d <- FrmlD(c(5, -3, 0, 2))
  expect_equal(d$coefficients, c(-3, 0, 6))
  expect_identical(d$degree, 2)
  expect_equal(FrmlD(7)$coefficients, 0)
  expect_error(FrmlD(numeric(0)), "at least one coefficient")
  q <- c(1, 3, 1)
  r <- Resaln(c(-2, 1), q)
  expect_equal(r$resultant, 1 + 3 * 2 + 2^2, tolerance = 1e-12)
  p2 <- c(6, -5, 1, 0)
  q2 <- c(-1, 0, 2, 3)
  r2 <- Resaln(p2, q2)
  expect_equal(r2$resultant, det(r2$sylvester), tolerance = 1e-9)
  roots <- polyroot(c(6, -5, 1))
  expect_equal(r2$resultant, Re(prod(vapply(roots, function(z) sum(q2 * z^(0:3)), complex(1)))), tolerance = 1e-9)
  expect_identical(c(r2$deg_p, r2$deg_q), c(2, 3))
  shared <- Resaln(c(-2, 1), c(-6, 5, -1))
  expect_true(shared$share_root)
  expect_error(Resaln(3, 5), "non-constant")
})

test_that("z-score flags, detrending, the RBF kernel and Renyi entropies", {
  x <- c(2, 3, 2.5, 3.1, 9, 2.8, 2.2)
  z <- ZscoreA(x, 2)
  expect_equal(z$z, abs(x - mean(x)) / stats::sd(x), tolerance = 1e-12)
  expect_identical(z$indices, 4L)
  expect_equal(ZscoreA(x, 2, ddof = 0)$sd, sqrt(mean((x - mean(x))^2)), tolerance = 1e-12)
  expect_error(ZscoreA(1, 1), "at least two")
  tt <- c(0, 1, 2, 4, 7, 8)
  y <- c(1, 2.5, 2.9, 5.2, 8.1, 9.4)
  dt <- Detrnd(y, tt)
  fit <- stats::lm(y ~ tt)
  expect_equal(c(dt$intercept, dt$slope), unname(stats::coef(fit)), tolerance = 1e-12)
  expect_equal(dt$detrended, unname(stats::residuals(fit)), tolerance = 1e-12)
  expect_equal(Detrnd(y)$slope, unname(stats::coef(stats::lm(y ~ I(0:5)))[2]), tolerance = 1e-12)
  expect_error(Detrnd(y, rep(1, 6)), "must not be constant")
  k <- Rbfk(c(1, 2, 3), c(2, 0, 3), 1.5)
  expect_equal(k$value, exp(-5 / (2 * 1.5^2)), tolerance = 1e-12)
  expect_error(Rbfk(1, 2, 0), "sigma must be positive")
  p <- c(0.5, 0.25, 0.125, 0.125, 0)
  expect_equal(Renent(p)$estimate, -log2(sum(p^2)), tolerance = 1e-12)
  expect_equal(Renent(p, alpha = 1, base = NULL)$estimate, -sum(p[p > 0] * log(p[p > 0])), tolerance = 1e-12)
  expect_equal(Renent(p, alpha = 0)$estimate, 2, tolerance = 1e-12)
  expect_equal(Renent(p, alpha = Inf)$estimate, 1, tolerance = 1e-12)
  expect_equal(Renent(2 * p, alpha = 3)$estimate, log2(sum(p^3)) / (1 - 3), tolerance = 1e-12)
  expect_error(Renent(p, base = 1), "not 1")
  expect_error(Renent(c(-1, 2)), "non-negative")
})
