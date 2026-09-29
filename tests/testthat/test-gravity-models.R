fl <- c(12, 0, 30, 2, 85, 4, 19, 1, 7, 40, 3, 16)
mo <- c(5, 5, 9, 9, 20, 20, 7, 7, 12, 12, 3, 3)
md <- c(9, 20, 5, 20, 5, 9, 20, 9, 7, 3, 12, 5)
d <- c(1, 3, 1, 2, 3, 2, 2.5, 1.5, 2, 1, 3, 1.2)

test_that("Igrav, Igravsq and Igraver", {
  k <- fl > 0
  f <- lm(log(fl[k]) ~ log(mo[k]) + log(md[k]) + log(d[k]))
  r <- Igrav(fl, mo, md, d)
  expect_equal(r$coefficients, unname(coef(f)), tolerance = 1e-10)
  expect_equal(r$r2, summary(f)$r.squared, tolerance = 1e-12)
  Fm <- rbind(c(0, 12, 5, 3), c(10, 0, 8, 2), c(6, 9, 0, 7), c(2, 3, 8, 0))
  D <- abs(outer(1:4, 1:4, "-"))
  M <- c(10, 8, 12, 5)
  off <- row(Fm) != col(Fm)
  g <- lm(log(Fm[off]) ~ log(outer(M, M)[off]) + log(D[off]))
  s <- Igravsq(Fm, M, D)
  expect_equal(s$coefficients, unname(coef(g)), tolerance = 1e-10)
  expect_equal(s$asymmetry, sum(abs(Fm - t(Fm))[upper.tri(Fm)]) / sum((Fm + t(Fm))[upper.tri(Fm)]))
  h <- fl + 1
  expect_equal(Igraver(fl, h)$srmse, 1 / mean(fl))
})

test_that("Igravbl and Igravwl reproduce the margins", {
  seed <- rbind(c(1, 2, 0.5), c(3, 4, 1), c(0.2, 1.5, 2))
  b <- Igravbl(seed, c(10, 20, 5), c(12, 15, 8))$balanced
  expect_equal(rowSums(b), c(10, 20, 5), tolerance = 1e-10)
  expect_equal(colSums(b), c(12, 15, 8), tolerance = 1e-10)
  fit <- loglin(array(c(10, 20, 5) %o% c(12, 15, 8) / 35, c(3, 3)), list(1, 2), start = seed, fit = TRUE,
                print = FALSE, eps = 1e-13, iter = 1000)$fit
  expect_equal(b, unname(fit), tolerance = 1e-9)
  C <- rbind(c(1, 2, 3), c(2, 1, 2.5), c(3, 2, 1))
  w <- Igravwl(NULL, c(10, 20, 15), c(18, 12, 15), C, 0.7)
  expect_equal(rowSums(w$T), c(10, 20, 15), tolerance = 1e-9)
  expect_equal(colSums(w$T), c(18, 12, 15), tolerance = 1e-9)
  expect_equal(w$T[1, 1] * w$T[2, 2] / (w$T[1, 2] * w$T[2, 1]), exp(-0.7 * (1 + 1 - 2 - 2)), tolerance = 1e-10)
})

test_that("Igravcl, Igravfe and Igravnb solve their score equations", {
  cl <- Igravcl(fl, mo, md, d)
  g <- glm(fl ~ log(d) + offset(log(mo * md)), family = poisson, control = glm.control(epsilon = 1e-14, maxit = 100))
  expect_equal(cl$statistic, -unname(coef(g)[2]), tolerance = 1e-9)
  o <- rep(c("a", "b", "c", "d"), each = 3)
  de <- rep(c("x", "y", "z"), 4)
  fe <- Igravfe(fl, o, de, d)
  expect_equal(unname(tapply(fe$fitted, o, sum)), unname(tapply(fl, o, sum)), tolerance = 1e-8)
  expect_equal(unname(tapply(fe$fitted, de, sum)), unname(tapply(fl, de, sum)), tolerance = 1e-8)
  nb <- Igravnb(fl, mo, md, d)
  X <- cbind(1, log(mo), log(md), log(d))
  expect_equal(as.vector(crossprod(X, (fl - nb$fitted) / (1 + nb$fitted / nb$theta))), rep(0, 4), tolerance = 1e-8)
})

test_that("Igravrt, Igravvf and Igravlm", {
  M <- c(10, 20, 5)
  D <- rbind(c(1, 2, 0.5), c(3, 1, 2))
  u <- sweep(D^-1.5, 2, M, "*")
  r <- Igravrt(M, D, 1.5, c(100, 50))
  expect_equal(r$probabilities, u / rowSums(u), tolerance = 1e-14)
  expect_equal(Igravvf(fl, 1.7, 1.5)$local_values, 1.7 * fl^1.5)
  W <- rbind(c(0, 1, 0, 0), c(0.5, 0, 0.5, 0), c(0, 0.5, 0, 0.5), c(0, 0, 1, 0))
  e <- c(0.3, -0.2, 0.5, -0.4)
  expect_equal(Igravlm(e, W)$statistic, (4 * sum(e * W %*% e) / sum(e^2))^2 / sum(W * (W + t(W))), tolerance = 1e-13)
})
