# Coverage for de Chaisemartin & D'Haultfoeuille (2020): the TWFE
# coefficient against lm() with group and period dummies, its decomposition
# weights on treated cells (the FWL residual of D, summing to one and
# reproducing beta_fe as a weighted sum of cell effects), and DID_M
# computed switcher by switcher against the matching stayers.

.panel <- function() {
  g <- rep(1:4, each = 3)
  t <- rep(1:3, 4)
  D <- c(0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0)
  set.seed(2)
  Y <- 1 + 0.5 * g + 0.3 * t + D * c(2, 3, 1, 4)[g] + stats::rnorm(12, sd = 0.1)
  list(Y = Y, D = D, g = g, t = t)
}

test_that("beta_fe equals the lm() TWFE coefficient and decomposes over treated cells", {
  p <- .panel()
  r <- morie_causdiddc_twfe(p$Y, p$D, p$g, p$t)
  f <- stats::lm(p$Y ~ p$D + factor(p$g) + factor(p$t))
  expect_equal(r$beta_fe, unname(stats::coef(f)[2]), tolerance = 1e-10)
  eps <- stats::residuals(stats::lm(p$D ~ factor(p$g) + factor(p$t)))
  w <- morie_causdiddc_weights(p$D, p$g, p$t)
  expect_equal(w$residual, unname(eps), tolerance = 1e-10)
  expect_equal(r$weight_sum, 1, tolerance = 1e-10)
  tr <- which(p$D == 1)
  expect_equal(unname(sort(unlist(w$weights))), unname(sort(eps[tr] / sum(eps[tr]))), tolerance = 1e-10)
  expect_identical(r$n_negative, sum(eps[tr] < 0))
  expect_equal(r$negative_mass, sum(abs(pmin(eps[tr], 0))) / sum(eps[tr]), tolerance = 1e-10)
  ww <- morie_causdiddc_weights(p$D, p$g, p$t, weights = rep(2, 12))
  expect_equal(unlist(ww$weights), unlist(w$weights), tolerance = 1e-10)
  expect_error(morie_causdiddc_twfe(p$Y, rep(1, 12), p$g, p$t), "no variation left")
})

test_that("DID_M compares each switcher with the stayers of its origin status", {
  p <- .panel()
  r <- morie_causdiddc_did_m(p$Y, p$D, p$g, p$t)
  cell <- function(gg, tt) p$Y[p$g == gg & p$t == tt]
  # 1 -> 2: group 1 joins (control: groups 2, 3 untreated at both); group 4 stays treated
  e1 <- (cell(1, 2) - cell(1, 1)) - mean(c(cell(2, 2) - cell(2, 1), cell(3, 2) - cell(3, 1)))
  # 2 -> 3: group 2 joins (control: group 3); group 4 leaves (control: group 1 treated at both)
  e2 <- (cell(2, 3) - cell(2, 2)) - (cell(3, 3) - cell(3, 2))
  e3 <- -((cell(4, 3) - cell(4, 2)) - (cell(1, 3) - cell(1, 2)))
  expect_equal(r$did_m, mean(c(e1, e2, e3)), tolerance = 1e-12)
  expect_identical(r$n_switches, 3L)
  expect_identical(vapply(r$switches, `[[`, "", "direction"), c("in", "in", "out"))
  a <- morie_causdiddc(p$Y, p$D, p$g, p$t)
  expect_equal(a$gap, a$beta_fe - a$did_m, tolerance = 1e-12)
  expect_identical(causdiddc, morie_causdiddc)
  expect_identical(twfe, morie_causdiddc_twfe)
  expect_identical(did_m, morie_causdiddc_did_m)
  expect_identical(causal_did_de_chaisemartin, morie_causdiddc_weights)
  # unbalanced panel: group 1 changes status only across a gap, so no cell
  # switches between consecutive periods; TWFE is identified, DID_M is not
  keep <- !(p$g == 1 & p$t == 2)
  D2 <- p$D
  D2[p$g == 1] <- c(0, 0, 1)
  D2[p$g == 4] <- 1
  D2[p$g == 2] <- 0
  st <- morie_causdiddc(p$Y[keep], D2[keep], p$g[keep], p$t[keep])
  expect_true(is.na(st$did_m))
  expect_true(is.finite(st$beta_fe))
  expect_error(morie_causdiddc_did_m(p$Y, rep(c(1, 0, 0, 1), each = 3), p$g, p$t), "not defined")
  expect_error(morie_causdiddc_did_m(p$Y, c(p$D[-1], 2), p$g, p$t), "binary 0/1")
  expect_error(morie_causdiddc_did_m(p$Y[1:3], p$D[1:3], p$g[1:3], p$t[1:3]), "at least four")
  expect_error(morie_causdiddc_did_m(p$Y, p$D, rep(1, 12), rep(1:2, 6)), "varies within")
})
