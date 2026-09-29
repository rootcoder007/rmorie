# Coverage tests for R/awltrn_native.R (Liu et al. 2018, augmented
# outcome-weighted learning): OWL and AOL weights, the weighted rule,
# the IPW regimen value and multi-stage backward induction.

aw_H <- cbind(c(-1, -0.5, 0, 0.5, 1, 1.5, -1.5, 0.2))
aw_A <- c(1, -1, 1, -1, 1, -1, -1, 1)
aw_R <- c(2.1, 0.4, 1.2, 0.3, 3.0, 0.2, 1.1, 1.9)

test_that("OWL weights shift the outcomes to be non-negative", {
  o <- owl_weights(aw_R, aw_A, aw_H)
  expect_equal(o$weights, aw_R / 0.5)
  Rn <- aw_R - 1
  o2 <- owl_weights(Rn, aw_A, aw_H, propensity = 0.4)
  expect_equal(o2$shift, 1 - min(aw_R))
  expect_equal(o2$weights, (Rn + o2$shift) / 0.4)
  expect_equal(o2$cv, sd(o2$weights) / mean(o2$weights), tolerance = 1e-12)
  expect_error(owl_weights(Rn, aw_A, aw_H, shift = 0), "non-negative")
  expect_error(owl_weights(aw_R, aw_A + 1, aw_H), "-1/\\+1")
  expect_error(owl_weights(aw_R[1:3], aw_A[1:3], aw_H[1:3, , drop = FALSE]), "at least 4")
})

test_that("AOL weights: absolute residual, flipped labels", {
  a <- aol_weights(aw_R, aw_A, aw_H)
  m <- fitted(lm(aw_R ~ aw_H))
  # the prognostic fit carries a 1e-8 ridge: compared at 1e-6
  expect_equal(a$prognostic, unname(m), tolerance = 1e-6)
  expect_equal(a$weights, abs(aw_R - a$prognostic) / 0.5, tolerance = 1e-12)
  expect_equal(a$labels, ifelse(aw_R - a$prognostic >= 0, aw_A, -aw_A))
  pm <- rep(1.2, 8)
  ap <- aol_weights(aw_R, aw_A, aw_H, prognostic = pm)
  expect_equal(ap$residual, aw_R - 1.2)
  expect_equal(ap$n_flipped, sum(aw_R < 1.2))
  expect_error(aol_weights(aw_R, aw_A, aw_H, prognostic = 1:3), "3 prognostic values")
})

test_that("weighted rule, regimen value and the AOL/OWL fits", {
  w <- c(1, 2, 0.5, 1, 3, 1, 0.2, 1.5)
  lab <- c(1, -1, 1, 1, 1, -1, -1, 1)
  wr <- weighted_rule(aw_H, lab, w)
  # 1e-6 ridge in the weighted least squares: compared at 1e-5
  expect_equal(as.numeric(wr$coef), unname(coef(lm(lab ~ aw_H, weights = w))), tolerance = 1e-5)
  expect_equal(wr$rule(0.7), if (sum(wr$coef * c(1, 0.7)) >= 0) 1L else -1L)
  expect_error(weighted_rule(aw_H, lab, -w), "non-negative")
  rule <- function(h) if (h[1] > 0) 1L else -1L
  follow <- vapply(1:8, function(i) rule(aw_H[i, ]) == aw_A[i], TRUE)
  expect_equal(regimen_value(aw_R, aw_A, aw_H, rule), mean(aw_R[follow]), tolerance = 1e-12)
  expect_error(regimen_value(aw_R, aw_A, aw_H, function(h) 0L), "no subject")
  f <- fit_aol(aw_R, aw_A, aw_H)
  expect_equal(f$value, regimen_value(aw_R, aw_A, aw_H, f$rule), tolerance = 1e-12)
  expect_equal(f$weights, aol_weights(aw_R, aw_A, aw_H)$weights)
  fo <- morie_awltrn(aw_R, aw_A, aw_H, method = "owl")
  expect_equal(fo$weights, aw_R / 0.5)
  expect_error(fit_aol(aw_R, aw_A, aw_H, method = "q"), "aol or owl")
})

test_that("augmented backward induction over two stages", {
  H2 <- cbind(c(0.3, -0.2, 1.1, -0.8, 0.5, 0.9, -1.2, 0.1))
  A2 <- c(-1, 1, 1, -1, 1, 1, -1, -1)
  R2 <- c(0.5, 1.2, 0.3, 0.8, 1.5, 0.2, 0.9, 1.1)
  st <- list(list(aw_R, aw_A, aw_H), list(R2, A2, H2))
  r <- fit_stages(st)
  f2 <- fit_aol(R2, A2, H2)
  a2 <- aol_weights(R2, A2, H2)
  fut <- ifelse(vapply(1:8, function(i) f2$rule(H2[i, ]) == A2[i], TRUE), R2, a2$prognostic)
  ps <- aw_R + fut
  f1 <- fit_aol(ps, aw_A, aw_H)
  a1 <- aol_weights(ps, aw_A, aw_H)
  fut1 <- ifelse(vapply(1:8, function(i) f1$rule(aw_H[i, ]) == aw_A[i], TRUE), ps, a1$prognostic)
  expect_equal(r$estimate, mean(fut1), tolerance = 1e-12)
  expect_equal(r$n_stages, 2L)
  expect_equal(morie_awltrn(stages = st)$estimate, r$estimate)
  expect_error(fit_stages(list()), "no stages")
  expect_error(fit_stages(list(list(aw_R, aw_A, aw_H), list(R2[-1], A2[-1], H2[-1, , drop = FALSE]))), "stage 1")
})
