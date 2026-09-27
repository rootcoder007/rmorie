test_that("L1 multinomial equals glmnet's ungrouped multinomial", {
  skip_if_not_installed("glmnet")
  i <- 1:60
  X <- cbind(sin(i), cos(3 * i), sin(2 * i) * 0.5 + 0.3 * cos(i))
  g <- ifelse(sin(i) + 0.5 * cos(3 * i) > 0.3, 0, ifelse(cos(3 * i) > -0.2, 1, 2))
  r <- morie_esl_multinomial_l1(X, g, 1.5)
  f <- glmnet::glmnet(X, factor(g), family = "multinomial", lambda = 1.5 / 60, standardize = FALSE,
                      type.multinomial = "ungrouped", control = list(thresh = 1e-22, maxit = 1e7))
  ref <- sapply(coef(f), function(m) as.numeric(m))
  expect_equal(r$intercepts, unname(ref[1, ]), tolerance = 1e-9)
  expect_equal(r$coefficients, unname(t(ref[-1, ])), tolerance = 1e-9)
})
