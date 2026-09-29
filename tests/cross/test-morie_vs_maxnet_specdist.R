# Cross test: MaxentFit is the maximum-likelihood limit of maxnet as the regularisation vanishes.

test_that("MaxentFit beats maxnet's objective and agrees in prediction", {
  skip_if_not_installed("maxnet")
  bg <- cbind(sin((0:199) * 0.37) * 2, cos((0:199) * 0.91) + 0.01 * (0:199))
  pr <- cbind(0.8 + 0.3 * sin(0:39), 0.5 + 0.2 * cos((0:39) * 1.3))
  feat <- function(X) cbind(X, X^2)
  df <- data.frame(x1 = c(pr[, 1], bg[, 1]), x2 = c(pr[, 2], bg[, 2]))
  m <- maxnet::maxnet(c(rep(1, 40), rep(0, 200)), df, f = ~ x1 + x2 + I(x1^2) + I(x2^2), regmult = 1e-9)
  got <- MaxentFit(feat(pr), feat(bg))
  B <- rbind(feat(bg), feat(pr))
  obj <- function(b) -mean(feat(pr) %*% b) + log(sum(exp(B %*% b)))
  # maxnet (glmnet path) stops short of the unpenalised optimum; ours is the optimum
  expect_lte(obj(got$lambdas), obj(as.numeric(m$betas)) + 1e-12)
  expect_gt(stats::cor(as.numeric(B %*% got$lambdas), as.numeric(B %*% m$betas)), 0.999)
  expect_equal(got$entropy, m$entropy, tolerance = 0.01)
})
