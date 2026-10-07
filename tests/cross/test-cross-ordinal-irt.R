# SPDX-License-Identifier: AGPL-3.0-or-later
test_that("native ordinal IRT agrees with MCMCpack::MCMCordfactanal", {
  skip_if_not_installed("MCMCpack")
  set.seed(7)
  n <- 200
  J <- 10
  th <- rnorm(n)
  eta <- outer(th, runif(J, 0.8, 1.6)) + matrix(rnorm(J, 0, 0.5), n, J, TRUE)
  Y <- matrix(cut(eta + rnorm(n * J), c(-Inf, -0.5, 0.4, 1.2, Inf), labels = FALSE), n, J)
  Y[sample(length(Y), 50)] <- NA
  f <- morie_spatial_voting_ordinal_irt(Y, n_samples = 3000L, burn_in = 1000L, seed = 1L)
  df <- as.data.frame(Y)
  for (j in seq_along(df)) df[[j]] <- factor(df[[j]], ordered = TRUE)
  fo <- stats::as.formula(paste("~", paste(names(df), collapse = "+")))
  r <- MCMCpack::MCMCordfactanal(fo, data = df, factors = 1,
                                 lambda.constraints = list(V1 = list(2, "+")),
                                 burnin = 1000, mcmc = 20000, thin = 5,
                                 store.scores = TRUE, seed = 3, verbose = 0)
  cm <- colMeans(r)
  ph <- cm[grep("^phi", names(cm))]
  expect_gt(cor(ph, f$ideal_points[, 1]), 0.999)
  expect_lt(max(abs(ph - f$ideal_points[, 1])), 0.12)
  expect_lt(max(abs(cm[grep("^Lambda.*\\.2$", names(cm))] - f$discrimination[, 1])), 0.1)
  expect_lt(max(abs(cm[grep("^Lambda.*\\.1$", names(cm))] - f$intercept)), 0.1)
  g2 <- vapply(f$cutpoints, function(g) g[2], 0)
  expect_lt(max(abs(cm[grep("^gamma2", names(cm))] - g2)), 0.1)
})
