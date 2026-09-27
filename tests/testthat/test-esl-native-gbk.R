test_that("K-class boosting with leaf_scale = 1 equals gbm's multinomial", {
  i <- 1:90
  X <- cbind(sin(i) + ((i %% 3) == 0) * 1.5, cos(2 * i) + ((i %% 3) == 1) * 1.2)
  g <- i %% 3
  q <- rbind(c(0.2, 0.5), c(1.5, -0.8))
  r <- morie_esl_gbm_multiclass(X, g, M = 10, nu = 0.1, max_depth = 1, min_leaf = 5, query = q, leaf_scale = 1)
  expect_equal(r$prob, rbind(c(0.202093559225793, 0.3642531923899715, 0.433653248384236),
                             c(0.744347530615853, 0.0658350911294626, 0.189817378254685)), tolerance = 1e-12)
  e <- morie_esl_gbm_multiclass(X, g, M = 10, nu = 0.1, max_depth = 1, min_leaf = 5, query = q)
  expect_equal(e$prob[1, ], c(0.26033967778951156, 0.34264694036234983, 0.39701338184813856), tolerance = 1e-12)
  expect_true(all(diff(e$deviance_path) < 0))
})
