test_that("morie_esl_backprop gradients equal central finite differences", {
  w <- list(alpha = rbind(c(0.3, -0.2, 0.5), c(0.1, 0.4, -0.3)), alpha0 = c(0.05, -0.1, 0.2),
            beta = rbind(c(0.7, -0.4), c(-0.5, 0.3), c(0.2, 0.6)), beta0 = c(0.1, -0.2))
  X <- cbind(sin(1:20), cos(2 * (1:20)))
  for (task in c("regression", "classification")) {
    y <- if (task == "regression") cbind(sin(3 * (1:20)), cos(1:20)) else (1:20) %% 2 + 1
    g <- morie_esl_backprop(X, y, w, task = task)
    for (key in c("alpha", "alpha0", "beta", "beta0")) {
      G <- g[[paste0("grad_", key)]]
      for (k in seq_along(w[[key]])) {
        up <- w
        dn <- w
        up[[key]][k] <- up[[key]][k] + 1e-6
        dn[[key]][k] <- dn[[key]][k] - 1e-6
        num <- (morie_esl_backprop(X, y, up, task = task)$loss - morie_esl_backprop(X, y, dn, task = task)$loss) / 2e-6
        expect_lt(abs(num - G[k]), 1e-7)
      }
    }
  }
  expect_equal(morie_esl_weight_decay(c(0.3, -0.2, 0.5, 0.7), lambda_ = 0.1)$penalty, 0.1 * 0.87, tolerance = 1e-14)
})
