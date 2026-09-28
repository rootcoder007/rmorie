test_that("serial-correlation LMM likelihood equals nlme::lme and the fit is at least as good", {
  skip_if_not_installed("nlme")
  e <- .morie_random_normal(400, seed = 21)
  ns <- 25
  id <- rep(seq_len(ns), each = 6)
  tt <- rep(c(0, 1, 2, 4, 5, 7), ns) + rep(e[seq_len(ns)] * 0.2, each = 6)
  x <- e[101:250]
  w <- numeric(150)
  for (s in seq_len(ns)) {
    r <- which(id == s)
    w[r] <- t(chol(0.8 * exp(-abs(outer(tt[r], tt[r], "-")) / 1.5))) %*% e[250 + r %% 150]
  }
  y <- 2 + 0.5 * tt + 1.2 * x + rep(e[251:275], each = 6) + w + 0.3 * e[(1:150) + 200]
  d <- data.frame(y, x, tt, id)
  X <- cbind(1, tt, x)
  for (reml in c(FALSE, TRUE)) {
    m <- nlme::lme(y ~ tt + x, random = ~ 1 | id, data = d, method = if (reml) "REML" else "ML",
                   correlation = nlme::corExp(form = ~ tt | id, nugget = TRUE),
                   control = nlme::lmeControl(opt = "optim", msMaxIter = 500))
    vc <- as.numeric(nlme::VarCorr(m)[, 1])
    cs <- stats::coef(m$modelStruct$corStruct, unconstrained = FALSE)
    at <- LmmSerialLoglik(y, X, id, tt, vc[1], vc[2] * (1 - cs[2]), 1 / cs[1], vc[2] * cs[2], reml = reml)
    expect_equal(at$loglik, as.numeric(stats::logLik(m)), tolerance = 1e-8)
    expect_equal(at$beta, unname(nlme::fixef(m)), tolerance = 1e-6)
    f <- LmmSerialFit(y, X, id, tt, reml = reml)
    expect_gte(f$loglik, as.numeric(stats::logLik(m)) - 1e-6)
    expect_equal(f$beta, unname(nlme::fixef(m)), tolerance = 1e-3)
  }
})
