test_that("outbreak detection and CUSUM equal surveillance; ecological regressions equal glm, glm.nb and zeroinfl", {
  skip_if_not_installed("surveillance")
  skip_if_not_installed("MASS")
  skip_if_not_installed("pscl")
  u <- .morie_random_uniform(400, seed = 21)
  obs <- as.integer(4 + 3 * sin((0:109) / 8) + 6 * u[1:110]) + ifelse(0:109 %in% c(90, 91), 15L, 0L)
  sts <- surveillance::create.disProg(week = seq_along(obs), observed = obs, state = rep(0, length(obs)), freq = 52)
  for (cfg in list(list(b = 1, w = 3), list(b = 0, w = 6))) {
    rng <- (cfg$b * 52 + cfg$w + 1):length(obs)
    ref <- suppressWarnings(surveillance::algo.bayes(sts, control = list(range = rng, b = cfg$b, w = cfg$w,
                                                                          actY = TRUE, alpha = 0.05)))
    ours <- BayesOutbreak(obs, 52, cfg$b, cfg$w)
    expect_equal(ours$upperbound, as.vector(ref$upperbound))
    expect_equal(ours$alarm, as.logical(ref$alarm))
  }
  for (tr in c("standard", "rossi", "anscombe")) {
    ref <- surveillance::algo.cusum(sts, control = list(range = 60:110, k = 1.04, h = 2.26, trans = tr,
                                                        reset = tr == "rossi"))
    ours <- CusumSurveillance(obs, start = 60, trans = tr, reset = tr == "rossi")
    expect_equal(ours$cusum, as.vector(ref$cusum), tolerance = 1e-12)
    expect_equal(ours$alarm, as.logical(ref$alarm))
  }
  n <- 40
  x1 <- .morie_random_normal(n, seed = 22)
  x2 <- u[201:240]
  e <- 3 + 5 * u[251:290]
  y <- pmax(as.integer(e * exp(0.2 + 0.4 * x1 - 0.5 * x2) * (0.5 + u[301:340])) - ifelse(u[351:390] > 0.2, 0L, 100L), 0L)
  df <- data.frame(y = y, x1 = x1, x2 = x2, e = e)
  X <- cbind(x1, x2)
  g <- stats::glm(y ~ x1 + x2 + offset(log(e)), family = stats::poisson(), data = df,
                  control = stats::glm.control(epsilon = 1e-14, maxit = 100))
  expect_equal(EcologicalRegression(y, e, X)$coefficients, unname(stats::coef(g)), tolerance = 1e-10)
  nb <- MASS::glm.nb(y ~ x1 + x2 + offset(log(e)), data = df, control = stats::glm.control(epsilon = 1e-14, maxit = 200))
  on <- EcologicalRegression(y, e, X, "negbin")
  expect_equal(on$coefficients, unname(stats::coef(nb)), tolerance = 1e-6)
  expect_equal(on$theta, nb$theta, tolerance = 1e-5)
  z <- pscl::zeroinfl(y ~ x1 + x2 + offset(log(e)) | 1, data = df, dist = "poisson",
                      control = pscl::zeroinfl.control(reltol = 1e-14, maxit = 5000))
  oz <- EcologicalRegression(y, e, X, "zip")
  expect_equal(oz$coefficients, unname(z$coefficients$count), tolerance = 1e-5)
  expect_equal(oz$loglik, as.numeric(stats::logLik(z)), tolerance = 1e-8)
})
