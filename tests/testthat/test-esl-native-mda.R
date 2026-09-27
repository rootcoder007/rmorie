test_that("MDA by EM agrees with mda::mda from the same start", {
  i <- 1:90
  X <- cbind(sin(i) + ((i %% 3) == 0) * 1.5 + ((i %% 7) < 3) * 0.8, cos(2 * i) + ((i %% 3) == 1) * 1.2 - ((i %% 5) < 2) * 0.9)
  g <- i %% 3
  q <- rbind(c(0.2, 0.5), c(1.5, -0.8), c(-0.5, 1.1))
  r <- morie_esl_mda(X, g, 2, query = q)
  ref <- rbind(c(1.47630920950233e-02, 0.3929668448304358, 0.5922700630745410),
               c(2.84808169363645e-01, 0.0629460807514707, 0.6522457498848843),
               c(1.45626100979666e-06, 0.9891088453292690, 0.0108896984097212))
  expect_lt(max(abs(r$posterior - ref)), 1e-6)
  expect_equal(r$prediction, c(2, 2, 1))
  expect_equal(r$loglik, -205.88565271053125, tolerance = 1e-9)
  tr <- morie_esl_mda(X, g, 2)
  expect_equal(unname(as.matrix(table(tr$prediction, g))), rbind(c(27, 8, 4), c(0, 19, 3), c(3, 3, 23)))
})
