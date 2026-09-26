pk_r <- rep(1:6, each = 4)
pk_c <- rep(1:4, 6)
pk_k <- seq_along(pk_r) - 1
pk_t <- c("A", "B", "C", "D")[(pk_r + 2 * pk_c) %% 4 + 1]
pk_z <- 10 + 0.8 * pk_r - 0.3 * pk_c + c(A = 0, B = 1.2, C = -0.5, D = 0.7)[pk_t] +
  0.4 * sin(3.7 * pk_k)
pk_z <- unname(pk_z)

test_that("papadk equals lm on the neighbour covariates (Sec 6.1.3.2)", {
  p <- papadk(pk_z, pk_r, pk_c, pk_t)
  expect_equal(p$treatment_effects, c(0.84900211261289049, -0.20123280111187125, 0.61809344815996758),
               tolerance = 1e-12)
  expect_equal(p$beta_neighbour, c(0.65113055913175832, 0.41483055271284214), tolerance = 1e-12)
  expect_equal(c(p$se_treatment, p$se_neighbour),
               c(0.41958878688856999, 0.29090510915199419, 0.40707815523031105,
                 0.23114763688194045, 0.28635972629784023), tolerance = 1e-12)
  expect_equal(p$sigma2, 0.25217108577785746, tolerance = 1e-12)
  pc <- papadk(pk_z, pk_r, pk_c, pk_t, combined = TRUE)
  expect_equal(c(pc$treatment_effects, pc$beta_neighbour, pc$sigma2),
               c(1.0950737125583965, -0.18587497750231208, 0.88244909427486995,
                 1.1109653644174815, 0.24517980352926533), tolerance = 1e-12)
})

test_that("fdiffm equals lm on within-column differences (eqs 6.12-6.13)", {
  f <- fdiffm(pk_z, pk_t, column = pk_c, row = pk_r)
  expect_equal(f$tau, c(1.0956879781041891, -0.14878075046031239, 0.81933491301455141), tolerance = 1e-12)
  expect_equal(f$se, c(0.40344155347946675, 0.44019082316910263, 0.3521526585352821), tolerance = 1e-12)
  expect_equal(f$sigma2, 0.93008621185100282, tolerance = 1e-12)
  expect_equal(f$df, 17)
})
