.sc_u <- .morie_random_uniform(200, seed = 51, stream = 0)
.sc_P <- cbind(10 * .sc_u[1:30], 10 * .sc_u[41:70])
.sc_pop <- round(200 + 800 * .sc_u[81:110])
.sc_y <- floor(.sc_pop * 0.01 * ifelse(sqrt(rowSums(sweep(.sc_P, 2, c(3, 3))^2)) < 2.5, 3,
    1) * (0.6 + 0.8 * .sc_u[121:150]))

test_that("ScanZones and KulldorffScan equal smerc", {
  expect_length(ScanZones(.sc_P, .sc_pop), 331)
  k <- KulldorffScan(.sc_P, .sc_y, .sc_pop, nsim = 0)
  expect_equal(lapply(k$all_zones, function(z) z - 1), list(c(0, 13, 7), c(9), c(19), c(27), c(10), c(15),
      c(1)))
  expect_equal(k$all_tobs, c(16.7772535110421, 2.9500721100645597, 0.5284501185224042, 0.01142547463981125,
      0.0035926777401025234, 0.00037873690087453227, 0), tolerance = 1e-12)
  b <- KulldorffScan(.sc_P, .sc_y, .sc_pop, kind = "binomial", ubpop = 0.3, nsim = 0)
  expect_equal(b$all_tobs, c(17.076424117345596, 2.998269995849114, 0.5361335557245184, 0.011577220662729815,
      0.003639907343313098, 0.00038368647801689804, 0), tolerance = 1e-9)
})

test_that("BesagNewell, TangoTest and StoneTest equal references", {
  r <- BesagNewell(.sc_P, .sc_y, .sc_pop, 20)
  expect_equal(r$m_values, c(1, 3, 5, 3, 2, 6, 4, 3, 4, 2, 4, 5, 4, 2, 2, 2, 3, 6, 6, 3, 4, 5, 2, 5, 3, 4, 5,
      4, 2, 5))
  expect_equal(r$k_values, c(26, 45, 22, 23, 21, 21, 24, 44, 28, 22, 25, 22, 24, 40, 21, 21, 22, 32, 26, 25,
      24, 30, 21, 26, 25, 25, 25, 25, 22, 28))
  expect_equal(r$p_values, c(7.78083211115943e-05, 6.121431564753976e-06, 0.9183090700114827,
      0.8591288819858462, 0.5965135933309287, 0.9862850995502661, 0.9599804828428992, 1.4828970534730423e-05,
      0.806409472358596, 0.09172623579654493, 0.7824568891658797, 0.9183090700114827, 0.9599804828428992,
      5.282173305420912e-07, 0.5500156076147518, 0.5500156076147518, 0.8123965401601088, 0.5673723862045595,
      0.9080038813332084, 0.32357906889634414, 0.9599804828428992, 0.5237896993509439, 0.5965135933309287,
      0.9311446572398533, 0.32357906889634414, 0.7824568891658797, 0.9306306446807814, 0.7824568891658797,
      0.09172623579654493, 0.9444041861311259), tolerance = 1e-12)
  t <- TangoTest(.sc_y, .sc_pop, exp(-as.matrix(stats::dist(.sc_P)) / 2))
  expect_equal(c(t$tstat, t$gof, t$sa, t$tstat_chisq, t$dfc, t$pvalue_chisq), c(0.016761913770384464,
      0.010827027794310776, 0.005934885976073689, 42.54587134873325, 6.884694552106496,
      3.6340093845232957e-07), tolerance = 1e-10)
  s <- StoneTest(c(4, 2, 1, 1), c(1, 2, 2, 3), 1:4)
  expect_equal(c(s$statistic, s$k), c(4, 1))
  expect_error(KulldorffScan(.sc_P, .sc_y, .sc_pop, kind = "normal", nsim = 0), "kind")
})
