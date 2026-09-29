# Coverage tests for R/clrnt_native.R (Wood, Houston and Hallifax 2017):
# incubational binding, blood clearance, in vitro to in vivo scaling,
# the well-stirred and parallel-tube liver models and accuracy metrics.

test_that("unbound fraction in microsomes and hepatocytes (eqs. 1-2)", {
  b <- function(x) 10^(0.072 * x^2 + 0.067 * x - 1.126)
  expect_equal(fu_microsomes(3.2, protein = 0.5), 1 / (1 + 0.5 * b(3.2)), tolerance = 1e-12)
  expect_equal(fu_hepatocytes(2.1), 1 / (1 + 125 * 0.005 * b(2.1)), tolerance = 1e-12)
  expect_error(fu_microsomes(1, protein = 0), "positive")
  expect_error(fu_hepatocytes(1, volume_ratio = -1), "positive")
})

test_that("blood clearance and scaling to the whole liver (eq. 3)", {
  a <- blood_from_plasma(6, 0.2, charge = "acidic")
  expect_equal(c(a$cl_blood, a$fu_blood, a$rb), c(6 / 0.55, 0.2 / 0.55, 0.55))
  expect_equal(blood_from_plasma(6, 0.2, blood_plasma_ratio = 1.5)$cl_blood, 4)
  expect_error(blood_from_plasma(6, 0.2, charge = "zwitterion"), "charge")
  expect_equal(scale_to_liver(10, 0.8), 10 * 120 * 21.4 / 0.8, tolerance = 1e-12)
  expect_equal(scale_to_liver(10, 0.8, system = "microsomes", species = "rat"), 10 * 60 * 40 / 0.8, tolerance = 1e-12)
  expect_equal(scale_to_liver(10, 1, pbsf = 50, liver_weight = 25), 12500)
  expect_error(scale_to_liver(10, 0), "\\(0, 1\\]")
  expect_error(scale_to_liver(10, 1, species = "dog"), "species")
})

test_that("observed CLint,u from the liver models (eq. 4)", {
  expect_equal(observed_clint_u(8, 0.3), 8 / (0.3 * (1 - 8 / 20.7)), tolerance = 1e-12)
  expect_equal(observed_clint_u(8, 0.3, liver_model = "parallel_tube"), -20.7 * log(1 - 8 / 20.7) / 0.3, tolerance = 1e-12)
  expect_equal(observed_clint_u(50, 0.5, species = "rat"), 50 / (0.5 * 0.5), tolerance = 1e-12)
  expect_error(observed_clint_u(25, 0.3), "cannot reach or exceed")
  expect_error(observed_clint_u(8, 0.3, liver_model = "dispersion"), "liver_model")
})

test_that("prediction accuracy: AFE, RMSE, ESF and the 2-fold count (eqs. 5-8)", {
  p <- c(10, 20, 5, 40)
  o <- c(15, 18, 12, 30)
  r <- prediction_accuracy(p, o)
  expect_equal(r$afe, 10^mean(log10(p / o)), tolerance = 1e-12)
  expect_equal(r$rmse, sqrt(mean((p - o)^2)), tolerance = 1e-12)
  expect_equal(r$esf, o / p)
  expect_equal(r$average_esf, 1 / r$afe, tolerance = 1e-12)
  expect_equal(r$within_fold, 0.75)
  expect_equal(prediction_accuracy(p, o, fold = 3)$within_fold, 1)
  expect_error(prediction_accuracy(p, o[-1]), "one observed value")
  expect_error(prediction_accuracy(c(0, 1), c(1, 1)), "positive")
})

test_that("end-to-end prediction versus observation", {
  r <- clrnt(c(10, 3), cl_h = c(8, 2), fu_blood = c(0.3, 0.6), log_pd = c(3, 1.5))
  fu <- c(fu_hepatocytes(3), fu_hepatocytes(1.5))
  pred <- c(10, 3) * 120 * 21.4 / fu
  obs <- c(observed_clint_u(8, 0.3), observed_clint_u(2, 0.6))
  expect_equal(r$predicted, pred, tolerance = 1e-12)
  expect_equal(r$observed, obs, tolerance = 1e-12)
  expect_equal(r$accuracy$afe, prediction_accuracy(pred, obs)$afe, tolerance = 1e-12)
  s <- clrnt(10, cl_plasma = 6, fu_plasma = 0.2, blood_plasma_ratio = 1.2, fu_incubation = 0.9, system = "microsomes")
  expect_equal(s$predicted, 10 * 40 * 21.4 / 0.9, tolerance = 1e-12)
  expect_equal(s$observed, observed_clint_u(5, 0.2 / 1.2), tolerance = 1e-12)
  expect_equal(s$blood_plasma_ratio, 1.2)
  expect_equal(morie_clrnt(c(10, 3), fu_incubation = 0.5)$predicted, c(10, 3) * 120 * 21.4 / 0.5, tolerance = 1e-12)
  expect_error(clrnt(10), "fu_incubation or log_pd")
  expect_error(clrnt(c(1, 2), fu_incubation = c(1, 1, 1)), "one entry per compound")
  expect_error(clrnt(1, cl_plasma = 2, fu_incubation = 1), "needs fu_plasma")
})
