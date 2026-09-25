# Scaled Schoenfeld residuals and the PH score test against
# survival::cox.zph (version 3, ties = "breslow", terms = FALSE).

.zph_data <- function() {
  i <- 0:39
  x1 <- ((i * 7) %% 11) / 5 - 1
  x2 <- cos(i)
  list(
    time = round(3 + ((i * 13) %% 17) / 2 + 0.7 * x1 + 0.3 * sin(i), 3),
    event = as.integer((i * 5) %% 7 != 0), X = cbind(x1, x2)
  )
}

test_that("the km-transform test equals cox.zph", {
  z <- .zph_data()
  r <- morie_scaled_schoenfeld(z$time, z$event, z$X, "km")
  expect_equal(r$statistic, c(0.47916647734089746, 0.8249263749171708), tolerance = 1e-9)
  expect_equal(r$global_statistic, 1.2360592061885678, tolerance = 1e-9)
})

test_that("the rank-transform test equals cox.zph", {
  z <- .zph_data()
  r <- morie_scaled_schoenfeld(z$time, z$event, z$X, "rank")
  expect_equal(r$statistic, c(0.464436490974312, 0.685375577344253), tolerance = 1e-9)
  expect_equal(r$global_statistic, 1.10549294759634, tolerance = 1e-9)
})
