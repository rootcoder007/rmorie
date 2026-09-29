ti <- outer(1:24, 0:23, function(y, x) as.numeric((x %/% 3) %% 2 == 0))

test_that("FilterSim honours hard data and uses training values", {
  r <- FilterSim(ti, 10, 8, template = 5, n_classes = 4, seed = 7, hard_data = rbind(c(2, 3, 0), c(7, 1, 1)))
  expect_equal(r$n_patterns, 400)
  expect_equal(sum(r$class_sizes), 400)
  expect_true(all(r$realisation %in% c(0, 1)))
  expect_equal(r$realisation[4, 3], 0)
  expect_equal(r$realisation[2, 8], 1)
  expect_error(FilterSim(ti, 5, 5, template = 4))
})
