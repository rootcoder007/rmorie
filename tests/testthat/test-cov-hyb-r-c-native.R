# Coverage for Burke's (2002) hybrid-recommender taxonomy helpers not
# exercised elsewhere: the order-sensitivity classification of the seven
# hybridisation methods, feature combination (row-wise concatenation of
# content and collaborative features), feature augmentation and the
# meta-level pipeline.

test_that("the first four methods are order-insensitive, the pipelines are not", {
  ins <- c("weighted", "switching", "mixed", "feature_combination")
  pip <- c("cascade", "feature_augmentation", "meta_level")
  expect_false(any(vapply(ins, function(m) is_order_sensitive(m)$order_sensitive, TRUE)))
  expect_true(all(vapply(pip, function(m) is_order_sensitive(m)$order_sensitive, TRUE)))
  expect_error(is_order_sensitive("stacking"), "method must be one of")
})

test_that("feature combination concatenates the two feature sets row by row", {
  C <- rbind(c(1, 0, 2), c(0, 1, 1))
  D <- list(c(0.5, 0.1), c(0.2, 0.9))
  f <- feature_combination(C, D)
  expect_equal(f$features, list(c(1, 0, 2, 0.5, 0.1), c(0, 1, 1, 0.2, 0.9)))
  expect_identical(c(f$content_dim, f$collaborative_dim), c(3L, 2L))
  e <- feature_combination(NULL, NULL)
  expect_length(e$features, 0L)
  expect_error(feature_combination(C, D[1]), "2 content rows but 1 collaborative")
})

test_that("augmentation feeds the base output, meta-level feeds the model", {
  a <- feature_augmentation(c(0.2, 0.9), function(v) v * 10)
  expect_equal(a$result, c(2, 9))
  m <- meta_level(function(d) stats::lm(y ~ x, data = d), function(mod) unname(stats::coef(mod)[2]),
                  data.frame(x = 1:5, y = c(2.1, 3.9, 6.2, 7.8, 10.1)))
  expect_equal(m$estimate, unname(stats::coef(stats::lm(c(2.1, 3.9, 6.2, 7.8, 10.1) ~ I(1:5)))[2]), tolerance = 1e-12)
  expect_s3_class(m$model, "lm")
  expect_identical(m$result, m$estimate)
})
