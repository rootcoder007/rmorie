# morie_has(): the predicate behind the package's example guards

test_that("morie_has answers for packages, tokens and bundled data", {
  expect_true(morie_has("stats", "utils"))
  expect_false(morie_has("stats", "no.such.package.xyz"))
  expect_true(morie_has("stats", "no.such.package.xyz", any = TRUE))
  expect_true(morie_has())
  expect_type(morie_has("sql"), "logical")
  expect_false(morie_has("no-such-file.csv") && !requireNamespace("rmoriedata", quietly = TRUE))
  withr::with_envvar(c(GCP_PROJECT = ""), expect_false(morie_has("bigquery")))
})
