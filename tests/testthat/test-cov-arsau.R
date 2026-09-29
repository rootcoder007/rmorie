# Coverage for the Ontario use-of-force registry accessors: the registry
# is keyed "year|kind" consistently with each entry's own fields, and the
# kind and year lists are the sorted distinct values drawn from it.

test_that("registry keys agree with entry fields; kinds and years are its distinct values", {
  reg <- ARSAU_REGISTRY()
  expect_gt(length(reg), 0L)
  keys <- vapply(reg, function(e) paste(e$year_or_range, e$kind, sep = "|"), "")
  expect_identical(unname(keys), names(reg))
  expect_identical(ARSAU_KINDS(), sort(unique(vapply(reg, `[[`, "", "kind"))))
  expect_identical(ARSAU_YEARS(), sort(unique(vapply(reg, `[[`, "", "year_or_range"))))
  expect_true("aggregate_summary" %in% ARSAU_KINDS())
  expect_true("2020-2022" %in% ARSAU_YEARS())
  expect_false(anyDuplicated(names(reg)) > 0)
})
