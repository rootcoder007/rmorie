# Guard against Python format specifiers in R format strings.
#
# 65 stop() calls across 29 files carried "%r", which is Python's repr
# conversion and not an R one. sprintf() rejects it, so every one of
# those validations raised
#
#   unrecognised format specification '%r'
#
# instead of the message it was written to give. The argument that was out
# of range never reached the user.
#
# A source scan cannot run here -- tests execute against the installed
# package, where R/ is not present -- so this triggers a spread of those
# validations and asserts each renders its own text with the offending
# value interpolated.

expect_renders <- function(expr, pattern, value) {
  msg <- tryCatch({ force(expr); NA_character_ },
                  error = function(e) conditionMessage(e))
  expect_false(is.na(msg))
  # the message must be the module's own, not sprintf complaining
  expect_no_match(msg, "format specification")
  expect_match(msg, pattern)
  # and the value that failed validation has to appear in it
  expect_match(msg, value, fixed = TRUE)
}

test_that("chrf_score reports the argument it rejected", {
  expect_renders(chrf_score("a", "b", beta = -1),
                 "beta must be positive", "-1")
  expect_renders(chrf_score("a", "b", n_char = 0L),
                 "n_char must be at least 1", "0")
})

test_that("offlrl reports the variant and backup it rejected", {
  ds <- list(list(0L, 0L, 1, 0L))
  expect_renders(offlrl(ds, variant = "nope"),
                 "variant must be one of", "nope")
  expect_renders(offlrl(ds, backup = "nope"),
                 "backup must be", "nope")
})

test_that("bcq reports the argument it rejected", {
  ds <- list(list(0L, 0L, 1, 0L))
  expect_renders(bcq(ds, tau = 2), "tau must lie in", "2")
  expect_renders(bcq(ds, loss = "nope"), "loss must be", "nope")
})

test_that("cooccurrence reports the window it rejected", {
  expect_renders(cooccurrence(list(c("a", "b")), window = 0L),
                 "window must be at least 1", "0")
})

test_that("sprintf would still reject the specifier that was there", {
  # the reason the above matters: R has no %r, so any reintroduction
  # breaks the message rather than degrading it
  expect_error(sprintf("got %r", "x"), "format specification")
})
