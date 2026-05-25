# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Tests for morie_describe() and morie_describe_by_name() —
# the R-side mirror of Python morie.describe(), shipped in
# v0.9.5.5 to close the describe()-parity gap with the Python
# sibling package.

test_that("morie_describe_by_name loads and returns a narrative", {
  result <- suppressMessages(morie_describe_by_name("aalen"))
  if (is.null(result)) {
    # If the describe corpus isn't bundled in this build, skip.
    skip("describe_corpus.Rds not bundled in this morie installation")
  }
  expect_type(result, "character")
  expect_true(nchar(result) > 0L)
  # The describe_*.md files use a section heading we can search for.
  expect_match(result, "WHAT IT DOES", fixed = TRUE)
})

test_that("morie_describe normalises the leading morie_ prefix", {
  with_prefix    <- suppressMessages(morie_describe_by_name("morie_aalen"))
  without_prefix <- suppressMessages(morie_describe_by_name("aalen"))
  if (is.null(without_prefix)) {
    skip("describe_corpus.Rds not bundled")
  }
  expect_identical(with_prefix, without_prefix)
})

test_that("morie_describe normalises a trailing .md", {
  with_ext    <- suppressMessages(morie_describe_by_name("aalen.md"))
  without_ext <- suppressMessages(morie_describe_by_name("aalen"))
  if (is.null(without_ext)) {
    skip("describe_corpus.Rds not bundled")
  }
  expect_identical(with_ext, without_ext)
})

test_that("morie_describe gives a helpful diagnostic for an unknown name", {
  result <- expect_message(
    morie_describe_by_name("definitely_not_a_morie_callable_12345"),
    "no narrative"
  )
  expect_null(suppressMessages(
    morie_describe_by_name("definitely_not_a_morie_callable_12345")
  ))
})

test_that("morie_describe rejects non-character non-function inputs", {
  expect_error(morie_describe(42L),    "function or a character")
  expect_error(morie_describe(NULL),   "function or a character")
  expect_error(morie_describe(list()), "function or a character")
})

test_that("morie_describe_by_name rejects empty or vector inputs", {
  expect_error(morie_describe_by_name(""),
               "non-empty character scalar")
  expect_error(morie_describe_by_name(character(0L)),
               "non-empty character scalar")
})

test_that("morie_describe accepts a function object via substitute capture", {
  corpus_ok <- !is.null(suppressMessages(
    morie_describe_by_name("aalen")
  ))
  if (!corpus_ok) {
    skip("describe_corpus.Rds not bundled")
  }
  # Define a local function whose name happens to map to a known
  # corpus entry — we are testing the substitute() capture, not the
  # actual function's existence.
  morie_aalen <- function() NULL
  result <- suppressMessages(morie_describe(morie_aalen))
  expect_type(result, "character")
  expect_true(nchar(result) > 0L)
})

test_that("the describe corpus is cached across calls", {
  # First call populates the cache; the second should not touch
  # the disk again. We test this indirectly by confirming the
  # cache env has been populated after a successful lookup.
  env <- get(".morie_describe_env", envir = asNamespace("morie"))
  rm(list = ls(env), envir = env)        # clear cache
  expect_null(env$corpus)
  result <- suppressMessages(morie_describe_by_name("aalen"))
  if (is.null(result)) skip("describe_corpus.Rds not bundled")
  expect_false(is.null(env$corpus))
  # Second call: cache hit; the env$corpus should not change identity.
  before <- env$corpus
  suppressMessages(morie_describe_by_name("aalen"))
  expect_identical(before, env$corpus)
})
