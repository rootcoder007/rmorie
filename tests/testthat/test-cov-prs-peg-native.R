# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/prsPEG_native.R (Ford 2004 parsing expression
# grammars, Ford 2002 packrat parsing). Match ends and step counts are
# derived by hand from the operator semantics of Sec. 3: every
# combinator invocation is one step.

test_that("lit, seq and choice: prioritised choice never tries the second branch", {
  a <- morie_prsPEG_lit("a")
  ab <- morie_prsPEG_lit("ab")
  r <- morie_prsPEG_parse(a, "a")
  expect_true(r$matched)
  expect_identical(r$end, 1L)
  expect_identical(r$steps, 1L)
  expect_identical(morie_prsPEG, morie_prsPEG_lit)
  # A <- "a" / "ab" cannot match "ab" fully: choice + lit("a") = 2 steps
  ch <- morie_prsPEG_choice(a, ab)
  r <- morie_prsPEG_parse(ch, "ab")
  expect_false(r$matched)
  expect_identical(r$end, 1L)
  expect_identical(r$steps, 2L)
  expect_true(morie_prsPEG_parse(ch, "ab", full = FALSE)$matched)
  expect_true(morie_prsPEG_parse(morie_prsPEG_choice(ab, a), "ab")$matched)
  # seq backtracks as a whole: seq + lit + lit = 3 steps
  s <- morie_prsPEG_seq(a, morie_prsPEG_lit("b"))
  r <- morie_prsPEG_parse(s, "ab")
  expect_true(r$matched)
  expect_identical(r$steps, 3L)
  r <- morie_prsPEG_parse(s, "aa")
  expect_false(r$matched)
  expect_true(is.na(r$end))
  expect_identical(r$consumed, 0L)
})

test_that("star, plus and opt are greedy and give nothing back", {
  a <- morie_prsPEG_lit("a")
  st <- morie_prsPEG_star(a)
  r <- morie_prsPEG_parse(st, "aaab", full = FALSE)
  expect_identical(r$end, 3L)
  # star + four lit attempts (three hits, one miss)
  expect_identical(r$steps, 5L)
  expect_true(morie_prsPEG_parse(st, "")$matched)
  # a* a never matches: the star ate every a
  expect_false(morie_prsPEG_parse(morie_prsPEG_seq(st, a), "aaa")$matched)
  pl <- morie_prsPEG_plus(a)
  expect_false(morie_prsPEG_parse(pl, "")$matched)
  expect_identical(morie_prsPEG_parse(pl, "aa")$end, 2L)
  op <- morie_prsPEG_opt(morie_prsPEG_lit("x"))
  expect_identical(morie_prsPEG_parse(op, "y", full = FALSE)$end, 0L)
  expect_identical(morie_prsPEG_parse(op, "xy", full = FALSE)$end, 1L)
  # a star over an empty match stops instead of looping
  expect_identical(morie_prsPEG_parse(morie_prsPEG_star(morie_prsPEG_lit("")), "zz", full = FALSE)$end, 0L)
})

test_that("and_ / not_ look ahead without consuming", {
  a <- morie_prsPEG_lit("a")
  any_char <- morie_prsPEG_choice(a, morie_prsPEG_lit("b"))
  expect_identical(morie_prsPEG_parse(morie_prsPEG_and_(a), "ab", full = FALSE)$end, 0L)
  expect_true(is.na(morie_prsPEG_parse(morie_prsPEG_and_(a), "b", full = FALSE)$end))
  expect_identical(morie_prsPEG_parse(morie_prsPEG_not_(a), "b", full = FALSE)$end, 0L)
  expect_true(is.na(morie_prsPEG_parse(morie_prsPEG_not_(a), "a", full = FALSE)$end))
  # (!"a" .)* consumes up to the first a
  upto <- morie_prsPEG_star(morie_prsPEG_seq(morie_prsPEG_not_(a), any_char))
  expect_identical(morie_prsPEG_parse(upto, "bbab", full = FALSE)$end, 2L)
})

test_that("packrat_parse recognises the same language and memoises the start rule", {
  a <- morie_prsPEG_lit("a")
  g <- morie_prsPEG_seq(morie_prsPEG_star(a), morie_prsPEG_lit("b"))
  for (txt in c("aab", "b", "aa", "aaba")) {
    p <- morie_prsPEG_parse(g, txt)
    k <- morie_prsPEG_packrat_parse(g, txt)
    expect_identical(k$matched, p$matched)
    expect_identical(k$end, p$end)
    expect_identical(k$steps, p$steps)
    expect_true(k$memoised)
    expect_identical(k$memo.entries, 1L)
  }
})
