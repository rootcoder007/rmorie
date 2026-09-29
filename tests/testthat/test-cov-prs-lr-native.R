# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/prsLR_native.R (Knuth 1965 LR(k), DeRemer SLR and
# LALR). The automaton sizes and conflicts are the textbook ones
# (Aho, Lam, Sethi & Ullman, Compilers, 2nd ed.): the expression
# grammar has 12 LR(0) states and 22 canonical LR(1) states; grammar
# 4.49 (S -> L = R | R) is LALR(1) but not SLR(1); the a/b/c/d/e
# grammar is LR(1) but has a reduce/reduce conflict under LALR merging.

.lr_rule <- function(lhs, ...) list(lhs, list(...))
.lr_expr <- list(start = "E", rules = list(
  .lr_rule("E", "E", "+", "T"), .lr_rule("E", "T"),
  .lr_rule("T", "T", "*", "F"), .lr_rule("T", "F"),
  .lr_rule("F", "(", "E", ")"), .lr_rule("F", "id")))
.lr_449 <- list(start = "S", rules = list(
  .lr_rule("S", "L", "=", "R"), .lr_rule("S", "R"),
  .lr_rule("L", "*", "R"), .lr_rule("L", "id"), .lr_rule("R", "L")))
.lr_rr <- list(start = "S", rules = list(
  .lr_rule("S", "a", "A", "d"), .lr_rule("S", "b", "B", "d"),
  .lr_rule("S", "a", "B", "e"), .lr_rule("S", "b", "A", "e"),
  .lr_rule("A", "c"), .lr_rule("B", "c")))

test_that("augment prepends S' -> S, avoiding a clash with existing names", {
  ag <- morie_augment(.lr_expr)
  expect_identical(ag$start, "S'")
  expect_identical(ag$rules[[1]], list("S'", list("E")))
  expect_length(ag$rules, 7L)
  g2 <- list(start = "S'", rules = list(.lr_rule("S'", "x")))
  expect_identical(morie_augment(g2)$start, "S''")
})

test_that("closure and goto build the LR(0) and LR(1) item sets", {
  ag <- morie_augment(.lr_expr)
  g0 <- list(rules = ag$rules, start = ag$start)
  first <- .prsLR_first_sets(g0)
  nts <- c("S'", "E", "T", "F")
  expect_setequal(first$E, c("(", "id"))
  # I0 = closure(S' -> .E): every rule of E, T and F with the dot first
  I0 <- morie_closure("1:0", ag, first, nts, k = 0)
  expect_identical(I0, sort(paste0(1:7, ":0")))
  # goto(I0, E) = {S' -> E., E -> E.+T}
  expect_identical(morie_goto(I0, "E", ag, first, nts, k = 0), c("1:1", "2:1"))
  expect_identical(morie_goto(I0, "+", ag, first, nts, k = 0), character(0))
  # LR(1): E -> .E+T carries lookaheads END and +
  I0k <- morie_closure("1:0:END", ag, first, nts, k = 1)
  expect_identical(c("2:0:END", "2:0:+", "4:0:*", "7:0:)") %in% I0k, c(TRUE, TRUE, TRUE, FALSE))
  expect_identical(morie_goto(I0k, "id", ag, first, nts, k = 1),
                   sort(paste0("7:1:", c("END", "+", "*"))))
})

test_that("canonical collections and tables have the textbook sizes", {
  expect_length(morie_canonical_collection(morie_augment(.lr_expr), k = 0)$states, 12L)
  expect_length(morie_canonical_collection(morie_augment(.lr_expr), k = 1)$states, 22L)
  expect_equal(morie_build_tables(.lr_expr, "slr1")$n_states, 12L)
  expect_equal(morie_build_tables(.lr_expr, "lr1")$n_states, 22L)
  expect_equal(morie_build_tables(.lr_expr, "lalr1")$n_states, 12L)
  expect_equal(morie_build_tables(.lr_449, "lr1")$n_states, 14L)
  expect_equal(morie_build_tables(.lr_449, "lalr1")$n_states, 10L)
  expect_error(morie_build_tables(.lr_expr, "ll1"), "method must be one of")
})

test_that("conflicts separate SLR, LALR and canonical LR", {
  for (m in c("slr1", "lalr1", "lr1")) expect_true(morie_conflicts(.lr_expr, m)$ok)
  s <- morie_conflicts(.lr_449, "slr1")
  expect_equal(s$n_conflicts, 1L)
  expect_identical(s$conflicts[[1]]$kind, "shift/reduce")
  expect_identical(s$conflicts[[1]]$lookahead, "=")
  expect_true(morie_conflicts(.lr_449, "lalr1")$ok)
  expect_true(morie_conflicts(.lr_rr, "lr1")$ok)
  r <- morie_conflicts(.lr_rr, "lalr1")
  expect_false(r$ok)
  expect_true(all(vapply(r$conflicts, function(x) x$kind, "") == "reduce/reduce"))
})

test_that("parse builds the derivation tree with * binding tighter than +", {
  toks <- c("id", "+", "id", "*", "id")
  for (m in c("slr1", "lalr1", "lr1")) {
    tr <- morie_parse(.lr_expr, toks, m)
    expect_identical(tr$symbol, "E")
    expect_identical(vapply(tr$children, function(k) k$symbol, ""), c("E", "+", "T"))
    rhs <- tr$children[[3]]
    expect_identical(vapply(rhs$children, function(k) k$symbol, ""), c("T", "*", "F"))
  }
  p <- morie_prsLR(.lr_expr, c("(", "id", "+", "id", ")", "*", "id"))
  expect_identical(p$yield, "( id + id ) * id")
  expect_equal(p$n_states, 22L)
  tb <- morie_build_tables(.lr_449, "lalr1")
  expect_identical(morie_parse(.lr_449, c("*", "id", "=", "id"), tables = tb)$symbol, "S")
  expect_error(morie_parse(.lr_expr, c("id", "+")), "syntax error at token 2")
  expect_error(morie_parse(.lr_449, c("id", "=", "id"), "slr1"), "not slr1")
  expect_error(morie_prsLR(list(rules = list()), "x"), "grammar must be a list")
})
