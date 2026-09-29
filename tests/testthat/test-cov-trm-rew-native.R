# Coverage for term rewriting: normal forms of Peano addition are checked
# against integer arithmetic, positions and replacement against a hand
# built tree, the LPO against its defining cases, critical pairs against
# the overlap computed by hand, and Knuth-Bendix completion against the
# convergent system it must produce.

.V <- function(x) morie_unifAlg_var(x)
.A <- function(f, ...) morie_unifAlg_app(f, ...)
.C <- function(f) morie_unifAlg_const(f)
.num <- function(k) if (k == 0) .C("0") else .A("s", .num(k - 1))
.addrules <- function() {
  list(morie_trmRew_rule(.A("plus", .C("0"), .V("y")), .V("y")),
       morie_trmRew_rule(.A("plus", .A("s", .V("x")), .V("y")), .A("s", .A("plus", .V("x"), .V("y")))))
}
.prec <- list(plus = 2, s = 1, "0" = 0)

test_that("Peano addition rewrites to the numeral of the sum", {
  R <- .addrules()
  for (a in 0:3) for (b in 0:2) {
    nf <- morie_trmRew_normal_form(.A("plus", .num(a), .num(b)), R)
    expect_identical(nf$normal_form, .num(a + b))
    expect_identical(nf$steps, a + 1L)
    expect_length(nf$trace, a + 1L)
    expect_identical(morie_trmRew_normal_form(.A("plus", .num(a), .num(b)), R, strategy = "outermost")$normal_form, .num(a + b))
  }
  tr <- morie_trmRew_term_rewriting(.A("plus", .num(2), .num(2)), R)
  expect_identical(tr$estimate, .num(4))
  expect_identical(tr$strategy, "innermost")
  st <- morie_trmRew_rewrite_step(.A("plus", .num(1), .num(1)), R)
  expect_identical(st$position, integer(0))
  expect_identical(st$rule, 1L)
  expect_identical(st$term, .A("s", .A("plus", .C("0"), .num(1))))
  expect_null(morie_trmRew_rewrite_step(.num(3), R))
  expect_error(morie_trmRew_rewrite_step(.num(1), R, strategy = "random"), "strategy must be one of")
  d <- morie_trmRew_decides(.A("plus", .num(1), .num(2)), .A("plus", .num(2), .num(1)), R)
  expect_true(d$equal)
  expect_false(morie_trmRew_decides(.num(1), .num(2), R)$equal)
})

test_that("innermost and outermost pick different redexes", {
  R <- list(morie_trmRew_rule(.A("f", .A("g", .V("x"))), .V("x")), morie_trmRew_rule(.A("g", .V("y")), .A("h", .V("y"))))
  t <- .A("f", .A("g", .C("a")))
  expect_identical(morie_trmRew_rewrite_step(t, R, "outermost")$position, integer(0))
  expect_identical(morie_trmRew_rewrite_step(t, R, "innermost")$position, 0L)
  expect_identical(morie_trmRew_normal_form(t, R, "outermost")$normal_form, .C("a"))
  expect_identical(morie_trmRew_normal_form(t, R, "innermost")$normal_form, .A("f", .A("h", .C("a"))))
})

test_that("positions, subterms and replacement address the tree", {
  t <- .A("f", .C("a"), .A("g", .C("b"), .V("x")))
  expect_identical(morie_trmRew_positions(t), list(integer(0), 0L, 1L, c(1L, 0L), c(1L, 1L)))
  expect_identical(morie_trmRew_subterm_at(t, c(1L, 0L)), .C("b"))
  expect_identical(morie_trmRew_subterm_at(t, integer(0)), t)
  expect_identical(morie_trmRew_replace_at(t, c(1L, 1L), .C("c")), .A("f", .C("a"), .A("g", .C("b"), .C("c"))))
  expect_identical(morie_trmRew_replace_at(t, integer(0), .C("z")), .C("z"))
  expect_error(morie_trmRew_subterm_at(t, 2L), "does not exist")
  expect_error(morie_trmRew_replace_at(t, c(0L, 0L), .C("c")), "does not exist")
  expect_identical(morie_trmRew_positions(.V("x")), list(integer(0)))
})

test_that("rules refuse variable left sides and unbound right-hand variables", {
  expect_error(morie_trmRew_rule(.V("x"), .C("a")), "bare variable")
  expect_error(morie_trmRew_rule(.A("f", .V("x")), .A("g", .V("x"), .V("y"))), "unbound variable")
  expect_identical(morie_trmRew_rule(.A("f", .V("x")), .V("x")), list(.A("f", .V("x")), .V("x")))
})

test_that("the lexicographic path order follows its defining cases", {
  x <- .V("x")
  y <- .V("y")
  p <- list(f = 2, g = 1)
  expect_true(morie_trmRew_lpo_greater(.A("f", x), x, p))
  expect_false(morie_trmRew_lpo_greater(x, .A("f", x), p))
  expect_false(morie_trmRew_lpo_greater(.C("a"), x, p))
  expect_true(morie_trmRew_lpo_greater(.A("f", x), .A("g", x), p))
  expect_false(morie_trmRew_lpo_greater(.A("g", x), .A("f", x), p))
  expect_true(morie_trmRew_lpo_greater(.A("f", .A("g", x), y), .A("f", x, .A("g", y)), p))
  expect_false(morie_trmRew_lpo_greater(.A("f", x, y), .A("f", y, x), p))
  expect_false(morie_trmRew_lpo_greater(.A("f", x), .A("f", x), p))
  R <- .addrules()
  expect_true(morie_trmRew_is_terminating(R, .prec)$terminating)
  bad <- morie_trmRew_is_terminating(list(R[[1]], morie_trmRew_rule(.A("g", x), .A("g", .A("g", x)))), .prec)
  expect_false(bad$terminating)
  expect_identical(bad$unoriented, 1L)
})

test_that("critical pairs, local confluence and Newman's lemma", {
  R <- list(morie_trmRew_rule(.A("f", .C("a")), .C("b")), morie_trmRew_rule(.C("a"), .C("c")))
  cp <- morie_trmRew_critical_pairs(R)
  expect_length(cp, 1L)
  expect_identical(cp[[1]]$left, .C("b"))
  expect_identical(cp[[1]]$right, .A("f", .C("c")))
  expect_identical(cp[[1]]$rules, c(0L, 1L))
  expect_identical(cp[[1]]$position, 0L)
  lc <- morie_trmRew_is_locally_confluent(R)
  expect_false(lc$locally_confluent)
  expect_length(lc$unjoinable, 1L)
  pr <- list(f = 3, a = 2, b = 1, c = 0)
  expect_false(morie_trmRew_is_confluent(R, pr)$confluent)
  R2 <- c(R, list(morie_trmRew_rule(.A("f", .C("c")), .C("b"))))
  expect_true(morie_trmRew_is_locally_confluent(R2)$locally_confluent)
  cf <- morie_trmRew_is_confluent(R2, pr)
  expect_true(cf$confluent && cf$terminating)
  expect_identical(cf$n_critical_pairs, 1L)
  expect_true(morie_trmRew_joinable(.C("b"), .A("f", .C("c")), R2))
  expect_false(morie_trmRew_joinable(.C("b"), .A("f", .C("c")), R))
  loop <- list(morie_trmRew_rule(.A("f", .V("x")), .A("f", .A("f", .V("x")))))
  expect_false(morie_trmRew_joinable(.A("f", .C("a")), .C("a"), loop, max_steps = 20))
  expect_error(morie_trmRew_normal_form(.A("f", .C("a")), loop, max_steps = 20), "no normal form after 20 steps")
  expect_length(morie_trmRew_critical_pairs(.addrules()), 0L)
})

test_that("Knuth-Bendix completion orients, interreduces, or reports failure", {
  pr <- list(f = 3, a = 2, b = 1, c = 0)
  eqs <- list(list(.A("f", .C("a")), .C("b")), list(.C("a"), .C("c")))
  kb <- morie_trmRew_complete(eqs, pr)
  expect_true(kb$complete)
  lhs <- lapply(kb$rules, function(r) r[[1]])
  expect_setequal(vapply(kb$rules, function(r) paste(deparse(r), collapse = ""), ""),
                  vapply(list(list(.C("a"), .C("c")), list(.A("f", .C("c")), .C("b"))), function(r) paste(deparse(r), collapse = ""), ""))
  expect_length(lhs, 2L)
  expect_true(morie_trmRew_is_confluent(kb$rules, pr)$confluent)
  expect_true(morie_trmRew_decides(.A("f", .C("a")), .C("b"), kb$rules)$equal)
  comm <- morie_trmRew_complete(list(list(.A("f", .V("x"), .V("y")), .A("f", .V("y"), .V("x")))), list(f = 1))
  expect_false(comm$complete)
  expect_identical(comm$reason, "unorientable equation")
  expect_match(morie_trmRew_cheatsheet(), "Knuth & Bendix")
})
