# Coverage tests for R/unifAlg_native.R (Robinson 1965 unification). The
# defining properties are checked: the mgu makes both terms identical,
# composition acts as sequential application, and every other unifier
# factors through the mgu.

uv <- morie_unifAlg_var
ua <- morie_unifAlg_app
uc <- morie_unifAlg_const

test_that("term constructors, variables and the occurs test", {
  t <- ua("f", uv("X"), ua("g", uv("Y"), uv("X")), uc("a"))
  expect_true(morie_unifAlg_is_var(uv("X")))
  expect_false(morie_unifAlg_is_var(uc("a")))
  expect_identical(uc("a"), ua("a"))
  expect_identical(morie_unifAlg_variables(t), c("X", "Y"))
  expect_true(morie_unifAlg_occurs("Y", t))
  expect_false(morie_unifAlg_occurs("Z", t))
  expect_error(morie_unifAlg_variables(list("junk")), "not a term")
})

test_that("substitution: one parallel pass versus a fixed point", {
  t <- ua("f", uv("X"), uv("Y"))
  s <- list(X = uv("Y"), Y = uc("b"))
  expect_identical(morie_unifAlg_substitute(t, s), ua("f", uv("Y"), uc("b")))
  expect_identical(morie_unifAlg_apply_subst(t, s), ua("f", uc("b"), uc("b")))
  expect_error(morie_unifAlg_apply_subst(uv("X"), list(X = ua("g", uv("X")))), "fixed point")
})

test_that("composition equals applying the inner then the outer substitution", {
  inner <- list(X = ua("g", uv("Y")), Z = uv("W"))
  outer <- list(Y = uc("a"), W = uv("Z"), V = uc("c"))
  comp <- morie_unifAlg_compose(outer, inner)
  t <- ua("h", uv("X"), uv("Z"), uv("Y"), uv("V"))
  expect_identical(morie_unifAlg_substitute(t, comp),
    morie_unifAlg_substitute(morie_unifAlg_substitute(t, inner), outer))
  # Z -> W -> Z collapses to the identity binding, which is dropped
  expect_false("Z" %in% names(comp))
})

test_that("disagreement set is the first differing subterm pair", {
  a <- ua("f", uc("a"), ua("g", uv("X")), uv("Y"))
  b <- ua("f", uc("a"), ua("g", uc("b")), uv("Z"))
  expect_identical(morie_unifAlg_disagreement(a, b), list(uv("X"), uc("b")))
  expect_null(morie_unifAlg_disagreement(a, a))
  expect_identical(morie_unifAlg_disagreement(uc("a"), ua("a", uc("b"))), list(uc("a"), ua("a", uc("b"))))
})

test_that("unification: the mgu unifies, clashes and occurs checks fail", {
  t1 <- ua("f", uv("X"), ua("g", uv("Y")), uv("Y"))
  t2 <- ua("f", ua("h", uv("Z")), uv("W"), uc("a"))
  u <- morie_unifAlg_unify(t1, t2)
  expect_true(u$unified)
  expect_identical(morie_unifAlg_apply_subst(t1, u$mgu), morie_unifAlg_apply_subst(t2, u$mgu))
  expect_identical(morie_unifAlg_apply_subst(uv("W"), u$mgu), ua("g", uc("a")))
  expect_equal(u$n_bindings, length(u$mgu))
  cl <- morie_unifAlg_unify(ua("f", uc("a")), ua("f", uc("b")))
  expect_false(cl$unified)
  expect_match(cl$reason, "symbol clash")
  oc <- morie_unifAlg_unify(uv("X"), ua("f", uv("X")))
  expect_false(oc$unified)
  expect_match(oc$reason, "occurs check")
  cy <- morie_unifAlg_unify(uv("X"), ua("f", uv("X")), occurs_check = FALSE)
  expect_true(cy$cyclic)
  expect_identical(cy$mgu$X, ua("f", uv("X")))
  expect_identical(morie_unifAlg_unify(uc("a"), uc("a"))$mgu, list())
})

test_that("matching is one-way, and other unifiers factor through the mgu", {
  pat <- ua("f", uv("X"), ua("g", uv("Y")), uv("X"))
  sub <- ua("f", uc("a"), ua("g", uc("b")), uc("a"))
  m <- morie_unifAlg_match(pat, sub)
  expect_identical(morie_unifAlg_substitute(pat, m), sub)
  expect_null(morie_unifAlg_match(pat, ua("f", uc("a"), ua("g", uc("b")), uc("c"))))
  expect_null(morie_unifAlg_match(uc("a"), uv("X")))
  t1 <- ua("f", uv("X"), uv("Y"))
  t2 <- ua("f", uv("Y"), ua("g", uv("Z")))
  mgu <- morie_unifAlg_unify(t1, t2)$mgu
  other <- list(X = ua("g", uc("c")), Y = ua("g", uc("c")), Z = uc("c"))
  expect_identical(morie_unifAlg_apply_subst(t1, other), morie_unifAlg_apply_subst(t2, other))
  delta <- morie_unifAlg_factor_through(mgu, other, c("X", "Y", "Z"))
  expect_false(is.null(delta))
  for (v in c("X", "Y", "Z")) {
    expect_identical(morie_unifAlg_apply_subst(morie_unifAlg_apply_subst(uv(v), mgu), delta),
      morie_unifAlg_apply_subst(uv(v), other))
  }
  expect_null(morie_unifAlg_factor_through(list(X = uc("a")), list(X = uc("b")), "X"))
})
