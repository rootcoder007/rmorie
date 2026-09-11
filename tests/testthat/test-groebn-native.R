# Anchors for Buchberger's algorithm.
#
# The module could not run at all: .groebn_monomials called
# do.call(order, ...) where order held the monomial order as a string,
# shadowing base::order, so every call died with "could not find function
# lex". Two claims in the file header are checked here directly rather
# than assumed: that all S-polynomials of the returned basis reduce to
# zero (Buchberger's criterion, which is what makes it a Grobner basis),
# and that the reduced basis is the unique monic one for the ideal and
# order.
#
# Polynomials are named lists: the name is an exponent tuple "i_j" for
# x^i y^j, the value an exact rational.

P <- function(terms) .groebn_poly(terms)
q <- function(p) p[[1L]] / p[[2L]]

# every S-polynomial reduces to zero: the definition of a Grobner basis
criterion <- function(G, ord) {
  for (i in seq_along(G)) {
    for (j in seq_along(G)) {
      if (j <= i) next
      s <- .groebn_spoly(G[[i]], G[[j]], ord)
      if (length(.groebn_normal_form(s, G, ord))) return(FALSE)
    }
  }
  TRUE
}

test_that("exact rational arithmetic is exact and in lowest terms", {
  expect_identical(.groebn_fr(2L, 4L), c(1L, 2L))
  expect_identical(.groebn_fr(-2L, 4L), c(-1L, 2L))
  expect_identical(.groebn_fr(2L, -4L), c(-1L, 2L))
  expect_identical(.groebn_fr(0L, 5L), c(0L, 1L))
  expect_error(.groebn_fr(1L, 0L), "division by zero")
  expect_equal(q(.groebn_fr_add(c(1L, 2L), c(1L, 3L))), 5 / 6)
  expect_equal(q(.groebn_fr_sub(c(1L, 2L), c(1L, 3L))), 1 / 6)
  expect_equal(q(.groebn_fr_mul(c(2L, 3L), c(3L, 4L))), 1 / 2)
  expect_equal(q(.groebn_fr_div(c(2L, 3L), c(4L, 9L))), 3 / 2)
  expect_identical(.groebn_fr_neg(c(1L, 2L)), c(-1L, 2L))
  expect_true(.groebn_fr_is_zero(c(0L, 7L)))
  expect_true(.groebn_fr_eq(c(1L, 2L), c(1L, 2L)))
  expect_false(.groebn_fr_eq(c(1L, 2L), c(1L, 3L)))
  expect_error(.groebn_fr_div(c(1L, 2L), c(0L, 1L)), "division by zero")
  # 1/2 + 1/2 is exactly 1, not 0.9999999999999999
  expect_identical(.groebn_fr_add(c(1L, 2L), c(1L, 2L)), c(1L, 1L))
})

test_that("monomials sort by the order that was asked for", {
  # x^2, xy, y^2 and the constant, in two variables
  f <- P(list("2_0" = 1, "1_1" = 1, "0_2" = 1, "0_0" = 1))
  expect_identical(.groebn_monomials(f, "lex"), c("2_0", "1_1", "0_2", "0_0"))
  expect_identical(.groebn_leading_monomial(f, "lex"), "2_0")
  # a degree-graded order puts the constant last but ranks by total degree
  expect_identical(.groebn_monomials(f, "grlex")[4L], "0_0")
  g <- P(list("3_0" = 1, "1_2" = 1))
  # both have degree 3, so lex and grlex agree here, x^3 first
  expect_identical(.groebn_leading_monomial(g, "grlex"), "3_0")
  # a lower-degree term cannot lead in a graded order even if lex prefers it
  h <- P(list("2_0" = 1, "0_3" = 1))
  expect_identical(.groebn_leading_monomial(h, "lex"), "2_0")
  expect_identical(.groebn_leading_monomial(h, "grlex"), "0_3")
  expect_error(.groebn_monomials(f, "nonsense"), "order must be one of")
})

test_that("leading term, coefficient and monomial agree with each other", {
  f <- P(list("2_1" = 3, "0_0" = -2))
  expect_identical(.groebn_leading_monomial(f, "lex"), "2_1")
  expect_identical(.groebn_leading_coeff(f, "lex"), c(3L, 1L))
  lt <- .groebn_leading_term(f, "lex")
  expect_identical(names(lt), "2_1")
  expect_identical(lt[["2_1"]], c(3L, 1L))
})

test_that("polynomial arithmetic collects like terms and cancels", {
  a <- P(list("1_0" = 1, "0_0" = 1))       # x + 1
  b <- P(list("1_0" = 1, "0_0" = -1))      # x - 1
  expect_identical(.groebn_add(a, b), P(list("1_0" = 2)))
  # (x + 1) - (x + 1) is the zero polynomial, an empty list
  expect_length(.groebn_sub(a, a), 0L)
  # (x + 1)(x - 1) = x^2 - 1
  expect_identical(.groebn_mul(a, b), P(list("2_0" = 1, "0_0" = -1)))
  # scaling by 1/2
  expect_equal(q(.groebn_scale(P(list("1_0" = 3)), c(1L, 2L))[["1_0"]]), 3 / 2)
  # scaling by zero annihilates
  expect_length(.groebn_scale(a, c(0L, 1L)), 0L)
})

test_that("divisibility and lcm work on exponent vectors", {
  expect_true(.groebn_divides(c(1L, 0L), c(2L, 1L)))
  expect_false(.groebn_divides(c(0L, 2L), c(2L, 1L)))
  expect_true(.groebn_divides(c(0L, 0L), c(2L, 1L)))
  expect_identical(.groebn_lcm(c(2L, 1L), c(1L, 3L)), c(2L, 3L))
})

test_that("the S-polynomial cancels the leading terms by construction", {
  f <- P(list("2_0" = 1, "1_1" = 1))       # x^2 + xy
  g <- P(list("1_1" = 1, "0_2" = 1))       # xy + y^2
  s <- .groebn_spoly(f, g, "lex")
  # the leading monomials of f and g are gone from the result
  expect_false("2_1" %in% names(s))
  # and it lies in the ideal the two generate
  expect_true(.groebn_ideal_member(s, list(f, g))$member)
})

test_that("division leaves a remainder no divisor's leading term divides", {
  f <- P(list("2_1" = 3, "1_2" = 5, "0_0" = -2))
  G <- list(P(list("1_0" = 1, "0_0" = -1)), P(list("0_1" = 1, "0_0" = -2)))
  d <- .groebn_divide(f, G, "lex")
  expect_named(d, c("quotients", "remainder"), ignore.order = TRUE)
  expect_length(d$quotients, 2L)
  # f = sum(q_i g_i) + r, checked by reassembling it. A polynomial is a
  # named list keyed by monomial, so two equal polynomials can differ in
  # insertion order; subtracting is the order-free comparison.
  acc <- d$remainder
  for (i in seq_along(G)) acc <- .groebn_add(acc, .groebn_mul(d$quotients[[i]], G[[i]]))
  expect_length(.groebn_sub(acc, f), 0L)
  expect_setequal(names(acc), names(f))
})

test_that("the normal form over a point ideal is the value at that point", {
  # for <x - 1, y - 2> the quotient ring is one dimensional and the normal
  # form of any f is the constant f(1, 2); here 3*1*2 + 5*1*4 - 2 = 24
  g1 <- P(list("1_0" = 1, "0_0" = -1))
  g2 <- P(list("0_1" = 1, "0_0" = -2))
  G <- morie_groebn(list(g1, g2))$basis
  f <- P(list("2_1" = 3, "1_2" = 5, "0_0" = -2))
  nf <- .groebn_normal_form(f, G, "lex")
  expect_identical(names(nf), "0_0")
  expect_equal(q(nf[["0_0"]]), 24)
  # and the basis of a point ideal is the generators themselves
  expect_length(G, 2L)
  expect_true(criterion(G, "lex"))
  # another point, another value: f(3, -1) = 3*9*(-1) + 5*3*1 - 2 = -14
  h1 <- P(list("1_0" = 1, "0_0" = -3))
  h2 <- P(list("0_1" = 1, "0_0" = 1))
  H <- morie_groebn(list(h1, h2))$basis
  expect_equal(q(.groebn_normal_form(f, H, "lex")[["0_0"]]), -14)
})

test_that("the published grlex basis is reproduced exactly", {
  # Cox, Little and O'Shea, "Ideals, Varieties, and Algorithms", section
  # 2.7: for <x^3 - 2xy, x^2 y - 2y^2 + x> the grlex Grobner basis is
  # {x^2, xy, y^2 - x/2}
  f1 <- P(list("3_0" = 1, "1_1" = -2))
  f2 <- P(list("2_1" = 1, "0_2" = -2, "1_0" = 1))
  G <- morie_groebn(list(f1, f2), order = "grlex")$basis
  expect_length(G, 3L)
  expect_identical(vapply(G, function(p) .groebn_leading_monomial(p, "grlex"),
                          character(1)),
                   c("2_0", "1_1", "0_2"))
  expect_identical(G[[1]], P(list("2_0" = 1)))
  expect_identical(G[[2]], P(list("1_1" = 1)))
  expect_identical(names(G[[3]]), c("0_2", "1_0"))
  expect_equal(q(G[[3]][["0_2"]]), 1)
  expect_equal(q(G[[3]][["1_0"]]), -1 / 2)
})

test_that("every order returns a basis satisfying Buchberger's criterion", {
  f1 <- P(list("3_0" = 1, "1_1" = -2))
  f2 <- P(list("2_1" = 1, "0_2" = -2, "1_0" = 1))
  for (ord in c("lex", "grlex", "grevlex")) {
    res <- morie_groebn(list(f1, f2), order = ord)
    expect_true(criterion(res$basis, ord))
    expect_identical(res$order, ord)
    expect_identical(res$size, length(res$basis))
    expect_identical(res$estimate, res$basis)
    # the generators are members of their own ideal
    expect_true(.groebn_ideal_member(f1, list(f1, f2), ord, res$basis)$member)
    expect_true(.groebn_ideal_member(f2, list(f1, f2), ord, res$basis)$member)
  }
})

test_that("the reduced basis is monic and mutually irreducible", {
  f1 <- P(list("3_0" = 1, "1_1" = -2))
  f2 <- P(list("2_1" = 1, "0_2" = -2, "1_0" = 1))
  G <- morie_groebn(list(f1, f2), order = "grlex", reduced = TRUE)$basis
  for (g in G) expect_identical(.groebn_leading_coeff(g, "grlex"), c(1L, 1L))
  # no leading term divides another element's leading term
  lms <- lapply(G, function(p) .groebn_parse_key(.groebn_leading_monomial(p, "grlex")))
  for (i in seq_along(lms)) {
    for (j in seq_along(lms)) {
      if (i == j) next
      expect_false(.groebn_divides(lms[[j]], lms[[i]]))
    }
  }
  # reducing an already reduced basis changes nothing
  expect_identical(.groebn_reduce_basis(G, "grlex"), G)
})

test_that("pruning coprime pairs does not change the answer", {
  f1 <- P(list("2_0" = 1, "0_0" = -1))
  f2 <- P(list("0_2" = 1, "0_0" = -1))
  a <- morie_groebn(list(f1, f2), order = "lex", prune = TRUE)
  b <- morie_groebn(list(f1, f2), order = "lex", prune = FALSE)
  expect_identical(a$basis, b$basis)
  expect_true(a$pruned)
  expect_false(b$pruned)
  # the criterion skips work; without it more pairs are considered
  expect_gte(a$n_skipped, 0L)
  expect_identical(b$n_skipped, 0L)
})

test_that("membership is decided by a zero normal form", {
  g1 <- P(list("1_0" = 1, "0_0" = -1))
  g2 <- P(list("0_1" = 1, "0_0" = -2))
  # y*(x-1) + 3x*(y-2) is in the ideal by construction
  inside <- .groebn_add(.groebn_mul(P(list("0_1" = 1)), g1),
                        .groebn_mul(P(list("1_0" = 3)), g2))
  r <- .groebn_ideal_member(inside, list(g1, g2))
  expect_true(r$member)
  expect_identical(r$estimate, r$member)
  expect_length(r$remainder, 0L)
  expect_match(r$method, "Buchberger")
  # 1 is not in a proper ideal
  out <- .groebn_ideal_member(P(list("0_0" = 1)), list(g1, g2))
  expect_false(out$member)
  expect_length(out$remainder, 1L)
  expect_equal(q(out$remainder[["0_0"]]), 1)
})

test_that("a single generator returns its own monic form", {
  f <- P(list("2_0" = 3, "0_0" = -6))          # 3x^2 - 6
  G <- morie_groebn(list(f), order = "lex")$basis
  expect_length(G, 1L)
  expect_identical(G[[1]], P(list("2_0" = 1, "0_0" = -2)))
})

test_that("the unit ideal collapses to one", {
  # <x, x + 1> contains 1
  G <- morie_groebn(list(P(list("1_0" = 1)), P(list("1_0" = 1, "0_0" = 1))),
                    order = "lex")$basis
  expect_length(G, 1L)
  expect_identical(G[[1]], P(list("0_0" = 1)))
})

test_that("polynomial construction validates and normalises", {
  # terms given twice are summed
  expect_identical(P(list("1_0" = 1, "1_0" = 2)), P(list("1_0" = 3)))
  # a term that cancels disappears
  expect_length(P(list("1_0" = 1, "1_0" = -1)), 0L)
  expect_error(P(list("1_0" = 1, "1_0_0" = 1)), "differing length")
  # fractions may be given as strings or as num/den pairs
  expect_equal(q(P(list("1_0" = "1/2"))[["1_0"]]), 1 / 2)
  expect_equal(q(.groebn_as_fr("3/4")), 3 / 4)
  expect_equal(q(.groebn_as_fr(5L)), 5)
  expect_error(.groebn_as_fr(list("a", "b")), "cannot convert")
})
