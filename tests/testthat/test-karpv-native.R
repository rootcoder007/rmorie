# Genetic programming over expression trees (Koza 1992).
#
# Anchors outside the module: the arithmetic of the function set, including
# Koza's protected division; the structural identities the tree surgery
# must satisfy, chiefly that replacing a subtree with itself leaves the
# tree unchanged; and a symbolic-regression target simple enough that the
# search has to find it exactly.

# ((x * x) + x), built by hand
TREE <- .karpv_fnode("+", list(
  .karpv_fnode("*", list(.karpv_tnode("x"), .karpv_tnode("x"))),
  .karpv_tnode("x")
))

test_that("the function set is ordinary arithmetic, division protected", {
  expect_equal(.karpv_apply("+", c(2, 3)), 5)
  expect_equal(.karpv_apply("-", c(2, 3)), -1)
  expect_equal(.karpv_apply("*", c(2, 3)), 6)
  expect_equal(.karpv_apply("%", c(6, 3)), 2)
  # Koza's protected division: a zero divisor returns one, so a single bad
  # division does not discard an otherwise good program
  expect_equal(.karpv_apply("%", c(6, 0)), 1)
  expect_equal(.karpv_apply("%", c(0, 0)), 1)
  expect_equal(.karpv_apply("%", c(-4, 0)), 1)
  expect_error(.karpv_apply("^", c(2, 3)), "unknown function")
})

test_that("nodes carry either an operator or a terminal", {
  f <- .karpv_fnode("+", list(.karpv_tnode("x"), .karpv_tnode(1)))
  t <- .karpv_tnode("x")
  expect_false(.karpv_is_term(f))
  expect_true(.karpv_is_term(t))
  expect_equal(f$op, "+")
  expect_length(f$args, 2L)
  expect_equal(t$term, "x")
  # a numeric constant is as valid a terminal as a variable
  expect_true(.karpv_is_term(.karpv_tnode(3.5)))
  expect_equal(.karpv_tnode(3.5)$term, 3.5)
})

test_that("evaluation walks the tree", {
  for (x in c(-2, 0, 1, 3)) {
    expect_equal(morie_karpV_evaluate(TREE, list(x = x)), x * x + x)
  }
  # a bare terminal evaluates to its binding
  expect_equal(morie_karpV_evaluate(.karpv_tnode("x"), list(x = 7)), 7)
  # a constant ignores the environment
  expect_equal(morie_karpV_evaluate(.karpv_tnode(2.5), list(x = 7)), 2.5)
  # protected division shows through the evaluator
  dz <- .karpv_fnode("%", list(.karpv_tnode("x"), .karpv_tnode(0)))
  expect_equal(morie_karpV_evaluate(dz, list(x = 9)), 1)
})

test_that("raw fitness is the summed absolute error", {
  cases <- lapply(c(-2, -1, 0, 1, 2),
                  function(x) list(x, x * x + x))
  # the exact program has no error at all
  expect_equal(morie_karpV_raw_fitness(TREE, cases, "x"), 0)
  # a program off by a constant accumulates one unit per case
  off <- .karpv_fnode("+", list(TREE, .karpv_tnode(1)))
  expect_equal(morie_karpV_raw_fitness(off, cases, "x"), length(cases))
  # the bare terminal misses by x squared on each case
  bare <- .karpv_tnode("x")
  expect_equal(morie_karpV_raw_fitness(bare, cases, "x"),
               sum(abs(c(-2, -1, 0, 1, 2)^2)))
  # a program that cannot produce a finite value is rejected outright
  huge <- .karpv_tnode(Inf)
  expect_equal(morie_karpV_raw_fitness(huge, cases, "x"), Inf)
})

test_that("collecting a tree enumerates every node once", {
  nodes <- .karpv_collect(TREE, integer(0), list())
  # the tree has five nodes: +, *, x, x, x
  expect_length(nodes, 5L)
  # the first is the root, reached by the empty path
  expect_length(nodes[[1]]$path, 0L)
  expect_false(.karpv_is_term(nodes[[1]]$node))
  # three of the five are terminals
  expect_equal(sum(vapply(nodes, function(p) .karpv_is_term(p$node),
                          logical(1))), 3L)
  # every recorded path really leads to the recorded node
  for (p in nodes) {
    expect_equal(.karpv_get(TREE, p$path)$term, p$node$term)
  }
  # a bare terminal is a one-node tree
  expect_length(.karpv_collect(.karpv_tnode("x"), integer(0), list()), 1L)
})

test_that("getting and replacing a subtree are inverse", {
  # the left child of the root is (x * x)
  sub <- .karpv_get(TREE, 1L)
  expect_equal(sub$op, "*")
  expect_equal(morie_karpV_evaluate(sub, list(x = 4)), 16)
  # the right child is the bare terminal
  expect_true(.karpv_is_term(.karpv_get(TREE, 2L)))
  # a deeper path reaches a leaf of the product
  expect_true(.karpv_is_term(.karpv_get(TREE, c(1L, 1L))))

  # replacing a subtree with itself leaves the tree unchanged, which is the
  # identity the crossover depends on
  nodes <- .karpv_collect(TREE, integer(0), list())
  for (p in nodes) {
    rt <- .karpv_replace(TREE, p$path, .karpv_get(TREE, p$path))
    for (x in c(-1, 2, 5)) {
      expect_equal(morie_karpV_evaluate(rt, list(x = x)),
                   morie_karpV_evaluate(TREE, list(x = x)))
    }
  }
  # replacing the root replaces the whole tree
  whole <- .karpv_replace(TREE, integer(0), .karpv_tnode(3))
  expect_equal(morie_karpV_evaluate(whole, list(x = 9)), 3)
  # replacing the right child with a constant changes only that term
  swapped <- .karpv_replace(TREE, 2L, .karpv_tnode(10))
  expect_equal(morie_karpV_evaluate(swapped, list(x = 4)), 16 + 10)
  # and the original is untouched, since the tree is rebuilt not mutated
  expect_equal(morie_karpV_evaluate(TREE, list(x = 4)), 20)
})

test_that("copying a tree preserves it", {
  cp <- .karpv_copy(TREE)
  for (x in c(-3, 0, 4)) {
    expect_equal(morie_karpV_evaluate(cp, list(x = x)),
                 morie_karpV_evaluate(TREE, list(x = x)))
  }
  expect_equal(length(.karpv_collect(cp, integer(0), list())),
               length(.karpv_collect(TREE, integer(0), list())))
  # a terminal copies to a terminal
  expect_true(.karpv_is_term(.karpv_copy(.karpv_tnode("x"))))
})

test_that("the random stream is deterministic and in range", {
  e1 <- .karpv_rng(12345)
  a <- vapply(1:50, function(i) .karpv_u32(e1), numeric(1))
  expect_true(all(a >= 0 & a < 2^32))
  expect_true(all(a == floor(a)))
  # the same seed replays the same stream
  e2 <- .karpv_rng(12345)
  b <- vapply(1:50, function(i) .karpv_u32(e2), numeric(1))
  expect_equal(a, b)
  # a different seed does not
  e3 <- .karpv_rng(999)
  expect_false(isTRUE(all.equal(a,
    vapply(1:50, function(i) .karpv_u32(e3), numeric(1)))))
  # the unit variate lies in the half-open unit interval
  e4 <- .karpv_rng(7)
  u <- vapply(1:500, function(i) .karpv_unit(e4), numeric(1))
  expect_true(all(u >= 0 & u < 1))
  expect_equal(mean(u), 0.5, tolerance = 0.06)
  # bounded draws stay in range, and a degenerate bound is always zero
  e5 <- .karpv_rng(3)
  d <- vapply(1:400, function(i) .karpv_below(e5, 5L), numeric(1))
  expect_true(all(d >= 0 & d < 5))
  expect_setequal(unique(d), 0:4)
  expect_equal(.karpv_below(.karpv_rng(1), 1L), 0)
  expect_equal(.karpv_below(.karpv_rng(1), 0L), 0)
})

test_that("roulette selection follows the fitness shares", {
  e <- .karpv_rng(5)
  # all the mass on the third entry, so it is always chosen; the index is
  # reported zero-based
  expect_equal(.karpv_roulette(e, c(0, 0, 1, 0), 1), 2L)
  for (i in 1:20) expect_equal(.karpv_roulette(e, c(0, 0, 1, 0), 1), 2L)
  # the first entry taking everything is likewise deterministic
  expect_equal(.karpv_roulette(e, c(1, 0), 1), 0L)
  # with no fitness at all the choice falls back on a uniform draw
  e2 <- .karpv_rng(11)
  fb <- vapply(1:200, function(i) .karpv_roulette(e2, c(0, 0, 0), 0),
               numeric(1))
  expect_true(all(fb >= 0 & fb < 3))
  expect_setequal(unique(fb), 0:2)
  # an even split picks both sides
  e3 <- .karpv_rng(13)
  ev <- vapply(1:200, function(i) .karpv_roulette(e3, c(1, 1), 2), numeric(1))
  expect_setequal(unique(ev), 0:1)
  expect_equal(mean(ev), 0.5, tolerance = 0.12)
})

test_that("grown trees respect their depth budget", {
  e <- .karpv_rng(21)
  depth <- function(nd) {
    if (.karpv_is_term(nd)) return(1L)
    1L + max(vapply(nd$args, depth, integer(1)))
  }
  fns <- .KARPV_FUNCTIONS
  # a depth budget of one admits only a terminal
  expect_true(.karpv_is_term(.karpv_grow(e, fns, "x", NULL, 1L, TRUE)))
  # a full tree reaches exactly the budget
  for (d in 2:5) {
    tr <- .karpv_grow(e, fns, "x", NULL, as.integer(d), TRUE)
    expect_equal(depth(tr), as.integer(d))
  }
  # a grown tree may stop early but never exceeds the budget
  for (i in 1:20) {
    tr <- .karpv_grow(e, fns, "x", NULL, 4L, FALSE)
    expect_true(depth(tr) <= 4L)
  }
  # an ephemeral random constant lands inside its range
  e2 <- .karpv_rng(4)
  for (i in 1:40) {
    nd <- .karpv_random_terminal(e2, character(0), c(-2, 2))
    expect_true(.karpv_is_term(nd))
    expect_true(nd$term >= -2 && nd$term <= 2)
  }
  # with no constants the terminal is drawn from the named set
  e3 <- .karpv_rng(6)
  for (i in 1:20) {
    expect_true(.karpv_random_terminal(e3, c("x", "y"), NULL)$term %in%
                  c("x", "y"))
  }
})

test_that("the crossover point respects the internal-node bias", {
  e <- .karpv_rng(31)
  # a bias of one always picks an internal node when the tree has any
  for (i in 1:20) {
    p <- .karpv_pick_point(e, TREE, internal_bias = 1)
    expect_false(.karpv_is_term(.karpv_get(TREE, p)))
  }
  # a bias of zero always picks a leaf
  for (i in 1:20) {
    p <- .karpv_pick_point(e, TREE, internal_bias = 0)
    expect_true(.karpv_is_term(.karpv_get(TREE, p)))
  }
  # a one-node tree has no internal node, so the leaf is returned whatever
  # the bias asks for
  leafp <- .karpv_pick_point(e, .karpv_tnode("x"), internal_bias = 1)
  expect_length(leafp, 0L)
})

test_that("the search finds a program it can express exactly", {
  # the target is the identity, for which the bare terminal is a perfect
  # program, so the search must reach zero error
  cases <- lapply(seq(-3, 3, by = 0.5), function(x) list(x, x))
  r <- morie_karpV(cases = cases, terminals = "x", gens = 5L,
                   pop_size = 40L, max_depth_init = 3L, seed = 2L)
  # it finds the identity exactly: the program is the bare terminal
  expect_equal(r$best_raw, 0)
  expect_equal(r$best_string, "x")
  expect_equal(r$best_size, 1)
  expect_equal(r$best_depth, 1)
  # Koza's adjusted fitness is 1 / (1 + raw), so a perfect program scores 1
  expect_equal(r$best_adjusted, 1 / (1 + r$best_raw))
  expect_equal(r$best_adjusted, 1)
  # the reported program really achieves the reported fitness
  expect_equal(morie_karpV_raw_fitness(r$best, cases, "x"), r$best_raw)
  expect_true(r$generation_found >= 0)
  expect_equal(r$pop_size, 40L)
  expect_equal(r$generations, 5L)
  expect_equal(r$seed, 2L)
  expect_true(r$evaluations > 0)
  expect_match(r$method, "Koza 1992")
  # the run reproduces from its seed
  again <- morie_karpV(cases = cases, terminals = "x", gens = 5L,
                       pop_size = 40L, max_depth_init = 3L, seed = 2L)
  expect_equal(again$best_raw, r$best_raw)
  expect_equal(again$best_string, r$best_string)
  # the best adjusted fitness recorded never falls across generations
  expect_length(r$history, 6L)
  h <- vapply(r$history, function(z) as.numeric(z)[3], numeric(1))
  expect_true(all(diff(h) >= -1e-12))
})

test_that("a harder target improves over the generations", {
  cases <- lapply(seq(-2, 2, by = 0.25), function(x) list(x, x * x + x))
  r <- morie_karpV(cases = cases, terminals = "x", gens = 8L,
                   pop_size = 60L, max_depth_init = 4L, seed = 7L)
  expect_true(is.finite(r$best_raw))
  expect_equal(morie_karpV_raw_fitness(r$best, cases, "x"), r$best_raw)
  expect_equal(r$best_adjusted, 1 / (1 + r$best_raw))
  # the best-so-far adjusted fitness is monotone by construction, and the
  # search ends no worse than it started
  h <- vapply(r$history, function(z) as.numeric(z)[3], numeric(1))
  expect_true(all(diff(h) >= -1e-12))
  expect_true(h[length(h)] >= h[1])
})

test_that("karpV needs something to optimise", {
  expect_error(morie_karpV(gens = 2L),
               "give either a fitness function or fitness cases")
  expect_error(morie_karpV(cases = list(), gens = 2L),
               "give either a fitness function or fitness cases")
  # an explicit fitness function is accepted in place of cases
  f <- function(tree) abs(morie_karpV_evaluate(tree, list(x = 1)) - 1)
  r <- morie_karpV(fitness = f, terminals = "x", gens = 2L, pop_size = 20L,
                   max_depth_init = 2L, seed = 3L)
  expect_true(is.finite(r$best_raw))
  expect_equal(f(r$best), r$best_raw)
})
