# Coverage tests for the tree helpers of R/karpV_native.R (Koza 1992):
# the xorshift32 stream, depth/size/printing, adjusted fitness and
# ramped half-and-half initialisation.

kv_tree <- function() {
  .karpv_fnode("+", list(.karpv_tnode("x"), .karpv_fnode("*", list(.karpv_tnode(2), .karpv_tnode("y")))))
}

test_that("the stream is Marsaglia xorshift32 (13, 17, 5)", {
  bits <- function(a) (a %/% 2^(0:31)) %% 2
  x32 <- function(a, b) sum(2^(0:31) * xor(bits(a), bits(b)))
  s <- 12345
  e <- .karpv_rng(12345)
  for (k in 1:5) {
    s <- x32(s, (s * 2^13) %% 2^32)
    s <- x32(s, s %/% 2^17)
    s <- x32(s, (s * 2^5) %% 2^32)
    expect_equal(.karpv_u32(e), s)
  }
  expect_equal(.karpv_u32(.karpv_rng(1)), 270369)
  expect_equal(.karpv_rng(0)$s, 2463534242)
  expect_equal(.karpv_below(e, 1), 0)
  expect_true(all(replicate(20, .karpv_below(e, 7)) %in% 0:6))
})

test_that("depth, size and prefix printing", {
  t <- kv_tree()
  expect_equal(morie_karpV_depth(t), 3L)
  expect_equal(morie_karpV_size(t), 5L)
  expect_equal(morie_karpV_to_string(t), "(+ x (* 2 y))")
  expect_equal(morie_karpV_to_string(.karpv_tnode(0.1)), sprintf("%.17g", 0.1))
  expect_equal(morie_karpV_depth(.karpv_tnode("x")), 1L)
  expect_equal(morie_karpV_evaluate(t, list(x = 1.5, y = -2)), 1.5 + 2 * -2)
})

test_that("adjusted fitness is 1 / (1 + raw), zero for a failed program", {
  expect_equal(morie_karpV_adjusted(0), 1)
  expect_equal(morie_karpV_adjusted(3), 0.25)
  expect_equal(morie_karpV_adjusted(Inf), 0)
  expect_equal(morie_karpV_adjusted(NaN), 0)
})

test_that("ramped half-and-half cycles depths and alternates full and grow", {
  fs <- .KARPV_FUNCTIONS
  e <- .karpv_rng(7)
  pop <- morie_karpV_ramped(e, 12, fs, c("x", "y"), c(-1, 1), 4L)
  d <- vapply(pop, morie_karpV_depth, 1L)
  target <- 2 + (0:11) %% 3
  full <- ((0:11) %/% 3) %% 2 == 1
  expect_equal(d[full], target[full])
  expect_true(all(d[!full] <= target[!full]))
  # every operator is binary, so a full tree of depth d has 2^d - 1 nodes
  expect_equal(vapply(pop[full], morie_karpV_size, 1L), as.integer(2^target[full] - 1))
  again <- morie_karpV_ramped(.karpv_rng(7), 12, fs, c("x", "y"), c(-1, 1), 4L)
  expect_identical(again, pop)
  one <- morie_karpV_ramped(.karpv_rng(3), 4, fs, "x", NULL, 2L)
  expect_true(all(vapply(one, morie_karpV_depth, 1L) <= 2L))
  leaves <- unlist(lapply(one, function(t) if (.karpv_is_term(t)) t$term else lapply(t$args, `[[`, "term")))
  expect_true(all(leaves == "x"))
  expect_error(morie_karpV(), "fitness function or fitness cases")
  expect_error(morie_karpV(function(t) 0, max_depth_init = 1L), "needs at least 2")
})
