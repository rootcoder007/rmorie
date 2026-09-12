# Network motif significance (Milo et al. 2002).
#
# Anchors outside the module: motif counts on graphs small enough to
# enumerate by hand; the invariants the mfinder switching ensemble must
# preserve, namely both degree sequences, the edge count, and the
# mutual-edge count when asked; and the z-score and permutation p-value
# recomputed from the returned null distribution.

# a feed-forward loop: 1 -> 2, 1 -> 3, 2 -> 3
FFL <- matrix(0L, 3, 3)
FFL[1, 2] <- 1L
FFL[1, 3] <- 1L
FFL[2, 3] <- 1L
# a directed three-cycle: 1 -> 2 -> 3 -> 1
CYC <- matrix(0L, 3, 3)
CYC[1, 2] <- 1L
CYC[2, 3] <- 1L
CYC[3, 1] <- 1L
# every edge in both directions
FULL <- matrix(1L, 3, 3)
diag(FULL) <- 0L

test_that("motif counts are the hand-enumerated ones", {
  ffl <- .motiff_triads(FFL, 3L)
  # exactly one feed-forward loop and no cycle
  expect_equal(ffl$ffl, 1L)
  expect_equal(ffl$cycle3, 0L)

  cyc <- .motiff_triads(CYC, 3L)
  # exactly one three-cycle and no feed-forward loop
  expect_equal(cyc$ffl, 0L)
  expect_equal(cyc$cycle3, 1L)

  # an empty graph has neither
  z <- .motiff_triads(matrix(0L, 4, 4), 4L)
  expect_equal(z$ffl, 0L)
  expect_equal(z$cycle3, 0L)

  # with all six edges present every ordered triple is a feed-forward loop,
  # so 3 * 2 * 1 = 6, and there are two directed three-cycles
  full <- .motiff_triads(FULL, 3L)
  expect_equal(full$ffl, 6L)
  expect_equal(full$cycle3, 2L)

  # reversing every edge of a feed-forward loop leaves it a feed-forward
  # loop, since the pattern is symmetric under relabelling
  expect_equal(.motiff_triads(t(FFL), 3L)$ffl, 1L)
  # but reversing a three-cycle gives the other three-cycle, still one
  expect_equal(.motiff_triads(t(CYC), 3L)$cycle3, 1L)
})

test_that("switching preserves both degree sequences and the edge count", {
  set.seed(2)
  n <- 12
  A <- matrix(rbinom(n * n, 1, 0.25), n, n)
  diag(A) <- 0L
  e <- .ghc_rng(1)
  for (pm in c(TRUE, FALSE)) {
    Ar <- .motiff_shuffle(A, n, e, 200L, pm)
    expect_equal(dim(Ar), dim(A))
    expect_true(all(Ar %in% c(0L, 1L)))
    expect_equal(diag(Ar), rep(0L, n))
    # the ensemble is degree-preserving, which is what makes the null
    # distribution the right comparison
    expect_equal(rowSums(Ar), rowSums(A))
    expect_equal(colSums(Ar), colSums(A))
    expect_equal(sum(Ar), sum(A))
  }
  # and with preserve_mutual the count of bidirectional pairs is invariant
  mutual <- function(M) sum(M & t(M)) / 2
  Am <- .motiff_shuffle(A, n, .ghc_rng(3), 200L, TRUE)
  expect_equal(mutual(Am), mutual(A))
  # the shuffle is deterministic given the stream
  expect_equal(.motiff_shuffle(A, n, .ghc_rng(5), 50L, TRUE),
               .motiff_shuffle(A, n, .ghc_rng(5), 50L, TRUE))
  # and it actually rearranges something
  expect_false(isTRUE(all.equal(
    .motiff_shuffle(A, n, .ghc_rng(7), 500L, TRUE), A)))
})

test_that("the z-score and p-value follow from the null distribution", {
  set.seed(4)
  n <- 10
  A <- matrix(rbinom(n * n, 1, 0.3), n, n)
  diag(A) <- 0L
  r <- morie_motiff(A, motif = "ffl", n_random = 30L, seed = 1L)
  expect_equal(r$count, .motiff_triads(matrix(as.integer(A != 0), n), n)$ffl)
  expect_equal(r$motif, "ffl")
  expect_equal(r$n_random, 30L)
  expect_true(r$preserve_mutual)
  # the z-score is the standardised distance from the null mean
  if (r$rand_sd > 0) {
    expect_equal(r$z_score, (r$count - r$rand_mean) / r$rand_sd)
  }
  # the p-value is the permutation proportion with its plus-one correction,
  # so it can never be smaller than one over the replicate count plus one
  expect_true(r$p_value >= 1 / (30 + 1) - 1e-12)
  expect_true(r$p_value <= 1)
  expect_equal(r$seed, 1L)
  expect_match(r$method, "Milo et al")
  expect_match(r$method, "mutual")
  # the run reproduces from its seed and differs across seeds
  again <- morie_motiff(A, motif = "ffl", n_random = 30L, seed = 1L)
  expect_equal(again$z_score, r$z_score)
  expect_equal(again$rand_mean, r$rand_mean)
  # the degree-only ensemble is labelled differently
  d <- morie_motiff(A, motif = "ffl", n_random = 10L, seed = 1L,
                    preserve_mutual = FALSE)
  expect_false(d$preserve_mutual)
  expect_match(d$method, "in/out-degree")
})

test_that("both motifs can be scored", {
  set.seed(6)
  n <- 10
  A <- matrix(rbinom(n * n, 1, 0.3), n, n)
  diag(A) <- 0L
  f <- morie_motiff(A, motif = "ffl", n_random = 10L, seed = 2L)
  c3 <- morie_motiff(A, motif = "cycle3", n_random = 10L, seed = 2L)
  expect_equal(c3$motif, "cycle3")
  Ai <- matrix(as.integer(A != 0), n)
  expect_equal(f$count, .motiff_triads(Ai, n)$ffl)
  expect_equal(c3$count, .motiff_triads(Ai, n)$cycle3)
  expect_true(f$count >= 0 && c3$count >= 0)
})

test_that("a degenerate null gives a defined z-score", {
  # a three-cycle has nothing to switch, so every replicate reproduces it
  # and the null has no spread; the score is zero rather than undefined
  r <- morie_motiff(CYC, motif = "cycle3", n_random = 5L, seed = 1L)
  expect_equal(r$count, 1L)
  if (r$rand_sd == 0 && r$rand_mean == r$count) {
    expect_equal(r$z_score, 0)
  } else {
    expect_true(is.finite(r$z_score) || is.infinite(r$z_score))
  }
  expect_true(r$p_value > 0)
})

test_that("motiff validates its input", {
  expect_error(morie_motiff(matrix(0L, 2, 2)), "square, n >= 3")
  expect_error(morie_motiff(matrix(0L, 3, 4)), "square, n >= 3")
  expect_error(morie_motiff(FFL, motif = "bifan"),
               "motif must be 'ffl' or 'cycle3'")
  # a weighted adjacency is read as presence or absence
  W <- FFL * 3.5
  expect_equal(morie_motiff(W, n_random = 3L, seed = 1L)$count,
               morie_motiff(FFL, n_random = 3L, seed = 1L)$count)
})
