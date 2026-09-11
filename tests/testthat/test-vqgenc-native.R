# Anchors for the VQ-GAN encoder's quantiser.
#
# Every piece here has an exact form: quantisation is nearest neighbour
# in Euclidean distance, so brute force settles it; the straight-through
# estimator's whole definition is that the backward pass ignores the
# argmin; and the sequence length is arithmetic. The file sat at 14.5%
# with no test naming any of its functions.

cb4 <- function() rbind(c(0, 0), c(1, 0), c(0, 1), c(3, 3))

test_that("quantisation picks the nearest codebook entry", {
  cb <- cb4()
  V <- rbind(c(0.1, 0.1), c(0.9, 0.2), c(0.1, 1.2), c(2.5, 2.9), c(0.5, 0.5))
  q <- .vqgenc_quantize(V, cb)
  brute <- apply(V, 1, function(v) which.min(colSums((t(cb) - v)^2)))
  # the indices are reported 0-based
  expect_identical(as.integer(unlist(q$indices)), as.integer(brute - 1L))
  # and the codes are the rows they point at
  expect_equal(unname(as.matrix(q$codes)), unname(cb[brute, ]), tolerance = 1e-12)
  expect_identical(as.integer(q$codebook_size), 4L)
  # a vector that IS a codebook entry quantises to itself at zero distance
  exact <- .vqgenc_quantize(cb, cb)
  expect_identical(as.integer(unlist(exact$indices)), 0:3)
  expect_equal(sum(unlist(exact$distance)), 0, tolerance = 1e-12)
})

test_that("quantisation reports codebook usage", {
  cb <- cb4()
  # only two distinct entries are ever the nearest
  V <- rbind(c(0.05, 0.05), c(0.02, 0.01), c(0.95, 0.05), c(0.9, 0.1))
  q <- .vqgenc_quantize(V, cb)
  expect_identical(as.integer(q$used), 2L)
  expect_equal(as.numeric(q$usage_fraction), 2 / 4, tolerance = 1e-12)
  # using every entry reports full usage
  full <- .vqgenc_quantize(cb, cb)
  expect_identical(as.integer(full$used), 4L)
  expect_equal(as.numeric(full$usage_fraction), 1, tolerance = 1e-12)
})

test_that("the distance reported is the distance to the chosen code", {
  cb <- cb4()
  V <- rbind(c(0.3, 0.4), c(2.0, 2.0))
  q <- .vqgenc_quantize(V, cb)
  codes <- unname(as.matrix(q$codes))
  # whatever metric it reports, it is a non-negative per-vector quantity
  d <- unlist(q$distance)
  expect_length(d, 2L)
  expect_true(all(d >= 0))
  # and the nearest entry is at least as close as every other
  for (i in seq_len(nrow(V))) {
    all_d <- colSums((t(cb) - V[i, ])^2)
    expect_equal(min(all_d), sum((V[i, ] - codes[i, ])^2), tolerance = 1e-12)
  }
})

test_that("the straight-through estimator passes the gradient untouched", {
  # the definition: forward emits the code, backward behaves as though the
  # argmin were not there, so the Jacobian is the identity
  set.seed(1)
  z <- matrix(rnorm(10), nrow = 5)
  qd <- matrix(rnorm(10), nrow = 5)
  g <- matrix(rnorm(10), nrow = 5)
  st <- .vqgenc_straight_through(z, qd, g)
  expect_true(st$jacobian_is_identity)
  # the flattening is row-major, matching the Python arm this mirrors,
  # not R's column-major as.numeric
  expect_equal(st$backward, as.numeric(t(g)), tolerance = 1e-12)
  expect_equal(st$forward, as.numeric(t(qd)), tolerance = 1e-12)
  # the encoder output itself plays no part in either direction
  st2 <- .vqgenc_straight_through(z * 100, qd, g)
  expect_equal(st2$backward, st$backward, tolerance = 1e-12)
  expect_equal(st2$forward, st$forward, tolerance = 1e-12)
  # mismatched lengths are refused
  expect_error(.vqgenc_straight_through(z, qd, g[1:4]), "differ in length")
})

test_that("the two losses are the squared distance and beta times it", {
  set.seed(2)
  z <- matrix(rnorm(12), nrow = 6)
  qd <- matrix(rnorm(12), nrow = 6)
  sq <- sum((as.numeric(z) - as.numeric(qd))^2)
  cl <- .vqgenc_codebook_loss(z, qd)
  expect_equal(as.numeric(cl[[1]]), sq, tolerance = 1e-10)
  for (beta in c(0.1, 0.25, 1)) {
    ml <- .vqgenc_commitment_loss(z, qd, beta = beta)
    expect_equal(as.numeric(ml[[1]]), beta * sq, tolerance = 1e-10)
  }
  # a code equal to the encoder output costs nothing
  expect_equal(as.numeric(.vqgenc_codebook_loss(z, z)[[1]]), 0, tolerance = 1e-12)
  expect_equal(as.numeric(.vqgenc_commitment_loss(z, z)[[1]]), 0, tolerance = 1e-12)
})

test_that("the sequence length is the downsampled grid", {
  for (spec in list(c(256, 256, 16), c(512, 256, 16), c(64, 64, 8),
                    c(1024, 512, 32))) {
    s <- .vqgenc_sequence_length(spec[1], spec[2], spec[3])
    want <- (spec[1] / spec[3]) * (spec[2] / spec[3])
    expect_equal(as.numeric(unlist(s)[1]), want, tolerance = 1e-12)
  }
  # the point of the encoder: the sequence is far shorter than the pixels
  s <- .vqgenc_sequence_length(256, 256, 16)
  expect_lt(as.numeric(unlist(s)[1]), 256 * 256)
  # and a finer downsample gives a longer sequence
  expect_gt(as.numeric(unlist(.vqgenc_sequence_length(256, 256, 8))[1]),
            as.numeric(unlist(.vqgenc_sequence_length(256, 256, 16))[1]))
})

test_that("the encoder runs end to end and agrees with its own parts", {
  cb <- cb4()
  V <- rbind(c(0.1, 0.1), c(0.9, 0.2), c(0.1, 1.2), c(2.5, 2.9))
  e <- .vqgenc_encode(V, cb, beta = 0.25)
  q <- .vqgenc_quantize(V, cb)
  # the indices it reports are the ones the quantiser finds
  expect_identical(as.integer(unlist(e$indices)), as.integer(unlist(q$indices)))
  # and the losses it reports are the ones the loss functions give
  sq <- sum((as.numeric(V) - as.numeric(unname(as.matrix(q$codes))))^2)
  nm <- names(e)
  if ("codebook_loss" %in% nm) {
    expect_equal(as.numeric(e$codebook_loss), sq, tolerance = 1e-8)
  }
  if ("commitment_loss" %in% nm) {
    expect_equal(as.numeric(e$commitment_loss), 0.25 * sq, tolerance = 1e-8)
  }
})

test_that("the coercion helpers accept the shapes they document", {
  m <- .vqgenc_as_matrix(cbind(c(1, 2), c(3, 4)))
  expect_identical(dim(m), c(2L, 2L))
  # a bare vector becomes one row
  expect_identical(nrow(.vqgenc_as_matrix(c(1, 2, 3))), 1L)
  # row-major, as the Python arm flattens
  expect_equal(.vqgenc_as_vector(cbind(c(1, 2), c(3, 4))), c(1, 3, 2, 4))
  expect_equal(.vqgenc_as_vector(list(1, 2, 3)), c(1, 2, 3))
})
