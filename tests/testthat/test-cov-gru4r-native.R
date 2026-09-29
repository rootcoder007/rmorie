# Coverage tests for R/gru4r_native.R (Hidasi et al. 2016, GRU4Rec):
# session-parallel mini-batches, BPR and TOP1 losses, the GRU cell and
# the ranking metrics, for both spellings.

test_that("session-parallel mini-batches hand finished slots to new sessions", {
  S <- list(c(1, 2, 3, 4), c(5, 6), c(7, 8, 9), c(10, 11))
  for (f in list(session_parallel_batches, gru4rec, gruforrecommendation, morie_gru4r)) {
    b <- f(S, 2)
    expect_equal(b$n_steps, 4L)
    s1 <- b$steps[[1]]
    expect_equal(unlist(s1$input), c(1, 5))
    expect_equal(unlist(s1$target), c(2, 6))
    s2 <- b$steps[[2]]
    # slot 2 finished session (5,6) and takes session 3 with a reset
    expect_equal(unlist(s2$input), c(2, 7))
    expect_equal(s2$reset, c(FALSE, TRUE))
    s4 <- b$steps[[4]]
    # slot 1 takes the last session (reset); slot 2 has nothing left
    expect_equal(s4$input[[1]], 10)
    expect_true(is.na(s4$input[[2]]))
    expect_equal(s4$reset, c(TRUE, FALSE))
    # every (event, next event) pair is emitted exactly once
    pairs <- unlist(lapply(b$steps, function(st) Map(function(i, t) if (!is.na(i)) paste(i, t), st$input, st$target)))
    ref <- unlist(lapply(S, function(s) paste(s[-length(s)], s[-1])))
    expect_setequal(pairs, ref)
    expect_length(pairs, length(ref))
    expect_error(f(list(1:3, 4L), 1), "at least 2 events")
    expect_error(f(S, 5), "batch_size")
  }
})

test_that("BPR and TOP1 losses", {
  rt <- 1.2
  neg <- c(0.3, 1.5, -0.4)
  for (f in list(bpr_loss, morie_gru4r_bpr)) {
    expect_equal(f(rt, neg), -mean(log(plogis(rt - neg))), tolerance = 1e-12)
    expect_error(f(rt, numeric(0)), "negative")
  }
  for (f in list(top1_loss, morie_gru4r_top1)) {
    expect_equal(f(rt, neg), mean(plogis(neg - rt)) + mean(plogis(neg^2)), tolerance = 1e-12)
    expect_equal(f(rt, neg, regularize = FALSE), mean(plogis(neg - rt)), tolerance = 1e-12)
    expect_error(f(rt, numeric(0)), "negative")
  }
  expect_equal(bpr_loss(0, 1e4), -log(1e-12), tolerance = 1e-12)
})

test_that("GRU cell update", {
  x <- c(0.5, -0.2, 0.1)
  h <- c(0.3, -0.6)
  mk <- function(r, c, s) matrix(sin(s * (1:(r * c))), r, c)
  W <- list(mk(2, 3, 1.1), mk(2, 2, 0.7), mk(2, 3, 1.9), mk(2, 2, 2.3), mk(2, 3, 0.4), mk(2, 2, 1.3))
  z <- plogis(W[[1]] %*% x + W[[2]] %*% h)
  r <- plogis(W[[3]] %*% x + W[[4]] %*% h)
  hh <- tanh(W[[5]] %*% x + W[[6]] %*% (r * h))
  ref <- as.numeric((1 - z) * h + z * hh)
  expect_equal(do.call(gru_step, c(list(x, h), W)), ref, tolerance = 1e-12)
  expect_equal(do.call(morie_gru4r_gru, c(list(x, h), W)), ref, tolerance = 1e-12)
})

test_that("recall@k and MRR@k", {
  ranked <- c(7, 3, 9, 1, 4)
  for (f in list(recall_at_k, morie_gru4r_recall)) {
    expect_equal(f(ranked, 9, 3), 1)
    expect_equal(f(ranked, 4, 3), 0)
    expect_equal(f(ranked, 4), 1)
  }
  for (f in list(mrr_at_k, morie_gru4r_mrr)) {
    expect_equal(f(ranked, 9, 3), 1 / 3)
    expect_equal(f(ranked, 4, 3), 0)
    expect_equal(f(ranked, 7), 1)
  }
})
