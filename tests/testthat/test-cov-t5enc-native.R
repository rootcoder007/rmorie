# T5 text-to-text pieces (Raffel et al. 2020): task prefixes, span
# corruption with sentinels, log-bucketed relative positions (the
# reference T5 bucketing), regression targets and strict label parsing.

t5_bucket_ref <- function(rp, bidirectional = TRUE, nb = 32, maxd = 128) {
  ret <- 0
  if (bidirectional) {
    nb <- nb %/% 2
    if (rp > 0) ret <- nb
    n <- abs(rp)
  } else {
    n <- max(-rp, 0)
  }
  ex <- nb %/% 2
  if (n < ex) return(ret + n)
  ret + min(ex + floor(log(n / ex) / log(maxd / ex) * (nb - ex)), nb - 1)
}
t5_rebuild <- function(r) {
  out <- character(0)
  tg <- r$target
  for (tok in r$input) {
    if (grepl("^<extra_id_", tok)) {
      k <- which(tg == tok)
      nxt <- which(grepl("^<extra_id_", tg) & seq_along(tg) > k)[1]
      out <- c(out, tg[seq_len(nxt - k - 1) + k])
    } else {
      out <- c(out, tok)
    }
  }
  out
}

test_that("task prefixes", {
  expect_identical(t5enc_task_prefix(" translate English to German ", "hi"),
                   "translate English to German: hi")
  expect_error(t5enc_task_prefix("  ", "x"), "cannot be empty")
  expect_identical(morie_t5enc("task_prefix", "cola", "x"), "cola: x")
})

test_that("span corruption drops spans behind one sentinel each and is invertible", {
  toks <- paste0("w", 1:40)
  r <- t5enc_span_corruption(toks, rate = 0.15, mean_span = 3, seed = 4)
  expect_identical(t5_rebuild(r), toks)
  sent_in <- grep("^<extra_id_", r$input, value = TRUE)
  expect_identical(sent_in, sprintf("<extra_id_%d>", seq_len(r$n_spans) - 1L))
  expect_identical(r$target[length(r$target)], sprintf("<extra_id_%d>", r$n_spans))
  expect_identical(r$corrupted_tokens, length(r$target) - r$n_spans - 1L)
  expect_equal(r$corruption_rate, r$corrupted_tokens / 40)
  expect_identical(r$target_shorter_by, length(r$input) - length(r$target))
  # span starts are the sorted distinct floor(u n) draws, first round(6/3) = 2
  u <- .ghc_unif(.ghc_rng(4), 6L)
  st <- sort(unique(as.integer(u * 40) %% 40))[1:2]
  expect_identical(r$input[st[1] + 1], "<extra_id_0>")
  expect_identical(t5encoder(toks, seed = 4), r)
  expect_identical(t5(toks, seed = 4), r)
  expect_error(t5enc_span_corruption(toks, rate = 0), "\\(0,1\\)")
  expect_error(t5enc_span_corruption(toks, mean_span = 0.5), "at least 1")
})

test_that("relative position buckets follow the T5 log bucketing", {
  for (rp in c(-200, -50, -9, -8, -7, -1, 0, 1, 5, 8, 9, 40, 127, 128, 500)) {
    expect_equal(t5enc_relative_bucket(rp), t5_bucket_ref(rp))
    expect_equal(t5enc_relative_bucket(rp, bidirectional = FALSE, num_buckets = 16,
                                       max_distance = 64),
                 t5_bucket_ref(rp, FALSE, 16, 64))
  }
  expect_error(t5enc_relative_bucket(1, num_buckets = 1), "at least 2")
})

test_that("regression targets round to the increment; predictions parse strictly", {
  expect_identical(t5enc_format_regression(3.27), "3.2")
  expect_identical(t5enc_format_regression(3.33), "3.4")
  expect_identical(t5enc_format_regression(9), "5.0")
  expect_identical(t5enc_format_regression(0.2), "1.0")
  expect_error(t5enc_format_regression(2, increment = 0), "positive")
  expect_identical(t5enc_parse_prediction(" 2.6 ")$value, 2.6)
  expect_false(t5enc_parse_prediction("two")$valid)
  ok <- t5enc_parse_prediction("positive", labels = c("positive", "negative"))
  expect_identical(ok$label, "positive")
  bad <- t5enc_parse_prediction("posit", labels = c("positive", "negative"))
  expect_false(bad$valid)
  expect_null(bad$label)
  expect_match(t5enc_cheatsheet(), "SPAN corruption", fixed = TRUE)
  expect_identical(morie_t5enc("cheatsheet"), t5enc_cheatsheet())
})
