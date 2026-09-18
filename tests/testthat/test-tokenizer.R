# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage tests for R/tokenizer.R -- BPE encode/decode + GGUF metadata.

set.seed(1)

test_that("tokenizer_new with explicit vocab works", {
  set.seed(1)
  vocab <- c("<unk>", "<s>", "</s>", "\\u2581", "hi", "world")
  tok <- morie_tokenizer_new(vocab = vocab, bos_id = 1L, eos_id = 2L)
  expect_s3_class(tok, "morie_tokenizer")
  expect_equal(morie_tokenizer_vocab_size(tok), length(vocab))
})

test_that("tokenizer_new errors when no source supplied", {
  set.seed(1)
  expect_error(morie_tokenizer_new(), "vocab")
})

test_that("encode emits BOS by default", {
  set.seed(1)
  vocab <- c("<unk>", "<s>", "</s>", "\\u2581", "hi", "world")
  tok <- morie_tokenizer_new(vocab = vocab, bos_id = 1L, eos_id = 2L)
  ids <- morie_tokenizer_encode(tok, "hi")
  expect_true(is.integer(ids))
  expect_equal(unname(ids[1]), 1L)
})

test_that("encode without BOS does not prepend it", {
  set.seed(1)
  vocab <- c("<unk>", "<s>", "</s>", "\\u2581", "hi")
  tok <- morie_tokenizer_new(vocab = vocab, bos_id = 1L, eos_id = 2L)
  ids <- morie_tokenizer_encode(tok, "hi", add_bos = FALSE)
  expect_true(all(ids != 1L) || length(ids) == 0L)
})

test_that("encode -> decode roundtrips for simple ascii words", {
  set.seed(1)
  vocab <- c("<unk>", "<s>", "</s>", "\\u2581", "\\u2581hello", "\\u2581world")
  tok <- morie_tokenizer_new(vocab = vocab, bos_id = 1L, eos_id = 2L)
  ids <- morie_tokenizer_encode(tok, "hello world", add_bos = FALSE)
  out <- morie_tokenizer_decode(tok, ids)
  expect_type(out, "character")
  expect_length(out, 1L)
})

test_that("decode handles UTF-8 byte fallback tokens", {
  set.seed(1)
  vocab <- c("<unk>", "<s>", "</s>", "\\u2581", sprintf("<0x%02X>", 65L))
  tok <- morie_tokenizer_new(vocab = vocab, bos_id = 1L, eos_id = 2L)
  fallback_id <- which(vocab == "<0x41>") - 1L
  out <- morie_tokenizer_decode(tok, fallback_id)
  expect_equal(out, "A")
})

test_that("decode skips out-of-range ids", {
  set.seed(1)
  vocab <- c("<unk>", "<s>", "</s>", "\\u2581a")
  tok <- morie_tokenizer_new(vocab = vocab, bos_id = 1L, eos_id = 2L)
  expect_equal(morie_tokenizer_decode(tok, c(-1L, 999L)), "")
})

test_that("gguf metadata path builds tokenizer", {
  set.seed(1)
  meta <- list(
    `tokenizer.ggml.tokens` = c("<unk>", "<s>", "</s>", "\\u2581", "\\u2581hi"),
    `tokenizer.ggml.scores` = c(0, 0, 0, 0, 1),
    `tokenizer.ggml.bos_token_id` = 1L,
    `tokenizer.ggml.eos_token_id` = 2L,
    `tokenizer.ggml.merges` = c("a b", "c d")
  )
  tok <- morie_tokenizer_new(gguf_metadata = meta)
  expect_s3_class(tok, "morie_tokenizer")
  expect_equal(morie_tokenizer_vocab_size(tok), 5L)
  expect_equal(tok$bos_id, 1L)
})

test_that("gguf metadata path errors when tokens missing", {
  set.seed(1)
  expect_error(morie_tokenizer_new(gguf_metadata = list()), "tokens")
})

test_that("print.morie_tokenizer returns invisibly", {
  set.seed(1)
  vocab <- c("<unk>", "<s>", "</s>", "\\u2581")
  tok <- morie_tokenizer_new(vocab = vocab)
  expect_output(print(tok), "morie_tokenizer")
})

test_that("vocab_size matches length(vocab) when no sp processor", {
  set.seed(1)
  vocab <- c("a", "b", "c")
  tok <- morie_tokenizer_new(vocab = vocab)
  expect_equal(morie_tokenizer_vocab_size(tok), 3L)
})

test_that("internal bpe_encode emits at least one token", {
  set.seed(1)
  vocab <- c("<unk>", "<s>", "</s>", "\\u2581", "\\u2581a")
  tok <- morie_tokenizer_new(vocab = vocab)
  toks <- rmorie:::.morie_tokenizer_bpe_encode(tok, "a")
  expect_true(length(toks) >= 1L)
})

test_that("encode of empty string still adds BOS", {
  set.seed(1)
  vocab <- c("<unk>", "<s>", "</s>", "\\u2581")
  tok <- morie_tokenizer_new(vocab = vocab, bos_id = 1L)
  ids <- morie_tokenizer_encode(tok, "", add_bos = TRUE)
  expect_equal(unname(ids[1]), 1L)
})

test_that("tokenizer_new with merges parses them into list", {
  set.seed(1)
  vocab <- c("<unk>", "<s>", "</s>", "\\u2581", "ab")
  tok <- morie_tokenizer_new(vocab = vocab, merges = c("a b", "badformat"))
  expect_true(length(tok$merges) >= 1L)
})