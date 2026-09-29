# Coverage for BIO named-entity decoding (Ramshaw & Marcus 1995; Viterbi
# 1967). The constrained Viterbi path is compared with brute-force
# enumeration of every BIO-valid label sequence, spans and span-level F1
# with hand-derived values, and the greedy score with the emission sum.

.lab <- c("O", "B-PER", "I-PER", "B-LOC", "I-LOC")

test_that("labels, transitions, starts and validity follow the BIO scheme", {
  expect_identical(bio_labels(c("PER", "LOC")), .lab)
  expect_error(bio_labels(character(0)), "no entity types")
  expect_error(bio_labels(c("PER", "PER")), "duplicate")
  T <- valid_transitions(.lab)
  expect_identical(T[, 3], c(FALSE, TRUE, TRUE, FALSE, FALSE))
  expect_identical(T[, 5], c(FALSE, FALSE, FALSE, TRUE, TRUE))
  expect_true(all(T[, c(1, 2, 4)]))
  expect_identical(start_allowed(.lab), c(TRUE, TRUE, FALSE, TRUE, FALSE))
  expect_true(is_valid_bio(c("B-PER", "I-PER", "O", "B-LOC")))
  expect_false(is_valid_bio(c("O", "I-PER")))
  expect_false(is_valid_bio(c("B-PER", "I-LOC")))
})

test_that("Viterbi finds the best BIO-valid path by brute force", {
  set.seed(7)
  E <- matrix(stats::rnorm(4 * 5), 4)
  S <- matrix(stats::rnorm(25, sd = 0.3), 5)
  v <- viterbi_decode(E, .lab, transition_scores = S)
  paths <- as.matrix(expand.grid(rep(list(1:5), 4)))
  sc <- apply(paths, 1, function(p) {
    if (!is_valid_bio(.lab[p])) return(-Inf)
    sum(E[cbind(1:4, p)]) + sum(S[cbind(p[-4], p[-1])])
  })
  best <- paths[which.max(sc), ]
  expect_identical(v$path, .lab[best])
  expect_equal(v$score, max(sc), tolerance = 1e-12)
  expect_true(is_valid_bio(v$path))
  g <- greedy_decode(E, .lab)
  expect_identical(g, .lab[apply(E, 1, which.max)])
  one <- viterbi_decode(E[1, , drop = FALSE], .lab)
  expect_identical(one$path, .lab[which.max(ifelse(start_allowed(.lab), E[1, ], -Inf))])
  expect_error(viterbi_decode(E[, 1:3], .lab), "one score per label")
})

test_that("spans and span-level F1", {
  path <- c("B-PER", "I-PER", "O", "B-LOC", "B-LOC", "I-LOC", "I-PER")
  sp <- extract_spans(path)
  expect_identical(vapply(sp, function(s) paste(s, collapse = ":"), ""), c("PER:1:2", "LOC:4:4", "LOC:5:6", "PER:7:7"))
  gold <- c("B-PER", "I-PER", "O", "B-LOC", "I-LOC", "I-LOC", "O")
  f <- span_f1(path, gold)
  expect_identical(c(f$true_positives, f$n_pred, f$n_gold), c(1L, 4L, 2L))
  expect_equal(c(f$precision, f$recall), c(1 / 4, 1 / 2), tolerance = 1e-12)
  expect_equal(f$f1, 2 * (1 / 4) * (1 / 2) / (3 / 4), tolerance = 1e-12)
  expect_identical(span_f1(rep("O", 3), rep("O", 3))$f1, 0)
})

test_that("ner_decode wires the decoders, scores and aliases together", {
  set.seed(7)
  E <- matrix(stats::rnorm(4 * 5), 4)
  r <- ner_decode(E, c("PER", "LOC"))
  v <- viterbi_decode(E, .lab)
  expect_identical(r$path, v$path)
  expect_equal(r$score, v$score, tolerance = 1e-12)
  expect_identical(r$n_spans, length(extract_spans(v$path)))
  g <- ner_decode(E, c("PER", "LOC"), decoder = "greedy", gold = v$path)
  expect_equal(g$score, sum(apply(E, 1, max)), tolerance = 1e-12)
  expect_equal(g$f1, span_f1(g$path, v$path)$f1, tolerance = 1e-12)
  expect_identical(nerdecode, ner_decode)
  expect_identical(named_entity, ner_decode)
  expect_identical(namedentity, ner_decode)
  expect_identical(morie_benRea, ner_decode)
  expect_error(ner_decode(E, c("PER", "LOC"), decoder = "beam"), "viterbi or greedy")
})
