# Closed-form cover for the kamath_llm2_native building blocks that neither
# the existing parity tests nor the Rd examples reach.
#
# Every assertion is anchored outside the module: against base R (qnorm,
# table, kronecker), against the defining formula written out longhand, or
# against a structural certificate (the forward-backward identity, and linear
# programming duality for the transport plan).

test_that("row cross-entropy is log-sum-exp less the diagonal", {
  L <- matrix(c(1, 2, 3, 0, 1, 0, 2, 0, 1), nrow = 3, byrow = TRUE)
  expect_equal(.morie_km2_rowce(L), .morie_km2_lse_rows(L) - diag(L))
  # written out longhand for one row
  expect_equal(.morie_km2_rowce(L)[1], log(sum(exp(L[1, ]))) - L[1, 1])
  # the loss is non-negative when the diagonal is the row maximum
  D <- diag(c(5, 5, 5))
  expect_true(all(.morie_km2_rowce(D) > 0))
  # and is shift invariant, since softmax is
  expect_equal(.morie_km2_rowce(L + 10), .morie_km2_rowce(L))
})

test_that("probability vectors are validated", {
  expect_equal(.morie_km2_dist(c(0.2, 0.8), "p"), c(0.2, 0.8))
  expect_equal(.morie_km2_dist(1, "p"), 1)
  expect_error(.morie_km2_dist(numeric(0), "pi"), "pi is empty")
  expect_error(.morie_km2_dist(c(-0.1, 1.1), "pi"), "negative probability")
  expect_error(.morie_km2_dist(c(0.2, 0.2), "pi"), "must sum to 1")
  # the tolerance is 1e-8, so rounding-level error is accepted
  expect_equal(sum(.morie_km2_dist(c(0.5, 0.5 + 1e-10), "p")), 1,
               tolerance = 1e-9)
})

test_that("token n-grams and counts match direct enumeration", {
  tk <- c("a", "b", "c", "d")
  expect_equal(.morie_km2_ngrams(tk, 1), tk)
  expect_equal(.morie_km2_ngrams(tk, 2),
               c("a\rb", "b\rc", "c\rd"))
  expect_equal(.morie_km2_ngrams(tk, 4), paste(tk, collapse = "\r"))
  # an n longer than the sequence has no n-grams
  expect_equal(.morie_km2_ngrams(tk, 5), character(0))
  expect_length(.morie_km2_ngrams(tk, 3), 2L)

  expect_equal(.morie_km2_counts(character(0)), integer(0))
  ct <- .morie_km2_counts(c("x", "y", "x", "x"))
  expect_equal(ct[["x"]], 3L)
  expect_equal(ct[["y"]], 1L)
  # the tally is base R's, so it must agree with table()
  k <- c("p", "q", "p", "r", "q", "p")
  expect_equal(as.integer(.morie_km2_counts(k)), as.integer(table(k)))
  expect_equal(names(.morie_km2_counts(k)), names(table(k)))
})

test_that("the causal language-model loss is mean token cross-entropy", {
  lg <- matrix(c(2, 1, 0, 0, 3, 1), nrow = 2, byrow = TRUE)
  got <- .morie_km2_causal_lm_loss(lg, c(0L, 1L))
  per <- c(-(2 - log(sum(exp(c(2, 1, 0))))),
           -(3 - log(sum(exp(c(0, 3, 1))))))
  expect_equal(got$token_losses, per)
  expect_equal(got$loss, mean(per))
  expect_equal(got$perplexity, exp(mean(per)))
  expect_equal(got$n_tokens, 2L)
  expect_equal(got$vocab_size, 3L)
  # targets equal to ignore_index drop out of the average entirely
  ig <- .morie_km2_causal_lm_loss(lg, c(0L, -100L))
  expect_equal(ig$loss, per[1])
  expect_equal(ig$n_tokens, 1L)
  # a custom ignore index is honoured
  ig2 <- .morie_km2_causal_lm_loss(lg, c(0L, 7L), ignore_index = 7L)
  expect_equal(ig2$loss, per[1])
  # a confident correct prediction costs almost nothing
  sharp <- matrix(c(50, 0, 0), nrow = 1)
  expect_equal(.morie_km2_causal_lm_loss(sharp, 0L)$loss, 0,
               tolerance = 1e-10)
  expect_error(.morie_km2_causal_lm_loss(lg, 1L), "one target per position")
  expect_error(.morie_km2_causal_lm_loss(lg, c(-100L, -100L)),
               "every position is ignored")
  expect_error(.morie_km2_causal_lm_loss(lg, c(0L, 3L)),
               "outside the vocabulary")
  expect_error(.morie_km2_causal_lm_loss(lg, c(0L, -1L)),
               "outside the vocabulary")
})

test_that("swish is z times the logistic of beta z", {
  z <- c(-40, -3, -0.5, 0, 0.5, 3, 40)
  for (b in c(0.5, 1, 2)) {
    expect_equal(morie_kamath_swish(z, b), z / (1 + exp(-b * z)))
  }
  # the two numerical branches meet at zero and stay finite at the extremes
  expect_equal(morie_kamath_swish(0), 0)
  expect_true(all(is.finite(morie_kamath_swish(c(-800, 800)))))
  expect_equal(morie_kamath_swish(-800), 0)
  expect_equal(morie_kamath_swish(800), 800)
  # beta -> large approaches the ReLU
  expect_equal(morie_kamath_swish(c(-2, 3), 50), c(0, 3), tolerance = 1e-9)
})

test_that("the normal quantile is qnorm on its domain", {
  u <- c(0.001, 0.1, 0.5, 0.9, 0.999)
  expect_equal(morie_kamath_normal_quantile(u), qnorm(u))
  expect_equal(morie_kamath_normal_quantile(0.5), 0)
  # symmetry of the standard normal
  expect_equal(morie_kamath_normal_quantile(0.25),
               -morie_kamath_normal_quantile(0.75))
  expect_error(morie_kamath_normal_quantile(0), "inside \\(0, 1\\)")
  expect_error(morie_kamath_normal_quantile(1), "inside \\(0, 1\\)")
  expect_error(morie_kamath_normal_quantile(-0.5), "inside \\(0, 1\\)")
})

test_that("the GloVe weighting function saturates at x_max", {
  x <- c(0, 1, 25, 99, 100, 500)
  xm <- 100
  al <- 0.75
  expect_equal(morie_kamath_glove_weight(x, xm, al),
               ifelse(x < xm, (x / xm)^al, 1))
  expect_equal(morie_kamath_glove_weight(0, xm, al), 0)
  expect_equal(morie_kamath_glove_weight(xm, xm, al), 1)
  expect_equal(morie_kamath_glove_weight(1e6, xm, al), 1)
  # it is non-decreasing, and alpha = 1 is the linear ramp
  w <- morie_kamath_glove_weight(seq(0, 200, by = 10), xm, al)
  expect_true(all(diff(w) >= 0))
  expect_equal(morie_kamath_glove_weight(50, 100, 1), 0.5)
})

test_that("character n-grams of a word are enumerated with boundaries", {
  # "<ab>" has the 2-grams <a, ab, b> and the 3-grams <ab, ab>, and the whole
  # marked word is always included as a token as well
  expect_equal(morie_kamath_word_ngrams("ab", 2, 3),
               c("<a", "ab", "b>", "<ab", "ab>", "<ab>"))
  # when the n range already produces the whole word it is not repeated
  expect_equal(morie_kamath_word_ngrams("ab", 2, 4),
               c("<a", "ab", "b>", "<ab", "ab>", "<ab>"))
  expect_equal(sum(morie_kamath_word_ngrams("ab", 2, 4) == "<ab>"), 1L)
  # a custom boundary pair is used verbatim
  expect_equal(morie_kamath_word_ngrams("ab", 4, 4, boundary = "[]"),
               "[ab]")
  # one n contributes L - n + 1 grams over the marked word, plus the word
  g <- morie_kamath_word_ngrams("hello", 3, 3)
  expect_length(g, nchar("<hello>") - 3 + 1 + 1)
  expect_true(all(nchar(head(g, -1)) == 3))
  expect_equal(tail(g, 1), "<hello>")
  # the whole marked word is present whatever the n range
  for (nn in 1:4) {
    expect_true("<ab>" %in% morie_kamath_word_ngrams("ab", nn, nn))
  }
  # n beyond the marked length contributes nothing rather than erroring
  expect_length(morie_kamath_word_ngrams("ab", 2, 99), 6L)
  expect_error(morie_kamath_word_ngrams("ab", 0, 2), "1 <= n_min <= n_max")
  expect_error(morie_kamath_word_ngrams("ab", 3, 2), "1 <= n_min <= n_max")
  expect_error(morie_kamath_word_ngrams("", 1, 2), "empty word")
})

test_that("row layer normalisation uses the population variance", {
  x <- matrix(c(1, 2, 3, 10, 20, 30), nrow = 2, byrow = TRUE)
  g <- morie_kamath_layer_norm_rows(x)
  for (i in 1:2) {
    r <- x[i, ]
    expect_equal(g[i, ], (r - mean(r)) / sqrt(mean((r - mean(r))^2) + 1e-5))
  }
  # each row is centred and has unit scale up to eps
  expect_equal(rowMeans(g), c(0, 0))
  expect_equal(apply(g, 1, function(r) sqrt(mean(r^2))), c(1, 1),
               tolerance = 1e-5)
  expect_equal(dim(g), dim(x))
  # a constant row cannot blow up: eps keeps the scale finite
  cg <- morie_kamath_layer_norm_rows(matrix(rep(7, 4), nrow = 1))
  expect_true(all(is.finite(cg)))
  expect_equal(as.numeric(cg), rep(0, 4))
  # a larger eps shrinks the output
  expect_true(max(abs(morie_kamath_layer_norm_rows(x, eps = 10))) <
              max(abs(g)))
})

test_that("the NF4 grid is symmetric and dequantisation is a scaled lookup", {
  nf <- morie_kamath_nf4_datatype(16)
  gr <- nf$normalized
  expect_length(gr, 16L)
  expect_equal(gr[1], -1)
  expect_equal(gr[16], 1)
  expect_true(all(diff(gr) > 0))
  # the grid is antisymmetric about zero
  expect_equal(gr, -rev(gr))

  codes <- matrix(c(0L, 15L, 8L, 7L), nrow = 2, byrow = TRUE)
  # a scalar absmax scales the whole tensor
  expect_equal(as.numeric(morie_kamath_dequantize_nf4(codes, 2)),
               as.numeric(matrix(gr[as.numeric(codes) + 1L], nrow = 2)) * 2)
  expect_equal(morie_kamath_dequantize_nf4(matrix(c(0L, 15L), nrow = 1), 2),
               matrix(c(-2, 2), nrow = 1))
  # a vector absmax scales blockwise, one scale per row
  bw <- morie_kamath_dequantize_nf4(codes, c(1, 4))
  expect_equal(bw[1, ], gr[codes[1, ] + 1L] * 1)
  expect_equal(bw[2, ], gr[codes[2, ] + 1L] * 4)
  expect_equal(dim(bw), dim(codes))
  expect_error(morie_kamath_dequantize_nf4(matrix(0.5), 1), "must be integers")
  expect_error(morie_kamath_dequantize_nf4(matrix(16L), 1), "outside the NF4 grid")
  expect_error(morie_kamath_dequantize_nf4(matrix(-1L), 1), "outside the NF4 grid")
  expect_error(morie_kamath_dequantize_nf4(matrix(0L), 0), "must be positive")
  expect_error(morie_kamath_dequantize_nf4(codes, c(1, 2, 3)),
               "one per row")
  expect_error(morie_kamath_dequantize_nf4(codes, c(1, -1)),
               "must be positive")
})

test_that("the unigram forward and backward passes agree", {
  probs <- list(a = 0.4, b = 0.3, ab = 0.2, c = 0.1)
  for (txt in c("a", "ab", "abc", "aba")) {
    f <- .morie_km2_forward(txt, probs, 2)
    b <- .morie_km2_backward(txt, probs, 2)
    L <- nchar(txt)
    expect_length(f$alpha, L + 1L)
    expect_length(b, L + 1L)
    expect_equal(f$alpha[1], 1)
    expect_equal(b[L + 1L], 1)
    # both passes compute the same total probability of all segmentations
    expect_equal(f$alpha[L + 1L], b[1])
  }
  # "ab" segments as a|b or ab, so the total is 0.4 * 0.3 + 0.2
  f <- .morie_km2_forward("ab", probs, 2)
  expect_equal(f$alpha[3], 0.4 * 0.3 + 0.2)
  expect_equal(morie_kamath_unigram_loglik("ab", probs),
               log(0.4 * 0.3 + 0.2))
  # the corpus log-likelihood is additive over strings
  expect_equal(morie_kamath_unigram_loglik(c("ab", "a"), probs),
               log(0.4 * 0.3 + 0.2) + log(0.4))
  # empty strings are skipped rather than treated as impossible
  expect_equal(morie_kamath_unigram_loglik(c("ab", ""), probs),
               morie_kamath_unigram_loglik("ab", probs))
  expect_error(morie_kamath_unigram_loglik("zz", probs),
               "no segmentation")
  expect_error(morie_kamath_unigram_loglik("a", list()),
               "piece table is empty")
})

test_that("pieces are indexed by the position they end at", {
  probs <- list(a = 0.4, b = 0.3, ab = 0.2)
  ends <- .morie_km2_pieces_by_end("ab", probs, 2)
  expect_length(ends, 3L)
  # nothing ends before the first character
  expect_null(ends[[1]])
  # only "a" ends at position 1; both "b" and "ab" end at position 2
  expect_equal(ends[[2]], "a")
  expect_setequal(ends[[3]], c("b", "ab"))
  # a maxlen of 1 cannot see the two-character piece
  expect_setequal(.morie_km2_pieces_by_end("ab", probs, 1)[[3]], "b")
  # unknown characters yield no piece at all
  expect_equal(.morie_km2_pieces_by_end("z", probs, 2)[[2]], character(0))
})

test_that("Viterbi segmentation is the best segmentation", {
  probs <- list(a = 0.4, b = 0.3, ab = 0.2, c = 0.1)
  # brute force over every segmentation of the text
  all_segs <- function(txt, vocab, maxlen) {
    if (!nchar(txt)) return(list(character(0)))
    out <- list()
    for (nn in seq_len(min(maxlen, nchar(txt)))) {
      w <- substr(txt, 1L, nn)
      if (is.null(vocab[[w]])) next
      for (rest in all_segs(substr(txt, nn + 1L, nchar(txt)), vocab, maxlen)) {
        out[[length(out) + 1L]] <- c(w, rest)
      }
    }
    out
  }
  for (txt in c("ab", "abc", "aab")) {
    segs <- all_segs(txt, probs, 2)
    lps <- vapply(segs, function(s) sum(log(unlist(probs[s]))), numeric(1))
    v <- morie_kamath_viterbi_segment(txt, probs)
    # the reported log-probability is the maximum over all segmentations
    expect_equal(v[[2]], max(lps))
    # and the reported pieces achieve it, and reassemble the text
    expect_equal(sum(log(unlist(probs[v[[1]]]))), max(lps))
    expect_equal(paste(v[[1]], collapse = ""), txt)
    # the best segmentation is never better than the sum over all of them
    expect_true(v[[2]] <= morie_kamath_unigram_loglik(txt, probs) + 1e-12)
  }
  # "ab" prefers the single piece 0.2 over a|b at 0.12
  v <- morie_kamath_viterbi_segment("ab", probs)
  expect_equal(v[[1]], "ab")
  expect_equal(v[[2]], log(0.2))
  expect_error(morie_kamath_viterbi_segment("zz", probs), "cannot be segmented")
})

test_that("the transport plan is feasible and provably optimal", {
  C <- matrix(c(0, 2, 2, 2, 0, 2, 2, 2, 0), 3, byrow = TRUE)
  p <- c(0.5, 0.3, 0.2)
  q <- c(0.2, 0.5, 0.3)
  tr <- .morie_km2_transport(p, q, C)
  # feasibility: the plan moves exactly the supply onto exactly the demand
  expect_equal(rowSums(tr$flow), p)
  expect_equal(colSums(tr$flow), q)
  expect_true(all(tr$flow >= -1e-12))
  # optimality certificate: dual feasibility u_i + v_j <= C_ij everywhere,
  # and strong duality, which together prove the plan is a true optimum
  expect_true(all(outer(tr$u, tr$v, "+") <= C + 1e-9))
  expect_equal(sum(C * tr$flow), sum(tr$u * p) + sum(tr$v * q))
  # complementary slackness holds wherever mass actually moves
  act <- tr$flow > 1e-12
  expect_equal(outer(tr$u, tr$v, "+")[act], C[act])

  expect_equal(morie_kamath_word_movers_distance(C, p, q),
               sum(C * tr$flow))
  # a zero cost matrix costs nothing, and identical distributions on a
  # zero-diagonal cost stay on the diagonal
  expect_equal(morie_kamath_word_movers_distance(matrix(0, 3, 3), p, q), 0)
  expect_equal(morie_kamath_word_movers_distance(C, p, p), 0)
  # the distance is symmetric under transposing the problem
  expect_equal(morie_kamath_word_movers_distance(t(C), q, p),
               morie_kamath_word_movers_distance(C, p, q))
  # against brute force on a small asymmetric problem: the optimum over all
  # assignments of a two-by-two plan
  C2 <- matrix(c(1, 5, 4, 2), 2, byrow = TRUE)
  got <- morie_kamath_word_movers_distance(C2, c(0.5, 0.5), c(0.5, 0.5))
  expect_equal(got, 0.5 * 1 + 0.5 * 2)
})

test_that("the Bradley-Terry loss is the softplus of the negative margin", {
  m <- c(2, 1, 0, -1, -3)
  bt <- .morie_km2_bt_loss(m)
  expect_equal(bt[[2]], log1p(exp(-m)))
  expect_equal(bt[[1]], mean(log1p(exp(-m))))
  # a zero margin is a coin flip
  expect_equal(.morie_km2_bt_loss(0)[[1]], log(2))
  # the loss decreases as the preferred response pulls ahead
  expect_true(all(diff(.morie_km2_bt_loss(c(-2, 0, 2))[[2]]) < 0))
  # and stays finite where the naive expression would overflow
  expect_true(all(is.finite(.morie_km2_bt_loss(c(-800, 800))[[2]])))
  expect_equal(.morie_km2_bt_loss(800)[[1]], 0)
  expect_error(.morie_km2_bt_loss(numeric(0)), "no preference pairs")
  expect_error(.morie_km2_bt_loss(c(1, NA)), "not finite")
  expect_error(.morie_km2_bt_loss(Inf), "not finite")
})

test_that("implicit rewards are beta times the log policy ratio", {
  r <- .morie_km2_implicit_rewards(c(0.6, 0.4), c(0.5, 0.5), 2)
  expect_equal(r[1], 2 * log(0.6 / 0.5))
  expect_equal(r[2], 2 * log(0.4 / 0.5))
  expect_equal(r[3], 2)
  # a policy equal to the reference earns nothing
  expect_equal(.morie_km2_implicit_rewards(c(0.5, 0.5), c(0.5, 0.5), 3)[1:2],
               c(0, 0))
  # the margin scales linearly in beta
  a <- .morie_km2_implicit_rewards(c(0.7, 0.3), c(0.5, 0.5), 1)
  b <- .morie_km2_implicit_rewards(c(0.7, 0.3), c(0.5, 0.5), 4)
  expect_equal(b[1] - b[2], 4 * (a[1] - a[2]))
  expect_error(.morie_km2_implicit_rewards(c(.5, .5), c(.5, .5), 0),
               "strictly positive")
  expect_error(.morie_km2_implicit_rewards(0.5, c(.5, .5), 1),
               "exactly two probabilities")
  expect_error(.morie_km2_implicit_rewards(c(0, 1), c(.5, .5), 1),
               "must lie in \\(0, 1\\]")
})

test_that("cosine means and the WEAT statistic follow their definitions", {
  W1 <- rbind(c(1, 0), c(0, 1))
  W2 <- rbind(c(-1, 0), c(0, -1))
  a <- c(1, 0)
  # mean cosine of a against each row of W
  expect_equal(.morie_km2_cos_mean(a, W1, "W"), mean(c(1, 0)))
  expect_equal(.morie_km2_cos_mean(a, W2, "W"), mean(c(-1, 0)))
  # cosine ignores the length of a
  expect_equal(.morie_km2_cos_mean(a * 10, W1, "W"),
               .morie_km2_cos_mean(a, W1, "W"))
  # the WEAT association is the difference of the two means
  expect_equal(.morie_km2_weat_s(a, W1, W2), 0.5 - (-0.5))
  # a target at the same angle to both sets has zero association
  expect_equal(.morie_km2_weat_s(c(1, 1), rbind(c(1, 0)), rbind(c(0, 1))), 0,
               tolerance = 1e-12)
  # swapping the attribute sets flips the sign
  expect_equal(.morie_km2_weat_s(a, W2, W1), -.morie_km2_weat_s(a, W1, W2))
  sums <- .morie_km2_weat_sums(rbind(c(1, 0)), rbind(c(0, 1)), W1, W2)
  expect_length(sums, 2L)
  expect_equal(sums[[1]], .morie_km2_weat_s(c(1, 0), W1, W2),
               ignore_attr = TRUE)
  expect_equal(sums[[2]], .morie_km2_weat_s(c(0, 1), W1, W2),
               ignore_attr = TRUE)
  expect_error(.morie_km2_cos_mean(a, matrix(0, 0, 2), "W1"), "W1 is empty")
  expect_error(.morie_km2_cos_mean(a, rbind(c(0, 0)), "W1"), "zero vector")
  expect_error(.morie_km2_cos_mean(c(0, 0), W1, "W1"), "a is a zero vector")
  expect_error(.morie_km2_cos_mean(c(1, 0, 0), W1, "W1"), "differ in width")
  expect_error(.morie_km2_weat_sums(matrix(0, 0, 2), rbind(c(0, 1)), W1, W2),
               "attribute word")
  expect_error(.morie_km2_weat_sums(rbind(c(1, 0)), rbind(c(0, 1, 2)),
                                    W1, W2), "differ in width")
})

test_that("similarity matrices normalise rows before the inner product", {
  X <- rbind(c(3, 0), c(0, 2))
  Y <- rbind(c(1, 0), c(1, 1))
  got <- .morie_km2_sim_matrix(X, Y, normalize = TRUE)
  nx <- X / sqrt(rowSums(X^2))
  ny <- Y / sqrt(rowSums(Y^2))
  expect_equal(got[[1]], nx)
  expect_equal(got[[2]], ny)
  expect_equal(got[[3]], nx %*% t(ny))
  # every entry of a normalised similarity matrix is a cosine
  expect_true(all(abs(got[[3]]) <= 1 + 1e-12))
  # without normalising, the raw inner products come back untouched
  raw <- .morie_km2_sim_matrix(X, Y, normalize = FALSE)
  expect_equal(raw[[1]], X)
  expect_equal(raw[[3]], X %*% t(Y))
  expect_error(.morie_km2_sim_matrix(matrix(0, 0, 2), Y, TRUE),
               "empty embeddings")
  expect_error(.morie_km2_sim_matrix(X, rbind(c(1, 2, 3)), TRUE),
               "widths differ")
  expect_error(.morie_km2_sim_matrix(rbind(c(0, 0)), Y, TRUE),
               "zero token embedding")
})

test_that("log probabilities come from a vector or a scorer", {
  expect_equal(.morie_km2_log_probs(c(0.5, 0.25), NULL, "p"),
               log(c(0.5, 0.25)))
  # a scorer is called with zero-based positions
  seen <- integer(0)
  sc <- function(i) {
    seen <<- c(seen, i)
    0.5
  }
  expect_equal(.morie_km2_log_probs(1:3, sc, "p"), rep(log(0.5), 3))
  expect_equal(seen, 0:2)
  expect_equal(.morie_km2_log_probs(1, NULL, "p"), 0)
  expect_error(.morie_km2_log_probs(list(), NULL, "seq"), "seq is empty")
  expect_error(.morie_km2_log_probs(0, NULL, "p"), "outside \\(0, 1\\]")
  expect_error(.morie_km2_log_probs(1.5, NULL, "p"), "outside \\(0, 1\\]")
})

test_that("word occurrences are counted across outputs", {
  outs <- list("the cat sat", "the dog", "a cat")
  expect_equal(.morie_km2_count_word("the", outs), 2)
  expect_equal(.morie_km2_count_word("cat", outs), 2)
  expect_equal(.morie_km2_count_word("zebra", outs), 0)
  # repeats within one output are counted individually
  expect_equal(.morie_km2_count_word("a", list("a a a")), 3)
  expect_equal(.morie_km2_count_word("x", list()), 0)
})

test_that("paired embeddings are looked up and shape-checked", {
  E <- list(king = c(1, 0), queen = c(0, 1), man = c(1, 1))
  out <- .morie_km2_pair_vectors(list(c("king", "queen")), E, "A")
  expect_length(out, 1L)
  expect_equal(out[[1]][[1]], c(1, 0))
  expect_equal(out[[1]][[2]], c(0, 1))
  # a function embedder is accepted in place of a table
  fout <- .morie_km2_pair_vectors(list(c("a", "b")),
                                  function(w) if (w == "a") c(2, 0) else c(0, 3),
                                  "A")
  expect_equal(fout[[1]][[1]], c(2, 0))
  expect_length(.morie_km2_pair_vectors(list(c("king", "man"),
                                             c("queen", "man")), E, "A"), 2L)
  expect_error(.morie_km2_pair_vectors(list(), E, "A"), "A is empty")
  expect_error(.morie_km2_pair_vectors(list("king"), E, "A"), "must be a pair")
  expect_error(.morie_km2_pair_vectors(list(c("king", "zzz")), E, "A"),
               "no embedding")
})

test_that("toxicity scores accept a vector or a scorer and stay in range", {
  Y <- list("a", "b", "c")
  got <- .morie_km2_tox_scores(Y, c(0.1, 0.5, 0.9))
  expect_equal(got[[1]], c(0.1, 0.5, 0.9))
  expect_equal(got[[2]], Y)
  # a scoring function is applied to each output
  fn <- .morie_km2_tox_scores(Y, function(y) if (y == "a") 0 else 1)
  expect_equal(fn[[1]], c(0, 1, 1))
  expect_error(.morie_km2_tox_scores(list(), 1, "Yhat"), "Yhat is empty")
  expect_error(.morie_km2_tox_scores(Y, c(0.1, 0.2)), "differ in length")
  expect_error(.morie_km2_tox_scores(Y, c(0.1, 0.2, 1.5)),
               "lie in \\[0, 1\\]")
  expect_error(.morie_km2_tox_scores(Y, c(-0.1, 0.2, 0.5)),
               "lie in \\[0, 1\\]")
})

test_that("hidden states come from a vector, a function, or the context", {
  expect_equal(.morie_km2_hidden(NULL, c(1, 2, 3), "h"), c(1, 2, 3))
  expect_equal(.morie_km2_hidden(c(4, 5), c(1, 2, 3), "h"), c(4, 5))
  expect_equal(.morie_km2_hidden(function(c) c * 2, c(1, 2), "h"), c(2, 4))
  expect_error(.morie_km2_hidden(numeric(0), 1, "h"), "h produced an empty")
  expect_error(.morie_km2_hidden(function(c) numeric(0), 1, "phi"),
               "phi produced an empty")
})

test_that("prompt templates fill their slots", {
  t1 <- "Question: [x] Answer:"
  f1 <- .morie_km2_fill_template(t1, "why", NULL)
  expect_equal(f1[[1]], "Question: why Answer:")
  expect_false(f1[[2]])
  # the answer slot is filled only when the template offers one
  t2 <- "Q: [x] A: [z]"
  f2 <- .morie_km2_fill_template(t2, "why", "because")
  expect_true(grepl("because", f2[[1]], fixed = TRUE))
  expect_true(grepl("why", f2[[1]], fixed = TRUE))
  expect_true(f2[[2]])
  expect_error(.morie_km2_fill_template("no slot", "x", NULL),
               "\\[x\\] slot")
  expect_error(.morie_km2_fill_template(42, "x", NULL), "\\[x\\] slot")
  expect_error(.morie_km2_fill_template(t1, "   ", NULL),
               "non-empty input string")
  expect_error(.morie_km2_fill_template(t1, "why", 5), "string or NULL")
  expect_error(.morie_km2_fill_template(t1, "why", "because"),
               "no \\[z\\] answer slot")

  res <- .morie_km2_tmpl_result("a b c", TRUE, "7.1", t1)
  expect_equal(res$tokens, c("a", "b", "c"))
  expect_equal(res$n, 3L)
  expect_equal(res$estimate, 3)
  expect_true(res$slot_filled)
  expect_equal(res$template, t1)
  expect_match(res$method, "7\\.1")
  # surrounding whitespace does not create empty tokens
  expect_equal(.morie_km2_tmpl_result("  a  b  ", FALSE, "1", t1)$n, 2L)
})

test_that("the adapter adds a low-rank delta to the frozen output", {
  H_o <- matrix(c(1, 2, 3, 4), nrow = 2)
  H_in <- matrix(c(1, 0, 0, 1), nrow = 2)
  Wd <- matrix(c(1, 1), nrow = 2)          # 2 -> 1 bottleneck
  Wu <- matrix(c(2, 3), nrow = 1)          # 1 -> 2
  got <- .morie_km2_adapter_core(H_o, H_in, Wd, Wu, identity)
  delta <- (H_in %*% Wd) %*% Wu
  expect_equal(got[[2]], delta)
  expect_equal(got[[1]], H_o + delta)
  expect_equal(got[[3]], 1L)
  # a zero-valued activation leaves the frozen output untouched
  zero <- .morie_km2_adapter_core(H_o, H_in, Wd, Wu, function(z) z * 0)
  expect_equal(zero[[1]], H_o)
  expect_error(.morie_km2_adapter_core(H_o, H_in, matrix(1, 3, 1), Wu, identity),
               "W_down does not fit")
  expect_error(.morie_km2_adapter_core(H_o, H_in, Wd, matrix(1, 2, 2), identity),
               "bottleneck disagrees")
  expect_error(.morie_km2_adapter_core(H_o, H_in, Wd, matrix(1, 1, 3), identity),
               "W_up does not return")
  expect_error(.morie_km2_adapter_core(H_o, rbind(c(1, 0)), Wd, Wu, identity),
               "row counts differ")
  expect_error(.morie_km2_adapter_core(H_o, H_in, Wd, Wu, "not a function"),
               "f must be a function")
})

test_that("the sequence objective sums conditional log-probabilities", {
  mdl <- function(ctx, prefix, tok) 0.5
  got <- .morie_km2_seq_obj(mdl, list("c1", "c2"),
                            list(list("a", "b"), list("a")))
  # three tokens in total, each at probability one half
  expect_equal(got[[2]], c(2 * log(0.5), log(0.5)))
  expect_equal(got[[1]], 3 * log(0.5))
  # the prefix grows as the sequence is scored
  seen <- list()
  mdl2 <- function(ctx, prefix, tok) {
    seen[[length(seen) + 1L]] <<- length(prefix)
    1
  }
  .morie_km2_seq_obj(mdl2, list("c"), list(list("a", "b", "c")))
  expect_equal(unlist(seen), c(0, 1, 2))
  expect_error(.morie_km2_seq_obj(mdl, list(), list()), "Z is empty")
  expect_error(.morie_km2_seq_obj(mdl, list("c"), list(list("a"), list("b"))),
               "contexts and targets differ")
  expect_error(.morie_km2_seq_obj("not a function", list("c"), list(list("a"))),
               "must be a function")
  expect_error(.morie_km2_seq_obj(mdl, list("c"), list(list())),
               "target sequence is empty")
  expect_error(.morie_km2_seq_obj(function(...) 0, list("c"), list(list("a"))),
               "must lie in \\(0, 1\\]")
})

test_that("the merged adapter is the Kronecker product scaled by s", {
  W <- matrix(0, 4, 4)
  A <- matrix(c(1, 0, 0, 1), 2)
  B <- matrix(c(1, 2, 3, 4), 2)
  got <- .morie_km2_tuned(W, A, B, 0.5)
  K <- kronecker(A, B)
  expect_equal(got[[3]], K)
  expect_equal(got[[1]], W + 0.5 * K)
  expect_equal(got[[2]], W)
  expect_equal(got[[4]], 0.5)
  # s = 0 leaves the frozen weights exactly as they were
  expect_equal(.morie_km2_tuned(W, A, B, 0)[[1]], W)
  expect_error(.morie_km2_tuned(matrix(0, 3, 3), A, B, 1),
               "cannot be merged")
  expect_error(.morie_km2_tuned(W, A, B, NA_real_), "s must be finite")
})

test_that("diagonal arguments are accepted as a vector or a matrix", {
  expect_equal(.morie_km2_diag(c(1, 2, 3), "S", 3), c(1, 2, 3))
  expect_equal(.morie_km2_diag(diag(c(1, 2, 3)), "S", 3), c(1, 2, 3))
  expect_error(.morie_km2_diag(matrix(1, 2, 2), "S", 2), "S must be diagonal")
  expect_error(.morie_km2_diag(matrix(1:6, 2, 3), "S", 2), "S must be diagonal")
  expect_error(.morie_km2_diag(c(1, 2), "Sigma", 3), "Sigma has the wrong length")
})
