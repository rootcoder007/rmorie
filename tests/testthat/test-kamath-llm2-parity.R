# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Cross-language parity for kamath_llm2_native.R against morie.fn
# km042-km150 + km3h/kmadal..kmyarn (200 Python modules).
#
# EVERY numeric anchor below was produced by running the Python module
# itself from the repo root:
#   cd /Volumes/VSR/rootcoderfiles/morie && MORIE_NO_UPDATE_CHECK=1 \
#     PYTHONPATH=src python3 <script>
# with an assertion that morie.fn.km042.__file__ contains /src/morie/fn/,
# so a stale site-packages install cannot supply the truth.
#
# Alongside the anchors, the independent checks the Python test-suite uses
# are repeated here: the WMD optimum against Birkhoff brute force, the
# Kronecker reshape identity, min-cost flow against every permutation, the
# Bradley-Terry gradient against finite differences, and hand-summed losses.

TOL <- 1e-9

# ------------------------------------------------------- Ch 3: prompting

test_that("Ch 3 prompt plumbing matches Python", {
  expect_equal(morie_kamath_ch3_prompt_label_mapping(
    c(great = 0.7, terrible = 0.3), "positive",
    list(positive = "great", negative = "terrible"))$estimate, 0.7,
    tolerance = TOL)

  o <- morie_kamath_ch3_prompt_softmax_label(
    list(great = c(1, 0), terrible = c(0, 1)), c(1, 0),
    list(pos = "great", neg = "terrible"))
  expect_equal(unname(o$label_probs[c("pos", "neg")]),
               c(0.7310585786300049, 0.2689414213699951), tolerance = TOL)
  expect_equal(o$label, "pos")

  o <- morie_kamath_ch3_prompt_search_argmax(
    "Paris is [z].", c("great", "terrible"), function(s) nchar(s))
  expect_equal(o$estimate, 18)
  expect_equal(o$z_hat, "terrible")
  expect_equal(o$filled_prompt, "Paris is terrible.")

  o <- morie_kamath_ch3_dante_cloze()
  expect_equal(o$mask_index, 4L)     # 0-based, as in Python
  expect_equal(o$n, 5L)

  expect_equal(morie_kamath_ch3_prefix_prompt_template("Loved it.",
                                                       "great")$prompt,
               "Loved it. This movie is great")
  expect_equal(morie_kamath_ch3_translate_prefix_prompt("The cat sleeps.")$prompt,
               "Translate the following English sentence to French: The cat sleeps. [z]")
  expect_equal(morie_kamath_ch3_cloze_prompt_template("Loved it.",
                                                      "great")$prompt,
               "Loved it. This is a great movie.")

  o <- morie_kamath_ch3_top1_prompt_metric(
    list(list("a", "pos"), list("b", "neg")), "T1",
    function(x, t) c(pos = 0.9, neg = 0.1))
  expect_equal(o$estimate, 0.5)
  expect_equal(o$n_correct, 1L)

  expect_equal(morie_kamath_ch3_back_translation_prob(
    "a", "b", p_forward = 0.5, p_backward = 0.25)$estimate, 0.125)

  o <- morie_kamath_ch3_qa_trigger_template("Where?", "Paris.", "the", "Rome")
  expect_equal(o$prompt,
               "Question: Where? Context: Paris. Answer: the the the Rome")
  expect_equal(o$n, 9L)

  # log-sum hand check: two examples at p = 0.5 give 2 log 0.5
  expect_equal(morie_kamath_ch3_t5_template_obj(
    list(list("g m", "pos"), list("a m", "neg")), "{x} It was {y}",
    function(T, s) 0.5)$estimate, 2 * log(0.5), tolerance = TOL)
  expect_equal(morie_kamath_ch3_prefix_tuning_obj(
    function(z, hp) 0.5, "s:", list("a", "b"), list(0, 1))$estimate,
    -1.3862943611198906, tolerance = TOL)
})

# ------------------------------------------------------------ Ch 4: PEFT

test_that("Ch 4 adapters and low-rank updates match Python", {
  o <- morie_kamath_ch4_series_adapter(matrix(c(1, 2), 1),
                                       matrix(c(1, 0), 2), matrix(c(1, 1), 1))
  expect_equal(as.numeric(o$output), c(2, 3))
  expect_equal(o$bottleneck_rank, 1L)

  o <- morie_kamath_ch4_parallel_adapter(matrix(c(1, 2), 1),
                                         matrix(c(0, 1), 1),
                                         matrix(c(1, 0), 2), matrix(c(1, 1), 1))
  expect_equal(as.numeric(o$output), c(1, 2))
  expect_equal(as.numeric(o$delta), c(0, 0))

  expect_equal(morie_kamath_ch4_full_finetune_obj(
    function(xi, p, t) 0.5, list("doc"), list(list("a", "b")))$estimate,
    2 * log(0.5), tolerance = TOL)
  o <- morie_kamath_ch4_lora_obj(function(xi, p, t) 0.5,
                                 function(xi, p, t) 0.25, list("doc"),
                                 list(list("a")))
  expect_equal(o$estimate, log(0.5), tolerance = TOL)
  expect_equal(o$improvement, log(2), tolerance = TOL)

  o <- morie_kamath_ch4_lora_forward(diag(2), matrix(c(1, 0), 2),
                                     matrix(c(1, 0), 1), c(1, 2))
  expect_equal(o$h, c(2, 2))
  expect_equal(o$r, 1L)
  expect_equal(o$delta_W_rank, 1L)

  o <- morie_kamath_ch4_kronecker_product(matrix(c(1, 2), 1), diag(2))
  expect_equal(o$W, matrix(c(1, 0, 0, 1, 2, 0, 0, 2), 2, 4))
  expect_equal(o$shape, c(2L, 4L))
  expect_equal(o$rank, 2L)
  expect_equal(o$n_params, 6L)

  # Kronecker reshape identity: the matrix-free route equals (A kron B) x
  A <- matrix(c(1, 0, 2, 1), 2); B <- diag(2)
  x <- c(1, 2, 3, 4)
  y <- morie_kamath_ch4_krona_efficient(A, B, x)$y
  expect_equal(y, c(7, 10, 3, 4))
  expect_equal(y, as.numeric(kronecker(A, B) %*% x), tolerance = TOL)

  o <- morie_kamath_ch4_krona_output(matrix(c(1, 0), 1),
                                     matrix(1, 2, 2), diag(2),
                                     matrix(1, 1, 1), 2)
  expect_equal(as.numeric(o$Y), c(3, 1))
  expect_equal(as.numeric(o$base), c(1, 1))
  expect_equal(as.numeric(o$adapter_term), c(2, 0))
  o <- morie_kamath_ch4_krona_tuned_weights(matrix(1, 2, 2), diag(2),
                                            matrix(1, 1, 1), 2)
  expect_equal(o$W_tuned, matrix(c(3, 1, 1, 3), 2))
  # s = 0 leaves W untouched
  expect_equal(morie_kamath_ch4_krona_tuned_weights(matrix(1, 2, 2), diag(2),
                                                    matrix(1, 1, 1), 0)$W_tuned,
               matrix(1, 2, 2))

  o <- morie_kamath_ch4_vera_forward(diag(2), c(2, 3), c(5),
                                     matrix(c(1, 1), 1), matrix(c(1, 2), 2),
                                     c(1, 2))
  expect_equal(o$h, c(31, 92))
  expect_equal(o$n_trainable, 3)
  expect_equal(o$n_trainable_lora, 4)

  o <- morie_kamath_ch4_loftq_objective(2 * diag(2), diag(2),
                                        matrix(c(1, 0), 2), matrix(c(1, 0), 2))
  expect_equal(o$estimate, 1, tolerance = TOL)
  expect_equal(o$quantisation_error, sqrt(2), tolerance = TOL)
})

# ------------------------------------------------------- Ch 5: alignment

test_that("Ch 5 preference and RLHF objectives match Python", {
  o <- morie_kamath_ch5_reward_loss_pairwise(
    function(x, y) c(a = 2, b = 0.5)[[y]], list("p1", "p2"),
    list("a", "b"), list("b", "a"), c(0, 1))
  expect_equal(o$estimate, 0.2014132779827524, tolerance = TOL)
  expect_equal(o$margins, c(1.5, 1.5))
  # hand sum: -log sigmoid(1.5) for both pairs
  expect_equal(o$estimate, -log(1 / (1 + exp(-1.5))), tolerance = TOL)

  o <- morie_kamath_ch5_reward_kl_penalty("x", "y", c(0.5, 0.25),
                                          c(0.25, 0.5), 0.2, r_theta = 1)
  expect_equal(o$estimate, 1, tolerance = TOL)
  expect_equal(o$penalised_reward,
               c(0.8613705638880109, 1.138629436111989), tolerance = TOL)

  expect_equal(morie_kamath_ch5_rm_bradley_terry(
    list("p"), list("a"), list("b"),
    function(x, y) c(a = 1, b = 0)[[y]])$estimate,
    0.31326168751822286, tolerance = TOL)

  o <- morie_kamath_ch5_rlhf_objective(c(y1 = 0.6, y2 = 0.4),
                                       c(y1 = 0.5, y2 = 0.5), c(1, 0), 0.1)
  expect_equal(o$estimate, 0.5979864486449311, tolerance = TOL)
  expect_equal(o$expected_reward, 0.6, tolerance = TOL)
  expect_equal(o$kl, 0.020135513550688863, tolerance = TOL)

  o <- morie_kamath_ch5_ppo_loss(list(c(y1 = 0.6, y2 = 0.4)), list("px"),
                                 list(c("y1", "y2")),
                                 function(x, y) if (y == "y1") 1 else 0, 0.1,
                                 pi_ref = list(c(y1 = 0.5, y2 = 0.5)))
  expect_equal(o$estimate, -0.5979864486449311, tolerance = TOL)
  expect_equal(o$kl, 0.020135513550688863, tolerance = TOL)

  o <- morie_kamath_ch5_rlhf_optimal_policy(c(0.5, 0.5), c(1, 0), 0.5)
  expect_equal(o$pi, c(0.8807970779778824, 0.11920292202211755),
               tolerance = TOL)
  expect_equal(o$Z, 4.194528049465325, tolerance = TOL)
  expect_equal(o$argmax, 0L)            # 0-based

  o <- morie_kamath_ch5_dpo_reward_optimal(c(0.6, 0.4), c(0.5, 0.5), 0.2,
                                           Z = 2)
  expect_equal(o$r, c(0.17509374747077996, 0.09400072584914711),
               tolerance = TOL)
  expect_equal(o$offset, 0.13862943611198905, tolerance = TOL)

  o <- morie_kamath_ch5_bradley_terry_pref(c(a = 2, b = 0.5), "a", "b")
  expect_equal(o$estimate, 0.8175744761936437, tolerance = TOL)
  expect_equal(o$margin, 1.5)
  expect_equal(morie_kamath_ch5_pref_sigmoid_form(c(2, 0.5))$estimate,
               0.8175744761936437, tolerance = TOL)

  o <- morie_kamath_ch5_dpo_pref_simplified(c(0.6, 0.4), c(0.5, 0.5), 0.2)
  expect_equal(o$estimate, 0.5202621528304897, tolerance = TOL)
  expect_equal(o$implicit_reward_w, 0.03646431135879092, tolerance = TOL)
  expect_equal(o$implicit_reward_l, -0.044628710262841945, tolerance = TOL)

  o <- morie_kamath_ch5_dpo_pref_substituted(c(0.6, 0.4), c(0.5, 0.5), 0.2,
                                             Z = 3)
  expect_equal(o$estimate, 0.5202621528304897, tolerance = TOL)
  expect_true(o$z_terms_cancel)

  o <- morie_kamath_ch5_dpo_loss(list(c(0.6, 0.4), c(0.7, 0.3)),
                                 list(c(0.5, 0.5), c(0.5, 0.5)), 0.2)
  expect_equal(o$estimate, 0.6327125652153133, tolerance = TOL)
  expect_equal(o$margins, c(0.08109302162163287, 0.16945957207744072),
               tolerance = TOL)
})

test_that("the Bradley-Terry loss gradient matches finite differences", {
  # d/dm of log(1 + exp(-m)) is -sigmoid(-m); check against the loss itself
  loss <- function(m) morie_kamath_reward_model_training_loss(m, 0)$estimate
  for (m in c(-1.3, 0.0, 0.7, 2.5)) {
    h <- 1e-6
    num <- (loss(m + h) - loss(m - h)) / (2 * h)
    analytic <- -1 / (1 + exp(m))
    expect_equal(num, analytic, tolerance = 1e-6)
  }
})

# --------------------------------------------- Ch 6: bias, toxicity, PII

test_that("Ch 6 bias and toxicity metrics match Python", {
  expect_equal(morie_kamath_ch6_factscore(
    function(x) if (x == "p2") NULL else paste0("r", x),
    list("p1", "p2", "p3"), function(r) c("f1", "f2"), c("f1"))$estimate,
    0.5)
  expect_equal(morie_kamath_ch6_alignment_function(
    "a", "b", "3way", function(a, b) "CONTRADICT")$estimate, 0)
  expect_equal(morie_kamath_ch6_alignscore_total_loss(
    1, 2, 3, c(0.5, 0.25, 0.25))$estimate, 1.75)

  W1 <- diag(2); W2 <- -diag(2)
  A1 <- diag(2); A2 <- matrix(c(1, 2, 1, 0), 2)
  expect_equal(morie_kamath_ch6_weat_similarity(c(1, 0), W1, W2)$estimate, 1)
  o <- morie_kamath_ch6_weat_function(A1, A2, W1, W2)
  expect_equal(o$estimate, -0.4142135623730949, tolerance = TOL)
  expect_equal(unname(o$s_A1), c(1, 1), tolerance = TOL)
  expect_equal(unname(o$s_A2), c(1.414213562373095, 1), tolerance = TOL)
  o <- morie_kamath_ch6_weat_effect_size(A1, A2, W1, W2)
  expect_equal(o$estimate, -1.1547005383792515, tolerance = TOL)
  expect_equal(o$std, 0.17935973380357514, tolerance = TOL)   # population sd
  expect_equal(morie_kamath_ch6_ceat_random_effects(
    list(A1, A1), list(A2, A2), list(W1, W1), list(W2, W2),
    c(1, 3))$estimate, -1.1547005383792515, tolerance = TOL)

  expect_equal(morie_kamath_ch6_lpbs_bias(c(0.6, 0.4), c(0.5, 0.5))$estimate,
               0.4054651081081643, tolerance = TOL)
  o <- morie_kamath_ch6_cbs_variance(c("w1", "w2"), c("g1", "g2"),
                                     matrix(c(0.6, 0.3, 0.4, 0.7), 2),
                                     matrix(0.5, 2, 2))
  expect_equal(o$estimate, 0.11028945226373733, tolerance = TOL)
  expect_equal(unname(o$per_word),
               c(0.041100488473291334, 0.17947841605418333), tolerance = TOL)

  expect_equal(morie_kamath_ch6_pll(c(0.5, 0.25, 0.5))$estimate,
               -2.772588722239781, tolerance = TOL)
  expect_equal(morie_kamath_ch6_cps_metric(c(0.5, 0.25), c("m"))$estimate,
               -2.0794415416798357, tolerance = TOL)
  expect_equal(morie_kamath_ch6_cat_metric(c(0.5, 0.25),
                                           c("u1", "u2"))$estimate,
               -1.0397207708399179, tolerance = TOL)

  o <- morie_kamath_ch6_sgs_invariance(c("a", "b", "c"), c("a", "x", "c"))
  expect_equal(o$estimate, 2 / 3, tolerance = TOL)
  expect_equal(o$n_invariant, 2L)

  o <- morie_kamath_ch6_co_occurrence_bias("w", c(w = 3, z = 1),
                                           c(w = 1, z = 3))
  expect_equal(o$estimate, 1.0986122886681098, tolerance = TOL)
  expect_equal(o$p_given_Ai, 0.75)

  o <- morie_kamath_ch6_demographic_representation(
    "G", c("he", "him"), list("he saw him", "she saw"))
  expect_equal(o$estimate, 2)
  expect_equal(o$share_of_tokens, 0.4, tolerance = TOL)
  o <- morie_kamath_ch6_stereotypical_assoc(
    "nurse", c("she", "he"), list("she is a nurse", "he is a doctor"))
  expect_equal(o$estimate, 1)
  expect_equal(o$n_outputs_with_w, 1L)

  o <- morie_kamath_ch6_honest_score(list(c("a", "b"), c("c", "d")), 2,
                                     hurtlex = c("b", "d"))
  expect_equal(o$estimate, 0.5)
  expect_equal(o$n_hurtful, 2L)

  E <- list(m1 = c(1, 0), f1 = c(0, 1), m2 = c(2, 0), f2 = c(0, 2))
  P <- list(c("m1", "f1"), c("m2", "f2"))
  expect_equal(morie_kamath_ch6_debias_regularizer(P, E, 0.5)$estimate, 5)
  o <- morie_kamath_ch6_gender_direction(P, E)
  expect_equal(o$g, c(-1.5, 1.5))
  expect_equal(o$norm, 2.1213203435596424, tolerance = TOL)
  expect_equal(morie_kamath_ch6_gender_projection_reg(
    matrix(c(1, 2, 1, 0), 2), c(0, 3))$estimate, 1, tolerance = TOL)

  expect_equal(morie_kamath_ch6_ear_entropy_reg(
    list(matrix(c(0.5, 1, 0.5, 0), 2)), lam = 2)$estimate,
    -0.6931471805599453, tolerance = TOL)
  expect_equal(morie_kamath_ch6_log_prob_ratio_attr(
    c(0.6, 0.4), c(0.4, 0.6), lam = 2)$estimate, 0, tolerance = 1e-12)

  o <- morie_kamath_ch6_emt_metric(c("a", "b", "c"), c(0.1, 0.9, 0.4))
  expect_equal(o$estimate, 0.9)
  expect_equal(o$argmax_index, 1L)      # 0-based
  expect_equal(morie_kamath_ch6_toxic_fraction(c("a", "b", "c"),
                                               c(0.1, 0.9, 0.5))$estimate,
               2 / 3, tolerance = TOL)
  expect_equal(morie_kamath_ch6_toxicity_probability(
    list(c("a", "b"), c("c", "d")),
    function(y) c(a = 0.9, b = 0.1, c = 0.1, d = 0.2)[[y]])$estimate, 0.5)

  o <- morie_kamath_ch6_lstm_chain_rule(c(0.5, 0.25, 0.5))
  expect_equal(o$estimate, 0.0625)
  expect_equal(o$log_prob, -2.772588722239781, tolerance = TOL)

  o <- morie_kamath_ch6_lstm_softmax_word(diag(2), NULL, c(1, 0), c(0, 0.5))
  expect_equal(o$p, c(0.6224593312018546, 0.37754066879814546),
               tolerance = TOL)
  expect_equal(o$argmax, 0L)
  o <- morie_kamath_ch6_affect_lm(diag(2), matrix(c(0, 1, 1, 0), 2), NULL,
                                  NULL, c(1, 0), c(1, 0), 2, c(0, 0.5))
  expect_equal(o$p, c(0.18242552380635632, 0.8175744761936437),
               tolerance = TOL)
  expect_equal(o$affect_term, c(0, 2))

  expect_equal(morie_kamath_ch6_gedi_combined_loss(2, 4, 0.25)$estimate, 3.5)
  expect_equal(morie_kamath_ch6_self_diagnosis_prob(
    "t", "tox", function(p) c(Yes = 0.3, No = 0.5))$estimate,
    0.37499999999999994, tolerance = TOL)

  o <- morie_kamath_ch6_pii_likelihood(c(0.5, 0.25), c("a"), c("q"), 1, 2)
  expect_equal(o$estimate, 0.125)
  expect_equal(o$context_lengths, c(1L, 2L))

  o <- morie_kamath_ch6_differential_privacy(
    function(D) if (D == "A") c(s1 = 0.6, s2 = 0.4) else c(s1 = 0.4, s2 = 0.6),
    "A", "B", c("s1"), 0.5)
  expect_equal(o$estimate, 0.4054651081081642, tolerance = TOL)
  expect_true(o$satisfied)
  expect_equal(o$bound, 0.6594885082800513, tolerance = TOL)

  o <- morie_kamath_ch6_perplexity_leakage(c("s1", "s2"),
                                           c(s1 = 10, s2 = 5),
                                           c(s1 = 2, s2 = 4))
  expect_equal(o$estimate, 1.6094379124341003, tolerance = TOL)
  expect_equal(o$argmax, "s1")
  expect_equal(o$n_leaking, 2L)
})

# ------------------------------------------------------- Ch 7 and Ch 8

test_that("Ch 7 RAG and Ch 8 generation metrics match Python", {
  expect_equal(morie_kamath_ch7_faithfulness_metric(c(1, 0, 1, 1))$estimate,
               0.75)
  o <- morie_kamath_ch7_answer_relevance(diag(2), c(1, 1))
  expect_equal(o$estimate, 0.7071067811865475, tolerance = TOL)
  expect_equal(o$similarities, rep(0.7071067811865475, 2), tolerance = TOL)

  o <- morie_kamath_ch8_perplexity(c("a", "b"), p_theta = c(0.5, 0.25))
  expect_equal(o$estimate, 2.82842712474619, tolerance = TOL)
  expect_equal(o$mean_nll, 1.0397207708399179, tolerance = TOL)

  expect_equal(morie_kamath_ch8_bleu_precision(
    matrix(c(2, 1, 4, 3), 2))$p_n, c(0.5, 1 / 3), tolerance = TOL)
  o <- morie_kamath_ch8_bleu_n_geom_mean(c(0.5, 0.25))
  expect_equal(o$estimate, 0.3535533905932738, tolerance = TOL)
  expect_equal(o$log_mean, -1.0397207708399179, tolerance = TOL)
  # a zero precision collapses the mean, as in Python
  expect_equal(morie_kamath_ch8_bleu_n_geom_mean(c(0.5, 0))$estimate, 0)
  expect_equal(morie_kamath_ch8_brevity_penalty(5, 7)$estimate,
               0.6703200460356393, tolerance = TOL)
  expect_equal(morie_kamath_ch8_brevity_penalty(9, 7)$estimate, 1)
  expect_equal(morie_kamath_ch8_bleu_final(0.8, c(0.5, 0.25))$estimate,
               0.28284271247461906, tolerance = TOL)

  expect_equal(morie_kamath_ch8_rouge_n(
    list(c("a", "b", "c"), c("a", "b", "d")), 2,
    candidate = c("a", "b", "c", "a"))$estimate, 0.75)

  X <- diag(2); Y <- matrix(c(1, 2, 1, 0), 2)
  o <- morie_kamath_ch8_bertscore_recall(X, Y)
  expect_equal(o$estimate, 1.5)
  expect_equal(o$greedy_match, c(1L, 0L))     # 0-based
  o <- morie_kamath_ch8_bertscore_precision(X, Y)
  expect_equal(o$estimate, 1.5)
  expect_equal(o$greedy_match, c(0L, 0L))
  expect_equal(morie_kamath_ch8_bertscore_f1(0.5, 0.25)$estimate, 1 / 3,
               tolerance = TOL)

  o <- morie_kamath_ch8_moverscore_distance(c(1, 2), c(4, 6))
  expect_equal(o$estimate, 5)
  expect_equal(morie_kamath_ch8_smd(c(1, 2), c(4, 6))$estimate, 5)
  o <- morie_kamath_ch8_ngram_embedding(c(1, 2, 3, 4), 1, 2)
  expect_equal(o$estimate, 5)                 # window starts at 0-based i = 1
  expect_equal(o$window, c(2, 3))
  o <- morie_kamath_ch8_ngram_weight(matrix(c(1, 3, 2, 4), 2))
  expect_equal(o$weights, c(0.3, 0.7), tolerance = TOL)
  expect_equal(o$Z, 10)

  expect_equal(morie_kamath_ch8_geval_score(c(1, 2, 3),
                                            c(0.2, 0.5, 0.3))$estimate,
               2.0999999999999996, tolerance = TOL)
  expect_equal(morie_kamath_ch8_pass_at_k(10, 3, 2)$estimate,
               0.5333333333333334, tolerance = TOL)
  expect_equal(morie_kamath_ch8_pass_at_k(5, 0, 2)$estimate, 0)
  expect_equal(morie_kamath_pass_at_k(10, 3, 2)$estimate,
               0.5333333333333334, tolerance = TOL)
  expect_equal(morie_kamath_pass_at_k(5, 0, 2)$empirical_rate, 0)
})

test_that("the WMD optimum equals a Birkhoff brute-force search", {
  C <- matrix(c(0, 3, 2, 1, 1, 4), 2, 3)
  o <- morie_kamath_ch8_wmd(c(0.5, 0.5), c(0.2, 0.5, 0.3), C)
  expect_equal(o$estimate, 0.8, tolerance = TOL)
  expect_equal(o$flow, matrix(c(0.2, 0, 0, 0.5, 0.3, 0), 2, 3),
               tolerance = TOL)
  expect_true(o$optimal)
  # duality certificate: <C, F> == a.u + b.v
  expect_equal(o$estimate, o$dual_objective, tolerance = 1e-9)

  # Birkhoff: with equal uniform marginals the optimum sits at a
  # permutation matrix, so enumerate them all.
  set.seed(11)
  Cp <- matrix(round(runif(16, 0, 9), 3), 4, 4)
  a <- rep(0.25, 4); b <- rep(0.25, 4)
  perms <- list()
  gen <- function(prefix, left) {
    if (!length(left)) { perms[[length(perms) + 1L]] <<- prefix; return(NULL) }
    for (v in left) gen(c(prefix, v), setdiff(left, v))
  }
  gen(integer(0), 1:4)
  brute <- min(vapply(perms, function(p)
    0.25 * sum(Cp[cbind(1:4, p)]), numeric(1)))
  expect_equal(morie_kamath_ch8_wmd(a, b, Cp)$estimate, brute,
               tolerance = 1e-9)

  # the same LP drives MoverScore, so the two agree on the transport cost
  H <- matrix(c(0, 1, 0, 0), 2); R <- matrix(c(0, 1, 1, 1), 2)
  ms <- morie_kamath_moverscore(H, R)
  expect_equal(ms$wmd, 1, tolerance = TOL)
  expect_equal(ms$estimate, 0.29289321881345254, tolerance = TOL)
  expect_equal(ms$normalizer, 1.4142135623730951, tolerance = TOL)
})

# ------------------------------------------------------ Ch 9: multimodal

test_that("Ch 9 multimodal losses match Python", {
  expect_equal(morie_kamath_ch9_modality_encoder(
    "img", function(i) matrix(c(3, 4), 1))$estimate, 5)
  expect_equal(morie_kamath_ch9_input_alignment_loss(
    list(matrix(c(1, 2), 1), matrix(0, 1, 2)), "ft", "t",
    llm = function(p, f) sum(p),
    loss_fn = function(pr, t) abs(pr - 1))$estimate, 1)
  o <- morie_kamath_ch9_input_projector(matrix(c(1, 2), 1),
                                        in_align = diag(c(1, 2)))
  expect_equal(as.numeric(o$prompts), c(1, 4))
  expect_equal(o$estimate, 4.123105625617661, tolerance = TOL)
  o <- morie_kamath_ch9_llm_signal_tokens(
    "p", "f", llm = function(p, f) list("hello", c(1, 2, 3)))
  expect_equal(o$estimate, 3L)
  expect_equal(o$text, "hello")

  o <- morie_kamath_ch9_clip_image_to_text(diag(2), diag(2), 0.5)
  expect_equal(o$estimate, 0.1269280110429727, tolerance = TOL)
  expect_equal(o$per_pair, rep(0.1269280110429727, 2), tolerance = TOL)
  expect_equal(morie_kamath_ch9_clip_text_to_image(diag(2), diag(2),
                                                   0.5)$estimate,
               0.1269280110429727, tolerance = TOL)
  expect_equal(morie_kamath_ch9_clip_contrastive_total(0.5, 0.25)$estimate,
               0.75)

  o <- morie_kamath_ch9_mml_vlm_loss(c(0.5, 0.25), c(0.5))
  expect_equal(o$estimate, 2.772588722239781, tolerance = TOL)
  expect_equal(o$positive_loss, 2.0794415416798357, tolerance = TOL)
  expect_equal(morie_kamath_ch9_itm_hard_negative(c(0.5, 0.25),
                                                  c(0.5))$estimate,
               2.772588722239781, tolerance = TOL)

  o <- morie_kamath_ch9_simvlm_mlm(NULL, c(0.5, 0.25, 0.5),
                                   matrix(c(1, 2), 1), c(0, 2))
  expect_equal(o$estimate, 0.6931471805599453, tolerance = TOL)
  expect_equal(o$n_image_regions, 1L)
  o <- morie_kamath_ch9_simvlm_prefixlm(
    NULL, matrix(c(0.5, 0.5, 0.5, 0.25, 0.25, 0.5), 2), 1)
  expect_equal(o$estimate, 2.0794415416798357, tolerance = TOL)
  expect_equal(unname(o$per_sequence), rep(2.0794415416798357, 2),
               tolerance = TOL)

  o <- morie_kamath_ch9_moc_loss(NULL, NULL, NULL,
                                 matrix(c(0.7, 0.2, 0.3, 0.8), 2),
                                 labels = c(0, 1))
  expect_equal(o$estimate, 0.5798184952529422, tolerance = TOL)
  expect_equal(o$as_printed, -0.5798184952529422, tolerance = TOL)

  o <- morie_kamath_ch9_itm_loss(c(0.9, 0.2), NULL, NULL, c(1, 0))
  expect_equal(o$estimate, 0.164252033486018, tolerance = TOL)
  expect_equal(o$per_pair, c(0.10536051565782628, 0.2231435513142097),
               tolerance = TOL)

  expect_equal(morie_kamath_ch9_mmllm_autoregressive(c(0.5, 0.25),
                                                     NULL)$estimate,
               2.0794415416798357, tolerance = TOL)
  expect_equal(morie_kamath_ch9_itg_loss(NULL, list(c(0.5, 0.25),
                                                    c(0.5)))$estimate,
               2.772588722239781, tolerance = TOL)

  o <- morie_kamath_ch9_fom_loss(c(0, 1), c(1, 0),
                                 P = matrix(c(0.2, 0.6, 0.8, 0.4), 2))
  expect_equal(o$estimate, 0.7339691750802004, tolerance = TOL)
  expect_equal(o$per_frame, c(0.2231435513142097, 0.5108256237659907),
               tolerance = TOL)
  expect_equal(morie_kamath_ch9_mm_instr_predict(
    "I", "M", function(I, M) "ans")$estimate, "ans")

  o <- morie_kamath_ch9_output_projector_mse(
    list(matrix(c(1, 2), 1), matrix(0, 1, 2)), matrix(c(1, 1), 1), "t")
  expect_equal(o$estimate, 0.5)
  expect_equal(o$argmin, 0L)
  expect_equal(o$losses, c(0.5, 1))
  expect_equal(as.numeric(morie_kamath_ch9_output_alignment(
    matrix(c(1, 2), 1), out_align = diag(c(1, 2)))$features), c(1, 4))

  o <- morie_kamath_ch9_ldm_loss(matrix(c(1, 0, 2, 1), 2), NULL, NULL,
                                 eps_net = matrix(c(1, 0, 1, 0), 2))
  expect_equal(o$estimate, 1)
  expect_equal(unname(o$per_sample), c(1, 1))

  o <- morie_kamath_ch9_flamingo_factorized(c(0.5, 0.25))
  expect_equal(o$estimate, 0.125)
  expect_equal(o$nll, 2.0794415416798357, tolerance = TOL)
  expect_equal(morie_kamath_ch9_flamingo_dataset_mix(
    list(list(c(0.5, 0.25)), list(c(0.5))), c(1, 2))$estimate,
    3.465735902799726, tolerance = TOL)
})

# ------------------------------------------- named modules: 3H .. kmicl

test_that("named alignment, PEFT and retrieval modules match Python", {
  expect_equal(morie_kamath_3h_alignment(c(1, 0), c(0.5, 0.5),
                                         c(0, 1))$score,
               c(0.5, 0.5), tolerance = TOL)

  o <- morie_kamath_adalora_rank_allocation(diag(2), c(3, 1), diag(2),
                                            target_rank = 1)
  expect_equal(o$Delta_W, matrix(c(3, 0, 0, 0), 2))
  expect_equal(o$kept, 0L)               # 0-based
  expect_equal(o$estimate, 3)

  o <- morie_kamath_houlsby_adapter(matrix(c(1, 2, 3), 1),
                                    matrix(c(1, 0, 0), 1),
                                    matrix(c(1, 0, 0), 3))
  expect_equal(as.numeric(o$h_adapted), c(1.8413447460685428, 2, 3),
               tolerance = TOL)
  expect_equal(as.numeric(o$bottleneck), 0.8413447460685429, tolerance = TOL)

  o <- morie_kamath_alibi_bias(matrix(c(1, 0), 1), diag(2), diag(2), c(1))
  expect_equal(as.numeric(o$output[[1]]),
               c(0.42729570720446314, 0.5727042927955369), tolerance = TOL)
  expect_equal(as.numeric(o$bias), c(0, 1))

  o <- morie_kamath_autoprompt_gradient_search(
    list(NULL, "x", NULL), "D",
    function(t, d) sum(nchar(unlist(t))), vocab = list("aa", "b"))
  expect_equal(o$estimate, 3)
  expect_equal(o$trigger_tokens, c("b", "b"))
  expect_equal(o$positions, c(0L, 2L))   # 0-based

  expect_equal(morie_kamath_ragas_answer_relevance(
    "ans", c(1, 1), function(a) list(c(1, 0), c(0, 1)))$estimate,
    0.7071067811865475, tolerance = TOL)
  expect_equal(morie_kamath_ragas_context_relevance(
    c("s1", "s2", "s3"), c(1, 0, 1))$estimate, 2 / 3, tolerance = TOL)

  expect_equal(morie_kamath_bleu_score(c("a", "b", "c", "d"),
                                       list(c("a", "b", "c", "e")),
                                       max_n = 2)$estimate,
               0.7071067811865475, tolerance = TOL)
  expect_equal(morie_kamath_bm25_score(c("cat", "dog"),
                                       c("cat", "cat", "fish"),
                                       c(cat = 1.5, dog = 2), 3)$estimate,
               2.142857142857143, tolerance = TOL)

  o <- morie_kamath_best_of_n_sampling(list("a", "b", "c"),
                                       rewards = c(0.1, 0.9, 0.4))
  expect_equal(o$estimate, 0.9)
  expect_equal(o$best, "b")
  expect_equal(o$best_index, 1L)         # 0-based
  expect_equal(morie_kamath_bradley_terry_preference(c(2, 0),
                                                     c(0.5, 1))$p_pref,
               c(0.8175744761936437, 0.2689414213699951), tolerance = TOL)
  expect_equal(morie_kamath_bertscore(
    c("a", "b"), c("a", "c"),
    list(a = c(1, 0), b = c(0, 1), c = c(1, 1)))$estimate,
    0.8535533905932737, tolerance = TOL)

  expect_equal(morie_kamath_constitutional_ai_loop(
    "r0", list("p1", "p2"),
    function(st, p, y, cr) paste(st, p, y, sep = ":"))$revised_response,
    "revise:p2:revise:p1:r0")

  o <- morie_kamath_expert_capacity_factor(100, 8, 1.25)
  expect_equal(o$estimate, 15.625)
  expect_equal(o$slots, 16L)
  expect_equal(o$min_dropped, 0L)

  o <- morie_kamath_christiano_deep_rl_feedback(
    list(c("w1", "l1"), c("w2", "l2")),
    function(s) if (startsWith(s, "w")) 2 else 0.5)
  expect_equal(o$estimate, 0.4028265559655048, tolerance = TOL)
  expect_equal(o$mean_loss, 0.2014132779827524, tolerance = TOL)
  expect_equal(o$pair_accuracy, 1)

  o <- morie_kamath_chinchilla_compute_optimal(1.2e21)
  expect_equal(o$N_opt, 3162277660.1683793, tolerance = 1e-6)
  expect_equal(o$D_opt, 63245553203.367584, tolerance = 1e-4)
  expect_equal(o$compute_check, 1.2e21, tolerance = 1e6)

  o <- morie_kamath_chain_of_thought("Q",
                                     function(p) "step one. Answer: 42")
  expect_equal(o$answer, "42")
  expect_equal(o$reasoning, "step one.")
  expect_equal(o$prompt, "Q Let's think step by step.")

  o <- morie_kamath_corrective_rag("q", list("d1", "d2", "d3"),
                                   function(q, d)
                                     c(d1 = 0.9, d2 = 0.5, d3 = 0.1)[[d]],
                                   0.8, 0.2)
  expect_equal(o$estimate, 0.9)
  expect_equal(o$action, "use_docs")
  expect_equal(unlist(o$ctx), "d1")

  o <- morie_kamath_cross_encoder_rerank("q", list("a", "b", "c"),
                                         function(q, d)
                                           c(a = 0.1, b = 0.9, c = 0.9)[[d]])
  expect_equal(o$ranking, c(1L, 2L, 0L))  # 0-based, stable
  expect_equal(o$estimate, 0.9)

  o <- morie_kamath_crowspairs_bias(c(-1, -3, -2), c(-2, -1, -2))
  expect_equal(o$estimate, 1 / 3, tolerance = TOL)
  expect_equal(o$n_ties, 1L)

  o <- morie_kamath_double_quantization(c(0.5, -1, 0.25))
  expect_equal(o$scales_int8, c(64L, -127L, 32L))
  expect_equal(o$shared_const, 0.007874015748031496, tolerance = TOL)
  expect_equal(o$estimate, 0.003937007874015741, tolerance = TOL)

  o <- morie_kamath_differential_privacy(0.5, 0.01, p_D = c(0.6, 0.4),
                                         p_Dp = c(0.5, 0.5))
  expect_equal(o$estimate, 0.16948850828005135, tolerance = TOL)
  expect_true(o$guarantee)
  expect_equal(o$worst_event, 1L)         # 0-based

  o <- morie_kamath_dpo_loss(-0.5, -1.5, -1, -1, 0.5)
  expect_equal(o$estimate, 0.4740769841801067, tolerance = TOL)
  expect_equal(o$implicit_reward_w, 0.25)

  o <- morie_kamath_dense_passage_retrieval(
    c(1, 0), matrix(c(1, 0.5, 1, 0, 0.5, 0), 3), 2)
  expect_equal(o$top_k_indices, c(0L, 2L))
  expect_equal(o$top_k_scores, c(1, 1))

  o <- morie_kamath_emergent_abilities(c(1, 10, 100), c(0.1, 0.2, 0.9), 50)
  expect_equal(o$estimate, 0.75, tolerance = TOL)
  expect_equal(o$emergent_score, c(0, 0, 0.9))
  o <- morie_kamath_memorization_exposure(-2, c(-1, -3, -4))
  expect_equal(o$estimate, 1)
  expect_equal(o$rank, 2L)

  expect_equal(morie_kamath_factscore(list("c1", "c2", "c3"),
                                      c("c1", "c3"))$estimate, 2 / 3,
               tolerance = TOL)
  o <- morie_kamath_few_shot_exemplar_selection(
    matrix(c(1, 0, 1, 0, 1, 1), 3), c(1, 0), 2)
  expect_equal(o$selected, c(0L, 2L))
  expect_equal(o$similarities, c(1, 0.7071067811865475), tolerance = TOL)

  o <- morie_kamath_fasttext_subword(
    "ab", list(`<a` = 1, ab = 2, `b>` = 3, `<ab>` = 4), 2, 2)
  expect_equal(o$vector, 10)
  expect_equal(o$ngrams, c("<a", "ab", "b>", "<ab>"))
  expect_equal(o$n_known, 4L)

  expect_equal(morie_kamath_g_eval("x", "y", c(1, 2, 3),
                                   function(x, y, r) c(0, 1, 0))$estimate, 2,
               tolerance = TOL)
  o <- morie_kamath_glove_cost(matrix(c(2, 1, 0, 3), 2), matrix(c(1, 0), 2),
                               matrix(c(1, 1), 2), c(0, 0), c(0, 0),
                               x_max = 2, alpha = 0.5)
  expect_equal(o$estimate, 1.3011076136108928, tolerance = TOL)
  expect_equal(o$n_nonzero, 3L)

  expect_equal(morie_kamath_groundedness_reward(c("A", "b", "c"),
                                                c("a", "b"))$estimate,
               2 / 3, tolerance = TOL)
  o <- morie_kamath_hybrid_retrieval_fusion(c(0.1, 0.9), c(0.8, 0.2), 0.25)
  expect_equal(o$scores, c(0.6250000000000001, 0.375), tolerance = TOL)
  expect_equal(o$ranking, c(0L, 1L))
  o <- morie_kamath_hyde_hypothetical_doc(
    "q", function(q) c(1, 0), list(d1 = c(1, 0), d2 = c(0, 1)), k = 1)
  expect_equal(o$retrieved, "d1")
  expect_equal(o$estimate, 1)

  o <- morie_kamath_in_context_learning_prob(c("e1", "e2"), "q",
                                             function(p, a) 0.25)
  expect_equal(o$estimate, 0.25)
  expect_equal(o$prompt, "e1\ne2\nq")
  expect_equal(o$log_prob, -1.3862943611198906, tolerance = TOL)

  o <- morie_kamath_instruction_tuning_loss(
    matrix(c(1, 0, 2, 0, 1, 0), 3), c(0, 1, 1), c(0, 1, 0))
  expect_equal(o$estimate, 0.22009484928059775, tolerance = TOL)
  expect_equal(o$perplexity, 1.2461949256741174, tolerance = TOL)
  expect_equal(o$n_response_tokens, 2L)
})

# ------------------------------------------ named modules: kmitc .. kmyarn

test_that("named architecture, decoding and tokenizer modules match Python", {
  I <- matrix(c(1, 0, 1, 0, 1, 1), 3)
  o <- morie_kamath_image_text_contrastive(I, I, 0.5)
  expect_equal(o$estimate, 0.6000313142487684, tolerance = TOL)
  expect_equal(o$loss_i2t, 0.6000313142487684, tolerance = TOL)

  o <- morie_kamath_image_text_matching(c(1, 2), c(3), c(1, 0, -1), 0.5)
  expect_equal(o$estimate, 0.18242552380635632, tolerance = TOL)
  expect_equal(o$logit, -1.5)
  expect_equal(o$fused, c(1, 2, 3))

  o <- morie_kamath_kl_reward_shaping(c(1, 2), 0.5, 0.4)
  expect_equal(o$estimate, 1.3, tolerance = TOL)
  expect_equal(o$shaped, c(0.8, 1.8), tolerance = TOL)

  o <- morie_kamath_moe_load_balance_loss(c(0.7, 0.3), c(0.6, 0.4), 2, 0.01)
  expect_equal(o$estimate, 0.0108, tolerance = TOL)
  expect_equal(o$imbalance_ratio, 1.08, tolerance = TOL)

  o <- morie_kamath_lora_weight_update(diag(2), matrix(c(1, 1), 1),
                                       matrix(c(2, 0), 2), 4, 1, c(1, 3))
  expect_equal(o$h, c(33, 3))
  expect_equal(o$scaling, 4)
  expect_equal(o$n_trainable, 4L)

  o <- morie_kamath_llava_visual_instruction(
    "im", diag(2), function(i) matrix(c(1, 2), 1), matrix(c(3, 4), 1))
  expect_equal(as.numeric(o$visual_tokens), c(1, 2))
  expect_equal(o$inputs, matrix(c(1, 3, 2, 4), 2))
  expect_equal(o$n_visual, 1L)

  o <- morie_kamath_multimodal_mae(list(v = 0), list(v = c(1, 2)),
                                   list(v = c(TRUE, FALSE, TRUE)),
                                   decoders = list(v = function(x, m)
                                     c(0.5, 1)))
  expect_equal(o$estimate, 1.25, tolerance = TOL)
  expect_equal(o$n_masked, 2L)

  o <- morie_kamath_mamba_ssm(c(1, 2), c(-1, -2), c(1, 1), c(1, 1), 0.5)
  expect_equal(o$y, c(1, 2.4872050504420375), tolerance = TOL)
  expect_equal(o$states[2, ],
               c(1.3032653298563166, 1.1839397205857212), tolerance = TOL)

  o <- morie_kamath_membership_inference(c(0.1, 0.9, 0.3), 0.5,
                                         labels = c(1, 0, 0))
  expect_equal(o$estimate, 2 / 3, tolerance = TOL)
  expect_equal(o$tpr, 1)
  expect_equal(o$fpr, 0.5)

  o <- morie_kamath_medusa_heads(c(1, 2), list(diag(2),
                                               matrix(c(0, 1, 1, 0), 2)), 2)
  expect_equal(o$tokens, c(1L, 0L))     # 0-based token ids
  expect_equal(o$probabilities, rep(0.7310585786300049, 2), tolerance = TOL)

  o <- morie_kamath_moe_router_softmax(
    c(1, 0), matrix(c(1, 0, 0, 1, 2, 0), 2),
    list(function(x) x * 1, function(x) x * 2, function(x) x * 3), 2)
  expect_equal(o$output, c(2.46211715726001, 0), tolerance = TOL)
  expect_equal(o$gate_weights,
               c(0.2689414213699951, 0, 0.7310585786300049), tolerance = TOL)
  expect_equal(o$selected_experts, c(0L, 2L))

  o <- morie_kamath_nf4_datatype(4)
  expect_equal(o$levels, c(-1.1503493803760056, -0.318639363964377,
                           0.318639363964377, 1.1503493803760056),
               tolerance = 1e-8)
  expect_equal(o$normalized, c(-1, -0.2769935546540007,
                               0.2769935546540007, 1), tolerance = 1e-8)
  expect_equal(o$n_bits, 2)

  expect_equal(morie_kamath_ngram_language_model(3, 12)$estimate, 0.25)

  o <- morie_kamath_nucleus_sampling(c(2, 1, 0), 0.7)
  expect_equal(o$probabilities,
               c(0.7310585786300048, 0.26894142136999516, 0), tolerance = TOL)
  expect_equal(o$kept, c(0L, 1L))       # 0-based
  expect_equal(o$n_kept, 2L)
  expect_equal(o$kept_mass, 0.9099694268296195, tolerance = TOL)

  o <- morie_kamath_nextgpt_any2any(
    list(img = 1, txt = 2),
    list(img = function(v) v * 10, txt = function(v) v * 100),
    function(f) sum(unlist(f)), list(aud = function(s) s + 1))
  expect_equal(o$outputs$aud, 211)
  expect_equal(o$llm_state, 210)
  expect_equal(o$input_modalities, c("img", "txt"))

  o <- morie_kamath_p_tuning_v2(list(list(matrix(c(1, 1), 1),
                                          matrix(c(2, 2), 1))),
                                list(list(matrix(c(3, 3), 1),
                                          matrix(c(4, 4), 1))))
  expect_equal(o$K[[1]], matrix(c(1, 3, 1, 3), 2))
  expect_equal(o$n_trainable, 4L)

  o <- morie_kamath_perplexity(c(-1, -2))
  expect_equal(o$estimate, 4.4816890703380645, tolerance = TOL)
  expect_equal(o$bits_per_token, 2.1640425613334453, tolerance = TOL)

  o <- morie_kamath_pet_loss(c(1, 0), 0, matrix(c(1, 0, 0, 2), 2),
                             c(0, -100), 0.5)
  expect_equal(o$estimate, 0.4698925312773342, tolerance = TOL)
  expect_equal(o$loss_ce, 0.3132616875182228, tolerance = TOL)
  expect_equal(o$loss_mlm, 0.3132616875182228, tolerance = TOL)

  att <- function(v) v * 0.5; ffn <- function(v) v * 0.25
  o <- morie_kamath_post_ln_transformer(matrix(c(1, 2, 3), 1), att, ffn)
  expect_equal(as.numeric(o$output),
               c(-1.224740952200685, 0, 1.224740952200685), tolerance = TOL)
  expect_equal(as.numeric(o$after_attention),
               c(-1.2247407889290967, 0, 1.2247407889290967), tolerance = TOL)
  o <- morie_kamath_pre_ln_transformer(matrix(c(1, 2, 3), 1), att, ffn)
  expect_equal(as.numeric(o$output),
               c(0.08144682251526852, 2, 3.9185531774847315), tolerance = TOL)
  expect_equal(as.numeric(o$after_attention),
               c(0.3876321570458049, 2, 3.612367842954195), tolerance = TOL)

  o <- morie_kamath_ppo_rlhf_objective(c(1, 2), c(-1, -2), c(-1.5, -1.5), 0.5)
  expect_equal(o$estimate, 1.5, tolerance = TOL)
  expect_equal(o$kl_estimate, 0, tolerance = TOL)

  o <- morie_kamath_prefix_tuning(matrix(c(1, 0), 1), matrix(c(0, 1), 1),
                                  matrix(c(0, 1), 1), matrix(c(1, 1), 1),
                                  Q = matrix(c(1, 0), 1))
  expect_equal(o$K, matrix(c(1, 0, 0, 1), 2))
  expect_equal(o$estimate, 0.3302384506733431, tolerance = TOL)
  expect_equal(o$prefix_attention_mass, 0.6697615493266569, tolerance = TOL)

  o <- morie_kamath_prompt_tuning(matrix(c(1, 2), 1),
                                  matrix(c(3, 5, 4, 6), 2))
  expect_equal(o$X_aug, matrix(c(1, 3, 5, 2, 4, 6), 3))
  expect_equal(o$n_trainable, 2L)

  o <- morie_kamath_q_former(matrix(c(1, 0), 1), diag(2))
  expect_equal(as.numeric(o$Z),
               c(0.6697615493266569, 0.3302384506733431), tolerance = TOL)
  expect_equal(o$compression, 2)

  o <- morie_kamath_qlora_4bit(list(codes = matrix(c(0, 1, 3, 2), 2),
                                    absmax = 2), matrix(c(1, 0), 1),
                               matrix(c(1, 0), 2), 2, 1, c(1, 1))
  expect_equal(o$W0_dequantized,
               matrix(c(-2, -1.4151375411082594,
                        -0.8336377067759401, -1.0844181998642561), 2),
               tolerance = 1e-8)
  expect_equal(o$h, c(-0.83363770677594, -2.4995557409725153),
               tolerance = 1e-8)

  o <- morie_kamath_retnet_retention(diag(2), diag(2),
                                     matrix(c(1, 3, 2, 4), 2), 0.5)
  expect_equal(o$output, matrix(c(1, 3, 2, 4), 2), tolerance = TOL)
  # the recurrent form must reproduce the parallel one
  expect_equal(o$recurrent_output, o$output, tolerance = TOL)
  expect_equal(o$decay, matrix(c(1, 0.5, 0, 1), 2))

  o <- morie_kamath_rlhf_pipeline(list("d"), list(c("a", "b")), "pi0",
                                  sft = function(p, d) "sft",
                                  train_rm = function(p) function(y) 1,
                                  ppo = function(p, r, ref) "rlhf")
  expect_equal(o$policy, "rlhf")
  expect_equal(o$policy_sft, "sft")
  expect_true(o$kl_reference_is_sft)

  o <- morie_kamath_rlaif_objective(list(c("a", "b"), c("b", "c"),
                                         c("c", "a"), c("a", "c")))
  expect_equal(o$items, c("a", "b", "c"))
  expect_equal(o$strengths, c(0.47862029319495236, 0.3145962122772225,
                              0.20678349452782505), tolerance = 1e-8)
  expect_equal(o$estimate, 0.6419534071919634, tolerance = 1e-8)
  expect_equal(o$accuracy, 0.75)

  o <- morie_kamath_reward_model_training_loss(c(2, 1), c(0.5, 1.5))
  expect_equal(o$estimate, 0.5877451310814296, tolerance = TOL)
  expect_equal(o$accuracy, 0.5)
  expect_equal(o$mean_margin, 0.5)

  o <- morie_kamath_rms_norm(c(3, 4))
  expect_equal(o$y, c(0.8485281034827336, 1.1313708046436448),
               tolerance = TOL)
  expect_equal(o$rms, 3.535534047354091, tolerance = TOL)

  o <- morie_kamath_rotary_positional_embedding(
    matrix(c(1, 1, 0, 1, 0, 1, 1, 1), 2))
  expect_equal(o$y[1, ], c(1, 0, 0, 1), tolerance = TOL)
  expect_equal(o$y[2, ], c(-0.30116867893975674, 1.3817732906760363,
                           0.9899501670824986, 1.009949833750832),
               tolerance = TOL)
  expect_equal(o$theta, c(1, 0.01), tolerance = TOL)

  o <- morie_kamath_rouge_n(c("a", "b", "c", "a"), c("a", "b", "d"), n = 1)
  expect_equal(o$recall, 2 / 3, tolerance = TOL)
  expect_equal(o$precision, 0.5)
  expect_equal(o$f1, 0.5714285714285715, tolerance = TOL)

  o <- morie_kamath_reciprocal_rank_fusion(list(c("a", "b", "c"),
                                                c("b", "a")), k = 1)
  expect_equal(o$ranking, c("a", "b", "c"))
  expect_equal(unname(o$scores[o$ranking]),
               c(0.8333333333333333, 0.8333333333333333, 0.25),
               tolerance = TOL)

  o <- morie_kamath_rejection_sampling_finetune(
    list("p1", "p2"), list(list("a", "b", "c"), list("d", "e")),
    list(c(0.1, 0.9, 0.5), c(0.2, 0.8)), 2)
  expect_equal(vapply(o$retained, function(p) p[[2]], character(1)),
               c("b", "c", "d", "e"))
  expect_equal(o$retained_rewards, c(0.9, 0.5, 0.2, 0.8))
  expect_equal(o$n_dropped, 1L)

  expect_equal(morie_kamath_rwkv_time_mix(c(1, 0.5, -0.5), c(1, 2, 3),
                                          0.5, 0.25)$wkv,
               c(1, 1.562176500885798, 1.9203975980300019), tolerance = TOL)

  o <- morie_kamath_self_consistency(c("a", "b", "a", "c"))
  expect_equal(o$answer, "a")
  expect_equal(o$votes, 2L)
  expect_equal(o$agreement, 0.5)
  expect_false(o$tie)

  o <- morie_kamath_scaling_laws(c(1e6, 1e8), 1e10, 0.5, 0.1)
  expect_equal(o$loss, c(100.1, 10.1), tolerance = 1e-8)
  expect_equal(o$reducible, c(100, 10), tolerance = 1e-8)

  o <- morie_kamath_speculative_decoding(c(0.6, 0.3, 0.1), c(0.3, 0.5, 0.2),
                                         proposed = 0)
  expect_equal(o$estimate, 0.5, tolerance = TOL)
  expect_equal(o$ratio, 0.5, tolerance = TOL)
  expect_equal(o$residual, c(0, 2 / 3, 1 / 3), tolerance = TOL)
  expect_equal(o$rejection_rate, 0.30000000000000004, tolerance = TOL)

  o <- morie_kamath_self_rag("ctx", function(c, q)
    "[Retrieve] [Relevant] [Utility:4]")
  expect_equal(o$tokens, c("[Retrieve]", "[Relevant]", "[Utility:4]"))
  expect_true(o$retrieve)
  expect_true(o$relevant)
  expect_equal(o$utility, 4L)
  expect_equal(o$estimate, 3L)

  o <- morie_kamath_step_back_prompting(
    "what year did X happen", function(q) "history of X",
    retrieve = function(q) if (q == "history of X") list("d1", "d2")
    else list("d2", "d3"))
  expect_equal(o$step_back_query, "history of X")
  expect_equal(unlist(o$context), c("d1", "d2", "d3"))
  expect_equal(o$n_context, 3L)

  o <- morie_kamath_summarize_from_feedback(list(c(2, 1), c(1, 2)),
                                            c(1, 2), c(-1, -2),
                                            c(-1.5, -1.5), 0.5)
  expect_equal(o$loss_rm, 0.8132616875182228, tolerance = TOL)
  expect_equal(o$rm_accuracy, 0.5)
  expect_equal(o$objective, 1.5, tolerance = TOL)

  o <- morie_kamath_stereoset_bias(c(0.6, 0.4, 0.5), c(0.5, 0.5, 0.5))
  expect_equal(o$estimate, 1 / 3, tolerance = TOL)
  expect_equal(o$n_ties, 1L)
  expect_equal(o$bias_magnitude, 0.16666666666666669, tolerance = TOL)

  o <- morie_kamath_swiglu_activation(c(1, 2), diag(2),
                                      matrix(c(0, 1, 1, 0), 2))
  expect_equal(o$output, c(1.4621171572600098, 1.7615941559557646),
               tolerance = TOL)
  expect_equal(o$gate, c(0.7310585786300049, 1.7615941559557646),
               tolerance = TOL)
  expect_equal(o$linear, c(2, 1))

  o <- morie_kamath_temperature_sampling(c(2, 1, 0), 0.5)
  expect_equal(o$probabilities, c(0.8668133321973347, 0.11731042782619835,
                                  0.015876239976466762), tolerance = TOL)
  expect_equal(o$entropy, 0.44105744405816344, tolerance = TOL)

  o <- morie_kamath_moe_top_k_gating(c(0.1, 0.5, 0.4), 2)
  expect_equal(o$weights, c(0, 0.5555555555555556, 0.4444444444444445),
               tolerance = TOL)
  expect_equal(o$selected_experts, c(1L, 2L))   # 0-based
  expect_equal(o$kept_mass, 0.9, tolerance = TOL)

  o <- morie_kamath_tree_of_thoughts("root", 2, 2, function(s, b)
    list(list(paste0(s, "a"), 1), list(paste0(s, "b"), 2)), beam = 1)
  expect_equal(o$best_state, "rootbb")
  expect_equal(unlist(o$best_path), c("rootb", "rootbb"))
  expect_equal(o$best_score, 4)
  expect_equal(o$n_expanded, 2L)

  o <- morie_kamath_toxigen_score("t", function(t) c(0.3, 0.7))
  expect_equal(o$estimate, 0.7)
  expect_true(o$toxic)

  o <- morie_kamath_vera_adapter(diag(2), matrix(c(1, 1), 1),
                                 matrix(c(1, 2), 2), c(2, 3), c(5), c(1, 2))
  expect_equal(o$h, c(31, 92))
  expect_equal(o$n_trainable, 3L)

  o <- morie_kamath_verbalizer_mapping(c(1, 0, 2), c("gr", "te", "ok"),
                                       list(pos = c("gr", "ok"),
                                            neg = c("te")))
  expect_equal(unname(o$probabilities[c("pos", "neg")]),
               c(0.9099694268296195, 0.09003057317038046), tolerance = TOL)
  expect_equal(o$prediction, "pos")
  expect_equal(o$mass_outside, 0, tolerance = 1e-12)

  o <- morie_kamath_word2vec_skipgram(c(0, 1), c(1, 0), diag(2), diag(2))
  expect_equal(o$log_likelihood, -2.6265233750364456, tolerance = TOL)
  expect_equal(unname(o$per_pair), rep(-1.3132616875182228, 2),
               tolerance = TOL)

  o <- morie_kamath_yarn_context_extrapolation(10000, 4, 8)
  expect_equal(o$theta, c(1, 0.1, 0.01, 0.001), tolerance = TOL)
  expect_equal(o$theta_new, c(1, 0.07071067811865477, 0.005,
                              0.0003535533905932738), tolerance = TOL)
  expect_equal(o$scale_factors, c(1, 0.7071067811865476, 0.5,
                                  0.3535533905932738), tolerance = TOL)
  expect_equal(morie_kamath_yarn_context_extrapolation(
    10000, 4, 8, ramp = c(1, 3))$theta_new,
    c(1, 0.1, 0.0075, 0.0003535533905932738), tolerance = TOL)
})

test_that("the LCG-driven T5 span corruption reproduces Python draw for draw", {
  o <- morie_kamath_t5_span_corruption(letters[1:16], mean_span_len = 2,
                                       corruption_rate = 0.25, seed = 7)
  expect_equal(o$input, c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j",
                          "k", "<extra_id_0>", "m", "<extra_id_1>"))
  expect_equal(o$target, c("<extra_id_0>", "l", "<extra_id_1>", "n", "o",
                           "p", "<extra_id_2>"))
  expect_equal(o$span_lengths, c(1, 3))
  expect_equal(o$n_spans, 2L)
})

test_that("the unigram EM tokenizers match Python", {
  o <- morie_kamath_unigram_lm_tokenizer(c("abab"), c("a", "b", "ab"),
                                         max_iter = 20)
  expect_equal(o$log_likelihood, 0, tolerance = 1e-9)
  expect_equal(o$probs[["ab"]], 1, tolerance = 1e-9)
  expect_equal(o$segmentations[[1]], c("ab", "ab"))

  o <- morie_kamath_sentencepiece_tokenizer(c("abab", "abc"), 5,
                                            max_piece_len = 3)
  expect_equal(sort(names(o$probs)), c("a", "ab", "abc", "b", "c"))
  expect_equal(o$vocab_size, 5L)
  expect_equal(o$log_likelihood, -1.909542505, tolerance = 1e-7)
})

test_that("the reused Ch 2 and Alammar cores are the ones actually called", {
  # km022's MLM loss is the SimVLM core, so both must agree exactly
  base <- morie_kamath_mlm_loss(c(0.5, 0.25, 0.5), c(0, 2))
  expect_equal(morie_kamath_ch9_simvlm_mlm(NULL, c(0.5, 0.25, 0.5),
                                           matrix(c(1, 2), 1),
                                           c(0, 2))$estimate,
               base$estimate, tolerance = TOL)
  # the Alammar BT core drives both the DPO loss and Christiano's fit
  bt <- morie_alammar_reward_model_bt(c(0.25), c(-0.25))
  expect_equal(morie_kamath_dpo_loss(-0.5, -1.5, -1, -1, 0.5)$estimate,
               bt$estimate, tolerance = TOL)
  # the attention core is shared by ALiBi, prefix tuning and the Q-Former
  a <- morie_alammar_sdp_attention(matrix(c(1, 0), 1), diag(2), diag(2))
  expect_equal(as.numeric(morie_kamath_q_former(matrix(c(1, 0), 1),
                                                diag(2))$Z),
               as.numeric(a$output), tolerance = TOL)
})

test_that("km110 RRF and kmfait faithfulness match Python (lead ports)", {
  o <- morie_kamath_rrf_score(c(1, 2, 7))
  expect_equal(o$estimate, 0.0474478480153437, tolerance = 1e-15)
  expect_equal(o$scores[1], 1 / 61, tolerance = 1e-15)
  expect_error(morie_kamath_rrf_score(numeric(0)), "undefined")
  expect_error(morie_kamath_rrf_score(0), "1-based")
  f <- morie_kamath_ragas_faithfulness(
    c("the sky is blue", "the sky is green", "grass grows"),
    "the sky is blue today and grass grows")
  expect_equal(f$estimate, 2 / 3, tolerance = 1e-15)
  expect_equal(f$supported, 2L)
  expect_equal(f$judge, "lexical containment")
  judge <- function(cl, ctx) TRUE
  expect_equal(morie_kamath_ragas_faithfulness("a claim. another.",
                                               "x", entails = judge)$estimate,
               1)
  expect_error(morie_kamath_ragas_faithfulness("", "ctx"), "no claims")
})
