# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Anchors for the wave-4c parity tests (57 hm* modules, rw4_c.txt).
# Generated from the canonical Python via gen_anchors_w4c.py, run from the
# morie repo root:
#   cd /Volumes/VSR/rootcoderfiles/morie
#   MORIE_NO_UPDATE_CHECK=1 PYTHONPATH=src python \
#     stage_rw4c/gen_anchors_w4c.py > stage_rw4c/anchors_w4c.json
# The generator asserts every module's __file__ contains /src/morie/fn/
# before recording anything, so these numbers are provably drawn from the
# canonical Python, not a stray copy. Values below are the literal JSON
# contents transcribed to R (jsonlite not required at test time).

A4c <- list(
  hmmxp2 = list(n_underflow = 1L, overflow = FALSE, max_safe_loss_scale = 65504.0,
                recommended_loss_scale = 32768.0, memory_bytes_fp32 = 8L, memory_bytes_fp16 = 4L),
  hmncsn = list(analytic_W = -0.333333, loss_decreases = TRUE),
  hmnmd = list(derivative = 12.0, n_evals = 4L),
  hmnmf = list(relative_error_small = TRUE, nonneg = TRUE),
  hmnmt = list(token_losses = c(0.693147, 0.693147), loss = 1.386294, greedy = c(1L, 1L), exact_match = TRUE),
  hmnov = list(is_novel = c(FALSE, TRUE), novel_fraction = 0.5),
  hmnsp = list(tokens = c("[CLS]", "the", "cat", "sat", "[SEP]", "the", "cat", "sat", "[SEP]"),
               segment_ids = c(0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L), logit = 2.0, probability = 0.880797),
  hmocsv = list(argmin_decision = 5L, outlier_fraction_le_half = TRUE),
  hmonl = list(losses = c(1.0, 0.0), theta0 = 1.0),
  hmonnx = list(input_shape = c(1L, 3L), output_shape = c(1L, 2L), n_nodes = 2L, n_parameters = 8L),
  hmoob = list(oob_score = 1.0, mean_oob_votes = 1.0),
  hmopt = list(n_clusters = 2L, labels_set = c(0L, 1L), core0 = 0.1),
  hmosf = list(prediction = "greeting", prompt_text = "hello -> greeting\ngoodbye ->"),
  hmovo = list(n_classifiers = 3L, accuracy = 1.0, predict_new = c(0L, 2L)),
  hmovr = list(n_classifiers = 3L, accuracy = 1.0, positive_rate = c(0.333333, 0.333333, 0.333333)),
  hmpas = list(predict = c(2.0, 2.0), train_mse = 1.0),
  hmpcac = list(ratio = c(1.0, 0.0), comp0 = c(1.0, 0.0)),
  hmpcav = list(ratio = c(1.0, 0.0), top_variance = 4.0, n_components_for_threshold = 1L),
  hmpd = list(shape = c(3L, 3L), sum = 1.0, center = 1.0),
  hmpemb = list(emb0 = c(1.0, 0.0), coverage = 0.5, oov = "zzz", dim = 2L),
  hmper = list(probabilities = c(0.25, 0.25, 0.5), weights = c(1.0, 1.0, 0.5)),
  hmpg = list(gradient = c(2.0, 0.0)),
  hmphp = list(g0 = 0.761594, c0 = 0.380797, h0 = 0.1817),
  hmplf = list(features = c(1.0, 2.0, 3.0, 4.0, 6.0, 9.0),
               names = c("1", "x0", "x1", "x0^2", "x0 x1", "x1^2"), n_out = 6L),
  hmpmps = list(source_dtype = "float64", dtype_on_device = "float32", downcast = TRUE),
  hmpol = list(probabilities = c(0.25, 0.75), greedy_action = 1L, entropy = 0.562335),
  hmppo = list(prob1_gt_09 = TRUE, return_monotone = TRUE),
  hmppp = list(bubble_fraction = 0.428571, n_slots = 7L,
               schedule = matrix(c(0, 1, 2, 3, -1, -1, -1,
                                    -1, 0, 1, 2, 3, -1, -1,
                                    -1, -1, 0, 1, 2, 3, -1,
                                    -1, -1, -1, 0, 1, 2, 3), nrow = 4, byrow = TRUE)),
  hmprc = list(average_precision = 1.0, ap_reversed = 0.5, best_f1_reversed = 0.666667),
  hmprcv = list(attn_shape = c(1L, 2L), attn_sum = 1.0, cost = 2L, self_cost = 4L),
  hmpre = list(precision = 0.666667, tp = 2L, fp = 1L, fn = 1L),
  hmprel = list(a = c(-0.5, 3.0), grad_z = c(0.25, 1.0), grad_alpha = -2.0),
  hmprio = list(outputs_shape = c(3L, 2L), decoder_attn_shape = c(3L, 2L), attn_row_sums = c(1.0, 1.0, 1.0)),
  hmpru = list(pruned = c(0.0, 0.0, 3.0, -4.0), threshold = 2.0, achieved_sparsity = 0.5, n_pruned = 2L),
  hmptq = list(activation_scale = 0.007843137, zero_point = 128L, weight_scale = 0.007874016,
               quantized_weights = c(-127L, 64L, 127L)),
  hmpttn = list(dtype = "float32", shape = c(2L, 2L), nbytes = 16L, dtype_changed = TRUE),
  hmpvt = list(output_shape = c(2L, 2L, 1L), token00 = 8.5),
  hmqat = list(q_weight_near2 = TRUE, loss_small = TRUE),
  hmrad = list(value = 12.0, gradient = c(4.0, 3.0)),
  hmrdt = list(threshold = 2.5, n_leaves = 2L, predictions = c(1.0, 1.0, 5.0, 5.0), mse = 0.0),
  hmrec = list(tp = 1L, fn = 1L, recall = 0.5),
  hmregn = list(mse_small = TRUE, loss_decreases = TRUE, n_parameters = 25L),
  hmrelu = list(a = c(0.0, 0.0, 3.0), gradient = c(0.0, 0.0, 1.0), dead_fraction = 0.666667),
  hmrfc = list(accuracy = 1.0, predict = c(0L, 1L), max_features = 2L),
  hmrgpt = list(layers = c("Linear(in_features=1, out_features=8)", "ReLU()",
                           "Linear(in_features=8, out_features=1)"), n_parameters = 25L, mse_small = TRUE),
  hmrl = list(mean_return = 1.75, lengths = 3L),
  hmrlhf = list(optimal_policy = c(0.268941, 0.731059), max_deviation_small = TRUE),
  hmrnfc = list(step = c(0.025, -0.025), theta = c(0.025, -0.025)),
  hmrnn = list(h0 = 0.761594),
  hmroc = list(auc = 0.75, auc_trapezoid = 0.75, tpr = c(0.0, 0.5, 0.5, 1.0, 1.0), fpr = c(0.0, 0.0, 0.5, 0.5, 1.0)),
  hmrpca = list(sv0 = 2.0, var0 = 2.0, ratio0 = 1.0),
  hmrpt = list(predict = c(2.0, 2.0, 2.0, 2.0), train_mse = 1.0),
  hmrsc = list(best_alpha = 0.0, best_score = 1.0),
  hmrsp = list(predict = c(2.0, 2.0), feature_set_sizes = 2L),
  hmrvat = list(alpha_sum = 1.0, alpha0_gt_alpha1 = TRUE),
  hmrvn = list(y = c(7.0, 10.0, 11.0, 15.0), reconstruction_error = 0.0),
  hmrwd = list(rewards = c(1.0, 0.0, 2.0), discounted_return = 1.5, returns = c(1.5, 1.0, 2.0))
)
