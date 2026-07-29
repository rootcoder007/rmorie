# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Cross-language parity for the Geron shelf (geron_ml_native.R) against
# the 80 morie.fn modules hm*/gr*.
#
# EVERY numeric anchor below was produced by RUNNING the Python module
# and pasting the printed repr -- none is recalled or hand-derived from
# the docstring. The generator is anchors.py in this directory:
#   cd /path/to/morie && MORIE_NO_UPDATE_CHECK=1 PYTHONPATH=src \
#     python anchors.py
#
# Independent-route checks (a second derivation, not a second call to
# the same code) are marked "independent route:" and cover the
# hand-derived optimiser first steps, Hungarian vs brute force, AUC vs
# pair counting, bf16 round-trip cases, schedule closed forms and the
# LCG-driven modules reproducing Python token for token.

tol <- 1e-9

# ------------------------------------------------------------- optimisers

test_that("Adam and AdamW match Python and their closed forms", {
  r <- morie_geron_adam(c(1, -2), m = c(0.1, 0), v = c(0.04, 0), eta = 0.1, t = 3)
  expect_equal(r$m, c(0.19, -0.19999999999999996), tolerance = tol)
  expect_equal(r$v, c(0.04096, 0.0040000000000000036), tolerance = tol)
  expect_equal(r$m_hat, c(0.701107011070111, -0.7380073800738008), tolerance = tol)
  expect_equal(r$v_hat, c(13.666995773441522, 1.3346675560001497), tolerance = tol)
  expect_equal(r$step, c(-0.01896477868084416, 0.06388135938689934), tolerance = tol)

  # independent route: hand-derived first step. From zeroed moments at
  # t = 1, m_hat = g and v_hat = g^2, so the step is exactly -eta*sign(g)
  # up to eps -- the bias correction is what makes it eta, not eta/100.
  r1 <- morie_geron_adam(2, eta = 0.5, t = 1, eps = 0)
  expect_equal(r1$step, -0.5, tolerance = tol)
  r1n <- morie_geron_adam(-2, eta = 0.5, t = 1, eps = 0)
  expect_equal(r1n$step, 0.5, tolerance = tol)

  ru <- morie_geron_adam_update(1, 0.1, 0, 0, t = 1, eta = 0.001)
  expect_equal(ru$theta_new, 0.9990000001, tolerance = tol)
  expect_equal(ru$m_hat, 0.1, tolerance = tol)
  expect_equal(ru$s_hat, 0.010000000000000002, tolerance = tol)
  expect_equal(ru$step, 0.00099999990000001, tolerance = tol)

  rw <- morie_geron_adamw(c(1, 2), theta = c(2, -1), eta = 0.1, wd = 0.5, t = 2)
  expect_equal(rw$theta, c(1.825586318695407, -1.0244136818306455), tolerance = tol)
  expect_equal(rw$adam_step, c(-0.07441368130459296, -0.07441368183064559),
               tolerance = tol)
  # independent route: decoupled decay is exactly -eta*wd*theta, NOT
  # divided by sqrt(v_hat).
  expect_equal(rw$decay_step, -0.1 * 0.5 * c(2, -1), tolerance = tol)
  expect_equal(rw$decay_step, c(-0.1, 0.05), tolerance = tol)
})

test_that("AdaMax and AdaGrad match Python", {
  r <- morie_geron_adamax(0.5, m = 0.1, u = 1, b1 = 0.9, b2 = 0.999,
                          eta = 0.1, t = 2)
  expect_equal(r$u, 0.999, tolerance = tol)
  expect_equal(r$step, -0.07375796849481063, tolerance = tol)
  expect_equal(r$m_hat, 0.7368421052631582, tolerance = tol)
  # independent route: u is an exponentially weighted max, so with
  # |g| < b2*u it is b2*u exactly.
  expect_equal(r$u, 0.999 * 1, tolerance = tol)

  a <- morie_geron_adagrad(2, s = 4, eta = 0.1, eps = 0)
  expect_equal(a$s, 8, tolerance = tol)
  expect_equal(a$step, -0.07071067811865475, tolerance = tol)
  expect_equal(a$effective_lr, 0.035355339059327376, tolerance = tol)
  # independent route: step = -eta*g/sqrt(s+g^2) = -0.1*2/sqrt(8).
  expect_equal(a$step, -0.1 * 2 / sqrt(8), tolerance = tol)

  u <- morie_geron_adagrad_update(c(1, 2), c(2, -1), c(0, 3), 0.1, eps = 1e-10)
  expect_equal(u$theta_new, c(0.900000000005, 2.0499999999975), tolerance = tol)
  expect_equal(u$s_new, c(4, 4), tolerance = tol)
  expect_equal(u$step, c(0.099999999995, -0.0499999999975), tolerance = tol)
})

test_that("1cycle schedule matches Python and its closed form", {
  r <- morie_geron_1cycle_schedule(0.1, 0.5, t = 1, T = 7)
  expect_equal(r$lr_schedule,
               c(0.1, 0.23333333333333334, 0.3666666666666667, 0.5,
                 0.3666666666666667, 0.23333333333333334, 0.1), tolerance = tol)
  expect_equal(r$momentum_schedule,
               c(0.95, 0.9166666666666666, 0.8833333333333333, 0.85,
                 0.8833333333333333, 0.9166666666666666, 0.95), tolerance = tol)
  expect_equal(r$peak_step, 3L)
  # independent route: the closed form on the rising half is
  # eta_min + (eta_max - eta_min) * i / peak.
  peak <- 3
  expect_equal(r$lr_schedule[1:4], 0.1 + (0.5 - 0.1) * (0:3) / peak,
               tolerance = tol)
  expect_equal(r$momentum_schedule[1:4], 0.95 + (0.85 - 0.95) * (0:3) / peak,
               tolerance = tol)
  # T = 2 collapses the peak onto step 0 -- the two-knot branch.
  r2 <- morie_geron_1cycle_schedule(0.1, 0.5, t = 1, T = 2)
  expect_equal(r2$lr_schedule, c(0.1, 0.5), tolerance = tol)
  expect_equal(r2$momentum_schedule, c(0.95, 0.85), tolerance = tol)
  expect_equal(r2$peak_step, 0L)
})

test_that("batch-size heuristic matches Python", {
  expect_equal(morie_geron_batch_size_heuristic(1000)$batch_size, 64)
  expect_equal(morie_geron_batch_size_heuristic(1000)$steps_per_epoch, 16L)
  expect_equal(morie_geron_batch_size_heuristic(50)$batch_size, 32)
  expect_equal(morie_geron_batch_size_heuristic(10^6)$batch_size, 512)
  expect_equal(morie_geron_batch_size_heuristic(10^6, memory_limit = 100)$batch_size, 64)
})

# -------------------------------------------------------------- criteria

test_that("AIC / BIC families match Python", {
  r <- morie_geron_aic(c(-10, -9, -12), c(2, 4, 1), n = 20)
  expect_equal(r$aic, c(24, 26, 26), tolerance = tol)
  expect_equal(r$aicc,
               c(24.705882352941178, 28.666666666666668, 26.22222222222222),
               tolerance = tol)
  expect_equal(r$delta, c(0, 2, 2), tolerance = tol)
  expect_equal(r$weights,
               c(0.5761168847658291, 0.21194155761708544, 0.21194155761708544),
               tolerance = tol)
  expect_equal(r$best_index, 0L)   # 0-based, as Python returns it

  b <- morie_geron_bic(c(-10, -8), c(2, 6), 100)
  expect_equal(b$bic, c(29.210340371976184, 43.63102111592855), tolerance = tol)
  expect_equal(b$weights, c(0.9992616399684764, 0.0007383600315236521),
               tolerance = tol)
  expect_equal(b$best_index, 0L)
  # independent route: BIC = -2 logL + k log n by hand.
  expect_equal(b$bic, -2 * c(-10, -8) + c(2, 6) * log(100), tolerance = tol)

  expect_equal(c(morie_geron_gmm_n_params(3, 2, "full"),
                 morie_geron_gmm_n_params(3, 2, "diag"),
                 morie_geron_gmm_n_params(3, 2, "spherical"),
                 morie_geron_gmm_n_params(4, 5, "full")),
               c(17L, 14L, 11L, 83L))
  expect_equal(morie_geron_aic_gmm(-100, 5)$aic, 210, tolerance = tol)
  g <- morie_geron_bic_gmm(-100, 100, 5)
  expect_equal(g$bic, 223.02585092994047, tolerance = tol)
  expect_equal(g$penalty_per_param, 4.605170185988092, tolerance = tol)
  expect_true(g$stricter_than_aic)
})

test_that("both AUC routes agree with Python and with pair counting", {
  yt <- c(0, 0, 1, 1, 0, 1); sc <- c(0.1, 0.4, 0.35, 0.8, 0.4, 0.4)
  m <- morie_geron_auc_roc(yt, sc)
  expect_equal(m$auc, 0.6666666666666666, tolerance = tol)
  expect_equal(m$fpr, c(0, 0, 0.6666666666666666, 0.6666666666666666, 1),
               tolerance = tol)
  expect_equal(m$tpr, c(0, 0.3333333333333333, 0.6666666666666666, 1, 1),
               tolerance = tol)
  expect_equal(m$thresholds[-1], c(0.8, 0.4, 0.35, 0.1), tolerance = tol)
  expect_equal(m$n_pos, 3L); expect_equal(m$n_neg, 3L)

  t_ <- morie_geron_auc_roc_trapezoid(yt, sc)
  expect_equal(t_$auc, 0.6666666666666667, tolerance = tol)
  expect_equal(t_$fpr, m$fpr, tolerance = tol)
  expect_equal(t_$tpr, m$tpr, tolerance = tol)

  # independent route: brute-force pair counting, ties credited 0.5.
  pos <- sc[yt == 1]; neg <- sc[yt == 0]
  wins <- 0
  for (p in pos) for (q in neg) wins <- wins + (p > q) + 0.5 * (p == q)
  expect_equal(m$auc, wins / (length(pos) * length(neg)), tolerance = tol)
  expect_equal(t_$auc, wins / (length(pos) * length(neg)), tolerance = tol)
})

test_that("confusion matrix, CART cost and binary classification match Python", {
  r <- morie_geron_confusion_matrix(c(0, 0, 1, 1, 2, 2, 0), c(0, 1, 1, 1, 2, 0, 2))
  expect_equal(unname(r$matrix),
               matrix(c(1L, 1L, 1L, 0L, 2L, 0L, 1L, 0L, 1L), 3, 3, byrow = TRUE))
  expect_equal(r$accuracy, 0.5714285714285714, tolerance = tol)
  expect_equal(r$precision, c(0.5, 0.6666666666666666, 0.5), tolerance = tol)
  expect_equal(r$recall, c(0.3333333333333333, 1, 0.5), tolerance = tol)
  expect_equal(r$f1, c(0.4, 0.8, 0.5), tolerance = tol)
  expect_equal(r$macro_f1, 0.5666666666666668, tolerance = tol)
  expect_equal(r$support, c(3L, 2L, 2L))

  X <- matrix(c(1, 2, 3, 4), ncol = 1)
  g <- morie_geron_cart_split_cost(X, c(0, 0, 1, 1), 0, 1.5)
  expect_equal(g$cost, 0.3333333333333333, tolerance = tol)
  expect_equal(g$impurity_left, 0, tolerance = tol)
  expect_equal(g$impurity_right, 0.4444444444444444, tolerance = tol)
  expect_equal(g$impurity_parent, 0.5, tolerance = tol)
  expect_equal(g$impurity_decrease, 0.16666666666666669, tolerance = tol)
  e <- morie_geron_cart_split_cost(X, c(0, 0, 1, 1), 0, 1.5, criterion = "entropy")
  expect_equal(e$cost, 0.6887218755408672, tolerance = tol)
  expect_equal(e$impurity_parent, 1, tolerance = tol)
  s <- morie_geron_cart_split_cost(X, c(1, 2, 8, 9), 0, 2.5, criterion = "mse")
  expect_equal(s$cost, 0.25, tolerance = tol)
  expect_equal(s$impurity_parent, 12.5, tolerance = tol)

  b <- morie_geron_binary_classification(
    matrix(c(1, 1, 1, 2, -3, 0), ncol = 2), c(0.5, 0.5), y_true = c(1, 1, 0))
  expect_equal(b$p_hat,
               c(0.8175744761936437, 0.2689414213699951, 0.6224593312018546),
               tolerance = tol)
  expect_equal(b$y_pred, c(1L, 0L, 1L))
  expect_equal(b$accuracy, 0.3333333333333333, tolerance = tol)
  expect_equal(b$precision, 0.5, tolerance = tol)
  expect_equal(b$recall, 0.5, tolerance = tol)
  expect_equal(b$f1, 0.5, tolerance = tol)
})

test_that("bias-variance, batch learning and gradient descent match Python", {
  bv <- morie_geron_bias_variance_tradeoff(
    matrix(c(1, 2, 3, 4, 2, 1), 3, 2, byrow = TRUE), c(2, 2))
  expect_equal(bv$bias2, 0.05555555555555561, tolerance = tol)
  expect_equal(bv$variance, 1.111111111111111, tolerance = tol)
  expect_equal(bv$noise, 2.220446049250313e-16, tolerance = 1e-12)
  expect_equal(bv$mse, 1.1666666666666667, tolerance = tol)
  expect_equal(bv$mean_pred, c(2, 2.3333333333333335), tolerance = tol)
  # independent route: the decomposition must close exactly.
  expect_equal(bv$bias2 + bv$variance + bv$noise, bv$mse, tolerance = 1e-12)
  # independent route: the ddof=0 (population) variance is what closes it;
  # the n-1 form would be 1.5x too big here (3 models).
  expect_false(isTRUE(all.equal(bv$variance, 1.111111111111111 * 3 / 2)))

  bl <- morie_geron_batch_learning(
    matrix(c(1, 0, 1, 1, 1, 2), 3, 2, byrow = TRUE), c(1, 3, 5.2), ridge = 0.5)
  expect_equal(bl$theta, c(1.0146341463414628, 1.882926829268293), tolerance = tol)
  expect_equal(bl$train_mse, 0.06223279793773558, tolerance = tol)
  expect_equal(bl$r2, 0.9788483692809811, tolerance = tol)

  gg <- morie_geron_batch_gd_grad(matrix(c(1, 1, 1, 2), 2, 2, byrow = TRUE),
                                  c(1, 2), c(0, 0), eta = 0.1)
  expect_equal(gg$gradient, c(-3, -5), tolerance = tol)
  expect_equal(gg$cost, 2.5, tolerance = tol)
  expect_equal(gg$theta_next, c(0.30000000000000004, 0.5), tolerance = tol)

  bg <- morie_geron_batch_gradient_descent(matrix(c(1, 2), ncol = 1),
                                           c(2, 4), 0, eta = 0.1, n_iter = 3)
  expect_equal(bg$theta, 1.75, tolerance = tol)
  expect_equal(bg$loss_history, c(10, 2.5, 0.625, 0.15625), tolerance = tol)
  expect_equal(bg$eta_max_stable, 0.4, tolerance = tol)
  # independent route: the stability bound is 2 / lambda_max of (2/m) X^T X
  # = 2 / ((2/2) * 5) = 0.4.
  expect_equal(bg$eta_max_stable, 2 / 5, tolerance = tol)
})

test_that("convolution arithmetic matches Python", {
  r <- morie_geron_conv_output_size(c(32, 28, 28), c(5, 3, 3),
                                    padding = c(2, 0, 1), stride = c(1, 2, 1))
  expect_equal(r$out_size, c(32L, 13L, 28L))
  expect_equal(r$receptive_field, c(5L, 3L, 3L))
  expect_equal(r$same_padding, c(2L, 1L, 1L))
  expect_equal(r$dropped_cells, c(0L, 1L, 0L))
  expect_false(r$is_same)

  X <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3, byrow = TRUE)
  cv <- morie_geron_conv2d_forward(X, matrix(c(1, 0, 0, 1), 2, 2, byrow = TRUE),
                                   b = 0.5, stride = 1, padding = 1)
  expect_equal(cv$Y, matrix(c(1.5, 2.5, 3.5, 0.5,
                              4.5, 6.5, 8.5, 3.5,
                              7.5, 12.5, 14.5, 6.5,
                              0.5, 7.5, 8.5, 9.5), 4, 4, byrow = TRUE),
               tolerance = tol)
  expect_equal(cv$out_shape, c(4L, 4L))
  expect_equal(cv$n_multiply_adds, 64L)
})

# -------------------------------------------- attention / RNN / networks

test_that("Bahdanau attention serves both hmbdn and grbah", {
  I2 <- diag(2)
  r <- morie_geron_bahdanau_attention(matrix(c(1, 0, 0, 1), 2, 2, byrow = TRUE),
                                      c(0.5, -0.5), I2, I2, c(1, 0.5),
                                      b = c(0.1, -0.2))
  expect_equal(r$scores, c(0.6194846658478896, 0.6827058732238307), tolerance = tol)
  expect_equal(r$alpha, c(0.4841999604313876, 0.5158000395686125), tolerance = tol)
  expect_equal(r$context, c(0.4841999604313876, 0.5158000395686125), tolerance = tol)
  expect_equal(r$entropy, 0.6926478149316595, tolerance = tol)
  expect_equal(r$argmax, 1L)   # 0-based

  # grbah's argument order maps on as h = encoder_states, s_prev =
  # decoder_state, W = Ws, U = Wh.
  g <- morie_geron_bahdanau_attention(matrix(c(1, 3), ncol = 1), 1,
                                      W = matrix(2), U = matrix(1), v = 1)
  expect_equal(g$weights, c(0.4987641067026967, 0.5012358932973033), tolerance = tol)
  expect_equal(g$context, 2.0024717865946067, tolerance = tol)
  expect_equal(g$scores, c(0.9950547536867305, 0.9999983369439447), tolerance = tol)
  expect_equal(g$argmax, 1L)
  expect_equal(sum(g$weights), 1, tolerance = tol)
})

test_that("cross-attention matches Python", {
  r <- morie_geron_cross_attention(
    matrix(c(1, 2, 0, 1), 2, 2, byrow = TRUE),
    matrix(c(1, 0, 0, 2, 1, 1), 3, 2, byrow = TRUE),
    WQ = diag(2), WK = matrix(c(0.5, 0, 0, 1), 2, 2, byrow = TRUE),
    WV = matrix(c(1, 2), ncol = 1))
  expect_equal(as.numeric(r$output), c(3.581412408347212, 3.295916855128606),
               tolerance = tol)
  expect_equal(r$attention_weights[1, ],
               c(0.058846177114942014, 0.6991047625770956, 0.2420490603079624),
               tolerance = tol)
  expect_equal(r$attention_weights[2, ],
               c(0.14002924504337802, 0.575975345215362, 0.28399540974126003),
               tolerance = tol)
  expect_equal(r$scale, 0.7071067811865475, tolerance = tol)
})

test_that("bidirectional RNN and its combiner match Python", {
  r <- morie_geron_bidirectional_rnn(matrix(c(1, 1, 0.5), ncol = 1),
                                     matrix(1), matrix(0.5), matrix(1),
                                     matrix(-0.5))
  expect_equal(as.numeric(r$h_fwd),
               c(0.7615941559557649, 0.8811296283442258, 0.7354816480474754),
               tolerance = tol)
  expect_equal(as.numeric(r$h_bwd),
               c(0.5894633498796511, 0.6463134841204403, 0.46211715726000974),
               tolerance = tol)
  expect_equal(r$final, c(0.7354816480474754, 0.5894633498796511), tolerance = tol)

  c_ <- morie_geron_bidirectional_combine(matrix(c(1, 2), ncol = 1),
                                          matrix(c(9, 8), ncol = 1),
                                          backward_in_reverse_order = TRUE)
  expect_equal(c_$h, matrix(c(1, 8, 2, 9), 2, 2, byrow = TRUE), tolerance = tol)
  expect_equal(c_$output_dim, 2L)
})

test_that("McCulloch-Pitts neuron and batch norms match Python", {
  n <- morie_geron_biological_neuron(matrix(c(1, 2, 0, -1), 2, 2, byrow = TRUE),
                                     c(0.5, -1), 0.25, activation = "sigmoid")
  expect_equal(n$z, c(-1.25, 1.25), tolerance = tol)
  expect_equal(n$a, c(0.22270013882530884, 0.7772998611746911), tolerance = tol)

  b <- morie_geron_batch_normalization(matrix(c(1, 4, 3, 0, 2, 2), 3, 2, byrow = TRUE),
                                       gamma = 2, beta = 5, eps = 1e-5)
  expect_equal(b$mu, c(2, 2), tolerance = tol)
  expect_equal(b$var, c(0.6666666666666666, 2.6666666666666665), tolerance = tol)
  expect_equal(as.numeric(t(b$x_hat)),
               c(-1.2247356859083902, 1.2247425750014138,
                 1.2247356859083902, -1.2247425750014138, 0, 0), tolerance = tol)
  expect_equal(as.numeric(t(b$y)),
               c(2.5505286281832196, 7.449485150002827,
                 7.449471371816781, 2.5505148499971724, 5, 5), tolerance = tol)
  expect_equal(b$running_mean, c(2, 2), tolerance = tol)
  # independent route: hmbntr's INFERENCE variance is the unbiased (n-1)
  # form even though the normalisation used the population form.
  expect_equal(b$running_var, c(1, 4), tolerance = tol)
  expect_equal(b$running_var, b$var * 3 / 2, tolerance = tol)

  p <- morie_geron_batch_normalization_paper(matrix(c(0, 1, 2, 5), 2, 2, byrow = TRUE),
                                             gamma = c(3, 1), beta = c(5, 0),
                                             eps = 0, momentum = 0.9)
  expect_equal(as.numeric(t(p$Y)), c(2, -1, 8, 1), tolerance = tol)
  expect_equal(as.numeric(t(p$x_hat)), c(-1, -1, 1, 1), tolerance = tol)
  expect_equal(p$batch_mean, c(1, 3), tolerance = tol)
  expect_equal(p$batch_var, c(1, 4), tolerance = tol)
  expect_equal(p$running_mean, c(0.09999999999999998, 0.29999999999999993),
               tolerance = tol)
  expect_equal(p$running_var, c(1, 1.2999999999999998), tolerance = tol)
})

test_that("backpropagation routes match Python", {
  W1 <- matrix(c(1, -1), 1, 2); W2 <- matrix(c(2, 3), 2, 1)
  r <- morie_geron_backpropagation(matrix(c(1, 2), ncol = 1),
                                   matrix(c(0, 1), ncol = 1),
                                   list(W1, W2), c("relu", "identity"))
  expect_equal(r$loss, 6.5, tolerance = tol)
  expect_equal(as.numeric(r$output), c(2, 4), tolerance = tol)
  expect_equal(as.numeric(r$grads_W[[1]]), c(16, 0), tolerance = tol)
  expect_equal(as.numeric(r$grads_W[[2]]), c(8, 0), tolerance = tol)
  expect_equal(r$grads_b[[1]], c(10, 0), tolerance = tol)

  ce <- morie_geron_backpropagation(diag(2), c(0, 1),
                                    list(matrix(c(1, 0.5, 0.5, 1), 2, 2, byrow = TRUE)),
                                    c("softmax"), loss = "ce")
  expect_equal(ce$loss, 0.47407698418010663, tolerance = tol)
  expect_equal(as.numeric(t(ce$output)),
               c(0.6224593312018546, 0.37754066879814546,
                 0.37754066879814546, 0.6224593312018546), tolerance = tol)
  expect_equal(as.numeric(t(ce$grads_W[[1]])),
               c(-0.1887703343990727, 0.18877033439907273,
                 0.18877033439907273, -0.1887703343990727), tolerance = tol)

  gr <- morie_geron_backpropagation_gradient(
    list(matrix(c(1, 2), 1, 2), matrix(3, 1, 1)),
    list(matrix(c(1, 1), 2, 1)), matrix(1, 1, 1), activation = "sigmoid")
  expect_equal(as.numeric(gr$grad_weights[[1]]), c(-12, -24), tolerance = tol)
  expect_equal(as.numeric(gr$deltas[[1]]), -12, tolerance = tol)
  expect_equal(gr$loss, 2, tolerance = tol)
  expect_equal(gr$grad_norm, 26.832815729997478, tolerance = tol)
  # independent route: err = a - y = 2, damped by a(1-a) = 3*(1-3) = -6,
  # so delta = -12 and dL/dW = a0^T delta = [-12, -24].
  expect_equal(as.numeric(gr$deltas[[1]]), 2 * (3 * (1 - 3)), tolerance = tol)
})

test_that("BPTT and the Jacobian chain match Python", {
  r <- morie_geron_backprop_through_time(
    matrix(c(1, 0.5, 0.5, 1), 2, 2, byrow = TRUE),
    matrix(c(0.2, -0.1, 0.4, 0.3), 2, 2, byrow = TRUE),
    matrix(c(1, 0, 0, 2), 2, 2, byrow = TRUE),
    W_h = matrix(c(0.5, 0.1, -0.2, 0.3), 2, 2, byrow = TRUE),
    h_init = c(0.1, 0.2))
  expect_equal(as.numeric(t(r$grad_Wx)),
               c(1.2489599999999998, 0.6821100000000001, 0.84, 1.82),
               tolerance = tol)
  expect_equal(as.numeric(t(r$grad_Wh)),
               c(0.208896, 0.250211, 0.20779199999999998, 0.045422000000000004),
               tolerance = tol)
  expect_equal(r$grad_b, c(1.6689599999999998, 1.5921100000000001), tolerance = tol)
  expect_equal(as.numeric(t(r$deltas)),
               c(1.2489599999999998, 0.6821100000000001, 0.42, 0.91),
               tolerance = tol)
  expect_equal(r$per_step_delta_norm,
               c(1.4230864814550097, 1.0022474744293446), tolerance = tol)
  expect_equal(r$vanishing_ratio, 1.4198953030690156, tolerance = tol)
  # independent route: the LAST delta has no recurrent term, so it is
  # exactly dL/dh_T * (1 - h_T^2).
  expect_equal(as.numeric(r$deltas[2, ]),
               c(0.5, 1) * (1 - c(0.4, 0.3)^2), tolerance = tol)

  ch <- morie_geron_autograd_chain_rule(
    list(matrix(c(2, 0, 0, 3), 2, 2, byrow = TRUE), matrix(c(1, 1), 1, 2)), 1)
  expect_equal(ch$grad_input, c(2, 3), tolerance = tol)
  expect_equal(ch$intermediate_grads[[1]], 1, tolerance = tol)
  expect_equal(ch$intermediate_grads[[2]], c(1, 1), tolerance = tol)
  expect_equal(ch$intermediate_grads[[3]], c(2, 3), tolerance = tol)
  expect_equal(ch$grad_norm, 3.605551275463989, tolerance = tol)
  # A callable node is the same vector-Jacobian product.
  ch2 <- morie_geron_autograd_chain_rule(
    list(matrix(c(2, 0, 0, 3), 2, 2, byrow = TRUE), function(g) c(g[1], g[1])), 1)
  expect_equal(ch2$grad_input, c(2, 3), tolerance = tol)
})

# --------------------------------------------- contrastive and multimodal

test_that("CLIP and InfoNCE match Python", {
  emb <- matrix(c(1, 0, 0, 1, 1, 1), 3, 2, byrow = TRUE)
  r <- morie_geron_clip_contrastive_loss(emb, emb, tau = 0.5)
  expect_equal(r$loss, 0.6000313142487683, tolerance = tol)
  expect_equal(r$loss_i2t, 0.6000313142487683, tolerance = tol)
  expect_equal(r$loss_t2i, 0.6000313142487683, tolerance = tol)
  expect_equal(r$similarity[1, ], c(1, 0, 0.7071067811865475), tolerance = tol)
  expect_equal(r$accuracy_i2t, 1, tolerance = tol)
  expect_equal(r$chance_loss, 1.0986122886681098, tolerance = tol)

  n <- morie_geron_contrastive_infonce(
    matrix(c(1, 0, 0, 1), 2, 2, byrow = TRUE),
    matrix(c(1, 0.2, 0.1, 1), 2, 2, byrow = TRUE),
    matrix(c(0, 1, 1, 1), 2, 2, byrow = TRUE), tau = 0.5)
  expect_equal(n$loss, 0.7433700923253208, tolerance = tol)
  expect_equal(n$per_anchor_loss, c(0.5419802798303048, 0.9447599048203368),
               tolerance = tol)
  expect_equal(n$pos_sim, c(0.9805806756909201, 0.9950371902099893), tolerance = tol)
  expect_equal(as.numeric(t(n$neg_sim)),
               c(0, 0.7071067811865475, 1, 0.7071067811865475), tolerance = tol)
  expect_equal(n$hardest_negative, c(0.7071067811865475, 1), tolerance = tol)
  expect_equal(n$accuracy, 0.5, tolerance = tol)
})

test_that("BLIP objectives and the Q-Former match Python", {
  r <- morie_geron_blip(matrix(c(1, 0, 0, 1, 1, 1), 3, 2, byrow = TRUE),
                        matrix(c(1, 0.1, 0.2, 1, 1, 0.9), 3, 2, byrow = TRUE),
                        temperature = 0.7, caption_logprobs = c(-0.5, -1, -0.25))
  expect_equal(r$itc_loss, 0.7624376272632314, tolerance = tol)
  expect_equal(r$itc_i2t, 0.7594422336220003, tolerance = tol)
  expect_equal(r$itc_t2i, 0.7654330209044625, tolerance = tol)
  expect_equal(r$itm_loss, 0.6945505471974862, tolerance = tol)
  expect_equal(r$lm_loss, 0.5833333333333334, tolerance = tol)
  expect_equal(r$total_loss, 2.040321507794051, tolerance = tol)
  expect_equal(r$retrieval_acc, 1, tolerance = tol)
  expect_equal(r$similarity[1, ],
               c(0.9950371902099893, 0.19611613513818402, 0.7432941462471663),
               tolerance = tol)

  cl <- array(0, dim = c(2, 2, 2))
  cl[1, , ] <- matrix(c(0, 1, 2, 0), 2, 2, byrow = TRUE)
  cl[2, , ] <- matrix(c(1, 1, 0, 3), 2, 2, byrow = TRUE)
  b <- morie_geron_blip_itm_itc(matrix(c(1, 0, 0, 1), 2, 2, byrow = TRUE),
                                matrix(c(1, 0.2, 0.1, 1), 2, 2, byrow = TRUE),
                                cl, matrix(c(0, 1, 1, -1), 2, 2, byrow = TRUE),
                                tau = 0.5)
  expect_equal(b$loss, 2.040742615766768, tolerance = tol)
  expect_equal(b$itc, 0.17153382342318818, tolerance = tol)
  expect_equal(b$itm, 0.4914298326365328, tolerance = tol)
  expect_equal(b$lm, 1.3777789597070471, tolerance = tol)
  expect_equal(b$itm_accuracy, 0.5, tolerance = tol)
  expect_equal(b$lm_perplexity, 3.966083007699707, tolerance = tol)

  # LCG-driven: the Q-Former weights come from the integer LCG, so R must
  # reproduce Python's numbers exactly, not merely in distribution.
  q <- morie_geron_blip2(matrix(c(1, 0, 0, 1, 1, 1), 3, 2, byrow = TRUE),
                         c(1, 0), n_query = 3, d_query = 4, d_llm = 2, seed = 5)
  expect_equal(as.numeric(t(q$query_output)),
               c(0.0024767130896208665, -0.0005379306994436539,
                 -0.0028512536928873462, -0.0014359265478811195,
                 0.0024649039914036515, -0.0005535848958363675,
                 -0.002845235460360196, -0.0014321687613870903,
                 0.0024590768863115963, -0.0005597690324921189,
                 -0.002841625252682352, -0.0014300533753952688), tolerance = tol)
  expect_equal(as.numeric(t(q$llm_input)),
               c(-5.061956808895602e-06, 4.503086017847096e-05,
                 -6.127208511049429e-06, 4.3355338913234276e-05,
                 -6.560748018771484e-06, 4.2652068219755146e-05), tolerance = tol)
  expect_equal(q$attention[1, ],
               c(0.3322122438287706, 0.3338125679179064, 0.3339751882533229),
               tolerance = tol)
  expect_equal(q$similarity, -0.14048456137560877, tolerance = tol)
  expect_equal(q$query_similarities,
               c(-0.14048456137560877, -0.1419916306550155, -0.14261019200692154),
               tolerance = tol)
  expect_equal(q$trainable_params, 60L)
  expect_equal(rowSums(q$attention), rep(1, 3), tolerance = tol)
})

test_that("DeiT, DALL-E and DETR match Python", {
  d <- morie_geron_deit_distillation_loss(
    matrix(c(1, 0, 0, 2), 2, 2, byrow = TRUE),
    matrix(c(0.5, 0.5, 1, 0), 2, 2, byrow = TRUE), c(0, 1),
    matrix(c(1, 0, 1, 0), 2, 2, byrow = TRUE), alpha = 0.3)
  expect_equal(d$loss, 0.30502772470814365, tolerance = tol)
  expect_equal(d$loss_cls, 0.22009484928059775, tolerance = tol)
  expect_equal(d$loss_dist, 0.5032044340390841, tolerance = tol)
  expect_equal(d$teacher_labels, c(0L, 0L))
  expect_equal(d$teacher_agreement, 0.5, tolerance = tol)
  expect_equal(d$accuracy_cls, 1, tolerance = tol)
  expect_equal(d$accuracy_dist, 1, tolerance = tol)
  # independent route: the mixture is exactly (1-a) CE_cls + a CE_dist.
  expect_equal(d$loss, 0.7 * d$loss_cls + 0.3 * d$loss_dist, tolerance = tol)

  skew <- function(ctx) c(0, 1, 2) + 0.1 * length(ctx)
  a <- morie_geron_dalle_autoregressive_token(c(0, 1), c(2, 0), skew,
                                              temperature = 0.5)
  expect_equal(a$log_likelihood, -2.8152119288887603, tolerance = tol)
  expect_equal(a$token_logprobs, c(-0.40760596444438024, -2.40760596444438),
               tolerance = tol)
  expect_equal(a$perplexity, 4.0861612696304865, tolerance = tol)
  expect_equal(a$next_token_probs,
               c(0.015876239976466772, 0.11731042782619835, 0.8668133321973349),
               tolerance = tol)
  expect_equal(a$next_token, 2L)
  tk <- morie_geron_dalle_autoregressive_token(0, integer(0),
                                               function(c) c(0, 5), top_k = 1)
  expect_equal(tk$next_token_probs, c(0, 1), tolerance = tol)

  pb <- matrix(c(0, 0, 1, 1, 10, 10, 11, 11, 0, 0, 2, 2), 3, 4, byrow = TRUE)
  pc <- matrix(c(10, 0, 0, 10, 1, 1), 3, 2, byrow = TRUE)
  gb <- matrix(c(0, 0, 1, 1, 10, 10, 11, 11), 2, 4, byrow = TRUE)
  r <- morie_geron_detr_hungarian_matching(pb, pc, gb, c(0, 1),
                                           no_object_class = 1)
  expect_equal(r$matching, list(c(0L, 0L), c(1L, 1L)))
  expect_equal(r$total_cost, -5.999909204262595, tolerance = tol)
  expect_equal(r$loss, 0.06940551585442827, tolerance = tol)
  expect_equal(r$loss_class, 9.079779843374107e-05, tolerance = 1e-14)
  expect_equal(r$loss_bbox, 0, tolerance = tol)
  expect_equal(r$loss_giou, 0, tolerance = tol)
  expect_equal(r$loss_no_object, 0.06931471805599453, tolerance = tol)
  expect_equal(r$matched_giou, c(1, 1), tolerance = tol)
  expect_equal(r$matched_l1, c(0, 0), tolerance = tol)
  expect_equal(r$unmatched_predictions, 2L)
  expect_equal(r$cost_matrix[1, ], c(-2.9999546021312975, 201.96689675089164),
               tolerance = tol)

  # independent route: brute force over all 3P2 = 6 injections of the two
  # ground-truth boxes into the three queries must find the same optimum
  # as the Jonker-Volgenant assignment.
  C <- r$cost_matrix
  best <- Inf; bestpair <- NULL
  for (i in 1:3) for (j in 1:3) if (i != j) {
    tot <- C[i, 1] + C[j, 2]
    if (tot < best) { best <- tot; bestpair <- c(i - 1L, j - 1L) }
  }
  expect_equal(best, r$total_cost, tolerance = tol)
  expect_equal(bestpair, c(r$matching[[1]][1], r$matching[[2]][1]))
})

# ------------------------------------------------- RL and diffusion steps

test_that("advantage, Double DQN and value iteration match Python", {
  ac <- morie_geron_actor_critic_advantage(c(0, 1, 2), c(0, 1, 2), c(1, 2, 0),
                                           c(1, -1, 0.5), 0.9,
                                           done = c(FALSE, TRUE, FALSE))
  expect_equal(ac$advantage, c(1.9, -2, -1.5), tolerance = tol)
  expect_equal(ac$td_target, c(1.9, -1, 0.5), tolerance = tol)
  expect_equal(ac$critic_loss, 3.2866666666666666, tolerance = tol)
  # independent route: a terminal transition drops the bootstrap, so its
  # advantage is r - V(s) exactly.
  expect_equal(ac$advantage[2], -1 - 1, tolerance = tol)

  dq <- morie_geron_double_dqn_target(matrix(c(0, 1, 2, 0), 2, 2, byrow = TRUE),
                                      matrix(c(10, -10, 1, 3), 2, 2, byrow = TRUE),
                                      s_next = c(0, 1, 0), r = c(0, 1, 2),
                                      gamma = 0.9, done = c(FALSE, FALSE, TRUE))
  expect_equal(dq$target, c(-9, 1.9, 2), tolerance = tol)
  expect_equal(dq$selected_action, c(1L, 0L, 1L))
  expect_equal(dq$vanilla_target, c(9, 3.7, 2), tolerance = tol)
  expect_equal(dq$overestimation_gap, c(18, 1.8000000000000003, 0), tolerance = tol)

  P <- array(0, dim = c(2, 2, 2))
  P[1, 1, ] <- c(0.7, 0.3); P[1, 2, ] <- c(0.2, 0.8)
  P[2, 1, ] <- c(0.5, 0.5); P[2, 2, ] <- c(0.1, 0.9)
  R <- matrix(c(1, 0, 0, 2), 2, 2, byrow = TRUE)
  vi <- morie_geron_value_iteration(c(0, 0), P, R, 0.9)
  expect_equal(vi$V, c(15.869565216504963, 18.043478259983228), tolerance = 1e-8)
  expect_equal(vi$policy, c(0L, 1L))
  expect_equal(vi$iterations, 225L)
  expect_lt(vi$residual, 1e-10)

  qv <- morie_geron_q_value_iteration(matrix(0, 2, 2), P, R, 0.9)
  expect_equal(qv$V, c(15.869565216504963, 18.043478259983228), tolerance = 1e-8)
  expect_equal(qv$policy, c(0L, 1L))
  expect_equal(qv$iterations, 225L)
  # independent route: the two modules solve the same MDP by different
  # operators (V-iteration vs Q-iteration) and must agree.
  expect_equal(qv$V, vi$V, tolerance = 1e-8)

  dd <- morie_geron_ddim_sampling_step(c(1, -0.5), t = 3, t_prev = 1,
                                       eps_pred = c(0.2, 0.1),
                                       alpha_bar = c(1, 0.9, 0.8, 0.5))
  expect_equal(dd$x_prev, c(1.2151496800931385, -0.7340659464533044), tolerance = tol)
  expect_equal(dd$x0_pred, c(1.214213562373095, -0.8071067811865474), tolerance = tol)
  expect_equal(dd$signal_scale, 0.9486832980505138, tolerance = tol)
  expect_equal(dd$noise_scale, 0.3162277660168379, tolerance = tol)
  # independent route: with eta = 0 the step is an exact rescale, so
  # x_prev = sqrt(ab_prev) x0 + sqrt(1 - ab_prev) eps by hand.
  expect_equal(dd$x_prev, sqrt(0.9) * dd$x0_pred + sqrt(0.1) * c(0.2, 0.1),
               tolerance = tol)
})

test_that("A2C and A3C reproduce Python's LCG action stream exactly", {
  env <- list(reset = function() 1,
              step = function(a) list(1, if (a == 0) 1 else 0, TRUE))
  r <- morie_geron_a2c(env, matrix(c(0, 0), 2, 1), 0, epochs = 25, lr = 0.5,
                       seed = 1)
  expect_equal(as.numeric(r$actor),
               c(1.5983609070309157, -1.5983609070309157), tolerance = tol)
  expect_equal(r$critic, 0.9987790286540985, tolerance = tol)
  expect_equal(r$returns,
               c(1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1,
                 1, 1, 1, 1, 1), tolerance = tol)
  expect_equal(r$policy(1), c(0.9607107267303103, 0.039289273269689756),
               tolerance = tol)
  expect_equal(r$value(1), 0.9987790286540985, tolerance = tol)

  r3 <- morie_geron_a3c(env, matrix(c(0, 0), 2, 1), 0, n_workers = 3,
                        epochs = 10, lr = 0.5, seed = 3)
  expect_equal(as.numeric(r3$actor),
               c(2.0586414204615755, -2.0586414204615755), tolerance = tol)
  expect_equal(r3$critic, 0.999937935732305, tolerance = tol)
  expect_equal(r3$updates, 30L)
  expect_equal(r3$worker_returns[1, ], c(1, 1, 0, 1, 1, 1, 1, 1, 1, 1),
               tolerance = tol)
  expect_equal(r3$worker_returns[2, ], c(1, 0, 1, 0, 1, 0, 1, 1, 1, 1),
               tolerance = tol)
  expect_equal(r3$worker_returns[3, ], rep(1, 10), tolerance = tol)
  expect_equal(r3$policy(1), c(0.983972356302139, 0.016027643697861162),
               tolerance = tol)
  # independent route: workers hold separate LCG streams (seed + 7919 w),
  # so their episode returns must NOT be identical.
  expect_false(isTRUE(all.equal(r3$worker_returns[1, ], r3$worker_returns[2, ])))
})

# ------------------------------------------ clustering, trees, ensembles

test_that("DBSCAN, agglomerative and BIRCH match Python", {
  d <- morie_geron_dbscan_core_point(matrix(c(0, 0.5, 1.2, 10), ncol = 1),
                                     eps = 1, min_samples = 2)
  expect_equal(d$is_core, c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(d$is_border, rep(FALSE, 4))
  expect_equal(d$is_noise, c(FALSE, FALSE, FALSE, TRUE))
  expect_equal(d$neighbor_counts, c(2L, 3L, 2L, 1L))
  expect_equal(d$neighbors[[2]], c(0L, 1L, 2L))   # 0-based

  a <- morie_geron_agglomerative(matrix(c(0, 1, 10, 11, 5), ncol = 1), 2,
                                 linkage = "average")
  expect_equal(a$labels, c(0L, 0L, 1L, 1L, 0L))
  expect_equal(a$merges, list(c(0L, 1L), c(2L, 3L), c(0L, 4L)))
  expect_equal(a$heights, c(1, 1, 4.5), tolerance = tol)

  b <- morie_geron_birch(matrix(c(0, 0.1, 0.2, 10, 10.1), ncol = 1),
                         n_clusters = 2, threshold = 0.5, branching_factor = 2)
  expect_equal(b$labels, c(0L, 0L, 0L, 1L, 1L))
  expect_equal(as.numeric(b$subcluster_centers),
               c(0.10000000000000002, 10.05), tolerance = tol)
  expect_equal(b$subcluster_labels, c(0L, 1L))
  expect_equal(b$subcluster_sizes, c(3, 2))
  expect_equal(b$n_subclusters, 2L)
  expect_equal(b$radii, c(0.08164965809277261, 0.049999999999835155),
               tolerance = 1e-8)
  expect_equal(b$n_leaves, 1L)
})

test_that("AdaBoost, its weight update and bagging match Python", {
  X <- matrix(c(1, 2, 3, 4, 5), ncol = 1)
  r <- morie_geron_adaboost(X, c(1, -1, 1, -1, 1), n_estimators = 3)
  expect_equal(r$alphas,
               c(0.2027325540540821, 0.3465735902799726, 0.39422868018213514),
               tolerance = tol)
  expect_equal(r$errors, c(0.4, 0.33333333333333337, 0.3125), tolerance = tol)
  expect_equal(r$train_errors, c(0.4, 0.4, 0.2), tolerance = tol)
  expect_equal(r$decision,
               c(0.2503876439562447, -0.15507746415191953, 0.5380697164080256,
                 -0.2503876439562447, -0.2503876439562447), tolerance = tol)
  expect_equal(r$weights,
               c(0.18181818181818182, 0.2000000000000001, 0.13636363636363635,
                 0.18181818181818182, 0.30000000000000004), tolerance = tol)
  expect_equal(as.numeric(r$predict(matrix(c(1, 4), ncol = 1))), c(1, -1))
  # independent route: alpha_1 = 0.5 log((1 - err)/err) by hand.
  expect_equal(r$alphas[1], 0.5 * log((1 - 0.4) / 0.4), tolerance = tol)

  w <- morie_geron_adaboost_weight_update(c(0, 0, 1, 1), c(0, 1, 1, 1),
                                          rep(0.25, 4), log(3))
  expect_equal(w$weights_new,
               c(0.16666666666666666, 0.5000000000000001, 0.16666666666666666,
                 0.16666666666666666), tolerance = tol)
  expect_equal(w$weighted_error, 0.25, tolerance = tol)
  expect_equal(w$boost_factor, 3.0000000000000004, tolerance = tol)
  expect_equal(w$misclassified, c(FALSE, TRUE, FALSE, FALSE))

  # LCG-driven: bootstrap indices come from the integer LCG, so R must
  # reproduce Python's replicates exactly.
  bg <- morie_geron_bagging(X, c(1, 1, 5, 5, 6), n_estimators = 6, seed = 1)
  expect_equal(bg$train_pred,
               c(1, 1, 4.458333333333333, 5.166666666666667, 5.166666666666667),
               tolerance = tol)
  expect_equal(bg$train_mse, 0.203125, tolerance = tol)
  expect_equal(bg$oob_pred[c(1, 3, 4, 5)], c(1, 1, 5.25, 5), tolerance = tol)
  expect_true(is.na(bg$oob_pred[2]))
  expect_equal(bg$oob_mse, 4.265625, tolerance = tol)
  expect_equal(bg$oob_coverage, 0.8, tolerance = tol)
  expect_equal(bg$predict(matrix(c(1, 5), ncol = 1)),
               c(1, 5.166666666666667), tolerance = tol)

  bp <- morie_geron_bagging_predictor(matrix(c(1, 2, 3, 4, 2, 9), 3, 2, byrow = TRUE))
  expect_equal(bp$prediction, c(2, 5), tolerance = tol)
  expect_equal(bp$per_instance_variance, c(1, 13), tolerance = tol)
  expect_equal(bp$mean_disagreement, 7, tolerance = tol)
  expect_equal(bp$se, 1.5275252316519468, tolerance = tol)
  v <- morie_geron_bagging_predictor(matrix(c(0, 1, 1, 1, 1, 0, 1, 0, 0), 3, 3,
                                            byrow = TRUE), aggregate = "vote")
  expect_equal(v$prediction, c(1, 1, 0), tolerance = tol)
})

test_that("K-fold cross-validation matches Python", {
  r <- morie_geron_cross_validation_score(matrix(c(1, 2, 3, 4, 5), ncol = 1),
                                          c(2, 4, 6, 8, 10.5), K = 2)
  expect_equal(r$cv_score, 0.9567467281380131, tolerance = tol)
  expect_equal(r$fold_scores, c(0.9934934562760261, 0.92), tolerance = tol)
  # independent route: numpy array_split puts the extra row in the first
  # fold, so the sizes are 3 and 2 -- not 2 and 3.
  expect_equal(r$fold_sizes, c(3L, 2L))
  expect_equal(r$se, 0.036746728138013045, tolerance = tol)
  expect_equal(r$worst_fold, 1L)
  expect_equal(r$spread, 0.0734934562760261, tolerance = tol)
  expect_error(morie_geron_cross_validation_score(matrix(1:6, ncol = 1),
                                                  as.numeric(1:6), K = 2,
                                                  shuffle = TRUE),
               "not portable")
})

# ------------------------------------------------------------ autoencoders

test_that("autoencoder family matches Python", {
  ae <- morie_geron_autoencoder(matrix(c(0, 0, 1, 1.1, 2, 2.3, 3, 2.9), 4, 2,
                                       byrow = TRUE), 1)
  expect_equal(ae$recon_error, 0.010936513573625468, tolerance = 1e-10)
  expect_equal(ae$explained_variance_ratio,
               c(0.9956199194698871, 0.004380080530112823), tolerance = 1e-10)
  expect_equal(ae$mean, c(1.5, 1.575), tolerance = tol)
  expect_equal(as.numeric(t(ae$reconstruction)),
               c(-0.03844634121675505, 0.038494915222925474,
                 1.0121845329999588, 1.0878000727762376,
                 2.112815367361093, 2.1870420990667405,
                 2.9134464408557035, 2.986662912934096), tolerance = 1e-9)

  l <- morie_geron_autoencoder_reconstruction_loss(
    matrix(c(1, 2, 3, 1), 2, 2, byrow = TRUE), matrix(c(0.5, 0.2), ncol = 1),
    matrix(c(1, 3, 2, 1), 2, 2, byrow = TRUE))
  expect_equal(l$loss, 1, tolerance = tol)
  expect_equal(l$mse_per_element, 0.5, tolerance = tol)
  expect_equal(l$per_sample_loss, c(1, 1), tolerance = tol)
  expect_equal(l$compression_ratio, 2, tolerance = tol)
  expect_equal(l$explained_variance, 0.19999999999999996, tolerance = tol)

  d <- morie_geron_denoising_autoencoder(
    matrix(c(1, 2, 0, 1), 2, 2, byrow = TRUE),
    matrix(c(0.1, -0.1, 0.2, 0.2), 2, 2, byrow = TRUE),
    matrix(c(1, 3, 0.1, 1), 2, 2, byrow = TRUE))
  expect_equal(d$loss, 0.505, tolerance = tol)
  expect_equal(as.numeric(t(d$x_tilde)), c(1.1, 1.9, 0.2, 1.2), tolerance = tol)
  expect_equal(d$noise_energy, 0.05000000000000001, tolerance = tol)
  expect_equal(d$denoising_gain, 0.09900990099009903, tolerance = tol)
  expect_equal(d$snr_db, 17.781512503836435, tolerance = tol)
  expect_equal(d$mse_per_element, 0.2525, tolerance = tol)

  c_ <- morie_geron_convolutional_autoencoder(
    matrix(c(1, 2, 3, 4), 2, 2, byrow = TRUE), list(matrix(1)),
    list(matrix(1, 2, 2)))
  expect_equal(as.numeric(c_$code), 1, tolerance = tol)
  expect_equal(c_$x_hat, matrix(1, 2, 2), tolerance = tol)
  expect_equal(c_$loss, 14, tolerance = tol)
  expect_equal(c_$compression_ratio, 4, tolerance = tol)

  zero <- function(A) matrix(0, nrow(as.matrix(A)), ncol(as.matrix(A)))
  an <- morie_geron_anomaly_autoencoder(zero, matrix(c(0, 3, 1, 5), ncol = 1),
                                        quantile = 0.6)
  expect_equal(an$errors, c(0, 9, 1, 25), tolerance = tol)
  expect_equal(an$is_anomaly, c(FALSE, TRUE, FALSE, TRUE))
  expect_equal(an$threshold, 7.399999999999999, tolerance = tol)
  expect_equal(an$n_anomalies, 2L)
})

test_that("DCGAN generator, aux pretraining and head fine-tuning match Python", {
  W0 <- matrix(c(1, 1, 1, 1, 0.5, -0.5, 0.25, 1), 2, 4, byrow = TRUE)
  K1 <- matrix(c(1, 0.5, 0.5, 1), 2, 2, byrow = TRUE)
  g <- morie_geron_dcgan_generator(c(1, 2), list(W0, K1), seed_shape = c(2, 2))
  expect_equal(g$image_shape, c(4L, 4L))
  expect_equal(g$seed, matrix(c(2, 0, 1.5, 3), 2, 2, byrow = TRUE), tolerance = tol)
  expect_equal(as.numeric(t(g$image)),
               c(0.9640275800758169, 0.7615941559557649, 0, 0,
                 0.7615941559557649, 0.9640275800758169, 0, 0,
                 0.9051482536448665, 0.6351489523872873, 0.9950547536867305,
                 0.9051482536448665,
                 0.6351489523872873, 0.9051482536448665, 0.9051482536448665,
                 0.9950547536867305), tolerance = tol)
  expect_equal(g$upsample_factor, 2, tolerance = tol)

  Xa <- matrix(c(1, 0, 0, 1, 1, 1, 2, 1), 4, 2, byrow = TRUE)
  Xt <- matrix(c(1, 1, 2, 0), 2, 2, byrow = TRUE)
  p <- morie_geron_auxiliary_task_pretraining(NULL, list(Xa, c(1, 2, 3, 4)),
                                              list(Xt, c(3, 2)),
                                              aux_epochs = 50, epochs = 5)
  expect_equal(p$theta_pretrained, c(1.1444672574126522, 1.766182240947651),
               tolerance = tol)
  expect_equal(p$theta, c(1.0678448042434991, 1.7955002048720239), tolerance = tol)
  expect_equal(p$theta_scratch, c(1.018965, 0.552655), tolerance = tol)
  expect_equal(p$target_loss, 0.01854312819249565, tolerance = tol)
  expect_equal(p$scratch_loss, 1.0208540546500002, tolerance = tol)
  expect_equal(p$transfer_gain, 1.0023109264575045, tolerance = tol)
  expect_equal(p$aux_losses[1:3], c(7.5, 4.96171875, 3.3157163085937498),
               tolerance = tol)
  expect_equal(p$finetune_losses,
               c(0.045733333000299733, 0.034794060435285205,
                 0.02809309567441813, 0.023757164775967378,
                 0.020761302691197803), tolerance = tol)

  ident <- function(A) as.matrix(A)
  f <- morie_geron_bert_finetune(ident,
                                 matrix(c(1, 0, 0, 1, 1, 1), 3, 2, byrow = TRUE),
                                 c(0, 1, 0), epochs = 25, lr = 0.5, l2 = 0.01)
  expect_equal(as.numeric(t(f$W)),
               c(1.4507439072683632, -1.450743907268363,
                 -0.5863562317648819, 0.586356231764882), tolerance = tol)
  expect_equal(f$b, c(-0.004884104665871334, 0.004884104665871365),
               tolerance = tol)
  expect_equal(f$losses[1:3],
               c(0.6931471805599453, 0.5763120088199503, 0.5125556775071097),
               tolerance = tol)
  expect_equal(f$losses[25], 0.19022091296894095, tolerance = tol)
  expect_equal(f$accuracy, 1, tolerance = tol)
  expect_equal(as.numeric(t(f$probabilities)),
               c(0.9474355862259052, 0.052564413774094945,
                 0.2346064578435847, 0.7653935421564153,
                 0.8480009057151229, 0.15199909428487718), tolerance = tol)
  # independent route: a zero-initialised head over K classes starts at
  # log K exactly.
  expect_equal(f$losses[1], log(2), tolerance = tol)
})

# -------------------------------------------------------------------- BF16

test_that("BF16 quantisation matches Python bit for bit", {
  vals <- c(1, 1.1, -1.1, 1.00390625, 0, 1.0078125, 3.0e38, 1e-40)
  r <- morie_geron_bf16(vals)
  expect_equal(r$values,
               c(1, 1.1015625, -1.1015625, 1, 0, 1.0078125,
                 3.00405527047391e+38, 9.183549615799121e-41), tolerance = 1e-12)
  expect_equal(r$bits,
               c("0011111110000000", "0011111110001101", "1011111110001101",
                 "0011111110000000", "0000000000000000", "0011111110000001",
                 "0111111101100010", "0000000000000001"))
  expect_equal(r$max_rel_error, 0.08164008856254029, tolerance = 1e-12)
  expect_equal(r$mantissa_bits, 7L)
  expect_equal(r$exponent_bits, 8L)
  # independent route: 1 + 1/256 sits exactly halfway between two bf16
  # values, so ties-to-even must round DOWN to 1.0, while truncation of
  # 1.1 gives 1.09375 (the next bf16 value below).
  expect_equal(r$values[4], 1, tolerance = 0)
  expect_equal(morie_geron_bf16(c(1.1, -1.1, 1.9), rounding = "truncate")$values,
               c(1.09375, -1.09375, 1.8984375), tolerance = 1e-12)
  # independent route: values needing no more than 7 mantissa bits survive
  # exactly (1, 1.0078125 = 1 + 2^-7, -2).
  expect_equal(r$abs_error[c(1, 5, 6)], c(0, 0, 0), tolerance = 0)

  g <- morie_geron_bf16_range(c(1, 1.0078125, -2, 1.00390625, 1e-40, 3.0e38))
  expect_equal(g$bf16, c(1, 1.0078125, -2, 1, 9.183549615799121e-41,
                         3.00405527047391e+38), tolerance = 1e-12)
  expect_equal(g$max_rel_error, 0.08164008856254029, tolerance = 1e-12)
  expect_equal(g$machine_eps, 0.0078125, tolerance = 0)
  expect_equal(g$n_overflow, 0L)
  expect_equal(g$n_underflow, 1L)
  expect_equal(g$exact, c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
})

# --------------------------------------------------------- autograd tape

test_that("reverse-mode autograd matches Python", {
  f <- function(p) p[[1]] * p[[2]] + exp(p[[1]])
  r <- morie_geron_autograd(f, c(0, 3))
  expect_equal(r$value, 1, tolerance = tol)
  expect_equal(r$grad, c(4, 0), tolerance = tol)
  expect_equal(r$tape_size, 5L)
  # independent route: d/dx (x y + e^x) at (0, 3) is y + e^x = 4, and
  # d/dy is x = 0.
  expect_equal(r$grad, c(3 + exp(0), 0), tolerance = tol)

  g2 <- function(p) morie_gvar_sigmoid(log(p[[1]]^2 + 1) * tanh(p[[2]]) -
                                         p[[1]] / p[[2]])
  r2 <- morie_geron_autograd(g2, c(2, 1.5))
  expect_equal(r2$value, 0.5308225133776785, tolerance = tol)
  expect_equal(r2$grad, c(0.014308403152655746, 0.29381048015294964),
               tolerance = tol)
  expect_equal(r2$tape_size, 11L)
  # independent route: central differences on the same scalar function.
  h <- 1e-6
  fv <- function(x, y) 1 / (1 + exp(-(log(x^2 + 1) * tanh(y) - x / y)))
  expect_equal(r2$grad[1], (fv(2 + h, 1.5) - fv(2 - h, 1.5)) / (2 * h),
               tolerance = 1e-6)
  expect_equal(r2$grad[2], (fv(2, 1.5 + h) - fv(2, 1.5 - h)) / (2 * h),
               tolerance = 1e-6)
})

# ----------------------------------------------- time series and decoding

test_that("ARIMA fitting and forecasting match Python", {
  y <- c(1, 0.5, 0.25, 0.125, 0.0625, 0.03125)
  r <- morie_geron_arima(y, p = 1, d = 0, q = 0, include_mean = FALSE)
  expect_equal(r$ar, 0.49999999999999994, tolerance = 1e-9)
  expect_lt(r$sigma2, 1e-25)
  expect_equal(r$forecast(2), c(0.015624999999999998, 0.007812499999999998),
               tolerance = 1e-12)

  d1 <- morie_geron_arima(c(1, 3, 5, 7), p = 0, d = 1, q = 0)
  expect_equal(d1$intercept, 2, tolerance = 1e-9)
  expect_equal(d1$forecast(2), c(9, 11), tolerance = 1e-9)
  expect_equal(d1$sigma2, 0, tolerance = 1e-25)

  y2 <- c(1, 2, 1.5, 2.5, 2, 3, 2.5, 3.5, 3, 4, 3.5, 4.5)
  ar <- morie_geron_arima(y2, p = 1, d = 0, q = 1)
  expect_equal(ar$ar, 0.28947368421052627, tolerance = 1e-9)
  expect_equal(ar$ma, 5.3812523592602437e-17, tolerance = 1e-9)
  expect_equal(ar$intercept, 2.539473684210526, tolerance = 1e-9)
  expect_equal(ar$sigma2, 0.6217105263157895, tolerance = 1e-9)
  expect_equal(ar$aic, 4.673035195573945, tolerance = 1e-9)
  expect_equal(ar$forecast(2),
               c(3.8421052631578942, 3.6516620498614953), tolerance = 1e-9)

  f <- morie_geron_arima_forecast(c(1, 2, 3, 4.5), phi = 0.5, theta = 0.25, d = 1)
  expect_equal(f$forecast, 5.484375, tolerance = tol)
  expect_equal(f$forecast_differenced, 0.984375, tolerance = tol)
  expect_equal(f$residuals, c(1, 0.25, 0.9375), tolerance = tol)
  expect_equal(f$differenced, c(1, 1, 1.5), tolerance = tol)
  expect_equal(f$sigma2, 0.6471354166666666, tolerance = tol)
  # independent route: the ARMA forecast on the differenced scale is
  # phi w_T + theta eps_T by hand, then the last level is added back.
  expect_equal(f$forecast_differenced, 0.5 * 1.5 + 0.25 * 0.9375, tolerance = tol)
  expect_equal(f$forecast, f$forecast_differenced + 4.5, tolerance = tol)
})

test_that("beam search routes match Python", {
  lp <- log(c(0.5, 0.3, 0.2))
  r <- morie_geron_beam_search(function(s, prefix) lp, NULL, beam_width = 2,
                               max_len = 3)
  expect_equal(r$sequence, c(0L, 0L, 0L))
  expect_equal(r$score, -2.0794415416798357, tolerance = tol)
  expect_equal(r$beams, list(c(0L, 0L, 0L), c(0L, 0L, 1L)))
  expect_equal(r$scores, c(-2.0794415416798357, -2.5902671654458267),
               tolerance = tol)
  expect_equal(r$finished, 2L)
  # independent route: the best 3-token score is log(0.5^3).
  expect_equal(r$score, 3 * log(0.5), tolerance = tol)

  e <- morie_geron_beam_search(function(s, prefix) log(c(0.1, 0.9)), NULL,
                               beam_width = 2, max_len = 3, eos = 1)
  expect_equal(e$sequence, 1L)
  expect_equal(e$score, -0.10536051565782628, tolerance = tol)

  S <- matrix(c(-0.1, -2, -3, -0.2, -3, -0.15, -1, -0.5, -2), 3, 3, byrow = TRUE)
  b <- morie_geron_beam_search_decoder(S, beam_width = 3, length_penalty = 0.5)
  expect_equal(b$best_sequence, c(0L, 2L, 1L))
  expect_equal(b$best_score, -0.75, tolerance = tol)
  expect_equal(b$beams[[1]]$sequence, c(0L, 2L, 1L))
  expect_equal(b$beams[[2]]$sequence, c(0L, 0L, 1L))
  expect_equal(b$beams[[3]]$sequence, c(0L, 2L, 0L))
  expect_equal(vapply(b$beams, function(x) x$score, numeric(1)),
               c(-0.75, -0.8, -1.25), tolerance = tol)
  expect_equal(b$normalised_scores,
               c(-0.43301270189221935, -0.46188021535170065,
                 -0.7216878364870323), tolerance = tol)
  expect_equal(b$greedy_sequence, c(0L, 2L, 1L))
  # independent route: with prefix-independent scores the best sequence is
  # the per-step argmax, and its score the sum of per-step maxima.
  expect_equal(b$best_score, sum(apply(S, 1, max)), tolerance = tol)
})

test_that("both BPE tie-break rules match Python", {
  corpus <- c(low = 5, lower = 2, newest = 6, widest = 3)
  r <- morie_geron_bpe_tokenizer(corpus, vocab_size = 20)
  expect_equal(r$merges,
               list(c("e", "s"), c("es", "t"), c("est", "</w>"), c("l", "o"),
                    c("lo", "w"), c("n", "e"), c("ne", "w"),
                    c("new", "est</w>"), c("low", "</w>")))
  expect_equal(r$n_merges, 9L)
  expect_equal(r$tokenize("newest"), "newest</w>")
  expect_equal(r$tokenize("lowest"), c("low", "est</w>"))
  expect_equal(names(r$vocab),
               c("l", "o", "w", "</w>", "e", "r", "n", "s", "t", "i", "d",
                 "es", "est", "est</w>", "lo", "low", "ne", "new",
                 "newest</w>", "low</w>"))
  expect_equal(morie_geron_bpe_tokenizer(corpus, vocab_size = 13)$n_merges, 2L)

  g <- morie_geron_bpe_merge(c(low = 5, lowest = 2, newer = 3), 4)
  expect_equal(g$merges, list(c("l", "o"), c("lo", "w"), c("low", "</w>"),
                              c("e", "r")))
  expect_equal(g$merge_counts, c(7, 7, 5, 3))
  expect_equal(g$vocab, c("</w>", "e", "er", "low", "low</w>", "n", "s", "t", "w"))
  expect_equal(g$splits[["low"]], "low</w>")
  expect_equal(g$splits[["lowest"]], c("low", "e", "s", "t", "</w>"))
  expect_equal(g$splits[["newer"]], c("n", "e", "w", "er", "</w>"))
  expect_equal(g$n_tokens_before, 52)
  expect_equal(g$n_tokens_after, 30)
  expect_equal(g$compression, 1.7333333333333334, tolerance = tol)
})

# ------------------------------------------------------------ transformers

test_that("ALBERT reproduces Python's LCG weights and parameter accounting", {
  ids <- c(1, 2, 3, 4, 5, 6, 0, 2)
  r <- morie_geron_albert(ids, n_layers = 3, n_heads = 2, d_model = 8,
                          d_embed = 4, vocab_size = 7, seed = 2)
  H <- matrix(r$hidden[1, , ], 8, 8)
  expect_equal(H[1, ],
               c(-1.1551111544120949, 0.9552045240812964, 0.0024182378045032584,
                 1.353350306347756, -0.02707894813262739, 0.2170468357492386,
                 0.5453190437738651, -1.8911488452119374), tolerance = 1e-9)
  expect_equal(H[8, ],
               c(1.1952272218067719, 0.7348881260690262, -0.6466360659887094,
                 -0.6520411511787417, 0.3324607509051006, -0.7920039977983735,
                 -1.575145465432593, 1.403250581617519), tolerance = 1e-9)
  expect_equal(r$n_params, 660L)
  expect_equal(r$n_params_unshared, 1732L)
  expect_equal(r$block_params, 536L)
  expect_equal(r$embedding_params, 60L)
  expect_equal(r$embedding_params_direct, 56L)
  expect_true(r$shared)
  # independent route: depth is free under sharing, and the unshared stack
  # costs exactly (L - 1) extra blocks.
  r1 <- morie_geron_albert(ids, n_layers = 1, n_heads = 2, d_model = 8,
                           d_embed = 4, vocab_size = 7, seed = 2)
  expect_equal(r$n_params, r1$n_params)
  expect_equal(r$n_params_unshared - r$n_params, 2L * r$block_params)
  # post-layernorm leaves every output row zero-mean.
  expect_equal(rowMeans(H), rep(0, 8), tolerance = 1e-12)
})

test_that("BERT and RoBERTa reproduce Python exactly", {
  ids <- c(1, 2, 3, 4, 5, 6, 0, 2)
  r <- morie_geron_bert(ids, n_layers = 2, n_heads = 2, d_model = 8,
                        vocab_size = 7, seed = 1)
  H <- matrix(r$hidden[1, , ], 8, 8)
  expect_equal(H[1, ],
               c(-1.0855679039800283, 0.8128893569529952, -0.35560326268526793,
                 -0.7670632882374321, -0.12847203990759942, 1.8654384031673434,
                 0.8015866525944906, -1.1432079179045014), tolerance = 1e-9)
  expect_equal(H[8, ],
               c(-0.6491229946582199, 0.160449687576375, -0.02897707284255786,
                 -1.0557639879150431, -1.1619725371956173, 0.9315479462009373,
                 -0.23660110377112925, 2.040440062605255), tolerance = 1e-9)
  expect_equal(r$mlm_loss, 2.399822645609443, tolerance = 1e-9)
  expect_equal(r$masked_positions[[1]], 1L)   # 0-based
  expect_equal(as.numeric(r$nsp_logits),
               c(0.21787194636636448, 0.2085144416919827), tolerance = 1e-9)
  expect_equal(r$n_params, 1216L)
  a0 <- matrix(r$attentions[[1]][[1]][1, , ], 8, 8)
  expect_equal(a0[1, ],
               c(0.12499946164980213, 0.124998110148695, 0.12499676529348241,
                 0.12500344034821523, 0.12500119615842142, 0.12499849727827686,
                 0.1250022013144804, 0.12500032780862658), tolerance = 1e-12)
  expect_equal(rowSums(a0), rep(1, 8), tolerance = 1e-12)
  expect_equal(rowMeans(H), rep(0, 8), tolerance = 1e-12)

  rb <- morie_geron_roberta(ids, n_layers = 2, n_heads = 2, d_model = 8,
                            vocab_size = 7, epochs = 3, seed = 1)
  Hb <- matrix(rb$hidden[1, , ], 8, 8)
  expect_equal(Hb[1, ],
               c(-1.0829881092427913, 0.8127166450649894, -0.36061898431280537,
                 -0.7751990017552053, -0.11633457063899946, 1.868211141699277,
                 0.7945865313015628, -1.1403736521160277), tolerance = 1e-9)
  expect_equal(rb$epoch_losses,
               c(2.0326013693252416, 2.2235191240776815, 2.2235191240776815),
               tolerance = 1e-9)
  expect_equal(rb$masks, list(4L, 6L, 6L))   # dynamic masking, 0-based
  expect_equal(rb$n_params, 1200L)
  expect_equal(rb$n_params_with_nsp, 1216L)
  expect_false(rb$has_nsp_head)
  # independent route: dropping the NSP head is exactly d_model*2 fewer
  # parameters, and BERT's count is the with-NSP one.
  expect_equal(rb$n_params_with_nsp - rb$n_params, 16L)
  expect_equal(rb$n_params_with_nsp, r$n_params)
})

test_that("BART corruption and AlexNet accounting match Python", {
  src <- c("the", "cat", "sat", "on", "the", "mat", "today", "ok")
  tgt <- c("the", "cat", "sat", "on", "the", "mat")
  r <- morie_geron_bart(src, tgt, mask_ratio = 0.35, mean_span = 2, seed = 2)
  expect_equal(r$corrupted, c("the", "<mask>", "the", "mat", "today", "ok"))
  expect_equal(r$spans, list(c(1, 3)))   # 0-based start, length
  expect_equal(r$n_masked, 3L)
  expect_equal(r$loss, 1.1499958486105293, tolerance = tol)
  expect_equal(r$perplexity, 3.15817979882819, tolerance = tol)
  expect_equal(r$token_logprobs,
               c(-1.0986122886681098, -1.252762968495368, -1.0986122886681098,
                 -1.0986122886681098, -1.0986122886681098, -1.252762968495368),
               tolerance = tol)
  # independent route: each span collapses to ONE mask token, so the
  # corrupted sequence is shorter by (length - 1) per span.
  expect_equal(length(r$corrupted), length(src) - (3 - 1))

  a <- morie_geron_alexnet(1000)
  expect_equal(a$total_params, 62378344)
  expect_equal(a$flatten_dim, 9216L)
  expect_equal(vapply(a$layers, function(l)
    if (l$kind == "conv") l$out else NA_integer_, numeric(1))[
      vapply(a$layers, function(l) l$kind == "conv", logical(1))],
    c(55, 27, 13, 13, 13))
  expect_equal(a$conv_params, 3747200)
  expect_equal(a$fc_params, 58631144)
  expect_equal(morie_geron_alexnet(10)$total_params, 58322314)
  # independent route: only the last FC layer depends on n_classes, so the
  # difference is (4096 + 1) * (10 - 1000).
  expect_equal(morie_geron_alexnet(10)$total_params - a$total_params,
               (4096 + 1) * (10 - 1000))
  a128 <- morie_geron_alexnet(5, input_size = 128, in_channels = 1)
  expect_equal(a128$total_params, 24724165)
  expect_equal(a128$flatten_dim, 1024L)
})
