# Coverage tests for R/bprMF_native.R (Rendle et al. 2009, BPR-MF): the
# BPR-Opt criterion, per-user AUC, LearnBPR and recommendation.

bp_W <- list(c(0.5, -0.2), c(0.1, 0.8))
bp_H <- list(c(1, 0), c(0.2, 0.9), c(-0.5, 0.3), c(0.4, 0.4))
bp_pos <- list(`0` = c(0L, 3L), `1` = 1L)

test_that("sigmoid and predictions", {
  expect_equal(bpr_sigmoid(-800), 0)
  expect_equal(bpr_sigmoid(1.3), plogis(1.3), tolerance = 1e-12)
  expect_equal(morie_bprMF(-2), plogis(-2), tolerance = 1e-12)
  expect_equal(bpr_predict(bp_W, bp_H, 1, 2), sum(bp_W[[2]] * bp_H[[3]]))
})

test_that("BPR-Opt and AUC over all (u, i, j) triples", {
  X <- t(sapply(bp_W, function(w) sapply(bp_H, function(h) sum(w * h))))
  ll <- log(plogis(X[1, 1] - X[1, 2])) + log(plogis(X[1, 1] - X[1, 3])) +
    log(plogis(X[1, 4] - X[1, 2])) + log(plogis(X[1, 4] - X[1, 3])) +
    sum(log(plogis(X[2, 2] - X[2, c(1, 3, 4)])))
  sq <- sum(unlist(bp_W)^2) + sum(unlist(bp_H)^2)
  o <- bpr_opt_R(bp_W, bp_H, bp_pos, 4, lam = 0.1)
  expect_equal(o$loglik, ll, tolerance = 1e-12)
  expect_equal(o$bpr_opt, ll - 0.1 * sq, tolerance = 1e-12)
  expect_equal(o$n_triples, 7L)
  a <- bpr_auc_R(bp_W, bp_H, bp_pos, 4)
  a0 <- mean(outer(X[1, c(1, 4)], X[1, c(2, 3)], ">"))
  a1 <- mean(X[2, 2] > X[2, c(1, 3, 4)])
  expect_equal(a$auc, (a0 + a1) / 2, tolerance = 1e-12)
  expect_error(bpr_auc_R(bp_W, bp_H, list(`0` = 0:3), 4), "no comparable pair")
})

test_that("LearnBPR: reproducible SGD that raises the AUC; recommendation", {
  pos <- list(`0` = c(0L, 1L), `1` = c(2L, 3L), `2` = c(0L, 1L), `3` = c(2L, 3L))
  f <- bpr_learn_bpr_R(pos, 4, 4, k_dim = 2, alpha = 0.1, lam = 0.01, iters = 1500, seed = 3)
  expect_identical(bpr_learn_bpr_R(pos, 4, 4, k_dim = 2, alpha = 0.1, lam = 0.01, iters = 1500, seed = 3), f)
  expect_equal(f$auc, bpr_auc_R(f$W, f$H, pos, 4)$auc, tolerance = 1e-12)
  expect_gt(f$auc, 0.9)
  expect_equal(f$param_norm, sqrt(sum(unlist(f$W)^2) + sum(unlist(f$H)^2)), tolerance = 1e-12)
  expect_equal(f$final_bpr_opt, bpr_opt_R(f$W, f$H, pos, 4, 0.01)$bpr_opt, tolerance = 1e-12)
  p <- bpr_learn_bpr_R(pos, 4, 4, k_dim = 2, iters = 200, seed = 3, regularizer_sign = "paper")
  expect_match(p$caveat, "\\+lambda")
  expect_identical(bayesianpersonalizedranking, bpr_learn_bpr_R)
  expect_error(bpr_learn_bpr_R(pos, 4, 4, regularizer_sign = "none"), "regularizer_sign")
  expect_error(bpr_learn_bpr_R(pos, 4, 1), "at least 1 user, 2 items")
  r <- bpr_recommend_R(bp_W, bp_H, 0, 4, top_k = 2, exclude = 0L)
  sc <- vapply(1:3, function(i) sum(bp_W[[1]] * bp_H[[i + 1]]), 0)
  expect_equal(vapply(r$ranking, function(z) z$i, 0), (1:3)[order(-sc)][1:2])
  expect_equal(r$n_scored, 3L)
})
