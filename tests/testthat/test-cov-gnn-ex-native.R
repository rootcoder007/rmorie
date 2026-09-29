# Coverage for GNNExplainer (Ying et al. 2019): the L-hop computation
# graph (BFS by hand), the conditional entropy of a prediction, the
# masked objective -log p_y + size + mean mask entropy, and the mask
# optimisation regenerated step by step with forward differences.

.adj <- list("0" = c(1, 2), "1" = c(0, 3), "2" = c(0), "3" = c(1, 4), "4" = c(3), "5" = integer(0))

test_that("the computation graph holds nodes within L hops and their edges", {
  g1 <- gnnEx_computation_graph(.adj, 0, 1)
  expect_identical(g1$nodes, c(0L, 1L, 2L))
  expect_identical(g1$edges, list(c(0L, 1L), c(0L, 2L)))
  g2 <- gnnEx_computation_graph(.adj, 0, 2)
  expect_identical(g2$nodes, 0:3)
  expect_identical(g2$size, 3L)
  expect_identical(gnnEx_computation_graph(.adj, 5, 3)$nodes, 5L)
  p <- gnnEx_computation_graph(list(c(2), c(1, 3), c(2)), 1, 1)
  expect_identical(p$nodes, 1:2)
})

test_that("conditional entropy and the masked objective", {
  p <- c(0.2, 0.5, 0.3)
  expect_equal(gnnEx_conditional_entropy(p), -sum(p * log(p)), tolerance = 1e-12)
  expect_equal(gnnEx_conditional_entropy(2 * p), -sum(p * log(p)), tolerance = 1e-12)
  expect_error(gnnEx_conditional_entropy(c(0, 0)), "no mass")
  pred <- function(edges, em, fm) {
    s <- sum(em) + 0.5 * sum(fm)
    c(1, exp(s)) / (1 + exp(s))
  }
  el <- c(0.3, -1)
  fl <- c(2, 0)
  o <- gnnEx_mask_objective(pred, list(1:2, 2:3), el, fl, y = 1, size_coef = 0.1, entropy_coef = 0.5)
  em <- stats::plogis(el)
  fm <- stats::plogis(fl)
  pr <- pred(NULL, em, fm)
  H <- mean(-(em * log(em) + (1 - em) * log(1 - em)))
  expect_equal(o$fit, -log(pr[2]), tolerance = 1e-12)
  expect_equal(o$size, 0.1 * (sum(em) + sum(fm)), tolerance = 1e-12)
  expect_equal(o$entropy, 0.5 * H, tolerance = 1e-12)
  expect_equal(o$loss, o$fit + o$size + o$entropy, tolerance = 1e-12)
})

test_that("explanation masks follow forward-difference gradient descent", {
  pred <- function(edges, em, fm) {
    s <- 3 * em[1] - em[2] + fm[1]
    c(1, exp(s)) / (1 + exp(s))
  }
  r <- gnnEx_explain_node(pred, .adj, 1, y = 1, n_features = 2, L = 1, iters = 6, lr = 0.5, seed = 2)
  edges <- gnnEx_computation_graph(.adj, 1, 1)$edges
  u <- .ghc_unif(.ghc_rng(2), 4)
  el <- (u[1:2] - 0.5) * 0.1
  fl <- (u[3:4] - 0.5) * 0.1
  L <- function(e, f) gnnEx_mask_objective(pred, edges, e, f, 1)$loss
  for (it in 1:6) {
    b <- L(el, fl)
    ge <- vapply(1:2, function(i) (L(replace(el, i, el[i] + 1e-4), fl) - b) / 1e-4, 1)
    gf <- vapply(1:2, function(i) (L(el, replace(fl, i, fl[i] + 1e-4)) - b) / 1e-4, 1)
    el <- el - 0.5 * ge
    fl <- fl - 0.5 * gf
  }
  expect_equal(r$edge_mask, stats::plogis(el), tolerance = 1e-12)
  expect_equal(r$feature_mask, stats::plogis(fl), tolerance = 1e-12)
  expect_identical(r$estimate[[1]], edges[[which.max(stats::plogis(el))]])
  expect_lt(r$loss_history[6], r$loss_history[1])
  up <- gnnEx_explain_node(pred, .adj, 1, y = 1, n_features = 2, L = 1, iters = 2, penalize = FALSE)
  expect_equal(up$final$size, 0)
  expect_identical(morie_gnnEx, gnnEx_explain_node)
  expect_identical(gnnexplainer, gnnEx_explain_node)
  expect_identical(gnn_explainer, gnnEx_explain_node)
  expect_error(gnnEx_explain_node(pred, .adj, 5, 1, 2), "empty computation graph")
  expect_match(gnnEx_cheatsheet(), "CONDITIONAL")
})
