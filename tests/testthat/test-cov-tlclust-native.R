# Clustered LTMLE inference (van der Laan & Rose 2018, Chap. 15): naive
# and cluster-aggregated influence-curve standard errors, the design
# effect, the pooled and sequential g-formula summaries.

test_that("naive and clustered standard errors of the mean influence curve", {
  ic <- c(0.3, -0.1, 0.4, 0.2, -0.5, -0.2, 0.1, 0.6)
  cl <- c("a", "a", "b", "b", "b", "c", "c", "d")
  expect_equal(naive_variance(ic), stats::sd(ic) / sqrt(8), tolerance = 1e-15)
  cv <- cluster_variance(ic, cl)
  sums <- as.numeric(tapply(ic, cl, sum))
  expect_equal(cv$cluster_sums, sums, tolerance = 1e-15)
  expect_equal(cv$se, stats::sd(sums) / sqrt(4) / (8 / 4), tolerance = 1e-15)
  de <- design_effect(ic, cl)
  expect_equal(de$ratio, cv$se / naive_variance(ic), tolerance = 1e-15)
  expect_error(naive_variance(1), "at least 2")
  expect_error(cluster_variance(ic, cl[-1]), "8 influence values for 7")
  expect_error(cluster_variance(ic, rep("a", 8)), "at least 2 clusters")
})

test_that("pooled and sequential g-formula summaries", {
  q <- c(0.2, 0.5, 0.9, 0.4)
  expect_equal(g_formula_pooled(q)$psi, mean(q))
  expect_equal(g_formula_pooled(q, weights = c(1, 2, 1, 0))$psi, (0.2 + 1 + 0.9) / 4)
  expect_error(g_formula_pooled(q, weights = rep(0, 4)), "sum to zero")
  s <- g_formula_sequential(list(q, q + 0.1))
  expect_equal(s$psi, mean(q))
  expect_identical(s$T, 2L)
  expect_equal(g_formula_sequential(list(q))$psi, mean(q))
  expect_error(g_formula_sequential(list()), "empty")
  expect_error(g_formula_sequential(list(q, q[-1])), "differ in length at time 0")
})

test_that("morie_tlclust runs LTMLE and reports clustered inference", {
  set.seed(37)
  n <- 40
  y <- stats::rbinom(n, 1, 0.4)
  Q2 <- rep(0.4, n)
  Q1 <- rep(0.45, n)
  H <- rep(1, n)
  cl <- rep(1:8, each = 5)
  r <- morie_tlclust(list(Q1, Q2), list(H, H), list(NULL, y), cl)
  lt <- tlltmle_ltmle(list(Q1, Q2), list(H, H), list(NULL, y))
  ic <- lt$Q_star[[1]] - lt$psi
  expect_equal(mean(ic), 0, tolerance = 1e-15)
  expect_equal(r$psi, lt$psi, tolerance = 1e-15)
  expect_equal(r$se_clustered, cluster_variance(ic, cl)$se, tolerance = 1e-15)
  expect_equal(r$se_naive, naive_variance(ic), tolerance = 1e-15)
  expect_identical(r$n_clusters, 8L)
})
