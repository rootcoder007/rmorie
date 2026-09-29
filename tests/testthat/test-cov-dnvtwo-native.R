# Coverage tests for R/dnvtwo_native.R (Oquab et al. 2024): embedding
# deduplication, retrieval augmentation, the KoLeo regulariser,
# Sinkhorn-Knopp centring and the self-distillation loss.

cosr <- function(a, b) sum(a * b) / sqrt(sum(a^2) * sum(b^2))

test_that("deduplication by cosine similarity", {
  E <- rbind(c(1, 0, 0), c(2, 0.001, 0), c(0, 1, 0), c(0.7, 0.7, 0), c(0, 3, 0.0001))
  d <- deduplicate(E)
  expect_equal(d$keep, c(1L, 3L, 4L))
  expect_equal(d$dropped, list(c(2L, 1L), c(5L, 3L)))
  expect_equal(d$n_after, 3L)
  expect_equal(deduplicate(E, threshold = 0.5)$keep, c(1L, 3L))
  expect_equal(deduplicate(NULL)$n_before, 0L)
})

test_that("retrieval augmentation takes the nearest uncurated items per query", {
  C <- rbind(c(1, 0), c(0, 1))
  U <- rbind(c(0.9, 0.1), c(0.6, 0.8), c(-1, 0.2), c(0.1, 1), c(1, 1))
  r <- retrieve_augment(C, U, per_query = 2)
  top <- function(q) order(-apply(U, 1, cosr, q))[1:2]
  expect_equal(r$per_query, list(`0` = top(c(1, 0)), `1` = top(c(0, 1))))
  expect_equal(r$retrieved, sort(unique(c(top(c(1, 0)), top(c(0, 1))))))
  expect_equal(r$n_added, length(r$retrieved))
  expect_equal(r$max_times_retrieved, max(table(c(top(c(1, 0)), top(c(0, 1))))))
  m <- retrieve_augment(C, U, per_query = 3, min_similarity = 0.9)
  expect_true(all(unlist(m$per_query) %in% which(apply(U, 1, cosr, c(1, 0)) >= 0.9 | apply(U, 1, cosr, c(0, 1)) >= 0.9)))
  expect_equal(retrieve_augment(C, NULL)$n_added, 0L)
  expect_error(retrieve_augment(NULL, U), "curated corpus is empty")
})

test_that("KoLeo is minus the mean log nearest-neighbour distance", {
  X <- rbind(c(1, 0), c(0, 2), c(-3, 0.1), c(0.5, 0.5))
  N <- X / sqrt(rowSums(X^2))
  D <- unname(as.matrix(dist(N)))
  diag(D) <- Inf
  k <- koleo(X)
  expect_equal(k$nearest_distances, apply(D, 1, min), tolerance = 1e-12)
  expect_equal(k$loss, -mean(log(apply(D, 1, min))), tolerance = 1e-12)
  expect_equal(koleo(rbind(c(1, 0), c(2, 0)))$loss, -log(1e-12))
  expect_error(koleo(c(1, 2)), "at least 2 features")
})

test_that("Sinkhorn-Knopp alternates column and row normalisation", {
  S <- rbind(c(0.1, 0.3, -0.2), c(0.5, 0.0, 0.1), c(-0.1, 0.2, 0.4), c(0.3, 0.3, 0.3))
  Q <- exp(S / 0.1)
  Q <- Q / sum(Q)
  for (i in 1:3) {
    Q <- sweep(Q, 2, colSums(Q) * 3, "/")
    Q <- Q / (rowSums(Q) * 4)
  }
  r <- sinkhorn_knopp(S, iterations = 3, epsilon = 0.1)
  expect_equal(r$Q, Q * 4, tolerance = 1e-12)
  expect_equal(r$row_sums, rep(1, 4), tolerance = 1e-12)
  expect_equal(colSums(sinkhorn_knopp(S, iterations = 200, epsilon = 0.1)$Q), rep(4 / 3, 3), tolerance = 1e-9)
  expect_equal(dim(sinkhorn_knopp(NULL)$Q), c(0L, 0L))
})

test_that("self-distillation cross-entropy with a sharpened teacher", {
  s <- c(0.2, -0.1, 0.5)
  t <- c(0.1, 0.4, -0.3)
  sm <- function(x, T) exp(x / T) / sum(exp(x / T))
  r <- self_distillation_loss(s, t)
  expect_equal(r$student, sm(s, 0.1), tolerance = 1e-12)
  expect_equal(r$teacher, sm(t, 0.04), tolerance = 1e-12)
  expect_equal(r$loss, -sum(sm(t, 0.04) * log(sm(s, 0.1))), tolerance = 1e-12)
  expect_equal(r$teacher_entropy, -sum(sm(t, 0.04) * log(sm(t, 0.04))), tolerance = 1e-12)
  expect_equal(self_distillation_loss(s, t, patch_level = TRUE)$level, "patch")
  expect_identical(dinov2, self_distillation_loss)
  expect_identical(dinov2_repr, self_distillation_loss)
  expect_same_function(morie_dnvtwo$koleo, koleo)
  expect_match(morie_dnvtwo$cheatsheet(), "KoLeo")
  expect_error(self_distillation_loss(s, t[-1]), "differ in width")
  expect_error(self_distillation_loss(s, t, temperature_t = 0), "temperatures must be positive")
})
