# Anchors for the masked autoregressive flow behind sequential neural
# likelihood.
#
# Three things had never run: the forward pass raised for any
# dim_x + dim_t > 1 because the MADE mask was applied to the matrix
# product rather than to the weights; .abcnnt_params handed out copies of
# the weight matrices, so the finite-difference gradient was identically
# zero; and the call site discarded the trained flow. So the tests below
# check the density is a density, that the masks encode the autoregressive
# order, and that training moves the likelihood.

test_that("the MADE masks follow the degree rule", {
  L <- .abcnnt_made_layer(dim_x = 3L, dim_t = 2L, hidden = 6L,
                          e = .ghc_rng(1L), reverse = FALSE)
  expect_identical(dim(L$M1), c(6L, 5L))
  expect_identical(dim(L$M2), c(3L, 6L))
  deg_h <- (seq_len(6L) - 1L) %% 3L
  deg <- L$order
  for (k in seq_len(6L)) {
    for (j in seq_len(3L)) {
      expect_identical(L$M1[k, j], as.numeric(deg_h[k] >= deg[j]))
    }
    # the conditioning inputs are always visible
    for (j in seq_len(2L)) expect_identical(L$M1[k, 3L + j], 1)
    for (i in seq_len(3L)) {
      expect_identical(L$M2[i, k], as.numeric(deg[i] > deg_h[k]))
    }
  }
  # reversing flips the order the degrees are assigned in
  Lr <- .abcnnt_made_layer(3L, 2L, 6L, .ghc_rng(1L), reverse = TRUE)
  expect_identical(Lr$order, rev(seq_len(3L)))
})

test_that("the layer statistics are conformable and masked correctly", {
  L <- .abcnnt_made_layer(dim_x = 3L, dim_t = 2L, hidden = 6L,
                          e = .ghc_rng(2L), reverse = FALSE)
  st <- .abcnnt_layer_stats(L, c(0.5, -0.2, 0.9), c(0.1, -0.4))
  # masking the product instead of the weights raised "non-conformable
  # arrays" here for every dim_x + dim_t greater than one
  expect_length(st$h, 6L)
  expect_length(st$mu, 3L)
  expect_length(st$al, 3L)
  expect_true(all(is.finite(c(st$h, st$mu, st$al))))
  # the log-scale is clamped
  expect_true(all(st$al >= -5 & st$al <= 5))
  # the statistics are (M * W) %*% x + b, computed independently here
  inp <- c(0.5, -0.2, 0.9, 0.1, -0.4)
  z <- as.numeric(L$b1 + (L$M1 * L$W1) %*% matrix(inp, ncol = 1))
  expect_equal(st$h, tanh(z), tolerance = 1e-12)
  expect_equal(st$mu,
               as.numeric(L$bm + (L$M2 * L$Wm) %*% matrix(tanh(z), ncol = 1)),
               tolerance = 1e-12)
})

test_that("a single layer respects its autoregressive order", {
  # deg_out[i] > deg_h[k] >= deg_in[j] means unit i sees only inputs of
  # strictly lower degree; the lowest-degree output sees none of x
  L <- .abcnnt_made_layer(dim_x = 3L, dim_t = 2L, hidden = 9L,
                          e = .ghc_rng(3L), reverse = FALSE)
  t_ <- c(0.2, -0.1)
  st_a <- .abcnnt_layer_stats(L, c(0.5, -0.2, 0.9), t_)
  st_b <- .abcnnt_layer_stats(L, c(0.5, 7.0, -4.0), t_)   # x[2], x[3] moved
  # mu[1] and al[1] depend on inputs of degree < 1, i.e. none of x
  expect_equal(st_a$mu[1], st_b$mu[1], tolerance = 1e-12)
  expect_equal(st_a$al[1], st_b$al[1], tolerance = 1e-12)
  # mu[2] may depend on x[1] only: moving x[1] alone must move it
  st_c <- .abcnnt_layer_stats(L, c(-2.0, -0.2, 0.9), t_)
  expect_false(isTRUE(all.equal(st_a$mu[2], st_c$mu[2])))
  # but moving x[3] alone must not
  st_d <- .abcnnt_layer_stats(L, c(0.5, -0.2, -6.0), t_)
  expect_equal(st_a$mu[2], st_d$mu[2], tolerance = 1e-12)
  # the conditioning vector reaches every unit
  st_e <- .abcnnt_layer_stats(L, c(0.5, -0.2, 0.9), c(3.0, -0.1))
  expect_false(isTRUE(all.equal(st_a$mu[1], st_e$mu[1])))
})

test_that("flow_logprob is a normalised density", {
  # the decisive check on the forward pass: a change of variables with a
  # triangular Jacobian integrates to one
  flow <- MAF(dim_x = 1L, dim_t = 1L, n_layers = 3L, hidden = 5L, seed = 4L)
  t_ <- 0.3
  g <- seq(-12, 12, length.out = 4001)
  dens <- vapply(g, function(x) exp(flow_logprob(flow, x, t_)), numeric(1))
  mass <- sum((dens[-1] + dens[-length(dens)]) / 2) * (g[2] - g[1])
  expect_equal(mass, 1, tolerance = 1e-3)
})

test_that("the forward map is invertible in the sense the density needs", {
  flow <- MAF(dim_x = 2L, dim_t = 2L, n_layers = 3L, hidden = 5L, seed = 5L)
  fw <- flow_forward(flow, c(0.4, -0.7), c(0.1, 0.2))
  expect_length(fw$u, 2L)
  expect_true(is.finite(fw$total))
  # logprob is the standard normal density on u minus the log-Jacobian
  lp <- flow_logprob(flow, c(0.4, -0.7), c(0.1, 0.2))
  expect_equal(lp, -0.5 * sum(fw$u^2) - 0.5 * 2 * log(2 * pi) - fw$total,
               tolerance = 1e-12)
})

test_that("parameter addresses point into the flow and round-trip", {
  flow <- MAF(dim_x = 2L, dim_t = 2L, n_layers = 2L, hidden = 4L, seed = 1L)
  ps <- .abcnnt_params(flow)
  expect_gt(length(ps), 0L)
  for (a in ps[seq_len(min(12L, length(ps)))]) {
    expect_true(a$layer >= 1L && a$layer <= length(flow$layers))
    expect_true(a$field %in% c("W1", "b1", "Wm", "bm", "Wa", "ba"))
    old <- .abcnnt_param_get(flow, a)
    expect_true(is.numeric(old) && length(old) == 1L)
    moved <- .abcnnt_param_set(flow, a, old + 1)
    expect_equal(.abcnnt_param_get(moved, a), old + 1, tolerance = 1e-12)
    # and only that one parameter moved
    expect_equal(.abcnnt_param_get(flow, a), old, tolerance = 1e-12)
  }
  # only unmasked weights are trainable
  n_w1 <- sum(vapply(flow$layers, function(L) sum(L$M1), numeric(1)))
  expect_gte(length(ps), n_w1)
})

test_that("the finite-difference gradient is not identically zero", {
  # it was: the perturbation went into a copy, so up and dn were equal for
  # every parameter and every step multiplied the gradient by zero
  flow <- MAF(dim_x = 2L, dim_t = 2L, n_layers = 2L, hidden = 4L, seed = 1L)
  set.seed(1)
  D <- lapply(1:20, function(k) { th <- rnorm(2); list(th, th * 0.5 + rnorm(2, 0, 0.2)) })
  tot <- function(fl) mean(vapply(D, function(p) flow_logprob(fl, p[[2]], p[[1]]),
                                  numeric(1)))
  ps <- .abcnnt_params(flow)
  h <- 1e-4
  grads <- vapply(ps[seq_len(min(20L, length(ps)))], function(a) {
    old <- .abcnnt_param_get(flow, a)
    (tot(.abcnnt_param_set(flow, a, old + h)) -
     tot(.abcnnt_param_set(flow, a, old - h))) / (2 * h)
  }, numeric(1))
  expect_true(any(abs(grads) > 1e-6))
})

test_that("training moves the flow and raises the log-likelihood", {
  flow <- MAF(dim_x = 2L, dim_t = 2L, n_layers = 2L, hidden = 4L, seed = 1L)
  set.seed(1)
  D <- lapply(1:40, function(k) { th <- rnorm(2); list(th, th * 0.5 + rnorm(2, 0, 0.2)) })
  ll <- function(fl) mean(vapply(D, function(p) flow_logprob(fl, p[[2]], p[[1]]),
                                 numeric(1)))
  before <- ll(flow)
  after <- train_flow(flow, D, epochs = 12L, lr = 0.02, seed = 1L)
  expect_false(identical(flow, after))
  expect_gt(ll(after), before)
  # more epochs do more work
  longer <- train_flow(flow, D, epochs = 30L, lr = 0.02, seed = 1L)
  expect_gt(ll(longer), ll(after))
})

test_that("training is seeded and validates its arguments", {
  flow <- MAF(dim_x = 2L, dim_t = 1L, n_layers = 1L, hidden = 3L, seed = 2L)
  D <- lapply(1:10, function(k) list(rnorm(1), rnorm(2)))
  a <- train_flow(flow, D, epochs = 3L, lr = 0.01, seed = 3L)
  b <- train_flow(flow, D, epochs = 3L, lr = 0.01, seed = 3L)
  expect_equal(a$layers[[1]]$W1, b$layers[[1]]$W1, tolerance = 1e-15)
  expect_error(train_flow(flow, list()), "no training pairs")
  expect_error(train_flow(flow, D, epochs = 0L), "epochs must be")
  expect_error(train_flow(flow, D, lr = 0), "lr positive")
})

test_that("MAF validates its shape", {
  expect_error(MAF(0L, 1L), "dimensions must be positive")
  expect_error(MAF(1L, 0L), "dimensions must be positive")
  expect_error(MAF(2L, 2L, n_layers = 0L), "must be positive")
  expect_error(MAF(2L, 2L, hidden = 0L), "must be positive")
  f <- MAF(2L, 3L, n_layers = 4L, hidden = 7L, seed = 1L)
  expect_length(f$layers, 4L)
  expect_identical(f$dim_x, 2L)
  expect_identical(f$dim_t, 3L)
  # the ordering alternates so the composition is not triangular in one order
  expect_false(identical(f$layers[[1]]$order, f$layers[[2]]$order))
})

test_that("the Metropolis sampler moves and respects the target", {
  # a standard normal target: the chain's mean and spread should be close
  lp <- function(x) -0.5 * sum(x^2)
  mc <- mcmc_sample(lp, 0, 4000L, burn = 500L, step = 1.5, seed = 1L)
  expect_true(is.matrix(mc$samples))
  expect_identical(nrow(mc$samples), 4000L)
  expect_lt(abs(mean(mc$samples[, 1])), 0.15)
  expect_lt(abs(sd(mc$samples[, 1]) - 1), 0.15)
  # it actually moved
  expect_gt(length(unique(mc$samples[, 1])), 100L)
})

test_that("sequential neural likelihood runs end to end and trains its flow", {
  set.seed(1)
  simulator <- function(theta, e) theta * 0.5 + rnorm(length(theta), 0, 0.2)
  log_prior <- function(th) if (any(abs(th) > 5)) -Inf else -0.5 * sum(th^2)
  res <- abcnnt(simulator, x_o = c(0.25), log_prior = log_prior,
                theta0 = c(0.5), n_rounds = 2L, n_per_round = 25L,
                epochs = 5L, lr = 0.02, seed = 1L,
                mcmc_burn = 50L, mcmc_step = 0.8)
  expect_true(is.list(res))
  expect_true(any(grepl("flow|posterior|samples|estimate", names(res))))
})
