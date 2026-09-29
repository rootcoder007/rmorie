# Coverage for the Geron wave-4d helpers: closed forms recomputed, the
# chain-matrix DP against brute-force parenthesisation, digamma against
# base::digamma, the t-SNE bandwidth search against its perplexity target,
# the VAE gradients against central differences, and the symbolic parser
# against R's own evaluator.

test_that("box IoU, layer norms and parameter counts", {
  expect_equal(morie_geron_box_iou(c(0, 0, 2, 2), c(1, 1, 3, 4)), 1 / (4 + 6 - 1), tolerance = 1e-12)
  expect_equal(morie_geron_box_iou(c(0, 0, 1, 1), c(2, 2, 3, 3)), 0)
  expect_equal(morie_geron_box_iou(c(0, 0, 1, 1), c(0, 0, 1, 1)), 1)
  g <- list(matrix(1:4, 2), c(3, 4), array(1, c(2, 2, 2)))
  expect_equal(morie_geron_layer_norms(g), c(sqrt(30), 5, sqrt(8)), tolerance = 1e-12)
  # Q, K, V, O without bias, a biased d -> d_ff -> d feedforward, two
  # layer-norm scale/shift pairs
  expect_identical(morie_geron_encoder_params(8, 32, 3), as.integer(3 * (4 * 64 + 8 * 32 + 32 + 32 * 8 + 8 + 4 * 8)))
  expect_identical(morie_geron_separable_params(3, 16, 32), as.integer(9 * 16 + 16 * 32))
})

test_that("digamma matches base::digamma", {
  x <- c(0.1, 0.5, 1, 2.5, 7, 40)
  # recurrence up to x >= 6, then the asymptotic series cut after the
  # x^-8 term: the first omitted term, 1 / (132 x^10), is 1.3e-10 at x = 6
  expect_lt(max(abs(morie_geron_digamma(x) - digamma(x))), 1 / (132 * 6^10))
  expect_lt(abs(morie_geron_digamma(1) + 0.5772156649015329), 1 / (132 * 6^10))
})

test_that("matrix-chain order DP equals brute-force parenthesisation", {
  dims <- c(10, 30, 5, 60, 8, 20)
  best <- function(i, j) {
    if (i == j) return(0)
    min(vapply(i:(j - 1), function(k) best(i, k) + best(k + 1, j) + dims[i] * dims[k + 1] * dims[j + 1], 1))
  }
  r <- morie_geron_matmul_order(dims)
  expect_equal(r$cost, best(1, 5))
  expect_equal(morie_geron_matmul_order(c(10, 30, 5, 60))$cost, 4500)
  expect_equal(morie_geron_matmul_order(c(4, 5))$cost, 0)
})

test_that("XLNet permutation masks follow the factorisation order", {
  perm <- c(2, 0, 3, 1)
  r <- morie_geron_permutation_masks(perm)
  rk <- order(perm) - 1
  expect_equal(r$content, outer(rk, rk, ">=") * 1)
  expect_equal(r$query, outer(rk, rk, ">") * 1)
  expect_equal(diag(r$query), rep(0, 4))
})

test_that("vector quantisation picks the nearest codebook row", {
  z <- rbind(c(0.1, 0.2), c(2, 2.1), c(-1, 0.9))
  cb <- rbind(c(0, 0), c(2, 2), c(-1, 1))
  r <- morie_geron_quantize(z, cb)
  d2 <- as.matrix(stats::dist(rbind(z, cb)))[1:3, 4:6]^2
  expect_equal(r$indices, unname(apply(d2, 1, which.min) - 1L))
  expect_equal(r$z_q, cb[unname(apply(d2, 1, which.min)), ])
})

test_that("t-SNE conditional P hits the perplexity target row by row", {
  X <- cbind(c(0, 1, 2, 4, 7, 11), c(0, 0.5, 2, 1, 3, 1))
  D2 <- as.matrix(stats::dist(X))^2
  r <- morie_geron_conditional_p(D2, perplexity = 3)
  expect_equal(rowSums(r$P), rep(1, 6), tolerance = 1e-12)
  expect_equal(diag(r$P), rep(0, 6))
  for (i in 1:6) {
    p <- r$P[i, -i]
    expect_equal(p, unname(exp(-D2[i, -i] * r$betas[i]) / sum(exp(-D2[i, -i] * r$betas[i]))), tolerance = 1e-12)
    expect_lt(abs(-sum(p * log(p)) - log(3)), 1e-5)
  }
})

test_that("UMAP a, b is the SSE minimiser over the documented grid", {
  r <- morie_geron_fit_ab(0.1, 1)
  d <- seq(0, 3, length.out = 300)
  target <- ifelse(d <= 0.1, 1, exp(-(d - 0.1)))
  sse <- function(a, b) sum((1 / (1 + a * pmax(d, 1e-12)^(2 * b)) - target)^2)
  expect_equal(r$sse, sse(r$a, r$b), tolerance = 1e-12)
  grid <- expand.grid(a = exp(seq(-3, 3, length.out = 121)), b = seq(0.25, 3, length.out = 56))
  expect_equal(r$sse, min(mapply(sse, grid$a, grid$b)), tolerance = 1e-12)
})

test_that("T5 span corruption round-trips through t5_restore", {
  toks <- paste0("w", 1:20)
  for (sd in c(0, 3, 11)) {
    r <- morie_geron_span_corrupt(toks, noise_density = 0.2, mean_span = 2, seed = sd)
    expect_identical(morie_geron_t5_restore(r$inputs, r$target), toks)
    nsent <- sum(startsWith(r$inputs, "<extra_id_"))
    expect_identical(nsent, length(r$spans))
    expect_identical(tail(r$target, 1), sprintf("<extra_id_%d>", nsent))
    lens <- vapply(r$spans, function(s) s[2], 1)
    expect_lte(sum(lens), round(20 * 0.2))
    starts <- vapply(r$spans, function(s) s[1], 1)
    expect_false(is.unsorted(starts))
  }
  expect_identical(morie_geron_t5_restore(c("a", "<extra_id_0>", "d"), c("<extra_id_0>", "b", "c", "<extra_id_1>")), c("a", "b", "c", "d"))
})

test_that("symbolic parse, evaluate and print agree with R's evaluator", {
  srcs <- c("x^2 + sin(x) * 3", "-(x - 2) / (y + 1)", "exp(-x) * log(y + 4) - 2^-1", "sqrt(x * y) + tanh(x - y)")
  env <- list(x = 1.3, y = 0.7)
  for (s in srcs) {
    t <- morie_geron_symd_parse(s)
    ref <- eval(parse(text = s), env)
    expect_equal(morie_geron_symd_evaluate(t, env), ref, tolerance = 1e-12)
    back <- morie_geron_symd_to_string(t)
    expect_equal(morie_geron_symd_evaluate(morie_geron_symd_parse(back), env), ref, tolerance = 1e-12)
  }
  expect_identical(morie_geron_symd_to_string(morie_geron_symd_parse("(a + b) * c")), "(a + b) * c")
  # printing must not re-associate: each tree survives parse(to_string(.))
  for (s in c("a - (b - c)", "a - (b + c)", "-(a + b) * c", "(-a)^2", "(a^b)^c", "a / (b / c)", "-(a * b)")) {
    t <- morie_geron_symd_parse(s)
    expect_identical(morie_geron_symd_parse(morie_geron_symd_to_string(t)), t)
    e2 <- list(a = 1.7, b = 0.6, c = 2.3)
    expect_equal(morie_geron_symd_evaluate(t, e2), eval(parse(text = s), e2), tolerance = 1e-12)
  }
  expect_identical(morie_geron_symd_to_string(morie_geron_symd_parse("2.5 * x")), "2.5 * x")
  expect_error(morie_geron_symd_parse("foo(x)"), "unknown function")
  expect_error(morie_geron_symd_parse("x + "), "unexpected end")
  expect_error(morie_geron_symd_parse("x $ y"), "unexpected character")
  expect_error(morie_geron_symd_parse("(x"), "unexpected token")
})

test_that("trace records each op's shapes and composes the forward pass", {
  X <- matrix(c(1, -2, 0.5, 3, 1, -1), 3)
  W <- matrix(c(0.2, -0.4, 1, 0.3, 0.1, 0.6), 2)
  model <- list(list(kind = "linear", param = W), list(kind = "bias", param = c(0.1, -0.2, 0.3)),
                list(kind = "relu"), function(z) z * 2, list(kind = "tanh"), list(kind = "sigmoid"))
  r <- morie_geron_trace(model, X)
  ref <- stats::plogis(tanh(2 * pmax(sweep(X %*% W, 2, c(0.1, -0.2, 0.3), "+"), 0)))
  expect_equal(r$output, ref, tolerance = 1e-12)
  expect_identical(r$graph[[1]]$in_shape, c(3L, 2L))
  expect_identical(r$graph[[1]]$out_shape, c(3L, 3L))
  expect_identical(r$graph[[4]]$kind, "callable")
  expect_error(morie_geron_trace(list(list(kind = "conv")), X), "unknown op")
})

test_that("VAE loss is recon + beta KL and its gradients match central differences", {
  X <- rbind(c(0.5, -1, 2), c(1.5, 0.2, -0.3), c(-0.4, 0.8, 1.1), c(0.9, -0.6, 0.4))
  set.seed(2)
  params <- list(matrix(stats::rnorm(6, sd = 0.4), 3), c(0.1, -0.1), matrix(stats::rnorm(6, sd = 0.4), 3), c(0, 0.2),
                 matrix(stats::rnorm(6, sd = 0.4), 2), c(0.05, 0, -0.05))
  eps <- matrix(stats::rnorm(8), 4)
  r <- morie_geron_vae_loss_and_grads(X, params, eps, beta = 0.7)
  mu <- sweep(X %*% params[[1]], 2, params[[2]], "+")
  lv <- sweep(X %*% params[[3]], 2, params[[4]], "+")
  xh <- sweep((mu + eps * exp(lv / 2)) %*% params[[5]], 2, params[[6]], "+")
  expect_equal(r$recon, mean((xh - X)^2), tolerance = 1e-12)
  expect_equal(r$kl, sum(0.5 * (mu^2 + exp(lv) - 1 - lv)) / 4, tolerance = 1e-12)
  expect_equal(r$loss, r$recon + 0.7 * r$kl, tolerance = 1e-12)
  f <- function(p) morie_geron_vae_loss_and_grads(X, p, eps, beta = 0.7)$loss
  h <- 1e-6
  for (k in seq_along(params)) {
    num <- vapply(seq_along(params[[k]]), function(e) {
      pp <- params
      pm <- params
      pp[[k]][e] <- pp[[k]][e] + h
      pm[[k]][e] <- pm[[k]][e] - h
      (f(pp) - f(pm)) / (2 * h)
    }, 1)
    # central differences carry O(h^2) truncation and O(eps/h) rounding
    expect_equal(as.numeric(r$grads[[k]]), num, tolerance = 1e-7)
  }
})
