# Anchors for the property-inference module.
#
# The file header states that this arm mirrors Python morie.fn.propinf
# exactly, so the generator is anchored on values taken from that arm
# rather than from this one:
#
#   st = 1
#   st = (1103515245 * st + 12345) % (1 << 31);  st / float(1 << 31)
#
# and the representations are anchored on the invariance property the
# attack of Ganju et al. (2018) rests on: permuting the units of a
# hidden layer changes none of the network's outputs, so a
# representation that is not invariant to that permutation is feeding
# the meta-classifier an artefact of unit ordering.

# a small net with hand-chosen weights; row metrics |sum(W[i, ])| are
# 5, 1, 9, deliberately not monotone in row order
fixed_net <- function() {
  list(
    list(W = matrix(c(2, 3,
                      1, 0,
                      4, 5), nrow = 3, byrow = TRUE), b = c(10, 20, 30)),
    list(W = matrix(c(0.5, -0.5, 0.25), nrow = 1), b = 7)
  )
}

test_that("the LCG reproduces the Python arm's stream exactly", {
  e <- .propinf_rng(1L)
  got <- replicate(5, .propinf_rng_next(e))
  expect_equal(got,
               c(0.513870078139, 0.175741303246, 0.308651516214,
                 0.534533886705, 0.947627925314),
               tolerance = 1e-11)
  # the uniform is st / 2^31; dividing by 2^31 - 1 agrees only to nine digits
  expect_false(isTRUE(all.equal(got[1], 0.513870078378, tolerance = 1e-12)))
  # lcg_draw is the same step on the same stream
  e2 <- .propinf_rng(1L)
  expect_equal(replicate(5, .propinf_lcg_draw(e2)), got, tolerance = 1e-15)
})

test_that("the seed is reduced by keeping the low 31 bits", {
  # int(seed) & 0x7FFFFFFF or 1
  expect_identical(.propinf_rng(1L)$st, 1L)
  expect_identical(.propinf_rng(0L)$st, 1L)          # 0 is replaced by 1
  expect_identical(.propinf_rng(-1L)$st, 2147483647L)
  expect_identical(.propinf_rng(2147483647L)$st, 2147483647L)
  expect_identical(.propinf_rng(2L)$st, 2L)
  # every state stays a valid 31-bit seed
  for (s in c(-99L, -1L, 0L, 1L, 12345L, 2147483646L)) {
    st <- .propinf_rng(s)$st
    expect_true(st >= 1 && st <= 2147483647)
  }
})

test_that("the normal draws are Box-Muller, not a half-normal", {
  # cos(2 * pi * u2) needs the second uniform: with a constant cosine
  # every draw comes back non-negative, which is what this catches
  e <- .propinf_rng(7L)
  draws <- replicate(4000, .propinf_normal(e))
  expect_true(any(draws < 0))
  expect_true(any(draws > 0))
  expect_lt(abs(mean(draws)), 0.1)
  expect_lt(abs(sd(draws) - 1), 0.1)
  # scale multiplies the draw
  e1 <- .propinf_rng(3L); e2 <- .propinf_rng(3L)
  expect_equal(.propinf_normal(e1, scale = 2.5),
               2.5 * .propinf_normal(e2), tolerance = 1e-12)
  # the lcg variant takes its angle from the stream too
  e3 <- .propinf_rng(11L)
  nl <- replicate(2000, .propinf_normal_lcg(e3))
  expect_true(any(nl < 0))
  expect_lt(abs(mean(nl)), 0.15)
})

test_that("the activations are the closed forms, and stable at the tails", {
  expect_identical(.propinf_relu(2.5), 2.5)
  expect_identical(.propinf_relu(-2.5), 0)
  expect_identical(.propinf_relu(0), 0)
  expect_equal(.propinf_sigmoid(0), 0.5)
  expect_equal(.propinf_sigmoid(2), 1 / (1 + exp(-2)), tolerance = 1e-15)
  expect_equal(.propinf_sigmoid(-2), 1 - .propinf_sigmoid(2), tolerance = 1e-14)
  # the two-branch form exists to avoid exp() overflowing
  expect_equal(.propinf_sigmoid(800), 1)
  expect_equal(.propinf_sigmoid(-800), 0)
  expect_false(is.nan(.propinf_sigmoid(-800)))
})

test_that("the forward pass computes what the weights say it does", {
  net <- list(
    list(W = matrix(c(1, 0, 0, 1), nrow = 2, byrow = TRUE), b = c(0, 0)),
    list(W = matrix(c(1, 1), nrow = 1), b = 0)
  )
  fp <- .propinf_forward(net, c(2, -3))
  expect_equal(fp$pre[[1]], c(2, -3))          # identity layer
  expect_equal(fp$acts[[2]], c(2, 0))          # ReLU clips the -3
  expect_equal(fp$pre[[2]], 2)                 # 2 + 0
  expect_equal(fp$acts[[3]], 1 / (1 + exp(-2)), tolerance = 1e-15)
  # the public predictor returns that last activation, per row
  X <- matrix(c(2, -3, 0, 0), nrow = 2, byrow = TRUE)
  expect_equal(morie_propinf_fcnn_predict(net, X),
               c(1 / (1 + exp(-2)), 0.5), tolerance = 1e-15)
})

test_that("the flat representation pairs each row of W with its own bias", {
  f <- morie_propinf_flat_representation(fixed_net())
  # row, then bias, layer by layer
  expect_identical(f, c(2, 3, 10, 1, 0, 20, 4, 5, 30, 0.5, -0.5, 0.25, 7))
  # walking matrix elements instead of rows ran the bias index past its
  # length and produced NAs
  expect_false(anyNA(f))
  expect_length(f, 3 * (2 + 1) + 1 * (3 + 1))
})

test_that("the set representation is one (weights, bias) vector per unit", {
  s <- morie_propinf_set_representation(fixed_net())
  expect_length(s, 2L)                  # one entry per layer
  expect_length(s[[1]], 3L)             # three units in the hidden layer
  expect_length(s[[2]], 1L)
  expect_identical(s[[1]][[1]], c(2, 3, 10))
  expect_identical(s[[1]][[3]], c(4, 5, 30))
  expect_identical(s[[2]][[1]], c(0.5, -0.5, 0.25, 7))
  expect_false(anyNA(unlist(s)))
})

test_that("the sorted representation orders units by descending metric", {
  # metrics are 5, 1, 9, so the canonical order of the rows is 3, 1, 2
  expect_identical(
    morie_propinf_sorted_representation(fixed_net()),
    c(4, 5, 30, 2, 3, 10, 1, 0, 20, 0.25, 0.5, -0.5, 7)
  )
  expect_identical(
    vapply(1:3, function(i) .propinf_node_metric(fixed_net()[[1]], i), numeric(1)),
    c(5, 1, 9)
  )
  # a custom metric is honoured: reverse the ordering
  rev_metric <- function(layer, i) -abs(sum(layer$W[i, ]))
  srt <- morie_propinf_sorted_representation(fixed_net(), metric = rev_metric)
  expect_identical(srt[1:3], c(1, 0, 20))     # the smallest metric first
})

test_that("the public representations and their internal twins agree", {
  net <- fixed_net()
  expect_identical(morie_propinf_flat_representation(net),
                   .propinf_flat_representation_internal(net))
  expect_identical(morie_propinf_set_representation(net),
                   .propinf_set_representation_internal(net))
  expect_identical(morie_propinf_sorted_representation(net),
                   .propinf_sorted_representation_internal(net))
  expect_identical(morie_propinf_permute_hidden_layer(net, 0L, c(2L, 0L, 1L)),
                   .propinf_permute_hidden_layer_internal(net, 0L, c(2L, 0L, 1L)))
})

test_that("permuting a hidden layer leaves the network's function unchanged", {
  net <- fixed_net()
  X <- matrix(c(1, -2, 0.5, 3, 2, -1, 0, 0), nrow = 4, byrow = TRUE)
  before <- morie_propinf_fcnn_predict(net, X)
  for (sigma in list(c(2L, 0L, 1L), c(1L, 2L, 0L), c(2L, 1L, 0L), c(0L, 1L, 2L))) {
    perm <- morie_propinf_permute_hidden_layer(net, 0L, sigma)
    expect_equal(morie_propinf_fcnn_predict(perm, X), before, tolerance = 1e-12)
  }
})

test_that("only the permutation-invariant representations survive a permutation", {
  net <- fixed_net()
  perm <- morie_propinf_permute_hidden_layer(net, 0L, c(2L, 0L, 1L))
  # the baseline moves -- this is the weakness the paper reports
  expect_false(identical(morie_propinf_flat_representation(net),
                         morie_propinf_flat_representation(perm)))
  # Algorithm 1 canonicalises, so it does not
  expect_identical(morie_propinf_sorted_representation(net),
                   morie_propinf_sorted_representation(perm))
  # Algorithm 2 is a set: same members, order allowed to differ
  a <- morie_propinf_set_representation(net)[[1]]
  b <- morie_propinf_set_representation(perm)[[1]]
  key <- function(x) sort(vapply(x, function(v) paste(v, collapse = ","), character(1)))
  expect_identical(key(a), key(b))
})

test_that("permutation rejects indices that are not a permutation", {
  net <- fixed_net()
  expect_error(morie_propinf_permute_hidden_layer(net, 0L, c(0L, 1L)), "not a permutation")
  expect_error(morie_propinf_permute_hidden_layer(net, 0L, c(0L, 1L, 1L)), "not a permutation")
  expect_error(morie_propinf_permute_hidden_layer(net, 0L, c(1L, 2L, 3L)), "not a permutation")
  expect_error(morie_propinf_permute_hidden_layer(net, -1L, c(0L, 1L, 2L)), "must index")
  expect_error(morie_propinf_permute_hidden_layer(net, 5L, c(0L, 1L, 2L)), "must index")
})

test_that("training separates a linearly separable problem and is reproducible", {
  set.seed(4)
  n <- 60L
  X <- cbind(runif(n, -1, 1), runif(n, -1, 1))
  y <- as.numeric(X[, 1] + X[, 2] > 0)
  net <- morie_propinf_train_fcnn(X, y, hidden = c(6L), epochs = 120L,
                                  lr = 0.3, batch_size = 8L, seed = 1L)
  acc <- mean((morie_propinf_fcnn_predict(net, X) >= 0.5) == (y == 1))
  expect_gt(acc, 0.85)
  # an untrained net of the same shape does no better than chance, so the
  # accuracy above is the training, not the architecture
  raw <- .propinf_init_net(2L, 6L, .propinf_rng(2L))
  expect_lt(mean((morie_propinf_fcnn_predict(raw, X) >= 0.5) == (y == 1)), 0.85)
  # the generator is seeded, so the same seed gives the same weights
  again <- morie_propinf_train_fcnn(X, y, hidden = c(6L), epochs = 120L,
                                    lr = 0.3, batch_size = 8L, seed = 1L)
  expect_equal(net[[1]]$W, again[[1]]$W, tolerance = 1e-15)
  other <- morie_propinf_train_fcnn(X, y, hidden = c(6L), epochs = 120L,
                                    lr = 0.3, batch_size = 8L, seed = 99L)
  expect_false(isTRUE(all.equal(net[[1]]$W, other[[1]]$W)))
})

test_that("training rejects malformed input", {
  X <- matrix(runif(20), nrow = 10)
  y <- rep(c(0, 1), 5)
  expect_error(morie_propinf_train_fcnn(X, y[1:5]), "different lengths")
  expect_error(morie_propinf_train_fcnn(X, rep(2, 10)), "y must be 0/1")
  expect_error(morie_propinf_train_fcnn(X, y, hidden = integer(0)), "hidden layer")
  expect_error(morie_propinf_train_fcnn(X, y, epochs = 0L), "epochs must be")
  expect_error(morie_propinf_train_fcnn(X, y, lr = 0), "lr must be positive")
  expect_error(morie_propinf_train_fcnn(X, y, batch_size = 0L), "batch_size must be")
})

# shadow models carrying a property the meta-classifier can find: nets
# trained on data where the property holds against nets where it does not
shadow_pair <- function(k = 6L) {
  set.seed(11)
  nets <- list(); labs <- numeric(0)
  for (i in seq_len(k)) {
    prop <- i %% 2L                     # the property being inferred
    n <- 40L
    X <- cbind(runif(n, -1, 1), runif(n, -1, 1))
    y <- if (prop == 1L) as.numeric(X[, 1] > 0) else as.numeric(X[, 2] > 0)
    nets[[i]] <- morie_propinf_train_fcnn(X, y, hidden = c(4L), epochs = 40L,
                                          lr = 0.3, batch_size = 8L,
                                          seed = as.integer(i))
    labs <- c(labs, prop)
  }
  list(nets = nets, labs = labs)
}

test_that("property inference runs on all three representations", {
  sp <- shadow_pair()
  for (repr in c("baseline", "sorting", "set")) {
    r <- morie_propinf_property_inference(sp$nets, sp$labs, representation = repr,
                                          epochs = 20L, seed = 3L)
    expect_identical(r$representation, repr)
    expect_identical(r$n_shadow, 6L)
    expect_identical(r$n_target, 6L)
    expect_length(r$score, 6L)
    expect_true(all(r$prediction %in% c(0L, 1L)))
    expect_true(all(r$score >= 0 & r$score <= 1))
    expect_gte(r$train_accuracy, 0.5)
    expect_match(r$method, "Ganju")
    # context is reported for the set route only
    if (repr == "set") expect_identical(r$context, "paired") else expect_null(r$context)
  }
})

test_that("property inference scores held-out targets and reports accuracy", {
  sp <- shadow_pair()
  r <- morie_propinf_property_inference(sp$nets, sp$labs,
                                        target_models = sp$nets[1:2],
                                        target_labels = sp$labs[1:2],
                                        representation = "set",
                                        epochs = 20L, seed = 3L)
  expect_length(r$score, 2L)
  expect_identical(r$n_target, 2L)
  expect_true(r$accuracy >= 0 && r$accuracy <= 1)
  expect_identical(r$estimate, r$accuracy)
  # with no labels the estimate falls back to the training accuracy
  r2 <- morie_propinf_property_inference(sp$nets, sp$labs, representation = "set",
                                         epochs = 20L, seed = 3L)
  expect_null(r2$accuracy)
  expect_identical(r2$estimate, r2$train_accuracy)
})

test_that("property inference rejects malformed input", {
  sp <- shadow_pair()
  expect_error(
    morie_propinf_property_inference(sp$nets, sp$labs, representation = "nope"),
    "representation must be one of")
  expect_error(
    morie_propinf_property_inference(sp$nets, sp$labs[1:3]),
    "one label per shadow model")
  expect_error(
    morie_propinf_property_inference(sp$nets[1:3], sp$labs[1:3]),
    "at least four shadow models")
  expect_error(
    morie_propinf_property_inference(sp$nets, rep(1, 6)),
    "must contain both classes")
  expect_error(
    morie_propinf_property_inference(sp$nets, rep(c(0, 2), 3)),
    "must be 0/1")
  expect_error(
    morie_propinf_property_inference(sp$nets, sp$labs, context = "sideways"),
    "context must be one of")
  expect_error(
    morie_propinf_property_inference(sp$nets, sp$labs, repr_dim = 0L),
    "repr_dim must be")
  expect_error(
    morie_propinf_property_inference(sp$nets, sp$labs, epochs = 0L),
    "epochs must be")
  # a shadow model with a different architecture
  odd <- sp$nets
  odd[[2]] <- morie_propinf_train_fcnn(
    cbind(runif(30, -1, 1), runif(30, -1, 1)), rep(c(0, 1), 15),
    hidden = c(5L), epochs = 5L, seed = 1L)
  expect_error(morie_propinf_property_inference(odd, sp$labs),
               "share one architecture")
})

test_that("standardisation centres and scales the feature matrix", {
  feats <- list(c(1, 10), c(3, 20), c(5, 30))
  st <- .propinf_standardise(feats)
  expect_equal(st$mu, c(3, 20))
  expect_equal(vapply(st$feats, function(f) f[1], numeric(1)),
               c(-1, 0, 1) * (2 / st$sd[1]), tolerance = 1e-12)
  # applying the stored centre and scale to a new row reproduces the same map
  expect_equal(.propinf_apply_standardise(c(3, 20), st$mu, st$sd), c(0, 0))
  # a constant column must not divide by zero
  st2 <- .propinf_standardise(list(c(1, 5), c(1, 7)))
  expect_false(anyNA(unlist(st2$feats)))
  expect_false(any(is.infinite(unlist(st2$feats))))
})

test_that("rows() accepts a matrix or a data frame and keeps row order", {
  X <- matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE)
  expect_identical(.propinf_rows(X), list(c(1, 2), c(3, 4)))
  expect_identical(.propinf_rows(as.data.frame(X)), list(c(1, 2), c(3, 4)))
})

test_that("mlp_backward returns the gradients it accumulated", {
  net <- .propinf_mlp_init(c(3L, 4L, 2L), .propinf_rng(1L))
  fp <- .propinf_mlp_forward(net, c(1, 2, 3), final = "linear")
  bw <- .propinf_mlp_backward(net, fp$acts, fp$pre, c(1, 1),
                              .propinf_zero_like(net), final = "linear")
  expect_named(bw, c("delta", "grads"))
  expect_length(bw$delta, 3L)                     # gradient w.r.t. the input
  # the gradients travel back in the return value: R hands the function a
  # copy of grads, so accumulating in place lost every one of them and the
  # meta-classifiers trained on zeros
  expect_true(any(unlist(lapply(bw$grads, function(L) c(L$W, L$b))) != 0))
  expect_identical(length(bw$grads), length(net))
  for (t in seq_along(net)) {
    expect_identical(dim(bw$grads[[t]]$W), dim(net[[t]]$W))
    expect_length(bw$grads[[t]]$b, length(net[[t]]$b))
  }
})

test_that("the analytic gradients match finite differences", {
  # final = "sigmoid" means dout is already dL/dz, which is the
  # cross-entropy convention; the reference objective has to match it
  h <- 1e-6
  worst <- function(sizes, final, hidden_act, seed = 5L, y = 1) {
    net <- .propinf_mlp_init(sizes, .propinf_rng(seed))
    x <- seq(0.3, by = -0.4, length.out = sizes[1])
    obj <- function(nt) {
      o <- .propinf_mlp_forward(nt, x, final = final,
                                hidden_act = hidden_act)
      o <- o$acts[[length(o$acts)]]
      if (final == "sigmoid") {
        q <- min(max(o[[1]], 1e-12), 1 - 1e-12)
        -(y * log(q) + (1 - y) * log(1 - q))
      } else {
        0.5 * sum((o - y)^2)
      }
    }
    fp <- .propinf_mlp_forward(net, x, final = final, hidden_act = hidden_act)
    o <- fp$acts[[length(fp$acts)]]
    dout <- if (final == "sigmoid") o[[1]] - y else (o - y)
    bw <- .propinf_mlp_backward(net, fp$acts, fp$pre, dout,
                                .propinf_zero_like(net), final = final,
                                hidden_act = hidden_act)
    w <- 0
    for (t in seq_along(net)) {
      for (i in seq_len(nrow(net[[t]]$W))) {
        for (j in seq_len(ncol(net[[t]]$W))) {
          up <- net; up[[t]]$W[i, j] <- up[[t]]$W[i, j] + h
          dn <- net; dn[[t]]$W[i, j] <- dn[[t]]$W[i, j] - h
          w <- max(w, abs((obj(up) - obj(dn)) / (2 * h) - bw$grads[[t]]$W[i, j]))
        }
      }
      for (i in seq_along(net[[t]]$b)) {
        up <- net; up[[t]]$b[i] <- up[[t]]$b[i] + h
        dn <- net; dn[[t]]$b[i] <- dn[[t]]$b[i] - h
        w <- max(w, abs((obj(up) - obj(dn)) / (2 * h) - bw$grads[[t]]$b[i]))
      }
    }
    w
  }
  expect_lt(worst(c(3L, 4L, 1L), "sigmoid", "relu"), 1e-6)
  expect_lt(worst(c(3L, 4L, 1L), "sigmoid", "tanh"), 1e-6)
  expect_lt(worst(c(3L, 5L, 2L), "linear", "relu"), 1e-6)
  expect_lt(worst(c(3L, 5L, 2L), "tanh", "tanh"), 1e-6)
  expect_lt(worst(c(4L, 6L, 3L, 2L), "tanh", "tanh"), 1e-6)
})

test_that("mlp_backward returns the gradient with respect to its input", {
  # the documented contract of the Python arm it mirrors: the returned
  # delta is dL/dx, so its length is the input dimension. Stopping the
  # propagation at the second layer returned dL/dz for the first layer
  # instead, and the DeepSets chain then sliced the wrong vector.
  for (sizes in list(c(3L, 4L, 1L), c(5L, 3L, 2L), c(4L, 6L, 3L, 2L))) {
    net <- .propinf_mlp_init(sizes, .propinf_rng(2L))
    x <- seq(0.2, by = 0.3, length.out = sizes[1])
    fp <- .propinf_mlp_forward(net, x, final = "linear")
    bw <- .propinf_mlp_backward(net, fp$acts, fp$pre,
                                rep(1, sizes[length(sizes)]),
                                .propinf_zero_like(net), final = "linear")
    expect_length(bw$delta, sizes[1])
    # and it equals the finite difference of the output sum w.r.t. x
    h <- 1e-6
    fd <- vapply(seq_along(x), function(k) {
      xu <- x; xu[k] <- xu[k] + h
      xd <- x; xd[k] <- xd[k] - h
      su <- sum(.propinf_mlp_forward(net, xu, final = "linear")$acts[[length(net) + 1L]])
      sd_ <- sum(.propinf_mlp_forward(net, xd, final = "linear")$acts[[length(net) + 1L]])
      (su - sd_) / (2 * h)
    }, numeric(1))
    expect_equal(bw$delta, fd, tolerance = 1e-6)
  }
})

test_that("the vector meta-classifier actually trains", {
  feats <- lapply(1:8, function(i) as.numeric(c(i, i^2 / 10, -i / 2, 1)))
  labs <- c(0, 0, 0, 0, 1, 1, 1, 1)
  init <- .propinf_mlp_init(c(4L, 8L, 1L), .propinf_rng(7L))
  trained <- .propinf_train_vector_meta(feats, labs, 8L, 200L, 0.05, 0L)
  # the weights moved: with the gradients discarded these were identical
  expect_false(isTRUE(all.equal(init[[1]]$W, trained[[1]]$W)))
  sq <- function(net) mean((vapply(feats, function(f)
    .propinf_vector_meta_predict(net, f), numeric(1)) - labs)^2)
  expect_lt(sq(trained), sq(init))
})

test_that("the DeepSets meta-classifier actually trains", {
  sp <- shadow_pair()
  sets <- lapply(sp$nets, .propinf_set_representation_internal)
  shapes <- lapply(sets[[1]], function(L) c(length(L), length(L[[1]]) - 1L))
  init <- .propinf_deepsets_init(shapes, 8L, 4L, 8L, .propinf_rng(11L))
  init$scalers <- .propinf_layer_scalers(sets)
  trained <- .propinf_train_set_meta(sets, sp$labs, 8L, 4L, 8L, 60L, 0.05, 0L)
  expect_false(isTRUE(all.equal(init$rho[[1]]$W, trained$rho[[1]]$W)))
  expect_false(isTRUE(all.equal(init$phis[[1]][[1]]$W, trained$phis[[1]][[1]]$W)))
  # the edge networks of the paired context are trained too
  expect_false(is.null(trained$psis[[2]]))
  expect_false(isTRUE(all.equal(init$psis[[2]][[1]]$W, trained$psis[[2]][[1]]$W)))
  sq <- function(m) mean((vapply(sets, function(s)
    .propinf_deepsets_forward(m, s)[[1L]], numeric(1)) - sp$labs)^2)
  expect_lt(sq(trained), sq(init))
})

test_that("psis keeps one entry per layer, NULL where there is no edge network", {
  shapes <- list(c(3L, 2L), c(1L, 3L))
  m <- .propinf_deepsets_init(shapes, 8L, 4L, 8L, .propinf_rng(1L),
                              context = "paired")
  # x[[i]] <- NULL deletes rather than appends, which left psis short and
  # made psis[[t]] index past the end
  expect_length(m$psis, length(shapes))
  expect_null(m$psis[[1]])           # the first layer has no previous layer
  expect_false(is.null(m$psis[[2]]))
  expect_length(m$phis, length(shapes))
  for (ctx in c("as_printed", "none")) {
    mc <- .propinf_deepsets_init(shapes, 8L, 4L, 8L, .propinf_rng(1L),
                                 context = ctx)
    expect_length(mc$psis, length(shapes))
    expect_true(all(vapply(mc$psis, is.null, logical(1))))
  }
  expect_error(.propinf_deepsets_init(shapes, 8L, 4L, 8L, .propinf_rng(1L),
                                      context = "sideways"),
               "context must be one of")
})

test_that("all three contexts run end to end", {
  sp <- shadow_pair()
  for (ctx in c("paired", "as_printed", "none")) {
    r <- morie_propinf_property_inference(sp$nets, sp$labs, representation = "set",
                                          context = ctx, epochs = 15L, seed = 2L)
    expect_identical(r$context, ctx)
    expect_length(r$score, 6L)
    expect_true(all(is.finite(r$score)))
  }
})
