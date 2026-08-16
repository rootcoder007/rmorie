# Property inference on fully connected neural networks.
# Sources: Ganju, K., Wang, Q., Yang, W., Gunter, C. A., and Borisov,
# N. (2018), "Property Inference Attacks on Fully Connected Neural
# Networks using Permutation Invariant Representations", CCS '18,
# 619-633, doi:10.1145/3243734.3243834 (the attack recipe, the three
# representations and the DeepSets meta-classifier); Goodfellow, I.,
# Bengio, Y. and Courville, A. (2016), Deep Learning, Ch. 6 (the
# standard back-prop derivations used here); Zaheer, M., Kottur, S.,
# Ravanbakhsh, S., Poczos, B., Salakhutdinov, R. R. and Smola, A. J.
# (2017), "Deep Sets", NIPS (the rho(sum phi) template the paper
# borrows from).
#
# Native implementation mirroring Python morie.fn.propinf exactly: the
# same hand-rolled LCG that drives the Python arm, the same per-row
# training loop, the same Algorithm 1 / Algorithm 2 representations,
# and the same three meta-classifiers (vector MLP for baseline and
# sorting, DeepSets for the set-based route).

#' Train a fully connected binary classifier by minibatch SGD
#'
#' Cross-entropy loss, ReLU hidden units, logistic output, with a
#' Fisher-Yates shuffle on the LCG every epoch (Section 4 of the
#' paper: shadow classifiers are ordinary fully connected networks
#' trained by the same procedure the victim was).
#'
#' @param X Numeric matrix of inputs.
#' @param y Numeric vector of 0/1 labels.
#' @param hidden Integer vector giving the size of each hidden layer.
#' @param epochs Number of passes over the data.
#' @param lr Positive learning rate.
#' @param batch_size Minibatch size.
#' @param seed Seed for the shared generator.
#' @return A list of layers, each \code{list(W=, b=)}.
#' @export
morie_propinf_train_fcnn <- function(X, y, hidden = c(8L, 4L), epochs = 40L,
                                    lr = 0.1, batch_size = 16L, seed = 0L) {
  rows <- .propinf_rows(X)
  lab <- as.numeric(y)
  if (length(lab) != length(rows))
    stop("propinf: X and y have different lengths")
  if (any(!(lab %in% c(0, 1))))
    stop("propinf: y must be 0/1")
  if (length(hidden) < 1L)
    stop("propinf: at least one hidden layer is required")
  if (as.integer(epochs) < 1L)
    stop("propinf: epochs must be at least 1")
  if (lr <= 0) stop("propinf: lr must be positive")
  if (as.integer(batch_size) < 1L)
    stop("propinf: batch_size must be at least 1")
  rnd <- .propinf_rng(as.integer(seed) + 1L)
  net <- .propinf_init_net(length(rows[[1]]), as.integer(hidden), rnd)
  n <- length(rows)
  order <- seq_len(n) - 1L
  for (ep in seq_len(as.integer(epochs))) {
    for (i in n:2L) {
      j <- as.integer(rnd() * (i))   # 0..i-1
      tmp <- order[i]
      order[i] <- order[j + 1L]
      order[j + 1L] <- tmp
    }
    for (start in seq(0L, n - 1L, by = as.integer(batch_size))) {
      chunk_idx <- order[(start + 1L):min(n, start + as.integer(batch_size))]
      gW <- lapply(net, function(L) matrix(0, nrow = length(L$b),
                                            ncol = length(L$W[[1]])))
      gb <- lapply(net, function(L) rep(0, length(L$b)))
      for (idx in chunk_idx) {
        acts_pre <- .propinf_forward(net, rows[[idx + 1L]])
        acts <- acts_pre$acts
        pre <- acts_pre$pre
        delta <- list(acts[[length(acts)]][[1L]] - lab[idx + 1L])
        for (t in rev(seq_along(net))) {
          a_in <- acts[[t]]
          for (i2 in seq_along(delta)) {
            d <- delta[[i2]]
            gb[[t]][i2] <- gb[[t]][i2] + d
            gW[[t]][i2, ] <- gW[[t]][i2, ] + d * a_in
          }
          if (t > 1L) {
            nxt <- rep(0, length(a_in))
            for (i2 in seq_along(delta)) {
              d <- delta[[i2]]
              wrow <- net[[t]]$W[[i2]]
              for (j2 in seq_along(a_in)) nxt[j2] <- nxt[j2] + d * wrow[j2]
            }
            delta <- as.list(ifelse(pre[[t - 1L]] > 0, nxt, 0))
          }
        }
      }
      scale <- lr / as.numeric(length(chunk_idx))
      for (t in seq_along(net)) {
        net[[t]]$W <- lapply(seq_along(net[[t]]$W), function(i2)
          net[[t]]$W[[i2]] - scale * gW[[t]][i2, ])
        net[[t]]$b <- net[[t]]$b - scale * gb[[t]]
      }
    }
  }
  net
}

#' Predict with a fully connected binary classifier
#'
#' Output of the network on each row of \code{X}.
#'
#' @param net Network as returned by \code{morie_propinf_train_fcnn}.
#' @param X Numeric matrix of inputs.
#' @return Numeric vector of predicted probabilities.
#' @export
morie_propinf_fcnn_predict <- function(net, X) {
  rows <- .propinf_rows(X)
  vapply(rows, function(r) {
    ap <- .propinf_forward(net, r)
    ap$acts[[length(ap$acts)]][[1L]]
  }, numeric(1))
}

#' Reorder the neurons of a hidden layer
#'
#' Section 5 of the paper: layer \code{t} becomes sigma(h_t) and the
#' next layer's weight columns are permuted to match, so the network
#' computes exactly the same function (Proposition 5.1).
#'
#' @param net Network as returned by \code{morie_propinf_train_fcnn}.
#' @param t Index of a hidden layer.
#' @param sigma Permutation of \code{seq_len(nrow(W[[t]]))}.
#' @return A permuted copy of the network.
#' @export
morie_propinf_permute_hidden_layer <- function(net, t, sigma) {
  t <- as.integer(t)
  if (t < 0L || t >= length(net))
    stop("propinf: t must index a hidden layer")
  sigma <- as.integer(sigma)
  m <- length(net[[t + 1L]]$W)
  if (!identical(sort(sigma), seq_len(m) - 1L))
    stop("propinf: sigma is not a permutation of the layer")
  out <- lapply(net, function(L) list(W = lapply(L$W, function(r) as.numeric(r)),
                                      b = as.numeric(L$b)))
  out[[t + 1L]]$W <- lapply(sigma + 1L, function(s) net[[t + 1L]]$W[[s]])
  out[[t + 1L]]$b <- net[[t + 1L]]$b[sigma + 1L]
  Wnext <- net[[t + 2L]]$W
  for (i in seq_along(Wnext)) {
    out[[t + 2L]]$W[[i]] <- Wnext[[i]][sigma + 1L]
  }
  out
}

#' Baseline feature vector: every weight and bias in one vector
#'
#' Flattening all the weights and biases into one vector. Kept
#' because it is the thing the other two have to beat, not because
#' it works.
#'
#' @param net Network as returned by \code{morie_propinf_train_fcnn}.
#' @return Numeric vector.
#' @export
morie_propinf_flat_representation <- function(net) {
  F <- numeric(0)
  for (L in net) {
    for (i in seq_along(L$W)) {
      F <- c(F, L$W[[i]], L$b[i])
    }
  }
  F
}

#' Sorted feature vector: canonicalise then flatten (Algorithm 1)
#'
#' Hidden layers are sorted by \code{metric} (default: the magnitude
#' of the sum of the node's weights) in descending order; the output
#' layer cannot be permuted and is appended as it stands.
#'
#' @param net Network as returned by \code{morie_propinf_train_fcnn}.
#' @param metric Optional function \code{(layer, i)} returning a
#'   scalar. Defaults to the magnitude of the node's weight sum.
#' @return Numeric vector.
#' @export
morie_propinf_sorted_representation <- function(net, metric = NULL) {
  if (is.null(metric)) metric <- .propinf_node_metric
  cur <- lapply(net, function(L) list(W = lapply(L$W, function(r) as.numeric(r)),
                                      b = as.numeric(L$b)))
  for (t in seq_len(length(cur) - 1L) - 1L) {
    vals <- vapply(seq_len(length(cur[[t + 1L]]$W)) - 1L,
                   function(i) metric(cur[[t + 1L]], i), numeric(1))
    sigma <- order(-vals) - 1L
    cur <- .propinf_permute_hidden_layer_internal(cur, t, sigma)
  }
  .propinf_flat_representation_internal(cur)
}

#' Set feature representation: each layer as a set of (weights, bias)
#'
#' Algorithm 2's input to the DeepSets meta-classifier.
#'
#' @param net Network as returned by \code{morie_propinf_train_fcnn}.
#' @return List of layers, each a list of \code{c(weights, bias)}
#'   numeric vectors.
#' @export
morie_propinf_set_representation <- function(net) {
  lapply(net, function(L) lapply(seq_along(L$W), function(i) c(L$W[[i]], L$b[i])))
}

#' Property inference on fully connected networks
#'
#' Train a meta-classifier on shadow model features labelled by the
#' training-set property, then apply it to the target models. The
#' representation determines the meta-classifier: \code{"baseline"}
#' and \code{"sorting"} feed a flat vector MLP; \code{"set"} feeds
#' the DeepSets form of Algorithm 2.
#'
#' @param shadow_models List of shadow networks.
#' @param shadow_labels Numeric 0/1 vector, one per shadow network.
#' @param target_models Optional list of target networks. Defaults to
#'   the shadow models.
#' @param target_labels Optional numeric 0/1 vector, one per target
#'   network.
#' @param representation One of \code{"baseline"}, \code{"sorting"},
#'   \code{"set"}.
#' @param meta_hidden Hidden layer sizes for the vector meta-classifier.
#' @param phi_hidden Hidden layer sizes for the per-layer phi nets.
#' @param repr_dim Node representation dimension.
#' @param rho_hidden Hidden layer sizes for the rho net.
#' @param context One of \code{"paired"}, \code{"as_printed"},
#'   \code{"none"}; only used for \code{representation="set"}.
#' @param epochs Number of SGD passes.
#' @param lr Positive learning rate.
#' @param seed Seed for the shared generator.
#' @return A list with \code{estimate}, \code{accuracy},
#'   \code{train_accuracy}, \code{prediction}, \code{score},
#'   \code{representation}, \code{context}, \code{n_shadow},
#'   \code{n_target}, \code{architecture}, \code{meta_classifier},
#'   \code{method}, \code{note}.
#' @export
morie_propinf_property_inference <- function(shadow_models, shadow_labels,
                                             target_models = NULL,
                                             target_labels = NULL,
                                             representation = "set",
                                             meta_hidden = c(16L),
                                             phi_hidden = c(8L),
                                             repr_dim = 4L,
                                             rho_hidden = c(8L),
                                             context = "paired",
                                             epochs = 30L, lr = 0.05,
                                             seed = 0L) {
  if (!(representation %in% .propinf_REPRS))
    stop(paste0("propinf: representation must be one of ",
                paste(.propinf_REPRS, collapse = ", ")))
  nets <- lapply(shadow_models, function(n) n)
  lab <- as.numeric(shadow_labels)
  if (length(nets) != length(lab))
    stop("propinf: one label per shadow model is required")
  if (length(nets) < 4L)
    stop("propinf: at least four shadow models are needed")
  if (any(!(lab %in% c(0, 1))))
    stop("propinf: shadow_labels must be 0/1")
  if (length(unique(lab)) < 2L)
    stop("propinf: shadow_labels must contain both classes")
  arch <- lapply(nets[[1]], function(L) c(length(L$W), length(L$W[[1]])))
  for (net in nets) {
    a <- lapply(net, function(L) c(length(L$W), length(L$W[[1]])))
    if (!identical(a, arch))
      stop("propinf: all shadow models must share one architecture")
  }
  if (!(context %in% .propinf_CONTEXTS))
    stop(paste0("propinf: context must be one of ",
                paste(.propinf_CONTEXTS, collapse = ", ")))
  if (as.integer(repr_dim) < 1L)
    stop("propinf: repr_dim must be at least 1")
  if (as.integer(epochs) < 1L || lr <= 0)
    stop("propinf: epochs must be >= 1 and lr positive")

  if (is.null(target_models)) {
    targets <- nets
  } else {
    targets <- lapply(target_models, function(n) n)
  }
  for (net in targets) {
    a <- lapply(net, function(L) c(length(L$W), length(L$W[[1]])))
    if (!identical(a, arch))
      stop("propinf: target model architecture differs from the shadow models")
  }

  if (representation == "set") {
    train_sets <- lapply(nets, .propinf_set_representation_internal)
    meta <- .propinf_train_set_meta(train_sets, lab,
                                    as.integer(phi_hidden),
                                    as.integer(repr_dim),
                                    as.integer(rho_hidden),
                                    as.integer(epochs), lr,
                                    as.integer(seed), context)
    scores <- vapply(targets, function(net)
      .propinf_deepsets_forward(meta, .propinf_set_representation_internal(net))[[1L]],
      numeric(1))
    fit <- vapply(train_sets, function(s)
      .propinf_deepsets_forward(meta, s)[[1L]], numeric(1))
  } else {
    extract <- if (representation == "sorting")
      .propinf_sorted_representation_internal else
        .propinf_flat_representation_internal
    raw <- lapply(nets, extract)
    std <- .propinf_standardise(raw)
    feats <- std$feats; mu <- std$mu; sd <- std$sd
    meta <- .propinf_train_vector_meta(feats, lab, as.integer(meta_hidden),
                                      as.integer(epochs), lr, as.integer(seed))
    scores <- vapply(targets, function(net)
      .propinf_vector_meta_predict(meta, .propinf_apply_standardise(extract(net), mu, sd)),
      numeric(1))
    fit <- vapply(feats, function(f) .propinf_vector_meta_predict(meta, f),
                  numeric(1))
  }

  pred <- ifelse(scores >= 0.5, 1L, 0L)
  train_acc <- mean(ifelse(fit >= 0.5, 1, 0) == lab)
  acc <- NULL
  if (!is.null(target_labels)) {
    tl <- as.numeric(target_labels)
    if (length(tl) != length(targets))
      stop("propinf: one target label per target model")
    acc <- mean(pred == tl)
  }

  list(estimate = if (!is.null(acc)) acc else train_acc,
       accuracy = acc,
       train_accuracy = train_acc,
       prediction = pred,
       score = scores,
       representation = representation,
       context = if (representation == "set") context else NULL,
       n_shadow = length(nets),
       n_target = length(targets),
       architecture = arch,
       meta_classifier = meta,
       method = paste0("property inference by shadow training ",
                       "(Ganju et al. 2018), ", representation, " representation"),
       note = paste0("baseline flattening is not permutation invariant ",
                     "and the paper reports 55-77% for it; sorting ",
                     "(Algorithm 1) canonicalises each hidden layer, ",
                     "set (Algorithm 2) is invariant by construction ",
                     "and is the paper's best"))
}

# ----------------- internal helpers (not exported) --------------------

.propinf_REPRS <- c("baseline", "sorting", "set")
.propinf_CONTEXTS <- c("paired", "as_printed", "none")

#' .propinf_relu
#'
#' A step of the propinf_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param v Passed to \code{>}.
#' @return One of two values, depending on the branch taken.
#' @export
.propinf_relu <- function(v) if (v > 0) v else 0

#' .propinf_sigmoid
#'
#' A step of the propinf_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param z Numeric; passed to \code{exp}.
#' @return One of two values, depending on the branch taken.
#' @export
.propinf_sigmoid <- function(z) {
  if (z >= 0) 1 / (1 + exp(-z)) else {
    e <- exp(z); e / (1 + e)
  }
}

#' .propinf_rng
#'
#' A step of the propinf_native implementation. Called by \code{.propinf_train_set_meta}, \code{.propinf_train_vector_meta}, \code{morie_propinf_train_fcnn}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param seed Coerced to integer by the body, with \code{as.integer}.
#' @return The value of \code{e}, as built in the body.
#' @export
.propinf_rng <- function(seed) {
  st <- as.integer(seed)
  # 2^31 exceeds .Machine$integer.max, so it cannot be an integer
  # literal: with the L suffix R warns and silently uses the double
  # anyway. Written as a double, which is what was always computed.
  if (st < 0L) st <- st + 2147483648
  if (st == 0L) st <- 1L
  st <- st %% 2147483647L
  e <- new.env(parent = emptyenv())
  e$st <- st
  e
}

#' .propinf_rng_next
#'
#' A step of the propinf_native implementation. Called by \code{.propinf_normal}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param e A list; the body reads \code{$st} from it.
#' @return A numeric value.
#' @export
.propinf_rng_next <- function(e) {
  e$st <- .ghc_lcg31(e$st)
  e$st / 2147483647
}

#' .propinf_normal
#'
#' A step of the propinf_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param rnd A list; the body reads \code{$parent} from it.
#' @param scale Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @return A numeric value.
#' @export
.propinf_normal <- function(rnd, scale = 1) {
  u <- max(.propinf_rng_next(rnd$parent$dummy %||% rnd), 1e-12)  # placeholder
  scale * sqrt(-2 * log(u)) * cos(2 * pi)
}

# The normal draws in the Python arm go through math.log and math.cos
# directly on the LCG output. Mirror that exactly.

#' .propinf_lcg_draw
#'
#' A step of the propinf_native implementation. Called by \code{.propinf_normal_lcg}, \code{.propinf_train_set_meta}, \code{.propinf_train_vector_meta}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param e A list; the body reads \code{$st} from it.
#' @return A numeric value.
#' @export
.propinf_lcg_draw <- function(e) {
  e$st <- .ghc_lcg31(e$st)
  e$st / 2147483647
}

#' .propinf_normal_lcg
#'
#' A step of the propinf_native implementation. Called by \code{.propinf_init_net}, \code{.propinf_mlp_init}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param e Passed to \code{.propinf_lcg_draw}.
#' @param scale Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @return A numeric value.
#' @export
.propinf_normal_lcg <- function(e, scale = 1) {
  u <- max(.propinf_lcg_draw(e), 1e-12)
  scale * sqrt(-2 * log(u)) * cos(2 * pi * .propinf_lcg_draw(e))
}

#' .propinf_init_net
#'
#' A step of the propinf_native implementation. Called by \code{morie_propinf_train_fcnn}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param n_in Coerced to integer by the body, with \code{as.integer}.
#' @param hidden Coerced to integer by the body, with \code{as.integer}.
#' @param rnd Passed to \code{.propinf_normal_lcg}.
#' @return The value of \code{net}, as built in the body.
#' @export
.propinf_init_net <- function(n_in, hidden, rnd) {
  sizes <- c(as.integer(n_in), as.integer(hidden), 1L)
  net <- list()
  for (t in 2:length(sizes)) {
    fan_in <- sizes[t - 1L]
    s <- sqrt(2 / fan_in)
    W <- matrix(0, nrow = sizes[t], ncol = fan_in)
    for (i in seq_len(sizes[t])) for (j in seq_len(fan_in))
      W[i, j] <- .propinf_normal_lcg(rnd, s)
    b <- rep(0, sizes[t])
    net[[length(net) + 1L]] <- list(W = W, b = b)
  }
  net
}

#' .propinf_forward
#'
#' A step of the propinf_native implementation. Called by \code{morie_propinf_fcnn_predict}, \code{morie_propinf_train_fcnn}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param net A vector; its length is taken and its elements indexed.
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{acts}, \code{pre}.
#' @export
.propinf_forward <- function(net, x) {
  acts <- list(as.numeric(x))
  pre <- list()
  for (t in seq_along(net)) {
    z <- as.numeric(net[[t]]$W %*% matrix(acts[[length(acts)]], ncol = 1) +
                    net[[t]]$b)
    pre[[length(pre) + 1L]] <- z
    last <- (t == length(net))
    if (last) {
      acts[[length(acts) + 1L]] <- vapply(z, .propinf_sigmoid, numeric(1))
    } else {
      acts[[length(acts) + 1L]] <- vapply(z, .propinf_relu, numeric(1))
    }
  }
  list(acts = acts, pre = pre)
}

#' .propinf_rows
#'
#' A step of the propinf_native implementation. Called by \code{morie_propinf_fcnn_predict}, \code{morie_propinf_train_fcnn}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param X A matrix; indexed by row and column.
#' @return The value of \code{out}, as built in the body.
#' @export
.propinf_rows <- function(X) {
  if (!is.matrix(X)) X <- as.matrix(X)
  out <- lapply(seq_len(nrow(X)), function(i) as.numeric(X[i, ]))
  for (row in out) {
    for (v in row) {
      if (!is.finite(v))
        stop("propinf: X contains a non-finite value")
    }
  }
  if (length(out) == 0L) stop("propinf: X is empty")
  p <- length(out[[1L]])
  if (p == 0L) stop("propinf: X has no columns")
  for (row in out) if (length(row) != p) stop("propinf: X is ragged")
  out
}

#' .propinf_node_metric
#'
#' A step of the propinf_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param layer A list; the body reads \code{$W} from it.
#' @param i See Usage.
#' @return A numeric value.
#' @export
.propinf_node_metric <- function(layer, i) {
  abs(sum(layer$W[i, ]))
}

#' .propinf_permute_hidden_layer_internal
#'
#' A step of the propinf_native implementation. Called by \code{.propinf_sorted_representation_internal}, \code{morie_propinf_sorted_representation}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param net A vector; indexed elementwise.
#' @param t Numeric; combined arithmetically in the body.
#' @param sigma Numeric; combined arithmetically in the body.
#' @return The value of \code{out}, as built in the body.
#' @export
.propinf_permute_hidden_layer_internal <- function(net, t, sigma) {
  out <- lapply(net, function(L) list(W = L$W, b = as.numeric(L$b)))
  out[[t + 1L]]$W <- net[[t + 1L]]$W[sigma + 1L, , drop = FALSE]
  out[[t + 1L]]$b <- net[[t + 1L]]$b[sigma + 1L]
  out[[t + 2L]]$W <- net[[t + 2L]]$W[, sigma + 1L, drop = FALSE]
  out
}

#' .propinf_flat_representation_internal
#'
#' A step of the propinf_native implementation. Called by \code{.propinf_sorted_representation_internal}, \code{morie_propinf_sorted_representation}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param net See Usage.
#' @return The value of \code{F}, as built in the body.
#' @export
.propinf_flat_representation_internal <- function(net) {
  F <- numeric(0)
  for (L in net) {
    for (i in seq_len(nrow(L$W))) {
      F <- c(F, L$W[i, ], L$b[i])
    }
  }
  F
}

#' .propinf_sorted_representation_internal
#'
#' A step of the propinf_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param net Iterated over elementwise, with \code{lapply}.
#' @param metric Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @return The value of \code{.propinf_flat_representation_internal}.
#' @export
.propinf_sorted_representation_internal <- function(net, metric = NULL) {
  if (is.null(metric)) metric <- .propinf_node_metric
  cur <- lapply(net, function(L) list(W = L$W, b = as.numeric(L$b)))
  for (t in seq_len(length(cur) - 1L) - 1L) {
    vals <- vapply(seq_len(nrow(cur[[t + 1L]]$W)) - 1L,
                   function(i) metric(cur[[t + 1L]], i), numeric(1))
    sigma <- order(-vals) - 1L
    cur <- .propinf_permute_hidden_layer_internal(cur, t, sigma)
  }
  .propinf_flat_representation_internal(cur)
}

#' .propinf_set_representation_internal
#'
#' A step of the propinf_native implementation. Called by \code{morie_propinf_property_inference}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param net Iterated over elementwise, with \code{lapply}.
#' @return The value of \code{lapply}.
#' @export
.propinf_set_representation_internal <- function(net) {
  lapply(net, function(L) lapply(seq_len(nrow(L$W)),
                                 function(i) c(L$W[i, ], L$b[i])))
}

# ----- meta-classifier MLPs (vector and set) -----

#' .propinf_mlp_init
#'
#' A step of the propinf_native implementation. Called by \code{.propinf_deepsets_init}, \code{.propinf_train_vector_meta}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param sizes A vector; its length is taken and its elements indexed.
#' @param rnd Passed to \code{.propinf_normal_lcg}.
#' @return The value of \code{net}, as built in the body.
#' @export
.propinf_mlp_init <- function(sizes, rnd) {
  net <- list()
  for (t in 2:length(sizes)) {
    s <- sqrt(2 / sizes[t - 1L])
    W <- matrix(0, nrow = sizes[t], ncol = sizes[t - 1L])
    for (i in seq_len(sizes[t])) for (j in seq_len(sizes[t - 1L]))
      W[i, j] <- .propinf_normal_lcg(rnd, s)
    b <- rep(0, sizes[t])
    net[[length(net) + 1L]] <- list(W = W, b = b)
  }
  net
}

#' .propinf_mlp_forward
#'
#' A step of the propinf_native implementation. Called by \code{.propinf_deepsets_forward}, \code{.propinf_train_vector_meta}, \code{.propinf_vector_meta_predict}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param net A vector; its length is taken and its elements indexed.
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param final One of \code{"linear"}, \code{"sigmoid"}, \code{"tanh"}. Defaults to \code{"relu"}.
#' @param hidden_act Compared against \code{"tanh"}. Defaults to \code{"relu"}.
#' @return A list with \code{acts}, \code{pre}.
#' @export
.propinf_mlp_forward <- function(net, x, final = "relu",
                                 hidden_act = "relu") {
  acts <- list(as.numeric(x))
  pre <- list()
  last <- length(net)
  for (t in seq_along(net)) {
    z <- as.numeric(net[[t]]$W %*% matrix(acts[[length(acts)]], ncol = 1) +
                    net[[t]]$b)
    pre[[length(pre) + 1L]] <- z
    if (t == last && final == "linear") {
      acts[[length(acts) + 1L]] <- z
    } else if (t == last && final == "sigmoid") {
      acts[[length(acts) + 1L]] <- vapply(z, .propinf_sigmoid, numeric(1))
    } else if (t == last && final == "tanh") {
      acts[[length(acts) + 1L]] <- tanh(z)
    } else if (hidden_act == "tanh") {
      acts[[length(acts) + 1L]] <- tanh(z)
    } else {
      acts[[length(acts) + 1L]] <- vapply(z, .propinf_relu, numeric(1))
    }
  }
  list(acts = acts, pre = pre)
}

#' .propinf_mlp_backward
#'
#' A step of the propinf_native implementation. Called by \code{.propinf_deepsets_backward}, \code{.propinf_train_vector_meta}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param net A vector; its length is taken and its elements indexed.
#' @param acts A vector; indexed elementwise.
#' @param pre A vector; indexed elementwise.
#' @param dout Coerced to numeric by the body, with \code{as.numeric}.
#' @param grads A vector; indexed elementwise.
#' @param final One of \code{"linear"}, \code{"sigmoid"}, \code{"tanh"}. Defaults to \code{"relu"}.
#' @param hidden_act Compared against \code{"tanh"}. Defaults to \code{"relu"}.
#' @return The value of \code{delta}, as built in the body.
#' @export
.propinf_mlp_backward <- function(net, acts, pre, dout, grads, final = "relu",
                                  hidden_act = "relu") {
  delta <- as.numeric(dout)
  last <- length(net)
  for (t in last:1L) {
    if (t == last && final == "tanh") {
      delta <- delta * (1 - acts[[t + 1L]]^2)
    } else if (!(t == last && final %in% c("linear", "sigmoid"))) {
      if (hidden_act == "tanh") {
        delta <- delta * (1 - acts[[t + 1L]]^2)
      } else {
        delta <- ifelse(pre[[t]] > 0, delta, 0)
      }
    }
    a_in <- acts[[t]]
    grads[[t]]$b <- grads[[t]]$b + delta
    grads[[t]]$W <- grads[[t]]$W + delta %o% a_in
    if (t > 1L) {
      nxt <- as.numeric(t(delta) %*% net[[t]]$W)
      delta <- nxt
    }
  }
  delta
}

#' .propinf_zero_like
#'
#' A step of the propinf_native implementation. Called by \code{.propinf_train_vector_meta}, \code{.propinf_zero_grads}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param net Iterated over elementwise, with \code{lapply}.
#' @return The value of \code{lapply}.
#' @export
.propinf_zero_like <- function(net) {
  lapply(net, function(L) list(W = matrix(0, nrow = nrow(L$W),
                                          ncol = ncol(L$W)),
                                b = rep(0, length(L$b))))
}

#' .propinf_sgd_step
#'
#' A step of the propinf_native implementation. Called by \code{.propinf_train_set_meta}, \code{.propinf_train_vector_meta}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param net A vector; its length is taken and its elements indexed.
#' @param grads A vector; indexed elementwise.
#' @param lr Numeric; combined arithmetically in the body.
#' @param scale Numeric; combined arithmetically in the body.
#' @return The value of \code{net}, as built in the body.
#' @export
.propinf_sgd_step <- function(net, grads, lr, scale) {
  for (k in seq_along(net)) {
    net[[k]]$W <- net[[k]]$W - lr * scale * grads[[k]]$W
    net[[k]]$b <- net[[k]]$b - lr * scale * grads[[k]]$b
  }
  net
}

#' .propinf_train_vector_meta
#'
#' A step of the propinf_native implementation. Called by \code{morie_propinf_property_inference}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param feats A vector; its length is taken and its elements indexed.
#' @param labels A vector; indexed elementwise.
#' @param hidden Coerced to integer by the body, with \code{as.integer}.
#' @param epochs Coerced to integer by the body, with \code{as.integer}.
#' @param lr Passed to \code{.propinf_sgd_step}.
#' @param seed Coerced to integer by the body, with \code{as.integer}.
#' @return The value of \code{net}, as built in the body.
#' @export
.propinf_train_vector_meta <- function(feats, labels, hidden, epochs, lr,
                                        seed) {
  rnd <- .propinf_rng(as.integer(seed) + 7L)
  d <- length(feats[[1L]])
  sizes <- c(d, as.integer(hidden), 1L)
  net <- .propinf_mlp_init(sizes, rnd)
  n <- length(feats)
  order <- seq_len(n) - 1L
  for (ep in seq_len(as.integer(epochs))) {
    for (i in n:2L) {
      j <- as.integer(.propinf_lcg_draw(rnd) * i)
      tmp <- order[i]
      order[i] <- order[j + 1L]
      order[j + 1L] <- tmp
    }
    for (idx in order) {
      ap <- .propinf_mlp_forward(net, feats[[idx + 1L]], final = "sigmoid")
      acts <- ap$acts; pre <- ap$pre
      grads <- .propinf_zero_like(net)
      .propinf_mlp_backward(net, acts, pre, acts[[length(acts)]][[1L]] -
                              labels[idx + 1L], grads, final = "sigmoid")
      net <- .propinf_sgd_step(net, grads, lr, 1)
    }
  }
  net
}

#' .propinf_vector_meta_predict
#'
#' A step of the propinf_native implementation. Called by \code{morie_propinf_property_inference}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param net A vector; its length is taken.
#' @param f Passed to \code{.propinf_mlp_forward}.
#' @return The value of \code{[[}.
#' @export
.propinf_vector_meta_predict <- function(net, f) {
  .propinf_mlp_forward(net, f, final = "sigmoid")$acts[[length(net) + 1L]][[1L]]
}

#' .propinf_layer_scalers
#'
#' A step of the propinf_native implementation. Called by \code{.propinf_train_set_meta}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param sets_list A vector; indexed elementwise.
#' @return The value of \code{out}, as built in the body.
#' @export
.propinf_layer_scalers <- function(sets_list) {
  out <- list()
  for (t in seq_along(sets_list[[1L]])) {
    ws <- unlist(lapply(sets_list, function(st)
      unlist(lapply(st[[t]], function(node) node[-length(node)]))))
    bs <- unlist(lapply(sets_list, function(st)
      vapply(st[[t]], function(node) node[length(node)], numeric(1))))
    stats <- list()
    for (vals in list(ws, bs)) {
      mu <- sum(vals) / length(vals)
      var <- sum((vals - mu)^2) / max(length(vals) - 1, 1)
      stats[[length(stats) + 1L]] <- c(mu, if (var > 1e-24) sqrt(var) else 1)
    }
    out[[length(out) + 1L]] <- stats
  }
  out
}

#' .propinf_deepsets_init
#'
#' A step of the propinf_native implementation. Called by \code{.propinf_train_set_meta}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param shapes A vector; its length is taken and its elements indexed.
#' @param phi_hidden Coerced to integer by the body, with \code{as.integer}.
#' @param repr_dim Numeric; combined arithmetically in the body.
#' @param rho_hidden Coerced to integer by the body, with \code{as.integer}.
#' @param rnd Passed to \code{.propinf_mlp_init}.
#' @param context One of \code{"none"}, \code{"paired"}. Defaults to \code{"paired"}.
#' @param edge_hidden Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{phis}, \code{psis}, \code{rho}, \code{repr_dim}, \code{shapes}, \code{context}, \code{scalers}.
#' @export
.propinf_deepsets_init <- function(shapes, phi_hidden, repr_dim, rho_hidden,
                                   rnd, context = "paired",
                                   edge_hidden = NULL) {
  if (!(context %in% .propinf_CONTEXTS))
    stop(paste0("propinf: context must be one of ",
                paste(.propinf_CONTEXTS, collapse = ", ")))
  phi_hidden <- as.integer(phi_hidden)
  edge_hidden <- if (is.null(edge_hidden)) phi_hidden
                 else as.integer(edge_hidden)
  phis <- list(); psis <- list()
  prev_nodes <- 0L
  for (t in seq_along(shapes)) {
    n_nodes <- shapes[[t]][1L]; n_in <- shapes[[t]][2L]
    psi <- NULL
    if (t == 1L || context == "none") {
      d_in <- n_in + 1L
    } else if (context == "paired") {
      psi <- .propinf_mlp_init(c(1L + repr_dim, edge_hidden, repr_dim), rnd)
      d_in <- 1L + repr_dim
    } else {
      d_in <- n_in + 1L + prev_nodes * repr_dim
    }
    psis[[length(psis) + 1L]] <- psi
    phis[[length(phis) + 1L]] <- .propinf_mlp_init(c(d_in, phi_hidden,
                                                     repr_dim), rnd)
    prev_nodes <- n_nodes
  }
  rho <- .propinf_mlp_init(c(length(shapes) * repr_dim,
                             as.integer(rho_hidden), 1L), rnd)
  list(phis = phis, psis = psis, rho = rho, repr_dim = as.integer(repr_dim),
       shapes = shapes, context = context, scalers = NULL)
}

#' .propinf_deepsets_forward
#'
#' A step of the propinf_native implementation. Called by \code{.propinf_train_set_meta}, \code{morie_propinf_property_inference}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param model A list; the body reads \code{$context}, \code{$phis}, \code{$psis}, \code{$repr_dim}, \code{$rho}, \code{$scalers} from it.
#' @param sets A vector; its length is taken and its elements indexed.
#' @return The value of \code{list}.
#' @export
.propinf_deepsets_forward <- function(model, sets) {
  phis <- model$phis; psis <- model$psis; r <- model$repr_dim
  ctx <- model$context
  caches <- list(); L <- list()
  prev_reprs <- list()
  scalers <- model$scalers
  for (t in seq_along(sets)) {
    layer <- sets[[t]]
    node_reprs <- list()
    layer_cache <- list()
    for (i in seq_along(layer)) {
      node <- layer[[i]]
      n_w <- length(node) - 1L
      w <- node[seq_len(n_w)]; b <- node[n_w + 1L]
      if (!is.null(scalers)) {
        sc <- scalers[[t]]
        mw <- sc[[1L]][1L]; sw <- sc[[1L]][2L]
        mb <- sc[[2L]][1L]; sb <- sc[[2L]][2L]
        w <- (w - mw) / sw
        b <- (b - mb) / sb
      }
      edges <- NULL
      if (t == 1L || ctx == "none") {
        x <- c(w, b)
      } else if (ctx == "paired") {
        acc <- rep(0, r)
        edges <- list()
        for (j in seq_along(w)) {
          ea_ep <- .propinf_mlp_forward(psis[[t]],
                                        c(w[j], prev_reprs[[j]]),
                                        final = "tanh", hidden_act = "tanh")
          ea <- ea_ep$acts; ep <- ea_ep$pre
          edges[[length(edges) + 1L]] <- list(acts = ea, pre = ep)
          acc <- acc + ea[[length(ea)]]
        }
        x <- c(b, acc)
      } else {
        x <- c(w, b, unlist(prev_reprs))
      }
      ap <- .propinf_mlp_forward(phis[[t]], x, final = "tanh",
                                 hidden_act = "tanh")
      node_reprs[[length(node_reprs) + 1L]] <- ap$acts[[length(ap$acts)]]
      layer_cache[[length(layer_cache) + 1L]] <- list(acts = ap$acts,
                                                      pre = ap$pre,
                                                      n_w = n_w, edges = edges)
    }
    caches[[length(caches) + 1L]] <- layer_cache
    Lsum <- rep(0, r)
    for (nr in node_reprs) Lsum <- Lsum + nr
    L[[length(L) + 1L]] <- Lsum
    prev_reprs <- node_reprs
  }
  F <- unlist(L)
  r_ap <- .propinf_mlp_forward(model$rho, F, final = "sigmoid",
                                hidden_act = "tanh")
  list(r_ap$acts[[length(r_ap$acts)]][[1L]],
       list(caches = caches, L = L, F = F, racts = r_ap$acts, rpre = r_ap$pre))
}

#' .propinf_deepsets_backward
#'
#' A step of the propinf_native implementation. Called by \code{.propinf_train_set_meta}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param model A list; the body reads \code{$context}, \code{$phis}, \code{$psis}, \code{$repr_dim}, \code{$rho} from it.
#' @param sets A vector; its length is taken.
#' @param cache A list; the body reads \code{$caches}, \code{$racts}, \code{$rpre} from it.
#' @param dout Passed to \code{.propinf_mlp_backward}.
#' @param grads A list; the body reads \code{$phis}, \code{$psis}, \code{$rho} from it.
#' @return The value of \code{for}.
#' @export
.propinf_deepsets_backward <- function(model, sets, cache, dout, grads) {
  r <- model$repr_dim
  ctx <- model$context
  dF <- .propinf_mlp_backward(model$rho, cache$racts, cache$rpre, dout,
                              grads$rho, final = "sigmoid", hidden_act = "tanh")
  dL <- lapply(seq_along(sets), function(t) dF[(t - 1L) * r + seq_len(r)])
  d_from_next <- as.list(rep(NA, length(sets)))
  for (t in rev(seq_along(sets))) {
    layer_cache <- cache$caches[[t]]
    n_prev <- if (t > 1L) length(cache$caches[[t - 1L]]) else 0L
    d_prev <- if (n_prev > 0L)
      lapply(seq_len(n_prev), function(j) rep(0, r)) else list()
    for (i in seq_along(layer_cache)) {
      lc <- layer_cache[[i]]
      dnode <- dL[[t]]
      extra <- d_from_next[[t]]
      if (!is.null(extra) && length(extra) > 0L && !identical(extra, NA) &&
          !is.na(extra[[1L]])) {
        dnode <- dnode + extra[[i]]
      }
      dx <- .propinf_mlp_backward(model$phis[[t]], lc$acts, lc$pre, dnode,
                                  grads$phis[[t]], final = "tanh",
                                  hidden_act = "tanh")
      if (t == 1L || ctx == "none") next
      if (ctx == "paired") {
        dacc <- dx[2L:(1L + r)]
        for (j in seq_along(lc$edges)) {
          ea <- lc$edges[[j]]$acts; ep <- lc$edges[[j]]$pre
          de <- .propinf_mlp_backward(model$psis[[t]], ea, ep, dacc,
                                      grads$psis[[t]], final = "tanh",
                                      hidden_act = "tanh")
          d_prev[[j]] <- d_prev[[j]] + de[2L:(1L + r)]
        }
      } else {
        tail <- dx[(lc$n_w + 2L):length(dx)]
        for (j in seq_len(n_prev)) {
          d_prev[[j]] <- d_prev[[j]] + tail[(j - 1L) * r + seq_len(r)]
        }
      }
    }
    if (t > 1L) d_from_next[[t - 1L]] <- d_prev
  }
}

#' .propinf_zero_grads
#'
#' A step of the propinf_native implementation. Called by \code{.propinf_train_set_meta}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param model A list; the body reads \code{$phis}, \code{$psis}, \code{$rho} from it.
#' @return A list with \code{phis}, \code{psis}, \code{rho}.
#' @export
.propinf_zero_grads <- function(model) {
  list(phis = lapply(model$phis, .propinf_zero_like),
       psis = lapply(model$psis, function(p)
         if (is.null(p)) NULL else .propinf_zero_like(p)),
       rho = .propinf_zero_like(model$rho))
}

#' .propinf_train_set_meta
#'
#' A step of the propinf_native implementation. Called by \code{morie_propinf_property_inference}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param sets_list A vector; its length is taken and its elements indexed.
#' @param labels A vector; indexed elementwise.
#' @param phi_hidden Passed to \code{.propinf_deepsets_init}.
#' @param repr_dim Passed to \code{.propinf_deepsets_init}.
#' @param rho_hidden Passed to \code{.propinf_deepsets_init}.
#' @param epochs Coerced to integer by the body, with \code{as.integer}.
#' @param lr Passed to \code{.propinf_sgd_step}.
#' @param seed Coerced to integer by the body, with \code{as.integer}.
#' @param context Passed to \code{.propinf_deepsets_init}. Defaults to \code{"paired"}.
#' @return The value of \code{model}, as built in the body.
#' @export
.propinf_train_set_meta <- function(sets_list, labels, phi_hidden, repr_dim,
                                    rho_hidden, epochs, lr, seed,
                                    context = "paired") {
  rnd <- .propinf_rng(as.integer(seed) + 11L)
  shapes <- lapply(sets_list[[1L]], function(layer)
    c(length(layer), length(layer[[1L]]) - 1L))
  model <- .propinf_deepsets_init(shapes, phi_hidden, repr_dim, rho_hidden,
                                  rnd, context = context)
  model$scalers <- .propinf_layer_scalers(sets_list)
  n <- length(sets_list)
  order <- seq_len(n) - 1L
  for (ep in seq_len(as.integer(epochs))) {
    for (i in n:2L) {
      j <- as.integer(.propinf_lcg_draw(rnd) * i)
      tmp <- order[i]
      order[i] <- order[j + 1L]
      order[j + 1L] <- tmp
    }
    for (idx in order) {
      fwd <- .propinf_deepsets_forward(model, sets_list[[idx + 1L]])
      out <- fwd[[1L]]; cache <- fwd[[2L]]
      grads <- .propinf_zero_grads(model)
      .propinf_deepsets_backward(model, sets_list[[idx + 1L]], cache,
                                 out - labels[idx + 1L], grads)
      for (k in seq_along(model$phis))
        model$phis[[k]] <- .propinf_sgd_step(model$phis[[k]], grads$phis[[k]],
                                             lr, 1)
      for (k in seq_along(model$psis)) {
        if (!is.null(model$psis[[k]]))
          model$psis[[k]] <- .propinf_sgd_step(model$psis[[k]],
                                               grads$psis[[k]], lr, 1)
      }
      model$rho <- .propinf_sgd_step(model$rho, grads$rho, lr, 1)
    }
  }
  model
}

#' .propinf_standardise
#'
#' A step of the propinf_native implementation. Called by \code{morie_propinf_property_inference}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param feats A vector; its length is taken and its elements indexed.
#' @return A list with \code{feats}, \code{mu}, \code{sd}.
#' @export
.propinf_standardise <- function(feats) {
  d <- length(feats[[1L]])
  n <- as.numeric(length(feats))
  mu <- vapply(seq_len(d), function(j)
    sum(vapply(feats, function(f) f[j], numeric(1))) / n, numeric(1))
  sd <- vapply(seq_len(d), function(j) {
    v <- sum(vapply(feats, function(f) (f[j] - mu[j])^2, numeric(1))) /
      max(n - 1, 1)
    if (v > 1e-24) sqrt(v) else 1
  }, numeric(1))
  list(feats = lapply(feats, function(f) (f - mu) / sd), mu = mu, sd = sd)
}

#' .propinf_apply_standardise
#'
#' A step of the propinf_native implementation. Called by \code{morie_propinf_property_inference}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param f Numeric; combined arithmetically in the body.
#' @param mu Numeric; combined arithmetically in the body.
#' @param sd Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.propinf_apply_standardise <- function(f, mu, sd) {
  (f - mu) / sd
}

# house entry point: the package exports one morie_<module>
morie_propinf <- morie_propinf_train_fcnn
