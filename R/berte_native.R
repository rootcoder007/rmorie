# berte -- BERT: the bidirectional encoder forward pass.
# Devlin et al. (2019); Vaswani et al. (2017); Ba et al. (2016); Hendrycks & Gimpel.
# Base R only.

.berte_EPS <- 1e-12
.berte_NEG <- -1e9

gelu <- function(x) {
  # GELU exact using erf; avoid pnorm dependency for portability
  x <- as.numeric(x)
  0.5 * x * (1 + sapply(x, function(v) {
    # erf approximation (Abramowitz & Stegun 7.1.26)
    sign_v <- sign(v)
    av <- abs(v)
    t_ <- 1 / (1 + 0.3275911 * av)
    a1 <-  0.254829592; a2 <- -0.284496736
    a3 <-  1.421413741; a4 <- -1.453152027
    a5 <-  1.061405429
    y <- 1 - (((((a5 * t_ + a4) * t_) + a3) * t_ + a2) * t_ + a1) * t_ * exp(-av * av)
    sign_v * y
  }))
}

layer_norm <- function(x, gain = NULL, bias = NULL, eps = 1e-12) {
  x <- as.numeric(x)
  d <- length(x)
  if (d == 0) stop("berte: empty vector")
  mu <- mean(x)
  var <- mean((x - mu)^2)
  inv <- 1 / sqrt(var + eps)
  out <- (x - mu) * inv
  if (!is.null(gain)) {
    gain <- as.numeric(gain)
    if (length(gain) != d) stop("berte: gain has wrong length")
    out <- out * gain
  }
  if (!is.null(bias)) {
    bias <- as.numeric(bias)
    if (length(bias) != d) stop("berte: bias has wrong length")
    out <- out + bias
  }
  out
}

.proj <- function(row, W, b = NULL) {
  W <- as.matrix(W); storage.mode(W) <- "double"
  v <- as.numeric(W) %*% as.numeric(row)
  if (is.null(b)) v else v + as.numeric(b)
}

attention_weights <- function(Q, K, n_heads, pad_mask = NULL, causal = FALSE) {
  Q <- as.matrix(Q); storage.mode(Q) <- "double"
  K <- as.matrix(K); storage.mode(K) <- "double"
  L <- nrow(Q); d <- ncol(Q)
  if (d %% n_heads != 0)
    stop(sprintf("berte: dimension %d is not divisible by %d heads",
                 d, n_heads))
  hd <- d / n_heads
  scale <- 1 / sqrt(hd)
  heads <- vector("list", n_heads)
  for (h in 1:n_heads) {
    Qh <- Q[, ((h - 1) * hd + 1):(h * hd), drop = FALSE]
    Kh <- K[, ((h - 1) * hd + 1):(h * hd), drop = FALSE]
    scores <- scale * (Qh %*% t(Kh))
    if (!is.null(pad_mask)) {
      bad <- !pad_mask
      scores[, bad] <- .berte_NEG
    }
    if (causal) {
      # j > i forbidden: rows are queries (i), columns are keys (j)
      scores[upper.tri(scores)] <- .berte_NEG
    }
    mx <- apply(scores, 1, max)
    e <- exp(scores - mx)
    tot <- rowSums(e)
    heads[[h]] <- e / tot
  }
  heads
}

multi_head_attention <- function(Q, K, V, n_heads, pad_mask = NULL,
                                 causal = FALSE) {
  Q <- as.matrix(Q); storage.mode(Q) <- "double"
  V <- as.matrix(V); storage.mode(V) <- "double"
  L <- nrow(Q); d <- ncol(Q)
  hd <- d / n_heads
  w <- attention_weights(Q, K, n_heads, pad_mask = pad_mask, causal = causal)
  out <- matrix(0, L, d)
  for (h in 1:n_heads) {
    Vh <- V[, ((h - 1) * hd + 1):(h * hd), drop = FALSE]
    out[, ((h - 1) * hd + 1):(h * hd)] <- w[[h]] %*% Vh
  }
  list(out = out, weights = w)
}

encoder_block <- function(X, Wq, Wk, Wv, Wo, W1, b1, W2, b2, n_heads,
                          pad_mask = NULL, gain1 = NULL, bias1 = NULL,
                          gain2 = NULL, bias2 = NULL, pre_norm = FALSE) {
  Xm <- as.matrix(X); storage.mode(Xm) <- "double"
  L <- nrow(Xm); d <- ncol(Xm)
  if (pre_norm) {
    src <- t(apply(Xm, 1, layer_norm, gain = gain1, bias = bias1))
  } else {
    src <- Xm
  }
  Q <- t(apply(src, 1, .proj, Wq))
  K <- t(apply(src, 1, .proj, Wk))
  V <- t(apply(src, 1, .proj, Wv))
  mha <- multi_head_attention(Q, K, V, n_heads, pad_mask = pad_mask)
  a <- t(apply(mha$out, 1, .proj, Wo))
  x1 <- Xm + a
  if (!pre_norm) x1 <- t(apply(x1, 1, layer_norm, gain = gain1, bias = bias1))
  if (pre_norm) {
    src2 <- t(apply(x1, 1, layer_norm, gain = gain2, bias = bias2))
  } else {
    src2 <- x1
  }
  f <- t(apply(src2, 1, .proj, W1, b1))
  f <- matrix(gelu(f), nrow = L, ncol = ncol(f))
  f <- t(apply(f, 1, .proj, W2, b2))
  x2 <- x1 + f
  if (!pre_norm) x2 <- t(apply(x2, 1, layer_norm, gain = gain2, bias = bias2))
  list(out = x2, weights = mha$weights)
}

bert_encoder <- function(X, blocks, n_heads, pad_mask = NULL,
                         pre_norm = FALSE) {
  cur <- as.matrix(X); storage.mode(cur) <- "double"
  attn <- list()
  for (b in blocks) {
    eb <- encoder_block(cur, b[[1]], b[[2]], b[[3]], b[[4]],
                        b[[5]], b[[6]], b[[7]], b[[8]], n_heads,
                        pad_mask = pad_mask, gain1 = b$gain1,
                        bias1 = b$bias1, gain2 = b$gain2,
                        bias2 = b$bias2, pre_norm = pre_norm)
    cur <- eb$out
    attn[[length(attn) + 1L]] <- eb$weights
  }
  L <- nrow(cur)
  list(estimate = cur, output = cur, attention = attn,
       pooled = cur[1, ], L = L, d = ncol(cur),
       n_layers = length(blocks), n_heads = n_heads,
       pre_norm = as.logical(pre_norm), bidirectional = TRUE,
       method = "BERT encoder forward pass, Devlin et al. (2019)")
}

bertencoder <- bert_encoder

# house entry point: the package exports one morie_<module>
morie_berte <- bert_encoder
