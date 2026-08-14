# Sources:
#   Wu, Y., DuBois, C., Zheng, A. X. & Ester, M. (2016) "Collaborative
#   Denoising Auto-Encoders for Top-N Recommender Systems", WSDM '16,
#   153-162, doi:10.1145/2835776.2835837.
#   Vincent, P., Larochelle, H., Bengio, Y. & Manzagol, P.-A. (2008)
#   "Extracting and composing robust features with denoising
#   autoencoders", ICML 2008, 1096-1103.
#   Rendle, S., Freudenthaler, C., Gantner, Z. & Schmidt-Thieme, L.
#   (2009) "BPR: Bayesian Personalized Ranking from Implicit Feedback",
#   UAI 2009, 452-461.

.cdae_acts <- c("sigmoid", "identity", "tanh")
.cdae_losses <- c("square", "log", "hinge", "cross_entropy")
.cdae_eps <- 1e-12

.cdae_act <- function(name, x) {
  if (name == "sigmoid") {
    if (x >= -700) return(1.0 / (1.0 + exp(-x)))
    return(0.0)
  }
  if (name == "identity") return(x)
  if (name == "tanh") return(tanh(x))
  stop(sprintf("cdaeRC: activation must be one of %s, got %r",
               paste(.cdae_acts, collapse = ", "), name))
}

.cdae_dact <- function(name, y) {
  if (name == "sigmoid") return(y * (1.0 - y))
  if (name == "identity") return(1.0)
  return(1.0 - y * y)
}

corrupt <- function(y, q, rng) {
  qq <- as.numeric(q)
  if (!(qq >= 0.0 && qq < 1.0))
    stop(sprintf("cdaeRC: q must lie in [0,1), got %r", q))
  d <- 1.0 / (1.0 - qq)
  u <- .ghc_unif(rng, length(y))
  ifelse(u < qq, 0.0, d * as.numeric(y))
}

encode <- function(y_tilde, W, V_u, b, activation = "sigmoid") {
  K <- length(b)
  z <- numeric(K)
  for (f in seq_len(K)) {
    s <- b[f] + V_u[f]
    yt <- y_tilde
    if (any(yt != 0.0)) {
      for (i in which(yt != 0.0)) s <- s + W[[i]][f] * yt[i]
    }
    z[f] <- .cdae_act(activation, s)
  }
  z
}

decode <- function(z, Wp, bp, items = NULL, activation = "sigmoid") {
  if (is.null(items)) idx <- seq_along(bp) else idx <- as.integer(items)
  sapply(idx, function(i) {
    .cdae_act(activation, bp[i] + sum(Wp[[i]] * z))
  })
}

loss <- function(y, y_hat, kind = "square") {
  if (!(kind %in% .cdae_losses))
    stop(sprintf("cdaeRC: loss must be one of %s, got %r",
                 paste(.cdae_losses, collapse = ", "), kind))
  yv <- as.numeric(y); yh <- as.numeric(y_hat)
  if (kind %in% c("log", "hinge") && yv == 0.0)
    stop(sprintf("cdaeRC: the %s loss needs y = -1 for negatives, not 0", kind))
  if (kind == "square") return(0.5 * (yv - yh)^2)
  if (kind == "log") {
    if (-yv * yh < 700) return(log(1.0 + exp(-yv * yh)))
    return(-yv * yh)
  }
  if (kind == "hinge") return(max(0.0, 1.0 - yv * yh))
  if (yh >= -700) p <- 1.0 / (1.0 + exp(-yh)) else p <- 0.0
  p <- min(max(p, .cdae_eps), 1.0 - .cdae_eps)
  -yv * log(p) - (1.0 - yv) * log(1.0 - p)
}

fit_cdae <- function(pos, n_users, n_items, k_dim = 8L, q = 0.2,
                     alpha = 0.05, lam = 0.01, iters = 30L,
                     n_neg = 5L, seed = 0, activation = "sigmoid",
                     init_scale = 0.1) {
  U <- as.integer(n_users); I <- as.integer(n_items); K <- as.integer(k_dim)
  if (U < 1L || I < 2L || K < 1L)
    stop("cdaeRC: need at least 1 user, 2 items and 1 hidden node")
  rng <- .ghc_rng(seed)

  rand <- function() (as.numeric(.ghc_unif(rng, 1L)) - 0.5) * 2.0 * init_scale

  W  <- replicate(I, replicate(K, rand(), simplify = FALSE), simplify = FALSE)
  Wp <- replicate(I, replicate(K, rand(), simplify = FALSE), simplify = FALSE)
  V  <- replicate(U, replicate(K, rand(), simplify = FALSE), simplify = FALSE)
  b  <- rep(0.0, K)
  bp <- rep(0.0, I)
  a <- as.numeric(alpha); lm <- as.numeric(lam)
  hist <- numeric(0)
  for (it in seq_len(as.integer(iters))) {
    tot <- 0.0
    for (u in seq_len(U) - 1L) {
      seen <- sort(unique(as.integer(pos[[as.character(u) + 1L]] %||% pos[[u + 1L]] %||% c())))
      if (length(seen) == 0L) next
      seen_set <- as.integer(seen) + 1L
      y <- ifelse(seq_len(I) %in% seen_set, 1.0, 0.0)
      yt <- corrupt(y, q, rng)
      z <- encode(yt, W, V[[u + 1L]], b, activation)
      neg <- integer(0)
      guard <- 0L
      n_neg_i <- as.integer(n_neg)
      while (length(neg) < n_neg_i && guard < 100L * n_neg_i) {
        j <- as.integer(.ghc_unif(rng, 1L) * I) %% I
        if (!(j %in% seen_set)) neg <- c(neg, j)
        guard <- guard + 1L
      }
      tgt <- c(seen_set, neg)
      out <- decode(z, Wp, bp, tgt, activation)
      dz <- rep(0.0, K)
      for (kk in seq_along(tgt)) {
        i <- tgt[kk]
        yi <- if (i %in% seen_set) 1.0 else 0.0
        e <- (out[kk] - yi) * .cdae_dact(activation, out[kk])
        tot <- tot + loss(yi, out[kk], "square")
        for (f in seq_len(K)) {
          dz[f] <- dz[f] + e * Wp[[i]][f]
          Wp[[i]][f] <- Wp[[i]][f] - a * (e * z[f] + lm * Wp[[i]][f])
        }
        bp[i] <- bp[i] - a * e
      }
      dpre <- dz * .cdae_dact(activation, z)
      nz <- which(yt != 0.0)
      for (i in nz) {
        for (f in seq_len(K)) {
          W[[i]][f] <- W[[i]][f] - a * (dpre[f] * yt[i] + lm * W[[i]][f])
        }
      }
      for (f in seq_len(K)) {
        V[[u + 1L]][f] <- V[[u + 1L]][f] - a * (dpre[f] + lm * V[[u + 1L]][f])
        b[f] <- b[f] - a * dpre[f]
      }
    }
    hist <- c(hist, tot)
  }
  list(
    estimate = list(W, Wp, V, b, bp),
    W = W, W_prime = Wp, V = V, b = b, b_prime = bp,
    loss_history = hist,
    final_loss = if (length(hist)) hist[length(hist)] else NaN,
    k = K, q = as.numeric(q), n_neg = as.integer(n_neg),
    activation = activation,
    method = "CDAE; Wu, DuBois, Zheng & Ester (2016) eqs. (9)-(13), Algorithm 1",
    note = "V_u is the user-specific input node -- without it this is an ordinary denoising auto-encoder over item vectors"
  )
}

recommend <- function(model, pos, u, n_items, top_k = 5L,
                      activation = "sigmoid") {
  W <- model$W; Wp <- model$W_prime; V <- model$V
  b <- model$b; bp <- model$b_prime
  u_int <- as.integer(u)
  seen <- unique(as.integer(pos[[as.character(u_int) + 1L]] %||% pos[[u_int + 1L]] %||% c()))
  seen_set <- seen + 1L
  I <- as.integer(n_items)
  y <- ifelse(seq_len(I) %in% seen_set, 1.0, 0.0)
  z <- encode(y, W, V[[u_int + 1L]], b, activation)
  out <- decode(z, Wp, bp, NULL, activation)
  s <- data.frame(i = seq_len(I) - 1L, score = out, stringsAsFactors = FALSE)
  s <- s[!(s$i + 1L) %in% seen_set, , drop = FALSE]
  s <- s[order(-s$score), , drop = FALSE]
  top_k_i <- as.integer(top_k)
  ranking <- lapply(seq_len(min(top_k_i, nrow(s))), function(k) c(s$i[k], s$score[k]))
  list(ranking = ranking, n_scored = nrow(s))
}

morie_cdaeRC <- function(pos, n_users, n_items, k_dim = 8L, q = 0.2,
                         alpha = 0.05, lam = 0.01, iters = 30L,
                         n_neg = 5L, seed = 0, activation = "sigmoid",
                         init_scale = 0.1) {
  fit_cdae(pos, n_users, n_items, k_dim, q, alpha, lam, iters, n_neg,
           seed, activation, init_scale)
}
