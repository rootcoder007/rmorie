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

#' .cdae_act
#'
#' A step of the cdaeRC_native implementation. Called by \code{decode}, \code{encode}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param name One of \code{"identity"}, \code{"sigmoid"}, \code{"tanh"}.
#' @param x Numeric; combined arithmetically in the body.
#' @return Nothing; this branch always raises.
#' @export
.cdae_act <- function(name, x) {
  if (name == "sigmoid") {
    # vectorised clamp: the scalar if() errors on vector activations
    return(1.0 / (1.0 + exp(-pmax(x, -700))))
  }
  if (name == "identity") return(x)
  if (name == "tanh") return(tanh(x))
  stop(sprintf("cdaeRC: activation must be one of %s, got %r",
               paste(.cdae_acts, collapse = ", "), name))
}

#' .cdae_dact
#'
#' A step of the cdaeRC_native implementation. Called by \code{fit_cdae}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param name One of \code{"identity"}, \code{"sigmoid"}.
#' @param y Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.cdae_dact <- function(name, y) {
  if (name == "sigmoid") return(y * (1.0 - y))
  if (name == "identity") return(1.0)
  return(1.0 - y * y)
}

#' corrupt
#'
#' A step of the cdaeRC_native implementation. Called by \code{fit_cdae}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y A vector; its length is taken.
#' @param q Coerced to numeric by the body, with \code{as.numeric}.
#' @param rng Passed to \code{.ghc_unif}.
#' @return The value of \code{ifelse}.
#' @export
corrupt <- function(y, q, rng) {
  qq <- as.numeric(q)
  if (!(qq >= 0.0 && qq < 1.0))
    stop(sprintf("cdaeRC: q must lie in [0,1), got %r", q))
  d <- 1.0 / (1.0 - qq)
  u <- .ghc_unif(rng, length(y))
  ifelse(u < qq, 0.0, d * as.numeric(y))
}

#' encode
#'
#' A step of the cdaeRC_native implementation. Called by \code{fit_cdae}, \code{recommend}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y_tilde See Usage.
#' @param W A vector; indexed elementwise.
#' @param V_u A vector; indexed elementwise.
#' @param b A vector; its length is taken and its elements indexed.
#' @param activation Passed to \code{.cdae_act}. Defaults to \code{"sigmoid"}.
#' @return The value of \code{z}, as built in the body.
#' @export
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

#' decode
#'
#' A step of the cdaeRC_native implementation. Called by \code{fit_cdae}, \code{recommend}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param z Numeric; combined arithmetically in the body.
#' @param Wp A vector; indexed elementwise.
#' @param bp A vector; its length is taken and its elements indexed.
#' @param items Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @param activation Passed to \code{.cdae_act}. Defaults to \code{"sigmoid"}.
#' @return A vector, from \code{sapply}.
#' @export
decode <- function(z, Wp, bp, items = NULL, activation = "sigmoid") {
  if (is.null(items)) idx <- seq_along(bp) else idx <- as.integer(items)
  sapply(idx, function(i) {
    .cdae_act(activation, bp[i] + sum(Wp[[i]] * z))
  })
}

#' loss
#'
#' A step of the cdaeRC_native implementation. Called by \code{.plcbsc_synthetic_control}, \code{.tlroad_score_spans_eic}, \code{fit_cdae} and 2 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @param y_hat Coerced to numeric by the body, with \code{as.numeric}.
#' @param kind One of \code{"hinge"}, \code{"log"}, \code{"square"}. Defaults to \code{"square"}.
#' @return A numeric value.
#' @export
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

#' fit_cdae
#'
#' A step of the cdaeRC_native implementation. Called by \code{morie_cdaeRC}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param pos A vector; indexed elementwise.
#' @param n_users Coerced to integer by the body, with \code{as.integer}.
#' @param n_items Coerced to integer by the body, with \code{as.integer}.
#' @param k_dim Coerced to integer by the body, with \code{as.integer}. Defaults to \code{8L}.
#' @param q Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.2}.
#' @param alpha Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.05}.
#' @param lam Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.01}.
#' @param iters Coerced to integer by the body, with \code{as.integer}. Defaults to \code{30L}.
#' @param n_neg Coerced to integer by the body, with \code{as.integer}. Defaults to \code{5L}.
#' @param seed Passed to \code{.ghc_rng}. Defaults to \code{0}.
#' @param activation Passed to \code{.cdae_dact}. Defaults to \code{"sigmoid"}.
#' @param init_scale Numeric; combined arithmetically in the body. Defaults to \code{0.1}.
#' @return A list with \code{estimate}, \code{W}, \code{W_prime}, \code{V}, \code{b}, \code{b_prime}, \code{loss_history}, \code{final_loss}, \code{k}, \code{q}, \code{n_neg}, \code{activation}, \code{method}, \code{note}.
#' @export
fit_cdae <- function(pos, n_users, n_items, k_dim = 8L, q = 0.2,
                     alpha = 0.05, lam = 0.01, iters = 30L,
                     n_neg = 5L, seed = 0, activation = "sigmoid",
                     init_scale = 0.1) {
  U <- as.integer(n_users); I <- as.integer(n_items); K <- as.integer(k_dim)
  if (U < 1L || I < 2L || K < 1L)
    stop("cdaeRC: need at least 1 user, 2 items and 1 hidden node")
  rng <- .ghc_rng(seed)

  rand <- function() (as.numeric(.ghc_unif(rng, 1L)) - 0.5) * 2.0 * init_scale

  # inner replicate must SIMPLIFY: with simplify = FALSE each weight
  # row was a list of scalar lists and every update was numeric * list
  W  <- replicate(I, replicate(K, rand()), simplify = FALSE)
  Wp <- replicate(I, replicate(K, rand()), simplify = FALSE)
  V  <- replicate(U, replicate(K, rand()), simplify = FALSE)
  b  <- rep(0.0, K)
  bp <- rep(0.0, I)
  a <- as.numeric(alpha); lm <- as.numeric(lam)
  hist <- numeric(0)
  for (it in seq_len(as.integer(iters))) {
    tot <- 0.0
    for (u in seq_len(U) - 1L) {
      seen_raw <- if (!is.null(names(pos))) pos[[as.character(u)]] else pos[[u + 1L]]
      seen <- sort(unique(as.integer(seen_raw %||% c())))
      if (length(seen) == 0L) next
      seen_set <- as.integer(seen) + 1L
      y <- ifelse(seq_len(I) %in% seen_set, 1.0, 0.0)
      yt <- corrupt(y, q, rng)
      z <- encode(yt, W, V[[u + 1L]], b, activation)
      neg <- integer(0)
      guard <- 0L
      n_neg_i <- as.integer(n_neg)
      while (length(neg) < n_neg_i && guard < 100L * n_neg_i) {
        # 1-based, to match seen_set: the 0-based draw put item 0 into
        # tgt and Wp[[0]] cannot exist
        j <- as.integer(.ghc_unif(rng, 1L) * I) %% I + 1L
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

#' recommend
#'
#' A step of the cdaeRC_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param model A list; the body reads \code{$b}, \code{$b_prime}, \code{$V}, \code{$W}, \code{$W_prime} from it.
#' @param pos A vector; indexed elementwise.
#' @param u Coerced to integer by the body, with \code{as.integer}.
#' @param n_items Coerced to integer by the body, with \code{as.integer}.
#' @param top_k Coerced to integer by the body, with \code{as.integer}. Defaults to \code{5L}.
#' @param activation Passed to \code{encode}. Defaults to \code{"sigmoid"}.
#' @return A list with \code{ranking}, \code{n_scored}.
#' @export
recommend <- function(model, pos, u, n_items, top_k = 5L,
                      activation = "sigmoid") {
  W <- model$W; Wp <- model$W_prime; V <- model$V
  b <- model$b; bp <- model$b_prime
  u_int <- as.integer(u)
  seen <- unique(as.integer((if (!is.null(names(pos))) pos[[as.character(u_int)]] else pos[[u_int + 1L]]) %||% c()))
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

#' morie_cdaeRC
#'
#' A step of the cdaeRC_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param pos Passed to \code{fit_cdae}.
#' @param n_users Passed to \code{fit_cdae}.
#' @param n_items Passed to \code{fit_cdae}.
#' @param k_dim Passed to \code{fit_cdae}. Defaults to \code{8L}.
#' @param q Passed to \code{fit_cdae}. Defaults to \code{0.2}.
#' @param alpha Passed to \code{fit_cdae}. Defaults to \code{0.05}.
#' @param lam Passed to \code{fit_cdae}. Defaults to \code{0.01}.
#' @param iters Passed to \code{fit_cdae}. Defaults to \code{30L}.
#' @param n_neg Passed to \code{fit_cdae}. Defaults to \code{5L}.
#' @param seed Passed to \code{fit_cdae}. Defaults to \code{0}.
#' @param activation Passed to \code{fit_cdae}. Defaults to \code{"sigmoid"}.
#' @param init_scale Passed to \code{fit_cdae}. Defaults to \code{0.1}.
#' @return The value of \code{fit_cdae}.
#' @export
morie_cdaeRC <- function(pos, n_users, n_items, k_dim = 8L, q = 0.2,
                         alpha = 0.05, lam = 0.01, iters = 30L,
                         n_neg = 5L, seed = 0, activation = "sigmoid",
                         init_scale = 0.1) {
  fit_cdae(pos, n_users, n_items, k_dim, q, alpha, lam, iters, n_neg,
           seed, activation, init_scale)
}
