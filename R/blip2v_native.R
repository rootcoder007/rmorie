# blip2v -- BLIP-2: bootstrapping language-image pre-training with
# frozen image encoders and large language models.
# Li, Li, Savarese & Hoi (2023) ICML, arXiv:2301.12597.
# Base R only.

.EPS <- 1e-12
.STAGES <- c(1, 2)

query_tokens <- function(n_queries, dim, seed = 0, scale = 0.02) {
  n <- as.integer(n_queries); d <- as.integer(dim)
  if (n < 1 || d < 1)
    stop("blip2v: the query count and dimension must be positive")
  set.seed(as.integer(seed))
  matrix((runif(n * d) - 0.5) * 2 * scale, nrow = n, ncol = d)
}

qformer_attend <- function(queries, image_features, WQ, WK, WV) {
  Q <- as.matrix(queries); storage.mode(Q) <- "double"
  F <- as.matrix(image_features); storage.mode(F) <- "double"
  dk <- nrow(WQ)
  if (dk <= 0) stop("blip2v: empty projection")
  proj <- function(W, x) as.numeric(W %*% x)
  out <- matrix(0, nrow(Q), ncol(WV))
  weights <- matrix(0, nrow(Q), nrow(F))
  for (i in seq_len(nrow(Q))) {
    qq <- proj(WQ, Q[i, ])
    sc <- numeric(nrow(F))
    for (j in seq_len(nrow(F))) {
      kj <- proj(WK, F[j, ])
      sc[j] <- sum(qq * kj) / sqrt(dk)
    }
    m <- max(sc)
    e <- exp(sc - m)
    z <- sum(e)
    w <- e / z
    weights[i, ] <- w
    Vs <- t(apply(F, 1, function(f) proj(WV, f)))
    out[i, ] <- as.numeric(t(w) %*% Vs)
  }
  list(output = out, weights = weights,
       n_queries = nrow(Q), n_patches = nrow(F),
       compression = nrow(F) / nrow(Q),
       note = "the output width is the QUERY count, whatever the image resolution")
}

trainable_fraction <- function(qformer_params, frozen_vision_params,
                               frozen_llm_params) {
  q <- as.numeric(qformer_params)
  tot <- q + as.numeric(frozen_vision_params) + as.numeric(frozen_llm_params)
  if (tot <= 0)
    stop("blip2v: the parameter counts must be positive")
  list(trainable = q, total = tot, fraction = q / tot,
       frozen_fraction = 1 - q / tot,
       note = "vision encoder and language model both frozen")
}

stage_one_objectives <- function(query_out, text_out, temperature = 0.07) {
  Q <- as.matrix(query_out); storage.mode(Q) <- "double"
  T_ <- as.numeric(text_out)
  t <- as.numeric(temperature)
  if (t <= 0) stop("blip2v: the temperature must be positive")
  cos_sim <- function(a, b) {
    na <- sqrt(sum(a^2)); nb <- sqrt(sum(b^2))
    if (na <= .EPS || nb <= .EPS)
      stop("blip2v: a zero embedding has no direction")
    sum(a * b) / (na * nb)
  }
  sims <- numeric(nrow(Q))
  for (i in seq_len(nrow(Q))) sims[i] <- cos_sim(Q[i, ], T_)
  list(per_query_similarity = sims,
       image_text_similarity = max(sims),
       best_query = which.max(sims),
       logit = max(sims) / t,
       note = "the image-text score is the MAXIMUM over queries, not the mean")
}

project_to_llm <- function(query_out, W, b = NULL) {
  Q <- as.matrix(query_out); storage.mode(Q) <- "double"
  W <- as.matrix(W); storage.mode(W) <- "double"
  d_out <- nrow(W)
  bb <- if (is.null(b)) rep(0, d_out) else as.numeric(b)
  if (ncol(W) != ncol(Q))
    stop(sprintf("blip2v: the projection expects %d inputs but the query output is %d",
                 ncol(W), ncol(Q)))
  out <- Q %*% t(W) + matrix(bb, nrow = nrow(Q), ncol = d_out,
                              byrow = TRUE)
  list(estimate = out, soft_prompt = out,
       n_tokens = nrow(out), dim = d_out,
       method = "BLIP-2 two-stage bridging; Li, Li, Savarese & Hoi (2023)",
       note = "the projected queries act as a soft prompt prefixed to the frozen LLM's input")
}

blip2 <- qformer_attend
blip2_qformer <- qformer_attend
blip2qformer <- qformer_attend

# house entry point: the package exports one morie_<module>
morie_blip2v <- qformer_attend
