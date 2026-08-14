# narm -- neural attentive session-based recommendation
# Reference: Li et al. (2017) "NARM" CIKM 2017, arXiv:1711.04725
# Base R only.

narm_softmax <- function(z) {
  v <- as.numeric(z)
  m <- max(v)
  e <- exp(v - m)
  e / sum(e)
}

narm_attention_weights <- function(h_t, H, A1, A2, v) {
  ht <- as.numeric(h_t)
  Hm <- as.matrix(H)
  if (nrow(Hm) == 0L) return(numeric(0))
  sc <- numeric(nrow(Hm))
  A1 <- as.matrix(A1); A2 <- as.matrix(A2)
  v <- as.numeric(v)
  for (j in seq_len(nrow(Hm))) {
    hj <- Hm[j, ]
    z <- plogis(A1 %*% ht + A2 %*% hj)
    sc[j] <- sum(v * z)
  }
  narm_softmax(sc)
}

narm_local_encoder <- function(H, alpha) {
  Hm <- as.matrix(H)
  a <- as.numeric(alpha)
  if (length(a) != nrow(Hm)) {
    stop(sprintf("narm: %d weights for %d hidden states", length(a), nrow(Hm)))
  }
  as.numeric(crossprod(a, Hm))
}

narm_session_repr <- function(h_t_global, c_local) {
  c(as.numeric(h_t_global), as.numeric(c_local))
}

narm_bilinear_scores <- function(embeddings, B, c_t) {
  E <- as.matrix(embeddings)
  B <- as.matrix(B)
  c_ <- as.numeric(c_t)
  if (ncol(B) != length(c_)) {
    stop(sprintf("narm: B has %d columns for a session vector of %d",
                 ncol(B), length(c_)))
  }
  Bc <- as.numeric(B %*% c_)
  if (ncol(E) != length(Bc)) {
    stop(sprintf("narm: embeddings are %d-dimensional but B has %d rows",
                 ncol(E), length(Bc)))
  }
  s <- as.numeric(E %*% Bc)
  list(estimate = s, scores = s, probabilities = narm_softmax(s),
       method = "bilinear decoder; Li et al. (2017) eq. (10)",
       note = "|D||H| parameters instead of |N||H|, and the paper reports better accuracy too")
}

narm_decoder_parameters <- function(n_items, hidden, emb_dim) {
  N <- as.integer(n_items); H <- as.integer(hidden); D <- as.integer(emb_dim)
  if (min(N, H, D) < 1L) stop("narm: all three sizes must be at least 1")
  list(fully_connected = N * H, bilinear = D * H,
       ratio = (N * H) / (D * H),
       note = "|D| is usually far smaller than |N|")
}

narm_cheatsheet <- function() {
  paste("narm: a purely sequential session model recommends trousers because the shopper clicked a pair by accident. Two encoders over the SAME GRU states: the global one takes h_t as the whole-behaviour summary, the local one attends over previous states to capture the session's MAIN PURPOSE. h_t^g and h_t^l have identical values and different roles. Concatenate, then score with a BILINEAR decoder emb_i' B c_t -- |D||H| parameters instead of |N||H|, and more accurate.")
}

# house entry point: the package exports one morie_<module>
morie_narm <- narm_softmax
