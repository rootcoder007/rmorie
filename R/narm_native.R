# narm -- neural attentive session-based recommendation
# Reference: Li et al. (2017) "NARM" CIKM 2017, arXiv:1711.04725
# Base R only.

#' narm_softmax
#'
#' Part of the narm_native implementation; see the file header for the
#' source it follows.
#'
#' @param z See Usage.
#' @return A numeric value.
#' @export
narm_softmax <- function(z) {
  v <- as.numeric(z)
  m <- max(v)
  e <- exp(v - m)
  e / sum(e)
}

#' narm_attention_weights
#'
#' Part of the narm_native implementation; see the file header for the
#' source it follows.
#'
#' @param h_t See Usage.
#' @param H See Usage.
#' @param A1 See Usage.
#' @param A2 See Usage.
#' @param v See Usage.
#' @return The value of \code{narm_softmax}.
#' @export
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

#' narm_local_encoder
#'
#' Part of the narm_native implementation; see the file header for the
#' source it follows.
#'
#' @param H See Usage.
#' @param alpha See Usage.
#' @return A vector, from \code{as.numeric}.
#' @export
narm_local_encoder <- function(H, alpha) {
  Hm <- as.matrix(H)
  a <- as.numeric(alpha)
  if (length(a) != nrow(Hm)) {
    stop(sprintf("narm: %d weights for %d hidden states", length(a), nrow(Hm)))
  }
  as.numeric(crossprod(a, Hm))
}

#' narm_session_repr
#'
#' Part of the narm_native implementation; see the file header for the
#' source it follows.
#'
#' @param h_t_global See Usage.
#' @param c_local See Usage.
#' @return A vector, from \code{c}.
#' @export
narm_session_repr <- function(h_t_global, c_local) {
  c(as.numeric(h_t_global), as.numeric(c_local))
}

#' narm_bilinear_scores
#'
#' Part of the narm_native implementation; see the file header for the
#' source it follows.
#'
#' @param embeddings See Usage.
#' @param B See Usage.
#' @param c_t See Usage.
#' @return A list with \code{estimate}, \code{scores}, \code{probabilities}, \code{method}, \code{note}.
#' @export
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

#' narm_decoder_parameters
#'
#' Part of the narm_native implementation; see the file header for the
#' source it follows.
#'
#' @param n_items See Usage.
#' @param hidden See Usage.
#' @param emb_dim See Usage.
#' @return A list with \code{fully_connected}, \code{bilinear}, \code{ratio}, \code{note}.
#' @export
narm_decoder_parameters <- function(n_items, hidden, emb_dim) {
  N <- as.integer(n_items); H <- as.integer(hidden); D <- as.integer(emb_dim)
  if (min(N, H, D) < 1L) stop("narm: all three sizes must be at least 1")
  list(fully_connected = N * H, bilinear = D * H,
       ratio = (N * H) / (D * H),
       note = "|D| is usually far smaller than |N|")
}

#' narm_cheatsheet
#'
#' Part of the narm_native implementation; see the file header for the
#' source it follows.
#'
#' @return A character value.
#' @export
narm_cheatsheet <- function() {
  paste("narm: a purely sequential session model recommends trousers because the shopper clicked a pair by accident. Two encoders over the SAME GRU states: the global one takes h_t as the whole-behaviour summary, the local one attends over previous states to capture the session's MAIN PURPOSE. h_t^g and h_t^l have identical values and different roles. Concatenate, then score with a BILINEAR decoder emb_i' B c_t -- |D||H| parameters instead of |N||H|, and more accurate.")
}

# house entry point: the package exports one morie_<module>
morie_narm <- narm_softmax
