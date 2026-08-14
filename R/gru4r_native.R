# GRU4Rec: session-based recommendation with a ranking loss.
# Sources: Hidasi, B., Karatzoglou, A., Baltrunas, L. and Tikk, D.
# (2016), Session-based Recommendations with Recurrent Neural
# Networks, ICLR 2016 (arXiv:1511.06939) -- the session-parallel
# mini-batch, the BPR and TOP1 ranking losses, and the finding that
# cross-entropy was stable in only 10 of 100 runs; Rendle, S. et al.
# (2009), BPR: Bayesian Personalized Ranking from Implicit Feedback,
# UAI 2009 -- the BPR loss; Cho, K. et al. (2014), Learning Phrase
# Representations using RNN Encoder-Decoder, EMNLP 2014 -- the GRU.
#
# Native implementation mirroring Python morie.fn.gru4r exactly: the
# same session-parallel slotting with a reset flag when a session is
# replaced, the same BPR loss as the smoothed log-sigmoid of
# (target - negative), the same TOP1 loss as smoothed relative rank
# plus the sigma(r_neg^2) regulariser, and the standard GRU update.

.GRU4R_EPS <- 1e-12

# Numerically stable sigmoid matching the Python helper.
.gh_sig <- function(x) {
  if (x > -700) 1 / (1 + exp(-x)) else 0
}

#' Build session-parallel mini-batches
#'
#' @param sessions List of integer sequences of length >= 2.
#' @param batch_size Number of parallel slots.
#' @return List with steps, n_steps, batch_size, n_sessions, note.
#' @export
session_parallel_batches <- function(sessions, batch_size) {
  S <- lapply(sessions, as.integer)
  if (any(vapply(S, length, integer(1)) < 2L))
    stop("gru4r: every session needs at least 2 events")
  B <- as.integer(batch_size)
  if (B < 1L || B > length(S))
    stop(paste0("gru4r: batch_size must lie in 1..", length(S),
                ", got ", B))
  slot <- seq_len(B) - 1L
  pos <- rep(0L, B)
  nxt <- B
  steps <- list()
  repeat {
    x <- vector("list", B); y <- vector("list", B)
    reset <- rep(FALSE, B); alive <- FALSE
    for (b in seq_len(B)) {
      if (is.na(slot[b])) { x[[b]] <- NA; y[[b]] <- NA; next }
      s <- S[[slot[b] + 1L]]
      if (pos[b] + 1L >= length(s)) {
        if (nxt < length(S)) {
          slot[b] <- nxt; pos[b] <- 0L; nxt <- nxt + 1L
          reset[b] <- TRUE
          s <- S[[slot[b] + 1L]]
        } else {
          slot[b] <- NA; x[[b]] <- NA; y[[b]] <- NA; next
        }
      }
      x[[b]] <- s[pos[b] + 1L]; y[[b]] <- s[pos[b] + 2L]
      pos[b] <- pos[b] + 1L
      alive <- TRUE
    }
    if (!alive) break
    steps[[length(steps) + 1L]] <- list(input = x, target = y,
                                        reset = reset)
  }
  list(steps = steps, n_steps = length(steps), batch_size = B,
       n_sessions = length(S),
       note = "a slot's hidden state is reset when a new session takes it, because sessions are assumed independent")
}

#' BPR ranking loss
#' @param r_target Target score.
#' @param r_negatives Vector of negative scores.
#' @return Scalar loss.
#' @export
bpr_loss <- function(r_target, r_negatives) {
  neg <- as.numeric(r_negatives)
  if (length(neg) == 0L)
    stop("gru4r: at least one negative is needed")
  r <- as.numeric(r_target)
  -sum(log(pmax(.gh_sig(r - neg), .GRU4R_EPS))) / length(neg)
}

#' TOP1 ranking loss
#' @param r_target Target score.
#' @param r_negatives Vector of negative scores.
#' @param regularize If TRUE, include the sigma(r_neg^2) term.
#' @return Scalar loss.
#' @export
top1_loss <- function(r_target, r_negatives, regularize = TRUE) {
  neg <- as.numeric(r_negatives)
  if (length(neg) == 0L)
    stop("gru4r: at least one negative is needed")
  r <- as.numeric(r_target)
  rank <- sum(.gh_sig(neg - r)) / length(neg)
  if (!regularize) return(rank)
  rank + sum(.gh_sig(neg * neg)) / length(neg)
}

#' One GRU update
#' @param x Input vector.
#' @param h Hidden state.
#' @param Wz Update gate input weight matrix.
#' @param Uz Update gate recurrent weight matrix.
#' @param Wr Reset gate input weight matrix.
#' @param Ur Reset gate recurrent weight matrix.
#' @param Wh Candidate input weight matrix.
#' @param Uh Candidate recurrent weight matrix.
#' @return New hidden state.
#' @export
gru_step <- function(x, h, Wz, Uz, Wr, Ur, Wh, Uh) {
  n <- length(h)
  xv <- as.numeric(x); hv <- as.numeric(h)
  Wz <- as.matrix(Wz); Uz <- as.matrix(Uz)
  Wr <- as.matrix(Wr); Ur <- as.matrix(Ur)
  Wh <- as.matrix(Wh); Uh <- as.matrix(Uh)
  z <- .gh_sig(as.numeric(Wz %*% xv + Uz %*% hv))
  r <- .gh_sig(as.numeric(Wr %*% xv + Ur %*% hv))
  hh <- tanh(as.numeric(Wh %*% xv + Uh %*% (r * hv)))
  (1 - z) * hv + z * hh
}

#' Recall at k
#' @param ranked Integer vector of ranked items.
#' @param target Target item.
#' @param kk Cutoff.
#' @return 1 if target in top kk, else 0.
#' @export
recall_at_k <- function(ranked, target, kk = 20L) {
  kk <- as.integer(kk)
  top <- as.integer(ranked)[seq_len(min(kk, length(ranked)))]
  if (as.integer(target) %in% top) 1 else 0
}

#' MRR at k
#' @inheritParams recall_at_k
#' @return Reciprocal rank, 0 if not in top k.
#' @export
mrr_at_k <- function(ranked, target, kk = 20L) {
  kk <- as.integer(kk)
  top <- as.integer(ranked)[seq_len(min(kk, length(ranked)))]
  if (as.integer(target) %in% top) 1 / (which(top == as.integer(target))[1])
  else 0
}

# Compact aliases
#' @export
gruforrecommendation <- session_parallel_batches
#' @export
gru4rec <- session_parallel_batches

# house entry point: the package exports one morie_<module>
morie_gru4r <- session_parallel_batches
