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
#' Numerically stable sigmoid matching the Python helper
#'
#' A step of the gru4r_native implementation. Called by \code{bpr_loss}, \code{gru_step},
#' \code{top1_loss}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @return One of two values, depending on the branch taken.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .gh_sig(x = x)
#' res
.gh_sig <- function(x) {
  # vectorised clamp: the scalar if() errors on any vector input
  xc <- pmax(x, -700)
  1 / (1 + exp(-xc))
}

#' Build session-parallel mini-batches
#'
#' @param sessions List of integer sequences of length >= 2.
#' @param batch_size Number of parallel slots.
#' @return List with steps, n_steps, batch_size, n_sessions, note.
#' @export
#' @examples
#' session_parallel_batches(list(c(1, 2, 3), c(4, 5), c(6, 7, 8, 9)), 2)
#' @keywords internal
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
    x <- vector("list", B)
    y <- vector("list", B)
    reset <- rep(FALSE, B)
    alive <- FALSE
    for (b in seq_len(B)) {
      if (is.na(slot[b])) { x[[b]] <- NA
      y[[b]] <- NA
      next }
      s <- S[[slot[b] + 1L]]
      if (pos[b] + 1L >= length(s)) {
        if (nxt < length(S)) {
          slot[b] <- nxt
          pos[b] <- 0L
          nxt <- nxt + 1L
          reset[b] <- TRUE
          s <- S[[slot[b] + 1L]]
        } else {
          slot[b] <- NA
          x[[b]] <- NA
          y[[b]] <- NA
          next
        }
      }
      x[[b]] <- s[pos[b] + 1L]
      y[[b]] <- s[pos[b] + 2L]
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
#' @examples
#' bpr_loss(r_target = 5L, r_negatives = 5L)
#' @keywords internal
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
#' @examples
#' top1_loss(r_target = 5L, r_negatives = 5L)
#' @keywords internal
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
#' @examples
#' set.seed(1)
#' h <- c(0, 0)
#' x <- c(1, -1)
#' W <- matrix(rnorm(4), 2, 2)
#' gru_step(x, h, W, W, W, W, W, W)
#' @keywords internal
gru_step <- function(x, h, Wz, Uz, Wr, Ur, Wh, Uh) {
  n <- length(h)
  xv <- as.numeric(x)
  hv <- as.numeric(h)
  Wz <- as.matrix(Wz)
  Uz <- as.matrix(Uz)
  Wr <- as.matrix(Wr)
  Ur <- as.matrix(Ur)
  Wh <- as.matrix(Wh)
  Uh <- as.matrix(Uh)
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
#' @examples
#' recall_at_k(ranked = c(1, 2, 3, 4, 5, 6, 7, 8), target = 5L)
#' @keywords internal
recall_at_k <- function(ranked, target, kk = 20L) {
  kk <- as.integer(kk)
  top <- as.integer(ranked)[seq_len(min(kk, length(ranked)))]
  if (as.integer(target) %in% top) 1 else 0
}

#' MRR at k
#' @inheritParams recall_at_k
#' @return Reciprocal rank, 0 if not in top k.
#' @export
#' @examples
#' mrr_at_k(ranked = c(1, 2, 3, 4, 5, 6, 7, 8), target = 5L)
#' @keywords internal
mrr_at_k <- function(ranked, target, kk = 20L) {
  kk <- as.integer(kk)
  top <- as.integer(ranked)[seq_len(min(kk, length(ranked)))]
  if (as.integer(target) %in% top) 1 / (which(top == as.integer(target))[1])
  else 0
}

# Compact aliases
#' @rdname session_parallel_batches
#' @export
gruforrecommendation <- session_parallel_batches
#' @rdname session_parallel_batches
#' @export
gru4rec <- session_parallel_batches

# house entry point: the package exports one morie_<module>
morie_gru4r <- session_parallel_batches

# -- restored: morie-only definition kept through the rmorie sync --
#' Numerically stable sigmoid: below -700 it is effectively 0
#'
#' A step of the gru4r_native implementation. Called by \code{morie_gru4r_bpr},
#' \code{morie_gru4r_gru}, \code{morie_gru4r_top1}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @return The value of \code{ifelse}.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .gru4r_sigmoid(x = x)
#' res
.gru4r_sigmoid <- function(x) {
  # Numerically stable sigmoid: below -700 it is effectively 0.
  ifelse(x > -700, 1 / (1 + exp(-x)), 0)
}

# -- restored: morie-only definition kept through the rmorie sync --
#' BPR ranking loss
#'
#' \eqn{-1/N_S \\sum_j \\log\\sigma(r_i - r_j)}, with the
#' log floored at 1e-12 for numerical stability.
#'
#' @param r_target Score of the target item.
#' @param r_negatives Numeric vector of negative-item scores.
#' @return Scalar loss.
#' @export
morie_gru4r_bpr <- function(r_target, r_negatives) {
  neg <- as.numeric(r_negatives)
  if (length(neg) == 0L) stop("gru4r: at least one negative is needed")
  rt <- as.numeric(r_target)
  s <- .gru4r_sigmoid(rt - neg)
  -sum(log(pmax(s, 1e-12))) / length(neg)
}

# -- restored: morie-only definition kept through the rmorie sync --
#' One GRU update (single layer)
#'
#' Update gate, reset gate, candidate, and the convex combination
#' \code{(1 - z) h + z hh} that defines a GRU.
#'
#' @param x Input vector (length d).
#' @param h Hidden state (length n).
#' @param Wz,Uz Update-gate linear maps.
#' @param Wr,Ur Reset-gate linear maps.
#' @param Wh,Uh Candidate-h linear maps.
#' @return New hidden state.
#' @export
morie_gru4r_gru <- function(x, h, Wz, Uz, Wr, Ur, Wh, Uh) {
  x <- as.numeric(x)
  h <- as.numeric(h)
  Wz <- as.matrix(Wz)
  Uz <- as.matrix(Uz)
  Wr <- as.matrix(Wr)
  Ur <- as.matrix(Ur)
  Wh <- as.matrix(Wh)
  Uh <- as.matrix(Uh)
  n <- length(h)
  lin <- function(W, U, xv, hv) as.numeric(W %*% xv + U %*% hv)
  z <- .gru4r_sigmoid(lin(Wz, Uz, x, h))
  r <- .gru4r_sigmoid(lin(Wr, Ur, x, h))
  hh <- tanh(lin(Wh, Uh, x, r * h))
  (1 - z) * h + z * hh
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Mean reciprocal rank at k
#'
#' 1 / position of the target within the top k, or 0 if absent.
#'
#' @param ranked Ordered integer vector of recommended item ids.
#' @param target Target item id.
#' @param kk Cutoff.
#' @return Scalar.
#' @export
morie_gru4r_mrr <- function(ranked, target, kk = 20) {
  top <- as.integer(ranked)[seq_len(min(as.integer(kk),
                                        length(ranked)))]
  t <- as.integer(target)
  if (t %in% top) 1.0 / (which(top == t)[1L]) else 0.0
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Recall at k
#'
#' 1 if the target is in the top \code{k} ranked items, else 0.
#'
#' @param ranked Ordered integer vector of recommended item ids.
#' @param target Target item id.
#' @param kk Cutoff.
#' @return 0 or 1.
#' @export
morie_gru4r_recall <- function(ranked, target, kk = 20) {
  top <- as.integer(ranked)[seq_len(min(as.integer(kk),
                                        length(ranked)))]
  if (as.integer(target) %in% top) 1.0 else 0.0
}

# -- restored: morie-only definition kept through the rmorie sync --
#' TOP1 ranking loss
#'
#' The smoothed relative rank
#' \eqn{\\sigma(r_j - r_i)} plus, when \code{regularize=TRUE}, the
#' load-bearing \eqn{\\sigma(r_j^2)} term that stops scores from
#' running away.
#'
#' @param r_target Score of the target item.
#' @param r_negatives Numeric vector of negative-item scores.
#' @param regularize Include the sigma(r_j^2) regulariser.
#' @return Scalar loss.
#' @export
morie_gru4r_top1 <- function(r_target, r_negatives, regularize = TRUE) {
  neg <- as.numeric(r_negatives)
  if (length(neg) == 0L) stop("gru4r: at least one negative is needed")
  rt <- as.numeric(r_target)
  rank <- sum(.gru4r_sigmoid(neg - rt)) / length(neg)
  if (!regularize) return(rank)
  rank + sum(.gru4r_sigmoid(neg * neg)) / length(neg)
}
