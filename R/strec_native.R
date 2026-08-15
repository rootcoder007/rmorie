# morie.fn -- function file (rootcoder007/morie)
# STAMP: give the last click priority over the session average.
#
# A session-based recommender has no user profile, only the clicks so
# far. Earlier neural models summarised that prefix and predicted from
# the summary, without allowing for the fact that **interests drift** --
# often because of unintended clicks. The paper's example is concrete: a
# user who has just clicked a digital camera is likely to act on *that*,
# not on whatever dominated the session ten clicks ago.
#
# **Two memories, and the short one gets priority.** The session prefix
# is an external memory; the average of its embeddings,
#
# .. math:: m_s = \frac{1}{t}\sum_{i=1}^{t} x_i,
#
# is the user's **general** interest, and the **last click**
# :math:`m_t = x_t` is the current one. Both pass through their own
# single-layer MLP (identical structure, independent parameters) to give
# :math:`h_s` and :math:`h_t`.
#
# **Scoring is trilinear, not a dot product.** With
# :math:`\langle a,b,c\rangle = \sum_i a_ib_ic_i = a^\top(b\odot c)`,
#
# .. math:: \hat z_i = \sigma(\langle h_s, h_t, x_i\rangle),
#
# so a candidate must agree with the general interest **and** the
# current one at once. A model that added the two representations would
# let a strong long-term signal carry a candidate the last click
# contradicts; the Hadamard product cannot.
#
# **The attention exists because the average is wrong.** :math:`m_s`
# weights every click in the prefix equally, which is exactly what fails
# when an unintended click sits in a long session. STAMP replaces it
# with an attention-weighted sum,
#
# .. math:: \alpha_i = W_0\,\sigma(W_1 x_i + W_2 x_t + W_3 m_s + b_a),
#          \qquad m_a = \sum_{i=1}^{t}\alpha_i x_i,
#
# where each weight sees the item, **the last click**, and the session
# average. Note what is absent: no softmax is applied to
# :math:`\alpha`, and the weights are not constrained to sum to one.
#
# References
# ----------
# Liu, Q., Zeng, Y., Mokhosi, R. & Zhang, H. (2018) "STAMP: Short-Term
# Attention/Memory Priority Model for Session-based Recommendation",
# *Proceedings of the 24th ACM SIGKDD International Conference on
# Knowledge Discovery & Data Mining (KDD '18)*, 1831-1839,
# doi:10.1145/3219819.3219950. [PDF supplied by Vee.] The abstract and
# Sec. 1 (that predicting from a session prefix without allowing for
# users' interests drifting with time is problematic, the digital-camera
# example, and the proposal of a short-term attention/memory priority
# model capturing general interests from the long-term memory of the
# session context while taking the current interests from the short-term
# memory of the last click); Sec. 3.1 (the trilinear product
# <a,b,c> = sum a_i b_i c_i = a^T (b (*) c)); Sec. 3.2 (the STMP model
# with m_s the average of the external memory, m_t = x_t the last click,
# two identically structured MLP cells with independent parameters, the
# score z_i = sigma(<h_s, h_t, x_i>), the softmax over candidates and
# the cross-entropy loss); and Sec. 3.3 (that treating each item in the
# prefix as equally important is problematic for interest drift in long
# sessions, and the attention net alpha_i = W_0 sigma(W_1 x_i + W_2 x_t
# + W_3 m_s + b_a) with m_a the attention-weighted sum replacing m_s).
#
# Li, J., Ren, P., Chen, Z., Ren, Z., Lian, T. & Ma, J. (2017) "Neural
# Attentive Session-based Recommendation", *CIKM 2017*, 1419-1428,
# arXiv:1711.04725. NARM, which the paper distinguishes itself from --
# it combines main purpose and sequential behaviour as equally
# important, where STAMP explicitly privileges the last click;
# implemented in :mod:`narm`.

.strec_EPS <- 1e-12

.strec_sigmoid <- function(x) {
  x <- max(-60.0, min(60.0, x))
  1.0 / (1.0 + exp(-x))
}

.strec_as_rows <- function(x) {
  if (is.list(x) && !is.data.frame(x)) {
    return(lapply(x, as.numeric))
  }
  if (is.matrix(x) || is.data.frame(x)) {
    return(lapply(seq_len(nrow(x)), function(i) as.numeric(x[i, ])))
  }
  list(as.numeric(x))
}

strec_trilinear <- function(a, b, c) {
  A <- as.numeric(a)
  B <- as.numeric(b)
  C <- as.numeric(c)
  if (!(length(A) == length(B) && length(B) == length(C))) {
    stop(sprintf("strec: the three vectors differ in length (%d, %d, %d)",
                 length(A), length(B), length(C)))
  }
  sum(A * B * C)
}

strec_session_average <- function(embeddings) {
  X <- .strec_as_rows(embeddings)
  t <- length(X)
  if (t < 1) {
    stop("strec: the session prefix is empty")
  }
  d <- length(X[[1]])
  m_s <- numeric(d)
  for (a in seq_len(d)) {
    s <- 0.0
    for (i in seq_len(t)) {
      s <- s + X[[i]][a]
    }
    m_s[a] <- s / t
  }
  list(m_s = m_s, m_t = X[[t]], length = t,
       note = "m_t is the LAST CLICK, and it is also part of the external memory")
}

strec_mlp_cell <- function(m, W, b = NULL, activation = "tanh") {
  v <- as.numeric(m)
  W <- as.matrix(W)
  if (ncol(W) != length(v)) {
    stop(sprintf("strec: the cell expects %d inputs but got %d",
                 ncol(W), length(v)))
  }
  if (is.null(b)) {
    bb <- rep(0.0, nrow(W))
  } else {
    bb <- as.numeric(b)
  }
  z <- as.numeric(W %*% v + bb)
  if (activation == "tanh") {
    return(tanh(z))
  } else if (activation == "identity") {
    return(z)
  } else {
    stop(sprintf("strec: activation must be tanh or identity, got %s", activation))
  }
}

strec_attention_weights <- function(embeddings, W1, W2, W3, W0, b_a = NULL) {
  X <- .strec_as_rows(embeddings)
  t <- length(X)
  if (t < 1) {
    stop("strec: the session prefix is empty")
  }
  d <- length(X[[1]])
  xt <- X[[t]]
  ms <- strec_session_average(X)$m_s
  W1 <- as.matrix(W1)
  W2 <- as.matrix(W2)
  W3 <- as.matrix(W3)
  W0 <- as.numeric(W0)
  h <- nrow(W1)
  if (is.null(b_a)) {
    bb <- rep(0.0, h)
  } else {
    bb <- as.numeric(b_a)
  }

  alphas <- numeric(t)
  for (i in seq_len(t)) {
    inner <- numeric(h)
    for (o in seq_len(h)) {
      s <- bb[o]
      for (j in seq_len(d)) {
        s <- s + W1[o, j] * X[[i]][j]
      }
      for (j in seq_len(d)) {
        s <- s + W2[o, j] * xt[j]
      }
      for (j in seq_len(d)) {
        s <- s + W3[o, j] * ms[j]
      }
      inner[o] <- .strec_sigmoid(s)
    }
    alphas[i] <- sum(W0 * inner)
  }

  m_a <- numeric(d)
  for (a in seq_len(d)) {
    s <- 0.0
    for (i in seq_len(t)) {
      s <- s + alphas[i] * X[[i]][a]
    }
    m_a[a] <- s
  }

  list(alpha = alphas, m_a = m_a, sum_alpha = sum(alphas),
       m_s = ms,
       note = "no softmax: the composition is a weighted sum, so the weights need not sum to 1")
}

strec_stamp_scores <- function(embeddings, item_table, Ws, Wt, bs = NULL, bt = NULL,
                                attention = NULL) {
  X <- .strec_as_rows(embeddings)
  V <- .strec_as_rows(item_table)

  base <- strec_session_average(X)
  if (is.null(attention)) {
    m_s <- base$m_s
  } else {
    m_s <- attention$m_a
  }
  m_t <- base$m_t
  h_s <- strec_mlp_cell(m_s, Ws, bs)
  h_t <- strec_mlp_cell(m_t, Wt, bt)

  z <- numeric(length(V))
  for (k in seq_along(V)) {
    z[k] <- .strec_sigmoid(strec_trilinear(h_s, h_t, V[[k]]))
  }

  mx <- max(z)
  e <- exp(z - mx)
  tot <- sum(e)
  y <- e / tot
  order <- order(-y)

  list(
    estimate = order[1],
    ranking = order,
    probability = y,
    score = z,
    h_s = h_s,
    h_t = h_t,
    attention_used = !is.null(attention),
    model = if (!is.null(attention)) "STAMP" else "STMP",
    method = "short-term attention/memory priority; Liu, Zeng, Mokhosi & Zhang (2018)",
    note = "trilinear, so a candidate must match the general AND the current interest -- a sum would let one carry it"
  )
}

strec_cross_entropy <- function(probability, target_index) {
  p <- as.numeric(probability)
  j <- as.integer(target_index)
  if (j < 1 || j > length(p)) {
    stop("strec: the target is outside the item dictionary")
  }
  tot <- 0.0
  for (i in seq_along(p)) {
    yi <- if (i == j) 1.0 else 0.0
    tot <- tot + (yi * log(max(p[i], .strec_EPS)) +
                  (1.0 - yi) * log(max(1.0 - p[i], .strec_EPS)))
  }
  -tot
}

strec_cheatsheet <- function() {
  "strec: a session recommender has no profile, only the clicks -- and interests DRIFT, often from unintended clicks. Keep TWO memories: m_s, the average of the session prefix (general interest), and m_t = x_t, the LAST CLICK (current interest), each through its own MLP cell. Score TRILINEARLY, sigma(<h_s, h_t, x_i>), so a candidate must match both at once -- a sum would let a stale long-term signal override the last click. The average weights every click equally, which is what breaks in a long session, so STAMP replaces it with attention alpha_i = W0 sigma(W1 x_i + W2 x_t + W3 m_s + b_a). No softmax on alpha."
}

# compact alias per ledger/NAMING.md
strec_stamp <- strec_stamp_scores

# Entry point
morie_strec <- strec_stamp_scores

#' @rdname strec_trilinear
#' @export
morie_strec <- strec_trilinear

#' @rdname strec_trilinear
#' @export
morie_strec <- strec_trilinear

#' @rdname strec_trilinear
#' @export
morie_strec <- strec_trilinear

#' @rdname strec_trilinear
#' @export
morie_strec <- strec_trilinear

#' @rdname strec_trilinear
#' @export
morie_strec <- strec_trilinear

#' @rdname strec_trilinear
#' @export
morie_strec <- strec_trilinear

#' @rdname strec_trilinear
#' @export
morie_strec <- strec_trilinear
