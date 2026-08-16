# ELMo: deep contextualized word representations.
# Sources: Peters, M. E., Neumann, M., Iyyer, M., Gardner, M., Clark,
# C., Lee, K. & Zettlemoyer, L. (2018) "Deep contextualized word
# representations", *Proceedings of NAACL-HLT 2018*, 2227-2237,
# doi:10.18653/v1/N18-1202, arXiv:1802.05365. Sec. 3, eq. (1), and the
# layer-weighting scheme. Hochreiter, S. & Schmidhuber, J. (1997)
# "Long Short-Term Memory", *Neural Computation* 9(8), 1735-1780,
# doi:10.1162/neco.1997.9.8.1735, for the recurrent cell the biLM is
# built from. Ba, J. L., Kiros, J. R. & Hinton, G. E. (2016) "Layer
# Normalization", arXiv:1607.06450, for the normalisation the paper
# applies per layer before weighting.
#
# Native implementation mirroring Python morie.fn.elmo exactly: the
# same softmax-normalised s^{task}, the same one-LSTM-cell step with
# gates in the order i, f, g, o, the same biLM forward with layer 0
# being the token vector duplicated as [x; x] (so its width equals the
# 2*hidden of a biLSTM concatenation) and the backward pass re-reversed
# so position k aligns with token k, the same eq. (1) mix, and the
# same per-layer width check that fails loudly if the mixture is over
# differently shaped vectors.

#' Softmax-normalised s^{task}. A simplex, not free weights: these
#'
#' choose WHICH layers to read and cannot alter the magnitude, which is
#' gamma\'s job alone.
#'
#' @param raw See Usage.
#' @return A numeric value.
#' @export
layer_weights <- function(raw) {
  # Softmax-normalised s^{task}. A simplex, not free weights: these
  # choose WHICH layers to read and cannot alter the magnitude, which
  # is gamma's job alone.
  if (length(raw) == 0L)
    stop("elmo: no layer weights given")
  raw <- as.numeric(raw)
  mx <- max(raw)
  e <- exp(raw - mx)
  e / sum(e)
}

#' One LSTM cell step, gates in the order i, f, g, o
#'
#' Part of the elmo_native implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param h See Usage.
#' @param c See Usage.
#' @param Wx See Usage.
#' @param Wh See Usage.
#' @param b See Usage.
#' @return A list with \code{h}, \code{c}.
#' @export
lstm_step <- function(x, h, c, Wx, Wh, b) {
  # One LSTM cell step, gates in the order i, f, g, o.
  d <- length(h)
  if (length(c) != d)
    stop("elmo: hidden and cell sizes differ")
  x <- as.numeric(x); h <- as.numeric(h); c <- as.numeric(c)
  Wx <- matrix(as.numeric(Wx), nrow = length(x), ncol = 4 * d)
  Wh <- matrix(as.numeric(Wh), nrow = d, ncol = 4 * d)
  b <- as.numeric(b)
  z <- as.numeric(as.vector(x) %*% Wx) +
       as.numeric(as.vector(h) %*% Wh) + b
  i_g <- 1 / (1 + exp(-z[seq_len(d)]))
  f_g <- 1 / (1 + exp(-z[d + seq_len(d)]))
  g_g <- tanh(z[2 * d + seq_len(d)])
  o_g <- 1 / (1 + exp(-z[3 * d + seq_len(d)]))
  cn <- f_g * c + i_g * g_g
  hn <- o_g * tanh(cn)
  list(h = hn, c = cn)
}

#' Run the biLM and return every layer\'s representation
#'
#' layers is a list of (Wxf, Whf, bf, Wxb, Whb, bb). The token dimension
#' must equal the hidden dimension, because layer 0 is the token vector
#' duplicated and every layer has to be the same width for eq. (1) to
#' add them. The backward pass reads the sequence in reverse and its
#' output is re-reversed before concatenation, so position k always
#' aligns with token k.
#'
#' @param X See Usage.
#' @param layers See Usage.
#' @return The value of \code{reps}, as built in the body.
#' @export
bilm_forward <- function(X, layers) {
  # Run the biLM and return every layer's representation.
  # layers is a list of (Wxf, Whf, bf, Wxb, Whb, bb). The token
  # dimension must equal the hidden dimension, because layer 0 is the
  # token vector duplicated and every layer has to be the same width
  # for eq. (1) to add them. The backward pass reads the sequence in
  # reverse and its output is re-reversed before concatenation, so
  # position k always aligns with token k.
  Xm <- as.matrix(X)
  L <- nrow(Xm)
  if (L == 0L)
    stop("elmo: empty sequence")
  # Layer 0 is the token representation DUPLICATED, h_{k,0} = [x_k;
  # x_k], so it is the same width as a biLSTM layer's forward-
  # backward concatenation and the mixture is well defined. Leaving
  # it at token width makes eq. (1) a sum over differently shaped
  # vectors, which fails loudly here rather than silently
  # broadcasting.
  reps <- list(lapply(seq_len(L), function(t)
    c(as.numeric(Xm[t, ]), as.numeric(Xm[t, ]))))
  cur <- lapply(seq_len(L), function(t) as.numeric(Xm[t, ]))
  for (layer in layers) {
    Wxf <- layer$Wxf; Whf <- layer$Whf; bf <- layer$bf
    Wxb <- layer$Wxb; Whb <- layer$Whb; bb <- layer$bb
    d <- length(Whf)
    if (length(reps[[1]][[1]]) != 2L * d)
      stop("elmo: token dimension ", ncol(Xm),
           " but hidden dimension ", d,
           "; layer 0 is [x; x] so they must match")
    h <- rep(0, d); c <- rep(0, d)
    fwd <- vector("list", L)
    for (t in seq_len(L)) {
      r <- lstm_step(cur[[t]], h, c, Wxf, Whf, bf)
      h <- r$h; c <- r$c
      fwd[[t]] <- h
    }
    h <- rep(0, d); c <- rep(0, d)
    bwd <- vector("list", L)
    for (t in seq_len(L):1) {
      r <- lstm_step(cur[[t]], h, c, Wxb, Whb, bb)
      h <- r$h; c <- r$c
      bwd[[t]] <- h
    }
    # re-align: position k is token k
    bwd <- bwd[seq_len(L)]
    cur <- lapply(seq_len(L), function(t) c(fwd[[t]], bwd[[t]]))
    reps <- c(reps, list(lapply(cur, function(r) as.numeric(r))))
  }
  reps
}

#' Eq. (1): gamma * sum_j s_j h_{k,j}
#'
#' Part of the elmo_native implementation; see the file header for the
#' source it follows.
#'
#' @param reps See Usage.
#' @param raw_weights See Usage.
#' @param gamma Defaults to \code{1}.
#' @param position Defaults to \code{NULL}.
#' @return One of two values, depending on the branch taken.
#' @export
elmo_mix <- function(reps, raw_weights, gamma = 1, position = NULL) {
  # Eq. (1): gamma * sum_j s_j h_{k,j}.
  n_layers <- length(reps)
  if (length(raw_weights) != n_layers)
    stop("elmo: ", length(raw_weights), " weights for ", n_layers,
         " layers")
  s <- layer_weights(raw_weights)
  L <- length(reps[[1]])
  dims <- unique(sapply(reps, function(r) length(r[[1]])))
  if (length(dims) != 1L)
    stop("elmo: layers have differing widths ",
         paste(dims, collapse = ", "))
  d <- dims
  idx <- if (is.null(position)) seq_len(L) else as.integer(position)
  out <- lapply(idx, function(t)
    as.numeric(gamma) *
      rowSums(sapply(seq_len(n_layers), function(j)
        s[j] * reps[[j]][[t]])))
  if (is.null(position)) out else out[[1]]
}

#' elmo_representation
#'
#' Part of the elmo_native implementation; see the file header for the
#' source it follows.
#'
#' @param X See Usage.
#' @param layers See Usage.
#' @param raw_weights Defaults to \code{NULL}.
#' @param gamma Defaults to \code{1}.
#' @return A list with \code{estimate}, \code{elmo}, \code{layers}, \code{weights}, \code{gamma}, \code{n_layers}, \code{L}, \code{d}, \code{top_layer}, \code{method}.
#' @export
elmo_representation <- function(X, layers, raw_weights = NULL,
                                gamma = 1) {
  # The biLM plus the task-specific mix, end to end.
  reps <- bilm_forward(X, layers)
  n <- length(reps)
  raw <- if (is.null(raw_weights)) rep(0, n) else as.numeric(raw_weights)
  mixed <- elmo_mix(reps, raw, gamma = gamma)
  s <- layer_weights(raw)
  list(estimate = mixed, elmo = mixed, layers = reps, weights = s,
       gamma = as.numeric(gamma), n_layers = n, L = length(reps[[1]]),
       d = if (length(mixed) > 0L) length(mixed[[1]]) else 0L,
       top_layer = reps[[n]],
       method = "ELMo layer mixture, Peters et al. (2018) eq. (1)")
}

.elmo_cheatsheet <- function() {
  paste0("elmo: ELMo_k = gamma * sum_j s_j h_{k,j}, s SOFTMAX-",
         "normalised (eq. 1). The simplex constraint means s chooses ",
         "WHICH layers to read and cannot scale the output -- all ",
         "magnitude is in gamma. Free s makes gamma unidentifiable; ",
         "no gamma leaves the scale wherever the biLM left it. The ",
         "backward pass must be re-reversed or position k stops ",
         "meaning token k, and the shapes will not tell you.")
}

# compact alias per ledger/NAMING.md
#' Compact alias per ledger/NAMING.md
#'
#' Part of the elmo_native implementation; see the file header for the
#' source it follows.
#'
#' @param X See Usage.
#' @param layers See Usage.
#' @param raw_weights Defaults to \code{NULL}.
#' @param gamma Defaults to \code{1}.
#' @return The value of \code{elmo_representation}.
#' @export
elmorepresentation <- function(X, layers, raw_weights = NULL,
                               gamma = 1) {
  elmo_representation(X, layers, raw_weights, gamma)
}

# public name resolved by fn/_lazy_map.json
#' Public name resolved by fn/_lazy_map.json
#'
#' Part of the elmo_native implementation; see the file header for the
#' source it follows.
#'
#' @param X See Usage.
#' @param layers See Usage.
#' @param raw_weights Defaults to \code{NULL}.
#' @param gamma Defaults to \code{1}.
#' @return The value of \code{elmo_representation}.
#' @export
elmo <- function(X, layers, raw_weights = NULL, gamma = 1) {
  elmo_representation(X, layers, raw_weights, gamma)
}

# morie entry point: matches the Python payload keys
#' Morie entry point: matches the Python payload keys
#'
#' Part of the elmo_native implementation; see the file header for the
#' source it follows.
#'
#' @param X See Usage.
#' @param layers See Usage.
#' @param raw_weights Defaults to \code{NULL}.
#' @param gamma Defaults to \code{1}.
#' @return The value of \code{elmo_representation}.
#' @export
morie_elmo <- function(X, layers, raw_weights = NULL, gamma = 1) {
  elmo_representation(X, layers, raw_weights, gamma)
}
