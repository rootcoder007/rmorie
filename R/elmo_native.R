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

#' Softmax-normalised s^\{task\}. A simplex, not free weights: these
#'
#' choose WHICH layers to read and cannot alter the magnitude, which is
#' gamma's job alone.
#'
#' @param raw A vector; its length is taken.
#' @return A numeric value.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' layer_weights(V)
#' @keywords internal
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
#' A step of the elmo_native implementation. Called by \code{bilm_forward}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x A vector; its length is taken.
#' @param h A vector; its length is taken.
#' @param c A vector; its length is taken.
#' @param Wx A matrix; passed to \code{\%*\%}.
#' @param Wh A matrix; passed to \code{\%*\%}.
#' @param b Numeric; combined arithmetically in the body.
#' @return A list with \code{h}, \code{c}.
#' @export
#' @examples
#' lstm_step(x = c(1, 2, 3, 4, 5, 6, 7, 8), h = c(1, 2, 3, 4, 5, 6, 7, 8),
#'   c = c(1, 2, 3, 4, 5, 6, 7, 8), Wx = c(1, 2, 3, 4, 5, 6, 7, 8),
#'   Wh = c(1, 2, 3, 4, 5, 6, 7, 8), b = 5L)
#' @keywords internal
lstm_step <- function(x, h, c, Wx, Wh, b) {
  # One LSTM cell step, gates in the order i, f, g, o.
  d <- length(h)
  if (length(c) != d)
    stop("elmo: hidden and cell sizes differ")
  x <- as.numeric(x)
  h <- as.numeric(h)
  c <- as.numeric(c)
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

#' Run the biLM and return every layer's representation
#'
#' layers is a list of (Wxf, Whf, bf, Wxb, Whb, bb). The token dimension
#' must equal the hidden dimension, because layer 0 is the token vector
#' duplicated and every layer has to be the same width for eq. (1) to
#' add them. The backward pass reads the sequence in reverse and its
#' output is re-reversed before concatenation, so position k always
#' aligns with token k.
#'
#' @param X A matrix; passed to \code{as.matrix}.
#' @param layers See Usage.
#' @return The value of \code{reps}, as built in the body.
#' @export
#' @examples
#' set.seed(1)
#' d <- 2
#' X <- matrix(rnorm(6, 0, 0.5), 3, d)
#' mkw <- function() matrix(rnorm(d * 4 * d, 0, 0.3), d, 4 * d)
#' layer <- list(Wxf = mkw(), Whf = mkw(), bf = rep(0, 4 * d),
#'               Wxb = mkw(), Whb = mkw(), bb = rep(0, 4 * d))
#' r <- bilm_forward(X, list(layer))
#' str(r, max.level = 1)
#' @keywords internal
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
    Wxf <- layer$Wxf
    Whf <- layer$Whf
    bf <- layer$bf
    Wxb <- layer$Wxb
    Whb <- layer$Whb
    bb <- layer$bb
    # hidden size = ROWS of Whf; length() of an R matrix counts every
    # element, where the reference len() counts rows
    d <- if (is.matrix(Whf)) nrow(Whf) else length(Whf)
    if (length(reps[[1]][[1]]) != 2L * d)
      stop("elmo: token dimension ", ncol(Xm),
           " but hidden dimension ", d,
           "; layer 0 is [x; x] so they must match")
    h <- rep(0, d)
    c <- rep(0, d)
    fwd <- vector("list", L)
    for (t in seq_len(L)) {
      r <- lstm_step(cur[[t]], h, c, Wxf, Whf, bf)
      h <- r$h
      c <- r$c
      fwd[[t]] <- h
    }
    h <- rep(0, d)
    c <- rep(0, d)
    bwd <- vector("list", L)
    # rev(seq_len(L)): the colon after seq_len(L) coerced the whole
    # vector to its first element and the backward pass only ever
    # visited t = 1
    for (t in rev(seq_len(L))) {
      r <- lstm_step(cur[[t]], h, c, Wxb, Whb, bb)
      h <- r$h
      c <- r$c
      bwd[[t]] <- h
    }
    # re-align: position k is token k
    bwd <- bwd[seq_len(L)]
    cur <- lapply(seq_len(L), function(t) c(fwd[[t]], bwd[[t]]))
    reps <- c(reps, list(lapply(cur, function(r) as.numeric(r))))
  }
  reps
}

#' Eq. (1): gamma * sum_j s_j h_\{k,j\}
#'
#' A step of the elmo_native implementation. Called by \code{elmo_representation}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param reps A vector; its length is taken and its elements indexed.
#' @param raw_weights A vector; its length is taken.
#' @param gamma Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @param position Optional; may be \code{NULL}. Coerced to integer by the body, with
#' \code{as.integer}.
#' @return One of two values, depending on the branch taken.
#' @export
#' @examples
#' set.seed(1)
#' d <- 2
#' X <- matrix(rnorm(6, 0, 0.5), 3, d)
#' mkw <- function() matrix(rnorm(d * 4 * d, 0, 0.3), d, 4 * d)
#' layer <- list(Wxf = mkw(), Whf = mkw(), bf = rep(0, 4 * d),
#'               Wxb = mkw(), Whb = mkw(), bb = rep(0, 4 * d))
#' reps <- bilm_forward(X, list(layer))
#' r <- elmo_mix(reps, raw_weights = c(0.5, -0.5))
#' str(r, max.level = 1)
#' @keywords internal
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
#' A step of the elmo_native implementation. Called by \code{elmo},
#' \code{elmorepresentation}, \code{morie_elmo}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X Passed to \code{bilm_forward}.
#' @param layers Passed to \code{bilm_forward}.
#' @param raw_weights Optional; may be \code{NULL}. Coerced to numeric by the body, with
#' \code{as.numeric}.
#' @param gamma Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @return A list with \code{estimate}, \code{elmo}, \code{layers}, \code{weights},
#' \code{gamma}, \code{n_layers}, \code{L}, \code{d}, \code{top_layer}, \code{method}.
#' @export
#' @examples
#' set.seed(2)
#' d <- 2
#' X <- matrix(rnorm(6, 0, 0.5), 3, d)
#' mkw <- function() matrix(rnorm(d * 4 * d, 0, 0.3), d, 4 * d)
#' layer <- list(Wxf = mkw(), Whf = mkw(), bf = rep(0, 4 * d),
#'               Wxb = mkw(), Whb = mkw(), bb = rep(0, 4 * d))
#' r <- elmo_representation(X, list(layer))
#' str(r, max.level = 1)
#' @keywords internal
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

#' .elmo_cheatsheet
#'
#' A step of the elmo_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
#' @examples
#' res <- .elmo_cheatsheet()
#' res
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
#' A step of the elmo_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X Passed to \code{elmo_representation}.
#' @param layers Passed to \code{elmo_representation}.
#' @param raw_weights Passed to \code{elmo_representation}.
#' @param gamma Passed to \code{elmo_representation}. Defaults to \code{1}.
#' @return The value of \code{elmo_representation}.
#' @export
#' @examples
#' set.seed(2)
#' d <- 2
#' X <- matrix(rnorm(6, 0, 0.5), 3, d)
#' mkw <- function() matrix(rnorm(d * 4 * d, 0, 0.3), d, 4 * d)
#' layer <- list(Wxf = mkw(), Whf = mkw(), bf = rep(0, 4 * d),
#'               Wxb = mkw(), Whb = mkw(), bb = rep(0, 4 * d))
#' r <- elmorepresentation(X, list(layer))
#' str(r, max.level = 1)
#' @keywords internal
elmorepresentation <- function(X, layers, raw_weights = NULL,
                               gamma = 1) {
  elmo_representation(X, layers, raw_weights, gamma)
}

# public name resolved by fn/_lazy_map.json
#' Public name resolved by fn/_lazy_map.json
#'
#' A step of the elmo_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X Passed to \code{elmo_representation}.
#' @param layers Passed to \code{elmo_representation}.
#' @param raw_weights Passed to \code{elmo_representation}.
#' @param gamma Passed to \code{elmo_representation}. Defaults to \code{1}.
#' @return The value of \code{elmo_representation}.
#' @export
#' @examples
#' set.seed(2)
#' d <- 2
#' X <- matrix(rnorm(6, 0, 0.5), 3, d)
#' mkw <- function() matrix(rnorm(d * 4 * d, 0, 0.3), d, 4 * d)
#' layer <- list(Wxf = mkw(), Whf = mkw(), bf = rep(0, 4 * d),
#'               Wxb = mkw(), Whb = mkw(), bb = rep(0, 4 * d))
#' r <- elmo(X, list(layer))
#' str(r, max.level = 1)
#' @keywords internal
elmo <- function(X, layers, raw_weights = NULL, gamma = 1) {
  elmo_representation(X, layers, raw_weights, gamma)
}

# morie entry point: matches the Python payload keys
#' Morie entry point: matches the Python payload keys
#'
#' A step of the elmo_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X Passed to \code{elmo_representation}.
#' @param layers Passed to \code{elmo_representation}.
#' @param raw_weights Passed to \code{elmo_representation}.
#' @param gamma Passed to \code{elmo_representation}. Defaults to \code{1}.
#' @return The value of \code{elmo_representation}.
#' @export
#' @examples
#' set.seed(2)
#' d <- 2
#' X <- matrix(rnorm(6, 0, 0.5), 3, d)
#' mkw <- function() matrix(rnorm(d * 4 * d, 0, 0.3), d, 4 * d)
#' layer <- list(Wxf = mkw(), Whf = mkw(), bf = rep(0, 4 * d),
#'               Wxb = mkw(), Whb = mkw(), bb = rep(0, 4 * d))
#' r <- morie_elmo(X, list(layer))
#' str(r, max.level = 1)
#' @keywords internal
morie_elmo <- function(X, layers, raw_weights = NULL, gamma = 1) {
  elmo_representation(X, layers, raw_weights, gamma)
}

# -- restored: morie-only definition kept through the rmorie sync --
#' .bilm_forward
#'
#' A step of the elmo_native implementation. Called by \code{.elmo_representation}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; passed to \code{as.matrix}.
#' @param layers A vector; its length is taken and its elements indexed.
#' @return The value of \code{reps}, as built in the body.
#' @export
.bilm_forward <- function(X, layers) {
  Xm <- as.matrix(X)
  L <- nrow(Xm)
  if (L == 0L) stop("elmo: empty sequence")
  reps <- vector("list", length(layers) + 1L)
  # Layer 0 is the token representation DUPLICATED, h_{k,0} = [x_k; x_k]
  xdup <- cbind(Xm, Xm)
  reps[[1L]] <- xdup
  cur <- Xm
  for (li in seq_along(layers)) {
    lyr <- layers[[li]]
    Wxf <- as.matrix(lyr[[1L]])
    Whf <- as.matrix(lyr[[2L]])
    bf <- as.numeric(lyr[[3L]])
    Wxb <- as.matrix(lyr[[4L]])
    Whb <- as.matrix(lyr[[5L]])
    bb <- as.numeric(lyr[[6L]])
    d <- ncol(Whf)
    if (ncol(reps[[1L]]) != 2L * d) {
      stop(sprintf("elmo: token dimension %d but hidden dimension %d; layer 0 is [x; x] so they must match",
                   ncol(Xm), d))
    }
    h <- rep(0, d)
    c <- rep(0, d)
    fwd <- matrix(0, nrow = L, ncol = d)
    for (t in seq_len(L)) {
      r <- .lstm_step(cur[t, , drop = FALSE], h, c, Wxf, Whf, bf)
      h <- r$h
      c <- r$c
      fwd[t, ] <- h
    }
    h <- rep(0, d)
    c <- rep(0, d)
    bwd <- matrix(0, nrow = L, ncol = d)
    for (t in L:1L) {
      r <- .lstm_step(cur[t, , drop = FALSE], h, c, Wxb, Whb, bb)
      h <- r$h
      c <- r$c
      bwd[t, ] <- h
    }
    cur <- cbind(fwd, bwd)
    reps[[li + 1L]] <- cur
  }
  reps
}

# -- restored: morie-only definition kept through the rmorie sync --
#' .elmo_mix
#'
#' A step of the elmo_native implementation. Called by \code{.elmo_representation}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param reps A vector; its length is taken and its elements indexed.
#' @param raw_weights A vector; its length is taken.
#' @param gamma Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @param position Optional; may be \code{NULL}. Coerced to integer by the body, with
#' \code{as.integer}.
#' @return One of two values, depending on the branch taken.
#' @export
.elmo_mix <- function(reps, raw_weights, gamma = 1.0, position = NULL) {
  n_layers <- length(reps)
  if (length(raw_weights) != n_layers) {
    stop(sprintf("elmo: %d weights for %d layers", length(raw_weights), n_layers))
  }
  s <- .layer_weights(raw_weights)
  L <- nrow(reps[[1L]])
  dims <- unique(vapply(reps, ncol, integer(1)))
  if (length(dims) != 1L) {
    stop(sprintf("elmo: layers have differing widths %s",
                 paste(sort(dims), collapse = ",")))
  }
  d <- dims
  idx <- if (is.null(position)) seq_len(L) else as.integer(position)
  out <- matrix(0, nrow = length(idx), ncol = d)
  for (ti in seq_along(idx)) {
    t <- idx[ti]
    for (j in seq_len(n_layers)) {
      for (cc in seq_len(d)) {
        out[ti, cc] <- out[ti, cc] + gamma * s[j] * reps[[j]][t, cc]
      }
    }
  }
  if (is.null(position)) out else out[1L, , drop = TRUE]
}

# -- restored: morie-only definition kept through the rmorie sync --
#' .elmo_representation
#'
#' A step of the elmo_native implementation. Called by \code{morie_elmo}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X Passed to \code{.bilm_forward}.
#' @param layers Passed to \code{.bilm_forward}.
#' @param raw_weights Optional; may be \code{NULL}. Coerced to numeric by the body, with
#' \code{as.numeric}.
#' @param gamma Passed to \code{.elmo_mix}. Defaults to \code{1}.
#' @return A list with \code{estimate}, \code{elmo}, \code{layers}, \code{weights},
#' \code{gamma}, \code{n_layers}, \code{L}, \code{d}, \code{top_layer}, \code{method}.
#' @export
.elmo_representation <- function(X, layers, raw_weights = NULL, gamma = 1.0) {
  reps <- .bilm_forward(X, layers)
  n <- length(reps)
  raw <- if (is.null(raw_weights)) rep(0, n) else as.numeric(raw_weights)
  mixed <- .elmo_mix(reps, raw, gamma = gamma)
  s <- .layer_weights(raw)
  list(
    estimate = mixed, elmo = mixed, layers = reps, weights = s,
    gamma = as.numeric(gamma), n_layers = n, L = nrow(reps[[1L]]),
    d = if (length(mixed) > 0L) ncol(mixed) else 0L,
    top_layer = reps[[n]],
    method = "ELMo layer mixture, Peters et al. (2018) eq. (1)"
  )
}

# -- restored: morie-only definition kept through the rmorie sync --
#' .elmo_sigmoid
#'
#' A step of the elmo_native implementation. Called by \code{.lstm_step}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .elmo_sigmoid(x = x)
#' res
.elmo_sigmoid <- function(x) 1 / (1 + exp(-x))

#' .layer_weights
#'
#' A step of the elmo_native implementation. Called by \code{.elmo_mix},
#' \code{.elmo_representation}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param raw A vector; its length is taken.
#' @return A vector, from \code{as.numeric}.
#' @export
.layer_weights <- function(raw) {
  if (length(raw) == 0L) stop("elmo: no layer weights given")
  mx <- max(raw)
  e <- exp(as.numeric(raw) - mx)
  tot <- sum(e)
  as.numeric(e / tot)
}

# -- restored: morie-only definition kept through the rmorie sync --

# -- restored: morie-only definition kept through the rmorie sync --
#' .lstm_step
#'
#' A step of the elmo_native implementation. Called by \code{.bilm_forward}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param h A vector; its length is taken.
#' @param c A vector; its length is taken.
#' @param Wx A matrix; passed to \code{as.matrix}.
#' @param Wh A matrix; passed to \code{as.matrix}.
#' @param b Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{h}, \code{c}.
#' @export
.lstm_step <- function(x, h, c, Wx, Wh, b) {
  d <- length(h)
  if (length(c) != d) stop("elmo: hidden and cell sizes differ")
  xa <- as.numeric(x)
  ha <- as.numeric(h)
  ca <- as.numeric(c)
  Wxa <- as.matrix(Wx)
  Wha <- as.matrix(Wh)
  ba <- as.numeric(b)
  z <- as.numeric(Wxa %*% xa + Wha %*% ha + ba)
  ig <- .elmo_sigmoid(z[seq_len(d)])
  fg <- .elmo_sigmoid(z[d + seq_len(d)])
  gg <- tanh(z[2 * d + seq_len(d)])
  og <- .elmo_sigmoid(z[3 * d + seq_len(d)])
  cn <- fg * ca + ig * gg
  hn <- og * tanh(cn)
  list(h = hn, c = cn)
}
