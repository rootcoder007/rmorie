# Sources: Gu, A. & Dao, T. (2023) "Mamba: Linear-Time Sequence Modeling
# with Selective State Spaces", arXiv:2312.00752.
# Gu, A., Goel, K. & Re, C. (2022) "Efficiently Modeling Long Sequences
# with Structured State Spaces", ICLR 2022, arXiv:2111.00396.
# Gu, A., Gupta, A., Goel, K. & Re, C. (2022) "On the Parameterization
# and Initialization of Diagonal State Space Models", NeurIPS 2022,
# arXiv:2206.11893.

.mambss_EPS <- 1e-12

#' softplus
#'
#' A step of the mambss_native implementation. Called by \code{morie_geron_blip_itm_itc}, \code{selective_scan}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param z Coerced to numeric by the body, with \code{as.numeric}.
#' @return The value of \code{log1p}.
#' @export
softplus <- function(z) {
  x <- as.numeric(z)
  if (x > 30.0) return(x)
  if (x < -30.0) return(exp(x))
  log1p(exp(x))
}

#' discretize_zoh
#'
#' A step of the mambss_native implementation. Called by \code{selective_ssm_step}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param delta Coerced to numeric by the body, with \code{as.numeric}.
#' @param A Coerced to numeric by the body, with \code{as.numeric}.
#' @param B Coerced to numeric by the body, with \code{as.numeric}.
#' @param rule One of \code{"euler"}, \code{"zoh"}. Defaults to \code{"zoh"}.
#' @return A list with \code{Abar}, \code{Bbar}.
#' @export
discretize_zoh <- function(delta, A, B, rule = "zoh") {
  if (!(rule %in% c("zoh", "euler")))
    stop(sprintf("mambss: rule must be zoh or euler, got %r", rule))
  d <- as.numeric(delta)
  if (d < 0.0) stop(sprintf("mambss: delta must be non-negative, got %r", delta))
  Av <- as.numeric(A)
  Bv <- as.numeric(B)
  if (length(Av) != length(Bv))
    stop(sprintf("mambss: A has %d entries but B has %d", length(Av), length(Bv)))
  Abar <- numeric(length(Av))
  Bbar <- numeric(length(Av))
  for (n in seq_along(Av)) {
    da <- d * Av[n]
    ea <- exp(da)
    Abar[n] <- ea
    if (rule == "euler") {
      Bbar[n] <- d * Bv[n]
    } else if (abs(da) < 1e-8) {
      Bbar[n] <- d * Bv[n] * (1.0 + 0.5 * da)
    } else {
      Bbar[n] <- (ea - 1.0) / Av[n] * Bv[n]
    }
  }
  list(Abar = Abar, Bbar = Bbar)
}

#' selective_ssm_step
#'
#' A step of the mambss_native implementation. Called by \code{selective_scan}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param h A vector; its length is taken and its elements indexed.
#' @param A A vector; its length is taken.
#' @param B Passed to \code{discretize_zoh}.
#' @param C A vector; its length is taken.
#' @param delta Passed to \code{discretize_zoh}.
#' @param rule Passed to \code{discretize_zoh}. Defaults to \code{"zoh"}.
#' @return A list with \code{h}, \code{y}.
#' @export
selective_ssm_step <- function(x, h, A, B, C, delta, rule = "zoh") {
  N <- length(A)
  if (length(h) != N)
    stop(sprintf("mambss: state has %d entries but A has %d", length(h), N))
  if (length(C) != N)
    stop(sprintf("mambss: C has %d entries but A has %d", length(C), N))
  Abar_Bbar <- discretize_zoh(delta, A, B, rule = rule)
  Abar <- Abar_Bbar$Abar
  Bbar <- Abar_Bbar$Bbar
  hn <- numeric(N)
  for (n in seq_len(N)) hn[n] <- Abar[n] * h[n] + Bbar[n] * as.numeric(x)
  y <- sum(C * hn)
  list(h = hn, y = y)
}

#' .linear
#'
#' A step of the mambss_native implementation. Called by \code{selective_scan}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x A vector; its length is taken and its elements indexed.
#' @param Wm A matrix; indexed by row and column.
#' @param b A vector; indexed elementwise.
#' @return The value of \code{out}, as built in the body.
#' @export
.linear <- function(x, Wm, b) {
  out <- numeric(nrow(Wm))
  for (r in seq_len(nrow(Wm))) {
    s <- b[r]
    for (c in seq_along(x)) s <- s + Wm[r, c] * x[c]
    out[r] <- s
  }
  out
}

#' .sigmoid
#'
#' A step of the mambss_native implementation. Called by \code{gated_rnn_equivalent}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param z Coerced to numeric by the body, with \code{as.numeric}.
#' @return A numeric value.
#' @export
.sigmoid <- function(z) 1.0 / (1.0 + exp(-as.numeric(z)))

#' selective_scan
#'
#' A step of the mambss_native implementation. Called by \code{morie_mambss}, \code{s6_layer}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; passed to \code{as.matrix}.
#' @param A A matrix; passed to \code{as.matrix}.
#' @param W_B A matrix; passed to \code{as.matrix}.
#' @param W_C A matrix; passed to \code{as.matrix}.
#' @param W_delta A matrix; passed to \code{as.matrix}.
#' @param delta_bias Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param b_B Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param b_C Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param b_delta Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.
#' @param rule Carried through into a list the body builds. Defaults to \code{"zoh"}.
#' @param D_skip Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{y}, \code{estimate}, \code{state}, \code{delta}, \code{L}, \code{D}, \code{N}, \code{rule}, \code{time_invariant}, \code{method}.
#' @export
selective_scan <- function(X, A, W_B, W_C, W_delta, delta_bias = NULL,
                           b_B = NULL, b_C = NULL, b_delta = 0.0,
                           rule = "zoh", D_skip = NULL) {
  Xm <- as.matrix(X)
  L <- nrow(Xm)
  if (L == 0L) stop("mambss: the input sequence is empty")
  D <- ncol(Xm)
  Am <- as.matrix(A)
  if (nrow(Am) != D)
    stop(sprintf("mambss: A has %d rows for %d channels", nrow(Am), D))
  N <- ncol(Am)
  WB <- as.matrix(W_B)
  WC <- as.matrix(W_C)
  if (nrow(WB) != N || nrow(WC) != N)
    stop(sprintf("mambss: W_B and W_C must have N=%d rows, got %d and %d",
                 N, nrow(WB), nrow(WC)))
  Wd <- as.matrix(W_delta)
  if (nrow(Wd) != 1L)
    stop(sprintf("mambss: W_delta must have exactly 1 row -- s_Delta projects to one dimension and is broadcast over channels; got %d", nrow(Wd)))
  bB <- if (is.null(b_B)) rep(0.0, N) else as.numeric(b_B)
  bC <- if (is.null(b_C)) rep(0.0, N) else as.numeric(b_C)
  dbias <- if (is.null(delta_bias)) rep(0.0, D) else as.numeric(delta_bias)
  if (length(dbias) != D)
    stop(sprintf("mambss: delta_bias has %d entries for %d channels",
                 length(dbias), D))
  skip <- if (is.null(D_skip)) rep(0.0, D) else as.numeric(D_skip)

  h <- matrix(0.0, D, N)
  Y <- matrix(0.0, L, D)
  deltas <- matrix(0.0, L, D)
  for (t in seq_len(L)) {
    xt <- as.numeric(Xm[t, ])
    Bt <- .linear(xt, WB, bB)
    Ct <- .linear(xt, WC, bC)
    raw <- .linear(xt, Wd, c(as.numeric(b_delta)))[1L]
    dt <- numeric(D)
    for (c in seq_len(D)) dt[c] <- softplus(raw + dbias[c])
    deltas[t, ] <- dt
    for (c in seq_len(D)) {
      ssm <- selective_ssm_step(xt[c], h[c, ], Am[c, ], Bt, Ct, dt[c], rule = rule)
      h[c, ] <- ssm$h
      Y[t, c] <- ssm$y + skip[c] * xt[c]
    }
  }
  list(y = Y, estimate = Y, state = h, delta = deltas, L = L, D = D, N = N,
       rule = rule, time_invariant = FALSE,
       method = "selective state space scan (S6), Gu & Dao (2023) Algorithm 2")
}

#' gated_rnn_equivalent
#'
#' A step of the mambss_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x A vector; its length is taken and its elements indexed.
#' @param w Coerced to numeric by the body, with \code{as.numeric}.
#' @param b Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.
#' @return A list with \code{h}, \code{g}.
#' @export
gated_rnn_equivalent <- function(x, w, b = 0.0) {
  h <- 0.0
  hs <- numeric(length(x))
  gs <- numeric(length(x))
  for (i in seq_along(x)) {
    g <- .sigmoid(as.numeric(w) * as.numeric(x[i]) + as.numeric(b))
    h <- (1.0 - g) * h + g * as.numeric(x[i])
    hs[i] <- h
    gs[i] <- g
  }
  list(h = hs, g = gs)
}

#' s6_layer
#'
#' A step of the mambss_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X Passed to \code{selective_scan}.
#' @param A Passed to \code{selective_scan}.
#' @param W_B Passed to \code{selective_scan}.
#' @param W_C Passed to \code{selective_scan}.
#' @param W_delta Passed to \code{selective_scan}.
#' @param ... Passed through.
#' @return The value of \code{$}.
#' @export
s6_layer <- function(X, A, W_B, W_C, W_delta, ...) {
  selective_scan(X, A, W_B, W_C, W_delta, ...)$y
}

#' .mambss_cheatsheet
#'
#' A step of the mambss_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.mambss_cheatsheet <- function() {
  "mambss: S6. B, C, Delta are FUNCTIONS of x (Alg. 2), so the model is time-varying and only the scan works -- no convolution. ZOH: Abar = exp(Delta A), Bbar = (exp(Delta A) - 1) B / A. s_Delta projects to ONE dim then broadcasts over D. Theorem 1: N=1, A=-1, B=1, softplus gives exactly g = sigmoid(Linear(x)), h = (1-g)h + g x."
}

selectivessmstep <- selective_ssm_step
mamba_ssm_step <- selective_ssm_step
mambassmstep <- selective_ssm_step

#' morie_mambss
#'
#' A step of the mambss_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X Passed to \code{selective_scan}.
#' @param A Passed to \code{selective_scan}.
#' @param W_B Passed to \code{selective_scan}.
#' @param W_C Passed to \code{selective_scan}.
#' @param W_delta Passed to \code{selective_scan}.
#' @param delta_bias Passed to \code{selective_scan}.
#' @param b_B Passed to \code{selective_scan}.
#' @param b_C Passed to \code{selective_scan}.
#' @param b_delta Passed to \code{selective_scan}. Defaults to \code{0}.
#' @param rule Passed to \code{selective_scan}. Defaults to \code{"zoh"}.
#' @param D_skip Passed to \code{selective_scan}.
#' @return The value of \code{selective_scan}.
#' @export
morie_mambss <- function(X, A, W_B, W_C, W_delta, delta_bias = NULL,
                        b_B = NULL, b_C = NULL, b_delta = 0.0,
                        rule = "zoh", D_skip = NULL) {
  selective_scan(X, A, W_B, W_C, W_delta, delta_bias = delta_bias,
                 b_B = b_B, b_C = b_C, b_delta = b_delta, rule = rule,
                 D_skip = D_skip)
}
