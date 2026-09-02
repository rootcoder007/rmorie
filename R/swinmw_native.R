# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Swin window MSA with relative position bias (Swinmw). Bit-identical
# mirror of src/morie/fn/swinmw.py. grswin/hmswin lack the bias term
# (Eq 4's addition), so this is not aliased to them; zero table
# reduces exactly to their computation.

#' .morie_swin_bias
#'
#' A step of the swinmw_native implementation. Called by \code{Swinmw}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param M Numeric; combined arithmetically in the body.
#' @param tab A matrix; indexed by row and column.
#' @return The value of \code{B}, as built in the body.
#' @export
.morie_swin_bias <- function(M, tab) {
  n <- M * M
  B <- matrix(0, n, n)
  for (p in seq_len(n)) {
    i1 <- (p - 1L) %/% M
    j1 <- (p - 1L) %% M
    for (q in seq_len(n)) {
      i2 <- (q - 1L) %/% M
      j2 <- (q - 1L) %% M
      B[p, q] <- tab[i1 - i2 + M, j1 - j2 + M]
    }
  }
  B
}

#' Swin window multi-head self-attention with relative position bias
#'
#' Liu et al. (2021), "Swin Transformer", ICCV 2021, arXiv:2103.14030,
#' Eq 4: Attention(Q,K,V) = SoftMax(Q K' / sqrt(d) + B) V inside each
#' non-overlapping M x M window, with B (M^2 x M^2) looked up from the
#' parameterised (2M-1) x (2M-1) table by the tokens' relative
#' position along each axis.
#'
#' @param x Feature map, array with dim c(H, W, d); M must divide H, W.
#' @param window_size Window side M.
#' @param relative_bias Optional (2M-1) x (2M-1) table; zeros if
#'   omitted.
#' @param WQ,WK,WV Optional projections (d x d_k / d x d_k / d x d_v);
#'   identity if omitted.
#' @return List with \code{output} (H x W x d_v array), \code{bias},
#'   \code{n_windows}, \code{tokens_per_window}, \code{estimate},
#'   \code{n}, \code{method}.
#' @references Liu, Z. et al. (2021), ICCV 2021, arXiv:2103.14030,
#'   Section 3.2, Eq 4. Local source:
#'   fetched-wave3/liu-etal-2021-swin-transformer-arxiv2103.14030.pdf.
#' @export
Swinmw <- function(x, window_size, relative_bias = NULL,
                   WQ = NULL, WK = NULL, WV = NULL) {
  A <- x
  if (!is.array(A) || length(dim(A)) != 3L) {
    stop("Swinmw: x must be an array with dim c(H, W, d)", call. = FALSE)
  }
  storage.mode(A) <- "double"
  dm <- dim(A)
  H <- dm[1L]
  W <- dm[2L]
  d <- dm[3L]
  if (H == 0L || W == 0L || d == 0L) stop("Swinmw: x must be non-empty", call. = FALSE)
  if (!all(is.finite(A))) stop("Swinmw: x contains non-finite values", call. = FALSE)
  M <- as.integer(window_size)
  if (M < 1L || H %% M != 0L || W %% M != 0L) {
    stop(sprintf("Swinmw: window_size must divide H and W, got %d for (%d, %d)", M, H, W), call. = FALSE)
  }
  Wq <- if (is.null(WQ)) diag(d) else { m <- as.matrix(WQ)
  storage.mode(m) <- "double"
  m }
  Wk <- if (is.null(WK)) diag(d) else { m <- as.matrix(WK)
  storage.mode(m) <- "double"
  m }
  Wv <- if (is.null(WV)) diag(d) else { m <- as.matrix(WV)
  storage.mode(m) <- "double"
  m }
  if (nrow(Wq) != d || nrow(Wk) != d || nrow(Wv) != d) {
    stop("Swinmw: projection rows must equal d", call. = FALSE)
  }
  if (ncol(Wq) != ncol(Wk)) stop("Swinmw: WQ and WK widths must match", call. = FALSE)
  dk <- ncol(Wq)
  dv <- ncol(Wv)
  if (is.null(relative_bias)) {
    tab <- matrix(0, 2L * M - 1L, 2L * M - 1L)
  } else {
    tab <- as.matrix(relative_bias)
    storage.mode(tab) <- "double"
    if (nrow(tab) != 2L * M - 1L || ncol(tab) != 2L * M - 1L) {
      stop(sprintf("Swinmw: relative_bias must be (%d, %d)", 2L * M - 1L, 2L * M - 1L), call. = FALSE)
    }
  }
  B <- .morie_swin_bias(M, tab)
  out <- array(0, dim = c(H, W, dv))
  scale <- 1 / sqrt(dk)
  n_windows <- 0L
  for (h0 in seq(0L, H - M, by = M)) {
    for (w0 in seq(0L, W - M, by = M)) {
      n_windows <- n_windows + 1L
      X <- matrix(0, M * M, d)
      for (i in seq_len(M)) for (j in seq_len(M)) {
        # token order matches Python: p = (i-1)*M + (j-1), row-major
        X[(i - 1L) * M + j, ] <- A[h0 + i, w0 + j, ]
      }
      Q <- X %*% Wq
      K <- X %*% Wk
      V <- X %*% Wv
      S <- (Q %*% t(K)) * scale + B
      Wt <- t(apply(S, 1L, function(row) {
        mx <- max(row)
        e <- exp(row - mx)
        e / sum(e)
      }))
      if (M * M == 1L) Wt <- matrix(Wt, nrow = 1L)
      O <- Wt %*% V
      for (p in seq_len(M * M)) {
        i <- (p - 1L) %/% M
        j <- (p - 1L) %% M
        out[h0 + i + 1L, w0 + j + 1L, ] <- O[p, ]
      }
    }
  }
  list(output = out, bias = B, n_windows = n_windows,
       tokens_per_window = M * M, estimate = out[1, 1, 1],
       n = H * W,
       method = "Swin window MSA softmax(QK^T/sqrt(d) + B)V (Liu et al. 2021, Eq 4)")
}
