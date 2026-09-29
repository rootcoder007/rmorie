#' SwiGLU feed-forward block
#'
#' `(x W1 * SiLU(x W3)) W2` (Shazeer 2020): a front-end to
#' `swiglu_activation` with `W = W3`, `V = W1`, followed by the output
#' projection `W2` when given.
#'
#' @param x Input matrix (rows are tokens).
#' @param W1 Up-projection weights (NULL with `W3` for elementwise gating).
#' @param W2 Output projection (NULL returns the hidden layer).
#' @param W3 Gate-projection weights.
#' @return List with `value` (output), `output` and `hidden`.
#' @references Shazeer, N. (2020). GLU variants improve Transformer.
#'   arXiv:2002.05202.
#' @examples
#' swiglu(rbind(c(1, -2)))$value
#' @export
swiglu <- function(x, W1 = NULL, W2 = NULL, W3 = NULL) {
  if (is.null(W1) != is.null(W3)) stop("provide both W1 and W3 or neither")
  h <- swiglu_activation(x, W3, W1)$tensor
  out <- if (is.null(W2)) h else h %*% W2
  list(name = "swiglu", value = out, output = out, hidden = h)
}
