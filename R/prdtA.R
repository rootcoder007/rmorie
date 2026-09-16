# SPDX-License-Identifier: AGPL-3.0-or-later
#' Evaluate a Polish-notation expression, right to left
#'
#' Scanning from the RIGHT, an operand is pushed and an operator pops
#' exactly its arity; no parentheses and no precedence table are needed. A
#' malformed expression is an error, not a plausible wrong number.
#'
#' Formula: scan right to left; operand -> push;
#'   binary operator -> pop a, pop b, push (a op b)
#'
#' @param tokens Character vector of tokens in prefix order. Operators are
#'   "+", "-", "*", "/", "^"; everything else must parse as a number.
#' @return List with \code{value}, \code{n_tokens}, \code{n_operators},
#'   \code{max_stack}.
#' @references Lukasiewicz, J. (1929/1963), Elements of Mathematical
#'   Logic, in which the parenthesis-free notation is introduced; usually
#'   dated to his 1924 lectures. The evaluation procedure is the standard
#'   stack algorithm and is not attributed to that source.
#' @export
#' @examples
#' Prefixev(tokens = 5L)
Prefixev <- function(tokens) {
  toks <- as.character(tokens)
  if (!length(toks)) stop("the expression is empty")
  opset <- c("+", "-", "*", "/", "^")
  st <- numeric(0)
  nop <- 0L
  mx <- 0L
  for (t in rev(toks)) {
    if (t %in% opset) {
      if (length(st) < 2L)
        stop(sprintf("operator '%s' has fewer than two operands", t))
      a <- st[length(st)]
      b <- st[length(st) - 1L]
      st <- st[seq_len(length(st) - 2L)]
      if (t == "/" && b == 0) stop("division by zero in the expression")
      v <- switch(t, "+" = a + b, "-" = a - b, "*" = a * b,
                  "/" = a / b, "^" = a^b)
      st <- c(st, v)
      nop <- nop + 1L
    } else {
      st <- c(st, as.numeric(t))
    }
    if (length(st) > mx) mx <- length(st)
  }
  if (length(st) != 1L)
    stop(sprintf("malformed prefix expression: %d values left on the stack",
                 length(st)))
  .t1_result(value = st[1], n_tokens = as.numeric(length(toks)),
             n_operators = as.numeric(nop), max_stack = as.numeric(mx),
             method = "Prefix (Polish) notation evaluation")
}
