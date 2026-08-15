.sctraj_matrix <- function(X) {
  if (is.list(X)) {
    rows <- lapply(X, function(r) as.numeric(r))
    n <- length(rows)
    if (n == 0) stop("sctraj: X is empty")
    p <- length(rows[[1]])
    if (p == 0) stop("sctraj: X has no columns")
    for (i in seq_len(n)) {
      r <- rows[[i]]
      if (length(r) != p) stop("sctraj: X is ragged")
      if (any(is.na(r)) || any(is.infinite(r))) stop("sctraj: X contains a non-finite value")
    }
    return(list(rows = rows, n = n, p = p))
  }
  # matrix
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  n <- nrow(X)
  if (n == 0) stop("sctraj: X is empty")
  p <- ncol(X)
  if (p == 0) stop("sctraj: X has no columns")
  if (any(is.na(X)) || any(is.infinite(X))) stop("sctraj: X contains a non-finite value")
  rows <- lapply(seq_len(n), function(i) X[i, ])
  return(list(rows = rows, n = n, p = p))
}
