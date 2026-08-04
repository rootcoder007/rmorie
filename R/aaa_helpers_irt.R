# Shared primitives for the item response theory modules.
#
# Kept in one place so icrf, irt2pl, irt3pl, iinfo, thetml, irtras, rsmand,
# irtnrm and nrm cannot drift apart. The Python arm of this file is
# src/morie/fn/_irtcore.py. Base R only.

#' @noRd
.irt_broadcast <- function(v, n, name) {
  v <- as.numeric(v)
  if (length(v) == 1L) {
    return(rep(v, n))
  }
  if (length(v) != n) {
    stop(sprintf("%s has length %d; expected 1 or %d", name, length(v), n),
         call. = FALSE)
  }
  v
}

#' @noRd
.irt_expit <- function(z) {
  # branch so neither tail overflows; identical to the Python arm
  z <- as.numeric(z)
  out <- numeric(length(z))
  pos <- z >= 0
  if (any(pos)) out[pos] <- 1 / (1 + exp(-z[pos]))
  if (any(!pos)) {
    e <- exp(z[!pos])
    out[!pos] <- e / (1 + e)
  }
  out
}

#' @noRd
.irt_softmax <- function(eta) {
  eta <- as.numeric(eta)
  ex <- exp(eta - max(eta))
  ex / sum(ex)
}

#' @noRd
.irt_as_matrix <- function(x, name) {
  if (is.list(x) && !is.data.frame(x)) {
    w <- unique(vapply(x, length, integer(1)))
    if (length(w) != 1L) stop(sprintf("%s has ragged rows.", name), call. = FALSE)
    x <- do.call(rbind, lapply(x, as.numeric))
  }
  x <- as.matrix(x)
  storage.mode(x) <- "double"
  if (nrow(x) == 0L || ncol(x) == 0L) {
    stop(sprintf("%s is empty.", name), call. = FALSE)
  }
  x
}
