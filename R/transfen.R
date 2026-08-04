# SPDX-License-Identifier: AGPL-3.0-or-later
#' Transfer entropy
#'
#' Schreiber (2000), Measuring information transfer, Physical Review
#' Letters 85(2), 461-464, eq. (4): T_(Y->X) = sum p(x_(n+1), x_n, y_n)
#' log[p(x_(n+1) | x_n, y_n) / p(x_(n+1) | x_n)], equivalently
#' H(X_(n+1)|X_n) - H(X_(n+1)|X_n, Y_n).  The PRL is paywalled; the
#' definition is quoted in its standard published form.  Transfer entropy
#' is DIRECTED, so both directions are computed -- reporting only one
#' invites the reader to treat it as symmetric, which it is not.
#'
#' @param x,y the two series, of equal length; entries are symbols.
#' @param lag the history length.
#' @return list: estimate, te_xy, te_yx, net, bits, lag, n, method.
#' @keywords internal
#' @examples
#' Transent(c(0, 1, 0, 1, 0, 1), c(0, 0, 1, 0, 1, 0))$te_xy
#' @export
Transent <- function(x, y, lag = 1) {
  a <- as.character(x); b <- as.character(y)
  te1 <- function(src, dst, L) {
    n <- length(dst)
    keys <- character(0); cnt <- numeric(0)
    if (n - 2L >= L) for (t in seq(L, n - 2L)) {
      i <- t + 1L
      k1 <- dst[i + 1L]
      k2 <- if (L > 1L) dst[i - L + 1L] else dst[i]
      k3 <- if (L > 1L) src[i - L + 1L] else src[i]
      kk <- paste(k1, k2, k3, sep = "\037")
      j <- match(kk, keys)
      if (is.na(j)) { keys <- c(keys, kk); cnt <- c(cnt, 1) } else cnt[j] <- cnt[j] + 1
    }
    tot <- 0
    for (v in cnt) tot <- tot + v
    if (tot <= 0) return(NaN)
    p3 <- cnt / tot
    parts <- strsplit(keys, "\037", fixed = TRUE)
    ka <- vapply(parts, function(z) z[1], "")
    kb <- vapply(parts, function(z) z[2], "")
    kc <- vapply(parts, function(z) z[3], "")
    kxy <- paste(kb, kc, sep = "\037")
    kx <- paste(ka, kb, sep = "\037")
    ux <- unique(kxy); p2xy <- numeric(length(ux))
    for (i in seq_along(keys)) {
      j <- match(kxy[i], ux); p2xy[j] <- p2xy[j] + p3[i]
    }
    uz <- unique(kx); p2x <- numeric(length(uz))
    for (i in seq_along(keys)) {
      j <- match(kx[i], uz); p2x[j] <- p2x[j] + p3[i]
    }
    ub <- unique(kb); p1 <- numeric(length(ub))
    for (i in seq_along(keys)) {
      j <- match(kb[i], ub); p1[j] <- p1[j] + p3[i]
    }
    ordk <- order(keys, method = "radix")
    te <- 0
    for (i in ordk) {
      num <- p3[i] / p2xy[match(kxy[i], ux)]
      den <- p2x[match(kx[i], uz)] / p1[match(kb[i], ub)]
      if (num > 0 && den > 0) te <- te + p3[i] * log(num / den)
    }
    te
  }
  txy <- te1(a, b, as.integer(lag))
  tyx <- te1(b, a, as.integer(lag))
  list(estimate = txy, te_xy = txy, te_yx = tyx, net = txy - tyx,
       bits = txy / log(2), lag = as.integer(lag), n = length(a),
       method = "Transfer entropy (Schreiber 2000, eq. 4); directed, so both directions are reported")
}
