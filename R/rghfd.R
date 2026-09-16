#' Higuchi fractal dimension -- Rangayyan Sec. 5.13.2, eqs (5.39)-(5.41), p.304
#'
#' Higuchi (1988) fractal dimension via curve-length scaling.
#'
#' @param x Numeric vector.
#' @param kmax Maximum lag (default 10).
#' @return Named list `HFD`, `log_L`, `log_inv_k`, `kmax`.
#' @references Higuchi (1988), Physica D 31:277. Rangayyan Sec. 5.13.2, eqs (5.39)-(5.41), p.304.
#' @export
#' @examples
#' set.seed(0)
#' rghfd(rnorm(500), kmax = 8)$HFD
rghfd <- function(x, kmax = 10L) {
  N <- length(x)
  if (N < 4 || kmax < 2) stop("Need length(x) >= 4 and kmax >= 2.")
  kmax <- as.integer(min(kmax, N %/% 2L))
  L <- numeric(kmax)
  for (k in seq_len(kmax)) {
    lk <- numeric(0)
    for (m in seq_len(k)) {
      ## Eq (5.39): m is 1-based in the book, so index directly.
      idx <- seq(m, N, by = k)
      if (length(idx) < 2) next
      diffs <- sum(abs(diff(x[idx])))
      ## Eq (5.40): the normaliser is floor((N - m)/k) with the book's 1-based
      ## m, and it must equal the number of difference terms actually summed.
      ## The previous code looped m over 0:(k-1) and passed that 0-based index
      ## here, so the denominator was floor((N - m + 1)/k) while the numerator
      ## still had floor((N - m)/k) terms. Deriving it from length(idx) keeps
      ## the two identical by construction.
      n_terms <- length(idx) - 1L
      norm <- (N - 1) / (k * n_terms)
      lk <- c(lk, (diffs / k) * norm)
    }
    L[k] <- if (length(lk)) mean(lk) else NA_real_
  }
  ks <- seq_len(kmax)
  log_L <- log(L)
  log_inv_k <- log(1 / ks)
  fit <- stats::lm(log_L ~ log_inv_k)
  list(
    HFD = unname(stats::coef(fit)[2]),
    intercept = unname(stats::coef(fit)[1]),
    log_L = log_L, log_inv_k = log_inv_k, kmax = kmax
  )
}

#' @rdname rghfd
#' @keywords internal
#' @export
morie_rangayyan_higuchi_fd <- rghfd
