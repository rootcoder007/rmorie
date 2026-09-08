# SAX: symbolic aggregate approximation of a time series.
# Source: Lin, J., Keogh, E., Lonardi, S. and Chiu, B. (2003), A
# symbolic representation of time series, with implications for
# streaming algorithms, DMKD 2003 workshop: z-normalisation
# (their Sec. 3), the piecewise aggregate approximation of their eq.
# (1), the equiprobable Gaussian breakpoints of their Table 3, and
# the MINDIST lower bound of their eq. (4)/Table 4.
#
# Native implementation mirroring Python morie.fn.saxR exactly,
# including the population standard deviation used for the z-score,
# the near-constant guard, and the >= convention when a PAA value
# lands exactly on a breakpoint.

.MOR_SAX_LETTERS <- strsplit("abcdefghijklmnopqrstuvwxyz", "")[[1]]

#' Equiprobable Gaussian breakpoints for SAX
#'
#' The \code{alphabet - 1} standard normal quantiles that cut the
#' standard normal into \code{alphabet} equiprobable pieces (Lin et
#' al. 2003, Table 3).
#'
#' @param alphabet Alphabet size, at least 2.
#' @return Numeric vector of breakpoints.
#' @references Lin, J., Keogh, E., Lonardi, S. and Chiu, B. (2003). A
#'   symbolic representation of time series. DMKD 2003.
#' @export
#' @examples
#' morie_sax_breakpoints(alphabet = 5L)
morie_sax_breakpoints <- function(alphabet) {
  a <- as.integer(alphabet)
  if (a < 2L) stop("alphabet size must be >= 2")
  vapply(seq_len(a - 1L), function(i) .morie_normal_quantile(i / a),
         numeric(1))
}

#' .mor_sax_paa
#'
#' A step of the saxR_native implementation. Called by \code{morie_saxR}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param z A vector; its length is taken and its elements indexed.
#' @param w A count; the body uses it as \code{seq_len(...)}.
#' @return A vector, from \code{vapply}.
#' @export
.mor_sax_paa <- function(z, w) {
  n <- length(z)
  if (n %% w != 0L)
    stop(paste("word length must divide the series length (eq 1 of",
               "Lin et al. 2003 uses equal frames)"))
  f <- n %/% w
  vapply(seq_len(w), function(i) sum(z[((i - 1L) * f + 1L):(i * f)]) / f,
         numeric(1))
}

#' SAX word for a series
#'
#' Z-normalises the series, reduces it to \code{window} piecewise
#' aggregate values (Lin et al. 2003, eq. 1), and maps each to a
#' letter using equiprobable Gaussian breakpoints.
#'
#' @param x Numeric series.
#' @param window Word length \code{w}; must divide \code{length(x)}.
#' @param alphabet Alphabet size.
#' @param eps Threshold below which the series counts as constant, in
#'   which case the middle symbol is used throughout.
#' @return A list with \code{word}, \code{symbols} (0-based),
#'   \code{paa}, \code{breakpoints}, \code{mean}, \code{sd},
#'   \code{estimate}, \code{n}, \code{method}.
#' @references Lin, J., Keogh, E., Lonardi, S. and Chiu, B. (2003). A
#'   symbolic representation of time series. DMKD 2003.
#' @export
#' @examples
#' morie_saxR(x = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), window = 3L, alphabet = 5L)
morie_saxR <- function(x, window, alphabet, eps = 1e-8) {
  xv <- as.numeric(x)
  n <- length(xv)
  w <- as.integer(window)
  a <- as.integer(alphabet)
  if (w < 1L || w > n) stop("need 1 <= window <= n")
  bps <- morie_sax_breakpoints(a)
  mu <- mean(xv)
  sdv <- sqrt(mean((xv - mu)^2))
  if (sdv < eps) {
    mid <- (a - 1L) %/% 2L
    syms <- rep(mid, w)
    paa <- rep(0, w)
  } else {
    z <- (xv - mu) / sdv
    paa <- .mor_sax_paa(z, w)
    syms <- vapply(paa, function(cv) sum(cv >= bps), numeric(1))
  }
  word <- paste(.MOR_SAX_LETTERS[syms + 1L], collapse = "")
  list(word = word, symbols = syms, paa = paa, breakpoints = bps,
       mean = mu, sd = sdv, estimate = word, n = n,
       method = "SAX (Lin-Keogh-Lonardi-Chiu 2003)")
}

#' MINDIST between two SAX words
#'
#' The lower bound of the true Euclidean distance given by Lin et al.
#' (2003), eq. (4): adjacent symbols contribute zero, otherwise the
#' gap between the enclosing breakpoints.
#'
#' @param word1,word2 SAX words of equal length.
#' @param n Length of the original series.
#' @param alphabet Alphabet size used to produce the words.
#' @return The MINDIST value.
#' @references Lin, J., Keogh, E., Lonardi, S. and Chiu, B. (2003). A
#'   symbolic representation of time series. DMKD 2003.
#' @export
morie_sax_mindist <- function(word1, word2, n, alphabet) {
  s1 <- strsplit(as.character(word1), "")[[1]]
  s2 <- strsplit(as.character(word2), "")[[1]]
  if (length(s1) != length(s2)) stop("words must have equal length")
  w <- length(s1)
  a <- as.integer(alphabet)
  bps <- morie_sax_breakpoints(a)
  tot <- 0
  for (k in seq_len(w)) {
    r <- match(s1[k], .MOR_SAX_LETTERS)
    cc <- match(s2[k], .MOR_SAX_LETTERS)
    if (max(r, cc) > a) stop("symbol outside alphabet")
    d <- if (abs(r - cc) <= 1L) 0 else bps[max(r, cc) - 1L] - bps[min(r, cc)]
    tot <- tot + d * d
  }
  sqrt(n / w) * sqrt(tot)
}
