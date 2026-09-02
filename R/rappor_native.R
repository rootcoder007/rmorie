# RAPPOR.  Source: Erlingsson, U., Pihur, V. & Korolova, A. (2014),
# RAPPOR: Randomized Aggregatable Privacy-Preserving Ordinal Response,
# Proc. 21st ACM CCS, 1054-1067.
#   Sec. 2 step 2 (PRR):  B'_i = 1 w.p. f/2, 0 w.p. f/2, B_i w.p. 1-f
#   Sec. 2 step 3 (IRR):  P(S_i = 1) = q if B'_i = 1 else p
#   Lemma 1:  q* = f(p+q)/2 + (1-f)q,  p* = f(p+q)/2 + (1-f)p
#   Theorem 1:  eps_inf = 2h ln((1 - f/2)/(f/2))
#   Theorem 2:  eps_1   = h log(q*(1-p*)/(p*(1-q*)))
#   Sec. 4 decode:  t = (c - (p + fq/2 - fp/2) N) / ((1-f)(q-p))
# Native implementation mirroring Python morie.fn.rappor, loop for loop,
# including the RNG draw order so the two arms agree bit for bit.

#' RAPPOR effective report probabilities (Lemma 1)
#'
#' @param f,p,q RAPPOR parameters.
#' @return List with \code{q_star} and \code{p_star}.
#' @export
morie_rappor_star <- function(f, p, q) {
  if (f < 0 || f > 1) stop("morie_rappor: f must lie in [0, 1]")
  if (p < 0 || p > 1) stop("morie_rappor: p must lie in [0, 1]")
  if (q < 0 || q > 1) stop("morie_rappor: q must lie in [0, 1]")
  half <- 0.5 * f * (p + q)
  list(q_star = half + (1 - f) * q, p_star = half + (1 - f) * p)
}

#' RAPPOR privacy parameters (Theorems 1 and 2)
#'
#' @param h Number of Bloom hash functions.
#' @param f Permanent randomized response parameter.
#' @param p,q Instantaneous randomized response parameters; when both
#'   are supplied, \code{eps_1} is returned as well.
#' @return List with \code{eps_infinity} and optionally \code{eps_1},
#'   \code{q_star}, \code{p_star}.
#' @references Erlingsson, Pihur & Korolova (2014), ACM CCS.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_rappor_epsilon(V, V)
morie_rappor_epsilon <- function(h, f, p = NULL, q = NULL) {
  h <- as.integer(h)[1]
  if (h < 1) stop("morie_rappor: h must be at least 1")
  f <- as.numeric(f)[1]
  if (!(f > 0 && f < 2))
    stop("morie_rappor: f must lie in (0, 2) for eps_inf to be finite")
  half_f <- 0.5 * f
  out <- list(eps_infinity = 2 * h * log((1 - half_f) / half_f))
  if (!is.null(p) && !is.null(q)) {
    st <- morie_rappor_star(f, p, q)
    qs <- st$q_star
    ps <- st$p_star
    if (!(ps > 0 && ps < 1) || !(qs > 0 && qs < 1))
      stop("morie_rappor: q* and p* must lie strictly in (0, 1)")
    out$eps_1 <- h * log((qs * (1 - ps)) / (ps * (1 - qs)))
    out$q_star <- qs
    out$p_star <- ps
  }
  out
}

#' .morie_rappor_bloom
#'
#' A step of the rappor_native implementation. Called by \code{morie_rappor}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param value Coerced to character by the body, with \code{as.character}.
#' @param k Numeric; combined arithmetically in the body.
#' @param h A count; the body uses it as \code{seq_len(...)}.
#' @param cohort Numeric; combined arithmetically in the body. Defaults to \code{0}.
#' @return A vector, from \code{sort}.
#' @export
.morie_rappor_bloom <- function(value, k, h, cohort = 0) {
  s <- as.character(value)
  chars <- utf8ToInt(s)
  bits <- integer(0)
  for (j in seq_len(h) - 1L) {
    # 31-bit polynomial rolling hash; every intermediate stays under
    # 2^38 so this is exact in doubles and matches the Python arm.
    acc <- (cohort * 7919 + j * 104729 + 1) %% 2147483647
    for (cc in chars) acc <- (acc * 131 + cc) %% 2147483647
    bits <- c(bits, acc %% k)
  }
  sort(unique(bits))
}

#' RAPPOR encode (Sec. 2, steps 1-4)
#'
#' Mirrors Python \code{morie.fn.rappor}.
#'
#' @param values Client values.
#' @param k Bloom filter width.
#' @param h Number of hash functions.
#' @param f,p,q RAPPOR parameters.
#' @param cohorts Number of cohorts.
#' @param variant One of \code{"full"}, \code{"one-time"} (skips the
#'   instantaneous step) or \code{"basic"} (one-hot, no Bloom filter).
#' @param seed RNG seed.
#' @return A list with the reports, per-cohort bit \code{counts} and
#'   \code{cohort_sizes}.
#' @references Erlingsson, Pihur & Korolova (2014), ACM CCS, 1054-1067.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_rappor(V)
morie_rappor <- function(values, k = 16, h = 2, f = 0.5, p = 0.5, q = 0.75,
                         cohorts = 1, variant = "full", seed = 0) {
  var <- tolower(as.character(variant)[1])
  if (!var %in% c("full", "one-time", "basic"))
    stop("morie_rappor: variant must be full, one-time or basic")
  vals <- values
  n <- length(vals)
  if (n < 1) stop("morie_rappor: need at least one value")
  k <- as.integer(k)[1]
  h <- as.integer(h)[1]
  m <- as.integer(cohorts)[1]
  if (k < 1) stop("morie_rappor: k must be at least 1")
  if (m < 1) stop("morie_rappor: cohorts must be at least 1")
  f <- as.numeric(f)[1]
  p <- as.numeric(p)[1]
  q <- as.numeric(q)[1]
  if (f < 0 || f > 1) stop("morie_rappor: f must lie in [0, 1]")
  if (p < 0 || p > 1) stop("morie_rappor: p must lie in [0, 1]")
  if (q < 0 || q > 1) stop("morie_rappor: q must lie in [0, 1]")

  alphabet <- NULL
  if (var == "basic") {
    alphabet <- sort(unique(as.character(vals)))
    k <- length(alphabet)
    h <- 1L
    m <- 1L
  }

  g <- .ghc_rng(seed)
  reports <- vector("list", n)
  cohort_of <- integer(n)
  counts <- matrix(0, nrow = m, ncol = k)
  sizes <- integer(m)

  for (idx in seq_len(n)) {
    v <- vals[[idx]]
    j <- if (m == 1L) 0L else as.integer(.ghc_unif(g) * m)
    if (j >= m) j <- m - 1L
    cohort_of[idx] <- j
    sizes[j + 1L] <- sizes[j + 1L] + 1L

    B <- integer(k)
    if (var == "basic") {
      B[match(as.character(v), alphabet)] <- 1L
    } else {
      for (b in .morie_rappor_bloom(v, k, h, cohort = j)) B[b + 1L] <- 1L
    }

    Bp <- integer(k)
    for (i in seq_len(k)) {
      u <- .ghc_unif(g)
      if (u < 0.5 * f) Bp[i] <- 1L
      else if (u < f) Bp[i] <- 0L
      else Bp[i] <- B[i]
    }

    if (var == "one-time") {
      S <- Bp
    } else {
      S <- integer(k)
      for (i in seq_len(k)) {
        thr <- if (Bp[i] == 1L) q else p
        S[i] <- if (.ghc_unif(g) < thr) 1L else 0L
      }
    }

    reports[[idx]] <- S
    counts[j + 1L, ] <- counts[j + 1L, ] + S
  }

  list(estimate = counts,
       reports = reports,
       counts = counts,
       cohort_sizes = sizes,
       cohort_of = cohort_of,
       k = as.integer(k), h = as.integer(h), cohorts = as.integer(m),
       alphabet = alphabet, variant = var,
       f = f, p = p, q = q, n = as.integer(n),
       method = "RAPPOR encode (Erlingsson, Pihur & Korolova 2014, Sec. 2)")
}

#' RAPPOR decode (Sec. 4)
#'
#' @param counts Per-cohort bit counts, a matrix or list of rows.
#' @param sizes Reports per cohort.
#' @param f,p,q RAPPOR parameters.
#' @return A list whose \code{t} holds the estimated true set-bit counts.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_rappor_decode(V, V)
morie_rappor_decode <- function(counts, sizes, f = 0.5, p = 0.5, q = 0.75) {
  f <- as.numeric(f)[1]
  p <- as.numeric(p)[1]
  q <- as.numeric(q)[1]
  denom <- (1 - f) * (q - p)
  if (denom == 0)
    stop("morie_rappor_decode: (1 - f)(q - p) is zero, so the reports ",
         "carry no signal and no unbiased estimate exists")
  shift <- p + 0.5 * f * q - 0.5 * f * p
  cm <- if (is.matrix(counts)) counts else
    do.call(rbind, lapply(counts, as.numeric))
  N <- as.numeric(sizes)
  if (nrow(cm) != length(N))
    stop("morie_rappor_decode: ", nrow(cm), " count rows but ",
         length(N), " cohort sizes")
  est <- (cm - shift * N) / denom
  list(estimate = est, t = est, shift = shift, denominator = denom,
       cohort_sizes = as.integer(N), f = f, p = p, q = q,
       method = "RAPPOR decode (Erlingsson, Pihur & Korolova 2014, Sec. 4)")
}
