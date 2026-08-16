# MAFFT: multiple sequence alignment through the fast Fourier transform.
# Sources: Katoh, K., Misawa, K., Kuma, K., & Miyata, T. (2002) "MAFFT:
# a novel method for rapid multiple sequence alignment based on fast
# Fourier transform", *Nucleic Acids Research* 30(14), 3059-3066.
# doi:10.1093/nar/gkf436
#
# Katoh, K., Kuma, K., Toh, H., & Miyata, T. (2005) "MAFFT version 5:
# improvement in accuracy of multiple sequence alignment", *NAR* 33(2),
# 511-518. doi:10.1093/nar/gki198
#
# Katoh, K., & Standley, D. M. (2013) "MAFFT Multiple Sequence Alignment
# Software Version 7", *MBE* 30(4), 772-780. doi:10.1093/molbev/mst010
#
# The 2013 paper is the options table; the algorithm below is the 2002
# paper's, which is where the FFT, the scoring system and the three
# named methods are actually defined.

.MAFFT_AA <- strsplit("ARNDCQEGHILKMFPSTWYV", "")[[1L]]
.MAFFT_NT <- strsplit("ACGT", "")[[1L]]

.MAFFT_GRANTHAM_POLARITY <- c(
  A = 8.1, R = 10.5, N = 11.6, D = 13.0, C = 5.5,
  Q = 10.5, E = 12.3, G = 9.0, H = 10.4, I = 5.2,
  L = 4.9, K = 11.3, M = 5.7, F = 5.2, P = 8.0,
  S = 9.2, T = 8.6, W = 5.4, Y = 6.2, V = 5.9)

.MAFFT_GRANTHAM_VOLUME <- c(
  A = 31.0, R = 124.0, N = 56.0, D = 54.0, C = 55.0,
  Q = 85.0, E = 83.0, G = 3.0, H = 96.0, I = 111.0,
  L = 111.0, K = 119.0, M = 105.0, F = 132.0, P = 32.5,
  S = 32.0, T = 61.0, W = 170.0, Y = 136.0, V = 84.0)

.MAFFT_METHODS <- c("FFT-NS-1", "FFT-NS-2", "FFT-NS-i", "NW-NS-1", "NW-NS-2")
.MAFFT_MATRICES <- c("normalized", "all_positive")
.MAFFT_SIX_GROUPS <- c("AGPST", "C", "DENQ", "FWY", "HKR", "ILMV")

#' .mafft_norm
#'
#' Part of the mafft_native implementation; see the file header for the
#' source it follows.
#'
#' @param vals See Usage.
#' @return A vector, from \code{c}.
#' @export
.mafft_norm <- function(vals) {
  n <- length(vals)
  mu <- sum(vals) / n
  sd <- sqrt(sum((vals - mu)^2) / n)
  if (sd <= 0) stop("mafft: a property with no variation cannot be normalised")
  c(mu = mu, sd = sd)
}

.MAFFT_PMU <- .mafft_norm(.MAFFT_GRANTHAM_POLARITY[.MAFFT_AA])["mu"]
.MAFFT_PSD <- .mafft_norm(.MAFFT_GRANTHAM_POLARITY[.MAFFT_AA])["sd"]
.MAFFT_VMU <- .mafft_norm(.MAFFT_GRANTHAM_VOLUME[.MAFFT_AA])["mu"]
.MAFFT_VSD <- .mafft_norm(.MAFFT_GRANTHAM_VOLUME[.MAFFT_AA])["sd"]

.MAFFT_VHAT <- ( .MAFFT_GRANTHAM_VOLUME[.MAFFT_AA] - .MAFFT_VMU ) / .MAFFT_VSD
.MAFFT_PHAT <- ( .MAFFT_GRANTHAM_POLARITY[.MAFFT_AA] - .MAFFT_PMU ) / .MAFFT_PSD
names(.MAFFT_VHAT) <- .MAFFT_AA
names(.MAFFT_PHAT) <- .MAFFT_AA

#' .mafft_clean
#'
#' Part of the mafft_native implementation; see the file header for the
#' source it follows.
#'
#' @param seqs See Usage.
#' @param seq_type Defaults to \code{NULL}.
#' @return A list with \code{seqs}, \code{type}.
#' @export
.mafft_clean <- function(seqs, seq_type = NULL) {
  out <- c()
  for (s in seqs) {
    t <- toupper(as.character(s))
    if (nchar(t) == 0L)
      stop("mafft: an empty sequence was given")
    out <- c(out, t)
  }
  if (is.null(seq_type)) {
    letters <- unique(unlist(strsplit(paste(out, collapse = ""), "")))
    letters <- setdiff(letters, c("-", "."))
    if (length(letters) > 0L && all(letters %in% c(.MAFFT_NT, "U", "N"))) {
      seq_type <- "nt"
    } else {
      seq_type <- "aa"
    }
  }
  if (!(seq_type %in% c("aa", "nt")))
    stop("mafft: seq_type must be 'aa' or 'nt'")
  list(seqs = out, type = seq_type)
}

#' residue_vectors
#'
#' Part of the mafft_native implementation; see the file header for the
#' source it follows.
#'
#' @param group See Usage.
#' @param weights Defaults to \code{NULL}.
#' @param seq_type Defaults to \code{"aa"}.
#' @return A list with \code{vol}, \code{pol}.
#' @export
residue_vectors <- function(group, weights = NULL, seq_type = "aa") {
  rows <- vapply(group, function(s) toupper(as.character(s)), character(1))
  if (length(rows) == 0L)
    stop("mafft: an empty group has no vectors")
  L <- nchar(rows[1L])
  for (r in rows) {
    if (nchar(r) != L)
      stop("mafft: sequences in a group must be aligned to the same length")
  }
  if (is.null(weights)) weights <- rep(1.0 / length(rows), length(rows))
  weights <- as.numeric(weights)
  if (length(weights) != length(rows))
    stop("mafft: one weight per sequence is required")
  if (seq_type == "nt") {
    comps <- list()
    for (base in .MAFFT_NT) {
      comps[[base]] <- vapply(seq_len(L), function(n) {
        sum(vapply(seq_along(rows), function(i) {
          if (substr(rows[i], n, n) == base) weights[i] else 0
        }, numeric(1)))
      }, numeric(1))
    }
    return(comps)
  }
  chars <- strsplit(paste(rows, collapse = "\n"), "")[[1L]]
  char_mat <- matrix(chars, nrow = length(rows), ncol = L, byrow = TRUE)
  vol <- numeric(L); pol <- numeric(L)
  for (n in seq_len(L)) {
    for (i in seq_along(rows)) {
      a <- char_mat[i, n]
      vol[n] <- vol[n] + weights[i] * if (a %in% .MAFFT_AA) .MAFFT_VHAT[[a]] else 0
      pol[n] <- pol[n] + weights[i] * if (a %in% .MAFFT_AA) .MAFFT_PHAT[[a]] else 0
    }
  }
  list(vol = vol, pol = pol)
}

#' .mafft_fft_size
#'
#' Part of the mafft_native implementation; see the file header for the
#' source it follows.
#'
#' @param n See Usage.
#' @param m See Usage.
#' @return The value of \code{size}, as built in the body.
#' @export
.mafft_fft_size <- function(n, m) {
  size <- 1L
  while (size < n + m) size <- size * 2L
  size
}

#' .mafft_fft
#'
#' Part of the mafft_native implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @return The value of \code{fft}.
#' @export
.mafft_fft <- function(x) {
  fft(x)
}
#' .mafft_ifft
#'
#' Part of the mafft_native implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @return A numeric value.
#' @export
.mafft_ifft <- function(x) {
  fft(x, inverse = TRUE) / length(x)
}

#' .mafft_xcorr_fft
#'
#' Part of the mafft_native implementation; see the file header for the
#' source it follows.
#'
#' @param a See Usage.
#' @param b See Usage.
#' @return A list with \code{out}, \code{size}.
#' @export
.mafft_xcorr_fft <- function(a, b) {
  n <- length(a); m <- length(b)
  size <- .mafft_fft_size(n, m)
  fa <- .mafft_fft(c(a, rep(0, size - n)))
  fb <- .mafft_fft(c(b, rep(0, size - m)))
  prod <- Conj(fa) * fb
  back <- .mafft_ifft(prod)
  list(out = Re(back), size = size)
}

#' .mafft_xcorr_direct
#'
#' Part of the mafft_native implementation; see the file header for the
#' source it follows.
#'
#' @param a See Usage.
#' @param b See Usage.
#' @param size See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.mafft_xcorr_direct <- function(a, b, size) {
  out <- rep(0.0, size)
  n <- length(a); m <- length(b)
  for (k in seq_len(size) - 1L) {
    tot <- 0.0
    for (i in seq_len(n)) {
      j <- (i + k) %% size
      if (j < m) tot <- tot + a[i] * b[j + 1L]
    }
    out[k + 1L] <- tot
  }
  out
}

#' correlation
#'
#' Part of the mafft_native implementation; see the file header for the
#' source it follows.
#'
#' @param group1 See Usage.
#' @param group2 See Usage.
#' @param weights1 Defaults to \code{NULL}.
#' @param weights2 Defaults to \code{NULL}.
#' @param seq_type Defaults to \code{"aa"}.
#' @param method Defaults to \code{"fft"}.
#' @return A list with \code{lags}, \code{c}, \code{size}.
#' @export
correlation <- function(group1, group2, weights1 = NULL, weights2 = NULL,
                        seq_type = "aa", method = "fft") {
  if (!(method %in% c("fft", "direct")))
    stop("mafft: method must be 'fft' or 'direct'")
  c1 <- residue_vectors(group1, weights1, seq_type)
  c2 <- residue_vectors(group2, weights2, seq_type)
  total <- NULL; size <- NULL
  for (i in seq_along(c1)) {
    a <- c1[[i]]; b <- c2[[i]]
    if (method == "fft") {
      res <- .mafft_xcorr_fft(a, b)
      part <- res$out; size <- res$size
    } else {
      if (is.null(size)) size <- .mafft_fft_size(length(a), length(b))
      part <- .mafft_xcorr_direct(a, b, size)
    }
    if (is.null(total)) {
      total <- part
    } else {
      total <- total + part
    }
  }
  half <- size %/% 2L
  lags <- c(seq_len(half) - 1L, seq(-half, -1L))
  list(lags = lags, c = total, size = size)
}

#' .mafft_peaks
#'
#' Part of the mafft_native implementation; see the file header for the
#' source it follows.
#'
#' @param lags See Usage.
#' @param c See Usage.
#' @param n_peaks See Usage.
#' @return The value of \code{[}.
#' @export
.mafft_peaks <- function(lags, c, n_peaks) {
  ord <- order(-c, seq_along(c))
  lags[ord[seq_len(min(as.integer(n_peaks), length(lags)))]]
}

.MAFFT_JTT_FREQ <- c(
  0.077, 0.051, 0.043, 0.052, 0.020, 0.041, 0.062, 0.074, 0.023, 0.052,
  0.091, 0.059, 0.024, 0.040, 0.051, 0.069, 0.059, 0.014, 0.032, 0.066)

.MAFFT_JTT_COUNTS <- c(
  247,
  216, 116,
  386, 48, 1433,
  106, 125, 32, 13,
  208, 750, 159, 130, 9,
  600, 119, 180, 2914, 8, 1027,
  1183, 614, 291, 577, 98, 84, 610,
  46, 446, 466, 144, 40, 635, 41, 41,
  173, 76, 130, 37, 19, 20, 43, 25, 26,
  257, 205, 63, 34, 36, 314, 65, 56, 134, 1324,
  200, 2348, 758, 102, 7, 858, 754, 142, 85, 75, 94,
  100, 61, 39, 27, 23, 52, 30, 27, 21, 704, 974, 103,
  51, 16, 15, 8, 66, 9, 13, 18, 50, 196, 1093, 7, 49,
  901, 217, 31, 39, 15, 395, 71, 93, 157, 31, 578, 77, 23, 36,
  2413, 413, 1738, 244, 353, 182, 156, 1131, 138, 172, 436, 228, 54, 309, 1138,
  2440, 230, 693, 151, 66, 149, 142, 164, 76, 930, 172, 398, 343, 39, 412, 2258,
  11, 109, 2, 5, 38, 12, 12, 69, 5, 12, 82, 9, 8, 37, 6, 36, 8,
  41, 46, 114, 89, 164, 40, 15, 15, 514, 61, 84, 20, 17, 850, 22, 164, 45, 41,
  1766, 69, 55, 127, 99, 58, 226, 276, 22, 3938, 1261, 58, 559, 189, 84, 219, 526, 27, 42)

#' .mafft_jtt_exchangeability
#'
#' Part of the mafft_native implementation; see the file header for the
#' source it follows.
#'
#' @return A list with \code{S}, \code{f}.
#' @export
.mafft_jtt_exchangeability <- function() {
  f <- .MAFFT_JTT_FREQ
  S <- matrix(0, 20L, 20L)
  k <- 0L
  for (i in 2L:20L) {
    for (j in 1L:(i - 1L)) {
      k <- k + 1L
      v <- .MAFFT_JTT_COUNTS[k] / (400.0 * f[i] * f[j])
      S[i, j] <- v
      S[j, i] <- v
    }
  }
  list(S = S, f = f)
}

#' jtt_matrix
#'
#' Part of the mafft_native implementation; see the file header for the
#' source it follows.
#'
#' @param pam Defaults to \code{200L}.
#' @param scale Defaults to \code{10}.
#' @return A list with \code{matrix}, \code{freqs}, \code{P}, \code{Q}, \code{pam}, \code{rate}.
#' @export
jtt_matrix <- function(pam = 200L, scale = 10.0) {
  if (pam <= 0)
    stop("mafft: pam must be positive")
  ex <- .mafft_jtt_exchangeability()
  S <- ex$S; f <- ex$f
  Q <- matrix(0, 20L, 20L)
  for (i in 1L:20L) {
    off <- 0.0
    for (j in 1L:20L) {
      if (i == j) next
      Q[i, j] <- S[i, j] * f[j]
      off <- off + Q[i, j]
    }
    Q[i, i] <- -off
  }
  mu <- -sum(f * diag(Q))
  for (i in 1L:20L) for (j in 1L:20L) Q[i, j] <- Q[i, j] / (mu * 100.0)
  rt <- sqrt(f)
  A <- matrix(0, 20L, 20L)
  for (i in 1L:20L) for (j in 1L:20L) A[i, j] <- Q[i, j] * rt[i] / rt[j]
  es <- eigen(A, symmetric = TRUE)
  w <- es$values
  V <- es$vectors
  e <- exp(w * as.numeric(pam))
  P <- matrix(0, 20L, 20L)
  for (i in 1L:20L) {
    for (j in 1L:20L) {
      tot <- sum(V[i, ] * e * V[j, ])
      P[i, j] <- tot * rt[j] / rt[i]
    }
  }
  M <- list()
  for (i in seq_along(.MAFFT_AA)) {
    for (j in seq_along(.MAFFT_AA)) {
      a <- .MAFFT_AA[i]; b <- .MAFFT_AA[j]
      p <- max(P[i, j], 1e-300)
      M[[paste(a, b, sep = "|")]] <- scale * log10(p / f[j])
    }
  }
  list(matrix = M, freqs = setNames(as.list(f), .MAFFT_AA), P = P, Q = Q,
       pam = as.integer(pam),
       rate = -sum(f * diag(Q)))
}

#' .mafft_default_raw_matrix
#'
#' Part of the mafft_native implementation; see the file header for the
#' source it follows.
#'
#' @param seq_type See Usage.
#' @param which Defaults to \code{"jtt200"}.
#' @return A list with \code{M}, \code{freqs}.
#' @export
.mafft_default_raw_matrix <- function(seq_type, which = "jtt200") {
  if (seq_type == "nt") {
    M <- list()
    for (a in .MAFFT_NT) for (b in .MAFFT_NT)
      M[[paste(a, b, sep = "|")]] <- if (a == b) 1.0 else -1.0
    return(list(M = M, freqs = NULL))
  }
  if (which == "grantham") {
    M <- list()
    for (a in .MAFFT_AA) for (b in .MAFFT_AA) {
      M[[paste(a, b, sep = "|")]] <- -((.MAFFT_VHAT[[a]] - .MAFFT_VHAT[[b]])^2 +
                                        (.MAFFT_PHAT[[a]] - .MAFFT_PHAT[[b]])^2)
    }
    return(list(M = M, freqs = NULL))
  }
  j <- jtt_matrix(200L)
  list(M = j$matrix, freqs = j$freqs)
}

#' .mafft_get
#'
#' Part of the mafft_native implementation; see the file header for the
#' source it follows.
#'
#' @param M See Usage.
#' @param a See Usage.
#' @param b See Usage.
#' @return One of two values, depending on the branch taken.
#' @export
.mafft_get <- function(M, a, b) {
  v <- M[[paste(a, b, sep = "|")]]
  if (is.null(v)) 0.0 else v
}

#' normalized_similarity_matrix
#'
#' Part of the mafft_native implementation; see the file header for the
#' source it follows.
#'
#' @param raw_matrix Defaults to \code{NULL}.
#' @param freqs Defaults to \code{NULL}.
#' @param s_a Defaults to \code{0.06}.
#' @param seq_type Defaults to \code{"aa"}.
#' @param mode Defaults to \code{"normalized"}.
#' @param default Defaults to \code{"jtt200"}.
#' @return A list with \code{matrix}, \code{s_a}, \code{alphabet}, \code{average1}, \code{average2}, \code{freqs}, \code{mode}.
#' @export
normalized_similarity_matrix <- function(raw_matrix = NULL, freqs = NULL,
                                         s_a = 0.06, seq_type = "aa",
                                         mode = "normalized",
                                         default = "jtt200") {
  if (!(mode %in% .MAFFT_MATRICES))
    stop("mafft: mode must be one of ", paste(.MAFFT_MATRICES, collapse = ", "))
  if (!(default %in% c("jtt200", "grantham")))
    stop("mafft: default must be 'jtt200' or 'grantham'")
  alpha <- if (seq_type == "nt") .MAFFT_NT else .MAFFT_AA
  default_f <- NULL
  if (is.null(raw_matrix)) {
    dm <- .mafft_default_raw_matrix(seq_type, default)
    M <- dm$M
    default_f <- dm$freqs
  } else {
    M <- list()
    for (k in names(raw_matrix)) M[[k]] <- raw_matrix[[k]]
  }
  if (is.null(freqs) && !is.null(default_f)) freqs <- default_f
  if (is.null(freqs)) freqs <- setNames(rep(1.0 / length(alpha), length(alpha)), alpha)
  tot <- sum(unlist(freqs))
  if (tot <= 0)
    stop("mafft: frequencies must be positive")
  for (a in names(freqs)) freqs[[a]] <- freqs[[a]] / tot
  for (a in alpha) for (b in alpha) {
    if (is.null(M[[paste(a, b, sep = "|")]]))
      stop("mafft: raw_matrix is missing (", a, ", ", b, ")")
  }
  avg1 <- sum(unlist(freqs)[alpha] * vapply(alpha, function(a) .mafft_get(M, a, a), numeric(1)))
  avg2 <- 0
  for (a in alpha) for (b in alpha) avg2 <- avg2 + freqs[[a]] * freqs[[b]] * .mafft_get(M, a, b)
  if (abs(avg1 - avg2) < 1e-15)
    stop("mafft: raw_matrix has no signal (average1 equals average2)")
  base <- list()
  for (a in alpha) for (b in alpha)
    base[[paste(a, b, sep = "|")]] <- (.mafft_get(M, a, b) - avg2) / (avg1 - avg2)
  if (mode == "all_positive") s_a <- -min(unlist(base))
  out <- list()
  for (k in names(base)) out[[k]] <- base[[k]] + s_a
  list(matrix = out, s_a = as.numeric(s_a), alphabet = alpha,
       average1 = avg1, average2 = avg2, freqs = freqs, mode = mode)
}

#' .mafft_site_score
#'
#' Part of the mafft_native implementation; see the file header for the
#' source it follows.
#'
#' @param M See Usage.
#' @param ga See Usage.
#' @param gb See Usage.
#' @param wa See Usage.
#' @param wb See Usage.
#' @param i See Usage.
#' @param j See Usage.
#' @return The value of \code{tot}, as built in the body.
#' @export
.mafft_site_score <- function(M, ga, gb, wa, wb, i, j) {
  tot <- 0.0
  for (wn in seq_along(wa)) {
    a <- substr(ga[wn], i, i)
    if (a == "-") next
    for (wm in seq_along(wb)) {
      b <- substr(gb[wm], j, j)
      if (b == "-") next
      tot <- tot + wa[wn] * wb[wm] * .mafft_get(M, a, b)
    }
  }
  tot
}

#' .mafft_gap_profiles
#'
#' Part of the mafft_native implementation; see the file header for the
#' source it follows.
#'
#' @param group See Usage.
#' @param weights See Usage.
#' @return A list with \code{gs}, \code{ge}.
#' @export
.mafft_gap_profiles <- function(group, weights) {
  L <- nchar(group[1L])
  gs <- rep(0.0, L + 1L); ge <- rep(0.0, L + 1L)
  for (k in seq_along(group)) {
    s <- group[k]; w <- weights[k]
    z <- ifelse(strsplit(s, "")[[1L]] == "-", 1.0, 0.0)
    a <- 1.0 - z
    for (x in seq_len(L)) {
      nxt <- if (x < L) z[x + 1L] else 0.0
      gs[x] <- gs[x] + w * a[x] * nxt
      prv <- if (x > 1L) z[x - 1L] else 0.0
      ge[x] <- ge[x] + w * prv * a[x]
    }
  }
  list(gs = gs, ge = ge)
}

#' .mafft_nw
#'
#' Part of the mafft_native implementation; see the file header for the
#' source it follows.
#'
#' @param g1 See Usage.
#' @param g2 See Usage.
#' @param M See Usage.
#' @param w1 See Usage.
#' @param w2 See Usage.
#' @param s_op See Usage.
#' @return A list with \code{out1}, \code{out2}.
#' @export
.mafft_nw <- function(g1, g2, M, w1, w2, s_op) {
  n <- nchar(g1[1L])
  m <- nchar(g2[1L])
  if (n == 0L) {
    out1 <- rep(strrep("-", m), length(g1))
    out2 <- g2
    return(list(out1 = out1, out2 = out2))
  }
  if (m == 0L) {
    out1 <- g1
    out2 <- rep(strrep("-", n), length(g2))
    return(list(out1 = out1, out2 = out2))
  }
  gp1 <- .mafft_gap_profiles(g1, w1)
  gp2 <- .mafft_gap_profiles(g2, w2)
  gs1 <- gp1$gs; ge1 <- gp1$ge
  gs2 <- gp2$gs; ge2 <- gp2$ge
  neg <- -Inf
  P <- matrix(neg, n + 1L, m + 1L)
  back <- vector("list", (n + 1L) * (m + 1L))
  dim(back) <- c(n + 1L, m + 1L)
  P[1L, 1L] <- 0.0
  for (i in 2L:(n + 1L)) {
    P[i, 1L] <- -s_op * (1.0 - (gs1[1L] + ge1[i - 1L]) / 2.0)
    back[[i, 1L]] <- list(kind = "I", pi = 0L, pj = 0L)
  }
  for (j in 2L:(m + 1L)) {
    P[1L, j] <- -s_op * (1.0 - (gs2[1L] + ge2[j - 1L]) / 2.0)
    back[[1L, j]] <- list(kind = "D", pi = 0L, pj = 0L)
  }
  for (i in 2L:(n + 1L)) {
    for (j in 2L:(m + 1L)) {
      h <- .mafft_site_score(M, g1, g2, w1, w2, i - 1L, j - 1L)
      best_v <- P[i - 1L, j - 1L]
      best_b <- list(kind = "M", pi = i - 1L, pj = j - 1L)
      for (x in 0L:(i - 1L)) {
        if (is.infinite(P[x + 1L, j])) next
        pen <- s_op * (1.0 - (gs1[x + 1L] + ge1[i - 1L]) / 2.0)
        v <- P[x + 1L, j] - pen
        if (v > best_v) { best_v <- v; best_b <- list(kind = "I", pi = x, pj = j - 1L) }
      }
      for (y in 0L:(j - 1L)) {
        if (is.infinite(P[i, y + 1L])) next
        pen <- s_op * (1.0 - (gs2[y + 1L] + ge2[j - 1L]) / 2.0)
        v <- P[i, y + 1L] - pen
        if (v > best_v) { best_v <- v; best_b <- list(kind = "D", pi = i - 1L, pj = y) }
      }
      P[i, j] <- h + best_v
      back[[i, j]] <- best_b
    }
  }
  cols <- list()
  i <- n; j <- m
  while (i > 0L && j > 0L) {
    b <- back[[i + 1L, j + 1L]]
    cols[[length(cols) + 1L]] <- c(i - 1L, j - 1L)
    if (b$kind == "I") {
      for (t in (i - 2L):b$pi) cols[[length(cols) + 1L]] <- c(t, NA)
    } else if (b$kind == "D") {
      for (t in (j - 2L):b$pj) cols[[length(cols) + 1L]] <- c(NA, t)
    }
    i <- b$pi; j <- b$pj
  }
  for (t in (i - 1L):0L) cols[[length(cols) + 1L]] <- c(t, NA)
  for (t in (j - 1L):0L) cols[[length(cols) + 1L]] <- c(NA, t)
  cols <- rev(cols)
  cols <- do.call(rbind, cols)
  out1 <- vapply(g1, function(s) {
    paste0(apply(cols, 1, function(row) {
      if (is.na(row[1L])) "-" else substr(s, row[1L] + 1L, row[1L] + 1L)
    }), collapse = "")
  }, character(1))
  out2 <- vapply(g2, function(s) {
    paste0(apply(cols, 1, function(row) {
      if (is.na(row[2L])) "-" else substr(s, row[2L] + 1L, row[2L] + 1L)
    }), collapse = "")
  }, character(1))
  list(out1 = out1, out2 = out2)
}

#' group_align
#'
#' Part of the mafft_native implementation; see the file header for the
#' source it follows.
#'
#' @param group1 See Usage.
#' @param group2 See Usage.
#' @param scoring See Usage.
#' @param weights1 Defaults to \code{NULL}.
#' @param weights2 Defaults to \code{NULL}.
#' @param s_op Defaults to \code{2.4}.
#' @param anchors Defaults to \code{NULL}.
#' @return The value of \code{.mafft_nw}.
#' @export
group_align <- function(group1, group2, scoring, weights1 = NULL, weights2 = NULL,
                        s_op = 2.4, anchors = NULL) {
  g1 <- vapply(group1, function(s) toupper(as.character(s)), character(1))
  g2 <- vapply(group2, function(s) toupper(as.character(s)), character(1))
  if (length(g1) == 0L || length(g2) == 0L)
    stop("mafft: both groups must be non-empty")
  lens <- c(vapply(g1, nchar, integer(1)), vapply(g2, nchar, integer(1)))
  if (length(unique(lens)) != 2L)
    stop("mafft: a group must be aligned to one length")
  w1 <- if (is.null(weights1)) rep(1.0 / length(g1), length(g1)) else weights1
  w2 <- if (is.null(weights2)) rep(1.0 / length(g2), length(g2)) else weights2
  if (length(w1) != length(g1) || length(w2) != length(g2))
    stop("mafft: one weight per sequence is required")
  M <- if (is.list(scoring)) scoring$matrix else scoring
  if (!is.null(anchors)) {
    n <- nchar(g1[1L]); m <- nchar(g2[1L])
    given <- unique(do.call(rbind, lapply(anchors, function(a) c(as.integer(a[1L]), as.integer(a[2L])))))
    given <- given[order(given[, 1L]), , drop = FALSE]
    if (nrow(given) > 1L) {
      for (r in 2L:nrow(given)) {
        if (given[r, 2L] < given[r - 1L, 2L])
          stop("mafft: anchors cross and cannot lie on one alignment path")
      }
    }
    for (r in seq_len(nrow(given))) {
      if (given[r, 1L] < 0L || given[r, 1L] > n || given[r, 2L] < 0L || given[r, 2L] > m)
        stop("mafft: an anchor is outside the groups")
    }
    pts <- rbind(c(0L, 0L), given, c(n, m))
    pts <- pts[!duplicated(pts), , drop = FALSE]
    out1 <- rep("", length(g1)); out2 <- rep("", length(g2))
    prev <- pts[1L, , drop = FALSE]
    for (pi in 2L:nrow(pts)) {
      pt <- pts[pi, , drop = FALSE]
      a1 <- vapply(g1, function(s) substr(s, prev[1L] + 1L, pt[1L]), character(1))
      a2 <- vapply(g2, function(s) substr(s, prev[2L] + 1L, pt[2L]), character(1))
      if (nchar(a1[1L]) == 0L && nchar(a2[1L]) == 0L) { prev <- pt; next }
      sub <- .mafft_nw(a1, a2, M, w1, w2, s_op)
      out1 <- paste0(out1, sub$out1)
      out2 <- paste0(out2, sub$out2)
      prev <- pt
    }
    return(list(out1 = out1, out2 = out2))
  }
  .mafft_nw(g1, g2, M, w1, w2, s_op)
}

#' find_homologous_segments
#'
#' Part of the mafft_native implementation; see the file header for the
#' source it follows.
#'
#' @param group1 See Usage.
#' @param group2 See Usage.
#' @param scoring See Usage.
#' @param weights1 Defaults to \code{NULL}.
#' @param weights2 Defaults to \code{NULL}.
#' @param seq_type Defaults to \code{"aa"}.
#' @param window Defaults to \code{30L}.
#' @param n_peaks Defaults to \code{20L}.
#' @param threshold Defaults to \code{0.7}.
#' @param max_len Defaults to \code{150L}.
#' @param corr_method Defaults to \code{"fft"}.
#' @return One of two values, depending on the branch taken.
#' @export
find_homologous_segments <- function(group1, group2, scoring,
                                     weights1 = NULL, weights2 = NULL,
                                     seq_type = "aa", window = 30L,
                                     n_peaks = 20L, threshold = 0.7,
                                     max_len = 150L, corr_method = "fft") {
  if (window < 1L || n_peaks < 1L || max_len < 1L)
    stop("mafft: window, n_peaks and max_len must be positive")
  M <- if (is.list(scoring)) scoring$matrix else scoring
  g1 <- vapply(group1, function(s) toupper(as.character(s)), character(1))
  g2 <- vapply(group2, function(s) toupper(as.character(s)), character(1))
  w1 <- if (is.null(weights1)) rep(1.0 / length(g1), length(g1)) else weights1
  w2 <- if (is.null(weights2)) rep(1.0 / length(g2), length(g2)) else weights2
  n <- nchar(g1[1L]); m <- nchar(g2[1L])
  cor <- correlation(g1, g2, w1, w2, seq_type, corr_method)
  segs <- list()
  for (k in .mafft_peaks(cor$lags, cor$c, n_peaks)) {
    lo <- max(0L, -k); hi <- min(n, m - k)
    if (hi - lo < window) next
    run <- NULL
    for (start in lo:(hi - window)) {
      score <- 0
      for (t in seq_len(window)) {
        score <- score + .mafft_site_score(M, g1, g2, w1, w2,
                                            start + t, start + t + k)
      }
      score <- score / window
      if (score > threshold) {
        if (is.null(run)) {
          run <- list(start = start, end = start + window, sc = score)
        } else {
          run$end <- start + window
          run$sc <- c(run$sc, score)
        }
      } else if (!is.null(run)) {
        sc <- mean(run$sc)
        segs[[length(segs) + 1L]] <- c(run$start, run$start + k, run$end - run$start, sc, k)
        run <- NULL
      }
    }
    if (!is.null(run)) {
      sc <- mean(run$sc)
      segs[[length(segs) + 1L]] <- c(run$start, run$start + k, run$end - run$start, sc, k)
    }
  }
  cut <- list()
  for (s in segs) {
    s1 <- s[1L]; s2 <- s[2L]; ln <- s[3L]; sc <- s[4L]; k <- s[5L]
    while (ln > max_len) {
      cut[[length(cut) + 1L]] <- c(s1, s2, max_len, sc, k)
      s1 <- s1 + max_len; s2 <- s2 + max_len; ln <- ln - max_len
    }
    if (ln > 0L) cut[[length(cut) + 1L]] <- c(s1, s2, ln, sc, k)
  }
  if (length(cut) > 0L) {
    mat <- do.call(rbind, cut)
    mat <- mat[order(mat[, 1L]), , drop = FALSE]
    lapply(seq_len(nrow(mat)), function(i) as.integer(mat[i, ]))
  } else {
    list()
  }
}

#' arrange_segments
#'
#' Part of the mafft_native implementation; see the file header for the
#' source it follows.
#'
#' @param segments See Usage.
#' @return A vector, from \code{rev}.
#' @export
arrange_segments <- function(segments) {
  segs <- segments[order(vapply(segments, `[`, integer(1), 1L))]
  n <- length(segs)
  if (n == 0L) return(list())
  best <- vapply(segs, function(s) s[4L] * s[3L], numeric(1))
  prev <- vector("list", n)
  for (i in seq_len(n)) {
    for (j in seq_len(i - 1L)) {
      a <- segs[[j]]; b <- segs[[i]]
      if (a[1L] + a[3L] <= b[1L] && a[2L] + a[3L] <= b[2L]) {
        v <- best[j] + segs[[i]][4L] * segs[[i]][3L]
        if (v > best[i]) { best[i] <- v; prev[[i]] <- j }
      }
    }
  }
  end <- which.max(best)
  chain <- list()
  while (!is.null(end)) {
    chain[[length(chain) + 1L]] <- segs[[end]]
    end <- prev[[end]]
  }
  rev(chain)
}

#' .mafft_anchors_from
#'
#' Part of the mafft_native implementation; see the file header for the
#' source it follows.
#'
#' @param chain See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.mafft_anchors_from <- function(chain) {
  out <- list()
  for (s in chain) {
    if (s[1L] >= 0L && s[2L] >= 0L)
      out[[length(out) + 1L]] <- c(s[1L] + s[3L] %/% 2L, s[2L] + s[3L] %/% 2L)
  }
  out
}

#' sixtuple_distance
#'
#' Part of the mafft_native implementation; see the file header for the
#' source it follows.
#'
#' @param seqs See Usage.
#' @return The value of \code{D}, as built in the body.
#' @export
sixtuple_distance <- function(seqs) {
  coded <- c()
  for (s in seqs) {
    t <- ""
    s <- toupper(as.character(s))
    chars <- strsplit(s, "")[[1L]]
    for (ch in chars) {
      if (ch == "-") next
      found <- FALSE
      for (gi in seq_along(.MAFFT_SIX_GROUPS)) {
        if (ch %in% strsplit(.MAFFT_SIX_GROUPS[gi], "")[[1L]]) {
          t <- paste0(t, letters[gi])
          found <- TRUE
          break
        }
      }
      if (!found) t <- paste0(t, "z")
    }
    coded <- c(coded, t)
  }
  tuple_tab <- lapply(coded, function(t) {
    d <- list()
    for (i in seq_len(nchar(t) - 5L)) {
      key <- substr(t, i, i + 5L)
      d[[key]] <- if (is.null(d[[key]])) 1L else d[[key]] + 1L
    }
    d
  })
  shared <- function(a, b) {
    tot <- 0
    for (k in names(a)) tot <- tot + min(a[[k]], if (is.null(b[[k]])) 0L else b[[k]])
    tot
  }
  n <- length(seqs)
  D <- matrix(0, n, n)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i == j) next
      denom <- min(shared(tuple_tab[[i]], tuple_tab[[i]]),
                   shared(tuple_tab[[j]], tuple_tab[[j]]))
      t <- shared(tuple_tab[[i]], tuple_tab[[j]])
      D[i, j] <- 1.0 - if (denom) t / denom else 0
    }
  }
  D
}

#' guide_tree
#'
#' Part of the mafft_native implementation; see the file header for the
#' source it follows.
#'
#' @param D See Usage.
#' @return The value of \code{lapply}.
#' @export
guide_tree <- function(D) {
  n <- nrow(D)
  if (n < 2L) stop("mafft: a guide tree needs at least two sequences")
  clusters <- as.list(seq_len(n))
  for (i in seq_len(n)) clusters[[i]] <- list(i)
  dist <- matrix(0, n, n)
  for (i in seq_len(n)) for (j in seq_len(n)) dist[i, j] <- D[i, j]
  merges <- list()
  nxt <- n + 1L
  active <- seq_len(n)
  while (length(active) > 1L) {
    best_d <- Inf; bi <- 0L; bj <- 0L
    for (k in seq_along(active)) {
      i <- active[k]
      for (kk in (k + 1L):length(active)) {
        j <- active[kk]
        if (dist[i, j] < best_d) { best_d <- dist[i, j]; bi <- i; bj <- j }
      }
    }
    members <- c(clusters[[bi]], clusters[[bj]])
    merges[[length(merges) + 1L]] <- list(i = bi, j = bj, new = nxt,
                                          members = members)
    for (k in active) {
      if (k %in% c(bi, bj)) next
      ni <- length(clusters[[bi]]); nj <- length(clusters[[bj]])
      d <- (ni * dist[bi, k] + nj * dist[bj, k]) / (ni + nj)
      dist[nxt, k] <- d; dist[k, nxt] <- d
    }
    clusters[[nxt]] <- members
    active <- c(active[!active %in% c(bi, bj)], nxt)
    nxt <- nxt + 1L
  }
  lapply(merges, function(m) list(i = m$i, j = m$j, new = m$new,
                                  members = m$members))
}

#' .mafft_weights
#'
#' Part of the mafft_native implementation; see the file header for the
#' source it follows.
#'
#' @param k See Usage.
#' @return The value of \code{rep}.
#' @export
.mafft_weights <- function(k) rep(1.0 / k, k)

#' progressive_align
#'
#' Part of the mafft_native implementation; see the file header for the
#' source it follows.
#'
#' @param seqs See Usage.
#' @param scoring See Usage.
#' @param tree Defaults to \code{NULL}.
#' @param seq_type Defaults to \code{"aa"}.
#' @param s_op Defaults to \code{2.4}.
#' @param use_fft Defaults to \code{TRUE}.
#' @param ... Passed through.
#' @return A vector, from \code{unlist}.
#' @export
progressive_align <- function(seqs, scoring, tree = NULL, seq_type = "aa",
                              s_op = 2.4, use_fft = TRUE, ...) {
  seqs <- vapply(seqs, function(s) toupper(as.character(s)), character(1))
  if (length(seqs) < 2L)
    stop("mafft: at least two sequences are needed")
  if (is.null(tree)) tree <- guide_tree(sixtuple_distance(seqs))
  profiles <- as.list(seq_along(seqs))
  for (i in seq_along(seqs)) profiles[[i]] <- list(i, seqs[i])
  names(profiles) <- as.character(seq_along(seqs))
  members <- as.list(seq_along(seqs))
  for (i in seq_along(seqs)) members[[i]] <- list(i)
  names(members) <- as.character(seq_along(seqs))
  kw <- list(...)
  for (m in tree) {
    i <- as.character(m$i); j <- as.character(m$j); new <- as.character(m$new)
    g1 <- profiles[[i]]; g2 <- profiles[[j]]
    if (length(g1) == 2L) g1 <- g1[[2L]]
    if (length(g2) == 2L) g2 <- g2[[2L]]
    if (is.list(g1) && !is.character(g1)) g1 <- unlist(g1)
    if (is.list(g2) && !is.character(g2)) g2 <- unlist(g2)
    anchors <- NULL
    if (use_fft) {
      segs <- find_homologous_segments(g1, g2, scoring,
                                       .mafft_weights(length(g1)),
                                       .mafft_weights(length(g2)),
                                       seq_type,
                                       window = kw$window %||% 30L,
                                       n_peaks = kw$n_peaks %||% 20L,
                                       threshold = kw$threshold %||% 0.7,
                                       max_len = kw$max_len %||% 150L)
      ac <- .mafft_anchors_from(arrange_segments(segs))
      if (length(ac) > 0L) anchors <- ac
    }
    a <- group_align(g1, g2, scoring, .mafft_weights(length(g1)),
                     .mafft_weights(length(g2)), s_op, anchors)
    profiles[[new]] <- c(a$out1, a$out2)
    members[[new]] <- c(members[[i]], members[[j]])
    profiles[[i]] <- NULL; profiles[[j]] <- NULL
    members[[i]] <- NULL; members[[j]] <- NULL
  }
  root <- names(profiles)[1L]
  order <- unlist(members[[root]])
  out <- vector("list", length(seqs))
  for (pos in seq_along(order)) {
    out[[order[pos]]] <- profiles[[root]][pos]
  }
  unlist(out)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' wsp_score
#'
#' Part of the mafft_native implementation; see the file header for the
#' source it follows.
#'
#' @param alignment See Usage.
#' @param scoring See Usage.
#' @param s_op Defaults to \code{2.4}.
#' @param weights Defaults to \code{NULL}.
#' @return The value of \code{total}, as built in the body.
#' @export
wsp_score <- function(alignment, scoring, s_op = 2.4, weights = NULL) {
  aln <- vapply(alignment, function(s) toupper(as.character(s)), character(1))
  if (length(unique(vapply(aln, nchar, integer(1)))) != 1L)
    stop("mafft: an alignment must be rectangular")
  M <- if (is.list(scoring)) scoring$matrix else scoring
  k <- length(aln)
  w <- if (is.null(weights)) .mafft_weights(k) else weights
  total <- 0.0
  for (i in seq_len(k)) {
    for (j in (i + 1L):k) {
      pair <- w[i] * w[j]
      opened <- FALSE
      cols_i <- strsplit(aln[i], "")[[1L]]
      cols_j <- strsplit(aln[j], "")[[1L]]
      for (r in seq_along(cols_i)) {
        a <- cols_i[r]; b <- cols_j[r]
        if (a == "-" || b == "-") {
          if (!opened) { total <- total - pair * s_op; opened <- TRUE }
        } else {
          opened <- FALSE
          total <- total + pair * .mafft_get(M, a, b)
        }
      }
    }
  }
  total
}

#' .mafft_degap
#'
#' Part of the mafft_native implementation; see the file header for the
#' source it follows.
#'
#' @param group See Usage.
#' @return A vector, from \code{vapply}.
#' @export
.mafft_degap <- function(group) {
  if (length(group) == 0L) return(group)
  L <- nchar(group[1L])
  keep <- integer(0)
  cols <- lapply(group, function(s) strsplit(s, "")[[1L]])
  for (i in seq_len(L)) {
    if (any(vapply(cols, function(c) c[i] != "-", logical(1))))
      keep <- c(keep, i)
  }
  vapply(group, function(s) {
    chars <- strsplit(s, "")[[1L]]
    paste0(chars[keep], collapse = "")
  }, character(1))
}

#' iterative_refine
#'
#' Part of the mafft_native implementation; see the file header for the
#' source it follows.
#'
#' @param alignment See Usage.
#' @param scoring See Usage.
#' @param tree Defaults to \code{NULL}.
#' @param s_op Defaults to \code{2.4}.
#' @param max_iterate Defaults to \code{16L}.
#' @param seq_type Defaults to \code{"aa"}.
#' @param use_fft Defaults to \code{TRUE}.
#' @param ... Passed through.
#' @return A list with \code{aln}, \code{score}, \code{rounds}.
#' @export
iterative_refine <- function(alignment, scoring, tree = NULL, s_op = 2.4,
                             max_iterate = 16L, seq_type = "aa",
                             use_fft = TRUE, ...) {
  aln <- vapply(alignment, function(s) toupper(as.character(s)), character(1))
  if (max_iterate < 1L)
    stop("mafft: max_iterate must be at least 1")
  best <- wsp_score(aln, scoring, s_op)
  if (is.null(tree)) {
    degap <- .mafft_degap(aln)
    tree <- guide_tree(sixtuple_distance(degap))
  }
  groups <- list()
  aln_n <- length(aln)
  for (m in tree[-length(tree)]) {
    members <- m$members
    rest <- setdiff(seq_len(aln_n), members)
    if (length(members) > 0L && length(rest) > 0L)
      groups[[length(groups) + 1L]] <- list(members = members, rest = rest)
  }
  rounds <- 0L
  for (it in seq_len(max_iterate)) {
    improved <- FALSE
    for (g in groups) {
      members <- g$members; rest <- g$rest
      g1 <- .mafft_degap(aln[members])
      g2 <- .mafft_degap(aln[rest])
      anchors <- NULL
      if (use_fft) {
        kw <- list(...)
        segs <- find_homologous_segments(g1, g2, scoring,
                                         .mafft_weights(length(g1)),
                                         .mafft_weights(length(g2)),
                                         seq_type,
                                         window = kw$window %||% 30L,
                                         n_peaks = kw$n_peaks %||% 20L,
                                         threshold = kw$threshold %||% 0.7,
                                         max_len = kw$max_len %||% 150L)
        ac <- .mafft_anchors_from(arrange_segments(segs))
        if (length(ac) > 0L) anchors <- ac
      }
      a <- group_align(g1, g2, scoring, .mafft_weights(length(g1)),
                       .mafft_weights(length(g2)), s_op, anchors)
      cand <- vector("list", aln_n)
      for (pos in seq_along(members)) cand[[members[pos]]] <- a$out1[pos]
      for (pos in seq_along(rest)) cand[[rest[pos]]] <- a$out2[pos]
      sc <- wsp_score(unlist(cand), scoring, s_op)
      if (sc > best + 1e-12) {
        aln <- unlist(cand); best <- sc; improved <- TRUE
      }
    }
    rounds <- rounds + 1L
    if (!improved) break
  }
  list(aln = aln, score = best, rounds = rounds)
}

#' mafft_alignment
#'
#' Part of the mafft_native implementation; see the file header for the
#' source it follows.
#'
#' @param sequences See Usage.
#' @param method Defaults to \code{"FFT-NS-2"}.
#' @param seq_type Defaults to \code{NULL}.
#' @param raw_matrix Defaults to \code{NULL}.
#' @param freqs Defaults to \code{NULL}.
#' @param s_a Defaults to \code{0.06}.
#' @param s_op Defaults to \code{2.4}.
#' @param matrix Defaults to \code{"normalized"}.
#' @param window Defaults to \code{30L}.
#' @param n_peaks Defaults to \code{20L}.
#' @param threshold Defaults to \code{0.7}.
#' @param max_len Defaults to \code{150L}.
#' @param max_iterate Defaults to \code{16L}.
#' @return A list with \code{estimate}, \code{alignment}, \code{score}, \code{method}, \code{seq_type}, \code{length}, \code{n}, \code{s_a}, \code{s_op}, \code{matrix_mode}, \code{tree}, \code{refine_rounds}, \code{note}.
#' @export
mafft_alignment <- function(sequences, method = "FFT-NS-2", seq_type = NULL,
                            raw_matrix = NULL, freqs = NULL, s_a = 0.06,
                            s_op = 2.4, matrix = "normalized",
                            window = 30L, n_peaks = 20L, threshold = 0.7,
                            max_len = 150L, max_iterate = 16L) {
  if (!(method %in% .MAFFT_METHODS))
    stop("mafft: method must be one of ", paste(.MAFFT_METHODS, collapse = ", "))
  cl <- .mafft_clean(sequences, seq_type)
  seqs <- cl$seqs; kind <- cl$type
  if (length(seqs) < 2L)
    stop("mafft: at least two sequences are needed")
  sc <- normalized_similarity_matrix(raw_matrix, freqs, s_a, kind, matrix)
  use_fft <- startsWith(method, "FFT")
  kw <- list(window = window, n_peaks = n_peaks, threshold = threshold,
             max_len = max_len)
  tree1 <- guide_tree(sixtuple_distance(seqs))
  aln <- progressive_align(seqs, sc, tree1, kind, s_op, use_fft,
                           window = window, n_peaks = n_peaks,
                           threshold = threshold, max_len = max_len)
  tree_used <- tree1; rounds <- 0L
  if (method %in% c("FFT-NS-2", "NW-NS-2", "FFT-NS-i")) {
    tree2 <- guide_tree(sixtuple_distance(aln))
    aln <- progressive_align(seqs, sc, tree2, kind, s_op, use_fft,
                             window = window, n_peaks = n_peaks,
                             threshold = threshold, max_len = max_len)
    tree_used <- tree2
  }
  score <- wsp_score(aln, sc, s_op)
  if (method == "FFT-NS-i") {
    r <- iterative_refine(aln, sc, tree_used, s_op, max_iterate, kind,
                          use_fft, window = window, n_peaks = n_peaks,
                          threshold = threshold, max_len = max_len)
    aln <- r$aln; score <- r$score; rounds <- r$rounds
  }
  list(estimate = aln, alignment = aln, score = as.numeric(score),
       method = method, seq_type = kind, length = nchar(aln[1L]),
       n = length(seqs), s_a = sc$s_a, s_op = as.numeric(s_op),
       matrix_mode = matrix, tree = tree_used, refine_rounds = rounds,
       note = "Katoh et al. 2002: the FFT finds homologous segments and the residue DP is restricted to the sub-matrices between their centres; NW-NS-* skip the FFT and matrix='all_positive' is the paper's NW-AP-2 control, whose S_a comes out at 0.8211 against the 0.82 the paper prints. The default raw matrix is the paper's own 200-PAM JTT log-odds; default='grantham' builds one from the volume/polarity vectors instead.")
}

mafftalignment <- mafft_alignment

#' .mafft_cheatsheet
#'
#' Part of the mafft_native implementation; see the file header for the
#' source it follows.
#'
#' @return A character value.
#' @export
.mafft_cheatsheet <- function() {
  paste("mafft: MAFFT (Katoh et al. 2002). Residues become Grantham ",
        "volume/polarity vectors, c(k) = c_v(k) + c_p(k) is got by ",
        "FFT as V1*(m).V2(m), a 30-site window over the top 20 peaks ",
        "at 0.7/site gives homologous segments (merged, then cut at ",
        "150), a segment DP arranges them, and the residue DP runs ",
        "only between their centres. Equation 7 rescales any matrix ",
        "so random sequence scores S_a and identity scores 1 + S_a; ",
        "the gap penalty S_op{1 - [g_start + g_end]/2} is zero where ",
        "the group already has that gap. method= FFT-NS-1, FFT-NS-2, ",
        "FFT-NS-i, NW-NS-1, NW-NS-2.", sep = "")
}

#' morie_mafft
#'
#' Part of the mafft_native implementation; see the file header for the
#' source it follows.
#'
#' @param op See Usage.
#' @param ... Passed through.
#' @return The value of \code{switch}.
#' @export
morie_mafft <- function(op, ...) {
  if (missing(op) || length(op) != 1L)
    stop("mafft: op must be one of mafft_alignment, residue_vectors, correlation, find_homologous_segments, arrange_segments, normalized_similarity_matrix, jtt_matrix, group_align, sixtuple_distance, guide_tree, progressive_align, iterative_refine, wsp_score, cheatsheet")
  op <- as.character(op)
  switch(op,
    "mafft_alignment" = mafft_alignment(...),
    "mafftalignment" = mafft_alignment(...),
    "residue_vectors" = residue_vectors(...),
    "correlation" = correlation(...),
    "find_homologous_segments" = find_homologous_segments(...),
    "arrange_segments" = arrange_segments(...),
    "normalized_similarity_matrix" = normalized_similarity_matrix(...),
    "jtt_matrix" = jtt_matrix(...),
    "group_align" = group_align(...),
    "sixtuple_distance" = sixtuple_distance(...),
    "guide_tree" = guide_tree(...),
    "progressive_align" = progressive_align(...),
    "iterative_refine" = iterative_refine(...),
    "wsp_score" = wsp_score(...),
    "cheatsheet" = list(cheatsheet = .mafft_cheatsheet()),
    stop("mafft: unknown op ", shQuote(op))
  )
}
