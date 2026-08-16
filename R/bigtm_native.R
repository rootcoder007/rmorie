# bigtm -- Bigram topic model.
# Wallach (2006) "Topic Modeling: Beyond Bag-of-Words", ICML.
# MacKay & Peto (1995); Blei, Ng & Jordan (2003); Griffiths & Steyvers (2004).
# Base R only.

.bigtm_EPS <- 1e-300
.PRIORS <- c(1, 2)

#' dirichlet_predictive
#'
#' Part of the bigtm_native implementation; see the file header for the
#' source it follows.
#'
#' @param N_ij See Usage.
#' @param N_j See Usage.
#' @param beta See Usage.
#' @param m See Usage.
#' @return A list with \code{predictive}, \code{lambda}, \code{f}, \code{interpolated}, \code{note}.
#' @export
dirichlet_predictive <- function(N_ij, N_j, beta, m) {
  n <- as.numeric(N_ij)
  mm <- as.numeric(m)
  if (length(n) != length(mm))
    stop(sprintf("bigtm: %d counts for %d prior weights", length(n), length(mm)))
  if (abs(sum(mm) - 1) > 1e-9)
    stop(sprintf("bigtm: m must sum to 1, got %.9f", sum(mm)))
  b <- as.numeric(beta); Nj <- as.numeric(N_j)
  if (b <= 0) stop("bigtm: beta must be positive")
  lam <- b / (Nj + b)
  f <- if (Nj > 0) n / Nj else rep(0, length(n))
  list(predictive = (n + b * mm) / (Nj + b),
       lambda = lam, f = f,
       interpolated = lam * mm + (1 - lam) * f,
       note = paste("eq. (6): lambda_j m_i + (1 - lambda_j) f_{i|j},",
                    "so m_i plays the role of the marginal frequency"))
}

#' lda_predictive
#'
#' Part of the bigtm_native implementation; see the file header for the
#' source it follows.
#'
#' @param N_ik See Usage.
#' @param N_k See Usage.
#' @param beta See Usage.
#' @param m See Usage.
#' @return A list with \code{predictive}, \code{lambda}, \code{eq15_as_printed}, \code{note}.
#' @export
lda_predictive <- function(N_ik, N_k, beta, m) {
  n <- as.numeric(N_ik)
  mm <- as.numeric(m)
  b <- as.numeric(beta); Nk <- as.numeric(N_k)
  if (b <= 0) stop("bigtm: beta must be positive")
  lam <- b / (Nk + b)
  f <- if (Nk > 0) n / Nk else rep(0, length(n))
  list(predictive = (n + b * mm) / (Nk + b),
       lambda = lam,
       eq15_as_printed = lam * f + (1 - lam) * mm,
       note = paste("eq. (15) as printed puts weight lambda_k on f_{i|k};",
                    "expanding eq. (13) puts it on m_i, as eq. (6) does.",
                    "Eq. (13) is used."))
}

#' bigram_topic_predictive
#'
#' Part of the bigtm_native implementation; see the file header for the
#' source it follows.
#'
#' @param N_ijk See Usage.
#' @param N_jk See Usage.
#' @param beta See Usage.
#' @param m See Usage.
#' @param prior Defaults to \code{1}.
#' @return A list with \code{predictive}, \code{prior}, \code{smoothed_by}.
#' @export
bigram_topic_predictive <- function(N_ijk, N_jk, beta, m, prior = 1) {
  if (!(as.integer(prior) %in% .PRIORS))
    stop(sprintf("bigtm: prior must be 1 or 2, got %s", prior))
  n <- as.numeric(N_ijk)
  mm <- as.numeric(m)
  b <- as.numeric(beta)
  list(predictive = (n + b * mm) / (as.numeric(N_jk) + b),
       prior = as.integer(prior),
       smoothed_by = if (as.integer(prior) == 1)
                       "m_i, the same for every context"
                     else "m_{i|k}, which varies with the topic")
}

.counts <- function(D, Tn, Vn, z) {
  N_ijk <- new.env(hash = TRUE)
  N_jk <- new.env(hash = TRUE)
  N_kd <- lapply(D, function(d) numeric(Tn))
  N_d <- numeric(length(D))
  for (d in seq_along(D)) {
    doc <- D[[d]]
    for (t in 2:length(doc)) {
      i <- doc[t]; j <- doc[t - 1]; kk <- z[[d]][t]
      k1 <- paste(i, j, kk, sep = "\r")
      N_ijk[[k1]] <- if (is.null(N_ijk[[k1]])) 1 else N_ijk[[k1]] + 1
      k2 <- paste(j, kk, sep = "\r")
      N_jk[[k2]] <- if (is.null(N_jk[[k2]])) 1 else N_jk[[k2]] + 1
      N_kd[[d]][kk + 1] <- N_kd[[d]][kk + 1] + 1
      N_d[d] <- N_d[d] + 1
    }
  }
  list(N_ijk = N_ijk, N_jk = N_jk, N_kd = N_kd, N_d = N_d)
}

# The Python arm draws from np.random.default_rng, i.e. SplitMix64 --
# not an LCG at all. Making the LCG exact would still have left the two
# arms on different generators, so this takes the bit-identical mirror
# of default_rng that .ghc_rng already provides.
.lcg_uniform <- function(seed, n) {
  .ghc_unif(.ghc_rng(as.numeric(seed)), n)
}

#' gibbs_bigram_topic
#'
#' Part of the bigtm_native implementation; see the file header for the
#' source it follows.
#'
#' @param docs See Usage.
#' @param T See Usage.
#' @param V See Usage.
#' @param alpha Defaults to \code{0.5}.
#' @param beta Defaults to \code{0.5}.
#' @param m Defaults to \code{NULL}.
#' @param n Defaults to \code{NULL}.
#' @param prior Defaults to \code{1}.
#' @param iters Defaults to \code{200L}.
#' @param seed Defaults to \code{0L}.
#' @param burn Defaults to \code{50L}.
#' @return A list with \code{estimate}, \code{z}, \code{topic_posterior}, \code{theta}, \code{N_ijk}, \code{N_jk}, \code{T}, \code{V}, \code{prior}, \code{iterations}, \code{burn_in}, \code{samples_kept}, \code{method}, \code{caveat}.
#' @export
gibbs_bigram_topic <- function(docs, T, V, alpha = 0.5, beta = 0.5,
                               m = NULL, n = NULL, prior = 1,
                               iters = 200L, seed = 0L, burn = 50L) {
  if (!(as.integer(prior) %in% .PRIORS))
    stop(sprintf("bigtm: prior must be 1 or 2, got %s", prior))
  D <- lapply(docs, function(d) as.integer(d))
  if (length(D) == 0) stop("bigtm: no documents given")
  Tn <- as.integer(T); Vn <- as.integer(V)
  if (Tn < 1 || Vn < 1) stop("bigtm: T and V must be at least 1")
  for (d in D) for (v in d)
    if (v < 0 || v >= Vn)
      stop(sprintf("bigtm: a word index is outside the vocabulary of %d", Vn))
  mm <- if (is.null(m)) rep(1 / Vn, Vn) else as.numeric(m)
  nn <- if (is.null(n)) rep(1 / Tn, Tn) else as.numeric(n)
  if (abs(sum(mm) - 1) > 1e-9 || abs(sum(nn) - 1) > 1e-9)
    stop("bigtm: m and n must each sum to 1")
  a <- as.numeric(alpha); b <- as.numeric(beta)
  u <- .lcg_uniform(seed, length(D))
  z <- lapply(seq_along(D), function(i) rep((as.integer(u[i] * Tn) %% Tn),
                                            length(D[[i]])))
  cts <- .counts(D, Tn, Vn, z)
  N_ijk <- cts$N_ijk; N_jk <- cts$N_jk
  N_kd <- cts$N_kd; N_d <- cts$N_d
  acc <- lapply(D, function(doc) matrix(0, nrow = length(doc), ncol = Tn))
  kept <- 0L
  for (it in seq_len(as.integer(iters))) {
    for (d in seq_along(D)) {
      doc <- D[[d]]
      for (t in 2:length(doc)) {
        i <- doc[t]; j <- doc[t - 1]
        old <- z[[d]][t]
        k1 <- paste(i, j, old, sep = "\r")
        N_ijk[[k1]] <- N_ijk[[k1]] - 1
        k2 <- paste(j, old, sep = "\r")
        N_jk[[k2]] <- N_jk[[k2]] - 1
        N_kd[[d]][old + 1] <- N_kd[[d]][old + 1] - 1
        p <- numeric(Tn)
        for (kk in 1:Tn) {
          k1n <- paste(i, j, kk - 1, sep = "\r")
          k2n <- paste(j, kk - 1, sep = "\r")
          nijk <- if (is.null(N_ijk[[k1n]])) 0 else N_ijk[[k1n]]
          njk <- if (is.null(N_jk[[k2n]])) 0 else N_jk[[k2n]]
          w <- (nijk + b * mm[i + 1]) / (njk + b)
          p[kk] <- w * (N_kd[[d]][kk] + a * nn[kk])
        }
        s <- sum(p)
        u2 <- runif(1) * s
        new_k <- Tn
        c_ <- 0
        for (kk in 1:Tn) {
          c_ <- c_ + p[kk]
          if (u2 <= c_) { new_k <- kk; break }
        }
        new_k <- new_k - 1
        z[[d]][t] <- new_k
        k1n <- paste(i, j, new_k, sep = "\r")
        N_ijk[[k1n]] <- if (is.null(N_ijk[[k1n]])) 1 else N_ijk[[k1n]] + 1
        k2n <- paste(j, new_k, sep = "\r")
        N_jk[[k2n]] <- if (is.null(N_jk[[k2n]])) 1 else N_jk[[k2n]] + 1
        N_kd[[d]][new_k + 1] <- N_kd[[d]][new_k + 1] + 1
      }
    }
    if (it > as.integer(burn)) {
      kept <- kept + 1L
      for (d in seq_along(D)) {
        for (t in seq_along(D[[d]])) {
          acc[[d]][t, z[[d]][t] + 1] <- acc[[d]][t, z[[d]][t] + 1] + 1
        }
      }
    }
  }
  post <- lapply(seq_along(D), function(d) {
    ld <- length(D[[d]])
    m <- matrix(0, ld, Tn)
    if (kept > 0) m <- acc[[d]] / kept
    m
  })
  theta <- lapply(seq_along(D), function(d) {
    (N_kd[[d]] + a * nn) / (N_d[d] + a)
  })
  N_ijk_out <- new.env(hash = TRUE)
  for (k in ls(N_ijk)) N_ijk_out[[k]] <- N_ijk[[k]]
  N_jk_out <- new.env(hash = TRUE)
  for (k in ls(N_jk)) N_jk_out[[k]] <- N_jk[[k]]
  list(estimate = z, z = z, topic_posterior = post, theta = theta,
       N_ijk = N_ijk_out, N_jk = N_jk_out,
       T = Tn, V = Vn, prior = as.integer(prior),
       iterations = as.integer(iters), burn_in = as.integer(burn),
       samples_kept = kept,
       method = paste("Gibbs sampling for the bigram topic model;",
                      "Wallach (2006) eqs. (28)-(29)"),
       caveat = paste("eq. (28) as printed divides by {N_k}_-t + beta;",
                      "the context count N_{j,k} + beta is used,",
                      "following eqs. (25) and (29)"))
}

.log_evidence <- function(D, Tn, Vn, z, mm, nn, a, b) {
  cts <- .counts(D, Tn, Vn, z)
  N_ijk <- cts$N_ijk; N_jk <- cts$N_jk
  N_kd <- cts$N_kd; N_d <- cts$N_d
  tot <- 0
  for (k2 in ls(N_jk)) {
    njk <- N_jk[[k2]]
    tot <- tot + lgamma(b) - lgamma(njk + b)
  }
  for (k1 in ls(N_ijk)) {
    parts <- strsplit(k1, "\r", fixed = TRUE)[[1]]
    i <- as.integer(parts[1])
    c <- N_ijk[[k1]]
    tot <- tot + lgamma(c + b * mm[i + 1]) - lgamma(b * mm[i + 1])
  }
  for (d in seq_along(D)) {
    tot <- tot + lgamma(a) - lgamma(N_d[d] + a)
    for (kk in 1:Tn) {
      tot <- tot + lgamma(N_kd[[d]][kk] + a * nn[kk]) -
        lgamma(a * nn[kk])
    }
  }
  tot
}

#' log_evidence
#'
#' Part of the bigtm_native implementation; see the file header for the
#' source it follows.
#'
#' @param docs See Usage.
#' @param T See Usage.
#' @param V See Usage.
#' @param z See Usage.
#' @param alpha Defaults to \code{0.5}.
#' @param beta Defaults to \code{0.5}.
#' @param m Defaults to \code{NULL}.
#' @param n Defaults to \code{NULL}.
#' @return The value of \code{.log_evidence}.
#' @export
log_evidence <- function(docs, T, V, z, alpha = 0.5, beta = 0.5,
                         m = NULL, n = NULL) {
  D <- lapply(docs, function(d) as.integer(d))
  Tn <- as.integer(T); Vn <- as.integer(V)
  mm <- if (is.null(m)) rep(1 / Vn, Vn) else as.numeric(m)
  nn <- if (is.null(n)) rep(1 / Tn, Tn) else as.numeric(n)
  a <- as.numeric(alpha); b <- as.numeric(beta)
  .log_evidence(D, Tn, Vn, z, mm, nn, a, b)
}

bigramtopicmodel <- gibbs_bigram_topic
bigram_topic <- gibbs_bigram_topic
bigramtopic <- gibbs_bigram_topic

# house entry point: the package exports one morie_<module>
morie_bigtm <- gibbs_bigram_topic
