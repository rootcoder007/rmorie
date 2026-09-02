# Motif discovery by fitting a two-component mixture model with EM
# (MM/MEME).  Sources: Bailey, T. L. & Elkan, C. (1994), "Fitting a
# mixture model by expectation maximization to discover motifs in
# biopolymers", ISMB-94, 28-36; Bailey, T. L. & Elkan, C. (1995),
# "The value of prior knowledge in discovering motifs with MEME",
# ISMB-95, 21-29.
#
# Native implementation mirroring Python morie.fn.motfsr exactly: the
# same two-component mixture, the same E-step (eq. 4) and M-step
# (eqs. 5, 9-13), the same overlap normalisation, the same erasing
# step, the same Bayes-optimal threshold and the same output layout.

.GHC_MOT_NEG_INF <- -Inf

#' @keywords internal
#' @noRd
.ghc_mot_alphabet <- function(seqs, alphabet) {
  if (!is.null(alphabet)) {
    alpha <- as.character(alphabet)
    if (any(duplicated(alpha)))
      stop("motfsr: alphabet has repeated letters")
    return(alpha)
  }
  seen <- unique(unlist(lapply(seqs, function(s) strsplit(s, "")[[1]])))
  if (length(seen) == 0L) stop("motfsr: the sequences contain no letters")
  sort(seen)
}

#' @keywords internal
#' @noRd
.ghc_mot_prepare <- function(sequences, w, alphabet) {
  seqs <- as.character(sequences)
  if (length(seqs) == 0L) stop("motfsr: sequences must be non-empty")
  w <- as.integer(w)
  if (w < 1L) stop("motfsr: w (motif width) must be >= 1")
  alpha <- .ghc_mot_alphabet(seqs, alphabet)
  idx <- setNames(seq_along(alpha) - 1L, alpha)
  coded <- lapply(seqs, function(s) {
    ch <- strsplit(s, "")[[1]]
    if (!all(ch %in% alpha))
      stop(paste0("motfsr: letter '", ch[!ch %in% alpha][1],
                  "' is not in the alphabet '",
                  paste(alpha, collapse = ""), "'"))
    unname(idx[ch])
  })
  starts <- list()
  for (i in seq_along(coded)) {
    row <- coded[[i]]
    for (j in seq_len(length(row) - w + 1L) - 1L)
      starts[[length(starts) + 1L]] <- c(i, j)
  }
  if (length(starts) == 0L)
    stop(paste0("motfsr: no sequence is at least w = ", w, " long"))
  list(coded = coded, alpha = alpha, starts = starts)
}

#' @keywords internal
#' @noRd
.ghc_mot_mu <- function(coded, L) {
  c <- rep(0, L)
  for (row in coded) c[row + 1L] <- c[row + 1L] + 1
  c / sum(c)
}

#' @keywords internal
#' @noRd
.ghc_mot_uniform_theta <- function(w, L, mu) {
  out <- vector("list", w + 1L)
  for (i in seq_len(w + 1L)) out[[i]] <- as.numeric(mu)
  out
}

#' @keywords internal
#' @noRd
.ghc_mot_theta_from_subsequence <- function(coded, i, j, w, L, mu, weight) {
  theta <- list(as.numeric(mu))
  for (t in seq_len(w)) {
    k <- coded[[i]][j + t]
    rest <- if (L > 1L) (1 - weight) / (L - 1) else 0
    row <- rep(rest, L); row[k + 1L] <- if (L > 1L) weight else 1
    theta[[length(theta) + 1L]] <- row
  }
  theta
}

#' @keywords internal
#' @noRd
.ghc_mot_log_component <- function(theta, coded, i, j, w, comp) {
  tot <- 0
  for (t in seq_len(w)) {
    k <- coded[[i]][j + t]
    f <- if (comp == 1L) theta[[t + 1L]][k + 1L] else theta[[1L]][k + 1L]
    if (f <= 0) return(.GHC_MOT_NEG_INF)
    tot <- tot + log(f)
  }
  tot
}

#' @keywords internal
#' @noRd
.ghc_mot_normalise_windows <- function(z, w, max_sweeps = 100) {
  if (w < 2L) return(z)
  for (s in seq_len(max_sweeps)) {
    worst <- 1 + 1e-12; wi <- -1L; wj <- -1L
    for (i in seq_along(z)) {
      row <- z[[i]]; m <- length(row)
      if (m < w) {
        run <- sum(row)
        if (run > worst) { worst <- run; wi <- i; wj <- 0L }
        next
      }
      run <- sum(row[seq_len(w)])
      if (run > worst) { worst <- run; wi <- i; wj <- 0L }
      for (j in 2:(m - w + 1L)) {
        run <- run + row[j + w - 1L] - row[j - 1L]
        if (run > worst) { worst <- run; wi <- i; wj <- j - 1L }
      }
    }
    if (wi < 0L) break
    hi <- min(wj + w, length(z[[wi]]))
    for (j in (wj + 1L):hi) z[[wi]][j] <- z[[wi]][j] / worst
  }
  z
}

#' One pass of MM: fit the two-component mixture by EM
#'
#' @param sequences Dataset.
#' @param w Motif width.
#' @param alphabet Optional alphabet.
#' @param theta0,lambda0 Starting point.
#' @param beta Pseudo-count total of eq. 13.
#' @param erasing Optional erasing factors.
#' @param max_iter,tol EM stopping rule.
#' @param normalize_overlaps Apply the window constraint.
#' @param erase_by "letter" or "start".
#' @return A list with theta, motif, background, lambda1, z,
#'   log_likelihood, log_likelihood_trace, n_iter, converged, alphabet,
#'   w.
#' @export
morie_motfsr_mm_fit <- function(sequences, w, alphabet = NULL,
                                 theta0 = NULL, lambda0 = NULL,
                                 beta = 0.01, erasing = NULL,
                                 max_iter = 1000, tol = 1e-6,
                                 normalize_overlaps = TRUE,
                                 erase_by = "letter") {
  prep <- .ghc_mot_prepare(sequences, w, alphabet)
  coded <- prep$coded; alpha <- prep$alpha; starts <- prep$starts
  L <- length(alpha); w <- as.integer(w)
  beta <- as.numeric(beta)
  if (beta < 0) stop("motfsr: beta must be >= 0")
  tol <- as.numeric(tol)
  if (tol <= 0) stop("motfsr: tol must be > 0")
  max_iter <- as.integer(max_iter)
  if (max_iter < 1L) stop("motfsr: max_iter must be >= 1")
  n <- length(starts)
  mu <- .ghc_mot_mu(coded, L)
  if (is.null(theta0)) {
    theta <- .ghc_mot_theta_from_subsequence(coded, starts[[1]][1],
                                              starts[[1]][2], w, L, mu, 0.5)
  } else {
    theta <- lapply(theta0, function(r) as.numeric(r))
    if (length(theta) != w + 1L || any(vapply(theta, length, integer(1)) != L))
      stop("motfsr: theta0 must be (w + 1) x L")
  }
  lam1 <- if (is.null(lambda0)) 1 / (2 * w) else as.numeric(lambda0)
  if (lam1 <= 0 || lam1 >= 1) stop("motfsr: lambda0 must lie in (0, 1)")
  if (!(erase_by %in% c("letter", "start")))
    stop("motfsr: erase_by must be 'letter' or 'start'")
  eps <- if (is.null(erasing)) NULL else lapply(erasing, as.numeric)
  trace <- c(); converged <- FALSE
  z_by_seq <- NULL
  it_done <- 0L
  for (it in seq_len(max_iter)) {
    it_done <- it
    z_by_seq <- lapply(coded, function(row) rep(0, max(0L, length(row) - w + 1L)))
    loglik <- 0
    log_l1 <- log(lam1); log_l2 <- log(1 - lam1)
    for (sj in seq_along(starts)) {
      i <- starts[[sj]][1]; j <- starts[[sj]][2]
      a <- log_l1 + .ghc_mot_log_component(theta, coded, i, j, w, 1L)
      b <- log_l2 + .ghc_mot_log_component(theta, coded, i, j, w, 2L)
      m <- if (a > b) a else b
      if (m == .GHC_MOT_NEG_INF) { z_by_seq[[i]][j + 1L] <- 0; next }
      ea <- exp(a - m); eb <- exp(b - m)
      z_by_seq[[i]][j + 1L] <- ea / (ea + eb)
      loglik <- loglik + m + log(ea + eb)
    }
    trace <- c(trace, loglik)
    if (normalize_overlaps)
      z_by_seq <- .ghc_mot_normalise_windows(z_by_seq, w)
    z_sum <- 0
    for (sj in seq_along(starts)) {
      i <- starts[[sj]][1]; j <- starts[[sj]][2]
      z_sum <- z_sum + z_by_seq[[i]][j + 1L]
    }
    lam1 <- min(max(z_sum / n, 1e-12), 1 - 1e-12)
    ccount <- vector("list", w + 1L)
    for (r in seq_len(w + 1L)) ccount[[r]] <- rep(0, L)
    for (sj in seq_along(starts)) {
      i <- starts[[sj]][1]; j <- starts[[sj]][2]
      z1 <- z_by_seq[[i]][j + 1L]; z2 <- 1 - z1
      e <- if (is.null(eps)) 1
           else if (erase_by == "start") eps[[i]][j + 1L] else NA
      for (t in seq_len(w)) {
        k <- coded[[i]][j + t]
        if (!is.null(eps) && erase_by == "letter") e <- eps[[i]][j + t]
        ccount[[t + 1L]][k + 1L] <- ccount[[t + 1L]][k + 1L] + e * z1
        ccount[[1L]][k + 1L] <- ccount[[1L]][k + 1L] + z2
      }
    }
    new <- vector("list", w + 1L)
    for (r in seq_len(w + 1L)) {
      denom <- sum(ccount[[r]]) + beta
      if (denom <= 0) { new[[r]] <- as.numeric(mu); next }
      new[[r]] <- (ccount[[r]] + beta * mu) / denom
    }
    delta <- 0
    for (r in seq_len(w + 1L)) for (k in seq_len(L))
      delta <- delta + (new[[r]][k] - theta[[r]][k])^2
    delta <- sqrt(delta)
    theta <- new
    if (delta < tol) { converged <- TRUE; break }
  }
  list(theta = theta,
       motif = lapply(theta[seq.int(2L, w + 1L)], function(r) as.numeric(r)),
       background = as.numeric(theta[[1L]]),
       lambda1 = lam1, z = z_by_seq,
       log_likelihood = if (length(trace) > 0L) trace[length(trace)]
                        else NaN,
       log_likelihood_trace = trace,
       n_iter = it_done, converged = converged,
       alphabet = alpha, w = w)
}

#' Log-odds classifier matrix
#'
#' @param motif,background Frequency matrices.
#' @return spec with spec\[i\]\[k\] = log(motif\[i\]\[k\] / background\[k\]).
#' @export
morie_motfsr_log_odds_matrix <- function(motif, background) {
  out <- vector("list", length(motif))
  for (i in seq_along(motif)) {
    r <- numeric(length(motif[[i]]))
    for (k in seq_along(motif[[i]])) {
      f <- motif[[i]][k]; b <- background[k]
      r[k] <- if (f <= 0) .GHC_MOT_NEG_INF
              else if (b <= 0) Inf
              else log(f / b)
    }
    out[[i]] <- r
  }
  out
}

#' Bayes-optimal threshold
#'
#' @param lambda1 Component 1 weight.
#' @param loss Optional 2x2 loss matrix.
#' @return The threshold t.
#' @export
morie_motfsr_bayes_threshold <- function(lambda1, loss = NULL) {
  lambda1 <- as.numeric(lambda1)
  if (lambda1 <= 0 || lambda1 >= 1)
    stop("motfsr: lambda1 must lie in (0, 1)")
  t <- log((1 - lambda1) / lambda1)
  if (is.null(loss)) return(t)
  r11 <- loss[[1]][[1]]; r12 <- loss[[1]][[2]]
  r21 <- loss[[2]][[1]]; r22 <- loss[[2]][[2]]
  num <- as.numeric(r12) - as.numeric(r22)
  den <- as.numeric(r21) - as.numeric(r11)
  if (num <= 0 || den <= 0)
    stop(paste0("motfsr: the loss matrix must have r12 > r22 and ",
                "r21 > r11 for the threshold to be defined"))
  t + log(num / den)
}

#' Score every W-mer of a sequence
#'
#' @param spec Log-odds matrix.
#' @param sequence Sequence string.
#' @param alphabet Alphabet.
#' @param threshold Optional threshold; if given, also returns hits.
#' @return Vector of scores, or list(scores, hits).
#' @export
morie_motfsr_score_sequence <- function(spec, sequence, alphabet,
                                         threshold = NULL) {
  alpha <- as.character(alphabet)
  idx <- setNames(seq_along(alpha) - 1L, alpha)
  w <- length(spec)
  s <- as.character(sequence)
  ch <- strsplit(s, "")[[1]]
  scores <- numeric(max(0L, length(ch) - w + 1L))
  for (j in seq_along(scores)) {
    tot <- 0
    for (t in seq_len(w)) {
      cj <- ch[j + t - 1L]
      if (!(cj %in% alpha))
        stop(paste0("motfsr: letter '", cj, "' is not in the alphabet"))
      tot <- tot + spec[[t]][idx[cj] + 1L]
    }
    scores[j] <- tot
  }
  if (is.null(threshold)) return(scores)
  hits <- which(scores >= as.numeric(threshold))
  list(scores = scores, hits = hits - 1L)
}

#' @keywords internal
#' @noRd
.ghc_mot_lambda_grid <- function(n_starts_total, n_seqs, w, lambda0) {
  if (!is.null(lambda0)) return(as.numeric(lambda0))
  lo <- sqrt(n_seqs) / n_starts_total
  hi <- 1 / (2 * w)
  lo <- min(max(lo, 1e-9), 0.5)
  hi <- min(max(hi, lo), 0.5)
  out <- c()
  v <- lo
  while (v < hi) { out <- c(out, v); v <- v * 2 }
  c(out, hi)
}

#' Discover motifs by fitting the MM mixture model
#'
#' @param sequences Dataset.
#' @param w Motif width.
#' @param alphabet Optional alphabet.
#' @param n_motifs Number of passes.
#' @param beta Pseudo-count total.
#' @param lambda0 Optional fixed lambda0.
#' @param max_iter,tol EM stopping rule.
#' @param normalize_overlaps Apply the window constraint.
#' @param starts "subsequences" or "uniform".
#' @param start_weight Probability on the observed letter when seeding
#'   from a subsequence.
#' @param max_starts Cap on subsequence starts tried.
#' @param start_scoring "one_step" or "none".
#' @param erase_by "letter" or "start".
#' @param loss Optional loss matrix.
#' @return A list with estimate / motifs, alphabet, w, n_subsequences,
#'   erasing, method.
#' @export
morie_motfsr <- function(sequences, w, alphabet = NULL, n_motifs = 1,
                          beta = 0.01, lambda0 = NULL,
                          max_iter = 1000, tol = 1e-6,
                          normalize_overlaps = TRUE,
                          starts = "subsequences",
                          start_weight = 0.5, max_starts = 200,
                          start_scoring = "one_step",
                          erase_by = "letter", loss = NULL) {
  if (!(starts %in% c("subsequences", "uniform")))
    stop("motfsr: starts must be 'subsequences' or 'uniform'")
  if (!(start_scoring %in% c("one_step", "none")))
    stop("motfsr: start_scoring must be 'one_step' or 'none'")
  n_motifs <- as.integer(n_motifs)
  if (n_motifs < 1L) stop("motfsr: n_motifs must be >= 1")
  start_weight <- as.numeric(start_weight)
  if (start_weight <= 0 || start_weight >= 1)
    stop("motfsr: start_weight must lie in (0, 1)")
  prep <- .ghc_mot_prepare(sequences, w, alphabet)
  coded <- prep$coded; alpha <- prep$alpha
  all_starts <- prep$starts
  L <- length(alpha); w <- as.integer(w)
  mu <- .ghc_mot_mu(coded, L)
  n <- length(all_starts)
  erasing <- lapply(coded, function(row) rep(1, length(row)))
  if (starts == "uniform") {
    cand <- list(.ghc_mot_uniform_theta(w, L, mu))
  } else {
    step <- max(1L, as.integer(ceiling(n / max(1L, as.integer(max_starts)))))
    cand <- lapply(all_starts[seq(1L, n, by = step)],
                   function(sj)
                     .ghc_mot_theta_from_subsequence(coded, sj[1], sj[2],
                                                      w, L, mu, start_weight))
  }
  lam_grid <- .ghc_mot_lambda_grid(n, length(coded), w, lambda0)
  motifs <- list()
  for (pass in seq_len(n_motifs)) {
    best <- NULL
    for (th0 in cand) {
      for (lam in lam_grid) {
        probe <- if (start_scoring == "one_step")
                   morie_motfsr_mm_fit(sequences, w, alpha, th0, lam,
                                        beta, erasing, 1L, tol,
                                        normalize_overlaps, erase_by)
                 else
                   morie_motfsr_mm_fit(sequences, w, alpha, th0, lam,
                                        beta, erasing, max_iter, tol,
                                        normalize_overlaps, erase_by)
        key <- probe$log_likelihood
        if (is.null(best) || key > best$key)
          best <- list(key = key, th0 = th0, lam = lam, probe = probe)
      }
    }
    fit <- if (start_scoring == "none") best$probe
           else morie_motfsr_mm_fit(sequences, w, alpha, best$th0,
                                     best$lam, beta, erasing, max_iter,
                                     tol, normalize_overlaps, erase_by)
    spec <- morie_motfsr_log_odds_matrix(fit$motif, fit$background)
    t <- morie_motfsr_bayes_threshold(fit$lambda1, loss)
    sites <- list()
    for (i in seq_along(fit$z)) {
      row <- fit$z[[i]]
      for (j in seq_along(row)) {
        sc <- 0
        for (q in seq_len(w)) sc <- sc + spec[[q]][coded[[i]][j + q - 1L] + 1L]
        if (sc >= t) sites[[length(sites) + 1L]] <- c(i - 1L, j - 1L, sc)
      }
    }
    if (length(sites) > 0L) {
      scv <- vapply(sites, function(s) s[3], numeric(1))
      sites <- sites[order(-scv)]
    }
    consensus <- vapply(fit$motif, function(row) {
      k <- which.max(row)
      if (length(k) == 0L) "" else alpha[k]
    }, character(1))
    consensus <- paste(consensus, collapse = "")
    motifs[[length(motifs) + 1L]] <- list(
      motif = fit$motif, background = fit$background,
      lambda1 = fit$lambda1, log_odds = spec, threshold = t,
      sites = sites, n_sites = length(sites), consensus = consensus,
      log_likelihood = fit$log_likelihood, n_iter = fit$n_iter,
      converged = fit$converged, z = fit$z)
    if (pass < n_motifs) {
      z <- fit$z
      for (i in seq_along(erasing)) {
        for (j in seq_along(erasing[[i]])) {
          f <- 1
          lo_ <- max(0L, j - w); hi_ <- min(j, length(z[[i]]))
          for (k in (lo_ + 1L):hi_) f <- f * (1 - z[[i]][k])
          erasing[[i]][j] <- erasing[[i]][j] * f
        }
      }
    }
  }
  list(estimate = motifs, motifs = motifs, alphabet = alpha, w = w,
       n_subsequences = n, erasing = erasing,
       method = "MM two-component mixture EM (Bailey & Elkan 1994)")
}

morie_motfsr_motif_meme <- morie_motfsr
morie_motfsr_motifmeme <- morie_motfsr
