# ROUGE: recall-oriented understudy for gisting evaluation.
#
# Lin, C.-Y. (2004) "ROUGE: A Package for Automatic Evaluation of
# Summaries", *Text Summarization Branches Out (ACL workshop)*, 74-81.
#
# Three variants, all in the paper and all implemented here.
#
# ROUGE-N (Sec. 2), n-gram recall against the reference:
#
#     ROUGE-N = sum_{S in {Ref}} sum_{g_n in S} Count_match(g_n) /
#               sum_{S in {Ref}} sum_{g_n in S} Count(g_n)
#
# where Count_match is clipped -- an n-gram occurring twice in the
# reference and five times in the candidate counts twice, not five.
# Without the clip, repeating one good word inflates the score.
#
# ROUGE-L (Sec. 3), longest common subsequence, which rewards in-order
# overlap without requiring contiguity:
#
#     R_lcs = LCS(X,Y) / m,  P_lcs = LCS(X,Y) / n,
#     F_lcs = (1+beta^2) R_lcs P_lcs / (R_lcs + beta^2 P_lcs)
#
# with m, n the reference and candidate lengths.
#
# ROUGE-W (Sec. 4), weighted LCS, which additionally prefers consecutive
# matches by scoring a run of length k as f(k) = k^alpha rather than k.
# Two candidates with the same LCS length but different contiguity get
# the same ROUGE-L and different ROUGE-W; that difference is the reason
# the variant exists. The inverse f^-1(x) = x^(1/alpha) is applied
# before forming the ratios, exactly as in the paper.
#
# Multiple references are handled per Sec. 3.2 by taking the best score
# over references.

.rouge_toks <- function(x) {
  if (is.character(x) && length(x) == 1L) {
    if (nchar(x) == 0L) return(character(0))
    return(strsplit(x, "\\s+")[[1]])
  }
  if (is.list(x)) {
    return(as.character(unlist(x)))
  }
  return(as.character(x))
}

.rouge_ngrams <- function(toks, n) {
  len <- length(toks)
  if (len < n) return(list())
  starts <- seq_len(len - n + 1L)
  lapply(starts, function(i) toks[i:(i + n - 1L)])
}

.rouge_counts <- function(keys) {
  if (length(keys) == 0L) {
    return(structure(integer(0), names = character(0)))
  }
  table(keys)
}

.rouge_prf <- function(match, n_cand, n_ref, beta) {
  p <- if (n_cand > 0L) as.numeric(match) / n_cand else 0.0
  r <- if (n_ref > 0L) as.numeric(match) / n_ref else 0.0
  if (p <= 0.0 || r <= 0.0) {
    f <- 0.0
  } else {
    b2 <- beta * beta
    f <- (1.0 + b2) * p * r / (r + b2 * p)
  }
  list(precision = p, recall = r, f1 = f)
}

.rouge_get_refs_complex <- function(reference) {
  if (is.list(reference)) {
    if (length(reference) == 0L) return(list())
    if (!is.character(reference[[1]])) {
      return(reference)
    }
    is_sentence <- vapply(reference, function(r) {
      grepl(" ", r, fixed = TRUE) || length(.rouge_toks(r)) > 1L
    }, logical(1))
    if (all(is_sentence)) {
      return(reference)
    }
    return(list(reference))
  }
  if (is.character(reference)) {
    if (length(reference) == 1L) {
      return(list(reference))
    }
    is_sentence <- vapply(reference, function(r) {
      grepl(" ", r, fixed = TRUE) || length(.rouge_toks(r)) > 1L
    }, logical(1))
    if (all(is_sentence)) {
      return(as.list(reference))
    }
    return(list(reference))
  }
  list(reference)
}

.rouge_get_refs_simple <- function(reference) {
  if (is.character(reference) && length(reference) == 1L) {
    return(list(reference))
  }
  if (is.list(reference)) {
    return(reference)
  }
  if (is.character(reference)) {
    return(as.list(reference))
  }
  list(reference)
}

.rouge_lcs_length <- function(a, b) {
  m <- length(a)
  n <- length(b)
  if (m == 0L || n == 0L) return(0L)
  prev <- rep(0L, n + 1L)
  for (i in 1:m) {
    cur <- rep(0L, n + 1L)
    ai <- a[i]
    for (j in 1:n) {
      if (identical(ai, b[j])) {
        cur[j + 1L] <- prev[j] + 1L
      } else {
        cur[j + 1L] <- if (cur[j] >= prev[j + 1L]) cur[j] else prev[j + 1L]
      }
    }
    prev <- cur
  }
  prev[n + 1L]
}

.rouge_wlcs <- function(a, b, alpha) {
  m <- length(a)
  n <- length(b)
  if (m == 0L || n == 0L) return(0.0)
  c_mat <- matrix(0.0, nrow = m + 1L, ncol = n + 1L)
  w_mat <- matrix(0L, nrow = m + 1L, ncol = n + 1L)
  for (i in 1:m) {
    for (j in 1:n) {
      if (identical(a[i], b[j])) {
        k <- w_mat[i, j]
        c_mat[i + 1L, j + 1L] <- c_mat[i, j] +
          ((k + 1.0) ^ alpha - as.numeric(k) ^ alpha)
        w_mat[i + 1L, j + 1L] <- k + 1L
      } else {
        if (c_mat[i, j + 1L] >= c_mat[i + 1L, j]) {
          c_mat[i + 1L, j + 1L] <- c_mat[i, j + 1L]
        } else {
          c_mat[i + 1L, j + 1L] <- c_mat[i + 1L, j]
        }
        w_mat[i + 1L, j + 1L] <- 0L
      }
    }
  }
  c_mat[m + 1L, n + 1L]
}

#' morie_lcs_length
#'
#' Part of the rouge_native implementation; see the file header for the
#' source it follows.
#'
#' @param a See Usage.
#' @param b See Usage.
#' @return The value of \code{.rouge_lcs_length}.
#' @export
morie_lcs_length <- function(a, b) {
  .rouge_lcs_length(a, b)
}

#' morie_rouge_n
#'
#' Part of the rouge_native implementation; see the file header for the
#' source it follows.
#'
#' @param candidate See Usage.
#' @param reference See Usage.
#' @param n Defaults to \code{1}.
#' @param beta Defaults to \code{1}.
#' @return The value of \code{best}, as built in the body.
#' @export
morie_rouge_n <- function(candidate, reference, n = 1, beta = 1.0) {
  n <- as.integer(n)
  if (n < 1L) {
    stop(sprintf("rouge_n: n must be at least 1, got %s", n))
  }
  c_toks <- .rouge_toks(candidate)
  c_ngrams <- .rouge_ngrams(c_toks, n)
  refs <- .rouge_get_refs_complex(reference)
  best <- NULL
  for (ref in refs) {
    rg <- .rouge_ngrams(.rouge_toks(ref), n)
    c_keys <- vapply(c_ngrams, function(x) paste(x, collapse = "\r"),
                     character(1))
    rg_keys <- vapply(rg, function(x) paste(x, collapse = "\r"),
                      character(1))
    cc <- .rouge_counts(c_keys)
    rc <- .rouge_counts(rg_keys)
    match <- 0L
    if (length(cc) > 0L) {
      for (g in names(cc)) {
        rc_count <- rc[[g]]
        if (is.na(rc_count)) rc_count <- 0L
        match <- match + min(cc[[g]], rc_count)
      }
    }
    prf <- .rouge_prf(match, length(c_ngrams), length(rg), beta)
    cand <- list(
      precision = prf$precision,
      recall = prf$recall,
      f1 = prf$f1,
      matches = match,
      n_candidate = length(c_ngrams),
      n_reference = length(rg)
    )
    if (is.null(best) || cand$f1 > best$f1) {
      best <- cand
    }
  }
  best$estimate <- best$recall
  best$n <- n
  best$method <- "ROUGE-N, clipped n-gram recall (Lin 2004, Sec. 2)"
  best
}

#' morie_rouge_l
#'
#' Part of the rouge_native implementation; see the file header for the
#' source it follows.
#'
#' @param candidate See Usage.
#' @param reference See Usage.
#' @param beta Defaults to \code{1}.
#' @return The value of \code{best}, as built in the body.
#' @export
morie_rouge_l <- function(candidate, reference, beta = 1.0) {
  c_toks <- .rouge_toks(candidate)
  refs <- .rouge_get_refs_simple(reference)
  best <- NULL
  for (ref in refs) {
    rt <- .rouge_toks(ref)
    l <- .rouge_lcs_length(c_toks, rt)
    prf <- .rouge_prf(l, length(c_toks), length(rt), beta)
    cand <- list(
      precision = prf$precision,
      recall = prf$recall,
      f1 = prf$f1,
      lcs = l,
      n_candidate = length(c_toks),
      n_reference = length(rt)
    )
    if (is.null(best) || cand$f1 > best$f1) {
      best <- cand
    }
  }
  best$estimate <- best$f1
  best$beta <- beta
  best$method <- "ROUGE-L, LCS-based F measure (Lin 2004, Sec. 3)"
  best
}

#' morie_rouge_w
#'
#' Part of the rouge_native implementation; see the file header for the
#' source it follows.
#'
#' @param candidate See Usage.
#' @param reference See Usage.
#' @param alpha Defaults to \code{1.2}.
#' @param beta Defaults to \code{1}.
#' @return The value of \code{best}, as built in the body.
#' @export
morie_rouge_w <- function(candidate, reference, alpha = 1.2, beta = 1.0) {
  alpha <- as.numeric(alpha)
  if (alpha < 1.0) {
    stop(sprintf("rouge_w: alpha must be at least 1 for consecutive matches to be preferred, got %s", alpha))
  }
  c_toks <- .rouge_toks(candidate)
  refs <- .rouge_get_refs_simple(reference)
  best <- NULL
  for (ref in refs) {
    rt <- .rouge_toks(ref)
    wl <- .rouge_wlcs(c_toks, rt, alpha)
    inv <- 1.0 / alpha
    p <- if (length(c_toks) > 0L) {
      (wl / (length(c_toks) ^ alpha)) ^ inv
    } else 0.0
    r <- if (length(rt) > 0L) {
      (wl / (length(rt) ^ alpha)) ^ inv
    } else 0.0
    if (p <= 0.0 || r <= 0.0) {
      f <- 0.0
    } else {
      b2 <- beta * beta
      f <- (1.0 + b2) * p * r / (r + b2 * p)
    }
    cand <- list(
      precision = p,
      recall = r,
      f1 = f,
      wlcs = wl,
      n_candidate = length(c_toks),
      n_reference = length(rt)
    )
    if (is.null(best) || cand$f1 > best$f1) {
      best <- cand
    }
  }
  best$estimate <- best$f1
  best$alpha <- alpha
  best$method <- "ROUGE-W, weighted LCS (Lin 2004, Sec. 4)"
  best
}

#' morie_rouge
#'
#' Part of the rouge_native implementation; see the file header for the
#' source it follows.
#'
#' @param candidate See Usage.
#' @param reference See Usage.
#' @param variant Defaults to \code{"L"}.
#' @param n Defaults to \code{1}.
#' @param alpha Defaults to \code{1.2}.
#' @param beta Defaults to \code{1}.
#' @return Nothing; this branch always raises.
#' @export
morie_rouge <- function(candidate, reference, variant = "L",
                        n = 1, alpha = 1.2, beta = 1.0) {
  v <- toupper(as.character(variant))
  if (v == "L") {
    return(morie_rouge_l(candidate, reference, beta = beta))
  }
  if (v == "W") {
    return(morie_rouge_w(candidate, reference, alpha = alpha, beta = beta))
  }
  if (v == "N") {
    return(morie_rouge_n(candidate, reference, n = n, beta = beta))
  }
  stop(sprintf("rouge: variant must be N, L or W, got %s", variant))
}

#' morie_rouge_cheatsheet
#'
#' Part of the rouge_native implementation; see the file header for the
#' source it follows.
#'
#' @return A character value.
#' @export
morie_rouge_cheatsheet <- function() {
  "rouge: ROUGE-N clipped n-gram recall; ROUGE-L LCS F with R=LCS/m, P=LCS/n; ROUGE-W weighted LCS f(k)=k^alpha with f^-1 before the ratios; best over multiple references."
}
