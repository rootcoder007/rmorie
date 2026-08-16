# BLAST -- local alignment by maximal segment pairs.
# Altschul, Gish, Miller, Myers & Lipman (1990) J. Mol. Biol. 215,
# 403-410. Word list, hit scan, X-drop extension. Karlin-Altschul
# (1990) PNAS 87, 2264-2268 provides lambda and K for the
# significance formulas eq.1 and eq.2.

#' morie_msp_exact
#'
#' A step of the blstn_native implementation. Called by \code{morie_estimate_gumbel}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param query Coerced to character by the body, with \code{as.character}.
#' @param subject Coerced to character by the body, with \code{as.character}.
#' @param match Defaults to \code{5}.
#' @param mismatch Defaults to \code{-4}.
#' @param matrix Optional; may be \code{NULL}. A matrix; indexed by row and column.
#' @param alphabet Character; passed to \code{strsplit}. Defaults to \code{"ACGT"}.
#' @return A list with \code{score}, \code{qstart}, \code{sstart}, \code{length}.
#' @export
morie_msp_exact <- function(query, subject, match = 5, mismatch = -4,
                            matrix = NULL, alphabet = "ACGT") {
  q <- as.character(query); s <- as.character(subject)
  if (is.null(matrix)) {
    sc <- function(a, b) if (a == b) match else mismatch
  } else {
    idx <- setNames(seq_along(strsplit(alphabet, "")[[1]]),
                    strsplit(alphabet, "")[[1]])
    sc <- function(a, b) matrix[[idx[[a]], idx[[b]]]]
  }
  qchars <- strsplit(q, "")[[1]]
  schars <- strsplit(s, "")[[1]]
  best <- c(0, 0, 0, 0)
  for (d in (-(length(qchars) - 1L)):(length(schars) - 1L)) {
    qi <- max(0L, -d)
    si <- max(0L, d)
    run <- 0.0
    run_start <- 0L
    t <- 0L
    while (qi + t < length(qchars) && si + t < length(schars)) {
      v <- sc(qchars[qi + t + 1L], schars[si + t + 1L])
      if (run <= 0) {
        run <- v
        run_start <- t
      } else {
        run <- run + v
      }
      if (run > best[1]) {
        best <- c(run, qi + run_start, si + run_start, t - run_start + 1L)
      }
      t <- t + 1L
    }
  }
  list(score = best[1], qstart = best[2], sstart = best[3], length = best[4])
}

#' morie_word_hits
#'
#' A step of the blstn_native implementation. Called by \code{morie_blstn}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param query Coerced to character by the body, with \code{as.character}.
#' @param subject Coerced to character by the body, with \code{as.character}.
#' @param w A count; the body uses it as \code{seq_len(...)}.
#' @param mode One of \code{"exact"}, \code{"neighborhood"}. Defaults to \code{"exact"}.
#' @param threshold Defaults to \code{NULL}.
#' @param match Defaults to \code{5}.
#' @param mismatch Defaults to \code{-4}.
#' @param matrix Optional; may be \code{NULL}. A matrix; indexed by row and column.
#' @param alphabet Character; passed to \code{strsplit}. Defaults to \code{"ACGT"}.
#' @return The value of \code{hits}, as built in the body.
#' @export
morie_word_hits <- function(query, subject, w, mode = "exact",
                            threshold = NULL, match = 5, mismatch = -4,
                            matrix = NULL, alphabet = "ACGT") {
  q <- as.character(query); s <- as.character(subject)
  w <- as.integer(w)
  if (w < 1L) stop("blstn: w must be >= 1")
  if (nchar(q) < w || nchar(s) < w) return(list())
  if (!(mode %in% c("exact", "neighborhood")))
    stop("blstn: mode must be 'exact' or 'neighborhood'")
  if (is.null(matrix)) {
    sc <- function(a, b) if (a == b) match else mismatch
  } else {
    idx <- setNames(seq_along(strsplit(alphabet, "")[[1]]),
                    strsplit(alphabet, "")[[1]])
    sc <- function(a, b) matrix[[idx[[a]], idx[[b]]]]
  }
  qchars <- strsplit(q, "")[[1]]
  schars <- strsplit(s, "")[[1]]
  table <- list()
  for (i in seq_len(length(qchars) - w + 1L)) {
    key <- paste(qchars[i:(i + w - 1L)], collapse = "")
    table[[key]] <- c(table[[key]], i)
  }
  if (mode == "exact") {
    hits <- list()
    for (j in seq_len(length(schars) - w + 1L)) {
      wkey <- paste(schars[j:(j + w - 1L)], collapse = "")
      if (!is.null(table[[wkey]]))
        for (i in table[[wkey]]) hits[[length(hits) + 1L]] <- c(i, j)
    }
    return(hits)
  }
  if (is.null(threshold))
    stop("blstn: mode='neighborhood' needs a threshold T")
  hits <- list()
  for (j in seq_len(length(schars) - w + 1L)) {
    word <- schars[j:(j + w - 1L)]
    for (qword in names(table)) {
      qw <- strsplit(qword, "")[[1]]
      tot <- 0
      for (tt in seq_len(w)) tot <- tot + sc(qw[tt], word[tt])
      if (tot >= threshold)
        for (i in table[[qword]])
          hits[[length(hits) + 1L]] <- c(i, j)
    }
  }
  hits
}

#' extend_one
#'
#' A step of the blstn_native implementation. Called by \code{morie_blstn}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param qchars A vector; its length is taken and its elements indexed.
#' @param schars A vector; its length is taken and its elements indexed.
#' @param qi Numeric; combined arithmetically in the body.
#' @param si Numeric; combined arithmetically in the body.
#' @param w A count; the body uses it as \code{seq_len(...)}.
#' @param sc See Usage.
#' @param X See Usage.
#' @return A list with \code{score}, \code{qs}, \code{ss}, \code{length}.
#' @export
extend_one <- function(qchars, schars, qi, si, w, sc, X) {
  score <- 0
  for (t in seq_len(w)) score <- score + sc(qchars[qi + t], schars[si + t])
  run <- score; cur <- score
  best_right <- 0L
  t <- 0L
  while (qi + w + t < length(qchars) && si + w + t < length(schars)) {
    cur <- cur + sc(qchars[qi + w + t + 1L], schars[si + w + t + 1L])
    t <- t + 1L
    if (cur > run) { run <- cur; best_right <- t }
    else if (run - cur > X) break
  }
  cur <- run
  best_left <- 0L
  t <- 1L
  while (qi - t >= 0 && si - t >= 0) {
    cur <- cur + sc(qchars[qi - t + 1L], schars[si - t + 1L])
    if (cur > run) { run <- cur; best_left <- t }
    else if (run - cur > X) break
    t <- t + 1L
  }
  list(score = run, qs = qi - best_left, ss = si - best_left,
       length = w + best_right + best_left)
}

#' morie_blstn
#'
#' A step of the blstn_native implementation. Called by \code{morie_blast}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param query Coerced to character by the body, with \code{as.character}.
#' @param subjects Character; the body checks with \code{is.character}.
#' @param w Numeric; combined arithmetically in the body. Defaults to \code{11L}.
#' @param match Passed to \code{morie_word_hits}. Defaults to \code{5}.
#' @param mismatch Passed to \code{morie_word_hits}. Defaults to \code{-4}.
#' @param cutoff Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param X Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{20}.
#' @param word_mode Passed to \code{morie_word_hits}. Defaults to \code{"exact"}.
#' @param threshold Passed to \code{morie_word_hits}.
#' @param matrix Optional; may be \code{NULL}. A matrix; indexed by row and column.
#' @param alphabet A vector; its length is taken. Defaults to \code{"ACGT"}.
#' @param lam Optional; may be \code{NULL}. Passed to \code{morie_blast_pvalue}.
#' @param K Optional; may be \code{NULL}. Passed to \code{morie_blast_pvalue}.
#' @param max_hsps Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @param letter_probs Defaults to \code{NULL}.
#' @param pvalues A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return A list with \code{estimate}, \code{hsps}, \code{best_score}, \code{n_hsps}, \code{n_hits}, \code{w}, \code{cutoff}, \code{X}, \code{word_mode}, \code{lam}, \code{K}, \code{karlin_altschul}, \code{note}, \code{method}.
#' @export
morie_blstn <- function(query, subjects, w = 11L, match = 5, mismatch = -4,
                        cutoff = NULL, X = 20, word_mode = "exact",
                        threshold = NULL, matrix = NULL, alphabet = "ACGT",
                        lam = NULL, K = NULL, max_hsps = NULL,
                        letter_probs = NULL, pvalues = TRUE) {
  q <- as.character(query)
  if (!nzchar(q)) stop("blstn: query must be non-empty")
  if (is.character(subjects)) subs <- list(subjects) else {
    subs <- lapply(subjects, as.character)
  }
  if (length(subs) == 0L) stop("blstn: subjects must be non-empty")
  w <- as.integer(w)
  if (w < 1L) stop("blstn: w must be >= 1")
  X <- as.numeric(X)
  if (X < 0) stop("blstn: X must be >= 0")
  qchars <- strsplit(q, "")[[1]]
  schars_all <- lapply(subs, function(x) strsplit(x, "")[[1]])
  if (is.null(matrix)) {
    sc <- function(a, b) if (a == b) match else mismatch
  } else {
    idx <- setNames(seq_along(strsplit(alphabet, "")[[1]]),
                    strsplit(alphabet, "")[[1]])
    sc <- function(a, b) matrix[[idx[[a]], idx[[b]]]]
  }
  if (is.null(cutoff)) {
    cutoff <- w * (if (is.null(matrix)) match else
                     max(diag(matrix)))
  }
  cutoff <- as.numeric(cutoff)

  hsps <- list(); n_hits <- 0L
  for (si in seq_along(subs)) {
    schars <- schars_all[[si]]
    s <- subs[[si]]
    hits <- morie_word_hits(q, s, w, word_mode, threshold, match, mismatch,
                            matrix, alphabet)
    n_hits <- n_hits + length(hits)
    seen <- list()
    for (h in hits) {
      qi <- h[1] - 1L; sj <- h[2] - 1L
      ext <- extend_one(qchars, schars, qi, sj, w, sc, X)
      key <- paste(si, qi - sj, ext$qs, ext$length, sep = "|")
      if (!is.null(seen[[key]]) || ext$score < cutoff) next
      seen[[key]] <- TRUE
      ident <- 0L
      for (tt in seq_len(ext$length))
        if (qchars[ext$qs + tt] == schars[ext$ss + tt]) ident <- ident + 1L
      hsps[[length(hsps) + 1L]] <- list(subject = si, score = ext$score,
                                         qstart = ext$qs, sstart = ext$ss,
                                         length = ext$length,
                                         identities = ident)
    }
  }
  # locally maximal: drop a segment pair contained in a better one
  if (length(hsps) > 0L) {
    scores <- vapply(hsps, function(h) h$score, numeric(1))
    hsps <- hsps[order(-scores)]
  }
  kept <- list()
  for (h in hsps) {
    d <- h$qstart - h$sstart
    covered <- FALSE
    for (g in kept) {
      if (g$subject == h$subject &&
          g$qstart - g$sstart == d &&
          g$qstart <= h$qstart &&
          h$qstart + h$length <= g$qstart + g$length) {
        covered <- TRUE; break
      }
    }
    if (!covered) kept[[length(kept) + 1L]] <- h
  }
  if (!is.null(max_hsps)) kept <- kept[seq_len(min(as.integer(max_hsps),
                                                   length(kept)))]

  ka <- NULL
  if (isTRUE(pvalues) && (is.null(lam) || is.null(K))) {
    try({
      ka <- morie_karlin_altschul(NULL, match, mismatch,
                                 if (is.null(letter_probs))
                                   rep(1 / nchar(alphabet), nchar(alphabet))
                                 else letter_probs,
                                 matrix)
      if (is.null(lam)) lam <- ka$lam
      if (is.null(K)) K <- ka$K
    }, silent = TRUE)
  }
  if (isTRUE(pvalues) && !is.null(lam) && !is.null(K)) {
    for (i in seq_along(kept)) {
      m <- nchar(subs[[kept[[i]]$subject]])
      kept[[i]]$pvalue <- morie_blast_pvalue(kept[[i]]$score,
                                              nchar(q), m, lam, K)
    }
  }
  best_score <- if (length(kept) > 0L)
    max(vapply(kept, function(h) h$score, numeric(1))) else 0
  list(estimate = kept, hsps = kept, best_score = best_score,
       n_hsps = length(kept), n_hits = n_hits, w = w, cutoff = cutoff,
       X = X, word_mode = word_mode, lam = lam, K = K,
       karlin_altschul = ka,
       note = "the X-drop extension is a heuristic: it may miss a higher-scoring extension (Altschul et al. 1990, section 2c); msp_exact gives the guaranteed MSP score",
       method = "BLAST maximal segment pairs (Altschul et al. 1990)")
}

#' morie_blast
#'
#' A step of the blstn_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param query Passed to \code{morie_blstn}.
#' @param subjects Passed to \code{morie_blstn}.
#' @param w Passed to \code{morie_blstn}. Defaults to \code{11L}.
#' @param match Passed to \code{morie_blstn}. Defaults to \code{5}.
#' @param mismatch Passed to \code{morie_blstn}. Defaults to \code{-4}.
#' @param cutoff Passed to \code{morie_blstn}.
#' @param X Passed to \code{morie_blstn}. Defaults to \code{20}.
#' @param word_mode Passed to \code{morie_blstn}. Defaults to \code{"exact"}.
#' @param threshold Passed to \code{morie_blstn}.
#' @param matrix Passed to \code{morie_blstn}.
#' @param alphabet Passed to \code{morie_blstn}. Defaults to \code{"ACGT"}.
#' @param lam Passed to \code{morie_blstn}.
#' @param K Passed to \code{morie_blstn}.
#' @param max_hsps Passed to \code{morie_blstn}.
#' @param letter_probs Passed to \code{morie_blstn}.
#' @param pvalues Passed to \code{morie_blstn}. Defaults to \code{TRUE}.
#' @return The value of \code{morie_blstn}.
#' @export
morie_blast <- function(query, subjects, w = 11L, match = 5, mismatch = -4,
                        cutoff = NULL, X = 20, word_mode = "exact",
                        threshold = NULL, matrix = NULL, alphabet = "ACGT",
                        lam = NULL, K = NULL, max_hsps = NULL,
                        letter_probs = NULL, pvalues = TRUE) {
  morie_blstn(query = query, subjects = subjects, w = w, match = match,
              mismatch = mismatch, cutoff = cutoff, X = X,
              word_mode = word_mode, threshold = threshold, matrix = matrix,
              alphabet = alphabet, lam = lam, K = K, max_hsps = max_hsps,
              letter_probs = letter_probs, pvalues = pvalues)
}

morie_blast_nucleotide <- morie_blstn

#' lattice_check
#'
#' A step of the blstn_native implementation. Called by \code{morie_karlin_altschul}, \code{morie_score_distribution}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @return The value of \code{n}, as built in the body.
#' @export
lattice_check <- function(x) {
  v <- as.numeric(x); n <- as.integer(round(v))
  if (abs(v - n) > 1e-9)
    stop("blstn: scores must lie on the integer lattice (got ", x,
         "); multiply the whole scheme by a common factor first")
  n
}

#' morie_score_distribution
#'
#' A step of the blstn_native implementation. Called by \code{morie_karlin_altschul}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param match Defaults to \code{5}.
#' @param mismatch Defaults to \code{-4}.
#' @param letter_probs Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param matrix Optional; may be \code{NULL}. A vector; indexed elementwise.
#' @param subject_probs Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @return The value of \code{out}, as built in the body.
#' @export
morie_score_distribution <- function(match = 5, mismatch = -4,
                                     letter_probs = NULL, matrix = NULL,
                                     subject_probs = NULL) {
  if (is.null(matrix)) {
    p <- if (is.null(letter_probs)) rep(0.25, 4)
         else as.numeric(letter_probs)
    p <- p / sum(p)
    q <- if (is.null(subject_probs)) p else as.numeric(subject_probs)
    q <- q / sum(q)
    out <- list()
    for (i in seq_along(p)) for (j in seq_along(q)) {
      sc <- lattice_check(if (i == j) match else mismatch)
      key <- as.character(sc)
      out[[key]] <- (if (is.null(out[[key]])) 0 else out[[key]]) +
                     p[i] * q[j]
    }
    return(out)
  }
  p <- as.numeric(letter_probs)
  q <- if (is.null(subject_probs)) p else as.numeric(subject_probs)
  out <- list()
  for (i in seq_along(p)) for (j in seq_along(q)) {
    sc <- lattice_check(matrix[[i]][[j]])
    key <- as.character(sc)
    out[[key]] <- (if (is.null(out[[key]])) 0 else out[[key]]) + p[i] * q[j]
  }
  out
}

#' lambda_star
#'
#' A step of the blstn_native implementation. Called by \code{morie_karlin_altschul}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param dist A vector; indexed elementwise.
#' @param hi Numeric; combined arithmetically in the body. Defaults to \code{20}.
#' @param tol Defaults to \code{1e-14}.
#' @param max_iter A count; the body uses it as \code{seq_len(...)}. Defaults to \code{300}.
#' @return A numeric value.
#' @export
lambda_star <- function(dist, hi = 20, tol = 1e-14, max_iter = 300) {
  scores <- as.numeric(names(dist))
  mean <- sum(scores * unlist(dist))
  if (mean >= 0) stop("blstn: the expected score per letter must be negative (it is ", mean, ")")
  if (max(scores) <= 0)
    stop("blstn: at least one score must be positive")
  f <- function(lam) {
    s <- 0
    for (sc in scores) s <- s + dist[[as.character(sc)]] * exp(lam * sc)
    s - 1
  }
  lo <- 1e-12
  while (f(hi) < 0 && hi < 700) hi <- hi * 2
  if (f(hi) < 0) stop("blstn: no positive root for lambda was found")
  for (k in seq_len(max_iter)) {
    mid <- 0.5 * (lo + hi)
    if (f(mid) > 0) hi <- mid else lo <- mid
    if (hi - lo < tol) break
  }
  0.5 * (lo + hi)
}

#' gcd_span
#'
#' A step of the blstn_native implementation. Called by \code{morie_karlin_altschul}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param scores See Usage.
#' @return One of two values, depending on the branch taken.
#' @export
gcd_span <- function(scores) {
  g <- 0
  for (s in scores) {
    a <- abs(as.integer(s))
    while (a) { tmp <- g %% a; g <- a; a <- tmp }
  }
  if (g > 0) g else 1
}

#' morie_karlin_altschul
#'
#' A step of the blstn_native implementation. Called by \code{morie_blstn}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param dist Optional; may be \code{NULL}. A vector; indexed elementwise.
#' @param match Passed to \code{morie_score_distribution}. Defaults to \code{5}.
#' @param mismatch Passed to \code{morie_score_distribution}. Defaults to \code{-4}.
#' @param letter_probs Passed to \code{morie_score_distribution}.
#' @param matrix Passed to \code{morie_score_distribution}.
#' @param subject_probs Passed to \code{morie_score_distribution}.
#' @param max_terms Coerced to integer by the body, with \code{as.integer}. Defaults to \code{1000}.
#' @param tol Defaults to \code{1e-12}.
#' @param bound One of \code{"lower"}, \code{"mid"}, \code{"upper"}. Defaults to \code{"upper"}.
#' @return A list with \code{lam}, \code{K}, \code{K_upper}, \code{K_lower}, \code{C}, \code{delta}, \code{terms}, \code{series}, \code{mean_score}, \code{distribution}.
#' @export
morie_karlin_altschul <- function(dist = NULL, match = 5, mismatch = -4,
                                  letter_probs = NULL, matrix = NULL,
                                  subject_probs = NULL, max_terms = 1000,
                                  tol = 1e-12, bound = "upper") {
  if (is.null(dist))
    dist <- morie_score_distribution(match, mismatch, letter_probs, matrix,
                                     subject_probs)
  keys <- as.integer(names(dist))
  vals <- as.numeric(unlist(dist))
  dist <- list()
  for (i in seq_along(keys)) {
    if (vals[i] > 0) dist[[as.character(lattice_check(keys[i]))]] <- vals[i]
  }
  tot <- sum(unlist(dist))
  for (k in names(dist)) dist[[k]] <- dist[[k]] / tot
  if (!(bound %in% c("upper", "lower", "mid")))
    stop("blstn: bound must be 'upper', 'lower' or 'mid'")
  lam <- lambda_star(dist)
  delta <- gcd_span(as.integer(names(dist)))
  denom <- 0
  for (k in names(dist)) {
    s <- as.integer(k)
    denom <- denom + s * dist[[k]] * exp(lam * s)
  }
  denom <- lam * denom
  if (denom <= 0)
    stop("blstn: the normalising expectation is not positive; check the scoring scheme")
  conv <- dist
  series <- 0
  terms_used <- 0
  for (k in seq_len(as.integer(max_terms))) {
    e_neg <- 0; p_ge0 <- 0
    for (nm in names(conv)) {
      s <- as.integer(nm)
      v <- conv[[nm]]
      if (s < 0) e_neg <- e_neg + v * exp(lam * s) else p_ge0 <- p_ge0 + v
    }
    term <- (e_neg + p_ge0) / k
    series <- series + term
    terms_used <- k
    if (term < tol && k > 5) break
    nxt <- list()
    for (a in names(conv)) for (b in names(dist)) {
      key <- as.character(as.integer(a) + as.integer(b))
      nxt[[key]] <- (if (is.null(nxt[[key]])) 0 else nxt[[key]]) +
                     conv[[a]] * dist[[b]]
    }
    conv <- list()
    for (nm in names(nxt)) {
      v <- nxt[[nm]]; s <- as.integer(nm)
      if (v * exp(lam * min(s, 0)) > 1e-300 && v > 1e-300) conv[[nm]] <- v
    }
  }
  c_star <- exp(-2 * series) / denom
  x <- lam * delta
  k_low <- c_star * x / (exp(x) - 1)
  k_high <- c_star * x / (1 - exp(-x))
  kval <- switch(bound, upper = k_high, lower = k_low,
                 mid = 0.5 * (k_low + k_high))
  list(lam = lam, K = kval, K_upper = k_high, K_lower = k_low,
       C = c_star, delta = delta, terms = terms_used, series = series,
       mean_score = sum(as.integer(names(dist)) * unlist(dist)),
       distribution = dist)
}

#' morie_blast_pvalue
#'
#' A step of the blstn_native implementation. Called by \code{morie_blstn}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param score Coerced to numeric by the body, with \code{as.numeric}.
#' @param m Coerced to numeric by the body, with \code{as.numeric}.
#' @param n Coerced to numeric by the body, with \code{as.numeric}.
#' @param lam Numeric; combined arithmetically in the body.
#' @param K Numeric; combined arithmetically in the body.
#' @param c Numeric; combined arithmetically in the body. Defaults to \code{1L}.
#' @return A numeric value.
#' @export
morie_blast_pvalue <- function(score, m, n, lam, K, c = 1L) {
  lam <- as.numeric(lam); K <- as.numeric(K)
  if (lam <= 0 || K <= 0) stop("blstn: lam and K must be positive")
  c <- as.integer(c)
  if (c < 1L) stop("blstn: c must be >= 1")
  y <- K * as.numeric(m) * as.numeric(n) * exp(-lam * as.numeric(score))
  tail <- 0
  term <- 1
  for (i in 0:(c - 1L)) {
    if (i > 0L) term <- term * y / i
    tail <- tail + term
  }
  p <- 1 - exp(-y) * tail
  min(max(p, 0), 1)
}

#' morie_estimate_gumbel
#'
#' A step of the blstn_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param m Coerced to numeric by the body, with \code{as.numeric}.
#' @param n Coerced to numeric by the body, with \code{as.numeric}.
#' @param letter_freqs Coerced to numeric by the body, with \code{as.numeric}.
#' @param match Passed to \code{morie_msp_exact}. Defaults to \code{5}.
#' @param mismatch Passed to \code{morie_msp_exact}. Defaults to \code{-4}.
#' @param matrix Passed to \code{morie_msp_exact}.
#' @param alphabet Character; passed to \code{strsplit}. Defaults to \code{"ACGT"}.
#' @param n_sim A count; the body uses it as \code{seq_len(...)}. Defaults to \code{200}.
#' @param seed Coerced to integer by the body, with \code{as.integer}. Defaults to \code{0}.
#' @param quantiles A vector; indexed elementwise. Defaults to \code{c(0.2, 0.9)}.
#' @return A list with \code{lam}, \code{K}, \code{scores}.
#' @export
morie_estimate_gumbel <- function(m, n, letter_freqs, match = 5,
                                  mismatch = -4, matrix = NULL,
                                  alphabet = "ACGT", n_sim = 200, seed = 0,
                                  quantiles = c(0.2, 0.9)) {
  freqs <- as.numeric(letter_freqs)
  tot <- sum(freqs)
  if (tot <= 0) stop("blstn: letter_freqs must be positive")
  freqs <- freqs / tot
  cum <- cumsum(freqs)
  state <- as.integer(seed); if (state == 0) state <- 1
  rnd <- function() {
    state <<- .ghc_lcg31(state)
    state / (1L * 2^31)
  }
  achars <- strsplit(alphabet, "")[[1]]
  draw <- function(length) {
    out <- character(length)
    for (i in seq_len(length)) {
      u <- rnd(); k <- 1L
      while (k < length(cum) && u > cum[k]) k <- k + 1L
      out[i] <- achars[k]
    }
    paste(out, collapse = "")
  }
  scores <- numeric(n_sim)
  for (i in seq_len(n_sim)) {
    a <- draw(as.integer(m)); b <- draw(as.integer(n))
    scores[i] <- morie_msp_exact(a, b, match, mismatch, matrix, alphabet)$score
  }
  scores <- sort(scores)
  N <- length(scores)
  lo <- max(1L, as.integer(quantiles[1] * N))
  hi <- min(N - 1L, as.integer(quantiles[2] * N))
  xs <- numeric(); ys <- numeric()
  for (r in lo:hi) {
    F <- r / (N + 1)
    if (F > 0 && F < 1) {
      xs <- c(xs, scores[r])
      ys <- c(ys, log(-log(F)))
    }
  }
  if (length(xs) < 2) stop("blstn: too few distinct simulated scores to fit; raise n_sim")
  mx <- mean(xs); my <- mean(ys)
  sxy <- sum((xs - mx) * (ys - my))
  sxx <- sum((xs - mx)^2)
  if (sxx <= 0) stop("blstn: simulated scores are constant; raise n_sim")
  slope <- sxy / sxx; intercept <- my - slope * mx
  lam <- -slope
  if (lam <= 0) stop("blstn: the fitted lambda is not positive")
  list(lam = lam, K = exp(intercept) / (as.numeric(m) * as.numeric(n)),
       scores = scores)
}
