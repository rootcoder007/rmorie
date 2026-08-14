# Sources: Deerwester, S., Dumais, S. T., Furnas, G. W., Landauer, T. K. &
# Harshman, R. (1990) "Indexing by Latent Semantic Analysis", Journal
# of the American Society for Information Science 41(6), 391-407,
# doi:10.1002/(SICI)1097-4571(199009)41:6<391::AID-ASI1>3.0.CO;2-9.
# Dumais, S. T. (1991) "Improving the retrieval of information from
# external sources", Behavior Research Methods, Instruments & Computers
# 23(2), 229-236, doi:10.3758/BF03203370.
# Hofmann, T. (1999) "Probabilistic Latent Semantic Analysis", UAI
# 1999, 289-296, arXiv:1301.6705.

.EPS <- 1e-12
.WEIGHTS <- c("raw", "log_entropy", "tfidf")

.ghc_svd <- function(A) {
  A <- as.matrix(A)
  s <- svd(A, nu = nrow(A), nv = ncol(A))
  list(T = s$u, S = s$d, Dt = t(s$v))
}

term_weighting <- function(X, how = "log_entropy") {
  if (!(how %in% .WEIGHTS))
    stop(sprintf("lsa: weighting must be one of %s, got %r",
                 paste(.WEIGHTS, collapse = ", "), how))
  A <- apply(X, c(1, 2), as.numeric)
  t <- nrow(A); d <- ncol(A)
  if (how == "raw") return(A)
  if (how == "tfidf") {
    out <- matrix(0, t, d)
    for (i in seq_len(t)) {
      df <- sum(A[i, ] > 0.0)
      idf <- log((1.0 + d) / (1.0 + df)) + 1.0
      for (j in seq_len(d)) out[i, j] <- A[i, j] * idf
    }
    return(out)
  }
  out <- matrix(0, t, d)
  for (i in seq_len(t)) {
    gf <- sum(A[i, ])
    if (gf <= .EPS) { out[i, ] <- 0; next }
    ent <- 0.0
    for (j in seq_len(d)) {
      p <- A[i, j] / gf
      if (p > 0.0) ent <- ent + p * log(p)
    }
    g <- if (d > 1) 1.0 + ent / log(d) else 1.0
    for (j in seq_len(d)) out[i, j] <- g * log(1.0 + A[i, j])
  }
  out
}

lsa_decompose <- function(X, k_dim = NULL, how = "log_entropy") {
  A <- term_weighting(X, how = how)
  sv <- .ghc_svd(A)
  T0 <- sv$T; S <- sv$S; Dt <- sv$Dt
  full <- length(S)
  kk <- if (is.null(k_dim)) full else as.integer(k_dim)
  if (kk < 1L || kk > full)
    stop(sprintf("lsa: k must lie in 1..%d, got %d", full, kk))
  Tk <- T0[, seq_len(kk), drop = FALSE]
  Sk <- S[seq_len(kk)]
  Dk <- t(Dt[, seq_len(kk), drop = FALSE])
  list(estimate = Tk, T = Tk, S = Sk, D = Dk,
       k = kk, full_rank = full, weighting = how,
       method = "truncated SVD of the term-document matrix; Deerwester et al. (1990)",
       note = "k = full rank reproduces X exactly, which is plain term matching -- the TRUNCATION is what generalises")
}

reconstruct <- function(model) {
  T <- model$T; S <- model$S; D <- model$D
  nT <- nrow(T); nD <- nrow(D); k <- length(S)
  out <- matrix(0, nT, nD)
  for (i in seq_len(nT)) {
    for (j in seq_len(nD)) {
      s <- 0
      for (q in seq_len(k)) s <- s + T[i, q] * S[q] * D[j, q]
      out[i, j] <- s
    }
  }
  out
}

fold_in <- function(query, model) {
  q <- as.numeric(query)
  T <- model$T; S <- model$S
  if (length(q) != nrow(T))
    stop(sprintf("lsa: the query has %d terms but the model has %d",
                 length(q), nrow(T)))
  k <- length(S)
  out <- numeric(k)
  for (f in seq_len(k)) {
    s <- 0
    for (i in seq_along(q)) s <- s + q[i] * T[i, f]
    out[f] <- s / max(S[f], .EPS)
  }
  out
}

cosine_ranking <- function(q_hat, model, top_k = 5) {
  D <- model$D; S <- model$S
  nD <- nrow(D); k <- length(S)
  out <- vector("list", nD)
  for (j in seq_len(nD)) {
    dv <- numeric(k)
    for (f in seq_len(k)) dv[f] <- D[j, f] * S[f]
    na <- sqrt(sum(q_hat * q_hat))
    nb <- sqrt(sum(dv * dv))
    if (na <= .EPS || nb <= .EPS) {
      out[[j]] <- c(j, 0.0)
    } else {
      sc <- 0
      for (f in seq_len(k)) sc <- sc + q_hat[f] * dv[f]
      out[[j]] <- c(j, sc / (na * nb))
    }
  }
  out <- do.call(rbind, out)
  o <- order(-out[, 2])
  list(ranking = lapply(seq_len(min(as.integer(top_k), nrow(out))),
                        function(i) as.integer(out[o[i], 1])),
       scores = out[o, 2],
       n_documents = nD)
}

cheatsheet <- function() {
  "lsa: literal term matching fails through SYNONYMY (the right document uses other words) and POLYSEMY (the wrong one shares a word). Take the SVD of the term-document matrix and keep ~100 factors: the TRUNCATION is the method, since k = full rank reproduces X exactly and generalises nothing. Queries are FOLDED IN as pseudo-documents, q' T S^-1, then ranked by cosine -- no re-decomposition, but new documents do not reshape the space. Weight the counts first; log-entropy is standard."
}

latentsemantic <- lsa_decompose
lsa <- lsa_decompose

morie_lsa <- function(X, k_dim = NULL, how = "log_entropy", query = NULL,
                      top_k = 5L) {
  if (!is.null(query)) {
    model <- lsa_decompose(X, k_dim = k_dim, how = how)
    q_hat <- fold_in(query, model)
    return(cosine_ranking(q_hat, model, top_k = top_k))
  }
  lsa_decompose(X, k_dim = k_dim, how = how)
}
