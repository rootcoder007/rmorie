# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Native counterparts for morie.fn.abndst, morie.fn.aglnvr,
# morie.fn.cbnrt, morie.fn.facea, morie.fn.midor and morie.fn.wnoma.
#
# Two names are deliberately NOT reused. R/sampling.R already has
# morie_effective_sample_size, which is Kish's weights-based quantity;
# the autocorrelation-based one here is a different thing and is called
# morie_ess_autocorrelation. R/wnom.R already has morie_wnominate, which
# EVALUATES the NOMINATE likelihood at given ideal points; the estimator
# here is morie_wnominate_fit. Merging either pair by name would be a
# silent behaviour change.

#' Bracken Bayesian abundance re-estimation
#'
#' A k-mer classifier assigns each read to the lowest node whose k-mers
#' identify it uniquely, so reads from species sharing genome stop at a
#' common ancestor. Reading species counts straight off the classifier
#' therefore understates every species by a DIFFERENT amount, which is a
#' distorted ranking rather than a noisy one.
#'
#' Bracken redistributes them by expectation-maximisation over
#' \eqn{P(j \mid i) \propto P(i \mid j)\theta_j}. What it cannot do is
#' invent information: species with identical k-mer columns leave the
#' likelihood flat along the direction trading one for the other, and
#' `identifiable` reports that rather than returning an arbitrary point
#' on a ridge.
#'
#' @param kraken_output Reads assigned to each classification node.
#' @param kmer_distribution P(node | species); columns sum to 1.
#' @param max_iter,tol Expectation-maximisation controls.
#' @return A list with `estimate`, `fractions`, `reads_reassigned`,
#'   `iterations`, `converged`, `identifiable`, `log_likelihood`.
#' @references Lu J, Breitwieser FP, Thielen P, Salzberg SL (2017)
#'   \emph{PeerJ Comput Sci} 3:e104, \doi{10.7717/peerj-cs.104}.
#' @export
morie_bracken_abundance <- function(kraken_output, kmer_distribution,
                                    max_iter = 1000L, tol = 1e-12) {
  r <- as.numeric(kraken_output)
  P <- as.matrix(kmer_distribution)
  if (nrow(P) != length(r)) {
    if (ncol(P) == length(r)) {
      P <- t(P)
    } else {
      stop(sprintf(
        "kmer_distribution is %d x %d but there are %d nodes.",
        nrow(P), ncol(P), length(r)
      ), call. = FALSE)
    }
  }
  n_sp <- ncol(P)
  if (n_sp < 1L) stop("need at least one species.", call. = FALSE)
  if (any(r < 0)) stop("read counts must be non-negative.", call. = FALSE)
  if (any(P < 0)) stop("k-mer probabilities must be non-negative.", call. = FALSE)
  cs <- colSums(P)
  if (!isTRUE(all.equal(as.numeric(cs), rep(1, n_sp), tolerance = 1e-6))) {
    stop(sprintf(
      paste(
        "each column of kmer_distribution must sum to 1; got",
        "column sums ranging %.6g to %.6g"
      ),
      min(cs), max(cs)
    ), call. = FALSE)
  }
  total <- sum(r)
  if (total <= 0) stop("no reads to distribute.", call. = FALSE)

  reachable <- rowSums(P) > 0
  stranded <- sum(r[!reachable])
  r_use <- ifelse(reachable, r, 0)

  theta <- rep(1 / n_sp, n_sp)
  it <- 0L
  converged <- FALSE
  for (i in seq_len(max_iter)) {
    it <- i
    w <- P * rep(theta, each = nrow(P))
    den <- rowSums(w)
    post <- w / ifelse(den > 0, den, 1)
    counts <- as.vector(crossprod(post, r_use))
    new <- counts / max(sum(counts), 1e-300)
    if (max(abs(new - theta)) < tol) {
      theta <- new
      converged <- TRUE
      break
    }
    theta <- new
  }
  w <- P * rep(theta, each = nrow(P))
  den <- rowSums(w)
  ok <- reachable & den > 0
  loglik <- sum(r[ok] * log(den[ok]))

  usable <- total - stranded
  est <- theta * usable
  uniq <- rowSums(P > 0) == 1
  naive <- rep(0, n_sp)
  for (i in which(uniq & reachable)) {
    naive[which.max(P[i, ])] <- naive[which.max(P[i, ])] + r[i]
  }
  gram <- crossprod(P)
  nrm <- sqrt(pmax(diag(gram), 1e-300))
  cosine <- gram / outer(nrm, nrm)
  diag(cosine) <- 0
  worst <- if (n_sp > 1L) max(cosine) else 0
  identifiable <- worst < 1 - 1e-9

  warns <- character(0)
  if (!converged) {
    warns <- c(warns, sprintf(paste(
      "Expectation-maximisation did not converge in %d iterations, so the",
      "abundances are not a fixed point."
    ), max_iter))
  }
  if (!identifiable) {
    warns <- c(warns, sprintf(paste(
      "Two species have indistinguishable k-mer distributions (cosine",
      "%.6f). The likelihood is flat along the direction trading one for",
      "the other, so the split between them reflects the starting point,",
      "not the data."
    ), worst))
  }
  if (stranded > 0) {
    warns <- c(warns, sprintf(paste(
      "%.0f reads sit at nodes no species in the distribution can produce.",
      "They are excluded from the total."
    ), stranded))
  }
  list(
    estimate = est, fractions = theta, naive_species_reads = naive,
    reads_reassigned = usable - sum(naive), reads_unassignable = stranded,
    total_reads = total, iterations = it, converged = converged,
    log_likelihood = loglik, identifiable = identifiable,
    max_column_cosine = worst, n_nodes = nrow(P), n = n_sp,
    warnings = warns, method = "Bracken Bayesian abundance re-estimation"
  )
}

#' Effective sample size of an autocorrelated sequence
#'
#' Geyer's initial positive sequence: sum autocorrelations in adjacent
#' pairs and stop at the first non-positive pair sum. NOT the same
#' quantity as `morie_effective_sample_size` in R/sampling.R, which is
#' Kish's weights-based design effect; this one is about serial
#' dependence in an ordered stream.
#'
#' @param x Ordered numeric sequence.
#' @param max_lag Optional cap on the lag searched.
#' @return A list with `ess`, `tau_int`, `rho`, `n_lags_used`.
#' @references Geyer CJ (1992) \emph{Statistical Science} 7(4):473-483,
#'   \doi{10.1214/ss/1177011137}.
#' @export
morie_ess_autocorrelation <- function(x, max_lag = NULL) {
  v <- as.numeric(x)
  v <- v[is.finite(v)]
  n <- length(v)
  if (n < 4L) {
    return(list(ess = n, rho = numeric(0), tau_int = 1, n_lags_used = 0L))
  }
  cc <- v - mean(v)
  denom <- sum(cc * cc)
  if (denom <= 0) {
    return(list(ess = n, rho = numeric(0), tau_int = 1, n_lags_used = 0L))
  }
  m <- if (is.null(max_lag)) n %/% 2L else min(as.integer(max_lag), n - 1L)
  rho <- numeric(m + 1L)
  rho[1] <- 1
  for (k in seq_len(m)) {
    rho[k + 1L] <- sum(cc[seq_len(n - k)] * cc[(k + 1L):n]) / denom
  }
  totl <- 0
  used <- 0L
  k <- 1L
  while (k + 1L <= m) {
    pair <- rho[k + 1L] + rho[k + 2L]
    if (pair <= 0) break
    totl <- totl + pair
    used <- k + 1L
    k <- k + 2L
  }
  tau <- max(1 + 2 * totl, 1e-12)
  list(ess = n / tau, rho = rho, tau_int = tau, n_lags_used = used)
}

#' Variance of a training-loss stream, keeping the correlation
#'
#' Losses recorded along a training run are NOT independent draws:
#' consecutive steps share network weights and, in self-play, share the
#' replay buffer the batches come from. The usual \eqn{s/\sqrt{n}}
#' treats them as independent and is therefore too small, often by a
#' large factor, which makes ordinary drift look like an improvement.
#' Dividing by the effective sample size instead gives an inflation of
#' \eqn{\sqrt{\tau_{int}}}, which is 1 on an independent stream.
#'
#' @param losses Total loss at each recorded step, IN ORDER.
#' @param alpha Two-sided level.
#' @return A list with `estimate`, `se`, `se_naive`, `se_inflation`,
#'   `ess`, `tau_int`, `ci_lower`, `ci_upper`, `warnings`.
#' @references Silver D et al (2018) \emph{Science} 362:1140-1144.
#'   Geyer CJ (1992) \emph{Statistical Science} 7(4):473-483.
#' @export
morie_loss_stream_variance <- function(losses, alpha = 0.05) {
  x <- as.numeric(losses)
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 1L) {
    stop("losses must contain at least one finite value.", call. = FALSE)
  }
  if (alpha <= 0 || alpha >= 1) {
    stop(sprintf("alpha must lie in (0, 1); got %s", alpha), call. = FALSE)
  }
  mu <- mean(x)
  vr <- if (n > 1L) stats::var(x) else 0
  se_naive <- if (n > 1L) sqrt(vr / n) else NA_real_
  ac <- morie_ess_autocorrelation(x)
  ess <- max(ac$ess, 1)
  se <- if (n > 1L) sqrt(vr / ess) else NA_real_
  infl <- if (n > 1L && is.finite(se_naive) && se_naive > 0) {
    se / se_naive
  } else {
    NA_real_
  }
  zc <- stats::qnorm(1 - alpha / 2)
  warns <- character(0)
  if (is.finite(infl) && infl > 1.5) {
    warns <- c(warns, sprintf(paste(
      "The loss stream is strongly autocorrelated (integrated time %.1f).",
      "Treating the %d recorded steps as independent would understate the",
      "standard error by a factor of %.2f."
    ), ac$tau_int, n, infl))
  }
  if (ess < 10) {
    warns <- c(warns, sprintf(paste(
      "The effective sample size is only %.1f. The mean is not well",
      "determined by this run however many steps were logged."
    ), ess))
  }
  list(
    estimate = mu, variance = vr, se = se, se_naive = se_naive,
    se_inflation = infl, ess = ess, tau_int = ac$tau_int,
    ci_lower = if (is.finite(se)) mu - zc * se else NA_real_,
    ci_upper = if (is.finite(se)) mu + zc * se else NA_real_,
    ci_naive_lower = if (is.finite(se_naive)) mu - zc * se_naive else NA_real_,
    ci_naive_upper = if (is.finite(se_naive)) mu + zc * se_naive else NA_real_,
    n = n, warnings = warns,
    method = "Autocorrelation-aware variance of a training loss stream"
  )
}

#' Term frequency-inverse document frequency, computed natively
#'
#' Smoothed inverse document frequency
#' \eqn{\log\frac{1+n}{1+df(w)} + 1}, with L2-normalised rows so
#' documents of different lengths are comparable.
#'
#' @param texts Character vector of documents.
#' @param min_df,max_df Document-frequency filters.
#' @return A list with `matrix`, `vocabulary`, `document_frequency`, `idf`.
#' @export
morie_tfidf <- function(texts, min_df = 1, max_df = 1) {
  docs <- tolower(as.character(texts))
  n <- length(docs)
  if (n < 1L) stop("texts must contain at least one document.", call. = FALSE)
  toks <- regmatches(docs, gregexpr("[a-z0-9']+", docs))
  df <- table(unlist(lapply(toks, unique)))
  lo <- if (min_df >= 1) min_df else min_df * n
  hi <- if (max_df <= 1) max_df * n else max_df
  vocab <- sort(names(df)[df >= lo & df <= hi])
  if (!length(vocab)) {
    stop(paste(
      "no terms survived the document-frequency filter; relax",
      "min_df or max_df."
    ), call. = FALSE)
  }
  tf <- matrix(0, n, length(vocab), dimnames = list(NULL, vocab))
  for (i in seq_len(n)) {
    tb <- table(toks[[i]])
    keep <- intersect(names(tb), vocab)
    if (length(keep)) tf[i, keep] <- as.numeric(tb[keep])
  }
  dfv <- colSums(tf > 0)
  idf <- log((1 + n) / (1 + dfv)) + 1
  X <- tf * rep(idf, each = n)
  nrm <- sqrt(rowSums(X^2))
  X <- X / ifelse(nrm > 0, nrm, 1)
  list(matrix = X, vocabulary = vocab, document_frequency = dfv, idf = idf)
}

#' B-spline basis by the Cox-de Boor recursion
#'
#' Equally spaced interior knots with boundary knots repeated
#' `degree + 1` times, so the basis spans the interval and sums to one.
#'
#' @param x Evaluation points.
#' @param n_basis,degree Basis size and spline degree.
#' @return A matrix with one column per basis function.
#' @export
morie_bspline_basis <- function(x, n_basis = 15L, degree = 3L) {
  x <- as.numeric(x)
  n_basis <- as.integer(n_basis)
  degree <- as.integer(degree)
  if (degree < 0L) stop("degree must be non-negative.", call. = FALSE)
  if (n_basis < degree + 1L) {
    stop(sprintf(
      "n_basis must be at least degree + 1 = %d; got %d",
      degree + 1L, n_basis
    ), call. = FALSE)
  }
  a <- min(x)
  b <- max(x)
  if (!(b > a)) stop("the evaluation range is degenerate.", call. = FALSE)
  n_int <- n_basis - degree - 1L
  interior <- seq(a, b, length.out = n_int + 2L)
  interior <- interior[-c(1L, length(interior))]
  knots <- c(rep(a, degree + 1L), interior, rep(b, degree + 1L))
  m <- length(knots) - degree - 1L
  one <- function(i, k) {
    if (k == 0L) {
      out <- as.numeric(x >= knots[i] & x < knots[i + 1L])
      if (knots[i + 1L] >= b) out[x == b] <- 1
      return(out)
    }
    out <- numeric(length(x))
    d1 <- knots[i + k] - knots[i]
    if (d1 > 0) out <- out + (x - knots[i]) / d1 * one(i, k - 1L)
    d2 <- knots[i + k + 1L] - knots[i + 1L]
    if (d2 > 0) out <- out + (knots[i + k + 1L] - x) / d2 * one(i + 1L, k - 1L)
    out
  }
  B <- matrix(0, length(x), m)
  for (i in seq_len(m)) B[, i] <- one(i, degree)
  B
}

#' FACE sandwich-smoothed covariance with FPCA
#'
#' The raw covariance is smoothed by a sandwich \eqn{S \hat C S'}, which
#' keeps the result symmetric by construction and never assembles the
#' two-dimensional problem.
#'
#' THE DIAGONAL MUST BE HELD OUT, and that is the whole method. With
#' white measurement error the raw diagonal sits a constant
#' \eqn{\sigma^2} above the surface while the off-diagonal is unbiased,
#' so smoothing through it drags that ridge outwards and inflates the
#' leading eigenvalue. The diagonal is instead imputed from the smoothed
#' surface and iterated to a fixed point. Filling it with the mean of
#' the rest of the row -- the obvious first choice -- is badly wrong: on
#' a two-component Karhunen-Loeve design with sigma^2 = 0.09 it returned
#' 0.334 and left 13 per cent of the eigenvalue mass negative.
#'
#' @param Y Curves, one row per subject, on a common grid.
#' @param argvals Grid points; defaults to an equally spaced grid.
#' @param n_basis,degree,penalty_order Spline controls.
#' @param lambdas Smoothing parameters to search.
#' @param pve Proportion of variance explained.
#' @return A list with `covariance`, `eigenvalues`, `eigenfunctions`,
#'   `noise_variance`, `npc`, `lambda`, `mean_function`.
#' @references Xiao L, Zipunnikov V, Ruppert D, Crainiceanu C (2016)
#'   \emph{Stat Comput} 26:409-421, \doi{10.1007/s11222-014-9485-x}.
#' @export
morie_face_smooth <- function(Y, argvals = NULL, n_basis = 12L, degree = 3L,
                              lambdas = NULL, pve = 0.99,
                              penalty_order = 2L) {
  M <- as.matrix(Y)
  n <- nrow(M)
  p <- ncol(M)
  if (n < 2L) stop(sprintf("need at least two curves; got %d", n), call. = FALSE)
  if (p < 4L) {
    stop(sprintf("need at least four grid points; got %d", p), call. = FALSE)
  }
  t <- if (is.null(argvals)) seq(0, 1, length.out = p) else as.numeric(argvals)
  if (length(t) != p) {
    stop(sprintf(
      "argvals has length %d but Y has %d columns.",
      length(t), p
    ), call. = FALSE)
  }
  if (pve <= 0 || pve > 1) {
    stop(sprintf("pve must lie in (0, 1]; got %s", pve), call. = FALSE)
  }
  obs <- is.finite(M)
  cnt <- colSums(obs)
  if (any(cnt < 2L)) {
    stop("every grid point needs at least two observed curves.", call. = FALSE)
  }
  mu <- colSums(ifelse(obs, M, 0)) / cnt
  Z <- ifelse(obs, M - rep(mu, each = n), 0)
  pair <- crossprod(obs * 1)
  raw <- crossprod(Z) / pmax(pair, 1)

  B <- morie_bspline_basis(t, n_basis = n_basis, degree = degree)
  D <- diag(ncol(B))
  for (i in seq_len(penalty_order)) D <- diff(D)
  P <- crossprod(D)
  off <- !diag(TRUE, p)
  if (is.null(lambdas)) lambdas <- 10^seq(-6, 4, length.out = 21L)

  best_gcv <- Inf
  lam <- lambdas[1]
  S <- NULL
  for (L in lambdas) {
    A <- crossprod(B) + L * P
    Smat <- B %*% tryCatch(solve(A, t(B)),
      error = function(e) .morie_ginv(A) %*% t(B)
    )
    fit <- Smat %*% raw %*% t(Smat)
    resid <- (raw - fit)[off]
    denom <- (1 - sum(diag(Smat)) / p)^2
    gcv <- sum(resid^2) / max(denom, 1e-12)
    if (gcv < best_gcv) {
      best_gcv <- gcv
      lam <- L
      S <- Smat
    }
  }

  idx <- cbind(seq_len(p), seq_len(p))
  filled <- raw
  nb <- numeric(p)
  for (i in seq_len(p)) {
    j <- c(i - 1L, i + 1L)
    j <- j[j >= 1L & j <= p]
    nb[i] <- mean(raw[i, j])
  }
  filled[idx] <- nb
  C <- S %*% filled %*% t(S)
  C <- (C + t(C)) / 2
  for (i in seq_len(10L)) {
    filled[idx] <- diag(C)
    Cn <- S %*% filled %*% t(S)
    Cn <- (Cn + t(Cn)) / 2
    if (max(abs(diag(Cn) - diag(C))) < 1e-12) {
      C <- Cn
      break
    }
    C <- Cn
  }
  klo <- max(as.integer(0.1 * p), 1L)
  khi <- as.integer(0.9 * p)
  if (khi <= klo) {
    klo <- 1L
    khi <- p
  }
  sigma2 <- mean(pmax((diag(raw) - diag(C))[klo:khi], 0))

  ev <- eigen(C, symmetric = TRUE)
  vals <- ev$values
  vecs <- ev$vectors
  neg <- sum(abs(vals[vals < 0]))
  vals <- pmax(vals, 0)
  totalv <- sum(vals)
  cum <- if (totalv > 0) cumsum(vals) / totalv else rep(0, length(vals))
  npc <- if (totalv > 0) max(1L, min(which(cum >= pve), p)) else 0L
  dt <- if (p > 1L) mean(diff(t)) else 1
  phi <- vecs / sqrt(max(dt, 1e-300))

  warns <- character(0)
  if (neg > 1e-8 * max(totalv, 1e-300)) {
    warns <- c(warns, sprintf(
      paste(
        "The smoothed covariance had negative eigenvalues carrying %.4g of",
        "mass, truncated to zero. A covariance operator cannot have any."
      ),
      neg
    ))
  }
  list(
    covariance = C, raw_covariance = raw, eigenvalues = vals,
    eigenfunctions = phi, mean_function = mu, noise_variance = sigma2,
    negative_eigenvalue_mass = neg, npc = npc, pve_cumulative = cum,
    total_variance = totalv, lambda = lam, gcv = best_gcv,
    n_basis = ncol(B), argvals = t, n = n, warnings = warns,
    method = "FACE sandwich-smoothed covariance with FPCA"
  )
}

#' .morie_descendants
#'
#' Part of the abundance_text_voting_native implementation; see the file
#' header for the source it follows.
#'
#' @param A A matrix; indexed by row and column.
#' @param start See Usage.
#' @return The value of \code{seen}, as built in the body.
#' @export
.morie_descendants <- function(A, start) {
  seen <- integer(0)
  stack <- start
  while (length(stack)) {
    u <- stack[1]
    stack <- stack[-1]
    ch <- which(A[u, ])
    new <- setdiff(ch, seen)
    seen <- c(seen, new)
    stack <- c(stack, new)
  }
  seen
}

#' .morie_reachable
#'
#' Part of the abundance_text_voting_native implementation; see the file
#' header for the source it follows.
#'
#' @param A A matrix; indexed by row and column.
#' @param x Carried through into a list the body builds.
#' @param Z See Usage.
#' @return The value of \code{setdiff}.
#' @export
.morie_reachable <- function(A, x, Z) {
  n <- nrow(A)
  anc <- integer(0)
  stack <- Z
  while (length(stack)) {
    u <- stack[1]
    stack <- stack[-1]
    if (u %in% anc) next
    anc <- c(anc, u)
    stack <- c(stack, which(A[, u]))
  }
  seen <- character(0)
  reach <- integer(0)
  frontier <- list(list(u = x, d = "up"))
  while (length(frontier)) {
    st <- frontier[[1]]
    frontier <- frontier[-1]
    key <- paste(st$u, st$d)
    if (key %in% seen) next
    seen <- c(seen, key)
    if (!(st$u %in% Z)) reach <- union(reach, st$u)
    if (st$d == "up" && !(st$u %in% Z)) {
      for (pa in which(A[, st$u])) frontier <- c(frontier, list(list(u = pa, d = "up")))
      for (ch in which(A[st$u, ])) frontier <- c(frontier, list(list(u = ch, d = "down")))
    } else if (st$d == "down") {
      if (!(st$u %in% Z)) {
        for (ch in which(A[st$u, ])) frontier <- c(frontier, list(list(u = ch, d = "down")))
      }
      if (st$u %in% anc) {
        for (pa in which(A[, st$u])) frontier <- c(frontier, list(list(u = pa, d = "up")))
      }
    }
  }
  setdiff(reach, x)
}

#' Does Z satisfy Pearl's back-door criterion?
#'
#' Two conditions: no member of `Z` descends from the treatment, and `Z`
#' blocks every path from treatment to outcome starting with an arrow
#' INTO the treatment. The second is checked by d-separation in the
#' graph with arrows out of the treatment deleted, using Bayes-ball
#' reachability -- conditioning blocks chains and forks but OPENS
#' colliders, and that asymmetry is the whole algorithm.
#'
#' @param adj Logical adjacency matrix; `adj\[i, j\]` means i -> j.
#' @param treatment,outcome Node indices (1-based).
#' @param Z Candidate adjustment set.
#' @return TRUE or FALSE.
#' @references Pearl J (2009) \emph{Causality}, 2nd ed., Sec 3.3.
#' @export
morie_is_backdoor_admissible <- function(adj, treatment, outcome, Z) {
  A <- matrix(as.logical(adj), nrow(adj), ncol(adj))
  t <- as.integer(treatment)
  y <- as.integer(outcome)
  Z <- as.integer(Z)
  if (t %in% Z || y %in% Z) {
    return(FALSE)
  }
  desc <- .morie_descendants(A, t)
  if (length(intersect(Z, desc))) {
    return(FALSE)
  }
  B <- A
  B[t, ] <- FALSE
  !(y %in% .morie_reachable(B, t, Z))
}

#' Admissible back-door adjustment sets, smallest first
#'
#' The empty set is checked too: if it is admissible there is no
#' confounding to adjust for, and adjusting anyway costs precision.
#'
#' @param adj Logical adjacency matrix.
#' @param treatment,outcome Node indices (1-based).
#' @param max_size Optional cap on set size.
#' @return A list of integer vectors.
#' @export
morie_backdoor_sets <- function(adj, treatment, outcome, max_size = NULL) {
  A <- matrix(as.logical(adj), nrow(adj), ncol(adj))
  n <- nrow(A)
  t <- as.integer(treatment)
  y <- as.integer(outcome)
  desc <- .morie_descendants(A, t)
  pool <- setdiff(seq_len(n), c(t, y, desc))
  lim <- if (is.null(max_size)) length(pool) else as.integer(max_size)
  found <- list()
  for (k in 0:min(lim, length(pool))) {
    if (k == 0L) {
      if (morie_is_backdoor_admissible(A, t, y, integer(0))) {
        found <- c(found, list(integer(0)))
      }
    } else {
      cmb <- utils::combn(pool, k)
      for (c in seq_len(ncol(cmb))) {
        if (morie_is_backdoor_admissible(A, t, y, cmb[, c])) {
          found <- c(found, list(sort(as.integer(cmb[, c]))))
        }
      }
    }
    if (length(found) && k >= 1L) break
  }
  found
}

#' Model, identify, estimate, refute
#'
#' The value is not the number at step three. It is that identification
#' says WHICH quantity the data can speak to, estimation produces it,
#' and refutation asks whether it survives perturbations that ought not
#' to change it: a placebo treatment (should collapse to zero), a random
#' common cause (should not move it) and random subsets (spread should
#' match the standard error).
#'
#' Adjusting for everything measured is not conservative. Conditioning
#' on a mediator removes part of the effect being measured, and
#' conditioning on a collider CREATES an association where none existed.
#' Both are flagged and the estimate still returned, so the damage is
#' visible rather than merely warned about.
#'
#' @param dag Logical adjacency matrix; `dag\[i, j\]` means i -> j.
#' @param data Observations, one column per node.
#' @param treatment,outcome Column indices (1-based).
#' @param adjustment Force a particular set instead of identifying one.
#' @param n_refute Replications per refutation.
#' @param seed Random seed.
#' @param alpha Two-sided level.
#' @return A list with `estimate`, `se`, `identified`, `adjustment_set`,
#'   `placebo_effect`, `random_cause_effect`, `subset_sd`,
#'   `refutations_passed`, `warnings`.
#' @references Pearl J (2009) \emph{Causality}, 2nd ed., Sec 3.3.
#'   Sharma A, Kiciman E (2020) arXiv:2011.04216.
#' @export
morie_identify_estimate_refute <- function(dag, data, treatment, outcome,
                                           adjustment = NULL, n_refute = 100L,
                                           seed = 0L, alpha = 0.05) {
  A <- matrix(as.logical(dag), nrow(dag), ncol(dag))
  D <- as.matrix(data)
  p <- nrow(A)
  if (ncol(A) != p) stop("dag must be square.", call. = FALSE)
  if (ncol(D) != p) {
    if (nrow(D) == p) {
      D <- t(D)
    } else {
      stop(sprintf(
        "data has %d columns but the dag has %d nodes.",
        ncol(D), p
      ), call. = FALSE)
    }
  }
  n <- nrow(D)
  t <- as.integer(treatment)
  y <- as.integer(outcome)
  if (t == y) stop("treatment and outcome must differ.", call. = FALSE)
  if (any(diag(A))) stop("the dag has a self-loop.", call. = FALSE)
  if (n < p + 3L) {
    stop(sprintf("need more rows than nodes; got %d rows, %d nodes.", n, p),
      call. = FALSE
    )
  }

  sets <- morie_backdoor_sets(A, t, y)
  if (!is.null(adjustment)) {
    Z <- sort(as.integer(adjustment))
    identified <- morie_is_backdoor_admissible(A, t, y, Z)
  } else if (length(sets)) {
    Z <- sets[[1]]
    identified <- TRUE
  } else {
    Z <- integer(0)
    identified <- FALSE
  }
  desc_t <- .morie_descendants(A, t)
  mediators <- intersect(Z, desc_t)
  colliders <- Z[vapply(Z, function(z) sum(A[, z]) >= 2L, logical(1))]

  fit <- function(dat, tcol, zc) {
    X <- cbind(1, dat[, tcol])
    if (length(zc)) X <- cbind(X, dat[, zc, drop = FALSE])
    b <- qr.coef(qr(X), dat[, y])
    b[is.na(b)] <- 0
    resid <- dat[, y] - as.vector(X %*% b)
    dof <- max(nrow(dat) - ncol(X), 1L)
    s2 <- sum(resid^2) / dof
    XtXi <- .morie_ginv(crossprod(X))
    unname(c(b[2], sqrt(max(s2 * XtXi[2, 2], 0))))
  }
  ef <- fit(D, t, Z)
  eff <- ef[1]
  se <- ef[2]

  set.seed(seed)
  placebo <- numeric(n_refute)
  common <- numeric(n_refute)
  subset <- numeric(n_refute)
  for (i in seq_len(n_refute)) {
    Dp <- D
    Dp[, t] <- sample(D[, t])
    placebo[i] <- fit(Dp, t, Z)[1]
    Dc <- cbind(D, stats::rnorm(n))
    common[i] <- fit(Dc, t, c(Z, p + 1L))[1]
    idx <- sample.int(n, max(as.integer(0.8 * n), p + 3L))
    subset[i] <- fit(D[idx, , drop = FALSE], t, Z)[1]
  }
  pm <- mean(placebo)
  psd <- stats::sd(placebo)
  cm <- mean(common)
  ssd <- stats::sd(subset)
  pass_p <- abs(pm) < 2 * max(psd, 1e-12)
  pass_c <- abs(cm - eff) < max(0.1 * abs(eff), 2 * se)
  pass_s <- ssd < 3 * max(se, 1e-12)
  passed <- sum(pass_p, pass_c, pass_s)

  warns <- character(0)
  if (!identified) {
    warns <- c(warns, paste(
      "The back-door criterion is not satisfied by any subset of the",
      "measured variables. Adjusting anyway does not make the estimate",
      "causal; it makes it a different biased number."
    ))
  }
  if (length(mediators)) {
    warns <- c(warns, sprintf(
      paste(
        "Node(s) %s in the adjustment set are descendants of the treatment.",
        "Conditioning on a mediator removes part of the effect being",
        "measured, so this estimate is attenuated by construction."
      ),
      paste(mediators, collapse = ", ")
    ))
  }
  if (length(colliders)) {
    warns <- c(warns, sprintf(
      paste(
        "Node(s) %s in the adjustment set are colliders. Conditioning on a",
        "collider CREATES an association between its parents where none",
        "existed, so adjusting here adds bias rather than removing it."
      ),
      paste(colliders, collapse = ", ")
    ))
  }
  zc <- stats::qnorm(1 - alpha / 2)
  list(
    estimate = eff, se = se, ci_lower = eff - zc * se,
    ci_upper = eff + zc * se, identified = identified,
    adjustment_set = Z, all_backdoor_sets = sets,
    n_backdoor_sets = length(sets),
    adjusted_for_mediator = mediators, adjusted_for_collider = colliders,
    placebo_effect = pm, placebo_sd = psd, random_cause_effect = cm,
    subset_sd = ssd, passed_placebo = pass_p,
    passed_random_cause = pass_c, passed_subset = pass_s,
    refutations_passed = passed, n = n, warnings = warns,
    method = "Model-identify-estimate-refute causal workflow"
  )
}

#' Treatment effect adjusting for what the text reveals
#'
#' The confounder is written down but not coded -- clinical notes, case
#' files, judicial reasons. The text becomes a low-dimensional
#' representation, that joins the adjustment set, and the estimator is
#' ordinary augmented inverse-probability weighting.
#'
#' This identifies the effect only if the representation captures ALL
#' the confounding the text carries, which is not testable from the
#' data. A bag of words discards order and negation: "history of
#' psychosis" and "no history of psychosis" have cosine 0.78 under this
#' encoding and exactly 1.0 once min_df drops "no". Passing `embedding`
#' is the supported route to a stronger representation and deliberately
#' the only one -- the function consumes a numeric matrix and takes no
#' model-runtime dependency, so it stays native and offline.
#'
#' `naive_difference` is returned so the movement from the unadjusted
#' contrast is visible: an adjustment that moves nothing has either
#' found no confounding or failed to represent it, and those two look
#' identical from outside.
#'
#' @param texts Character vector, one document per unit; may be NULL
#'   when `embedding` is supplied.
#' @param T Binary treatment.
#' @param Y Outcome.
#' @param X Optional extra numeric covariates.
#' @param n_components Dimension of the text representation.
#' @param trim Propensity clipping bound.
#' @param alpha Two-sided level.
#' @param embedding Optional precomputed representation.
#' @return A list with `estimate`, `se`, `naive_difference`,
#'   `adjustment_movement`, `propensity`, `max_weight_share`, `warnings`.
#' @references Veitch V, Sridhar D, Blei DM (2020) arXiv:1905.12741.
#' @export
morie_text_ate <- function(texts, T, Y, X = NULL, n_components = 10L,
                           trim = 0.02, alpha = 0.05, embedding = NULL) {
  tv <- as.numeric(T)
  yv <- as.numeric(Y)
  n <- length(tv)
  if (length(yv) != n) {
    stop(sprintf("Y has length %d but T has %d.", length(yv), n), call. = FALSE)
  }
  if (!all(tv %in% c(0, 1))) stop("T must be binary 0/1.", call. = FALSE)
  if (trim < 0 || trim >= 0.5) {
    stop(sprintf("trim must lie in [0, 0.5); got %s", trim), call. = FALSE)
  }
  if (n < 10L) stop(sprintf("need at least 10 units; got %d", n), call. = FALSE)

  if (!is.null(embedding)) {
    E <- as.matrix(embedding)
    if (nrow(E) != n) E <- t(E)
    if (nrow(E) != n) {
      stop(sprintf(
        "embedding has %d rows but there are %d units.",
        nrow(E), n
      ), call. = FALSE)
    }
  } else {
    if (length(texts) != n) {
      stop(sprintf(
        "texts has %d documents but there are %d units.",
        length(texts), n
      ), call. = FALSE)
    }
    tf <- morie_tfidf(texts)$matrix
    k <- min(as.integer(n_components), min(dim(tf)))
    Xc <- scale(tf, center = TRUE, scale = FALSE)
    sv <- svd(Xc)
    E <- sv$u[, seq_len(k), drop = FALSE] *
      rep(sv$d[seq_len(k)], each = n)
  }
  W <- E
  if (!is.null(X)) {
    Xa <- as.matrix(X)
    if (nrow(Xa) != n) Xa <- t(Xa)
    W <- cbind(W, Xa)
  }
  Wd <- cbind(1, W)
  gf <- .morie_bounds_logit(Wd, tv)
  e_raw <- gf$fitted
  e <- pmin(pmax(e_raw, trim), 1 - trim)
  n_trim <- sum(e_raw < trim | e_raw > 1 - trim)

  m1 <- tv == 1
  m0 <- tv == 0
  if (sum(m1) < ncol(Wd) + 1L || sum(m0) < ncol(Wd) + 1L) {
    stop(sprintf(
      paste(
        "an arm has too few units (%d treated, %d control)",
        "for %d design columns. Reduce n_components."
      ),
      sum(m1), sum(m0), ncol(Wd)
    ), call. = FALSE)
  }
  b1 <- qr.coef(qr(Wd[m1, , drop = FALSE]), yv[m1])
  b1[is.na(b1)] <- 0
  b0 <- qr.coef(qr(Wd[m0, , drop = FALSE]), yv[m0])
  b0[is.na(b0)] <- 0
  mu1 <- as.vector(Wd %*% b1)
  mu0 <- as.vector(Wd %*% b0)
  psi <- mu1 - mu0 + tv * (yv - mu1) / e - (1 - tv) * (yv - mu0) / (1 - e)
  ate <- mean(psi)
  se <- sqrt(mean((psi - ate)^2) / n)
  naive <- mean(yv[m1]) - mean(yv[m0])
  wts <- ifelse(tv == 1, 1 / e, 1 / (1 - e))
  share <- max(wts) / sum(wts)

  warns <- paste(
    "The adjustment set is a bag-of-words representation,",
    "which cannot encode negation or word order. Confounding",
    "carried by those is not removed, and the estimate will",
    "look adjusted regardless."
  )
  if (isTRUE(gf$separated)) {
    warns <- c(warns, paste(
      "The propensity model separates the data: the",
      "text predicts treatment almost perfectly."
    ))
  }
  if (share > 0.05) {
    warns <- c(warns, sprintf(
      paste(
        "A single unit carries %.1f%% of the",
        "total weight. Reduce n_components."
      ),
      100 * share
    ))
  }
  zc <- stats::qnorm(1 - alpha / 2)
  list(
    estimate = ate, se = se, ci_lower = ate - zc * se,
    ci_upper = ate + zc * se, naive_difference = naive,
    adjustment_movement = ate - naive, propensity = e,
    propensity_untrimmed = e_raw, n_trimmed = n_trim,
    max_weight_share = share, embedding = E, n_components = ncol(E),
    n = n, warnings = warns,
    method = "Treatment effect with a text-derived adjustment set"
  )
}

#' W-NOMINATE ideal points by alternating optimisation
#'
#' ESTIMATES legislator ideal points and roll-call parameters. This is
#' not `morie_wnominate` in R/wnom.R, which EVALUATES the NOMINATE
#' likelihood at ideal points already in hand.
#'
#' Reparameterised so each half-step is an ordinary probit: with
#' \eqn{a_j = \beta(|z^N_j|^2 - |z^Y_j|^2)} and
#' \eqn{w_j = 2\beta(z^Y_j - z^N_j)} the linear predictor is exactly
#' \eqn{\eta_{ij} = a_j + x_i \cdot w_j}.
#'
#' Three things are structural rather than numerical. The configuration
#' is identified only up to rotation, reflection and scale, so raw
#' coordinates are not comparable across fits and `polarity` fixes the
#' sign. Unanimous roll calls separate nobody and are dropped.
#' Correct classification must be read against the modal baseline, so
#' `aggregate_pre` is the number worth quoting.
#'
#' The objective is NOT jointly concave, so the start matters: from a
#' random configuration 60 sweeps reached a log-likelihood of -19801
#' with an ideal-point correlation of 0.47 against the truth, while
#' seeding from the leading singular vectors reached -6913 and 0.990 in
#' 26 sweeps.
#'
#' @param votes Matrix, 1 yea / 0 nay / NA absent.
#' @param n_dims Dimensions of the policy space.
#' @param polarity Index of a legislator fixed to positive on dim 1.
#' @param max_iter,tol Alternating-optimisation controls.
#' @param ridge Small penalty for near-separating roll calls.
#' @return A list with `ideal_points`, `rollcall_normals`,
#'   `correct_classification`, `modal_baseline`, `aggregate_pre`,
#'   `log_likelihood`, `converged`, `warnings`.
#' @references Poole KT, Rosenthal H (1985) \emph{AJPS} 29(2):357-384,
#'   \doi{10.2307/2111172}.
#' @export
morie_wnominate_fit <- function(votes, n_dims = 1L, polarity = NULL,
                                max_iter = 250L, tol = 1e-7, ridge = 1e-3) {
  V <- as.matrix(votes)
  n_dims <- as.integer(n_dims)
  if (n_dims < 1L) {
    stop(sprintf("n_dims must be at least 1; got %d", n_dims), call. = FALSE)
  }
  vals <- V[is.finite(V)]
  if (!length(vals)) stop("votes contains no observed entries.", call. = FALSE)
  if (!all(vals %in% c(0, 1))) {
    stop("votes must be 1 (yea), 0 (nay) or NA (absent).", call. = FALSE)
  }
  keep <- integer(0)
  for (j in seq_len(ncol(V))) {
    col <- V[is.finite(V[, j]), j]
    if (length(col) >= 2L && sum(col) > 0 && sum(col) < length(col)) {
      keep <- c(keep, j)
    }
  }
  n_dropped <- ncol(V) - length(keep)
  if (length(keep) < n_dims + 1L) {
    stop(sprintf(
      paste(
        "only %d roll calls divide the chamber, which cannot",
        "identify a %d-dimensional space."
      ),
      length(keep), n_dims
    ), call. = FALSE)
  }
  V <- V[, keep, drop = FALSE]
  obs <- is.finite(V)
  Y <- ifelse(obs, V, 0)
  n <- nrow(V)
  m <- ncol(V)
  if (n < n_dims + 2L) {
    stop(sprintf("need at least %d legislators; got %d", n_dims + 2L, n),
      call. = FALSE
    )
  }

  Cm <- ifelse(obs, Y - 0.5, 0)
  sv <- svd(Cm)
  k <- min(n_dims, ncol(sv$u))
  x <- matrix(0, n, n_dims)
  w <- matrix(0, m, n_dims)
  x[, seq_len(k)] <- sv$u[, seq_len(k), drop = FALSE]
  w[, seq_len(k)] <- sv$v[, seq_len(k), drop = FALSE] *
    rep(sv$d[seq_len(k)], each = m)
  sdv <- apply(x[, seq_len(k), drop = FALSE], 2, stats::sd)
  sdv[sdv <= 0] <- 1
  x[, seq_len(k)] <- x[, seq_len(k), drop = FALSE] / rep(sdv, each = n)
  w[, seq_len(k)] <- w[, seq_len(k), drop = FALSE] * rep(sdv, each = m)
  a <- numeric(m)

  pfit <- function(design, yr, start, offset = NULL) {
    b <- start
    off <- if (is.null(offset)) numeric(length(yr)) else offset
    for (i in seq_len(6L)) {
      eta <- pmin(pmax(as.vector(design %*% b) + off, -8), 8)
      pr <- pmin(pmax(stats::pnorm(eta), 1e-9), 1 - 1e-9)
      ph <- pmax(stats::dnorm(eta), 1e-10)
      wt <- ph^2 / (pr * (1 - pr))
      z <- eta - off + (yr - pr) / ph
      A <- (t(design) * rep(wt, each = ncol(design))) %*% design +
        ridge * diag(ncol(design))
      g <- (t(design) * rep(wt, each = ncol(design))) %*% z
      nb <- tryCatch(as.vector(solve(A, g)),
        error = function(e) as.vector(.morie_ginv(A) %*% g)
      )
      if (!all(is.finite(nb))) break
      step <- nb - b
      nrm <- max(abs(step))
      if (nrm > 5) nb <- b + step * (5 / nrm)
      b <- nb
    }
    b
  }
  loglik <- function(eta) {
    p <- pmin(pmax(stats::pnorm(pmin(pmax(eta, -8), 8)), 1e-9), 1 - 1e-9)
    sum(obs * (Y * log(p) + (1 - Y) * log(1 - p)))
  }

  ll_old <- -Inf
  delta <- Inf
  it <- 0L
  converged <- FALSE
  for (i in seq_len(max_iter)) {
    it <- i
    for (li in seq_len(n)) {
      oi <- obs[li, ]
      if (sum(oi) < n_dims + 1L) next
      x[li, ] <- pfit(w[oi, , drop = FALSE], Y[li, oi], x[li, ], a[oi])
    }
    for (j in seq_len(m)) {
      oj <- obs[, j]
      if (sum(oj) < n_dims + 2L) next
      Dm <- cbind(1, x[oj, , drop = FALSE])
      bj <- pfit(Dm, Y[oj, j], c(a[j], w[j, ]))
      a[j] <- bj[1]
      w[j, ] <- bj[-1]
    }
    ll <- loglik(matrix(a, n, m, byrow = TRUE) + x %*% t(w))
    delta <- abs(ll - ll_old)
    if (delta < tol * (1 + abs(ll_old))) {
      ll_old <- ll
      converged <- TRUE
      break
    }
    ll_old <- ll
  }

  centre <- colMeans(x)
  a <- a + as.vector(centre %*% t(w))
  x <- x - rep(centre, each = n)
  rms <- sqrt(mean(rowSums(x^2)))
  if (rms > 0) {
    x <- x / rms
    w <- w * rms
  }
  flipped <- FALSE
  if (!is.null(polarity)) {
    kk <- as.integer(polarity)
    if (kk < 1L || kk > n) {
      stop(sprintf("polarity must lie in 1 .. %d; got %d", n, kk), call. = FALSE)
    }
    if (x[kk, 1] < 0) {
      x[, 1] <- -x[, 1]
      w[, 1] <- -w[, 1]
      flipped <- TRUE
    }
  }
  eta <- matrix(a, n, m, byrow = TRUE) + x %*% t(w)
  pred <- (eta > 0) * 1
  correct <- sum(obs * (pred == Y)) / max(sum(obs), 1)
  modal_err <- 0
  for (j in seq_len(m)) {
    col <- Y[obs[, j], j]
    modal_err <- modal_err + min(sum(col), length(col) - sum(col))
  }
  totalo <- sum(obs)
  modal <- 1 - modal_err / max(totalo, 1)
  errors <- sum(obs * (pred != Y))
  pre <- if (modal_err > 0) (modal_err - errors) / modal_err else NA_real_

  warns <- character(0)
  if (!converged) {
    warns <- c(warns, sprintf(paste(
      "Alternating optimisation stopped at %d iterations with the",
      "log-likelihood still moving by %.4g per sweep. The objective is not",
      "jointly concave, so a stopped fit is not necessarily near an",
      "optimum."
    ), max_iter, delta))
  }
  if (is.null(polarity)) {
    warns <- c(warns, paste(
      "No polarity was fixed. The configuration is identified only up to",
      "rotation, reflection and scale, so the sign of every dimension is",
      "arbitrary and will differ between starts."
    ))
  }
  if (n_dropped > 0L) {
    warns <- c(warns, sprintf(paste(
      "%d roll calls were unanimous and carry no information about",
      "position. They are excluded; including them would inflate correct",
      "classification without improving the fit."
    ), n_dropped))
  }
  list(
    ideal_points = x, rollcall_normals = w, rollcall_intercepts = a,
    log_likelihood = ll_old, log_likelihood_change = delta,
    correct_classification = correct, modal_baseline = modal,
    aggregate_pre = pre, iterations = it, converged = converged,
    n_dropped_rollcalls = n_dropped, rollcalls_kept = keep,
    polarity_flipped = flipped, n_dims = n_dims, n = n, warnings = warns,
    method = "W-NOMINATE alternating ideal-point estimation"
  )
}
