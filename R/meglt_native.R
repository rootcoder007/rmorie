# Exact matrix completion by nuclear norm minimisation.
# Sources: Candes, E. J. & Recht, B. (2009) "Exact Matrix Completion
# via Convex Optimization", *Foundations of Computational Mathematics*
# 9(6), 717-772, doi:10.1007/s10208-009-9045-5, arXiv:0805.4471. The
# abstract and Sec. 1 (the sampling bound m >= C n^1.2 r log n, that
# the 1.25 exponent covers all ranks, the nuclear norm of eq. (1.4)
# as the sum of singular values and its use in place of the rank, the
# connection to compressed sensing, and the incoherence conditions --
# with the motivating example of a matrix whose singular vectors are
# extremely sparse, for which sampling reveals nothing).
#
# Cai, J.-F., Candes, E. J. & Shen, Z. (2010) "A Singular Value
# Thresholding Algorithm for Matrix Completion", *SIAM Journal on
# Optimization* 20(4), 1956-1982, doi:10.1137/080738970,
# arXiv:0810.3286. The iterative algorithm implemented here.
#
# Fazel, M. (2002) *Matrix Rank Minimization with Applications*, PhD
# thesis, Stanford University. The nuclear norm as the convex envelope
# of the rank.

.MEGLT_EPS <- 1e-12

.meglt_mat <- function(X) {
  if (is.matrix(X)) {
    out <- vector("list", nrow(X))
    for (i in seq_len(nrow(X))) out[[i]] <- as.numeric(X[i, ])
    out
  } else {
    lapply(X, function(r) as.numeric(unlist(r)))
  }
}

.meglt_svd <- function(M) {
  s <- svd(M, nu = nrow(M), nv = ncol(M))
  list(U = s$u, s = s$d, Vt = s$v)
}

nuclear_norm <- function(A) {
  M <- .meglt_mat(A)
  s <- .meglt_svd(M)
  sum(s$s)
}

coherence <- function(A, rank = NULL) {
  M <- .meglt_mat(A)
  sv <- .meglt_svd(M)
  s <- sv$s; U <- sv$U; Vt <- sv$Vt
  tol <- max(length(M), length(M[[1L]])) * (if (length(s) > 0L) s[1L] else 0) * 1e-12
  r <- if (is.null(rank)) sum(s > tol) else as.integer(rank)
  if (r < 1L)
    stop("meglt: the matrix is numerically zero")
  n1 <- length(M); n2 <- length(M[[1L]])
  mu_u <- 0
  for (i in seq_len(n1)) {
    ss <- 0
    for (j in seq_len(r)) ss <- ss + U[i, j]^2
    if (ss > mu_u) mu_u <- ss
  }
  mu_v <- 0
  for (i in seq_len(n2)) {
    ss <- 0
    for (j in seq_len(r)) ss <- ss + Vt[j, i]^2
    if (ss > mu_v) mu_v <- ss
  }
  list(mu_row = n1 * mu_u / r, mu_col = n2 * mu_v / r,
       mu = max(n1 * mu_u / r, n2 * mu_v / r), rank = r,
       note = "large mu means concentrated singular vectors, and then sampling reveals nothing")
}

sample_bound <- function(n, r, C = 1.0, exponent = 1.2) {
  if (!(exponent %in% c(1.2, 1.25)))
    stop("meglt: the exponent must be 1.2 (moderate rank) or 1.25 (all ranks), got ", format(exponent))
  nn <- as.integer(n); rr <- as.integer(r)
  if (nn < 2L || rr < 1L)
    stop("meglt: need n >= 2 and r >= 1")
  m <- as.numeric(C) * (nn^as.numeric(exponent)) * rr * log(nn)
  list(m = m, fraction = m / as.numeric(nn * nn), n = nn, r = rr,
       exponent = as.numeric(exponent),
       note = "the 1.25 exponent holds for ALL ranks; 1.2 assumes the rank is not too large")
}

svt <- function(M, observed, tau = NULL, step = 1.9, iters = 200L,
                tol = 1e-6) {
  A <- .meglt_mat(M)
  n1 <- length(A); n2 <- length(A[[1L]])
  obs <- unique(matrix(c(as.integer(sapply(observed, `[`, 1L)),
                        as.integer(sapply(observed, `[`, 2L))),
                      ncol = 2L))
  if (nrow(obs) == 0L)
    stop("meglt: no entries were observed")
  t <- if (is.null(tau)) 5.0 * sqrt(n1 * n2) else as.numeric(tau)
  Y <- matrix(0, n1, n2)
  X <- matrix(0, n1, n2)
  hist <- numeric(0)
  for (it in seq_len(as.integer(iters))) {
    Amat <- do.call(rbind, A)
    sv <- .meglt_svd(Y)
    U <- sv$U; s <- sv$s; Vt <- sv$Vt
    sh <- pmax(0, s - t)
    X <- U %*% diag(sh) %*% Vt
    res <- 0
    for (r in seq_len(nrow(obs))) {
      i <- obs[r, 1L]; j <- obs[r, 2L]
      d <- A[[i + 1L]][j + 1L] - X[i + 1L, j + 1L]
      res <- res + d^2
      Y[i + 1L, j + 1L] <- Y[i + 1L, j + 1L] + as.numeric(step) * d
    }
    hist <- c(hist, sqrt(res))
    if (hist[length(hist)] < as.numeric(tol)) break
  }
  list(estimate = X, X = X, residual_history = hist,
       final_residual = hist[length(hist)], tau = t,
       n_observed = nrow(obs),
       fraction_observed = nrow(obs) / as.numeric(n1 * n2),
       nuclear_norm = nuclear_norm(X),
       method = "singular value thresholding for the nuclear-norm program; Candes & Recht (2009), Cai, Candes & Shen (2010)")
}

relative_error <- function(X, M) {
  A <- .meglt_mat(M)
  num <- 0; den <- 0
  for (i in seq_along(A)) for (j in seq_along(A[[1L]])) {
    d <- X[[i]][j] - A[[i]][j]
    num <- num + d^2
    den <- den + A[[i]][j]^2
  }
  num <- sqrt(num); den <- sqrt(den)
  if (den <= .MEGLT_EPS)
    stop("meglt: the reference matrix is zero")
  num / den
}

matrixcompletion <- svt
matrix_completion_low_rank <- svt

cheatsheet <- function() {
  paste("meglt: most low-rank matrices are recovered EXACTLY from ",
        "m >= C n^1.2 r log n sampled entries -- 1.25 covers all ",
        "ranks. Rank minimisation is NP-hard, so minimise the ",
        "NUCLEAR NORM (sum of singular values), the rank's convex ",
        "surrogate as l1 is for sparsity. INCOHERENCE is required, ",
        "not decorative: e_1 e_1' is rank 1 and unrecoverable ",
        "because nearly every sampled entry is zero. Solved by ",
        "singular value thresholding.", sep = "")
}

morie_meglt <- function(op, ...) {
  if (missing(op) || length(op) != 1L)
    stop("meglt: op must be one of nuclear_norm, coherence, sample_bound, svt, relative_error, cheatsheet")
  op <- as.character(op)
  switch(op,
    "nuclear_norm" = list(nuclear_norm = nuclear_norm(...)),
    "coherence" = coherence(...),
    "sample_bound" = sample_bound(...),
    "svt" = svt(...),
    "matrixcompletion" = svt(...),
    "matrix_completion_low_rank" = svt(...),
    "relative_error" = list(relative_error = relative_error(...)),
    "cheatsheet" = list(cheatsheet = cheatsheet()),
    stop("meglt: unknown op ", shQuote(op))
  )
}
