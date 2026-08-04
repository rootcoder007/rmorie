# Rangayyan chapters 9 and 10, second block: the decomposition,
# dictionary-learning, blind-source-separation, network and application
# routines.  Mirror of the Python bsaclass chunk that carries mlpbp through
# vagtfd; the Python module is the reference implementation and this file
# reproduces its arithmetic step for step, including the deterministic
# generator, so the two arms agree to the last bit rather than to the
# platform.
#
# Base R only.  Summation goes through .morie_fsum (aaa_dist_native.R) for
# the same reason it does on the Python side: math.fsum there, Neumaier
# compensation here, so an iterative update does not drift apart between
# languages.
#
# Indices in the payloads are 0-BASED, as in the Python arm and as the
# existing Rangayyan R files already do (see SvmKern's support_vectors).

# ---------------------------------------------------------------- helpers

.morie_bx_vec <- function(v, name = "x") {
  out <- as.numeric(v)
  if (length(out) == 0L)
    stop(name, " must be a non-empty sequence")
  if (any(!is.finite(out)))
    stop(name, " must contain only finite values")
  out
}

.morie_bx_mat <- function(M, name = "X") {
  if (is.null(M)) stop(name, " is required")
  if (is.list(M) && !is.data.frame(M)) {
    if (length(M) == 0L) stop(name, " must have at least one row")
    w <- vapply(M, length, integer(1))
    if (any(w == 0L)) stop(name, " rows must be non-empty")
    if (length(unique(w)) != 1L) stop(name, " must be rectangular")
    M <- do.call(rbind, lapply(M, as.numeric))
  }
  M <- as.matrix(M)
  storage.mode(M) <- "double"
  if (nrow(M) < 1L) stop(name, " must have at least one row")
  if (ncol(M) < 1L) stop(name, " rows must be non-empty")
  if (any(!is.finite(M))) stop(name, " must contain only finite values")
  M
}

.morie_bx_dot <- function(a, b) .morie_fsum(a * b)

.morie_bx_nrm <- function(a) sqrt(.morie_fsum(a * a))

.morie_bx_mm <- function(A, B) {
  # matrix product with compensated inner sums, not BLAS: the summation
  # order has to match the Python arm or an iterative update diverges
  if (ncol(A) != nrow(B))
    stop("inner matrix dimensions do not agree")
  out <- matrix(0, nrow(A), ncol(B))
  for (i in seq_len(nrow(A)))
    for (j in seq_len(ncol(B)))
      out[i, j] <- .morie_fsum(A[i, ] * B[, j])
  out
}

.morie_bx_mv <- function(A, v) {
  if (ncol(A) != length(v))
    stop("matrix and vector dimensions do not agree")
  vapply(seq_len(nrow(A)), function(i) .morie_fsum(A[i, ] * v), numeric(1))
}

.morie_bx_mean <- function(v) .morie_fsum(v) / length(v)

.morie_bx_sd <- function(v, ddof = 1) {
  n <- length(v)
  if (n - ddof < 1) return(0)
  m <- .morie_bx_mean(v)
  sqrt(.morie_fsum((v - m)^2) / (n - ddof))
}

.morie_bx_kurt <- function(v) {
  # kurtosis EXCESS K' = K - 3, eq (3.5) and the note below it: zero for a
  # Gaussian, positive for a peaked heavy-tailed PDF
  n <- length(v)
  if (n < 4L) stop("kurtosis needs at least four samples")
  m <- .morie_bx_mean(v)
  s2 <- .morie_fsum((v - m)^2) / n
  if (s2 <= 0) return(0)
  m4 <- .morie_fsum((v - m)^4) / n
  m4 / (s2 * s2) - 3
}

.morie_bx_solve <- function(A, b) {
  # Gaussian elimination with partial pivoting; raises rather than
  # returning garbage on a singular system
  n <- nrow(A)
  if (n != length(b) || ncol(A) != n)
    stop("linear system is not square or is inconsistent")
  M <- cbind(A, as.numeric(b))
  for (c in seq_len(n)) {
    p <- c - 1L + which.max(abs(M[c:n, c]))
    if (abs(M[p, c]) < 1e-300) stop("linear system is singular")
    if (p != c) {
      tmp <- M[c, ]; M[c, ] <- M[p, ]; M[p, ] <- tmp
    }
    pv <- M[c, c]
    for (r in seq_len(n)) {
      if (r == c) next
      f <- M[r, c] / pv
      if (f == 0) next
      k <- c:(n + 1L)
      M[r, k] <- M[r, k] - f * M[c, k]
    }
  }
  vapply(seq_len(n), function(i) M[i, n + 1L] / M[i, i], numeric(1))
}

.morie_bx_lstsq <- function(A, y, ridge = 1e-10) {
  At <- t(A)
  G <- .morie_bx_mm(At, A)
  diag(G) <- diag(G) + ridge
  .morie_bx_solve(G, .morie_bx_mv(At, y))
}

.morie_bx_jacobi <- function(S, sweeps = 60L, tol = 1e-12) {
  # cyclic Jacobi rotations: the whole spectrum, repeated eigenvalues
  # included, without the deflation error of power iteration
  n <- nrow(S)
  A <- S
  V <- diag(1, n)
  offmask <- row(A) != col(A)
  for (sw in seq_len(sweeps)) {
    off <- .morie_fsum(A[offmask]^2)
    if (off <= tol) break
    if (n >= 2L) for (p in seq_len(n - 1L)) for (q in (p + 1L):n) {
      if (abs(A[p, q]) < 1e-300) next
      theta <- (A[q, q] - A[p, p]) / (2 * A[p, q])
      tt <- (if (theta >= 0) 1 else -1) /
        (abs(theta) + sqrt(theta * theta + 1))
      cc <- 1 / sqrt(tt * tt + 1)
      ss <- tt * cc
      akp <- A[, p]; akq <- A[, q]
      A[, p] <- cc * akp - ss * akq
      A[, q] <- ss * akp + cc * akq
      apk <- A[p, ]; aqk <- A[q, ]
      A[p, ] <- cc * apk - ss * aqk
      A[q, ] <- ss * apk + cc * aqk
      vkp <- V[, p]; vkq <- V[, q]
      V[, p] <- cc * vkp - ss * vkq
      V[, q] <- ss * vkp + cc * vkq
    }
  }
  vals <- diag(A)
  ord <- order(-vals, method = "radix")
  list(values = vals[ord], vectors = V[, ord, drop = FALSE])
}

.morie_bx_rng <- function(seed) {
  # Numerical Recipes ranqd1 LCG on (0, 1).  1664525 * (2^32 - 1) is below
  # 2^53, so the double arithmetic here is exact and matches the Python
  # arm's 32-bit masked integer step for step.
  st <- as.numeric(seed) %% 4294967296
  function() {
    st <<- (1664525 * st + 1013904223) %% 4294967296
    (st + 0.5) / 4294967296
  }
}

.morie_bx_fill <- function(nr, nc, u, f) {
  # row-major fill, the order the Python list comprehensions draw in
  m <- matrix(0, nr, nc)
  for (i in seq_len(nr)) for (j in seq_len(nc)) m[i, j] <- f(u())
  m
}

.morie_bx_nsum <- function(v) {
  # plain sequential double accumulation, NOT compensated: mirrors the one
  # place the Python arm uses `x += ...` in a loop instead of fsum, so the
  # two arms round identically there too
  s <- 0
  for (t in v) s <- s + t
  s
}

.morie_bx_cov <- function(X, unbiased = TRUE) {
  n <- nrow(X); p <- ncol(X)
  if (unbiased && n < 2L)
    stop("covariance needs at least two observations")
  mu <- vapply(seq_len(p), function(j) .morie_fsum(X[, j]) / n, numeric(1))
  d <- if (unbiased) n - 1 else n
  C <- matrix(0, p, p)
  for (a in seq_len(p)) for (b in a:p) {
    v <- .morie_fsum((X[, a] - mu[a]) * (X[, b] - mu[b])) / d
    C[a, b] <- v
    C[b, a] <- v
  }
  list(mu = mu, C = C)
}

.morie_bx_nmfmu <- function(V, r, maxiter, tol, seed, cost) {
  # Lee-Seung multiplicative updates: "ls" is eqs (9.49)-(9.50), "kld" is
  # eqs (9.54)-(9.55)
  m <- nrow(V); n <- ncol(V)
  if (any(V < 0)) stop("NMF requires a nonnegative matrix V")
  r <- as.integer(r)
  if (r < 1L || r > min(m, n))
    stop("rank r must satisfy 1 <= r <= min(rows, cols) of V")
  u <- .morie_bx_rng(seed)
  rowsums <- vapply(seq_len(m), function(i) .morie_fsum(V[i, ]), numeric(1))
  scale <- sqrt(max(.morie_fsum(rowsums) / (m * n), 1e-12) / r)
  W <- .morie_bx_fill(m, r, u, function(z) scale * (0.5 + z))
  H <- .morie_bx_fill(r, n, u, function(z) scale * (0.5 + z))
  eps <- 1e-12
  prev <- NULL
  err <- NaN
  it <- 0L
  for (it in seq_len(as.integer(maxiter))) {
    if (identical(cost, "ls")) {
      Wt <- t(W)
      WtV <- .morie_bx_mm(Wt, V)
      WtWH <- .morie_bx_mm(.morie_bx_mm(Wt, W), H)
      H <- H * WtV / (WtWH + eps)
      Ht <- t(H)
      VHt <- .morie_bx_mm(V, Ht)
      WHHt <- .morie_bx_mm(W, .morie_bx_mm(H, Ht))
      W <- W * VHt / (WHHt + eps)
    } else {
      R <- .morie_bx_mm(W, H)
      Q <- V / (R + eps)
      num <- .morie_bx_mm(Q, t(H))
      den <- vapply(seq_len(r), function(b) .morie_fsum(H[b, ]), numeric(1))
      W <- W * num / rep(den + eps, each = m)
      R <- .morie_bx_mm(W, H)
      Q <- V / (R + eps)
      num <- .morie_bx_mm(t(W), Q)
      den <- vapply(seq_len(r), function(a) .morie_fsum(W[, a]), numeric(1))
      H <- H * num / (den + eps)
    }
    R <- .morie_bx_mm(W, H)
    err <- sqrt(.morie_fsum(as.numeric(t((V - R)^2))))
    if (!is.null(prev) && abs(prev - err) <= tol * max(1, prev)) break
    prev <- err
  }
  list(W = W, H = H, error = err, iterations = it)
}

.morie_bx_omp <- function(x, D, sparsity, tol) {
  # greedy atom picks with a least-squares reprojection on the support
  n <- length(x)
  if (ncol(D) != n)
    stop("every dictionary atom must have the same length as x")
  norms <- vapply(seq_len(nrow(D)), function(j) .morie_bx_nrm(D[j, ]),
                  numeric(1))
  if (any(norms <= 0)) stop("dictionary atoms must have nonzero norm")
  nd <- nrow(D)
  r <- x
  sup <- integer(0)
  coef <- numeric(nd)
  k <- if (is.null(sparsity)) nd else as.integer(sparsity)
  if (k < 1L) stop("sparsity must be a positive integer")
  for (step in seq_len(min(k, nd, n))) {
    if (.morie_bx_nrm(r) <= tol) break
    best <- -1L; bv <- -1
    for (j in seq_len(nd)) {
      if (j %in% sup) next
      v <- abs(.morie_bx_dot(D[j, ], r)) / norms[j]
      if (v > bv) { best <- j; bv <- v }
    }
    if (best < 0L) break
    sup <- c(sup, best)
    A <- t(D[sup, , drop = FALSE])
    w <- .morie_bx_lstsq(A, x)
    coef <- numeric(nd)
    coef[sup] <- w
    approx <- .morie_bx_mv(A, w)
    r <- x - approx
  }
  list(coefficients = coef, support = sup, residual = r)
}

.morie_bx_gabor <- function(n, natoms, seed = 1) {
  # real Gabor dictionary, eqs (9.2)-(9.3), on a fixed dyadic grid so the
  # dictionary is reproducible without an RNG
  natoms <- as.integer(natoms)
  if (n < 4L || natoms < 1L)
    stop("need n >= 4 samples and at least one atom")
  atoms <- vector("list", 0L)
  params <- vector("list", 0L)
  scales <- n / 2^(1:5)
  tvec <- seq_len(n) - 1L
  j <- 0L
  repeat {
    s <- scales[(j %% length(scales)) + 1L]
    step <- max(1L, as.integer(s / 2))
    for (tau in seq(0L, n - 1L, by = step)) {
      for (f in c(0, 0.5 / s, 1 / s, 2 / s, 4 / s)) {
        z <- (tvec - tau) / s
        a <- ifelse(abs(z) > 6, 0,
                    2^0.25 / sqrt(s) * exp(-pi * z * z) *
                      cos(2 * pi * f * tvec))
        nr <- .morie_bx_nrm(a)
        if (nr <= 1e-12) next
        atoms[[length(atoms) + 1L]] <- a / nr
        params[[length(params) + 1L]] <-
          list(scale = s, translation = as.numeric(tau), frequency = f)
        if (length(atoms) >= natoms)
          return(list(atoms = do.call(rbind, atoms), params = params))
      }
    }
    j <- j + 1L
    if (j > 64L) break
  }
  list(atoms = if (length(atoms)) do.call(rbind, atoms) else
    matrix(0, 0L, n), params = params)
}

.morie_bx_dftmag <- function(x) {
  n <- length(x)
  tt <- seq_len(n) - 1L
  vapply(0:(n %/% 2L), function(k) {
    w <- -2 * pi * k / n
    re <- .morie_fsum(x * cos(w * tt))
    im <- .morie_fsum(x * sin(w * tt))
    sqrt(re * re + im * im)
  }, numeric(1))
}

.morie_bx_stft <- function(x, nwin, hop) {
  n <- length(x)
  if (nwin < 4L || nwin > n)
    stop("window length must satisfy 4 <= nwin <= len(x)")
  if (hop < 1L || hop > nwin)
    stop("hop must satisfy 1 <= hop <= nwin")
  win <- 0.5 - 0.5 * cos(2 * pi * (seq_len(nwin) - 1L) / nwin)
  nb <- nwin %/% 2L + 1L
  tt <- seq_len(nwin) - 1L
  re_f <- list(); im_f <- list(); mag_f <- list()
  st <- 0L
  while (st + nwin <= n) {
    seg <- x[st + seq_len(nwin)] * win
    re_r <- numeric(nb); im_r <- numeric(nb); mg_r <- numeric(nb)
    for (k in seq_len(nb)) {
      w <- -2 * pi * (k - 1L) / nwin
      re <- .morie_fsum(seg * cos(w * tt))
      im <- .morie_fsum(seg * sin(w * tt))
      re_r[k] <- re; im_r[k] <- im
      mg_r[k] <- sqrt(re * re + im * im)
    }
    re_f[[length(re_f) + 1L]] <- re_r
    im_f[[length(im_f) + 1L]] <- im_r
    mag_f[[length(mag_f) + 1L]] <- mg_r
    st <- st + hop
  }
  if (!length(mag_f))
    stop("signal is too short for the requested window")
  list(re = do.call(rbind, re_f), im = do.call(rbind, im_f),
       mag = do.call(rbind, mag_f), win = win)
}

.morie_bx_istft <- function(re_f, im_f, nwin, hop, win, n) {
  out <- numeric(n)
  wsum <- numeric(n)
  nb <- nwin %/% 2L + 1L
  for (fi in seq_len(nrow(re_f))) {
    st <- (fi - 1L) * hop
    seg <- numeric(nwin)
    for (t in 0:(nwin - 1L)) {
      acc <- re_f[fi, 1L]
      if (nb >= 2L) for (k in 1:(nb - 1L)) {
        w <- 2 * pi * k * t / nwin
        cf <- if (k < nwin - k) 2 else 1
        acc <- acc + cf * (re_f[fi, k + 1L] * cos(w) -
                             im_f[fi, k + 1L] * sin(w))
      }
      seg[t + 1L] <- acc / nwin
    }
    for (t in seq_len(nwin)) {
      if (st + t <= n) {
        out[st + t] <- out[st + t] + seg[t] * win[t]
        wsum[st + t] <- wsum[st + t] + win[t] * win[t]
      }
    }
  }
  ifelse(wsum > 1e-12, out / wsum, 0)
}

.morie_bx_confusion <- function(true, pred) {
  list(tp = sum(true == 1 & pred == 1), tn = sum(true == 0 & pred == 0),
       fp = sum(true == 0 & pred == 1), fn = sum(true == 1 & pred == 0))
}

.morie_bx_scores <- function(tp, tn, fp, fn) {
  tot <- tp + tn + fp + fn
  list(sensitivity = if (tp + fn > 0) tp / (tp + fn) else NaN,
       specificity = if (tn + fp > 0) tn / (tn + fp) else NaN,
       accuracy = if (tot > 0) (tp + tn) / tot else NaN)
}

.morie_bx_sig <- function(b) {
  # logistic node function, eq (10.81), saturated rather than overflowing
  ifelse(b < -700, 0, ifelse(b > 700, 1, 1 / (1 + exp(-b))))
}

# ------------------------------------------------------------- functions

MlpBp <- function(X, y, hidden = 4, eta = 0.5, alpha = 0.9, maxiter = 500,
                  tol = 1e-4, seed = 1) {
  # Section 10.8, Figure 10.5.  Forward pass eqs (10.79)-(10.81), weight
  # and offset updates eqs (10.82)-(10.85) with the gain eta and the
  # momentum alpha.  When neither prior probabilities nor a symbolic rule
  # base exist, a network that infers the decision surface from labelled
  # instances is the practical alternative to the parametric classifiers.
  Xm <- .morie_bx_mat(X, "X")
  yv <- .morie_bx_vec(y, "y")
  if (nrow(Xm) != length(yv))
    stop("X and y must have the same number of rows")
  hidden <- as.integer(hidden)
  if (hidden < 1L) stop("hidden must be a positive integer")
  if (!(eta > 0 && eta <= 10) || !(alpha >= 0 && alpha < 1))
    stop("need 0 < eta <= 10 and 0 <= alpha < 1")
  maxiter <- as.integer(maxiter)
  if (maxiter < 1L) stop("maxiter must be a positive integer")

  labels <- sort(unique(as.integer(yv)))
  if (length(labels) < 2L)
    stop("y must contain at least two distinct classes")
  K <- if (length(labels) == 2L) 1L else length(labels)
  idx <- match(as.integer(yv), labels) - 1L
  n <- nrow(Xm); I <- ncol(Xm)
  D <- matrix(0, n, K)
  for (s in seq_len(n)) {
    if (K == 1L) D[s, 1L] <- idx[s]
    else D[s, idx[s] + 1L] <- 1
  }

  u <- .morie_bx_rng(seed)
  W1 <- .morie_bx_fill(I, hidden, u, function(z) z - 0.5)
  T1 <- vapply(seq_len(hidden), function(i) u() - 0.5, numeric(1))
  W2 <- .morie_bx_fill(hidden, K, u, function(z) z - 0.5)
  T2 <- vapply(seq_len(K), function(i) u() - 0.5, numeric(1))
  dW1 <- matrix(0, I, hidden); dT1 <- numeric(hidden)
  dW2 <- matrix(0, hidden, K); dT2 <- numeric(K)

  mse <- NaN
  it <- 0L
  for (it in seq_len(maxiter)) {
    tot <- 0
    for (s in seq_len(n)) {
      xs <- Xm[s, ]
      xh <- .morie_bx_sig(vapply(seq_len(hidden), function(j)
        .morie_fsum(W1[, j] * xs), numeric(1)) - T1)
      yo <- .morie_bx_sig(vapply(seq_len(K), function(k)
        .morie_fsum(W2[, k] * xh), numeric(1)) - T2)
      dk <- yo * (1 - yo) * (D[s, ] - yo)
      tot <- tot + .morie_fsum((D[s, ] - yo)^2)
      step <- eta * outer(xh, dk) + alpha * dW2
      W2 <- W2 + step
      dW2 <- step
      stepb <- -eta * dk + alpha * dT2
      T2 <- T2 + stepb
      dT2 <- stepb
      bp <- xh * (1 - xh) * vapply(seq_len(hidden), function(j)
        .morie_fsum(dk * W2[j, ]), numeric(1))
      step1 <- eta * outer(xs, bp) + alpha * dW1
      W1 <- W1 + step1
      dW1 <- step1
      step1b <- -eta * bp + alpha * dT1
      T1 <- T1 + step1b
      dT1 <- step1b
    }
    mse <- tot / (n * K)
    if (mse <= tol) break
  }

  pred <- integer(n)
  raw <- matrix(0, n, K)
  for (s in seq_len(n)) {
    xs <- Xm[s, ]
    xh <- .morie_bx_sig(vapply(seq_len(hidden), function(j)
      .morie_fsum(W1[, j] * xs), numeric(1)) - T1)
    yo <- .morie_bx_sig(vapply(seq_len(K), function(k)
      .morie_fsum(W2[, k] * xh), numeric(1)) - T2)
    raw[s, ] <- yo
    pred[s] <- if (K == 1L) (if (yo[1L] >= 0.5) labels[2L] else labels[1L])
    else labels[which.max(yo)]
  }
  acc <- sum(as.integer(yv) == pred) / n
  list(weights = list(input_hidden = W1, hidden_output = W2),
       offsets = list(hidden = T1, output = T2),
       predictions = pred, outputs = raw, classes = labels,
       accuracy = acc, mse = mse, iterations = it,
       method = paste0("two-layer perceptron trained by back-propagation, ",
                       "Rangayyan Biomedical Signal Analysis 3rd ed. ",
                       "Section 10.8, eqs. (10.79)-(10.85)"))
}

Bbb <- function(qrsdur, criteria = NULL) {
  # Section 10.2.1.  Bundle-branch block desynchronises ventricular
  # contraction and shows as a wider-than-normal QRS; the published logic
  # is a conjunction of duration and amplitude measurements on named
  # leads, so a program applies it once those measurements exist.
  qrsdur <- suppressWarnings(as.numeric(qrsdur))
  if (length(qrsdur) != 1L || is.na(qrsdur))
    stop("qrsdur must be a number of milliseconds")
  if (!is.finite(qrsdur) || qrsdur <= 0)
    stop("qrsdur must be a positive, finite duration in ms")
  if (is.null(criteria)) criteria <- list()
  if (!is.list(criteria))
    stop("criteria must be a list of boolean measurements")
  g <- function(k)
    k %in% names(criteria) && isTRUE(as.logical(criteria[[k]])[1L])

  left_parts <- list(
    qrs_105_to_120_ms = qrsdur >= 105 && qrsdur <= 120,
    qrs_negative_in_v1_and_v2 = g("qrsneg_v1v2"),
    q_or_s_at_least_80_ms_in_v1_and_v2 = g("qsdur80_v1v2"),
    no_q_in_two_of_i_v5_v6 = g("noq_two_of_i_v5_v6"),
    r_over_60_ms_in_two_of_i_avl_v5_v6 = g("rdur60_two_of_i_avl_v5_v6"))
  right_parts <- list(
    qrs_91_to_120_ms = qrsdur >= 91 && qrsdur <= 120,
    s_at_least_40_ms_in_two_of_i_avl_v4_v5_v6 =
      g("sdur40_two_of_i_avl_v4_v5_v6"),
    r_or_rprime_pattern_in_v1_or_v2 = g("r_v1v2") || g("rprime_v1v2"))
  left <- all(unlist(left_parts))
  right <- all(unlist(right_parts))
  wide <- qrsdur > 100

  block <- if (left && right)
    "criteria met for both left and right incomplete block"
  else if (left) "incomplete left bundle-branch block"
  else if (right) "incomplete right bundle-branch block"
  else if (qrsdur > 120) "QRS wider than 120 ms, complete block not excluded"
  else if (wide) "QRS wider than normal, block criteria not met"
  else "no bundle-branch block by these criteria"

  list(blocktype = block, qrsdur = qrsdur, wide = wide, left = left,
       right = right,
       satisfied = list(left = left_parts, right = right_parts),
       method = paste0("incomplete bundle-branch block decision rules, ",
                       "Rangayyan Biomedical Signal Analysis 3rd ed. ",
                       "Section 10.2.1"))
}

PvcBayes <- function(features, labels, priors = NULL, query = NULL) {
  # Section 10.11.2 with the normal-pattern Bayes classifier of 10.6.2.
  # The linear rule of 10.11.1 commits to a hard boundary; the Bayes rule
  # states the posterior odds and lets the prevalence of ectopy enter,
  # which matters because a PVC prior is far below one half.
  F <- .morie_bx_mat(features, "features")
  y <- as.integer(.morie_bx_vec(labels, "labels"))
  if (nrow(F) != length(y))
    stop("features and labels must have the same length")
  classes <- sort(unique(y))
  if (length(classes) != 2L)
    stop("PvcBayes expects exactly two classes, e.g. 0 and 1")
  p <- ncol(F)
  if (is.null(priors)) {
    pri <- c(0.5, 0.5)
  } else {
    pri <- .morie_bx_vec(priors, "priors")
    if (length(pri) != 2L || any(pri < 0) || .morie_fsum(pri) <= 0)
      stop("priors must be two nonnegative numbers")
    pri <- pri / .morie_fsum(pri)
  }

  scale <- vapply(seq_len(p), function(j) {
    sd <- .morie_bx_sd(F[, j]); if (sd > 0) sd else 1
  }, numeric(1))
  Z <- F / rep(scale, each = nrow(F))

  means <- list(); covs <- list(); invs <- list(); dets <- numeric(2)
  for (ci in seq_along(classes)) {
    rows <- Z[y == classes[ci], , drop = FALSE]
    if (nrow(rows) < p + 1L)
      stop("each class needs more rows than features to estimate a ",
           "covariance matrix")
    cv <- .morie_bx_cov(rows)
    C <- cv$C
    diag(C) <- diag(C) + 1e-9
    ev <- .morie_bx_jacobi(C)
    if (min(ev$values) <= 0)
      stop("a class covariance matrix is not positive definite")
    dets[ci] <- prod(ev$values)
    inv <- matrix(0, p, p)
    for (i in seq_len(p)) for (j in seq_len(p))
      inv[i, j] <- .morie_fsum(ev$vectors[i, ] * ev$vectors[j, ] / ev$values)
    means[[ci]] <- cv$mu
    covs[[ci]] <- C
    invs[[ci]] <- inv
  }

  post <- function(z) {
    dens <- numeric(2)
    for (ci in 1:2) {
      d <- z - means[[ci]]
      q <- .morie_fsum(as.numeric(t(outer(d, d) * invs[[ci]])))
      dens[ci] <- pri[ci] * exp(-0.5 * q) /
        sqrt(((2 * pi)^p) * dets[ci])
    }
    tot <- .morie_fsum(dens)
    if (tot <= 0) return(c(0.5, 0.5))
    dens / tot
  }

  P <- t(vapply(seq_len(nrow(Z)), function(i) post(Z[i, ]), numeric(2)))
  pred <- ifelse(P[, 1L] >= P[, 2L], classes[1L], classes[2L])
  cf <- .morie_bx_confusion(as.integer(y == classes[2L]),
                            as.integer(pred == classes[2L]))
  sc <- .morie_bx_scores(cf$tp, cf$tn, cf$fp, cf$fn)

  qcls <- NULL
  if (!is.null(query)) {
    Q <- .morie_bx_mat(query, "query")
    if (ncol(Q) != p)
      stop("query must have the same number of features")
    qcls <- vapply(seq_len(nrow(Q)), function(i) {
      t <- post(Q[i, ] / scale)
      if (t[1L] >= t[2L]) classes[1L] else classes[2L]
    }, numeric(1))
  }

  list(predictions = pred, queryclass = qcls, posterior = P, means = means,
       covariances = covs, scale = scale, confusion = cf,
       accuracy = sc$accuracy, sensitivity = sc$sensitivity,
       specificity = sc$specificity, priors = pri, classes = classes,
       method = paste0("Gaussian Bayes classifier on [QRSTA, FF] beat ",
                       "features, Rangayyan Biomedical Signal Analysis 3rd ",
                       "ed. Section 10.11.2 with the normal-pattern ",
                       "classifier of Section 10.6.2"))
}

.morie_bx_chsel <- function(trials, nselect, rank, maxiter, tol, seed) {
  # shared core of BciChSel and NmfChSel: eqs (9.94)-(9.96)
  X <- .morie_bx_mat(trials, "trials")
  nch <- nrow(X)
  if (nch < 2L) stop("need at least two EEG channels")
  nselect <- as.integer(nselect)
  if (!(nselect >= 1L && nselect <= nch))
    stop("nselect must satisfy 1 <= nselect <= number of channels")
  rank <- as.integer(rank)
  if (rank < 3L)
    stop("rank must be at least 3: with r = 2 the min-max normalisation ",
         "of eq. (9.95) maps every basis row to {0, 1}, so the RMS ",
         "deviation of eq. (9.96) is exactly 0.5 for every channel and ",
         "ranks nothing")
  cv <- .morie_bx_cov(t(X))
  C <- cv$C
  shift <- min(C)
  V <- if (shift < 0) C - shift else C
  fac <- .morie_bx_nmfmu(V, rank, maxiter, tol, seed, "ls")
  W <- fac$W
  Wn <- matrix(0, nch, rank)
  rmsd <- numeric(nch)
  for (j in seq_len(nch)) {
    row <- W[j, ]
    lo <- min(row); hi <- max(row)
    nr <- if (hi - lo <= 0) rep(0.5, length(row)) else (row - lo) / (hi - lo)
    Wn[j, ] <- nr
    rmsd[j] <- sqrt(.morie_fsum((nr - 0.5)^2) / length(nr))
  }
  ranking <- order(-rmsd, seq_len(nch), method = "radix")
  list(X = X, C = C, W = W, H = fac$H, error = fac$error, rmsd = rmsd,
       normalized = Wn, ranking = ranking - 1L,
       selected = sort(ranking[seq_len(nselect)]) - 1L)
}

BciChSel <- function(trials, nselect, rank = 4, maxiter = 200, tol = 1e-8,
                     seed = 1) {
  # Section 9.12.1 in full: a BCI runs under hardware complexity limits and
  # the optimal channel set is subject specific, so the selector has to be
  # data driven.  Channel covariance eq (9.94) -> NMF -> min-max normalised
  # basis rows eq (9.95) -> RMS deviation score eq (9.96); the final step
  # of that section weights the selected channels by their scores.
  r <- .morie_bx_chsel(trials, nselect, rank, maxiter, tol, seed)
  sel <- r$selected + 1L
  weighted <- r$X[sel, , drop = FALSE] * r$rmsd[sel]
  list(selected = r$selected, weights = r$rmsd[sel], rmsd = r$rmsd,
       normalized = r$normalized, weighted = weighted, W = r$W, H = r$H,
       covariance = r$C, error = r$error,
       method = paste0("NMF-based EEG channel selection and weighting for ",
                       "BCI, Rangayyan Biomedical Signal Analysis 3rd ed. ",
                       "Section 9.12.1, eqs. (9.94)-(9.96)"))
}

BPursuit <- function(x, D, lam = 0.01, maxiter = 2000, tol = 1e-10) {
  # NOT from Rangayyan: the book covers matching pursuit (9.3) and EMD
  # dictionary learning (9.5), not basis pursuit.  Chen, Donoho and
  # Saunders, SIAM J. Sci. Comput. 20(1):33-61, 1998 for the L1
  # formulation; Daubechies, Defrise and De Mol, CPAM 57(11):1413-1457,
  # 2004 for the thresholded-Landweber solver.  Greedy pursuit fixes an
  # atom the moment it is chosen; here every coefficient stays free.
  x <- .morie_bx_vec(x, "x")
  A <- .morie_bx_mat(D, "D")
  n <- length(x)
  if (ncol(A) != n)
    stop("every dictionary atom must have the same length as x")
  lam <- as.numeric(lam)
  if (lam < 0) stop("lam must be nonnegative")
  maxiter <- as.integer(maxiter)
  if (maxiter < 1L) stop("maxiter must be a positive integer")

  m <- nrow(A)
  v <- rep(1 / sqrt(m), m)
  L <- 1
  At <- t(A)
  for (i in seq_len(60L)) {
    w <- .morie_bx_mv(A, .morie_bx_mv(At, v))
    nw <- .morie_bx_nrm(w)
    if (nw <= 1e-300) break
    v <- w / nw
    L <- nw
  }
  L <- max(L, 1e-12)

  a <- numeric(m)
  it <- 0L
  for (it in seq_len(maxiter)) {
    approx <- .morie_bx_mv(At, a)
    r <- x - approx
    g <- .morie_bx_mv(A, r)
    shift <- 0
    for (j in seq_len(m)) {
      z <- a[j] + g[j] / L
      th <- lam / L
      new <- if (abs(z) <= th) 0 else if (z > 0) z - th else z + th
      shift <- max(shift, abs(new - a[j]))
      a[j] <- new
    }
    if (shift <= tol) break
  }

  approx <- .morie_bx_mv(At, a)
  r <- x - approx
  l1 <- .morie_fsum(abs(a))
  list(alpha = a, support = which(a != 0) - 1L, reconstruction = approx,
       residual = r, l1norm = l1,
       objective = 0.5 * .morie_fsum(r * r) + lam * l1, iterations = it,
       method = paste0("basis-pursuit denoising by iterative soft ",
                       "thresholding; Chen, Donoho and Saunders, SIAM J. ",
                       "Sci. Comput. 20(1):33-61, 1998; solver of ",
                       "Daubechies, Defrise and De Mol, Comm. Pure Appl. ",
                       "Math. 57(11):1413-1457, 2004 (not covered by ",
                       "Rangayyan)"))
}

CadPipe <- function(features, labels, k = 5, standardize = TRUE) {
  # Chapter 10: a CAD system is a chain, not a classifier, and the accuracy
  # quoted for it means nothing unless the test patterns were unseen.
  # Prototype discriminant of 10.4.1, partitioning of 10.10.3, scored by
  # eqs (10.100)-(10.103).
  F <- .morie_bx_mat(features, "features")
  y <- as.integer(.morie_bx_vec(labels, "labels"))
  if (nrow(F) != length(y))
    stop("features and labels must have the same length")
  if (!identical(sort(unique(y)), c(0L, 1L)))
    stop("labels must contain both 0 (without) and 1 (with) the disease")
  n <- nrow(F); p <- ncol(F)
  k <- as.integer(k)
  if (!(k >= 2L && k <= n))
    stop("k must satisfy 2 <= k <= number of patterns")

  folds <- vector("list", k)
  for (i in seq_len(k)) folds[[i]] <- integer(0)
  for (cc in c(0L, 1L)) {
    members <- which(y == cc)
    for (jj in seq_along(members)) {
      f <- ((jj - 1L) %% k) + 1L
      folds[[f]] <- c(folds[[f]], members[jj])
    }
  }
  if (any(vapply(folds, length, integer(1)) == 0L))
    stop("k is too large: some fold is empty")

  pred <- integer(n)
  for (f in seq_len(k)) {
    test <- folds[[f]]
    train <- setdiff(seq_len(n), test)
    if (length(unique(y[train])) < 2L)
      stop("a training partition lost a class; reduce k")
    sc <- rep(1, p)
    if (isTRUE(standardize))
      sc <- vapply(seq_len(p), function(j) {
        s <- .morie_bx_sd(F[train, j]); if (s > 0) s else 1
      }, numeric(1))
    proto <- lapply(c(0L, 1L), function(cc) {
      rows <- train[y[train] == cc]
      vapply(seq_len(p), function(j)
        .morie_fsum(F[rows, j] / sc[j]) / length(rows), numeric(1))
    })
    for (i in test) {
      z <- F[i, ] / sc
      d0 <- .morie_fsum((z - proto[[1L]])^2)
      d1 <- .morie_fsum((z - proto[[2L]])^2)
      pred[i] <- if (d1 < d0) 1L else 0L
    }
  }

  cf <- .morie_bx_confusion(y, pred)
  sc <- .morie_bx_scores(cf$tp, cf$tn, cf$fp, cf$fn)
  prev <- sum(y == 1L) / n
  list(accuracy = sc$accuracy, sensitivity = sc$sensitivity,
       specificity = sc$specificity,
       weightedaccuracy = sc$sensitivity * prev +
         sc$specificity * (1 - prev),
       confusion = cf, predictions = pred,
       folds = lapply(folds, function(f) sort(f) - 1L), prevalence = prev,
       method = paste0("cross-validated prototype-discriminant CAD ",
                       "pipeline, Rangayyan Biomedical Signal Analysis 3rd ",
                       "ed. Sections 10.4.1 and 10.10.3, scored by ",
                       "eqs. (10.100)-(10.103)"))
}

CnnSig <- function(x, kernels, bias = NULL, pool = 2, dense = NULL) {
  # Section 10.8.2 names CNNs as the common deep model but gives no layer
  # equations; the convolution, rectifier and pooling used here are those
  # of LeCun, Bengio and Hinton, Nature 521(7553):436-444, 2015, which is
  # reference [35] of that section.
  x <- .morie_bx_vec(x, "x")
  K <- .morie_bx_mat(kernels, "kernels")
  pool <- as.integer(pool)
  if (pool < 1L) stop("pool must be a positive integer")
  if (ncol(K) > length(x))
    stop("every kernel must be no longer than the signal")
  if (is.null(bias)) {
    b <- numeric(nrow(K))
  } else {
    b <- .morie_bx_vec(bias, "bias")
    if (length(b) != nrow(K)) stop("bias must have one entry per kernel")
  }

  maps <- vector("list", nrow(K))
  pooled <- vector("list", nrow(K))
  for (ki in seq_len(nrow(K))) {
    w <- K[ki, ]
    m <- length(w)
    conv <- vapply(seq_len(length(x) - m + 1L), function(i)
      max(0, .morie_fsum(w * x[i + seq_len(m) - 1L]) + b[ki]), numeric(1))
    maps[[ki]] <- conv
    st <- if (length(conv) >= pool)
      seq(1L, length(conv) - pool + 1L, by = pool) else integer(0)
    pl <- if (length(st))
      vapply(st, function(i) max(conv[i:(i + pool - 1L)]), numeric(1))
    else max(conv)
    pooled[[ki]] <- pl
  }

  feat <- unlist(pooled, use.names = FALSE)
  scores <- NULL; best <- NULL
  if (!is.null(dense)) {
    Wd <- .morie_bx_mat(dense, "dense")
    if (ncol(Wd) != length(feat))
      stop("dense rows must match the pooled feature length of ",
           length(feat))
    z <- .morie_bx_mv(Wd, feat)
    e <- exp(z - max(z))
    scores <- e / .morie_fsum(e)
    best <- which.max(scores) - 1L
  }

  list(maps = maps, pooled = pooled, features = feat, scores = scores,
       predicted = best,
       method = paste0("1-D convolution, rectifier and max-pooling forward ",
                       "pass; architecture per LeCun, Bengio and Hinton, ",
                       "Nature 521(7553):436-444, 2015, cited as ref. [35] ",
                       "of Rangayyan Biomedical Signal Analysis 3rd ed. ",
                       "Section 10.8.2, which gives no equations for these ",
                       "layers"))
}

FecgNmf <- function(x, fs, nwin = 64, hop = NULL, rank = 4, lam = 0,
                    maxiter = 150, taum = 0.6, tauf = 0.45, seed = 1) {
  # Section 9.11.  The fetal and maternal ECG overlap in the spectrum so no
  # filter separates them, and a single abdominal lead has no spatial
  # diversity: the separation must come from structure in the STFT
  # magnitude matrix.  Mixture eq (9.88), sparse updates (9.89)-(9.90),
  # activation thresholding eq (9.91).
  x <- .morie_bx_vec(x, "x")
  fs <- as.numeric(fs)
  if (fs <= 0) stop("fs must be a positive sampling rate in Hz")
  nwin <- as.integer(nwin)
  hop <- if (is.null(hop)) nwin %/% 2L else as.integer(hop)
  if (!(taum > 0 && taum <= 1) || !(tauf > 0 && tauf <= 1))
    stop("taum and tauf must lie in (0, 1]")
  if (as.numeric(lam) < 0) stop("lam must be nonnegative")

  sp <- .morie_bx_stft(x, nwin, hop)
  V <- t(sp$mag)
  fac <- .morie_bx_nmfmu(V, rank, maxiter, 1e-10, seed,
                         if (as.numeric(lam) == 0) "ls" else "kld")
  W <- fac$W; H <- fac$H
  if (as.numeric(lam) > 0) {
    eps <- 1e-12
    for (i in seq_len(20L)) {
      R <- .morie_bx_mm(W, H)
      for (a in seq_len(nrow(H))) for (bb in seq_len(ncol(H))) {
        num <- .morie_fsum(V[, bb] * W[, a])
        den <- .morie_fsum(R[, bb] * W[, a]) + as.numeric(lam)
        H[a, bb] <- H[a, bb] * num / (den + eps)
      }
    }
  }

  peakcount <- function(row, tau) {
    m <- max(abs(row))
    if (m <= 0) return(0L)
    nr <- abs(row) / m
    if (length(nr) < 3L) return(0L)
    i <- 2:(length(nr) - 1L)
    sum(nr[i] > tau & nr[i] >= nr[i - 1L] & nr[i] > nr[i + 1L])
  }

  cnt <- vapply(seq_len(nrow(H)), function(a) peakcount(H[a, ], tauf),
                numeric(1))
  ord <- order(cnt, seq_along(cnt), method = "radix")
  mrow <- ord[1L] - 1L
  frow <- if (length(ord) > 1L) ord[2L] - 1L else ord[1L] - 1L
  mcount <- peakcount(H[mrow + 1L, ], taum)
  per_row <- as.list(cnt)
  names(per_row) <- as.character(seq_along(cnt) - 1L)
  peaks <- list(per_row = per_row, maternal_row_count_at_taum = mcount)

  rebuild <- function(row) {
    R <- .morie_bx_mm(W, H)
    C <- outer(W[, row + 1L], H[row + 1L, ])
    ren <- matrix(0, nrow(sp$mag), ncol(sp$mag))
    imn <- matrix(0, nrow(sp$mag), ncol(sp$mag))
    for (fi in seq_len(nrow(sp$mag))) for (k in seq_len(ncol(sp$mag))) {
      tot <- R[k, fi]
      msk <- if (tot > 1e-12) C[k, fi] / tot else 0
      ren[fi, k] <- sp$re[fi, k] * msk
      imn[fi, k] <- sp$im[fi, k] * msk
    }
    .morie_bx_istft(ren, imn, nwin, hop, sp$win, length(x))
  }

  list(fetal = rebuild(frow), maternal = rebuild(mrow), fetalrow = frow,
       maternalrow = mrow, peaks = peaks, W = W, H = H, error = fac$error,
       method = paste0("single-channel fetal ECG extraction by NMF of the ",
                       "STFT magnitude with activation-peak selection, ",
                       "Rangayyan Biomedical Signal Analysis 3rd ed. ",
                       "Section 9.11, eqs. (9.88)-(9.91)"))
}

PvcLinDf <- function(rr, ff, train = NULL) {
  # Section 10.11.1, eq (10.131).  A PVC has both a shorter preceding RR
  # interval and a more complex waveshape, and the form factor of eq (5.26)
  # turns the qualitative half of the clinical rule into a number.
  rr <- .morie_bx_vec(rr, "rr")
  ff <- .morie_bx_vec(ff, "ff")
  if (length(rr) != length(ff))
    stop("rr and ff must have the same length")

  if (is.null(train)) {
    a <- 1; b <- -5.56; cc <- 11.44
    proto <- list(normal = c(0.66, 1.58), pvc = c(0.45, 2.74))
    src <- "published coefficients of eq. (10.131)"
  } else {
    if (!is.list(train) || length(train) != 3L)
      stop("train must be a (rr, ff, labels) triple")
    trr <- .morie_bx_vec(train[[1L]], "train rr")
    tff <- .morie_bx_vec(train[[2L]], "train ff")
    tlab <- as.integer(.morie_bx_vec(train[[3L]], "train labels"))
    if (!(length(trr) == length(tff) && length(tff) == length(tlab)))
      stop("training rr, ff and labels must have equal length")
    if (!identical(sort(unique(tlab)), c(0L, 1L)))
      stop("training labels must contain both 0 (normal) and 1 (PVC)")
    p0 <- c(.morie_bx_mean(trr[tlab == 0L]), .morie_bx_mean(tff[tlab == 0L]))
    p1 <- c(.morie_bx_mean(trr[tlab == 1L]), .morie_bx_mean(tff[tlab == 1L]))
    d <- p1 - p0
    if (abs(d[1L]) < 1e-12 && abs(d[2L]) < 1e-12)
      stop("the two class prototypes coincide")
    mid <- 0.5 * (p0 + p1)
    a <- -d[1L]; b <- -d[2L]
    cc <- d[1L] * mid[1L] + d[2L] * mid[2L]
    proto <- list(normal = p0, pvc = p1)
    src <- "coefficients derived from the supplied training set"
  }

  disc <- a * rr + b * ff + cc
  list(labels = as.integer(disc <= 0), discriminant = disc,
       coefficients = list(rr = a, ff = b, constant = cc),
       prototypes = proto, source = src,
       method = paste0("linear discriminant on [RR interval, form factor] ",
                       "for normal vs. ectopic beats, Rangayyan Biomedical ",
                       "Signal Analysis 3rd ed. Section 10.11.1, ",
                       "eq. (10.131)"))
}

EegBands <- function(x, fs, bands = NULL) {
  # Band limits from Section 1.2.6, fractional power by eq (6.44) as used
  # in Section 10.2.3.  The clinical question -- is there an alpha rhythm
  # -- is answered by the fraction of power in that band, not by the power.
  x <- .morie_bx_vec(x, "x")
  fs <- as.numeric(fs)
  if (fs <= 0) stop("fs must be a positive sampling rate in Hz")
  if (length(x) < 8L)
    stop("need at least eight samples to estimate a spectrum")
  nyq <- fs / 2
  if (is.null(bands))
    bands <- list(delta = c(0.5, 4), theta = c(4, 8), alpha = c(8, 13),
                  beta = c(13, nyq), gamma = c(30, min(80, nyq)))
  if (!is.list(bands) || !length(bands))
    stop("bands must be a non-empty list of (f1, f2) pairs")

  xc <- x - .morie_bx_mean(x)
  mag <- .morie_bx_dftmag(xc)
  n <- length(xc)
  freqs <- (seq_along(mag) - 1L) * fs / n
  psd <- mag * mag
  total <- .morie_fsum(psd)

  power <- list(); frac <- list()
  for (nm in names(bands)) {
    lim <- as.numeric(bands[[nm]])
    if (length(lim) < 2L || any(is.na(lim)))
      stop("band ", nm, " must be an (f1, f2) pair")
    f1 <- lim[1L]; f2 <- lim[2L]
    if (f1 < 0 || f2 <= f1)
      stop("band ", nm, " must satisfy 0 <= f1 < f2")
    p <- .morie_fsum(psd[freqs >= f1 & freqs <= f2])
    power[[nm]] <- p
    frac[[nm]] <- if (total > 0) p / total else 0
  }

  list(power = power, fraction = frac,
       dominant = if (total > 0) names(frac)[which.max(unlist(frac))]
       else NULL,
       totalpower = total, frequencies = freqs,
       bands = lapply(bands, function(v) as.numeric(v[1:2])),
       method = paste0("fractional power in the EEG rhythm bands, band ",
                       "limits from Rangayyan Biomedical Signal Analysis ",
                       "3rd ed. Section 1.2.6, fraction by eq. (6.44) as ",
                       "used in Section 10.2.3"))
}

SeizDict <- function(signals, labels, iterations = 7, atoms = NULL,
                     test = NULL) {
  # Section 9.8 with the framework of 9.5, Algorithm 9.2 verbatim.  The EEG
  # is nonstationary so a fixed basis represents seizure and nonseizure
  # segments equally badly; atoms learned from the recordings carry what is
  # unique to each class.
  S <- .morie_bx_mat(signals, "signals")
  y <- as.integer(.morie_bx_vec(labels, "labels"))
  if (nrow(S) != length(y))
    stop("signals and labels must have the same length")
  if (length(unique(y)) < 2L)
    stop("labels must contain at least two classes")
  iterations <- as.integer(iterations)
  if (iterations < 1L) stop("iterations must be a positive integer")
  n <- ncol(S)

  rawdict <- function(sig) {
    if (!is.null(atoms)) {
      A <- .morie_bx_mat(atoms, "atoms")
      if (ncol(A) != n)
        stop("atoms must have the same length as the signals")
      return(A)
    }
    seg <- max(4L, n %/% 4L)
    out <- list()
    for (st in seq(0L, n - seg, by = max(1L, seg %/% 2L))) {
      a <- numeric(n)
      a[st + seq_len(seg)] <- sig[st + seq_len(seg)]
      nr <- .morie_bx_nrm(a)
      if (nr > 1e-12) out[[length(out) + 1L]] <- a / nr
    }
    if (length(out)) do.call(rbind, out) else matrix(0, 0L, n)
  }

  trained <- list()
  for (s in seq_len(nrow(S))) {
    raw <- rawdict(S[s, ])
    x <- S[s, ]
    for (i in seq_len(iterations)) {
      if (!nrow(raw)) break
      best <- -1L; bv <- -1
      for (j in seq_len(nrow(raw))) {
        v <- abs(.morie_bx_dot(x, raw[j, ]))
        if (v > bv) { best <- j; bv <- v }
      }
      if (best < 0L) break
      psi <- raw[best, ]
      raw <- raw[-best, , drop = FALSE]
      dup <- FALSE
      for (d in trained) if (all(abs(psi - d) < 1e-12)) { dup <- TRUE; break }
      if (!dup) trained[[length(trained) + 1L]] <- psi
      a <- .morie_bx_dot(x, psi)
      x <- x - a * psi
    }
  }
  if (!length(trained))
    stop("dictionary learning produced no atoms; check that the signals ",
         "are not all zero")
  Dm <- do.call(rbind, trained)

  feats <- function(sig) {
    co <- vapply(seq_len(nrow(Dm)), function(j)
      .morie_bx_dot(sig, Dm[j, ]), numeric(1))
    rec <- vapply(seq_len(n), function(i)
      .morie_fsum(co * Dm[, i]), numeric(1))
    err <- .morie_bx_nrm(sig - rec)
    list(v = c(co, err), co = co, err = err)
  }

  Ftr <- lapply(seq_len(nrow(S)), function(i) feats(S[i, ]))
  classes <- sort(unique(y))
  cent <- lapply(classes, function(cc) {
    rows <- do.call(rbind, lapply(which(y == cc), function(i) Ftr[[i]]$v))
    vapply(seq_len(ncol(rows)), function(j)
      .morie_fsum(rows[, j]) / nrow(rows), numeric(1))
  })

  assign1 <- function(v)
    classes[which.min(vapply(cent, function(cn)
      .morie_fsum((v - cn)^2), numeric(1)))]

  pred <- vapply(Ftr, function(f) assign1(f$v), numeric(1))
  acc <- sum(y == pred) / length(y)

  tcls <- NULL
  if (!is.null(test)) {
    Tm <- .morie_bx_mat(test, "test")
    if (ncol(Tm) != n)
      stop("test signals must have the same length as training signals")
    tcls <- vapply(seq_len(nrow(Tm)), function(i)
      assign1(feats(Tm[i, ])$v), numeric(1))
  }

  list(dictionary = Dm, coefficients = lapply(Ftr, function(f) f$co),
       error = vapply(Ftr, function(f) f$err, numeric(1)),
       predictions = pred, isseizure = pred == max(classes),
       testclass = tcls, accuracy = acc,
       method = paste0("signal-derived dictionary learning (Algorithm 9.2) ",
                       "with projection-coefficient and reconstruction-",
                       "error features for seizure detection, Rangayyan ",
                       "Biomedical Signal Analysis 3rd ed. Section 9.8"))
}

IcaFix <- function(X, ncomp = NULL, maxiter = 200, tol = 1e-8, seed = 1) {
  # Section 9.7.2: model eq (9.43), unmixing eq (9.44).  PCA can only make
  # components uncorrelated, which is independence only for Gaussians, and
  # a linear mixture tends TOWARDS a Gaussian -- so driving the estimates
  # away from Gaussianity is what unmixes them.  Fixed-point update from
  # Hyvarinen and Oja, Neural Networks 13, 2000, ref [50] of that section.
  Y <- .morie_bx_mat(X, "X")
  K <- nrow(Y); T <- ncol(Y)
  if (T < 4L) stop("need at least four samples per channel")
  L <- if (is.null(ncomp)) K else as.integer(ncomp)
  if (!(L >= 1L && L <= K))
    stop("ncomp must satisfy 1 <= ncomp <= number of channels")
  maxiter <- as.integer(maxiter)
  if (maxiter < 1L) stop("maxiter must be a positive integer")

  mu <- vapply(seq_len(K), function(i) .morie_bx_mean(Y[i, ]), numeric(1))
  Yc <- Y - mu
  C <- matrix(0, K, K)
  for (i in seq_len(K)) for (j in seq_len(K))
    C[i, j] <- .morie_fsum(Yc[i, ] * Yc[j, ]) / T
  ev <- .morie_bx_jacobi(C)
  if (ev$values[L] <= 1e-14)
    stop("the mixture covariance is rank deficient for ", L, " components")
  Wh <- t(ev$vectors[, seq_len(L), drop = FALSE] /
            rep(sqrt(ev$values[seq_len(L)]), each = K))
  Z <- .morie_bx_mm(Wh, Yc)

  u <- .morie_bx_rng(seed)
  Wm <- matrix(0, 0L, L)
  iters <- integer(0)
  for (cix in seq_len(L)) {
    w <- vapply(seq_len(L), function(i) u() - 0.5, numeric(1))
    nr <- .morie_bx_nrm(w)
    w <- if (nr > 1e-12) w / nr else as.numeric(seq_len(L) == cix)
    it <- 0L
    for (it in seq_len(maxiter)) {
      gv <- tanh(vapply(seq_len(T), function(t)
        .morie_fsum(w * Z[, t]), numeric(1)))
      g <- vapply(seq_len(L), function(i)
        .morie_bx_nsum(Z[i, ] * gv), numeric(1))
      gp <- .morie_bx_nsum(1 - gv * gv)
      wn <- g / T - (gp / T) * w
      if (nrow(Wm)) for (pi_ in seq_len(nrow(Wm))) {
        d <- .morie_bx_dot(wn, Wm[pi_, ])
        wn <- wn - d * Wm[pi_, ]
      }
      nr <- .morie_bx_nrm(wn)
      if (nr <= 1e-12) {
        wn <- as.numeric(seq_len(L) == cix)
        nr <- 1
      }
      wn <- wn / nr
      if (abs(abs(.morie_bx_dot(wn, w)) - 1) < tol) { w <- wn; break }
      w <- wn
    }
    Wm <- rbind(Wm, w)
    iters <- c(iters, it)
  }

  S <- .morie_bx_mm(Wm, Z)
  W <- .morie_bx_mm(Wm, Wh)
  Wt <- t(W)
  G <- .morie_bx_mm(W, Wt)
  diag(G) <- diag(G) + 1e-12
  Ginv <- vapply(seq_len(L), function(j)
    .morie_bx_solve(G, as.numeric(seq_len(L) == j)), numeric(L))
  A <- matrix(0, K, L)
  for (i in seq_len(K)) for (j in seq_len(L))
    A[i, j] <- .morie_fsum(Wt[i, ] * Ginv[, j])

  list(sources = S, unmixing = W, mixing = A, whitening = Wh, mean = mu,
       iterations = iters,
       method = paste0("FastICA fixed-point ICA with the tanh ",
                       "nonlinearity; model and unmixing per Rangayyan ",
                       "Biomedical Signal Analysis 3rd ed. Section 9.7.2, ",
                       "eqs. (9.43)-(9.44); update rule from Hyvarinen and ",
                       "Oja, Neural Networks 13, 2000 (ref. [50] there)"))
}

IcaClean <- function(X, ncomp = NULL, kurtosis = 3, drop = NULL,
                     maxiter = 200, seed = 1) {
  # Section 9.7.2 with the kurtosis excess of eq (3.5).  Blinks, muscle and
  # the ECG reach the scalp through separate physical paths, so they are
  # separate components rather than a separable frequency band; zero the
  # component and back-project, and the neural channels survive.
  ica <- IcaFix(X, ncomp = ncomp, maxiter = maxiter, seed = seed)
  S <- ica$sources
  A <- ica$mixing
  mu <- ica$mean
  L <- nrow(S); T <- ncol(S)

  kv <- vapply(seq_len(L), function(c) .morie_bx_kurt(S[c, ]), numeric(1))
  if (is.null(drop)) {
    thr <- as.numeric(kurtosis)
    if (thr < 0) stop("kurtosis threshold must be nonnegative")
    art <- which(abs(kv) > thr) - 1L
  } else {
    art <- sort(unique(as.integer(drop)))
    if (any(art < 0L | art >= L))
      stop("drop indices must lie in [0, ", L, ")")
  }

  Sk <- S
  if (length(art)) Sk[art + 1L, ] <- 0
  rec <- .morie_bx_mm(A, Sk)
  clean <- rec + mu[seq_len(nrow(A))]
  orig <- .morie_fsum(vapply(seq_len(L), function(c)
    .morie_fsum(S[c, ]^2), numeric(1)))
  gone <- if (length(art)) .morie_fsum(vapply(art + 1L, function(c)
    .morie_fsum(S[c, ]^2), numeric(1))) else 0
  list(clean = clean, components = S, kurtosis = kv, artifacts = art,
       mixing = A, removedpower = if (orig > 0) gone / orig else 0,
       method = paste0("ICA artifact removal by zeroing high-kurtosis ",
                       "components and back-projection, Rangayyan ",
                       "Biomedical Signal Analysis 3rd ed. Section 9.7.2 ",
                       "with the kurtosis excess of eq. (3.5)"))
}

Infomax <- function(X, ncomp = NULL, eta = 0.05, maxiter = 300, tol = 1e-8,
                    seed = 1) {
  # NOT from Rangayyan: Section 9.7.2 gives only the generic gradient rule
  # (9.45).  Bell and Sejnowski, Neural Computation 7(6):1129-1159, 1995
  # and Amari, Cichocki and Yang, NIPS 8:757-763, 1996 for the natural
  # gradient.  The logistic nonlinearity matches super-Gaussian sources;
  # on sub-Gaussian ones it separates only partially -- use IcaFix when the
  # kurtosis sign is unknown.
  Y <- .morie_bx_mat(X, "X")
  K <- nrow(Y); T <- ncol(Y)
  if (T < 4L) stop("need at least four samples per channel")
  L <- if (is.null(ncomp)) K else as.integer(ncomp)
  if (!(L >= 1L && L <= K))
    stop("ncomp must satisfy 1 <= ncomp <= number of channels")
  eta <- as.numeric(eta)
  if (!(eta > 0 && eta <= 1)) stop("eta must lie in (0, 1]")
  maxiter <- as.integer(maxiter)
  if (maxiter < 1L) stop("maxiter must be a positive integer")

  mu <- vapply(seq_len(K), function(i) .morie_bx_mean(Y[i, ]), numeric(1))
  Yc <- Y - mu
  C <- matrix(0, K, K)
  for (i in seq_len(K)) for (j in seq_len(K))
    C[i, j] <- .morie_fsum(Yc[i, ] * Yc[j, ]) / T
  ev <- .morie_bx_jacobi(C)
  if (ev$values[L] <= 1e-14)
    stop("the mixture covariance is rank deficient for ", L, " components")
  Wh <- t(ev$vectors[, seq_len(L), drop = FALSE] /
            rep(sqrt(ev$values[seq_len(L)]), each = K))
  Z <- .morie_bx_mm(Wh, Yc)

  u <- .morie_bx_rng(seed)
  W <- diag(1, L) + .morie_bx_fill(L, L, u, function(z) 0.01 * (z - 0.5))

  it <- 0L
  chg <- NaN
  for (it in seq_len(maxiter)) {
    U <- .morie_bx_mm(W, Z)
    P <- 1 - 2 * .morie_bx_sig(U)
    M <- diag(1, L)
    for (i in seq_len(L)) for (j in seq_len(L))
      M[i, j] <- M[i, j] + .morie_fsum(P[i, ] * U[j, ]) / T
    D <- .morie_bx_mm(M, W)
    chg <- max(abs(D)) * eta
    W <- W + eta * D
    nrm <- max(abs(W))
    if (!is.finite(nrm) || nrm > 1e8)
      stop("Infomax diverged; reduce eta")
    if (chg <= tol) break
  }

  list(sources = .morie_bx_mm(W, Z), unmixing = .morie_bx_mm(W, Wh),
       whitening = Wh, mean = mu, iterations = it, change = chg,
       method = paste0("Infomax ICA with the natural gradient; unmixing ",
                       "model per Rangayyan Biomedical Signal Analysis 3rd ",
                       "ed. eq. (9.44), update from Bell and Sejnowski, ",
                       "Neural Computation 7(6):1129-1159, 1995 and Amari, ",
                       "Cichocki and Yang, NIPS 8:757-763, 1996 (not ",
                       "covered by Rangayyan)"))
}

VagClass <- function(segments, durations = NULL, segclass = NULL,
                     arthro = NULL) {
  # Section 10.12.  VAG signals are nonstationary so each locally
  # stationary segment is classified alone, and the subject-level verdict
  # is a duration-weighted rule over the segments -- one noisy segment must
  # not condemn a normal knee.  Two-step 90%/10% rule of Moussavi et al. as
  # stated in that section.
  S <- .morie_bx_mat(segments, "segments")
  ns <- nrow(S)
  if (is.null(durations)) {
    d <- rep(as.numeric(ncol(S)), ns)
  } else {
    d <- .morie_bx_vec(durations, "durations")
    if (length(d) != ns)
      stop("durations must have one entry per segment")
    if (any(d <= 0)) stop("durations must be positive")
  }
  total <- .morie_fsum(d)

  smeans <- vapply(seq_len(ns), function(i) .morie_bx_mean(S[i, ]),
                   numeric(1))
  vms <- if (ns > 1L) .morie_bx_sd(smeans)^2 else 0

  dec <- NULL; stage <- NULL; abn <- NULL
  fabn <- NaN; fnor <- NaN
  if (!is.null(segclass)) {
    sc <- as.integer(.morie_bx_vec(segclass, "segclass"))
    if (length(sc) != ns)
      stop("segclass must have one entry per segment")
    if (any(!(sc %in% c(0L, 1L))))
      stop("segclass entries must be 0 (normal) or 1 (abnormal)")
    fabn <- .morie_fsum(d[sc == 1L]) / total
    fnor <- 1 - fabn
    if (fnor > 0.90) {
      dec <- "normal"; stage <- 1L; abn <- FALSE
    } else if (fabn > 0.90) {
      dec <- "abnormal"; stage <- 1L; abn <- TRUE
    } else if (is.null(arthro)) {
      dec <- "undecided, four-group classifier required"
      stage <- 1L; abn <- NULL
    } else {
      aa <- as.integer(.morie_bx_vec(arthro, "arthro"))
      if (length(aa) != ns)
        stop("arthro must have one entry per segment")
      faa <- .morie_fsum(d[aa == 1L]) / total
      if (faa > 0.10) {
        dec <- "abnormal"; stage <- 2L; abn <- TRUE
      } else {
        dec <- "normal"; stage <- 2L; abn <- FALSE
      }
    }
  }

  list(varmeans = vms, segmentmeans = smeans, abnormalfraction = fabn,
       normalfraction = fnor, decision = dec, stage = stage, abnormal = abn,
       durations = d,
       method = paste0("VAG cartilage-pathology screening: variance of ",
                       "segment means plus the two-step 90%/10% duration ",
                       "rule, Rangayyan Biomedical Signal Analysis 3rd ed. ",
                       "Section 10.12"))
}

KsvdFit <- function(Y, natoms, sparsity, maxiter = 15, tol = 1e-10,
                    seed = 1) {
  # NOT from Rangayyan: Section 9.5 gives EMD-based dictionary learning
  # (Algorithm 9.1) and cites only the label-consistent variant.  Aharon,
  # Elad and Bruckstein, IEEE TSP 54(11):4311-4322, 2006.  An analytic
  # dictionary represents whatever its functions happen to match; K-SVD
  # re-fits each atom to exactly the signals that currently use it.
  S <- .morie_bx_mat(Y, "Y")
  m <- nrow(S); n <- ncol(S)
  natoms <- as.integer(natoms)
  sparsity <- as.integer(sparsity)
  if (natoms < 1L) stop("natoms must be a positive integer")
  if (!(sparsity >= 1L && sparsity <= natoms))
    stop("sparsity must satisfy 1 <= sparsity <= natoms")
  maxiter <- as.integer(maxiter)
  if (maxiter < 1L) stop("maxiter must be a positive integer")

  u <- .morie_bx_rng(seed)
  D <- matrix(0, natoms, n)
  for (k in seq_len(natoms)) {
    k0 <- k - 1L
    a <- if (k0 < m) S[k0 %% m + 1L, ]
    else vapply(seq_len(n), function(i) u() - 0.5, numeric(1))
    a <- a + vapply(seq_len(n), function(i) 1e-3 * (u() - 0.5), numeric(1))
    nr <- .morie_bx_nrm(a)
    D[k, ] <- if (nr > 1e-12) a / nr else as.numeric(seq_len(n) == k0 %% n + 1L)
  }

  prev <- NULL; err <- NaN; it <- 0L
  Xc <- matrix(0, m, natoms)
  for (it in seq_len(maxiter)) {
    Xc <- t(vapply(seq_len(m), function(i)
      .morie_bx_omp(S[i, ], D, sparsity, 1e-12)$coefficients,
      numeric(natoms)))
    for (k in seq_len(natoms)) {
      users <- which(Xc[, k] != 0)
      if (!length(users)) {
        resnorm <- vapply(seq_len(m), function(i)
          .morie_bx_nrm(S[i, ] - vapply(seq_len(n), function(t)
            .morie_fsum(Xc[i, ] * D[, t]), numeric(1))), numeric(1))
        worst <- which.max(resnorm)
        nr <- .morie_bx_nrm(S[worst, ])
        if (nr > 1e-12) D[k, ] <- S[worst, ] / nr
        next
      }
      E <- t(vapply(users, function(i)
        S[i, ] - vapply(seq_len(n), function(t)
          .morie_fsum(Xc[i, -k] * D[-k, t]), numeric(1)),
        numeric(n)))
      v <- D[k, ]
      for (p in seq_len(30L)) {
        w <- vapply(seq_len(nrow(E)), function(r)
          .morie_bx_dot(E[r, ], v), numeric(1))
        nv <- vapply(seq_len(n), function(t)
          .morie_fsum(w * E[, t]), numeric(1))
        nrm <- .morie_bx_nrm(nv)
        if (nrm <= 1e-14) break
        v <- nv / nrm
      }
      D[k, ] <- v
      for (r in seq_along(users))
        Xc[users[r], k] <- .morie_bx_dot(E[r, ], v)
    }
    resid <- vapply(seq_len(m), function(i)
      S[i, ] - vapply(seq_len(n), function(t)
        .morie_fsum(Xc[i, ] * D[, t]), numeric(1)), numeric(n))
    err <- sqrt(.morie_fsum(as.numeric(resid)^2))
    if (!is.null(prev) && abs(prev - err) <= tol * max(1, prev)) break
    prev <- err
  }

  list(dictionary = D, coefficients = Xc, error = err, iterations = it,
       method = paste0("K-SVD dictionary learning with OMP sparse coding; ",
                       "Aharon, Elad and Bruckstein, IEEE Trans. Signal ",
                       "Processing 54(11):4311-4322, 2006 (not the ",
                       "EMD-based scheme of Rangayyan Section 9.5)"))
}

DictCode <- function(Y, D, sparsity, tol = 1e-12) {
  # Section 9.5 for the greedy stage and 9.8 for the use of the resulting
  # coefficients and reconstruction error as features; the least-squares
  # reprojection is Pati, Rezaiifar and Krishnaprasad, Asilomar 1993, which
  # Rangayyan does not cover.  Learning a dictionary and using one are
  # separate steps: here the coefficients, not the signals, are the
  # feature vectors.
  S <- .morie_bx_mat(Y, "Y")
  A <- .morie_bx_mat(D, "D")
  n <- ncol(S)
  if (ncol(A) != n)
    stop("dictionary atoms must have the same length as the signals")
  sparsity <- as.integer(sparsity)
  if (!(sparsity >= 1L && sparsity <= nrow(A)))
    stop("sparsity must satisfy 1 <= sparsity <= number of atoms")

  coefs <- list(); sups <- list(); recs <- list(); res <- list()
  for (i in seq_len(nrow(S))) {
    r <- .morie_bx_omp(S[i, ], A, sparsity, as.numeric(tol))
    coefs[[i]] <- r$coefficients
    sups[[i]] <- r$support - 1L
    recs[[i]] <- vapply(seq_len(n), function(t)
      .morie_fsum(r$coefficients * A[, t]), numeric(1))
    res[[i]] <- r$residual
  }
  err <- sqrt(.morie_fsum(unlist(res, use.names = FALSE)^2))
  list(coefficients = coefs, support = sups, reconstruction = recs,
       residual = res, error = err,
       method = paste0("sparse coding of signals in a fixed dictionary by ",
                       "orthogonal matching pursuit; greedy framework of ",
                       "Rangayyan Biomedical Signal Analysis 3rd ed. ",
                       "Section 9.5, orthogonalised per Pati, Rezaiifar ",
                       "and Krishnaprasad, Asilomar 1993"))
}

Lstm <- function(sequences, labels = NULL, hidden = 8, ridge = 1e-6,
                 seed = 1, weights = NULL) {
  # NOT from Rangayyan: Section 10.8.2 discusses deep learning in prose and
  # gives no gated-cell equations.  Hochreiter and Schmidhuber, Neural
  # Computation 9(8):1735-1780, 1997 with the forget gate of Gers,
  # Schmidhuber and Cummins, Neural Computation 12(10):2451-2471, 2000.
  # The recurrent weights are FIXED and only the readout is fitted: this is
  # a reservoir-style readout, not back-propagation through time.
  if (!is.list(sequences) || !length(sequences))
    stop("sequences must be a non-empty list of sequences")
  steps <- lapply(sequences, function(s) {
    m <- if (is.matrix(s)) s else matrix(as.numeric(s), ncol = 1L)
    if (!nrow(m)) stop("every sequence must have at least one time step")
    storage.mode(m) <- "double"
    m
  })
  d <- ncol(steps[[1L]])
  if (any(vapply(steps, ncol, integer(1)) != d))
    stop("all time steps must have the same number of inputs")
  H <- as.integer(hidden)
  if (H < 1L) stop("hidden must be a positive integer")

  gates <- c("i", "f", "o", "g")
  if (is.null(weights)) {
    u <- .morie_bx_rng(seed)
    sc <- 1 / sqrt(H + d)
    W <- list()
    for (k in gates)
      W[[k]] <- .morie_bx_fill(H, H + d, u, function(z) sc * (2 * z - 1))
    B <- list(i = numeric(H), f = rep(1, H), o = numeric(H), g = numeric(H))
  } else {
    if (!is.list(weights)) stop("weights must be a list of gate matrices")
    W <- list()
    for (k in gates) {
      if (!(k %in% names(weights))) stop("weights is missing gate ", k)
      M <- .morie_bx_mat(weights[[k]], paste0("weights[", k, "]"))
      if (nrow(M) != H || ncol(M) != H + d)
        stop("weights[", k, "] must be ", H, " x ", H + d)
      W[[k]] <- M
    }
    bb <- .morie_bx_mat(if (is.null(weights$bias))
      matrix(0, 4L, H) else weights$bias, "weights['bias']")
    if (nrow(bb) != 4L || ncol(bb) != H)
      stop("weights['bias'] must be 4 rows of length ", H)
    B <- list(i = bb[1L, ], f = bb[2L, ], o = bb[3L, ], g = bb[4L, ])
  }

  hs <- matrix(0, length(steps), H)
  cs <- matrix(0, length(steps), H)
  for (si in seq_along(steps)) {
    h <- numeric(H); cvec <- numeric(H)
    for (ti in seq_len(nrow(steps[[si]]))) {
      z <- c(h, steps[[si]][ti, ])
      gi <- .morie_bx_sig(.morie_bx_mv(W$i, z) + B$i)
      gf <- .morie_bx_sig(.morie_bx_mv(W$f, z) + B$f)
      go <- .morie_bx_sig(.morie_bx_mv(W$o, z) + B$o)
      gg <- tanh(.morie_bx_mv(W$g, z) + B$g)
      cvec <- gf * cvec + gi * gg
      h <- go * tanh(cvec)
    }
    hs[si, ] <- h
    cs[si, ] <- cvec
  }

  pred <- NULL; acc <- NaN; read <- NULL; classes <- NULL
  if (!is.null(labels)) {
    y <- as.integer(.morie_bx_vec(labels, "labels"))
    if (length(y) != length(steps))
      stop("labels must have one entry per sequence")
    classes <- sort(unique(y))
    if (length(classes) < 2L)
      stop("labels must contain at least two classes")
    A <- cbind(hs, 1)
    read <- t(vapply(classes, function(cc)
      .morie_bx_lstsq(A, as.numeric(y == cc), as.numeric(ridge)),
      numeric(H + 1L)))
    pred <- vapply(seq_along(y), function(i)
      classes[which.max(.morie_bx_mv(read, A[i, ]))], numeric(1))
    acc <- sum(y == pred) / length(y)
  }

  list(hidden = hs, cell = cs, predictions = pred, accuracy = acc,
       readout = read, classes = classes,
       method = paste0("LSTM recurrence with a ridge least-squares readout ",
                       "on the final hidden state; Hochreiter and ",
                       "Schmidhuber, Neural Computation 9(8):1735-1780, ",
                       "1997, with the forget gate of Gers, Schmidhuber ",
                       "and Cummins, Neural Computation 12(10):2451-2471, ",
                       "2000 (not covered by Rangayyan)"))
}

MPursuit <- function(x, dictionary = NULL, natoms = 20, tol = 1e-10,
                     decaystop = NULL) {
  # Section 9.3, eqs (9.1)-(9.7) with the Gabor dictionary of (9.2)-(9.3).
  # A fixed transform imposes one tiling of the time-frequency plane on
  # every signal; MP picks whatever currently matches best, so what it
  # selects are the coherent structures and what is left over may be taken
  # as noise -- which is why the truncated expansion filters.
  x <- .morie_bx_vec(x, "x")
  n <- length(x)
  natoms <- as.integer(natoms)
  if (natoms < 1L) stop("natoms must be a positive integer")
  if (is.null(dictionary)) {
    gb <- .morie_bx_gabor(n, max(64L, 8L * natoms))
    D <- gb$atoms
    params <- gb$params
  } else {
    D <- .morie_bx_mat(dictionary, "dictionary")
    if (ncol(D) != n)
      stop("every atom must have the same length as x")
    params <- vector("list", nrow(D))
    for (j in seq_len(nrow(D))) {
      nr <- .morie_bx_nrm(D[j, ])
      if (nr <= 1e-12) stop("dictionary atoms must have nonzero norm")
      D[j, ] <- D[j, ] / nr
    }
  }
  if (!nrow(D)) stop("the dictionary is empty")

  r <- x
  e_prev <- .morie_fsum(r * r)
  if (e_prev <= 0) stop("x has zero energy")
  e0 <- e_prev
  coef <- numeric(0); idx <- integer(0); decay <- numeric(0)
  for (step in seq_len(min(natoms, nrow(D)))) {
    best <- -1L; bv <- -1
    for (j in seq_len(nrow(D))) {
      if (j %in% idx) next
      v <- abs(.morie_bx_dot(D[j, ], r))
      if (v > bv) { best <- j; bv <- v }
    }
    if (best < 0L) break
    a <- .morie_bx_dot(D[best, ], r)
    r <- r - a * D[best, ]
    coef <- c(coef, a)
    idx <- c(idx, best)
    e_now <- .morie_fsum(r * r)
    lam <- if (e_prev > 0) sqrt(max(0, 1 - e_now / e_prev)) else 0
    decay <- c(decay, lam)
    e_prev <- e_now
    if (e_now <= tol) break
    if (!is.null(decaystop) && lam < as.numeric(decaystop)) break
  }

  rec <- vapply(seq_len(n), function(i)
    .morie_fsum(coef * D[idx, i]), numeric(1))
  list(coefficients = coef, atoms = D[idx, , drop = FALSE],
       indices = idx - 1L, residual = r, reconstruction = rec,
       decay = decay, energyratio = 1 - e_prev / e0,
       parameters = params[idx],
       method = paste0("matching-pursuit decomposition into time-frequency ",
                       "atoms, Rangayyan Biomedical Signal Analysis 3rd ed. ",
                       "Section 9.3, eqs. (9.1)-(9.7) with the Gabor ",
                       "dictionary of eqs. (9.2)-(9.3)"))
}

BmiDec <- function(y, C, a = NULL, procnoise = 1e-4, obsnoise = 1e-2,
                   p0 = 1e-2) {
  # Section 8.18 with the filter of 8.7: a BMI has no ground truth about
  # intended kinematics -- that is exactly what motor impairment removes --
  # so the decoder estimates a hidden state recursively.  Models eqs (8.60)
  # and (8.63), recursion eqs (8.95)-(8.99), initial conditions
  # xtilde(1|Y_0) = 0 and phi_ep(1, 0) = D_0.
  Y <- .morie_bx_mat(y, "y")
  Cm <- .morie_bx_mat(C, "C")
  K <- nrow(Cm); L <- ncol(Cm)
  if (ncol(Y) != K)
    stop("each observation row must have ", K, " entries")
  if (is.null(a)) {
    A <- diag(1, L)
  } else {
    A <- .morie_bx_mat(a, "a")
    if (nrow(A) != L || ncol(A) != L)
      stop("a must be ", L, " x ", L)
  }
  if (as.numeric(p0) <= 0) stop("p0 must be positive")

  cov <- function(arg, k, name) {
    if (is.numeric(arg) && length(arg) == 1L) {
      if (arg <= 0) stop(name, " must be positive")
      return(diag(as.numeric(arg), k))
    }
    M <- .morie_bx_mat(arg, name)
    if (nrow(M) != k || ncol(M) != k)
      stop(name, " must be ", k, " x ", k)
    M
  }
  Qd <- cov(procnoise, L, "procnoise")
  Qo <- cov(obsnoise, K, "obsnoise")

  xh <- numeric(L)
  P <- diag(as.numeric(p0), L)
  Ct <- t(Cm)
  states <- list(); innov <- list(); gains <- list(); preds <- list()
  for (t in seq_len(nrow(Y))) {
    PCt <- .morie_bx_mm(P, Ct)
    Sm <- .morie_bx_mm(Cm, PCt) + Qo
    APCt <- .morie_bx_mm(A, PCt)
    Kg <- vapply(seq_len(K), function(col)
      .morie_bx_solve(Sm, as.numeric(seq_len(K) == col)), numeric(K))
    Kmat <- matrix(0, L, K)
    for (i in seq_len(L)) for (j in seq_len(K))
      Kmat[i, j] <- .morie_fsum(APCt[i, ] * Kg[, j])
    pred <- .morie_bx_mv(Cm, xh)
    z <- Y[t, ] - pred
    preds[[t]] <- pred
    innov[[t]] <- z
    states[[t]] <- xh
    gains[[t]] <- Kmat
    xh <- vapply(seq_len(L), function(i)
      .morie_fsum(A[i, ] * xh) + .morie_fsum(Kmat[i, ] * z), numeric(1))
    KC <- .morie_bx_mm(Kmat, Cm)
    Pf <- matrix(0, L, L)
    for (i in seq_len(L)) for (j in seq_len(L))
      Pf[i, j] <- P[i, j] - .morie_fsum(KC[i, ] * P[, j])
    P <- .morie_bx_mm(.morie_bx_mm(A, Pf), t(A)) + Qd
  }

  list(states = states, innovations = innov, gain = gains,
       predicted = preds,
       method = paste0("Kalman-filter neural decoder for prosthesis ",
                       "control, Rangayyan Biomedical Signal Analysis 3rd ",
                       "ed. Section 8.18 with the recursion of Section 8.7, ",
                       "eqs. (8.60), (8.63) and (8.95)-(8.99)"))
}

NmfMu <- function(V, r, maxiter = 200, tol = 1e-10, cost = "ls", seed = 1) {
  # Section 9.7.3, eqs (9.46), (9.49)-(9.50) and (9.53)-(9.55).  PCA and
  # ICA are free to use negative coefficients so their components cancel;
  # nonnegativity makes the columns of W basis vectors and the rows of H
  # the activations that switch them on.  The book warns the divergence
  # form is undefined where V or WH has a zero.
  M <- .morie_bx_mat(V, "V")
  if (!cost %in% c("ls", "kld")) stop("cost must be 'ls' or 'kld'")
  if (as.integer(maxiter) < 1L)
    stop("maxiter must be a positive integer")
  if (identical(cost, "kld") && any(M <= 0))
    stop("the divergence cost is undefined where V has a zero element; ",
         "use cost='ls'")
  fac <- .morie_bx_nmfmu(M, r, as.integer(maxiter), as.numeric(tol), seed,
                         cost)
  W <- fac$W; H <- fac$H
  subs <- lapply(seq_len(nrow(H)), function(k) outer(W[, k], H[k, ]))
  list(W = W, H = H, submatrices = subs, error = fac$error,
       iterations = fac$iterations, cost = cost,
       method = paste0("nonnegative matrix factorisation by multiplicative ",
                       "updates, Rangayyan Biomedical Signal Analysis 3rd ",
                       "ed. Section 9.7.3, eqs. (9.46), (9.49)-(9.50) and ",
                       "(9.53)-(9.55)"))
}

NmfChSel <- function(trials, nselect, rank = 4, maxiter = 200, tol = 1e-8,
                     seed = 1) {
  # Section 9.12.1, eqs (9.94)-(9.96).  Channel relevance varies strongly
  # between subjects, so a fixed montage is the wrong answer; a row that
  # stays near 0.5 loads uniformly on every factor and carries nothing
  # specific.  Eq (9.96) as printed reuses j as both row and summation
  # index, which cannot be read literally: the per-row RMS deviation over
  # the r factors is the reading consistent with the surrounding text.
  r <- .morie_bx_chsel(trials, nselect, rank, maxiter, tol, seed)
  list(selected = r$selected, rmsd = r$rmsd, ranking = r$ranking,
       normalized = r$normalized, W = r$W, H = r$H, covariance = r$C,
       error = r$error,
       method = paste0("NMF-based EEG channel ranking by normalised ",
                       "basis-row RMS deviation, Rangayyan Biomedical ",
                       "Signal Analysis 3rd ed. Section 9.12.1, ",
                       "eqs. (9.94)-(9.96)"))
}

OmpFit <- function(x, D, sparsity = NULL, tol = 1e-10) {
  # NOT from Rangayyan: Section 9.3 gives plain matching pursuit, not the
  # orthogonalised variant.  Pati, Rezaiifar and Krishnaprasad, Proc. 27th
  # Asilomar Conf., pp. 40-44, 1993.  Re-solving the whole active set after
  # every pick keeps the residue orthogonal to it, so each atom is chosen
  # at most once and k atoms give the best k-term fit on that support.
  x <- .morie_bx_vec(x, "x")
  A <- .morie_bx_mat(D, "D")
  n <- length(x)
  if (ncol(A) != n)
    stop("every dictionary atom must have the same length as x")
  e0 <- .morie_fsum(x * x)
  if (e0 <= 0) stop("x has zero energy")
  r <- .morie_bx_omp(x, A, sparsity, as.numeric(tol))
  rec <- vapply(seq_len(n), function(i)
    .morie_fsum(r$coefficients * A[, i]), numeric(1))
  err <- .morie_bx_nrm(r$residual)
  list(coefficients = r$coefficients, support = r$support - 1L,
       reconstruction = rec, residual = r$residual, error = err,
       energyratio = 1 - (err * err) / e0,
       method = paste0("orthogonal matching pursuit; Pati, Rezaiifar and ",
                       "Krishnaprasad, Proc. 27th Asilomar Conf., ",
                       "pp. 40-44, 1993 (Rangayyan Section 9.3 covers plain ",
                       "matching pursuit)"))
}

PcaSig <- function(X, ncomp = NULL) {
  # Section 9.7.1, eqs (9.37)-(9.41).  Multichannel recordings pick up the
  # same sources through different paths, so the channels are redundant;
  # rotating onto the covariance eigenvectors puts most power in a few
  # components and makes the truncation error the sum of the DISCARDED
  # eigenvalues, which is why decreasing order minimises the MSE.
  Y <- .morie_bx_mat(X, "X")
  K <- nrow(Y); T <- ncol(Y)
  if (T < 2L) stop("need at least two samples per channel")
  L <- if (is.null(ncomp)) K else as.integer(ncomp)
  if (!(L >= 1L && L <= K))
    stop("ncomp must satisfy 1 <= ncomp <= number of channels")

  mu <- vapply(seq_len(K), function(i) .morie_bx_mean(Y[i, ]), numeric(1))
  Yc <- Y - mu
  S <- matrix(0, K, K)
  for (i in seq_len(K)) for (j in seq_len(K))
    S[i, j] <- .morie_fsum(Yc[i, ] * Yc[j, ]) / (T - 1)
  ev <- .morie_bx_jacobi(S)
  W <- t(ev$vectors[, seq_len(L), drop = FALSE])
  P <- .morie_bx_mm(W, Yc)
  tot <- .morie_fsum(pmax(0, ev$values))
  list(components = P, eigenvalues = ev$values, eigenvectors = ev$vectors,
       mean = mu, covariance = S,
       varexplained = if (tot > 0) pmax(0, ev$values[seq_len(L)]) / tot
       else numeric(L),
       mse = if (L < K) .morie_fsum(pmax(0, ev$values[(L + 1L):K])) else 0,
       method = paste0("principal component analysis of signal mixtures by ",
                       "eigendecomposition of the covariance matrix, ",
                       "Rangayyan Biomedical Signal Analysis 3rd ed. ",
                       "Section 9.7.1, eqs. (9.37)-(9.41)"))
}

MixCmp <- function(X, ncomp = NULL, maxiter = 200, seed = 1) {
  # Section 9.7.4.  The three decompositions answer different questions of
  # the same data -- uncorrelated, independent, nonnegative-parts -- and
  # which to use is empirical, so reconstruct with each and measure.  NMF
  # runs on the nonnegatively shifted mixture, as eq (9.46) requires.
  Y <- .morie_bx_mat(X, "X")
  K <- nrow(Y); T <- ncol(Y)
  L <- if (is.null(ncomp)) K else as.integer(ncomp)
  if (!(L >= 1L && L <= K))
    stop("ncomp must satisfy 1 <= ncomp <= number of channels")
  denom <- sqrt(.morie_fsum(as.numeric(t(Y))^2))
  if (denom <= 0) stop("X has zero energy")

  relerr <- function(R) sqrt(.morie_fsum(as.numeric(t((Y - R)^2)))) / denom

  p <- PcaSig(Y, ncomp = L)
  B <- p$eigenvectors[, seq_len(L), drop = FALSE]
  Rp <- .morie_bx_mm(B, p$components) + p$mean

  ic <- IcaFix(Y, ncomp = L, maxiter = maxiter, seed = seed)
  Ri <- .morie_bx_mm(ic$mixing, ic$sources) + ic$mean

  lo <- min(Y)
  V <- if (lo < 0) Y - lo else Y
  nm <- NmfMu(V, L, maxiter = maxiter, seed = seed)
  Rn <- .morie_bx_mm(nm$W, nm$H)
  if (lo < 0) Rn <- Rn + lo

  err <- list(pca = relerr(Rp), ica = relerr(Ri), nmf = relerr(Rn))
  list(error = err, best = names(err)[which.min(unlist(err))],
       components = list(pca = p$components, ica = ic$sources,
                         nmf = list(W = nm$W, H = nm$H)),
       rank = L,
       method = paste0("comparison of PCA, ICA and NMF by relative ",
                       "reconstruction error, Rangayyan Biomedical Signal ",
                       "Analysis 3rd ed. Section 9.7.4"))
}

Rbfn <- function(X, y, ncenters = NULL, spread = 1, centers = NULL,
                 ridge = 1e-8, query = NULL) {
  # Section 10.8.1, eqs (10.86)-(10.87).  Cover's theorem: a set that is
  # not linearly separable becomes so under a nonlinear projection, so a
  # layer of localised Gaussians leaves only a linear output layer and the
  # fit is closed form.  Note the log_e(2) in the book's phi: it is exactly
  # one half at ||x - c|| = sigma, so spread READS as the half-response
  # radius.  The book notes random centres give a needlessly large network,
  # so the default here is greedy forward selection.
  F <- .morie_bx_mat(X, "X")
  tv <- .morie_bx_vec(y, "y")
  if (nrow(F) != length(tv))
    stop("X and y must have the same number of rows")
  n <- nrow(F); p <- ncol(F)
  spread <- as.numeric(spread)
  if (spread <= 0) stop("spread must be positive")
  lg2 <- log(2)
  phi <- function(a, c) exp(-lg2 * .morie_fsum((a - c)^2) / (spread * spread))

  if (!is.null(centers)) {
    Cs <- .morie_bx_mat(centers, "centers")
    if (ncol(Cs) != p)
      stop("centers must have the same dimension as X")
  } else {
    J <- if (is.null(ncenters)) min(8L, n) else as.integer(ncenters)
    if (!(J >= 1L && J <= n))
      stop("ncenters must satisfy 1 <= ncenters <= number of rows")
    chosen <- integer(0)
    resid <- tv
    for (step in seq_len(J)) {
      best <- -1L; bv <- -1
      for (i in seq_len(n)) {
        if (i %in% chosen) next
        col <- vapply(seq_len(n), function(k) phi(F[k, ], F[i, ]),
                      numeric(1))
        den <- .morie_fsum(col * col)
        if (den <= 1e-14) next
        v <- .morie_fsum(col * resid)^2 / den
        if (v > bv) { best <- i; bv <- v }
      }
      if (best < 0L) break
      chosen <- c(chosen, best)
      A <- cbind(vapply(chosen, function(i)
        vapply(seq_len(n), function(k) phi(F[k, ], F[i, ]), numeric(1)),
        numeric(n)), 1)
      w <- .morie_bx_lstsq(A, tv, as.numeric(ridge))
      resid <- tv - .morie_bx_mv(A, w)
    }
    if (!length(chosen))
      stop("no usable centre found; check spread and X")
    Cs <- F[chosen, , drop = FALSE]
  }

  A <- cbind(vapply(seq_len(nrow(Cs)), function(j)
    vapply(seq_len(n), function(k) phi(F[k, ], Cs[j, ]), numeric(1)),
    numeric(n)), 1)
  w <- .morie_bx_lstsq(A, tv, as.numeric(ridge))
  fit <- .morie_bx_mv(A, w)
  mse <- .morie_fsum((tv - fit)^2) / n

  qv <- NULL
  if (!is.null(query)) {
    Q <- .morie_bx_mat(query, "query")
    if (ncol(Q) != p)
      stop("query must have the same dimension as X")
    qv <- vapply(seq_len(nrow(Q)), function(i)
      .morie_fsum(w[seq_len(nrow(Cs))] * vapply(seq_len(nrow(Cs)),
        function(j) phi(Q[i, ], Cs[j, ]), numeric(1))) + w[length(w)],
      numeric(1))
  }

  list(centers = Cs, weights = w[-length(w)], bias = w[length(w)],
       predictions = fit, queryvalues = qv, mse = mse, spread = spread,
       method = paste0("radial basis function network with greedy forward ",
                       "centre selection, Rangayyan Biomedical Signal ",
                       "Analysis 3rd ed. Section 10.8.1, ",
                       "eqs. (10.86)-(10.87)"))
}

Ahi <- function(airflow, fs, spo2 = NULL, hours = NULL, apneafrac = 0.10,
                hypofrac = 0.50, minsec = 10, desat = 0, envsec = 1) {
  # Section 10.13.  Severity is reported as one number, so the whole
  # scoring problem is detect episodes and divide.  Definitions from that
  # section: apnea is total absence of airflow, hypopnea a partial
  # collapse, an episode must last at least 10 s and be linked to a drop in
  # oxygenation, and the bands are 5-15 mild, 15-30 moderate, >30 severe.
  # The book gives NO numeric amplitude or desaturation threshold, so
  # apneafrac, hypofrac and desat are parameters, not hidden constants.
  x <- .morie_bx_vec(airflow, "airflow")
  fs <- as.numeric(fs)
  if (fs <= 0) stop("fs must be a positive sampling rate in Hz")
  minsec <- as.numeric(minsec)
  if (minsec <= 0) stop("minsec must be positive")
  if (!(apneafrac > 0 && apneafrac < hypofrac && hypofrac <= 1))
    stop("need 0 < apneafrac < hypofrac <= 1")
  dur <- length(x) / fs
  hrs <- if (is.null(hours)) dur / 3600 else as.numeric(hours)
  if (hrs <= 0) stop("hours must be positive")
  envsec <- as.numeric(envsec)
  if (envsec <= 0) stop("envsec must be positive")
  ox <- NULL
  if (!is.null(spo2)) {
    ox <- .morie_bx_vec(spo2, "spo2")
    if (length(ox) != length(x))
      stop("spo2 must have the same length as airflow")
  }

  w <- max(1L, as.integer(round(envsec * fs)))
  nx <- length(x)
  env <- vapply(seq_len(nx), function(i)
    max(abs(x[max(1L, i - w):min(nx, i + w)])), numeric(1))
  base <- sort(env)[as.integer(0.75 * (nx - 1L)) + 1L]
  if (base <= 0) stop("airflow has no measurable amplitude")

  need <- as.integer(round(minsec * fs))
  events <- list()
  i <- 1L
  while (i <= nx) {
    if (env[i] < hypofrac * base) {
      j <- i
      while (j <= nx && env[j] < hypofrac * base) j <- j + 1L
      if (j - i >= need) {
        seg <- env[i:(j - 1L)]
        kind <- if (min(seg) < apneafrac * base) "apnea" else "hypopnea"
        ok <- TRUE
        dv <- NULL
        if (!is.null(ox)) {
          pre <- ox[max(1L, i - as.integer(fs * 30)):i]
          dv <- max(pre) - min(ox[i:(j - 1L)])
          ok <- dv > as.numeric(desat)
        }
        if (ok)
          events[[length(events) + 1L]] <-
            list(kind = kind, start = (i - 1L) / fs, end = (j - 1L) / fs,
                 duration = (j - i) / fs, desaturation = dv)
      }
      i <- j
    } else {
      i <- i + 1L
    }
  }

  na <- sum(vapply(events, function(e) e$kind == "apnea", logical(1)))
  nh <- length(events) - na
  index <- (na + nh) / hrs
  sev <- if (index < 5) "normal" else if (index < 15) "mild"
  else if (index < 30) "moderate" else "severe"

  list(ahi = index, severity = sev, apnea = na, hypopnea = nh,
       events = events, hours = hrs, baseline = base, envsec = envsec,
       oxygenchecked = !is.null(ox),
       method = paste0("apnea-hypopnea index from airflow and oximetry ",
                       "with the 10 s minimum episode duration and the ",
                       "mild/moderate/severe bands of Rangayyan Biomedical ",
                       "Signal Analysis 3rd ed. Section 10.13; amplitude ",
                       "and desaturation thresholds are parameters, not ",
                       "values given by that section"))
}

SparseCode <- function(x, D, sparsity = NULL, lam = NULL, maxiter = 2000,
                       tol = 1e-10) {
  # Section 9.5 for the greedy framing -- the book calls it a greedy
  # approximation, taking the best option at each step without regard to
  # the final outcome.  Two ways to ask for "few": bound the atom count
  # (OMP, Pati et al. 1993) or penalise the coefficient magnitudes (lasso,
  # Tibshirani, JRSS B 58(1):267-288, 1996).  Rangayyan presents neither
  # solver; only the plain matching pursuit of Section 9.3.
  if (is.null(sparsity) == is.null(lam))
    stop("give exactly one of sparsity (atom budget) or lam (L1 penalty)")
  x <- .morie_bx_vec(x, "x")
  A <- .morie_bx_mat(D, "D")
  n <- length(x)
  if (ncol(A) != n)
    stop("every dictionary atom must have the same length as x")
  e0 <- .morie_fsum(x * x)
  if (e0 <= 0) stop("x has zero energy")

  if (!is.null(sparsity)) {
    r <- OmpFit(x, A, sparsity = as.integer(sparsity), tol = as.numeric(tol))
    mode <- "omp"
    src <- paste0("orthogonal matching pursuit; Pati, Rezaiifar and ",
                  "Krishnaprasad, Asilomar 1993")
    a <- r$coefficients
  } else {
    r <- BPursuit(x, A, lam = as.numeric(lam), maxiter = as.integer(maxiter),
                  tol = as.numeric(tol))
    mode <- "lasso"
    src <- paste0("lasso by iterative soft thresholding; Tibshirani, JRSS ",
                  "B 58(1):267-288, 1996")
    a <- r$alpha
  }
  err <- .morie_bx_nrm(r$residual)
  list(alpha = a, support = which(a != 0) - 1L,
       reconstruction = r$reconstruction, residual = r$residual,
       error = err, energyratio = 1 - (err * err) / e0, mode = mode,
       method = paste0("sparse representation of a biomedical signal in a ",
                       "learned dictionary, in the greedy-approximation ",
                       "framing of Rangayyan Biomedical Signal Analysis 3rd ",
                       "ed. Section 9.5; solver: ", src))
}

VagTfd <- function(x, fs, natoms = 12, nfreq = 32, ntime = NULL, lag = 12) {
  # Section 9.6 applied to VAG in Section 9.9.  Bilinear TFDs buy
  # resolution with cross-terms; decompose first and the interaction is
  # known, so the cross-term double sum of eq (9.15) is simply left out --
  # that omission is what makes the TFD adaptive rather than smoothed.
  # Features EP (9.79), ESP (9.80), FP (9.81), FSP (9.82).
  x <- .morie_bx_vec(x, "x")
  fs <- as.numeric(fs)
  if (fs <= 0) stop("fs must be a positive sampling rate in Hz")
  n <- length(x)
  nfreq <- as.integer(nfreq)
  lag <- as.integer(lag)
  if (nfreq < 2L || lag < 1L) stop("need nfreq >= 2 and lag >= 1")
  nt <- if (is.null(ntime)) n else as.integer(ntime)
  if (!(nt >= 1L && nt <= n))
    stop("ntime must satisfy 1 <= ntime <= len(x)")

  mp <- MPursuit(x, natoms = natoms)
  coef <- mp$coefficients
  atoms <- mp$atoms
  if (!nrow(atoms)) stop("matching pursuit selected no atoms")

  tidx <- if (nt > 1L)
    as.integer(round((seq_len(nt) - 1L) * (n - 1) / (nt - 1))) else 0L
  mrange <- (-lag):lag
  tfd <- matrix(0, nt, nfreq)
  for (ai in seq_len(nrow(atoms))) {
    g <- atoms[ai, ]
    w2 <- coef[ai]^2
    for (ti in seq_len(nt)) {
      c0 <- tidx[ti]
      p <- c0 + mrange
      q <- c0 - mrange
      ok <- p >= 0 & p < n & q >= 0 & q < n
      prod <- ifelse(ok, g[pmin(pmax(p, 0), n - 1L) + 1L] *
                       g[pmin(pmax(q, 0), n - 1L) + 1L], 0)
      for (fi in seq_len(nfreq)) {
        th <- -2 * pi * (fi - 1L) / (2 * nfreq)
        tfd[ti, fi] <- tfd[ti, fi] +
          w2 * 2 * .morie_fsum(prod * cos(th * 2 * mrange))
      }
    }
  }

  freqs <- (seq_len(nfreq) - 1L) * fs / (2 * nfreq)
  times <- tidx / fs
  ep <- numeric(nt); esp <- numeric(nt); fp <- numeric(nt); fsp <- numeric(nt)
  for (ti in seq_len(nt)) {
    row <- tfd[ti, ]
    e <- .morie_fsum(row) / nfreq
    ep[ti] <- e
    esp[ti] <- sqrt(max(0, .morie_fsum((row - e)^2) / nfreq))
    pos <- pmax(0, row)
    s <- .morie_fsum(pos)
    if (s > 0) {
      f1 <- .morie_fsum(freqs * pos) / s
      fp[ti] <- f1
      fsp[ti] <- sqrt(max(0, .morie_fsum((freqs - f1)^2 * pos) / s))
    }
  }

  list(tfd = tfd, times = times, frequencies = freqs, ep = ep, esp = esp,
       fp = fp, fsp = fsp, coefficients = coef,
       method = paste0("matching-pursuit adaptive TFD of a VAG signal with ",
                       "the EP/ESP/FP/FSP features, Rangayyan Biomedical ",
                       "Signal Analysis 3rd ed. Section 9.6, eq. (9.15) ",
                       "diagonal term, and Section 9.9, eqs. (9.79)-(9.82)"))
}

# pre-policy spellings kept as aliases
morie_rangayyan_ann_mlp <- MlpBp
morie_rangayyan_bundle_branch_block <- Bbb
morie_rangayyan_ecg_bbb_normal <- PvcBayes
morie_rangayyan_bci_nmf <- BciChSel
morie_rangayyan_basis_pursuit <- BPursuit
morie_rangayyan_cad_pipeline <- CadPipe
morie_rangayyan_cnn_signal <- CnnSig
morie_rangayyan_fetal_ecg_single <- FecgNmf
morie_rangayyan_ecg_normal_ectopic <- PvcLinDf
morie_rangayyan_eeg_rhythms <- EegBands
morie_rangayyan_epilepsy_ksvd <- SeizDict
morie_rangayyan_fastica <- IcaFix
morie_rangayyan_ica_artifact <- IcaClean
morie_rangayyan_infomax_ica <- Infomax
morie_rangayyan_knee_classify <- VagClass
morie_rangayyan_ksvd <- KsvdFit
morie_rangayyan_dictionary_sparse <- DictCode
morie_rangayyan_lstm_signal <- Lstm
morie_rangayyan_matching_pursuit <- MPursuit
morie_rangayyan_neural_decode <- BmiDec
morie_rangayyan_nmf <- NmfMu
morie_rangayyan_nmf_channel_sel <- NmfChSel
morie_rangayyan_omp <- OmpFit
morie_rangayyan_pca_signals <- PcaSig
morie_rangayyan_pca_vs_ica <- MixCmp
morie_rangayyan_rbf_network <- Rbfn
morie_rangayyan_sleep_apnea_nmf <- Ahi
morie_rangayyan_sparse_rep <- SparseCode
morie_rangayyan_vag_adaptive_tfd <- VagTfd
