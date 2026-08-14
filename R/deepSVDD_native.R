# Support vector data description (Tax & Duin 2004).
# Sources: Tax, D. M. J. & Duin, R. P. W. (2004). Support vector
# data description. Machine Learning, 54(1), 45-66, Eqs. 3-14.

.deep_svdd_kernel <- function(a, b, kern, gamma) {
  if (kern == "linear")
    return(sum(a * b))
  d2 <- sum((a - b)^2)
  exp(-gamma * d2)
}

svdd <- function(X, C = 1.0, kernel = "linear", gamma = 1.0,
                 tol = 1e-10, max_sweeps = 500L) {
  Xv <- lapply(seq_len(nrow(X)), function(i) as.numeric(X[i, ]))
  n <- length(Xv)
  if (n < 2L)
    stop("need at least two objects")
  C <- as.numeric(C)
  if (C < 1.0 / n)
    stop("need C >= 1/n for a feasible dual")
  kern <- tolower(as.character(kernel))
  if (!kern %in% c("linear", "rbf"))
    stop("kernel must be 'linear' or 'rbf'")
  K <- matrix(0, n, n)
  for (i in seq_len(n))
    for (j in seq_len(n))
      K[i, j] <- .deep_svdd_kernel(Xv[[i]], Xv[[j]], kern, gamma)
  alpha <- rep(1.0 / n, n)

  .grad_i <- function(i) {
    K[i, i] - 2.0 * sum(alpha * K[i, ])
  }

  for (sweep in seq_len(as.integer(max_sweeps))) {
    moved <- 0.0
    for (i in 1:(n - 1L)) {
      for (j in (i + 1L):n) {
        s <- alpha[i] + alpha[j]
        lo <- max(0.0, s - C)
        hi <- min(C, s)
        if (hi - lo < 1e-15) next
        denom <- 2.0 * (K[i, i] - 2.0 * K[i, j] + K[j, j])
        gi <- .grad_i(i)
        gj <- .grad_i(j)
        if (denom <= 1e-300) {
          new <- if (gi - gj > 0) hi else lo
        } else {
          new <- alpha[i] + (gi - gj) / denom
        }
        new <- min(max(new, lo), hi)
        delta <- new - alpha[i]
        if (abs(delta) > 1e-16) {
          alpha[i] <- new
          alpha[j] <- s - new
          moved <- max(moved, abs(delta))
        }
      }
    }
    if (moved < tol) break
  }
  sup <- which(alpha > 1e-8)
  boundary <- which(alpha > 1e-8 & alpha < C - 1e-8)
  out <- which(alpha >= C - 1e-8)

  .dist2_i <- function(i) {
    K[i, i] -
      2.0 * sum(alpha * K[i, ]) +
      sum(alpha[sup] %*% t(alpha[sup]) * K[sup, sup])
  }

  if (length(boundary) > 0L) {
    r2s <- vapply(boundary, .dist2_i, numeric(1))
    radius2 <- mean(r2s)
    r2_spread <- max(r2s) - min(r2s)
  } else {
    r2s <- vapply(seq_len(n), .dist2_i, numeric(1))
    radius2 <- max(r2s)
    r2_spread <- 0.0
  }
  viol <- r2_spread
  for (i in seq_len(n)) {
    d2 <- .dist2_i(i)
    if (alpha[i] < 1e-8 && d2 > radius2 + 1e-6)
      viol <- max(viol, d2 - radius2)
    if (alpha[i] >= C - 1e-8 && C < 1.0 && d2 < radius2 - 1e-6)
      viol <- max(viol, radius2 - d2)
  }
  center <- NULL
  if (kern == "linear") {
    d <- length(Xv[[1L]])
    center <- numeric(d)
    for (k in seq_len(d))
      center[k] <- sum(alpha * vapply(Xv, function(x) x[k], numeric(1)))
  }
  list(alpha = alpha,
       center = center,
       radius2 = radius2,
       support = sup,
       outliers = out,
       kkt_violation = viol,
       kernel = kern,
       C = C,
       method = "SVDD (Tax & Duin 2004, Eqs. 6-14)")
}

deepSVDD <- svdd
support_vector_data_description <- svdd
deep_svdd <- svdd
deepsvdd <- svdd

morie_deepSVDD <- svdd

deepSVDD_cheatsheet <- function() {
  "svdd: max sum a K_ii - aa'K, sum a=1, 0<=a<=C; a = center weights"
}
