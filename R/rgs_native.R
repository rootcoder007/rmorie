# R arm of rgs -- the functional linear model Y = int beta(t) X(t) dt by
# principal-component truncation.
#
# Cardot, H., Ferraty, F. & Sarda, P. (1999) "Functional linear model",
# Statistics & Probability Letters 45(1), 11-22,
# doi:10.1016/S0167-7152(99)00036-X.
#
# Mirrors src/morie/fn/rgs.py. The covariance operator is discretised with
# trapezoid weights so the eigenproblem is the one for the INTEGRAL operator
# rather than for the raw matrix; beta is divided by the eigenvalues, so the
# truncation level k is the estimator and not a tuning detail.

.rgs_EPS <- 1e-12

#' .rgs_grid_weights
#'
#' Part of the rgs_native implementation; see the file header for the
#' source it follows.
#'
#' @param n_t See Usage.
#' @return The value of \code{w}, as built in the body.
#' @export
.rgs_grid_weights <- function(n_t) {
  if (n_t < 2L) return(1.0)
  h <- 1.0 / (n_t - 1L)
  w <- rep(h, n_t)
  w[1L] <- 0.5 * h
  w[n_t] <- 0.5 * h
  w
}

#' morie_rgs_functional_regression
#'
#' Part of the rgs_native implementation; see the file header for the
#' source it follows.
#'
#' @param X See Usage.
#' @param Y See Usage.
#' @param basis Defaults to \code{NULL}.
#' @return A list with \code{estimate}, \code{beta}, \code{fitted}, \code{residuals}, \code{k}, \code{eigenvalues}, \code{explained}, \code{scores}, \code{mean_curve}, \code{r_squared}, \code{n}, \code{n_grid}, \code{method}, \code{note}.
#' @export
morie_rgs_functional_regression <- function(X, Y, basis = NULL) {
  Xm <- as.matrix(X); storage.mode(Xm) <- "double"
  y <- as.numeric(Y)
  n <- nrow(Xm); T <- ncol(Xm)
  if (n == 0L) stop("rgs: no curves")
  if (length(y) != n)
    stop(sprintf("rgs: %d curves but %d responses", n, length(y)))
  if (n < 2L)
    stop("rgs: the covariance operator needs at least two curves")
  w <- .rgs_grid_weights(T)

  B <- NULL; kk <- NULL
  if (!is.null(basis)) {
    if (is.matrix(basis) || is.data.frame(basis)) {
      B <- as.matrix(basis); storage.mode(B) <- "double"
      if (nrow(B) != T)
        stop(sprintf("rgs: the basis has %d rows for a grid of %d",
                     nrow(B), T))
    } else {
      kk <- as.integer(basis)
      if (kk < 1L) stop("rgs: the truncation level must be at least 1")
    }
  }

  xbar <- colSums(Xm) / n
  Xc <- sweep(Xm, 2L, xbar, "-")

  C <- crossprod(Xc) / n                       # T x T
  rw <- sqrt(w)
  Cw <- outer(rw, rw) * C
  ev <- eigen(Cw, symmetric = TRUE)
  ord <- order(ev$values, decreasing = TRUE)
  lam <- pmax(ev$values[ord], 0.0)
  U <- ev$vectors[, ord, drop = FALSE]         # eigenvectors are COLUMNS
  denom <- ifelse(rw > .rgs_EPS, rw, 1.0)
  phi <- U / denom                             # phi[, j] is an eigenfunction
  # An eigenvector is defined only up to sign, so the reported SCORES would
  # carry an arbitrary sign unless it is pinned. Make the entry of largest
  # magnitude positive, exactly as the Python arm does.
  for (j in seq_len(ncol(phi))) {
    top <- which.max(abs(phi[, j]))
    if (phi[top, j] < 0) phi[, j] <- -phi[, j]
  }

  total <- sum(lam)
  if (total <= .rgs_EPS)
    stop("rgs: the curves carry no variation to regress on")
  explained <- lam / total

  if (!is.null(B)) kk <- min(ncol(B), T)
  if (is.null(kk)) {
    run <- 0.0; kk <- T
    for (j in seq_len(T)) {
      run <- run + explained[j]
      if (run >= 0.99) { kk <- j; break }
    }
  }
  kk <- max(1L, min(as.integer(kk), T, n - 1L))
  keep <- sum(lam[seq_len(kk)] > .rgs_EPS * total)
  kk <- max(1L, keep)

  scores <- matrix(0.0, n, kk)
  for (j in seq_len(kk))
    scores[, j] <- as.numeric(Xc %*% (phi[, j] * w))

  ybar <- sum(y) / n
  b <- numeric(kk)
  for (j in seq_len(kk))
    b[j] <- (sum(scores[, j] * (y - ybar)) / n) / lam[j]
  beta <- as.numeric(phi[, seq_len(kk), drop = FALSE] %*% b)

  fitted <- ybar + as.numeric(scores %*% b)
  resid <- y - fitted
  sst <- sum((y - ybar) ^ 2)
  sse <- sum(resid ^ 2)
  r2 <- if (sst > .rgs_EPS) 1.0 - sse / sst else 0.0

  list(
    estimate = beta,
    beta = beta,
    fitted = fitted,
    residuals = resid,
    k = as.integer(kk),
    eigenvalues = lam[seq_len(kk)],
    explained = explained[seq_len(kk)],
    scores = lapply(seq_len(n), function(i) as.numeric(scores[i, ])),
    mean_curve = as.numeric(xbar),
    r_squared = r2,
    n = as.integer(n),
    n_grid = as.integer(T),
    method = paste0("functional linear model by principal-component ",
                    "truncation (Cardot, Ferraty & Sarda 1999)"),
    note = paste0("beta is divided by the eigenvalues, so each extra ",
                  "component amplifies a direction the data constrain ",
                  "less -- k is the estimator, not a detail")
  )
}

#' .rgs_cheatsheet
#'
#' Part of the rgs_native implementation; see the file header for the
#' source it follows.
#'
#' @return A character value.
#' @export
.rgs_cheatsheet <- function() {
  paste0("rgs: morie_rgs_functional_regression(X, Y, basis) -> the ",
         "functional linear model Y = int beta(t) X(t) dt by FPC ",
         "truncation (Cardot, Ferraty & Sarda 1999, Stat. Probab. Lett. ",
         "45(1), 11-22)")
}

morie_rgs <- morie_rgs_functional_regression
