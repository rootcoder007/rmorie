#' morie_linprm
#'
#' A step of the linprm_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param c Coerced to numeric by the body, with \code{as.numeric}.
#' @param A Iterated over elementwise, with \code{lapply}.
#' @param b A vector; its length is taken and its elements indexed.
#' @param tol Passed to \code{<}. Defaults to \code{1e-10}.
#' @param max_iter Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200}.
#' @return A list with \code{x}, \code{y}, \code{s}, \code{iterations}, \code{gap},
#' \code{converged}, \code{mu_history}, \code{primal_residual}, \code{dual_residual}.
#' @export
morie_linprm <- function(c, A, b, tol = 1e-10, max_iter = 200) {
  cv <- as.numeric(c)
  M <- lapply(A, function(row) as.numeric(row))
  bb <- as.numeric(b)
  m <- length(M)
  n <- length(cv)
  if (m == 0 || any(sapply(M, length) != n) || length(bb) != m) {
    stop(sprintf("linprm: A must be %d by %d with a right-hand side of length %d", m, n, m))
  }
  x <- rep(1.0, n)
  s <- rep(1.0, n)
  y <- rep(0.0, m)
  hist <- c()
  chol_step <- function(L, b) {
    nn <- length(b)
    yy <- numeric(nn)
    for (i in seq_len(nn)) {
      s_ <- b[i]
      if (i > 1) {
        for (k in seq_len(i - 1)) {
          s_ <- s_ - L[i, k] * yy[k]
        }
      }
      yy[i] <- s_ / L[i, i]
    }
    xx <- numeric(nn)
    for (i in rev(seq_len(nn))) {
      s_ <- yy[i]
      if (i < nn) {
        for (k in seq.int(i + 1, nn)) {
          s_ <- s_ - L[k, i] * xx[k]
        }
      }
      xx[i] <- s_ / L[i, i]
    }
    xx
  }
  cholesky <- function(MM) {
    nn <- nrow(MM)
    L <- matrix(0, nn, nn)
    for (i in seq_len(nn)) {
      for (j in seq_len(i)) {
        s_ <- MM[i, j]
        if (j > 1) {
          for (k in seq_len(j - 1)) {
            s_ <- s_ - L[i, k] * L[j, k]
          }
        }
        if (i == j) {
          if (s_ <= 1e-14) s_ <- 1e-14
          L[i, i] <- sqrt(s_)
        } else {
          L[i, j] <- s_ / L[j, j]
        }
      }
    }
    L
  }
  ada <- function(M, d) {
    mm <- length(M)
    nn <- length(M[[1]])
    out <- matrix(0, mm, mm)
    for (i in seq_len(mm)) {
      for (j in seq_len(mm)) {
        acc <- 0
        for (k in seq_len(nn)) acc <- acc + M[[i]][k] * d[k] * M[[j]][k]
        out[i, j] <- acc
      }
    }
    out
  }
  for (it in seq_len(as.integer(max_iter))) {
    rp <- sapply(seq_len(m), function(i) {
      acc <- bb[i]
      for (j in seq_len(n)) acc <- acc - M[[i]][j] * x[j]
      acc
    })
    rd <- sapply(seq_len(n), function(j) {
      acc <- cv[j]
      for (i in seq_len(m)) acc <- acc - M[[i]][j] * y[i]
      acc - s[j]
    })
    mu <- sum(x * s) / n
    gap <- sum(x * s)
    pr <- sqrt(sum(rp * rp))
    dr <- sqrt(sum(rd * rd))
    hist <- c(hist, mu)
    if (gap < tol && pr < tol && dr < tol) {
      return(list(x = x, y = y, s = s, iterations = it,
                  gap = gap, primal_residual = pr,
                  dual_residual = dr, converged = TRUE,
                  mu_history = hist))
    }
    d <- x / s
    L <- cholesky(ada(M, d))
    step_fn <- function(r3) {
      t_ <- r3 / s
      rhs <- sapply(seq_len(m), function(i) {
        acc <- rp[i]
        acc <- acc - sum(M[[i]] * t_)
        acc <- acc + sum(M[[i]] * d * rd)
        acc
      })
      dy <- chol_step(L, as.numeric(rhs))
      ds <- sapply(seq_len(n), function(j) rd[j] - sum(sapply(seq_len(m), function(i) M[[i]][j] * dy[i])))
      dx <- t_ - d * ds
      list(dx = dx, dy = dy, ds = ds)
    }
    alpha_fn <- function(v, dv) {
      a_ <- 1.0
      for (j in seq_along(v)) {
        if (dv[j] < 0) a_ <- min(a_, -v[j] / dv[j])
      }
      a_
    }
    xs <- -x * s
    pr1 <- step_fn(xs)
    dxa <- pr1$dx
    dya <- pr1$dy
    dsa <- pr1$ds
    ap <- min(1.0, alpha_fn(x, dxa))
    ad <- min(1.0, alpha_fn(s, dsa))
    mu_aff <- sum((x + ap * dxa) * (s + ad * dsa)) / n
    sigma <- if (mu > 0) (mu_aff / mu)^3 else 0.0
    r3 <- -x * s - dxa * dsa + sigma * mu
    pr2 <- step_fn(r3)
    dx <- pr2$dx
    dy <- pr2$dy
    ds <- pr2$ds
    ap <- min(1.0, 0.99 * alpha_fn(x, dx))
    ad <- min(1.0, 0.99 * alpha_fn(s, ds))
    x <- x + ap * dx
    s <- s + ad * ds
    y <- y + ad * dy
  }
  list(x = x, y = y, s = s, iterations = as.integer(max_iter),
       gap = sum(x * s), converged = FALSE, mu_history = hist,
       primal_residual = NA_real_, dual_residual = NA_real_)
}
