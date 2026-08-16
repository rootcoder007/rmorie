# Proximal gradient and FISTA for composite problems.
# Sources: Beck, A., & Teboulle, M. (2009) "A Fast Iterative
# Shrinkage-Thresholding Algorithm for Linear Inverse Problems",
# SIAM J. Imaging Sciences 2(1), 183-202. Mirroring morie.fn.prxgms:
# ISTA is the plain proximal-gradient step; FISTA is the same step with
# the t_{k+1} = (1+sqrt(1+4t_k^2))/2 extrapolation; the backtracking
# line search is the paper's Sec. 4 on L; the lasso uses
# soft-thresholding with tau = lam / L (not bare tau = 1/L, which
# would solve a different problem).

#' morie_prxgms_soft_threshold
#'
#' A step of the prxgms_native implementation. Called by \code{morie_prxgms_lasso_fista}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v A vector; indexed elementwise.
#' @param tau Numeric; combined arithmetically in the body.
#' @return The value of \code{out}, as built in the body.
#' @export
morie_prxgms_soft_threshold <- function(v, tau) {
  v <- as.numeric(v); tau <- as.numeric(tau)
  out <- v
  pos <- v > tau; neg <- v < -tau
  out[pos] <- v[pos] - tau
  out[neg] <- v[neg] + tau
  out[!pos & !neg] <- 0
  out
}

#' morie_prxgms_prox_gradient
#'
#' A step of the prxgms_native implementation. Called by \code{morie_prxgms_lasso_fista}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param fun Accepted by the signature and not used anywhere in the body.
#' @param grad Accepted by the signature and not used anywhere in the body.
#' @param prox Accepted by the signature and not used anywhere in the body.
#' @param x0 Coerced to numeric by the body, with \code{as.numeric}.
#' @param L Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @param max.iter Coerced to integer by the body, with \code{as.integer}. Defaults to \code{500L}.
#' @param tol Passed to \code{<=}. Defaults to \code{1e-10}.
#' @param accelerate A flag; the body branches on it. Defaults to \code{TRUE}.
#' @param backtrack A flag; the body branches on it. Defaults to \code{FALSE}.
#' @param eta Numeric; combined arithmetically in the body. Defaults to \code{2}.
#' @param g.fun Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @return A list with \code{estimate}, \code{x}, \code{fun}, \code{objective}, \code{iterations}, \code{L}, \code{accelerated}, \code{converged}, \code{method}.
#' @export
morie_prxgms_prox_gradient <- function(fun, grad, prox, x0, L = 1,
                                        max.iter = 500L, tol = 1e-10,
                                        accelerate = TRUE, backtrack = FALSE,
                                        eta = 2, g.fun = NULL) {
  x <- as.numeric(x0); n <- length(x)
  L <- as.numeric(L)
  if (L <= 0) stop(paste0("prox_gradient: L must be positive, got ", L))
  y <- x; t <- 1; prev <- x
  obj <- numeric(0)
  it <- 0L; converged <- FALSE
  for (it in seq_len(as.integer(max.iter))) {
    gy <- as.numeric(grad(y))
    Lk <- L
    if (backtrack) {
      fy <- as.numeric(fun(y))
      for (k in seq_len(60L)) {
        z <- prox(y - gy / Lk, 1 / Lk)
        d <- z - y
        q <- fy + sum(gy * d) + 0.5 * Lk * sum(d^2)
        if (as.numeric(fun(z)) <= q + 1e-15) break
        Lk <- Lk * eta
      }
      L <- Lk
    } else {
      z <- prox(y - gy / Lk, 1 / Lk)
    }
    if (accelerate) {
      t.next <- 0.5 * (1 + sqrt(1 + 4 * t * t))
      w <- (t - 1) / t.next
      y <- z + w * (z - prev)
      t <- t.next
    } else y <- z
    step <- sqrt(sum((z - prev)^2))
    prev <- z
    fz <- as.numeric(fun(z))
    gz <- if (is.null(g.fun)) 0 else as.numeric(g.fun(z))
    obj <- c(obj, fz + gz)
    if (step <= tol) { converged <- TRUE; break }
  }
  list(estimate = prev, x = prev, fun = as.numeric(fun(prev)),
       objective = obj, iterations = as.integer(it),
       L = as.numeric(L), accelerated = accelerate,
       converged = converged,
       method = if (accelerate)
         "FISTA (Beck & Teboulle 2009, eq. 4.1-4.3)"
       else "ISTA (Beck & Teboulle 2009, Sec. 2)")
}

#' morie_prxgms_lasso_fista
#'
#' A step of the prxgms_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A A matrix; passed to \code{as.matrix}.
#' @param b Coerced to numeric by the body, with \code{as.numeric}.
#' @param lam Numeric; combined arithmetically in the body.
#' @param max.iter Passed to \code{morie_prxgms_prox_gradient}. Defaults to \code{500L}.
#' @param tol Passed to \code{morie_prxgms_prox_gradient}. Defaults to \code{1e-10}.
#' @param accelerate Passed to \code{morie_prxgms_prox_gradient}. Defaults to \code{TRUE}.
#' @return The value of \code{res}, as built in the body.
#' @export
morie_prxgms_lasso_fista <- function(A, b, lam, max.iter = 500L, tol = 1e-10,
                                     accelerate = TRUE) {
  Am <- as.matrix(A); bv <- as.numeric(b)
  n.rows <- nrow(Am); p <- ncol(Am)
  lam <- as.numeric(lam)
  f <- function(x) {
    r <- as.numeric(Am %*% x) - bv
    0.5 * sum(r^2)
  }
  g <- function(x) as.numeric(crossprod(Am, Am %*% x - bv))
  # L by power iteration
  v <- rep(1, p)
  L <- 1
  for (k in seq_len(200L)) {
    Av <- as.numeric(Am %*% v)
    u <- as.numeric(crossprod(Am, Av))
    nrm <- sqrt(sum(u^2))
    if (nrm <= 0) break
    v <- u / nrm; L <- nrm
  }
  L <- max(L, 1e-12)
  prox <- function(v, t) morie_prxgms_soft_threshold(v, lam * t)
  g.fun <- function(x) lam * sum(abs(x))
  res <- morie_prxgms_prox_gradient(f, g, prox, rep(0, p), L = L,
                                    max.iter = max.iter, tol = tol,
                                    accelerate = accelerate,
                                    g.fun = g.fun)
  res$lambda <- lam; res$L <- L
  res
}

# house entry point: the package exports one morie_<module>
morie_prxgms <- morie_prxgms_soft_threshold
