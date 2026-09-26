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
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_prxgms_soft_threshold(V, V)
#' @keywords internal
morie_prxgms_soft_threshold <- function(v, tau) {
  v <- as.numeric(v)
  tau <- as.numeric(tau)
  out <- v
  pos <- v > tau
  neg <- v < -tau
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
#' @return A list with \code{estimate}, \code{x}, \code{fun}, \code{objective},
#' \code{iterations}, \code{L}, \code{accelerated}, \code{converged}, \code{method}.
#' @export
#' @keywords internal
morie_prxgms_prox_gradient <- function(fun, grad, prox, x0, L = 1,
                                        max.iter = 500L, tol = 1e-10,
                                        accelerate = TRUE, backtrack = FALSE,
                                        eta = 2, g.fun = NULL) {
  x <- as.numeric(x0)
  n <- length(x)
  L <- as.numeric(L)
  if (L <= 0) stop(paste0("prox_gradient: L must be positive, got ", L))
  y <- x
  t <- 1
  prev <- x
  obj <- numeric(0)
  it <- 0L
  converged <- FALSE
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
    if (step <= tol) { converged <- TRUE
    break }
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
#' @examples
#' morie_prxgms_lasso_fista(A = c(1, 2, 3, 4, 5, 6, 7, 8), b = 5L, lam = 5L)
#' @keywords internal
morie_prxgms_lasso_fista <- function(A, b, lam, max.iter = 500L, tol = 1e-10,
                                     accelerate = TRUE) {
  Am <- as.matrix(A)
  bv <- as.numeric(b)
  n.rows <- nrow(Am)
  p <- ncol(Am)
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
    v <- u / nrm
    L <- nrm
  }
  L <- max(L, 1e-12)
  prox <- function(v, t) morie_prxgms_soft_threshold(v, lam * t)
  g.fun <- function(x) lam * sum(abs(x))
  res <- morie_prxgms_prox_gradient(f, g, prox, rep(0, p), L = L,
                                    max.iter = max.iter, tol = tol,
                                    accelerate = accelerate,
                                    g.fun = g.fun)
  res$lambda <- lam
  res$L <- L
  res
}

# house entry point: the package exports one morie_<module>
morie_prxgms <- morie_prxgms_soft_threshold

# -- restored: morie-only definition kept through the rmorie sync --
#' Lasso via ISTA / FISTA
#'
#' Minimises \code{1/2 ||Ax - b||^2 + lam * ||x||_1}; the prox is
#' soft-thresholding at \code{lam * t}, not at \code{t}.
#'
#' @param A Numeric matrix.
#' @param b Numeric vector.
#' @param lam Non-negative penalty.
#' @param max_iter Integer, maximum iterations.
#' @param tol Stop when the step is at most \code{tol}.
#' @param accelerate If \code{TRUE} run FISTA, otherwise ISTA.
#' @return A list with \code{estimate}, \code{x}, \code{fun},
#'   \code{objective}, \code{iterations}, \code{L}, \code{accelerated},
#'   \code{converged}, \code{method}, \code{lambda}, \code{L}.
#' @export
lasso_fista <- function(A, b, lam, max_iter = 500L, tol = 1e-10,
                        accelerate = TRUE) {
  Am <- as.matrix(A)
  storage.mode(Am) <- "double"
  bv <- as.numeric(b)
  n_rows <- nrow(Am)
  p <- ncol(Am)
  lam <- as.numeric(lam)
  f <- function(x) {
    r <- as.numeric(Am %*% x) - bv
    0.5 * sum(r^2)
  }
  g <- function(x) {
    r <- as.numeric(Am %*% x) - bv
    as.numeric(crossprod(Am, r))
  }
  # Power iteration for the largest eigenvalue of A'A
  v <- rep(1.0, p)
  L <- 1.0
  for (it in seq_len(200L)) {
    Av <- as.numeric(Am %*% v)
    u <- as.numeric(crossprod(Am, Av))
    nrm <- sqrt(sum(u^2))
    if (nrm <= 0) break
    v <- u / nrm
    L <- nrm
  }
  L <- max(L, 1e-12)
  prox <- function(v, t) soft_threshold(v, lam * t)
  res <- .morie_prxgms_pgm(f, g, prox, rep(0.0, p), L = L,
                      max_iter = max_iter, tol = tol,
                      accelerate = accelerate,
                      g_fun = function(x) lam * sum(abs(x)))
  res$lambda <- lam
  res$L <- L
  res
}

# -- restored: morie-only definition kept through the rmorie sync --
#' prxgms cheatsheet
#'
#' One-paragraph summary of the method and the traps in using it.
#' @return A character string.
#' @examples
#' prxgms_cheatsheet()
#' @export
prxgms_cheatsheet <- function() {
  paste0("prxgms: ISTA/FISTA, x = prox_{g/L}(x - grad f / L), ",
         "t_{k+1} = (1+sqrt(1+4t^2))/2, ",
         "y = x + (t-1)/t_next (x - x_prev); ",
         "soft threshold for the lasso.")
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Soft-thresholding
#'
#' Prox of \code{tau * |.|_1}, applied elementwise.
#'
#' @param v Numeric vector.
#' @param tau Non-negative threshold.
#' @return Numeric vector of the same length.
#' @export
soft_threshold <- function(v, tau) {
  v <- as.numeric(v)
  out <- numeric(length(v))
  for (i in seq_along(v)) {
    x <- v[i]
    out[i] <- if (x > tau) x - tau
              else if (x < -tau) x + tau
              else 0.0
  }
  out
}

# -- restored: morie-only objects kept through the rmorie sync --
prxgms <- morie_prxgms

# -- restored: pre-sync proximal-gradient solver (morie_prxgms in the old
# morie arm; rmorie's morie_prxgms is the soft-threshold alias) --
#' @noRd
.morie_prxgms_pgm <- function(fun, grad, prox, x0, L = 1.0, max_iter = 500L,
                         tol = 1e-10, accelerate = TRUE, backtrack = FALSE,
                         eta = 2.0, g_fun = NULL) {
  x <- as.numeric(x0)
  n <- length(x)
  L <- as.numeric(L)
  if (L <= 0)
    stop(sprintf("prox_gradient: L must be positive, got %r", L))
  y <- x
  t <- 1.0
  prev <- x
  obj <- numeric(0)
  it <- 0L
  converged <- FALSE
  Lk <- L
  for (it in seq_len(as.integer(max_iter))) {
    gy <- as.numeric(grad(y))
    if (backtrack) {
      fy <- as.numeric(fun(y))
      Lk <- L
      for (bt in seq_len(60L)) {
        z <- prox(y - gy / Lk, 1.0 / Lk)
        d <- z - y
        q <- fy + sum(gy * d) + 0.5 * Lk * sum(d^2)
        if (as.numeric(fun(z)) <= q + 1e-15) break
        Lk <- Lk * eta
      }
      L <- Lk
    } else {
      z <- prox(y - gy / Lk, 1.0 / Lk)
    }
    if (accelerate) {
      t_next <- 0.5 * (1.0 + sqrt(1.0 + 4.0 * t * t))
      w <- (t - 1.0) / t_next
      y <- z + w * (z - prev)
      t <- t_next
    } else {
      y <- z
    }
    step <- sqrt(sum((z - prev)^2))
    prev <- z
    fz <- as.numeric(fun(z))
    obj <- c(obj, fz + if (!is.null(g_fun)) as.numeric(g_fun(z)) else 0.0)
    if (step <= tol) {
      converged <- TRUE
      break
    }
  }
  list(estimate = prev, x = prev, fun = as.numeric(fun(prev)),
       objective = obj, iterations = as.integer(it),
       L = as.numeric(L), accelerated = isTRUE(accelerate),
       converged = converged,
       method = if (accelerate)
         "FISTA (Beck & Teboulle 2009, eq. 4.1-4.3)"
         else "ISTA (Beck & Teboulle 2009, Sec. 2)")
}

# -- restored: pre-sync definition (prox_gradient) --
#' @noRd
prox_gradient <- .morie_prxgms_pgm

# -- restored: pre-sync definition (proximal_gradient_method) --
#' @noRd
proximal_gradient_method <- .morie_prxgms_pgm
