# SPDX-License-Identifier: AGPL-3.0-or-later
#' Accelerated projected gradient on a box
#'
#' FISTA with the proximal map of a box indicator, i.e. the component-wise
#' clamp: x_k = clamp(y_k - grad f(y_k)/L),
#' t_\{k+1\} = (1 + sqrt(1 + 4 t_k^2))/2,
#' y_\{k+1\} = x_k + ((t_k-1)/t_\{k+1\})(x_k - x_\{k-1\}).
#'
#' @param X Design matrix.
#' @param y Response of length n.
#' @param lower,upper Box bounds; NULL means unbounded on that side.
#' @param steps Fixed iteration count.
#' @param lipschitz L; NULL uses a fixed 50-step power iteration on X'X.
#'
#' @return List with beta, objective, lipschitz, steps, nactive, n, p.
#' @references Beck and Teboulle (2009), SIAM J. Imaging Sci. 2(1),
#'   183-202.  Standard published form; the SIAM article is paywalled and
#'   was not read.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Agdproj(V, V)
Agdproj <- function(X, y, lower = NULL, upper = NULL, steps = 100,
                    lipschitz = NULL) {
  Xm <- .t1_mat(X); y <- .t1_vec(y); steps <- as.integer(steps)
  n <- nrow(Xm); p <- ncol(Xm)
  if (n != length(y)) stop("X must have one row per entry of y")
  bnd <- function(b, d) {
    if (is.null(b)) return(rep(d, p))
    v <- .t1_vec(b)
    if (length(v) == 1L) return(rep(v, p))
    if (length(v) != p) stop("bound must be scalar or of length p")
    v
  }
  lo <- bnd(lower, -Inf); hi <- bnd(upper, Inf)
  if (any(lo > hi)) stop("lower must not exceed upper")
  L <- if (is.null(lipschitz)) .k01_speclip(Xm, p) else as.numeric(lipschitz)
  if (L <= 0) L <- 1
  x <- pmin(pmax(0, lo), hi); yv <- x; t <- 1
  for (k in seq_len(steps)) {
    g <- as.numeric(t(Xm) %*% (as.numeric(Xm %*% yv) - y))
    xn <- pmin(pmax(yv - g / L, lo), hi)
    tn <- (1 + sqrt(1 + 4 * t * t)) / 2
    w <- (t - 1) / tn
    yv <- xn + w * (xn - x)
    x <- xn
    t <- tn
  }
  res <- as.numeric(Xm %*% x) - y
  .t1_result(beta = x, objective = 0.5 * sum(res^2), lipschitz = L,
             steps = steps, nactive = sum(x == lo | x == hi), n = n, p = p,
             method = "Accelerated projected gradient on a box (Beck-Teboulle 2009)")
}
