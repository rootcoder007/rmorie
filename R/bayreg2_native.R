# R arm of bayreg2 -- Student-t linear model by EM as a scale mixture of
# normals. West, M. (1984) JRSS B 46(3), 431-439; Geweke, J. (1993) J. Appl.
# Econometrics 8(S1), S19-S40; Lange, Little & Taylor (1989) JASA 84(408),
# 881-896. Mirrors src/morie/fn/bayreg2.py.

.bayreg2_EPS <- 1e-12

#' morie_bayreg2_student_t_regression
#'
#' Part of the bayreg2_native implementation; see the file header for
#' the source it follows.
#'
#' @param X See Usage.
#' @param y See Usage.
#' @param nu Defaults to \code{4}.
#' @param max_iter Defaults to \code{200}.
#' @param tol Defaults to \code{1e-10}.
#' @param add_intercept Defaults to \code{TRUE}.
#' @return A list with \code{estimate}, \code{coefficients}, \code{std_error}, \code{weights}, \code{residuals}, \code{scale2}, \code{fitted}, \code{iterations}, \code{converged}, \code{nu}, \code{loglik}, \code{n}, \code{p}, \code{method}, \code{note}.
#' @export
morie_bayreg2_student_t_regression <- function(X, y, nu = 4.0,
                                               max_iter = 200, tol = 1e-10,
                                               add_intercept = TRUE) {
  Xm <- as.matrix(X); storage.mode(Xm) <- "double"
  yv <- as.numeric(y)
  n <- nrow(Xm)
  if (n == 0L) stop("bayreg2: no observations")
  if (length(yv) != n)
    stop(sprintf("bayreg2: %d rows but %d responses", n, length(yv)))
  if (isTRUE(add_intercept)) Xm <- cbind(1.0, Xm)
  p <- ncol(Xm)
  nu <- as.numeric(nu)
  if (nu <= 0.0) stop("bayreg2: the degrees of freedom must be positive")
  if (n <= p)
    stop(sprintf("bayreg2: %d observations cannot identify %d coefficients",
                 n, p))

  wls <- function(w) {
    A <- crossprod(Xm * w, Xm)
    diag(A) <- diag(A) + 1e-12
    b <- as.numeric(crossprod(Xm, w * yv))
    Lc <- chol(A)
    list(beta = as.numeric(backsolve(Lc, forwardsolve(t(Lc), b))), A = A)
  }

  w <- rep(1.0, n)
  f <- wls(w); beta <- f$beta; A <- f$A
  s2 <- 1.0; it <- 0L; converged <- FALSE
  for (it in seq_len(as.integer(max_iter))) {
    res <- yv - as.numeric(Xm %*% beta)
    s2 <- sum(w * res * res) / n
    if (s2 <= .bayreg2_EPS) s2 <- .bayreg2_EPS
    w_new <- (nu + 1.0) / (nu + res * res / s2)
    f <- wls(w_new)
    shift <- max(abs(f$beta - beta))
    beta <- f$beta; A <- f$A; w <- w_new
    if (shift < tol) { converged <- TRUE; break }
  }

  res <- yv - as.numeric(Xm %*% beta)
  Lc <- chol(A)
  cov <- matrix(0.0, p, p)
  for (a in seq_len(p)) {
    e <- numeric(p); e[a] <- 1.0
    cov[, a] <- as.numeric(backsolve(Lc, forwardsolve(t(Lc), e)))
  }
  se <- sqrt(pmax(s2 * diag(cov), 0.0))
  z <- res * res / s2
  loglik <- sum(lgamma((nu + 1.0) / 2.0) - lgamma(nu / 2.0)
                - 0.5 * log(pi * nu * s2)
                - (nu + 1.0) / 2.0 * log1p(z / nu))

  list(estimate = beta, coefficients = beta, std_error = se,
       weights = w, residuals = res, scale2 = s2,
       fitted = yv - res, iterations = as.integer(it),
       converged = converged, nu = nu, loglik = loglik,
       n = as.integer(n), p = as.integer(p),
       method = paste0("Student-t linear model by EM as a scale mixture of ",
                       "normals (West 1984; Geweke 1993; Lange, Little & ",
                       "Taylor 1989)"),
       note = paste0("the weight (nu+1)/(nu+r^2/s^2) is chosen by the ",
                     "model, not by a threshold; as nu grows every weight ",
                     "tends to 1 and the fit returns to least squares"))
}

#' .bayreg2_cheatsheet
#'
#' Part of the bayreg2_native implementation; see the file header for
#' the source it follows.
#'
#' @return A character value.
#' @export
.bayreg2_cheatsheet <- function() {
  paste0("bayreg2: morie_bayreg2_student_t_regression(X, y, nu) -> robust ",
         "regression by EM on the Student-t scale mixture (West 1984)")
}

morie_bayreg2 <- morie_bayreg2_student_t_regression
