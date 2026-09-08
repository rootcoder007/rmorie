# R arm of svyrcq -- survey-weighted quantile regression by
# majorise-minimise. Koenker, R. (2005) Quantile Regression, CUP;
# Hunter, D. R. & Lange, K. (2000) JCGS 9(1), 60-77; Lumley, T. (2010)
# Complex Surveys, Wiley. Mirrors src/morie/fn/svyrcq.py.

.svyrcq_EPS <- 1e-12

#' morie_svyrcq_survey_quantile_regression
#'
#' A step of the svyrcq_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; passed to \code{as.matrix}.
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @param tau Numeric; combined arithmetically in the body. Defaults to \code{0.5}.
#' @param weights Optional; may be \code{NULL}. Coerced to numeric by the body, with
#' \code{as.numeric}.
#' @param add_intercept A flag; the body branches on it. Defaults to \code{TRUE}.
#' @param max_iter Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200}.
#' @param tol Passed to \code{<}. Defaults to \code{1e-10}.
#' @param eps Passed to \code{pmax}. Defaults to \code{1e-06}.
#' @return A list with \code{estimate}, \code{coefficients}, \code{residuals},
#' \code{fitted}, \code{objective}, \code{objective_path},
#' \code{weighted_fraction_below}, \code{tau}, \code{iterations}, \code{converged},
#' \code{n}, \code{p}, \code{sum_weights}, \code{method}, \code{note}.
#' @export
morie_svyrcq_survey_quantile_regression <- function(X, y, tau = 0.5,
                                                    weights = NULL,
                                                    add_intercept = TRUE,
                                                    max_iter = 200,
                                                    tol = 1e-10,
                                                    eps = 1e-6) {
  Xm <- as.matrix(X)
  storage.mode(Xm) <- "double"
  yv <- as.numeric(y)
  n <- nrow(Xm)
  if (n == 0L) stop("svyrcq: no observations")
  if (length(yv) != n)
    stop(sprintf("svyrcq: %d rows but %d responses", n, length(yv)))
  tau <- as.numeric(tau)
  if (!(tau > 0.0 && tau < 1.0))
    stop(sprintf("svyrcq: tau must lie strictly in (0, 1), got %g", tau))
  if (is.null(weights)) {
    w <- rep(1.0, n)
  } else {
    w <- as.numeric(weights)
    if (length(w) != n)
      stop(sprintf("svyrcq: %d rows but %d weights", n, length(w)))
    if (any(w < 0.0)) stop("svyrcq: design weights cannot be negative")
  }
  if (isTRUE(add_intercept)) Xm <- cbind(1.0, Xm)
  p <- ncol(Xm)
  if (n <= p)
    stop(sprintf("svyrcq: %d observations cannot identify %d coefficients",
                 n, p))

  check_loss <- function(beta) {
    u <- yv - as.numeric(Xm %*% beta)
    sum(w * u * (tau - ifelse(u < 0.0, 1.0, 0.0)))
  }
  wls <- function(om, adj) {
    A <- crossprod(Xm * om, Xm)
    scale <- sum(diag(A)) / p
    ridge <- if (scale > .svyrcq_EPS) 1e-10 * scale else 1e-12
    diag(A) <- diag(A) + ridge
    b <- as.numeric(crossprod(Xm, om * yv + adj))
    Lc <- chol(A)
    as.numeric(backsolve(Lc, forwardsolve(t(Lc), b)))
  }

  beta <- wls(w, rep(0.0, n))
  obj <- check_loss(beta)
  it <- 0L
  converged <- FALSE
  for (it in seq_len(as.integer(max_iter))) {
    u <- yv - as.numeric(Xm %*% beta)
    d <- pmax(abs(u), eps)
    om <- w / (2.0 * d)
    adj <- w * (tau - 0.5)
    new <- wls(om, adj)
    shift <- max(abs(new - beta))
    beta <- new
    obj <- c(obj, check_loss(beta))
    if (shift < tol) { converged <- TRUE
    break }
  }

  res <- yv - as.numeric(Xm %*% beta)
  below <- sum(w[res < 0.0])
  tot <- sum(w)
  list(estimate = beta, coefficients = beta, residuals = res,
       fitted = yv - res,
       objective = obj[length(obj)], objective_path = obj,
       weighted_fraction_below = if (tot > .svyrcq_EPS) below / tot else 0.0,
       tau = tau, iterations = as.integer(it), converged = converged,
       n = as.integer(n), p = as.integer(p), sum_weights = tot,
       method = paste0("survey-weighted quantile regression by ",
                       "majorise-minimise (Koenker 2005; Hunter & Lange ",
                       "2000; Lumley 2010)"),
       note = paste0("the majorising quadratic touches the check loss at ",
                     "the current fit, so the objective cannot increase -- ",
                     "objective_path makes that checkable rather than ",
                     "asserted"))
}

#' .svyrcq_cheatsheet
#'
#' A step of the svyrcq_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
#' @examples
#' res <- .svyrcq_cheatsheet()
#' res
.svyrcq_cheatsheet <- function() {
  paste0("svyrcq: morie_svyrcq_survey_quantile_regression(X, y, tau, ",
         "weights) -> design-weighted quantile regression by MM ",
         "(Koenker 2005; Lumley 2010)")
}

morie_svyrcq <- morie_svyrcq_survey_quantile_regression
