#' Robust linear discriminant analysis
#'
#' Class centres and the pooled within-class scatter from FAST-MCD fits
#' (\code{\link{Fastm}}) instead of sample moments. The pooled scatter is
#' the sum of h_k S_k over classes divided by (sum h_k - g), the mcd-A
#' pooling of rrcov::Linda; linear scores m_k' W^-1 x - m_k' W^-1 m_k / 2 +
#' log prior_k. Identical to the Python arm \code{morie.fn.robustda}.
#'
#' @param X Numeric matrix.
#' @param y Class labels.
#' @param prior Optional class priors (default class proportions).
#' @param newdata Optional matrix to classify (default X).
#' @return List: classes, centers, pooled_cov, prior, scores, predicted and
#'   apparent_error (resubstitution, when newdata is NULL).
#' @references Hawkins, D. M. and McLachlan, G. J. (1997). High-breakdown
#'   linear discriminant analysis. Journal of the American Statistical
#'   Association 92, 136-143.
#'
#'   Todorov, V. and Filzmoser, P. (2009). An object-oriented framework for
#'   robust multivariate analysis. Journal of Statistical Software 32(3).
#' @examples
#' X <- rbind(c(0, 0), c(1, 0), c(0, 1), c(1, 1), c(0.5, 0.4), c(9, 0), c(0.2, 0.8),
#'            c(5, 5), c(6, 5), c(5, 6), c(6, 6), c(5.5, 5.4), c(5.2, 5.8), c(-9, 9))
#' RobustLda(X, rep(0:1, each = 7))$predicted
#' @export
RobustLda <- function(X, y, prior = NULL, newdata = NULL) {
  X <- as.matrix(X) + 0
  if (length(y) != nrow(X)) stop("X and y must have the same length")
  p <- ncol(X)
  classes <- sort(unique(y))
  g <- length(classes)
  if (g < 2) stop("need at least two classes")
  centers <- list()
  W <- matrix(0, p, p)
  htot <- 0
  for (k in seq_len(g)) {
    fit <- Fastm(X[y == classes[k], , drop = FALSE])
    centers[[k]] <- as.numeric(fit$center)
    htot <- htot + fit$h
    W <- W + fit$h * matrix(unlist(fit$cov), p, p, byrow = is.list(fit$cov))
  }
  W <- W / (htot - g)
  if (is.null(prior)) prior <- vapply(classes, function(cl) sum(y == cl) / length(y), 0)
  prior <- as.numeric(prior)
  coef <- lapply(centers, function(m) solve(W, m))
  const <- vapply(seq_len(g), function(k) -0.5 * .rd_ss(coef[[k]] * centers[[k]]) + log(prior[k]), 0)
  Z <- if (is.null(newdata)) X else as.matrix(newdata) + 0
  scores <- t(apply(Z, 1, function(z) vapply(seq_len(g), function(k) .rd_ss(coef[[k]] * z) + const[k], 0)))
  if (g == 1) scores <- t(scores)
  pred <- classes[apply(scores, 1, which.max)]
  out <- list(classes = classes, centers = centers, pooled_cov = W, prior = prior, scores = unname(scores), predicted = pred)
  if (is.null(newdata)) out$apparent_error <- sum(pred != y) / length(y)
  out
}

.rd_ss <- function(v) {
  s <- 0
  for (a in v) s <- s + a
  s
}
