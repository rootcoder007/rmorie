# SPDX-License-Identifier: AGPL-3.0-or-later
#' Flexible discriminant analysis by optimal scoring
#'
#' Regress the class-indicator matrix on (1, h(x)), take the optimal scores
#' from the eigen-decomposition of Y'Y_hat / N relative to D_pi (Theta' D_pi
#' Theta = I, the constant score dropped), and classify the discriminant
#' functions eta(x) = Theta' eta*(x) to the nearest centroid with weights
#' 1 / (alpha^2 (1 - alpha^2)) (ESL sec 12.5.1, as mda::fda). The linear basis
#' gives LDA.
#'
#' @param X Predictors, N by p.
#' @param g Class labels.
#' @param query Points to classify (default X).
#' @param basis Optional function expanding a row.
#' @return Named list: eigenvalues, scores, variates, centroids, prediction,
#'   classes.
#' @references Hastie, T., Tibshirani, R. & Buja, A. (1994). JASA 89, 1255-1270.
#' @examples
#' i <- 1:60
#' X <- cbind(sin(i) + 0.2 * (i %% 3), cos(3 * i) + 0.4 * (i %% 3))
#' morie_esl_fda(X, i %% 3)$eigenvalues
#' @export
morie_esl_fda <- function(X, g, query = NULL, basis = NULL) {
  h <- if (is.null(basis)) identity else basis
  X <- as.matrix(X)
  Hx <- t(apply(X, 1, h))
  if (ncol(X) == 1 && is.null(basis)) Hx <- t(Hx)
  cl <- sort(unique(g))
  N <- nrow(X)
  K <- length(cl)
  if (length(g) != N || K < 2) stop("need X and g of equal length and at least two classes", call. = FALSE)
  D <- cbind(1, Hx)
  Y <- outer(g, cl, "==") * 1
  B <- qr.coef(qr(D), Y)
  Yh <- D %*% B
  pi <- colSums(Y) / N
  Dm <- diag(1 / sqrt(pi), K)
  M <- Dm %*% (crossprod(Y, Yh) / N) %*% Dm
  e <- eigen((M + t(M)) / 2, symmetric = TRUE)
  Th <- (Dm %*% e$vectors)[, -1, drop = FALSE]
  vals <- e$values[-1]
  ft <- Yh %*% Th
  cent <- t(vapply(cl, function(k) colMeans(ft[g == k, , drop = FALSE]), numeric(K - 1)))
  if (K == 2) cent <- t(cent)
  w <- ifelse(vals > 0 & vals < 1, 1 / (vals * (1 - vals)), 0)
  Q <- if (is.null(query)) Hx else {
    qh <- t(apply(rbind(query), 1, h))
    if (ncol(rbind(query)) == 1 && is.null(basis)) t(qh) else qh
  }
  et <- cbind(1, Q) %*% B %*% Th
  d2 <- vapply(seq_len(K), function(k) colSums(w * (t(et) - cent[k, ])^2), numeric(nrow(et)))
  d2 <- matrix(d2, nrow(et))
  list(eigenvalues = vals, scores = Th, variates = et, centroids = cent,
       prediction = cl[max.col(-d2, ties.method = "first")], classes = cl)
}
