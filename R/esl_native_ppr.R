# SPDX-License-Identifier: AGPL-3.0-or-later
.esl11_ridge_eval <- function(term, t) {
  u <- term$knots
  f <- term$values
  g <- term$gamma
  m <- length(u)
  if (m == 1) return(c(f[1], 0))
  if (t <= u[1]) {
    h <- u[2] - u[1]
    d <- (f[2] - f[1]) / h - h * (2 * g[1] + g[2]) / 6
    return(c(f[1] + d * (t - u[1]), d))
  }
  if (t >= u[m]) {
    h <- u[m] - u[m - 1]
    d <- (f[m] - f[m - 1]) / h + h * (g[m - 1] + 2 * g[m]) / 6
    return(c(f[m] + d * (t - u[m]), d))
  }
  k <- findInterval(t, u)
  h <- u[k + 1] - u[k]
  a <- t - u[k]
  b <- u[k + 1] - t
  c((a * f[k + 1] + b * f[k]) / h - a * b / 6 * ((1 + a / h) * g[k + 1] + (1 + b / h) * g[k]),
    (f[k + 1] - f[k]) / h + ((3 * a * a - h * h) * g[k + 1] - (3 * b * b - h * h) * g[k]) / (6 * h))
}

.esl11_fit_ridge <- function(X, r, omega, penalty) {
  s <- .esl9_spline_smooth(drop(X %*% omega), r, penalty, rep(1, nrow(X)), full = TRUE)
  list(term = list(omega = omega, knots = s$u, values = s$f, gamma = s$gamma), fit = s$fit)
}

#' Projection pursuit regression
#'
#' Friedman-Tukey projection pursuit regression as in ESL Sec 11.2 (eqs
#' 11.1-11.4): terms are added forward stage-wise; each alternates a cubic
#' smoothing spline of the residual on the projection with the Gauss-Newton
#' direction update (weighted least squares with weights g'^2, solved as the
#' equivalent regression of g' v + r - g on g' x), halving the step if the
#' residual sum of squares rises. The ridge functions are readjusted by
#' backfitting after each new term; the directions are not.
#'
#' @param X Numeric matrix, N by p.
#' @param y Response vector.
#' @param M Number of ridge terms.
#' @param penalty Smoothing-spline penalty (> 0) on the projection scale.
#' @param max_iter,tol Gauss-Newton controls per term.
#' @param backfit Maximum backfitting passes after each term.
#' @param newdata Optional matrix of points to predict.
#' @return Named list: omega, fitted, residuals, rss, rss_path, intercept,
#'   terms, predicted.
#' @references Friedman, J. H. & Tukey, J. W. (1974). IEEE Trans. Computers
#'   C-23, 881-890.
#' @examples
#' X <- cbind((0:20) / 5 - 2, ((7 * (0:20)) %% 11) / 5 - 1)
#' morie_esl_projection_pursuit(X, rowSums(X)^2, M = 1, penalty = 0.01)$omega
#' @export
morie_esl_projection_pursuit <- function(X, y, M = 2, penalty = 1, max_iter = 50, tol = 1e-8, backfit = 10,
                                         newdata = NULL) {
  X <- as.matrix(X)
  n <- nrow(X)
  if (n < 4 || length(y) != n || M < 1 || penalty <= 0) {
    stop("need N >= 4 matching rows, M >= 1 and penalty > 0", call. = FALSE)
  }
  ybar <- mean(y)
  r <- y - ybar
  terms <- list()
  fits <- list()
  path <- numeric(0)
  rss_of <- function(a) sum(a^2)
  for (step_m in seq_len(M)) {
    omega <- qr.coef(qr(scale(X, scale = FALSE)), r)
    omega <- if (sum(omega^2) > 0) omega / sqrt(sum(omega^2)) else c(1, rep(0, ncol(X) - 1))
    cur <- .esl11_fit_ridge(X, r, omega, penalty)
    rss <- rss_of(r - cur$fit)
    for (it in seq_len(max_iter)) {
      v <- drop(X %*% omega)
      gd <- vapply(v, function(t) .esl11_ridge_eval(cur$term, t), numeric(2))
      new <- qr.coef(qr(gd[2, ] * X), gd[2, ] * v + r - gd[1, ])
      step <- 1
      ok <- FALSE
      for (h in 0:10) {
        cand <- omega + step * (new - omega)
        cand <- cand / sqrt(sum(cand^2))
        cc <- .esl11_fit_ridge(X, r, cand, penalty)
        crss <- rss_of(r - cc$fit)
        if (crss <= rss) {
          ok <- TRUE
          break
        }
        step <- step / 2
      }
      if (!ok) break
      moved <- 1 - abs(sum(cand * omega))
      omega <- cand
      cur <- cc
      rss <- crss
      if (moved < tol) break
    }
    k <- length(terms) + 1
    terms[[k]] <- cur$term
    fits[[k]] <- cur$fit
    r <- r - cur$fit
    if (k > 1) {
      for (pass in seq_len(backfit)) {
        change <- 0
        for (m in seq_len(k)) {
          part <- r + fits[[m]]
          nf <- .esl11_fit_ridge(X, part, terms[[m]]$omega, penalty)
          change <- max(change, abs(nf$fit - fits[[m]]))
          terms[[m]] <- nf$term
          fits[[m]] <- nf$fit
          r <- part - nf$fit
        }
        if (change < 1e-10) break
      }
    }
    path <- c(path, sum(r^2))
  }
  pred <- NULL
  if (!is.null(newdata)) {
    newdata <- as.matrix(newdata)
    pred <- ybar + rowSums(vapply(terms, function(tm) {
      vapply(drop(newdata %*% tm$omega), function(t) .esl11_ridge_eval(tm, t)[1], numeric(1))
    }, numeric(nrow(newdata))))
  }
  list(omega = lapply(terms, `[[`, "omega"), fitted = unname(y - r), residuals = unname(r), rss = path[M],
       rss_path = path, intercept = ybar, terms = terms, predicted = pred)
}
