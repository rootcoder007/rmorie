# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Discrete-time link-function survival regression (Survlnk) and the
# Wolbers competing-risks concordance (Sscompv). Bit-identical mirrors
# of src/morie/fn/survlnk.py and sscompv.py.

#' Discrete-time survival regression with a chosen hazard link
#'
#' Observed event times define discrete risk intervals; subject i at
#' risk at event time t_k contributes a Bernoulli observation with
#' hazard g^{-1}(alpha_k + beta' x_i). cloglog gives the
#' Prentice-Gloeckler grouped proportional-hazards model, logit gives
#' Cox's discrete proportional-odds model. Klein & Moeschberger (2003),
#' Survival Analysis 2nd ed., Section 8.4.
#'
#' @param time Observed times.
#' @param event Event indicator (1 = event, 0 = censored).
#' @param X Covariate matrix.
#' @param link "cloglog" or "logit".
#' @param max_iter,tol Newton controls.
#' @return List with \code{estimate}, \code{se}, \code{alpha},
#'   \code{event_times}, \code{loglik}, \code{n_iter}, \code{link}.
#' @references Klein, J. P. and Moeschberger, M. L. (2003), Survival
#'   Analysis, 2nd ed., Springer, Section 8.4.
#' @export
#' @examples
#' Survlnk(time = c(1, 2, 3, 4, 5, 6, 7, 8), event = c(0, 1, 0, 1, 1, 0, 1, 0), X = c(1, 2, 3, 4, 5, 6, 7, 8))
Survlnk <- function(time, event, X, link = "cloglog",
                    max_iter = 100L, tol = 1e-10) {
  tt <- as.numeric(time); e <- as.numeric(event)
  Xm <- as.matrix(X); storage.mode(Xm) <- "double"
  if (nrow(Xm) != length(tt)) Xm <- t(Xm)
  n <- length(tt); p <- ncol(Xm)
  if (!link %in% c("cloglog", "logit")) {
    stop("link must be 'cloglog' or 'logit'", call. = FALSE)
  }
  etimes <- sort(unique(tt[e == 1]))
  K <- length(etimes)
  if (K == 0L) stop("no events in the data", call. = FALSE)
  rows_k <- integer(0); rows_i <- integer(0); rows_y <- numeric(0)
  for (i in seq_len(n)) {
    for (k in seq_len(K)) {
      if (tt[i] >= etimes[k]) {
        rows_k <- c(rows_k, k); rows_i <- c(rows_i, i)
        rows_y <- c(rows_y, as.numeric(tt[i] == etimes[k] && e[i] == 1))
      } else break
    }
  }
  q <- K + p
  theta <- rep(0, q); ll <- 0; info <- matrix(0, q, q); it <- 0L
  for (it in seq_len(max_iter)) {
    U <- rep(0, q); info <- matrix(0, q, q); ll <- 0
    for (r in seq_along(rows_k)) {
      k <- rows_k[r]; i <- rows_i[r]; y <- rows_y[r]
      eta <- theta[k] + sum(Xm[i, ] * theta[(K + 1):q])
      if (link == "cloglog") {
        ec <- exp(pmax(pmin(eta, 30), -30))
        h <- 1 - exp(-ec)
        h <- pmin(pmax(h, 1e-12), 1 - 1e-12)
        dh <- ec * (1 - h)
      } else {
        h <- 1 / (1 + exp(-pmax(pmin(eta, 30), -30)))
        h <- pmin(pmax(h, 1e-12), 1 - 1e-12)
        dh <- h * (1 - h)
      }
      ll <- ll + y * log(h) + (1 - y) * log(1 - h)
      gscal <- (y - h) / (h * (1 - h)) * dh
      wscal <- dh * dh / (h * (1 - h))
      g <- rep(0, q); g[k] <- 1; g[(K + 1):q] <- Xm[i, ]
      U <- U + gscal * g
      info <- info + wscal * tcrossprod(g)
    }
    step <- solve(info, U)
    theta <- theta + step
    if (max(abs(step)) < tol) break
  }
  cov <- solve(info)
  dg <- diag(cov)[(K + 1):q]
  if (any(!is.finite(dg)) || any(dg <= 0)) {
    stop("information matrix is singular", call. = FALSE)
  }
  list(estimate = theta[(K + 1):q], se = sqrt(dg), alpha = theta[1:K],
       event_times = etimes, loglik = ll, n_iter = it, link = link,
       method = "Klein-Moeschberger (2003) sec. 8.4 discrete-time hazard regression")
}

#' Wolbers concordance for competing-risks predictions
#'
#' Pair (i, j) is comparable when i has a cause-1 event and either
#' T_i < T_j or j experienced a competing event (Fine-Gray risk set);
#' concordant when the predicted cause-1 risk of i exceeds that of j,
#' ties one half.
#'
#' @param time Observed times.
#' @param event_type 0 censored, 1 cause of interest, >= 2 competing.
#' @param predicted_F Predicted cause-1 cumulative incidence.
#' @return List with \code{estimate}, \code{concordant}, \code{tied},
#'   \code{comparable}.
#' @references Wolbers, M., Blanche, P., Koller, M. T., Witteman,
#'   J. C. M. and Gerds, T. A. (2014), Biostatistics 15(3), 526-539.
#' @export
#' @examples
#' Sscompv(time = c(1, 2, 3, 4, 5, 6, 7, 8), event_type = c(1, 2, 3, 4, 5, 6, 7, 8), predicted_F = c(1, 2, 3, 4, 5, 6, 7, 8))
Sscompv <- function(time, event_type, predicted_F) {
  t <- as.numeric(time); d <- as.numeric(event_type)
  F <- as.numeric(predicted_F)
  n <- length(t)
  if (length(d) != n || length(F) != n) {
    stop("time, event_type and predicted_F must have equal length",
         call. = FALSE)
  }
  conc <- 0; tied <- 0; comp <- 0L
  for (i in seq_len(n)) {
    if (d[i] != 1) next
    for (j in seq_len(n)) {
      if (j == i) next
      if (t[i] < t[j] || d[j] >= 2) {
        comp <- comp + 1L
        if (F[i] > F[j]) conc <- conc + 1
        else if (F[i] == F[j]) tied <- tied + 1
      }
    }
  }
  if (comp == 0L) stop("no comparable pairs", call. = FALSE)
  list(estimate = (conc + 0.5 * tied) / comp, concordant = conc,
       tied = tied, comparable = comp,
       method = "Wolbers et al (2014) competing-risks concordance")
}
