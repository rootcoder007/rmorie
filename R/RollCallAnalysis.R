# SPDX-License-Identifier: AGPL-3.0-or-later
# Roll-call cohesion, party influence and logistic ideal points.
# Identical to the Python arm morie.fn.rcanalysis.

#' Roll-call analysis: cohesion and agreement, Snyder-Groseclose party influence, logistic ideal points
#'
#' \code{RollcallCohesion}: per party and vote the Rice index
#' \code{|Y - N| / (Y + N)} and the Hix-Noury-Roland agreement index, per
#' legislator the participation rate, and pairwise agreement.
#' \code{PartyInfluence}: ideal points from the first principal component of
#' lopsided votes, then per close vote the linear probability model
#' \code{y = a + b x + g party}. \code{LogitIdealPoints}: penalised joint ML of
#' \code{logistic(alpha_j + beta_j x_i)} by alternating Newton steps with
#' \code{mean(x) = 0}, \code{sd(x) = 1}. Votes are 1, 0 or NA.
#'
#' @param votes Legislators by votes matrix (1, 0, NA).
#' @param party Party labels.
#' @param party_dummy 0/1 party indicator.
#' @param lopsided Winning-side share defining a lopsided vote.
#' @param prior_sd Prior standard deviation of bill parameters.
#' @param max_iter,tol Iteration limit and tolerance.
#' @return A list.
#' @references Rice, S. A. (1925). Political Science Quarterly 40, 60-72.
#'   Hix, S., Noury, A. and Roland, G. (2005). BJPS 35, 209-234. Snyder, J. M.
#'   and Groseclose, T. (2000). AJPS 44, 193-211. Clinton, J., Jackman, S. and
#'   Rivers, D. (2004). APSR 98, 355-370.
#' @examples
#' RollcallCohesion(rbind(c(1, 1), c(1, 0), c(0, 0)), c("a", "a", "b"))$rice
#' @export
RollcallCohesion <- function(votes, party) {
  V <- as.matrix(votes)
  n <- nrow(V)
  m <- ncol(V)
  parties <- sort(unique(as.character(party)))
  rice <- agree <- list()
  for (p in parties) {
    rows <- which(as.character(party) == p)
    y <- colSums(V[rows, , drop = FALSE] == 1, na.rm = TRUE)
    no <- colSums(V[rows, , drop = FALSE] == 0, na.rm = TRUE)
    a <- length(rows) - y - no
    rice[[p]] <- ifelse(y + no > 0, abs(y - no) / (y + no), NaN)
    mx <- pmax(y, no, a)
    agree[[p]] <- (mx - (y + no + a - mx) / 2) / (y + no + a)
  }
  ok <- !is.na(V)
  pair <- matrix(NaN, n, n)
  for (i in seq_len(n)) for (k in seq_len(n)) {
    both <- ok[i, ] & ok[k, ]
    if (any(both)) pair[i, k] <- mean(V[i, both] == V[k, both])
  }
  list(rice = rice, agreement = agree, participation = rowSums(ok) / m, pairwise = pair, parties = parties)
}

#' @rdname RollcallCohesion
#' @export
PartyInfluence <- function(votes, party_dummy, lopsided = 0.65) {
  V <- as.matrix(votes)
  n <- nrow(V)
  yes <- colMeans(V, na.rm = TRUE)
  lop <- which(pmax(yes, 1 - yes) >= lopsided)
  close <- which(pmax(yes, 1 - yes) < lopsided)
  pd <- as.numeric(party_dummy)
  x <- rep(0, n)
  if (length(lop)) {
    M <- V[, lop, drop = FALSE]
    for (j in seq_len(ncol(M))) M[is.na(M[, j]), j] <- mean(M[, j], na.rm = TRUE)
    C <- sweep(M, 2, colMeans(M))
    v <- eigen(tcrossprod(C), symmetric = TRUE)$vectors[, 1]
    x <- if (v[which.max(abs(v))] < 0) -v else v
  }
  if (sum((x - mean(x)) * (pd - mean(pd))) < 0) x <- -x
  gam <- vapply(close, function(j) {
    rows <- which(!is.na(V[, j]))
    X <- cbind(1, x[rows], pd[rows])
    as.numeric(solve(crossprod(X), crossprod(X, V[rows, j])))[3]
  }, 0)
  list(ideal_points = x, party_effect = gam, mean_party_effect = if (length(gam)) mean(gam) else NaN,
       close_votes = close - 1, lopsided_votes = lop - 1)
}

#' @rdname RollcallCohesion
#' @export
LogitIdealPoints <- function(votes, prior_sd = 5, max_iter = 500, tol = 1e-10) {
  Y <- as.matrix(votes) * 1
  n <- nrow(Y)
  m <- ncol(Y)
  sig <- function(z) ifelse(z >= 0, 1 / (1 + exp(-z)), exp(z) / (1 + exp(z)))
  cm <- colMeans(Y, na.rm = TRUE)
  s0 <- rowSums(sweep(Y, 2, cm), na.rm = TRUE)
  sd0 <- sqrt(sum((s0 - mean(s0))^2) / n)
  if (sd0 == 0) sd0 <- 1
  x <- (s0 - mean(s0)) / sd0
  ref <- x
  a <- rep(0, m)
  b <- rep(1, m)
  it <- 0
  for (iter in seq_len(max_iter)) {
    it <- it + 1
    old <- c(x, a, b)
    for (j in seq_len(m)) {
      rows <- which(!is.na(Y[, j]))
      for (k in 1:3) {
        p <- sig(a[j] + b[j] * x[rows])
        r <- Y[rows, j] - p
        w <- p * (1 - p)
        g <- c(sum(r) - a[j] / prior_sd^2, sum(r * x[rows]) - b[j] / prior_sd^2)
        H <- -rbind(c(sum(w), sum(w * x[rows])), c(sum(w * x[rows]), sum(w * x[rows]^2))) - diag(1 / prior_sd^2, 2)
        d <- solve(H, g)
        a[j] <- a[j] - d[1]
        b[j] <- b[j] - d[2]
      }
    }
    for (i in seq_len(n)) {
      cols <- which(!is.na(Y[i, ]))
      for (k in 1:3) {
        p <- sig(a[cols] + b[cols] * x[i])
        g <- sum((Y[i, cols] - p) * b[cols]) - x[i]
        h <- -sum(p * (1 - p) * b[cols]^2) - 1
        x[i] <- x[i] - g / h
      }
    }
    mx <- sum(x) / n
    sx <- sqrt(sum((x - mx)^2) / n)
    if (sum((x - mx) * ref) < 0) sx <- -sx
    x <- (x - mx) / sx
    a <- a + b * mx
    b <- b * sx
    if (max(abs(old - c(x, a, b))) <= tol) break
  }
  list(ideal_points = x, alpha = a, beta = b, iterations = it)
}
