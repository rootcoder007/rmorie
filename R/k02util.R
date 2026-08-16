# SPDX-License-Identifier: AGPL-3.0-or-later
# k02 batch shared helpers -- internal, not exported.
# Mirrors src/morie/fn/k02util.py arithmetic exactly.

#' k02fe
#'
#' A step of the k02util implementation. Called by \code{k02dl}, \code{mafix}, \code{mai2} and 2 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y A vector; its length is taken.
#' @param v Numeric; combined arithmetically in the body.
#' @return A list with \code{mu}, \code{var}, \code{sw}, \code{Q}, \code{df}.
#' @export
k02fe <- function(y, v) {
  y <- as.numeric(y); v <- as.numeric(v)
  w <- 1 / v
  sw <- sum(w)
  mu <- sum(w * y) / sw
  q <- sum(w * (y - mu)^2)
  list(mu = mu, var = 1 / sw, sw = sw, Q = q, df = length(y) - 1L)
}

#' k02dl
#'
#' A step of the k02util implementation. Called by \code{mabay}, \code{mac3}, \code{macum} and 7 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Numeric; combined arithmetically in the body.
#' @param v Numeric; combined arithmetically in the body.
#' @return A list with \code{tau2}, \code{mu}, \code{var}, \code{Q}, \code{df}.
#' @export
k02dl <- function(y, v) {
  y <- as.numeric(y); v <- as.numeric(v)
  fe <- k02fe(y, v)
  w <- 1 / v
  cc <- fe$sw - sum(w * w) / fe$sw
  tau2 <- if (cc > 0) max(0, (fe$Q - fe$df) / cc) else 0
  ws <- 1 / (v + tau2)
  sws <- sum(ws)
  list(tau2 = tau2, mu = sum(ws * y) / sws, var = 1 / sws, Q = fe$Q, df = fe$df)
}

#' k02mm
#'
#' A step of the k02util implementation. Called by \code{matr}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Numeric; combined arithmetically in the body.
#' @param v Numeric; combined arithmetically in the body.
#' @param tau0 Numeric; combined arithmetically in the body.
#' @return One of two values, depending on the branch taken.
#' @export
k02mm <- function(y, v, tau0) {
  y <- as.numeric(y); v <- as.numeric(v)
  a <- 1 / (v + tau0)
  sa <- sum(a); sa2 <- sum(a * a)
  yb <- sum(a * y) / sa
  num <- sum(a * (y - yb)^2) - sum(a * v) + sum(a * a * v) / sa
  den <- sa - sa2 / sa
  if (den > 0) max(0, num / den) else 0
}

#' k02z
#'
#' A step of the k02util implementation. Called by \code{mabay}, \code{macum}, \code{mafix} and 9 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p See Usage.
#' @return The value of \code{stats::qnorm}.
#' @export
k02z <- function(p) stats::qnorm(p)
#' k02tq
#'
#' A step of the k02util implementation. Called by \code{mahks}, \code{mahsj}, \code{matau2pi}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p See Usage.
#' @param df See Usage.
#' @return The value of \code{stats::qt}.
#' @export
k02tq <- function(p, df) stats::qt(p, df)
#' k02p2z
#'
#' A step of the k02util implementation. Called by \code{mafix}, \code{marndm}, \code{matr}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param z Numeric; passed to \code{abs}.
#' @return A numeric value.
#' @export
k02p2z <- function(z) 2 * stats::pnorm(abs(z), lower.tail = FALSE)
#' k02p2t
#'
#' A step of the k02util implementation. Called by \code{mahks}, \code{mahsj}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param tv Numeric; passed to \code{abs}.
#' @param df See Usage.
#' @return A numeric value.
#' @export
k02p2t <- function(tv, df) 2 * stats::pt(abs(tv), df, lower.tail = FALSE)
#' k02pchi
#'
#' A step of the k02util implementation. Called by \code{mafix}, \code{mai2}, \code{marndm} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param q See Usage.
#' @param df See Usage.
#' @return The value of \code{stats::pchisq}.
#' @export
k02pchi <- function(q, df) stats::pchisq(q, df, lower.tail = FALSE)

.k02invphi <- 0.6180339887498949

#' k02gold
#'
#' A step of the k02util implementation. Called by \code{magpa}, \code{matrl}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param f See Usage.
#' @param lo See Usage.
#' @param hi See Usage.
#' @param iters Defaults to \code{80L}.
#' @return A numeric value.
#' @export
k02gold <- function(f, lo, hi, iters = 80L) {
  a <- as.numeric(lo); b <- as.numeric(hi)
  cc <- b - .k02invphi * (b - a)
  dd <- a + .k02invphi * (b - a)
  fc <- f(cc); fd <- f(dd)
  for (i in seq_len(as.integer(iters))) {
    if (fc < fd) {
      b <- dd; dd <- cc; fd <- fc
      cc <- b - .k02invphi * (b - a); fc <- f(cc)
    } else {
      a <- cc; cc <- dd; fc <- fd
      dd <- a + .k02invphi * (b - a); fd <- f(dd)
    }
  }
  0.5 * (a + b)
}

#' k02gh
#'
#' A step of the k02util implementation. Called by \code{magpa}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n A count; the body uses it as \code{numeric(...)}.
#' @return A list with \code{x}, \code{w}.
#' @export
k02gh <- function(n) {
  n <- as.integer(n)
  pim4 <- 0.7511255444649425
  eps <- 3.0e-14
  x <- numeric(n); w <- numeric(n)
  z <- 0; pp <- 0
  m <- (n + 1L) %/% 2L
  for (i in seq_len(m)) {
    if (i == 1L) {
      z <- sqrt(2 * n + 1) - 1.85575 * (2 * n + 1)^(-0.16667)
    } else if (i == 2L) {
      z <- z - 1.14 * n^0.426 / z
    } else if (i == 3L) {
      z <- 1.86 * z - 0.86 * x[1]
    } else if (i == 4L) {
      z <- 1.91 * z - 0.91 * x[2]
    } else {
      z <- 2 * z - x[i - 2L]
    }
    for (it in 1:20) {
      p1 <- pim4; p2 <- 0
      for (j in 1:n) {
        p3 <- p2; p2 <- p1
        p1 <- z * sqrt(2 / j) * p2 - sqrt((j - 1) / j) * p3
      }
      pp <- sqrt(2 * n) * p2
      z1 <- z
      z <- z1 - p1 / pp
      if (abs(z - z1) <= eps) break
    }
    x[i] <- z; x[n - i + 1L] <- -z
    w[i] <- 2 / (pp * pp); w[n - i + 1L] <- w[i]
  }
  list(x = x, w = w)
}

#' k02mod
#'
#' A step of the k02util implementation. Called by \code{louv}, \code{sgtcoml}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A A matrix; passed to \code{as.matrix}.
#' @param comm A vector; indexed elementwise.
#' @return A numeric value.
#' @export
k02mod <- function(A, comm) {
  a <- as.matrix(A); n <- nrow(a)
  k <- rowSums(a); m2 <- sum(a)
  if (m2 <= 0) return(0)
  q <- 0
  for (i in seq_len(n)) for (j in seq_len(n)) {
    if (comm[i] == comm[j]) q <- q + (a[i, j] - k[i] * k[j] / m2)
  }
  q / m2
}

#' k02bfs
#'
#' A step of the k02util implementation. Called by \code{smwgrp}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A A matrix; passed to \code{as.matrix}.
#' @return The value of \code{out}, as built in the body.
#' @export
k02bfs <- function(A) {
  a <- as.matrix(A); n <- nrow(a)
  nbr <- lapply(seq_len(n), function(i) which(a[i, ] != 0 & seq_len(n) != i))
  out <- matrix(-1L, n, n)
  for (s in seq_len(n)) {
    dist <- rep(-1L, n); dist[s] <- 0L
    queue <- c(s); head <- 1L
    while (head <= length(queue)) {
      u <- queue[head]; head <- head + 1L
      for (v in nbr[[u]]) if (dist[v] < 0L) { dist[v] <- dist[u] + 1L; queue <- c(queue, v) }
    }
    out[s, ] <- dist
  }
  out
}
