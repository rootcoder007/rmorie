# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Enumerative combinatorics. Mirrors morie.fn.enumcb.
#
# Every count here runs through R/bigint_native.R rather than doubles.
# That is not fastidiousness: Bell(25) is 4638590332229999353 and
# p(1000) has 32 digits, both far past the 2^53 where an R double stops
# being an exact integer. Returning a double would give an answer that
# looks right, prints plausibly, and is wrong in its low-order digits
# with nothing to say so.
#
# The functions therefore return `morie_bigint` objects. Use
# as.character() for the exact decimal string -- which is what the
# cross-language parity test compares, so a silent loss of precision
# fails the test rather than passing it.
#
# Stanley RP (2011), Enumerative Combinatorics vol. 1, 2nd ed., Sec 1.9
# (twelvefold way, Stirling numbers) and 3.7 (Mobius inversion).
# Andrews GE (1976), The Theory of Partitions (pentagonal numbers).

#' Stirling numbers of the second kind
#'
#' \eqn{S(n, k)} is the number of ways to partition an \eqn{n}-set into
#' exactly \eqn{k} non-empty unlabelled blocks, by the recurrence
#' \eqn{S(n,k) = k S(n-1,k) + S(n-1,k-1)}.
#'
#' @param n Non-negative whole number.
#' @param k Block count; if NULL the whole row is returned.
#' @return A `morie_bigint`, or a list of them when `k` is NULL.
#' @references Stanley RP (2011) vol. 1, Sec 1.9.
#' @export
morie_stirling_second <- function(n, k = NULL) {
  n <- as.integer(n)
  if (is.na(n) || n < 0L) {
    stop(sprintf("n must be non-negative; got %s", n), call. = FALSE)
  }
  row <- c(list(morie_bigint(1)),
           replicate(n, morie_bigint(0), simplify = FALSE))
  if (n >= 1L) {
    for (i in seq_len(n)) {
      new <- replicate(n + 1L, morie_bigint(0), simplify = FALSE)
      for (j in seq_len(i)) {
        new[[j + 1L]] <- morie_big_add(
          morie_big_mul(morie_bigint(j), row[[j + 1L]]), row[[j]]
        )
      }
      row <- new
    }
  }
  if (is.null(k)) return(row)
  k <- as.integer(k)
  if (k < 0L || k > n) return(morie_bigint(0))
  row[[k + 1L]]
}

#' Unsigned Stirling numbers of the first kind
#'
#' \eqn{c(n, k)} is the number of permutations of \eqn{n} elements with
#' exactly \eqn{k} cycles, by
#' \eqn{c(n,k) = (n-1) c(n-1,k) + c(n-1,k-1)}. With `signed` the result
#' is \eqn{s(n,k) = (-1)^{n-k} c(n,k)}.
#'
#' @param n Non-negative whole number.
#' @param k Cycle count; if NULL the whole row is returned.
#' @param signed Return the signed Stirling numbers.
#' @return A `morie_bigint`, or a list of them when `k` is NULL.
#' @references Stanley RP (2011) vol. 1, Sec 1.3.
#' @export
morie_stirling_first <- function(n, k = NULL, signed = FALSE) {
  n <- as.integer(n)
  if (is.na(n) || n < 0L) {
    stop(sprintf("n must be non-negative; got %s", n), call. = FALSE)
  }
  row <- c(list(morie_bigint(1)),
           replicate(n, morie_bigint(0), simplify = FALSE))
  if (n >= 1L) {
    for (i in seq_len(n)) {
      new <- replicate(n + 1L, morie_bigint(0), simplify = FALSE)
      for (j in seq_len(i)) {
        new[[j + 1L]] <- morie_big_add(
          morie_big_mul(morie_bigint(i - 1L), row[[j + 1L]]), row[[j]]
        )
      }
      row <- new
    }
  }
  flip <- function(v, j) {
    if (!isTRUE(signed) || (n - j) %% 2L == 0L) return(v)
    morie_big_sub(morie_bigint(0), v)
  }
  if (is.null(k)) {
    return(lapply(seq_along(row), function(j) flip(row[[j]], j - 1L)))
  }
  k <- as.integer(k)
  if (k < 0L || k > n) return(morie_bigint(0))
  flip(row[[k + 1L]], k)
}

#' Bell numbers by the Bell triangle
#'
#' \eqn{B_n} counts partitions of an \eqn{n}-set into any number of
#' blocks, so \eqn{B_n = \sum_k S(n,k)}. The triangle is used instead of
#' that sum because it needs only additions.
#'
#' \eqn{B_{25} = 4638590332229999353}, past the exact range of a double,
#' which is why this returns a `morie_bigint`.
#'
#' @param n Non-negative whole number.
#' @return A `morie_bigint`.
#' @references Stanley RP (2011) vol. 1, Sec 1.9.
#' @export
morie_bell_number <- function(n) {
  n <- as.integer(n)
  if (is.na(n) || n < 0L) {
    stop(sprintf("n must be non-negative; got %s", n), call. = FALSE)
  }
  if (n == 0L) return(morie_bigint(1))
  row <- list(morie_bigint(1))
  for (i in seq_len(n)) {
    nxt <- list(row[[length(row)]])
    for (v in row) {
      nxt[[length(nxt) + 1L]] <- morie_big_add(nxt[[length(nxt)]], v)
    }
    row <- nxt
  }
  row[[1L]]
}

#' Catalan numbers
#'
#' \eqn{C_n = \binom{2n}{n} - \binom{2n}{n+1}}, which stays in exact
#' integers and never divides. Counts balanced bracket sequences,
#' triangulations of a convex \eqn{(n+2)}-gon and binary trees on
#' \eqn{n} nodes, among many others.
#'
#' @param n Non-negative whole number.
#' @return A `morie_bigint`.
#' @export
morie_catalan_number <- function(n) {
  n <- as.integer(n)
  if (is.na(n) || n < 0L) {
    stop(sprintf("n must be non-negative; got %s", n), call. = FALSE)
  }
  morie_big_sub(morie_big_binom(2L * n, n), morie_big_binom(2L * n, n + 1L))
}

#' Integer partition counts by the pentagonal number theorem
#'
#' \deqn{p(n) = \sum_{k \ge 1} (-1)^{k+1}\left\[
#'   p(n - k(3k-1)/2) + p(n - k(3k+1)/2)\right\],}
#' which needs \eqn{O(\sqrt n)} terms per value rather than \eqn{O(n)}.
#'
#' `distinct` counts partitions into distinct parts and `odd_only` into
#' odd parts. Euler's theorem says those two counts are EQUAL at every
#' \eqn{n}, which the parity test checks rather than assumes.
#'
#' @param n Non-negative whole number.
#' @param distinct Count partitions into distinct parts.
#' @param odd_only Count partitions into odd parts.
#' @return A `morie_bigint`.
#' @references Andrews GE (1976), The Theory of Partitions.
#' @export
morie_partition_count <- function(n, distinct = FALSE, odd_only = FALSE) {
  n <- as.integer(n)
  if (is.na(n) || n < 0L) {
    stop(sprintf("n must be non-negative; got %s", n), call. = FALSE)
  }
  if (isTRUE(distinct) && isTRUE(odd_only)) {
    stop("distinct and odd_only are alternatives, not a pair.", call. = FALSE)
  }
  if (isTRUE(distinct) || isTRUE(odd_only)) {
    dp <- replicate(n + 1L, morie_bigint(0), simplify = FALSE)
    dp[[1L]] <- morie_bigint(1)
    if (isTRUE(distinct)) {
      for (part in seq_len(n)) {
        if (part > n) break
        for (total in seq.int(n, part)) {
          dp[[total + 1L]] <- morie_big_add(dp[[total + 1L]],
                                            dp[[total - part + 1L]])
        }
      }
    } else {
      part <- 1L
      while (part <= n) {
        for (total in seq.int(part, n)) {
          dp[[total + 1L]] <- morie_big_add(dp[[total + 1L]],
                                            dp[[total - part + 1L]])
        }
        part <- part + 2L
      }
    }
    return(dp[[n + 1L]])
  }
  p <- replicate(n + 1L, morie_bigint(0), simplify = FALSE)
  p[[1L]] <- morie_bigint(1)
  if (n >= 1L) {
    for (m in seq_len(n)) {
      total <- morie_bigint(0)
      k <- 1L
      repeat {
        # PARENTHESES REQUIRED. In R, %/% binds TIGHTER than *, so
        # `k * (3L * k - 1L) %/% 2L` parses as
        # `k * ((3L * k - 1L) %/% 2L)` and gives 4 at k = 2 where the
        # pentagonal number is 5. Python's // shares precedence with *
        # and associates left to right, so the identical-looking
        # expression is correct there and wrong here. This produced
        # p(4) = 4 instead of 5 and every later value was wrong too.
        g1 <- (k * (3L * k - 1L)) %/% 2L
        g2 <- (k * (3L * k + 1L)) %/% 2L
        if (g1 > m && g2 > m) break
        pos <- (k %% 2L == 1L)
        if (g1 <= m) {
          total <- if (pos) morie_big_add(total, p[[m - g1 + 1L]]) else
            morie_big_sub(total, p[[m - g1 + 1L]])
        }
        if (g2 <= m) {
          total <- if (pos) morie_big_add(total, p[[m - g2 + 1L]]) else
            morie_big_sub(total, p[[m - g2 + 1L]])
        }
        k <- k + 1L
      }
      p[[m + 1L]] <- total
    }
  }
  p[[n + 1L]]
}

#' Partitions of n into exactly k positive parts
#'
#' \eqn{p(n,k) = p(n-1,k-1) + p(n-k,k)}: either the smallest part is 1,
#' or every part exceeds 1 and one can be removed from each.
#'
#' @param n,k Non-negative whole numbers.
#' @return A `morie_bigint`.
#' @export
morie_partitions_into_parts <- function(n, k) {
  n <- as.integer(n); k <- as.integer(k)
  if (is.na(n) || is.na(k) || n < 0L || k < 0L) {
    stop("n and k must be non-negative.", call. = FALSE)
  }
  if (k == 0L) return(morie_bigint(if (n == 0L) 1 else 0))
  if (k > n) return(morie_bigint(0))
  dp <- vector("list", (n + 1L) * (k + 1L))
  idx <- function(i, j) i * (k + 1L) + j + 1L
  for (i in 0:n) for (j in 0:k) dp[[idx(i, j)]] <- morie_bigint(0)
  dp[[idx(0L, 0L)]] <- morie_bigint(1)
  for (i in seq_len(n)) {
    for (j in seq_len(min(i, k))) {
      v <- dp[[idx(i - 1L, j - 1L)]]
      if (i >= j) v <- morie_big_add(v, dp[[idx(i - j, j)]])
      dp[[idx(i, j)]] <- v
    }
  }
  dp[[idx(n, k)]]
}

#' Derangements: permutations with no fixed point
#'
#' \eqn{D_n = n D_{n-1} + (-1)^n}.
#'
#' @param n Non-negative whole number.
#' @return A `morie_bigint`.
#' @export
morie_derangements <- function(n) {
  n <- as.integer(n)
  if (is.na(n) || n < 0L) {
    stop(sprintf("n must be non-negative; got %s", n), call. = FALSE)
  }
  d <- morie_bigint(1)
  if (n >= 1L) {
    for (i in seq_len(n)) {
      d <- morie_big_mul(morie_bigint(i), d)
      d <- if (i %% 2L == 0L) morie_big_add(d, morie_bigint(1)) else
        morie_big_sub(d, morie_bigint(1))
    }
  }
  d
}

#' The twelvefold way
#'
#' Functions from an \eqn{n}-set to a \eqn{k}-set counted under every
#' combination of labelling and restriction. Stanley's point in
#' tabulating the twelve together is that they are one problem, not
#' twelve.
#'
#' @param n,k Balls and boxes.
#' @param balls,boxes "labelled" or "unlabelled".
#' @param condition "any", "injective" or "surjective".
#' @return A list with `count` (a `morie_bigint`), `formula`, `cell`.
#' @references Stanley RP (2011) vol. 1, Sec 1.9.
#' @export
morie_twelvefold_way <- function(n, k, balls = c("labelled", "unlabelled"),
                                 boxes = c("labelled", "unlabelled"),
                                 condition = c("any", "injective",
                                               "surjective")) {
  balls <- match.arg(balls); boxes <- match.arg(boxes)
  condition <- match.arg(condition)
  n <- as.integer(n); k <- as.integer(k)
  if (is.na(n) || is.na(k) || n < 0L || k < 0L) {
    stop("n and k must be non-negative.", call. = FALSE)
  }
  lb <- balls == "labelled"; lx <- boxes == "labelled"
  falling <- function(a, b) {
    out <- morie_bigint(1)
    if (b > a) return(morie_bigint(0))
    if (b == 0L) return(out)
    for (i in seq_len(b)) out <- morie_big_mul(out, morie_bigint(a - i + 1L))
    out
  }
  if (lb && lx) {
    if (condition == "any") { cnt <- morie_big_pow(k, n); f <- "k^n" }
    else if (condition == "injective") {
      cnt <- falling(k, n); f <- "k falling factorial n"
    } else {
      cnt <- morie_big_mul(morie_big_factorial(k), morie_stirling_second(n, k))
      f <- "k! S(n,k)"
    }
  } else if (!lb && lx) {
    if (condition == "any") {
      cnt <- morie_big_binom(n + k - 1L, n); f <- "C(n+k-1, n)"
    } else if (condition == "injective") {
      cnt <- morie_big_binom(k, n); f <- "C(k, n)"
    } else {
      cnt <- if (n >= k && k >= 1L) morie_big_binom(n - 1L, n - k) else
        morie_bigint(if (n == 0L && k == 0L) 1 else 0)
      f <- "C(n-1, n-k)"
    }
  } else if (lb && !lx) {
    if (condition == "any") {
      cnt <- morie_bigint(0)
      for (j in 0:k) cnt <- morie_big_add(cnt, morie_stirling_second(n, j))
      f <- "sum_j S(n,j)"
    } else if (condition == "injective") {
      cnt <- morie_bigint(if (n <= k) 1 else 0); f <- "[n <= k]"
    } else { cnt <- morie_stirling_second(n, k); f <- "S(n,k)" }
  } else {
    if (condition == "any") {
      cnt <- morie_bigint(0)
      for (j in 0:k) cnt <- morie_big_add(cnt,
                                          morie_partitions_into_parts(n, j))
      f <- "sum_j p(n,j)"
    } else if (condition == "injective") {
      cnt <- morie_bigint(if (n <= k) 1 else 0); f <- "[n <= k]"
    } else { cnt <- morie_partitions_into_parts(n, k); f <- "p(n,k)" }
  }
  list(count = cnt, formula = f,
       cell = sprintf("%s balls, %s boxes, %s", balls, boxes, condition),
       balls = balls, boxes = boxes, condition = condition, n = n, k = k,
       method = "Twelvefold way (Stanley, Enumerative Combinatorics 1.9)")
}

#' Mobius inversion over the divisor lattice
#'
#' Given \eqn{f(n) = \sum_{d | n} g(d)}, recover
#' \eqn{g(n) = \sum_{d | n} \mu(n/d) f(d)}.
#'
#' The check that matters is the defining property of \eqn{\mu}:
#' \eqn{\sum_{d | n} \mu(d)} is 1 at \eqn{n = 1} and 0 everywhere else.
#' That is an identity, and the residual is returned rather than assumed.
#'
#' @param f_values `f_values\[i\]` is \eqn{f(i)}.
#' @return A list with `g`, `mobius`, `reconstruction_residual`,
#'   `mobius_identity_residual`, `divisor_sums`.
#' @references Stanley RP (2011) vol. 1, Sec 3.7.
#' @export
morie_mobius_inversion <- function(f_values) {
  f <- as.numeric(f_values)
  n <- length(f)
  if (n < 1L) stop("f_values must not be empty.", call. = FALSE)
  mu <- numeric(n + 1L)
  mu[2] <- 1
  if (n >= 1L) {
    for (i in seq_len(n)) {
      j <- 2L * i
      while (j <= n) {
        mu[j + 1L] <- mu[j + 1L] - mu[i + 1L]
        j <- j + i
      }
    }
  }
  mu <- mu[-1]
  g <- numeric(n)
  for (m in seq_len(n)) {
    d <- seq_len(m)
    d <- d[m %% d == 0]
    g[m] <- sum(mu[m %/% d] * f[d])
  }
  rebuilt <- vapply(seq_len(n), function(m) {
    d <- seq_len(m); d <- d[m %% d == 0]; sum(g[d])
  }, numeric(1))
  sums <- vapply(seq_len(n), function(m) {
    d <- seq_len(m); d <- d[m %% d == 0]; sum(mu[d])
  }, numeric(1))
  list(g = g, mobius = mu, divisor_sums = sums,
       reconstruction_residual = max(abs(f - rebuilt)),
       mobius_identity_residual = max(abs(sums - c(1, rep(0, n - 1)))),
       n = n, method = "Mobius inversion (Stanley 3.7)")
}
