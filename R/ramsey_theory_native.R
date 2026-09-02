# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Ramsey theory: guaranteed monochromatic structure in large graphs.
# Mirrors morie.fn.ramthy.
#
# This is F. P. Ramsey's combinatorics. It has nothing to do with
# J. B. Ramsey's RESET specification test in R/diagnostics.R -- a
# different person, a different subject -- and the two are kept in
# separate files so nobody merges them by name.
#
# Everything here is integer arithmetic on exactly-known quantities, so
# the cross-language parity anchors are exact rather than to a
# tolerance.

# Exactly known nontrivial two-colour values, DS1 revision 17 Table Ia.
# Nine values, and the list has not grown since 1995.
.morie_ramsey_known <- list(
  "3,3" = 6L, "3,4" = 9L, "3,5" = 14L, "3,6" = 18L, "3,7" = 23L,
  "3,8" = 28L, "3,9" = 36L, "4,4" = 18L, "4,5" = 25L
)

# Best published bounds for unknown cases, DS1 Tables Ia and Ib.
.morie_ramsey_bounds <- list(
  "3,10" = c(40L, 41L), "3,11" = c(47L, 50L), "3,12" = c(53L, 59L),
  "3,13" = c(60L, 68L), "4,6" = c(36L, 40L), "4,7" = c(49L, 58L),
  "4,8" = c(59L, 79L), "5,5" = c(43L, 46L), "5,6" = c(59L, 85L),
  "6,6" = c(102L, 160L)
)

.morie_ramsey_credits <- list(
  "3,3" = "Kurschak (1947); Putnam problem (1953)",
  "3,4" = "Greenwood and Gleason (1955)",
  "3,5" = "Greenwood and Gleason (1955)",
  "3,6" = "Kery (1964)",
  "4,4" = "Greenwood and Gleason (1955)",
  "4,5" = "McKay and Radziszowski (1995); HOL4-verified",
  "3,8" = "Grinstead and Roberts (1982); DRAT-verified"
)

#' .morie_es_bound
#'
#' A step of the ramsey_theory_native implementation. Called by \code{morie_ramsey_number}, \code{morie_ramsey_upper_bound}.
#' See the file header for the source the module follows.
#' for the source it follows.
#'
#' @param k Numeric; combined arithmetically in the body.
#' @param l Numeric; combined arithmetically in the body.
#' @return The value of \code{choose}.
#' @export
.morie_es_bound <- function(k, l) choose(k + l - 2, k - 1)

#' Ramsey number R(k, l), exactly or as an interval
#'
#' \eqn{R(k, l)} is the least \eqn{n} such that every red-blue colouring
#' of the edges of \eqn{K_n} contains a red \eqn{K_k} or a blue
#' \eqn{K_l}.
#'
#' Almost nothing is known. For \eqn{k, l \ge 3} exactly nine values
#' have ever been determined and the list has not grown since 1995.
#' \eqn{R(5,5)} is unknown, lying in \\[43, 46\\]. The difficulty is not
#' that the problem is open-ended but that it is finite and enormous:
#' settling \eqn{R(5,5)} by exhaustive search means examining the
#' \eqn{2^{903}} colourings of \eqn{K_{43}}.
#'
#' A value is returned only when one is known, and an interval
#' otherwise. Nothing is interpolated and no bound is ever returned as
#' though it were a value.
#'
#' The claim \eqn{R(5,5) = 50} circulates and is WRONG; DS1 records
#' that it has been shown incorrect more than once and is still cited.
#' Asking for it returns the interval and a warning.
#'
#' @param k,l Clique sizes; `l` defaults to `k` for the diagonal case.
#' @return A list with `value` (NULL when unknown), `lower`, `upper`,
#'   `exact`, `credit`, `erdos_szekeres_bound`, `warnings`.
#' @references Radziszowski SP, \emph{Small Ramsey Numbers}, Electronic
#'   Journal of Combinatorics, Dynamic Survey DS1, revision 17 (2024),
#'   \doi{10.37236/21}, Tables Ia and Ib.
#' @export
#' @examples
#' morie_ramsey_number(k = 5L)
morie_ramsey_number <- function(k, l = NULL) {
  if (is.null(l)) l <- k
  k <- as.integer(k); l <- as.integer(l)
  if (is.na(k) || is.na(l) || k < 1L || l < 1L) {
    stop(sprintf("k and l must be at least 1; got %s, %s", k, l),
         call. = FALSE)
  }
  a <- min(k, l); b <- max(k, l)
  key <- paste(a, b, sep = ",")
  warns <- character(0)

  if (a == 1L) {
    val <- 1L; lo <- 1L; hi <- 1L; credit <- "trivial: R(1, l) = 1"
  } else if (a == 2L) {
    val <- b; lo <- b; hi <- b; credit <- "trivial: R(2, l) = l"
  } else if (!is.null(.morie_ramsey_known[[key]])) {
    val <- .morie_ramsey_known[[key]]; lo <- val; hi <- val
    credit <- if (!is.null(.morie_ramsey_credits[[key]]))
      .morie_ramsey_credits[[key]] else "DS1 Table Ia"
  } else if (!is.null(.morie_ramsey_bounds[[key]])) {
    val <- NULL
    lo <- .morie_ramsey_bounds[[key]][1]
    hi <- .morie_ramsey_bounds[[key]][2]
    credit <- "DS1 Tables Ia and Ib (bounds only)"
  } else {
    val <- NULL; lo <- NULL; hi <- .morie_es_bound(a, b)
    credit <- "no published bound tabulated here"
  }

  if (is.null(val)) {
    warns <- c(warns, sprintf(paste(
      "R(%d, %d) has never been determined. The interval is a pair of",
      "proofs, not an estimate, and the true value is not more likely to",
      "sit in the middle of it."), k, l))
  }
  if (a == 5L && b == 5L) {
    warns <- c(warns, paste(
      "The frequently repeated claim that R(5,5) = 50 is incorrect. DS1",
      "records that it has been shown wrong more than once and is still",
      "being cited. The published interval is [43, 46], with 43",
      "conjectured."))
  }
  list(k = k, l = l, value = val, lower = lo, upper = hi,
       exact = !is.null(val), credit = credit,
       erdos_szekeres_bound = .morie_es_bound(a, b),
       warnings = warns,
       method = "Ramsey number lookup with certified bounds")
}

#' Upper bounds on R(k, l) from the classical arguments
#'
#' The recursion is \eqn{R(k,l) \le R(k-1,l) + R(k,l-1)}, STRICT when
#' both terms on the right are even. Unrolling it with
#' \eqn{R(2,l) = l} gives the Erdos-Szekeres binomial bound
#' \eqn{\binom{k+l-2}{k-1}}, which is much weaker.
#'
#' `use_known` decides whether the recursion may short-circuit on the
#' nine tabulated values. On, it gives the tightest answer but makes
#' the result partly a lookup; off, it gives what the argument alone
#' supports. The two differ: 25 against 31 for \eqn{R(4,5)}. Any claim
#' that the recursion reproduces a known value must use FALSE.
#'
#' @param k,l Clique sizes.
#' @param use_known Consult the table of known values during recursion.
#' @return A list with `binomial`, `recursive`, `best`,
#'   `parity_saving`, `used_known_values`.
#' @references Greenwood RE, Gleason AM (1955) \emph{Canadian Journal of
#'   Mathematics} 7:1-7. Erdos P, Szekeres G (1935)
#'   \emph{Compositio Mathematica} 2:463-470.
#' @export
#' @examples
#' morie_ramsey_upper_bound(k = 5L, l = 5L)
morie_ramsey_upper_bound <- function(k, l, use_known = TRUE) {
  k <- as.integer(k); l <- as.integer(l)
  if (k < 1L || l < 1L) {
    stop(sprintf("k and l must be at least 1; got %d, %d", k, l),
         call. = FALSE)
  }
  memo <- new.env(parent = emptyenv())
  rec <- function(a, b) {
    lo <- min(a, b); hi <- max(a, b)
    if (lo == 1L) return(1L)
    if (lo == 2L) return(hi)
    key <- paste(lo, hi, sep = ",")
    if (use_known && !is.null(.morie_ramsey_known[[key]])) {
      return(.morie_ramsey_known[[key]])
    }
    if (!is.null(memo[[key]])) return(memo[[key]])
    u1 <- rec(lo - 1L, hi)
    u2 <- rec(lo, hi - 1L)
    val <- u1 + u2
    if (u1 %% 2L == 0L && u2 %% 2L == 0L) val <- val - 1L
    memo[[key]] <- val
    val
  }
  r <- rec(k, l)
  binom <- .morie_es_bound(min(k, l), max(k, l))
  list(binomial = binom, recursive = r, best = min(binom, r),
       parity_saving = binom - r, used_known_values = isTRUE(use_known))
}

#' Erdos's probabilistic lower bound on the diagonal R(k, k)
#'
#' Colour each edge of \eqn{K_n} independently at random. The expected
#' number of monochromatic \eqn{K_k} is
#' \eqn{\binom{n}{k} 2^{1 - \binom{k}{2}}}; if that is below 1 then some
#' colouring has none, so \eqn{R(k,k) > n}.
#'
#' The argument proves an object exists without producing one, and
#' seventy-five years later no explicit construction comes close. The
#' union-bound calculation is also stronger than the clean
#' \eqn{2^{k/2}} form usually quoted from it.
#'
#' @param k Clique size, at least 2.
#' @return A list with `bound`, `certifies`, `expected_at_bound`,
#'   `asymptotic_2_to_k_over_2`.
#' @references Erdos P (1947) \emph{Bulletin of the AMS} 53:292-294.
#' @export
#' @examples
#' morie_ramsey_lower_bound_probabilistic(k = 5L)
morie_ramsey_lower_bound_probabilistic <- function(k) {
  k <- as.integer(k)
  if (is.na(k) || k < 2L) {
    stop(sprintf("k must be at least 2; got %s", k), call. = FALSE)
  }
  expo <- 1 - choose(k, 2)
  n <- k
  best <- k - 1L
  while (n < 100000L) {
    log2e <- (lgamma(n + 1) - lgamma(k + 1) - lgamma(n - k + 1)) / log(2) +
      expo
    if (log2e < 0) {
      best <- n
      n <- n + 1L
    } else break
  }
  log2_at <- (lgamma(best + 1) - lgamma(k + 1) -
                lgamma(best - k + 1)) / log(2) + expo
  list(bound = best, certifies = sprintf("R(%d,%d) > %d", k, k, best),
       expected_at_bound = 2^log2_at,
       asymptotic_2_to_k_over_2 = 2^(k / 2))
}

#' Monochromatic triangles by Goodman's identity
#'
#' For any two-colouring of \eqn{K_n}, a triangle fails to be
#' monochromatic exactly when two of its vertices see one edge of each
#' colour. Summing over vertices counts each such triangle twice, so
#' \deqn{\#mono = \binom{n}{3} - \frac{1}{2}\sum_v r_v b_v,}
#' with \eqn{r_v}, \eqn{b_v} the red and blue degrees. This is an
#' IDENTITY holding exactly for every colouring, not an approximation;
#' `brute_force = TRUE` checks it against direct enumeration.
#'
#' The identity is what makes \eqn{R(3,3) \le 6} provable by hand.
#' Since \eqn{r_v + b_v = n-1}, the product is at most
#' \eqn{\lfloor (n-1)^2/4 \rfloor}, so at \eqn{n = 6} the count is at
#' least \eqn{20 - 18 = 2}: every colouring of \eqn{K_6} contains not
#' one monochromatic triangle but two. At \eqn{n = 5} the bound gives
#' zero and the five-cycle attains it.
#'
#' @param colouring Symmetric 0/1 matrix; 1 marks a red edge.
#' @param brute_force Also enumerate all triangles and check.
#' @return A list with `monochromatic`, `bichromatic`,
#'   `total_triangles`, `goodman_minimum`, `identity_residual`.
#' @references Goodman AW (1959) \emph{American Mathematical Monthly}
#'   66(9):778-783.
#' @export
morie_goodman_triangles <- function(colouring, brute_force = FALSE) {
  A <- as.matrix(colouring)
  if (nrow(A) != ncol(A)) {
    stop(sprintf("colouring must be square; got %d x %d",
                 nrow(A), ncol(A)), call. = FALSE)
  }
  n <- nrow(A)
  if (n < 3L) {
    stop(sprintf("need at least three vertices; got %d", n), call. = FALSE)
  }
  R <- matrix(as.integer(A != 0), n, n)
  diag(R) <- 0L
  if (!identical(R, t(R))) stop("colouring must be symmetric.", call. = FALSE)

  r <- rowSums(R)
  b <- (n - 1L) - r
  bi2 <- sum(r * b)
  if (bi2 %% 2 != 0) {
    stop("the vertex sum is odd, which is impossible for a valid colouring.",
         call. = FALSE)
  }
  bi <- bi2 / 2
  total <- choose(n, 3)
  mono <- total - bi

  red_t <- NULL; blue_t <- NULL; resid <- NULL
  if (isTRUE(brute_force)) {
    red_t <- 0L; blue_t <- 0L
    if (n >= 3L) {
      cmb <- utils::combn(n, 3)
      for (c in seq_len(ncol(cmb))) {
        i <- cmb[1, c]; j <- cmb[2, c]; k <- cmb[3, c]
        s <- R[i, j] + R[i, k] + R[j, k]
        if (s == 3L) red_t <- red_t + 1L else if (s == 0L)
          blue_t <- blue_t + 1L
      }
    }
    resid <- abs((red_t + blue_t) - mono)
  }
  gmin <- morie_goodman_minimum(n)$minimum
  warns <- character(0)
  if (mono < gmin) {
    warns <- c(warns, sprintf(paste(
      "The count %d is below Goodman's minimum %d, which is impossible.",
      "The colouring or the arithmetic is wrong."), mono, gmin))
  }
  list(monochromatic = mono, bichromatic = bi, total_triangles = total,
       red_triangles = red_t, blue_triangles = blue_t,
       identity_residual = resid, goodman_minimum = gmin,
       attains_minimum = mono == gmin, red_degrees = r, n = n,
       warnings = warns,
       method = "Goodman (1959) monochromatic-triangle identity")
}

#' Least monochromatic triangles in any two-colouring of K_n
#'
#' Maximising \eqn{\sum_v r_v b_v} subject to \eqn{r_v + b_v = n-1} in
#' Goodman's identity gives
#' \deqn{\#mono \ge \binom{n}{3} - \frac{n}{2}
#'   \left\lfloor \frac{(n-1)^2}{4} \right\rfloor .}
#'
#' @param n Number of vertices, at least 3.
#' @return A list with `minimum`, `total_triangles`, `max_bichromatic`.
#' @references Goodman AW (1959) \emph{Amer Math Monthly} 66(9):778-783.
#' @export
#' @examples
#' morie_goodman_minimum(n = 5L)
morie_goodman_minimum <- function(n) {
  n <- as.integer(n)
  if (is.na(n) || n < 3L) {
    stop(sprintf("n must be at least 3; got %s", n), call. = FALSE)
  }
  total <- choose(n, 3)
  max_bi <- (n * ((n - 1L)^2 %/% 4L)) %/% 2L
  list(minimum = max(total - max_bi, 0), total_triangles = total,
       max_bichromatic = max_bi)
}

#' Verify a Ramsey lower-bound witness
#'
#' A bound \eqn{R(k,l) > n} is proved by exhibiting a colouring of
#' \eqn{K_n} with no red \eqn{K_k} and no blue \eqn{K_l}. This checks
#' one by exhaustive search over cliques, which is the only honest way
#' to accept a witness.
#'
#' @param colouring Symmetric 0/1 matrix.
#' @param k,l Clique sizes to rule out.
#' @return A list with `valid`, `red_clique`, `blue_clique`, `certifies`.
#' @export
#' @examples
#' morie_ramsey_witness(colouring = 5L, k = 5L, l = 5L)
morie_ramsey_witness <- function(colouring, k, l) {
  A <- as.matrix(colouring)
  if (nrow(A) != ncol(A)) stop("colouring must be square.", call. = FALSE)
  n <- nrow(A)
  R <- matrix(as.integer(A != 0), n, n)
  diag(R) <- 0L
  if (!identical(R, t(R))) stop("colouring must be symmetric.", call. = FALSE)
  k <- as.integer(k); l <- as.integer(l)

  find <- function(size, want_red) {
    if (size > n) return(NULL)
    cmb <- utils::combn(n, size)
    for (c in seq_len(ncol(cmb))) {
      v <- cmb[, c]
      pairs <- utils::combn(v, 2)
      ok <- TRUE
      for (p in seq_len(ncol(pairs))) {
        e <- R[pairs[1, p], pairs[2, p]]
        if (want_red && e != 1L) { ok <- FALSE; break }
        if (!want_red && e != 0L) { ok <- FALSE; break }
      }
      if (ok) return(as.integer(v) - 1L)
    }
    NULL
  }
  red <- find(k, TRUE)
  blue <- find(l, FALSE)
  valid <- is.null(red) && is.null(blue)
  list(valid = valid, red_clique = red, blue_clique = blue, n = n,
       certifies = if (valid) sprintf("R(%d,%d) > %d", k, l, n) else NULL)
}

#' The party problem: R(3,3) = 6, proved rather than quoted
#'
#' Among any six people, three are mutual acquaintances or three are
#' mutual strangers; five is not enough. The upper half follows from
#' Goodman's identity, which forces at least two monochromatic
#' triangles at \eqn{n = 6}. The lower half needs a witness, and the
#' five-cycle colouring is one -- acquaintance around a pentagon,
#' strangers on the diagonals, and neither the pentagon nor the
#' pentagram contains a triangle.
#'
#' @param n_people Size of the party, at least 3.
#' @return A list with `guaranteed`, `minimum_monochromatic`, `witness`,
#'   `witness_valid`, `ramsey_number`.
#' @references Goodman AW (1959); Greenwood RE, Gleason AM (1955).
#' @export
#' @examples
#' morie_party_problem()
morie_party_problem <- function(n_people = 6L) {
  n <- as.integer(n_people)
  if (is.na(n) || n < 3L) {
    stop(sprintf("n_people must be at least 3; got %s", n), call. = FALSE)
  }
  gmin <- morie_goodman_minimum(n)$minimum
  guaranteed <- gmin >= 1
  witness <- NULL
  valid <- NULL
  if (!guaranteed) {
    C <- matrix(0L, n, n)
    for (i in seq_len(n)) {
      j <- if (i == n) 1L else i + 1L
      C[i, j] <- 1L; C[j, i] <- 1L
    }
    witness <- C
    valid <- morie_ramsey_witness(C, 3L, 3L)$valid
  }
  list(n_people = n, guaranteed = guaranteed,
       minimum_monochromatic = gmin, witness = witness,
       witness_valid = valid, ramsey_number = 6L, n = n,
       method = "Party problem via Goodman's identity")
}
