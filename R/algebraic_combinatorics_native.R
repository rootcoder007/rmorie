# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Algebraic combinatorics. Mirrors morie.fn.algcmb.
#
# Two of these are bijections rather than formulas, and a bijection
# admits a stronger check than a count: RSK is round-tripped over every
# permutation of small n, and its corollary sum (f^lambda)^2 = n! is
# checked as an identity. Burnside is checked against orbits counted
# directly.
#
# The Polya here is Polya ENUMERATION -- counting orbits under a group
# action. It is unrelated to the Polya TREE priors in
# R/ghosal_native.R, which are Bayesian nonparametrics named after the
# same mathematician for a different reason.
#
# Sagan BE (2001) The Symmetric Group 2nd ed.; Stanley RP (1999)
# Enumerative Combinatorics vol 2 Ch 7; Frame, Robinson and Thrall
# (1954); Schensted C (1961); Knuth DE (1970); Burnside W (1897);
# Polya G (1937).

# ------------------------------------------------------------------
# Partitions
# ------------------------------------------------------------------

#' Every partition of an integer
#'
#' @param n Non-negative integer.
#' @param max_part Largest part allowed; defaults to `n`.
#' @return A list of integer vectors, each weakly decreasing.
#' @export
morie_partitions_of <- function(n, max_part = NULL) {
  n <- as.integer(n)
  if (is.na(n) || n < 0L) {
    stop(sprintf("n must be non-negative; got %s", n), call. = FALSE)
  }
  if (n == 0L) return(list(integer(0)))
  cap <- if (is.null(max_part)) n else min(as.integer(max_part), n)
  out <- list()
  for (first in seq.int(cap, 1L)) {
    for (rest in morie_partitions_of(n - first, first)) {
      out[[length(out) + 1L]] <- c(first, rest)
    }
  }
  out
}

# ------------------------------------------------------------------
# Hook lengths
# ------------------------------------------------------------------

#' Hook length of every cell of a Young diagram
#'
#' The hook of a cell is the cell itself, everything to its right in the
#' row, and everything below it in the column:
#' \eqn{h(i,j) = \lambda_i - j + \lambda'_j - i + 1}.
#'
#' @param shape Integer vector, weakly decreasing, all parts positive.
#' @return A list with `hooks` (a list of integer vectors, row-major),
#'   `product`, `shape`, `conjugate`, `n`.
#' @references Frame JS, Robinson G de B, Thrall RM (1954)
#'   \emph{Canad J Math} 6:316-324.
#' @export
morie_hook_lengths <- function(shape) {
  lam <- as.integer(shape)
  if (anyNA(lam) || any(lam <= 0L)) {
    stop(sprintf("every part must be positive; got %s",
                 paste(shape, collapse = ", ")), call. = FALSE)
  }
  if (length(lam) > 1L && any(diff(lam) > 0L)) {
    stop(sprintf("a partition must be weakly decreasing; got %s",
                 paste(shape, collapse = ", ")), call. = FALSE)
  }
  conj <- vapply(seq_len(lam[1]), function(j) sum(lam > j - 1L), integer(1))
  hooks <- vector("list", length(lam))
  for (i in seq_along(lam)) {
    j <- seq_len(lam[i])
    hooks[[i]] <- lam[i] - j + conj[j] - i + 1L
  }
  list(hooks = hooks, product = prod(as.numeric(unlist(hooks))),
       shape = lam, conjugate = conj, n = sum(lam))
}

# helper: exponent of prime p in n!, by Legendre's formula
.morie_legendre <- function(n, p) {
  e <- 0
  q <- p
  while (q <= n) {
    e <- e + n %/% q
    if (q > n / p) break
    q <- q * p
  }
  e
}

# helper: prime exponents of a small integer, indexed against `primes`
#
# Trial division stops once p^2 exceeds what is left, at which point the
# remainder is itself prime -- it must still be RECORDED, not discarded.
# Dropping it silently would understate the hook product and make the
# division look inexact.
.morie_factorise <- function(x, exps, primes) {
  for (idx in seq_along(primes)) {
    p <- primes[idx]
    if (p * p > x) break
    while (x %% p == 0) {
      exps[idx] <- exps[idx] + 1
      x <- x %/% p
    }
    if (x == 1) break
  }
  if (x > 1) {
    idx <- match(x, primes)
    if (is.na(idx)) return(list(exps = exps, remainder = x))
    exps[idx] <- exps[idx] + 1
  }
  list(exps = exps, remainder = 1)
}

#' The hook length formula
#'
#' \eqn{f^\lambda = n! / \prod_{(i,j)} h(i,j)} counts standard Young
#' tableaux of the given shape -- fillings by \eqn{1, \ldots, n}
#' increasing along every row and down every column.
#'
#' That a product of hook lengths divides \eqn{n!} exactly is not
#' obvious, so it is verified rather than assumed. The count is built
#' from prime exponents, which never overflows, and then multiplied
#' back against the hook product and compared with \eqn{n!} in
#' arbitrary precision. `remainder` reports the shortfall, which must
#' be zero for a genuine partition.
#'
#' Doubles stop being exact integers past \eqn{2^{53}}, so `factorial`,
#' `hook_product` and `count` are accompanied by exact decimal strings.
#'
#' @param shape Integer vector, a partition.
#' @return A list with `count`, `exact`, `factorial`, `hook_product`,
#'   `remainder`, `divides_exactly`, `hooks`, `shape`, `n`.
#' @export
morie_standard_tableaux_count <- function(shape) {
  h <- morie_hook_lengths(shape)
  n <- h$n
  hooks <- as.numeric(unlist(h$hooks))

  primes <- .morie_primes_upto(max(n, 2L))
  exps <- numeric(length(primes))
  for (idx in seq_along(primes)) {
    exps[idx] <- .morie_legendre(n, primes[idx])
  }
  negative <- FALSE
  for (hv in hooks) {
    f <- .morie_factorise(hv, numeric(length(primes)), primes)
    if (f$remainder > 1) {
      # a hook length can never exceed n, so every prime factor of it
      # is already in the sieve; reaching here means the sieve is wrong
      stop(sprintf("hook length %g has a prime factor above %d",
                   hv, max(primes)), call. = FALSE)
    }
    exps <- exps - f$exps
  }
  if (any(exps < 0)) negative <- TRUE

  count <- morie_bigint(1)
  if (!negative) {
    for (idx in seq_along(primes)) {
      if (exps[idx] > 0) {
        count <- morie_big_mul(count,
                               morie_big_pow(morie_bigint(primes[idx]),
                                             exps[idx]))
      }
    }
  } else {
    count <- morie_bigint(0)
  }

  fact <- morie_big_factorial(n)
  hprod <- morie_bigint(1)
  for (hv in hooks) hprod <- morie_big_mul(hprod, morie_bigint(hv))
  rem <- morie_big_sub(fact, morie_big_mul(count, hprod))
  divides <- morie_big_cmp(rem, morie_bigint(0)) == 0

  out <- list(
    count = as.numeric(as.character(count)),
    exact = as.character(count),
    estimate = as.numeric(as.character(count)),
    factorial = as.numeric(as.character(fact)),
    exact_factorial = as.character(fact),
    hook_product = h$product,
    exact_hook_product = as.character(hprod),
    remainder = as.numeric(as.character(rem)),
    exact_remainder = as.character(rem),
    divides_exactly = divides,
    fits_double = morie_big_fits_double(count),
    hooks = h$hooks, shape = h$shape, n = n,
    warnings = character(0),
    method = "Hook length formula (Frame, Robinson and Thrall 1954)")
  if (!divides) {
    out$warnings <- c(out$warnings, sprintf(paste(
      "The hook product %s does not divide %d! exactly (remainder %s),",
      "which cannot happen for a genuine partition. The shape or the",
      "hook computation is wrong."), as.character(hprod), n,
      as.character(rem)))
  }
  out
}

# small sieve; hook lengths never exceed n, so this is all we need
.morie_primes_upto <- function(n) {
  n <- as.integer(n)
  if (n < 2L) return(integer(0))
  sieve <- rep(TRUE, n)
  sieve[1] <- FALSE
  # seq_len, not 2:floor(sqrt(n)) -- for n < 4 the latter counts DOWN
  # from 2 to 1 and hands seq.int a negative step
  for (p in seq_len(floor(sqrt(n)))) {
    if (p < 2L) next
    if (sieve[p]) sieve[seq.int(p * p, n, by = p)] <- FALSE
  }
  which(sieve)
}

# ------------------------------------------------------------------
# Robinson-Schensted
# ------------------------------------------------------------------

#' Row-insert one value into a tableau, bumping
#'
#' @param tableau A list of integer vectors.
#' @param value The value to insert.
#' @return A list with `tableau` and `row`, the 1-BASED index of the row
#'   that gained a cell. (The Python mirror returns a 0-based index;
#'   the values differ by one by design, not by accident.)
#' @export
morie_rsk_insert <- function(tableau, value) {
  tt <- lapply(tableau, as.numeric)
  x <- as.numeric(value)
  for (i in seq_along(tt)) {
    row <- tt[[i]]
    pos <- which(row > x)
    if (length(pos) == 0L) {
      tt[[i]] <- c(row, x)
      return(list(tableau = tt, row = i))
    }
    j <- pos[1]
    tmp <- row[j]
    row[j] <- x
    x <- tmp
    tt[[i]] <- row
  }
  tt[[length(tt) + 1L]] <- x
  list(tableau = tt, row = length(tt))
}

#' The Robinson-Schensted correspondence
#'
#' Maps a permutation bijectively to a pair of standard Young tableaux
#' of the SAME shape. The shape is what makes it useful: the first row
#' length is the longest increasing subsequence and the first column
#' length is the longest decreasing one, so a purely combinatorial
#' statistic falls out of an algebraic construction.
#'
#' @param permutation Integer vector, a permutation of `1:n`.
#' @return A list with `p_tableau`, `q_tableau`, `shape`, `q_shape`,
#'   `same_shape`, `longest_increasing`, `longest_decreasing`.
#' @references Robinson G de B (1938); Schensted C (1961)
#'   \emph{Canad J Math} 13:179-191; Knuth DE (1970).
#' @export
morie_rsk_correspondence <- function(permutation) {
  w <- as.integer(permutation)
  n <- length(w)
  if (anyNA(w) || !identical(sort(w), seq_len(n))) {
    stop(sprintf("expected a permutation of 1..%d; got %s", n,
                 paste(permutation, collapse = ", ")), call. = FALSE)
  }
  p <- list()
  q <- list()
  for (step in seq_len(n)) {
    ins <- morie_rsk_insert(p, w[step])
    p <- ins$tableau
    while (length(q) < ins$row) q[[length(q) + 1L]] <- numeric(0)
    q[[ins$row]] <- c(q[[ins$row]], step)
  }
  shape <- vapply(p, length, integer(1))
  qshape <- vapply(q, length, integer(1))
  inc <- if (length(shape)) shape[1] else 0L
  dec <- length(shape)
  list(p_tableau = p, q_tableau = q, shape = shape, q_shape = qshape,
       same_shape = identical(shape, qshape),
       longest_increasing = inc, longest_decreasing = dec,
       estimate = as.numeric(inc), permutation = w, n = n,
       method = "Robinson-Schensted (Robinson 1938; Schensted 1961)")
}

#' Recover the permutation from its two tableaux
#'
#' RSK is a bijection, so this must return the original word exactly.
#' Round-tripping is the strongest available check on the forward map,
#' and is what the tests use.
#'
#' @param p_tableau,q_tableau Tableaux of the same shape.
#' @return The permutation, as an integer vector.
#' @export
morie_rsk_inverse <- function(p_tableau, q_tableau) {
  p <- lapply(p_tableau, as.numeric)
  q <- lapply(q_tableau, as.numeric)
  if (!identical(vapply(p, length, integer(1)),
                 vapply(q, length, integer(1)))) {
    stop("P and Q must have the same shape.", call. = FALSE)
  }
  n <- sum(vapply(p, length, integer(1)))
  word <- numeric(n)
  for (step in seq.int(n, 1L)) {
    row <- NA_integer_
    for (i in seq_along(q)) {
      if (length(q[[i]]) && q[[i]][length(q[[i]])] == step) { row <- i; break }
    }
    if (is.na(row)) {
      stop(sprintf("%d is not at the end of any row of Q.", step),
           call. = FALSE)
    }
    q[[row]] <- q[[row]][-length(q[[row]])]
    x <- p[[row]][length(p[[row]])]
    p[[row]] <- p[[row]][-length(p[[row]])]
    if (row > 1L) {
      for (i in seq.int(row - 1L, 1L)) {
        pos <- which(p[[i]] < x)
        j <- pos[length(pos)]
        tmp <- p[[i]][j]
        p[[i]][j] <- x
        x <- tmp
      }
    }
    word[step] <- x
    while (length(p) && length(p[[length(p)]]) == 0L) p[[length(p)]] <- NULL
    while (length(q) && length(q[[length(q)]]) == 0L) q[[length(q)]] <- NULL
  }
  as.integer(word)
}

# ------------------------------------------------------------------
# Burnside and Polya enumeration
# ------------------------------------------------------------------

#' Burnside's lemma
#'
#' The number of orbits is the AVERAGE number of fixed points,
#' \eqn{|X/G| = |G|^{-1} \sum_g |X^g|}, and for colourings of positions
#' \eqn{|X^g| = k^{c(g)}} with \eqn{c(g)} the number of cycles.
#'
#' The lemma is often stated as "divide by the symmetry", which is
#' wrong whenever some arrangements have extra symmetry of their own.
#' `naive_division` is returned to make the gap visible: two-colour
#' necklaces of length 4 have 16 colourings and 4 rotations but 6
#' orbits, not 4.
#'
#' @param group_permutations A list of integer vectors, each a
#'   permutation of `0:(n-1)`, matching the Python mirror's 0-based
#'   convention.
#' @param n_colours Number of colours.
#' @return A list with `orbits`, `fixed_points`, `cycle_counts`,
#'   `sum_fixed`, `naive_division`, `naive_is_wrong`, `divides_exactly`.
#' @export
morie_burnside_orbit_count <- function(group_permutations, n_colours) {
  g <- lapply(group_permutations, as.integer)
  k <- as.integer(n_colours)
  if (length(g) == 0L) {
    stop("the group must contain at least the identity.", call. = FALSE)
  }
  n <- length(g[[1]])
  if (any(vapply(g, length, integer(1)) != n)) {
    stop("every group element must permute the same set.", call. = FALSE)
  }
  if (is.na(k) || k < 1L) {
    stop(sprintf("n_colours must be positive; got %s", n_colours),
         call. = FALSE)
  }
  for (el in g) {
    if (anyNA(el) || !identical(sort(el), seq.int(0L, n - 1L))) {
      stop(sprintf("%s is not a permutation of 0 .. %d",
                   paste(el, collapse = ", "), n - 1L), call. = FALSE)
    }
  }
  cyc <- vapply(g, function(el) {
    seen <- rep(FALSE, n)
    count <- 0L
    for (i in seq_len(n)) {
      if (seen[i]) next
      count <- count + 1L
      j <- i
      while (!seen[j]) { seen[j] <- TRUE; j <- el[j] + 1L }
    }
    count
  }, integer(1))
  fixed <- k^as.numeric(cyc)
  total <- sum(fixed)
  gorder <- length(g)
  orbits <- floor(total / gorder)
  rem <- total - orbits * gorder
  total_col <- k^as.numeric(n)
  naive <- total_col / gorder
  out <- list(orbits = orbits, estimate = orbits,
              exact = format(orbits, scientific = FALSE, trim = TRUE),
              fixed_points = fixed, cycle_counts = cyc, sum_fixed = total,
              group_order = gorder, total_colourings = total_col,
              naive_division = naive,
              naive_is_wrong = abs(naive - orbits) > 1e-12,
              divides_exactly = rem == 0, n = n,
              warnings = character(0), method = "Burnside's lemma")
  if (rem != 0) {
    out$warnings <- c(out$warnings, sprintf(paste(
      "The fixed-point total %g is not divisible by the group order %d,",
      "which is impossible if the supplied permutations really form a",
      "group. Check closure."), total, gorder))
  }
  if (total > 2^53) {
    out$warnings <- c(out$warnings, paste(
      "The fixed-point total exceeds 2^53, where doubles stop being exact",
      "integers, so the orbit count may be off by a small amount."))
  }
  out
}

#' Necklaces: colourings up to rotation
#'
#' Burnside applied to the cyclic group gives
#' \eqn{n^{-1} \sum_{d \mid n} \varphi(n/d) k^d}. Computed both that way
#' and by direct Burnside over the explicit rotations, then compared --
#' the closed form is a shortcut, and shortcuts are where errors hide.
#'
#' @param n Number of beads.
#' @param k Number of colours.
#' @return A list with `count`, `direct_burnside`, `agrees`,
#'   `divides_exactly`, `total_colourings`.
#' @references Polya G (1937). Kombinatorische Anzahlbestimmungen fuer
#'   Gruppen, Graphen und chemische Verbindungen. \emph{Acta
#'   Mathematica}, 68, 145-254.
#' @export
morie_cycle_index_necklaces <- function(n, k) {
  n <- as.integer(n); k <- as.integer(k)
  if (is.na(n) || n < 1L) {
    stop(sprintf("n must be positive; got %s", n), call. = FALSE)
  }
  if (is.na(k) || k < 1L) {
    stop(sprintf("k must be positive; got %s", k), call. = FALSE)
  }
  phi <- function(m) {
    r <- m; mm <- m; p <- 2L
    while (p * p <= mm) {
      if (mm %% p == 0L) {
        while (mm %% p == 0L) mm <- mm %/% p
        r <- r - r %/% p
      }
      p <- p + 1L
    }
    if (mm > 1L) r <- r - r %/% mm
    r
  }
  divs <- seq_len(n)[n %% seq_len(n) == 0L]
  total <- sum(vapply(divs, function(d) phi(n %/% d) * k^as.numeric(d),
                      numeric(1)))
  count <- floor(total / n)
  rem <- total - count * n
  rot <- lapply(seq.int(0L, n - 1L), function(s) (seq.int(0L, n - 1L) + s) %% n)
  direct <- morie_burnside_orbit_count(rot, k)$orbits
  list(count = count, estimate = count,
       exact = format(count, scientific = FALSE, trim = TRUE),
       direct_burnside = direct, agrees = count == direct,
       divides_exactly = rem == 0, total_colourings = k^as.numeric(n),
       n = n, k = k,
       method = "Cycle index of the cyclic group (Polya enumeration)")
}
