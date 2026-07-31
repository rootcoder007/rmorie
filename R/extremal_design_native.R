# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Extremal combinatorics and design theory.
# Mirrors morie.fn.extrgt and morie.fn.dsgnth.
#
# Both shelves are exactly checkable rather than checkable to a
# tolerance: every bound here is attained by an explicit construction,
# and the parity tests build the construction and verify it rather than
# comparing a float.
#
# Bollobas B (2004), Extremal Graph Theory. Turan P (1941); Mantel W
# (1907); Sperner E (1928); Erdos P, Ko C, Rado R (1961); Dilworth RP
# (1950). Stinson DR (2004), Combinatorial Designs. Fisher RA (1940);
# Kirkman TP (1847); Bose RC (1939); Hamming RW (1950); Singleton RC
# (1964).

#' Number of edges in a simple undirected graph
#'
#' @param adjacency Symmetric 0/1 matrix.
#' @return Integer edge count.
#' @export
morie_count_edges <- function(adjacency) {
  A <- as.matrix(adjacency)
  sum(A[upper.tri(A)] != 0)
}

#' Does the graph contain a clique on k vertices?
#'
#' Exhaustive, which is the only honest answer for a certificate.
#'
#' @param adjacency Symmetric 0/1 matrix.
#' @param k Clique size.
#' @return The clique as an integer vector, or NULL.
#' @export
morie_has_clique <- function(adjacency, k) {
  A <- as.matrix(adjacency)
  n <- nrow(A)
  k <- as.integer(k)
  if (k <= 0L) return(integer(0))
  if (k > n) return(NULL)
  cmb <- utils::combn(n, k)
  for (c in seq_len(ncol(cmb))) {
    v <- cmb[, c]
    pr <- utils::combn(v, 2)
    if (all(A[cbind(pr[1, ], pr[2, ])] != 0)) return(as.integer(v))
  }
  NULL
}

#' The Turan graph: complete r-partite with parts as equal as possible
#'
#' It contains no \eqn{K_{r+1}}, because any \eqn{r+1} vertices must
#' repeat a part and two vertices in the same part are non-adjacent.
#'
#' @param n Vertices.
#' @param r Number of parts.
#' @return A list with `adjacency`, `parts`, `edges`.
#' @export
morie_turan_graph <- function(n, r) {
  n <- as.integer(n); r <- as.integer(r)
  if (n < 0L) stop(sprintf("n must be non-negative; got %d", n), call. = FALSE)
  if (r < 1L) stop(sprintf("r must be at least 1; got %d", r), call. = FALSE)
  A <- matrix(0L, n, n)
  part_of <- if (n > 0L) ((seq_len(n) - 1L) %% r) else integer(0)
  if (n > 1L) {
    for (i in seq_len(n - 1L)) {
      for (j in seq.int(i + 1L, n)) {
        if (part_of[i] != part_of[j]) { A[i, j] <- 1L; A[j, i] <- 1L }
      }
    }
  }
  parts <- lapply(seq_len(r) - 1L, function(p) which(part_of == p))
  list(adjacency = A, parts = parts, edges = morie_count_edges(A))
}

#' Turan's theorem: most edges with no clique on r+1 vertices
#'
#' The maximum is attained by the complete \eqn{r}-partite graph with
#' parts as equal as possible, and that graph is the unique extremal
#' example.
#'
#' The familiar \eqn{(1 - 1/r)n^2/2} is exact ONLY when \eqn{r} divides
#' \eqn{n}; otherwise it is an upper bound. At \eqn{n = 10, r = 3} it
#' gives 33.33 against an exact 33. Both are returned so the looseness
#' is visible.
#'
#' @param n Vertices.
#' @param r Forbid \eqn{K_{r+1}}.
#' @return A list with `count`, `rounded_formula`, `part_sizes`,
#'   `attained`, `formula_is_exact`.
#' @references Turan P (1941). Bollobas B (2004), Ch VI.
#' @export
morie_turan_number <- function(n, r) {
  n <- as.integer(n); r <- as.integer(r)
  if (n < 0L) stop(sprintf("n must be non-negative; got %d", n), call. = FALSE)
  if (r < 1L) stop(sprintf("r must be at least 1; got %d", r), call. = FALSE)
  sizes <- (n %/% r) + as.integer(seq_len(r) <= (n %% r))
  exact <- choose(n, 2) - sum(choose(sizes, 2))
  approx <- (1 - 1 / r) * n * n / 2
  g <- morie_turan_graph(n, r)
  list(count = exact, rounded_formula = approx,
       formula_is_exact = abs(approx - exact) < 1e-9,
       part_sizes = sizes, construction_edges = g$edges,
       attained = g$edges == exact, forbidden_clique = r + 1L,
       n = n, r = r, method = "Turan's theorem (Turan 1941)")
}

#' Mantel's theorem: the triangle-free case
#'
#' \eqn{\lfloor n^2/4 \rfloor} edges, attained by the balanced complete
#' bipartite graph. The floor matters at odd \eqn{n}.
#'
#' @param n Vertices.
#' @return As [morie_turan_number()], with `floor_n2_over_4`.
#' @references Mantel W (1907). Problem 28. \emph{Wiskundige Opgaven},
#'   10, 60-61.
#' @export
morie_mantel_number <- function(n) {
  out <- morie_turan_number(n, 2L)
  out$floor_n2_over_4 <- (as.integer(n)^2) %/% 4L
  out$method <- "Mantel's theorem (Mantel 1907)"
  out
}

#' Sperner's theorem: the largest antichain in the subset lattice
#'
#' No family of subsets in which none contains another exceeds
#' \eqn{\binom{n}{\lfloor n/2 \rfloor}}. At even \eqn{n} the extremal
#' family is unique; at odd \eqn{n} the two central layers both attain
#' it.
#'
#' @param n Ground set size.
#' @return A list with `count`, `extremal_layers`, `unique_extremal`.
#' @references Sperner E (1928). Ein Satz ueber Untermengen einer
#'   endlichen Menge. \emph{Mathematische Zeitschrift}, 27(1), 544-548.
#' @export
morie_sperner_width <- function(n) {
  n <- as.integer(n)
  if (n < 0L) stop(sprintf("n must be non-negative; got %d", n), call. = FALSE)
  w <- choose(n, n %/% 2L)
  layers <- if (n %% 2L == 0L) n %/% 2L else c(n %/% 2L, n %/% 2L + 1L)
  list(count = w, extremal_layers = layers,
       unique_extremal = n %% 2L == 0L, total_subsets = 2^n, n = n,
       method = "Sperner's theorem (Sperner 1928)")
}

#' Erdos-Ko-Rado: the largest intersecting family of k-subsets
#'
#' For \eqn{n \ge 2k} the maximum is \eqn{\binom{n-1}{k-1}}, attained by
#' the star of all \eqn{k}-sets through a fixed element.
#'
#' The condition \eqn{n \ge 2k} is not decoration. BELOW it no two
#' \eqn{k}-subsets can be disjoint at all, so every family is
#' intersecting and the answer is the trivial \eqn{\binom{n}{k}};
#' quoting the star bound there gives an answer that is too small and
#' looks reasonable. `ekr_regime` reports which case applies.
#'
#' @param n,k Ground set and subset sizes.
#' @return A list with `count`, `star_size`, `ekr_regime`, `warnings`.
#' @references Erdos P, Ko C, Rado R (1961) \emph{Quart J Math}
#'   12:313-320.
#' @export
morie_erdos_ko_rado <- function(n, k) {
  n <- as.integer(n); k <- as.integer(k)
  if (n < 0L || k < 0L) stop("n and k must be non-negative.", call. = FALSE)
  if (k > n) {
    stop(sprintf("k must not exceed n; got k = %d, n = %d", k, n),
         call. = FALSE)
  }
  regime <- n >= 2L * k
  star <- if (k >= 1L) choose(n - 1L, k - 1L) else 0
  count <- if (regime) star else choose(n, k)
  warns <- character(0)
  if (!regime) {
    warns <- sprintf(paste(
      "n = %d is below 2k = %d, outside the Erdos-Ko-Rado regime. The star",
      "bound C(n-1, k-1) = %g is NOT the answer here; it is smaller than the",
      "truth because every family is intersecting when no two k-sets can be",
      "disjoint."), n, 2L * k, star)
  }
  list(count = count, star_size = star, all_k_sets = choose(n, k),
       ekr_regime = regime, n = n, k = k, warnings = warns,
       method = "Erdos-Ko-Rado theorem (1961)")
}

#' Dilworth's theorem, with both sides computed
#'
#' The largest antichain in a finite poset equals the fewest chains
#' needed to cover it. Both are computed -- the antichain exhaustively,
#' the chain cover by a bipartite matching -- and the equality is then
#' CHECKED rather than assumed, since equality is the content.
#'
#' @param leq Square logical matrix; `leq\[i, j\]` when \eqn{i \le j}.
#' @return A list with `antichain_size`, `chain_cover_size`,
#'   `dilworth_holds`, `antichain`, `warnings`.
#' @references Dilworth RP (1950) \emph{Annals of Mathematics}
#'   51(1):161-166.
#' @export
morie_dilworth_decomposition <- function(leq) {
  M <- matrix(as.logical(as.matrix(leq)), nrow(leq), ncol(leq))
  n <- nrow(M)
  if (ncol(M) != n) stop("leq must be square.", call. = FALSE)
  for (i in seq_len(n)) {
    if (!M[i, i]) {
      stop(sprintf("leq must be reflexive; element %d is not.", i),
           call. = FALSE)
    }
  }
  for (i in seq_len(n)) for (j in seq_len(n)) {
    if (i != j && M[i, j] && M[j, i]) {
      stop(sprintf(paste("leq must be antisymmetric; %d and %d are mutually",
                         "below one another."), i, j), call. = FALSE)
    }
  }
  for (i in seq_len(n)) for (j in seq_len(n)) for (k in seq_len(n)) {
    if (M[i, j] && M[j, k] && !M[i, k]) {
      stop(sprintf(paste("leq must be transitive; %d <= %d <= %d but not",
                         "%d <= %d."), i, j, k, i, k), call. = FALSE)
    }
  }
  strict <- M & !diag(TRUE, n)

  match_v <- rep(0L, n)
  try_aug <- function(u, seen) {
    for (v in seq_len(n)) {
      if (strict[u, v] && !seen[v]) {
        seen[v] <<- TRUE
        if (match_v[v] == 0L || try_aug(match_v[v], seen)) {
          match_v[v] <<- u
          return(TRUE)
        }
      }
    }
    FALSE
  }
  size <- 0L
  for (u in seq_len(n)) {
    seen <- rep(FALSE, n)
    if (try_aug(u, seen)) size <- size + 1L
  }
  chain_cover <- n - size

  best <- integer(0)
  for (sz in seq.int(n, 1L)) {
    cmb <- utils::combn(n, sz)
    found <- NULL
    for (c in seq_len(ncol(cmb))) {
      v <- cmb[, c]
      if (sz == 1L) { found <- v; break }
      pr <- utils::combn(v, 2)
      if (!any(strict[cbind(pr[1, ], pr[2, ])] |
               strict[cbind(pr[2, ], pr[1, ])])) { found <- v; break }
    }
    if (!is.null(found)) { best <- as.integer(found); break }
  }
  warns <- character(0)
  if (length(best) != chain_cover) {
    warns <- sprintf(paste(
      "The antichain (%d) and chain cover (%d) disagree, contradicting",
      "Dilworth's theorem. One computation is wrong, or the relation is not",
      "a partial order."), length(best), chain_cover)
  }
  list(antichain_size = length(best), chain_cover_size = chain_cover,
       dilworth_holds = length(best) == chain_cover, antichain = best,
       n = n, warnings = warns, method = "Dilworth's theorem (1950)")
}

#' Necessary conditions for a balanced incomplete block design
#'
#' Counting incidences two ways forces \eqn{r = \lambda(v-1)/(k-1)} and
#' \eqn{b = vr/k}, both whole; Fisher's inequality adds \eqn{b \ge v}.
#'
#' THESE ARE NECESSARY, NOT SUFFICIENT. (22, 7, 2) passes every one of
#' them and cannot exist, by Bruck-Ryser-Chowla. `feasible` and
#' `exists` are therefore separate, and `exists` is left NULL wherever
#' the arithmetic cannot settle it rather than reporting feasibility as
#' existence.
#'
#' @param v,k,lam Design parameters.
#' @return A list with `r`, `b`, `divisibility_ok`, `fisher_ok`,
#'   `feasible`, `exists`, `warnings`.
#' @references Fisher RA (1940). Stinson DR (2004).
#' @export
morie_bibd_parameters <- function(v, k, lam) {
  v <- as.integer(v); k <- as.integer(k); lam <- as.integer(lam)
  if (v < 1L || k < 1L || lam < 1L) {
    stop("v, k and lambda must be positive.", call. = FALSE)
  }
  if (k > v) {
    stop(sprintf("block size k = %d cannot exceed v = %d", k, v),
         call. = FALSE)
  }
  if (k < 2L) {
    stop(sprintf("block size k must be at least 2; got %d", k), call. = FALSE)
  }
  num_r <- lam * (v - 1L); den_r <- k - 1L
  r_ok <- num_r %% den_r == 0L
  r <- if (r_ok) num_r %/% den_r else NULL
  b_ok <- FALSE; b <- NULL
  if (!is.null(r)) {
    b_ok <- (v * r) %% k == 0L
    if (b_ok) b <- (v * r) %/% k
  }
  div_ok <- r_ok && b_ok
  fisher_ok <- !is.null(b) && b >= v
  feasible <- div_ok && fisher_ok

  exists <- NULL; note <- ""
  if (!feasible) {
    exists <- FALSE; note <- "ruled out by the counting or Fisher conditions"
  } else if (v == 22L && k == 7L && lam == 2L) {
    exists <- FALSE
    note <- paste("ruled out by Bruck-Ryser-Chowla despite passing every",
                  "counting condition")
  } else if (k == v) {
    exists <- TRUE
    note <- "trivial: the single block containing every point"
  }
  warns <- character(0)
  if (feasible && is.null(exists)) {
    warns <- c(warns, paste(
      "The counting conditions and Fisher's inequality are NECESSARY, not",
      "sufficient. Their passing does not establish that the design exists",
      "-- (22, 7, 2) passes all of them and is impossible by",
      "Bruck-Ryser-Chowla. `exists` is left undetermined here."))
  }
  if (identical(exists, FALSE) && feasible) {
    warns <- c(warns, sprintf(paste(
      "This parameter set is arithmetically feasible but the design does not",
      "exist: %s."), note))
  }
  list(v = v, k = k, lambda = lam, r = r, b = b,
       divisibility_ok = div_ok, r_integral = r_ok, b_integral = b_ok,
       fisher_ok = fisher_ok, feasible = feasible, exists = exists,
       note = note, n = v, warnings = warns,
       method = "BIBD necessary conditions with Fisher's inequality")
}

#' Steiner triple systems, with the Bose construction verified
#'
#' An STS(v) exists exactly when \eqn{v \equiv 1} or \eqn{3 \pmod 6}.
#' Unlike the general BIBD case this condition is both necessary AND
#' sufficient, which is why a definite answer is possible here.
#'
#' When `construct` and \eqn{v \equiv 3 \pmod 6}, the Bose construction
#' is carried out and checked: every pair covered exactly once.
#'
#' @param v Number of points.
#' @param construct Build and verify the system where possible.
#' @return A list with `exists`, `n_triples`, `triples`, `verified`.
#' @references Kirkman TP (1847); Bose RC (1939).
#' @export
morie_steiner_triple_system <- function(v, construct = TRUE) {
  v <- as.integer(v)
  if (v < 0L) stop(sprintf("v must be non-negative; got %d", v), call. = FALSE)
  exists <- v %in% c(0L, 1L) || (v %% 6L) %in% c(1L, 3L)
  n_triples <- if (exists) (v * (v - 1L)) %/% 6L else NULL
  triples <- NULL; verified <- NULL

  if (isTRUE(construct) && exists && v >= 3L && v %% 6L == 3L) {
    m <- v %/% 3L
    idx <- function(i, j) i * 3L + j + 1L
    half <- (m + 1L) %/% 2L
    tl <- list()
    for (i in seq_len(m) - 1L) {
      tl[[length(tl) + 1L]] <- c(idx(i, 0L), idx(i, 1L), idx(i, 2L))
    }
    for (j in seq_len(3L) - 1L) {
      for (a in seq_len(m) - 1L) {
        for (b in seq_len(m) - 1L) {
          if (b <= a) next
          mm <- ((a + b) * half) %% m
          tl[[length(tl) + 1L]] <- c(idx(a, j), idx(b, j),
                                     idx(mm, (j + 1L) %% 3L))
        }
      }
    }
    triples <- tl
    seen <- new.env(parent = emptyenv())
    for (t in tl) {
      s <- sort(t)
      for (p in list(c(s[1], s[2]), c(s[1], s[3]), c(s[2], s[3]))) {
        key <- paste(p, collapse = "-")
        seen[[key]] <- (if (is.null(seen[[key]])) 0L else seen[[key]]) + 1L
      }
    }
    vals <- unlist(as.list(seen), use.names = FALSE)
    verified <- length(tl) == n_triples && length(vals) == choose(v, 2) &&
      all(vals == 1L)
  }
  warns <- character(0)
  if (identical(verified, FALSE)) {
    warns <- paste("The Bose construction did not produce a valid system:",
                   "some pair is covered a number of times other than once.")
  }
  list(v = v, exists = exists, n_triples = n_triples, triples = triples,
       verified = verified, condition = "v = 1 or 3 (mod 6)",
       condition_is_sufficient = TRUE, n = v, warnings = warns,
       method = "Steiner triple system (Kirkman 1847; Bose 1939)")
}

#' A Latin square of order n
#'
#' The cyclic construction \eqn{L\[i\]\[j\] = (i + j) \bmod n} works for
#' every \eqn{n}, so Latin squares exist unconditionally.
#'
#' @param n Order.
#' @param method "cyclic" or "shifted".
#' @return A list with `square`, `order`, `valid`.
#' @export
morie_latin_square <- function(n, method = c("cyclic", "shifted")) {
  method <- match.arg(method)
  n <- as.integer(n)
  if (n < 1L) stop(sprintf("n must be at least 1; got %d", n), call. = FALSE)
  i <- seq_len(n) - 1L
  L <- if (method == "cyclic") outer(i, i, function(a, b) (a + b) %% n) else
    outer(i, i, function(a, b) (a * 2L + b) %% n)
  list(square = L, order = n, valid = morie_is_latin_square(L)$valid,
       method = method)
}

#' Is every symbol used once in each row and each column?
#'
#' @param square A square matrix of symbols.
#' @return A list with `valid`, `rows_ok`, `columns_ok`.
#' @export
morie_is_latin_square <- function(square) {
  L <- as.matrix(square)
  n <- nrow(L)
  if (n == 0L) stop("the square must not be empty.", call. = FALSE)
  if (ncol(L) != n) stop("the square must be square.", call. = FALSE)
  rows_ok <- all(apply(L, 1L, function(r) length(unique(r)) == n))
  cols_ok <- all(apply(L, 2L, function(c) length(unique(c)) == n))
  syms_ok <- length(unique(as.vector(L))) == n
  list(valid = rows_ok && cols_ok && syms_ok, rows_ok = rows_ok,
       columns_ok = cols_ok, symbol_count_ok = syms_ok, order = n)
}

#' Are two Latin squares orthogonal?
#'
#' Every ordered pair of symbols must occur exactly once when the two
#' are superimposed. BOTH squares are validated first: orthogonality is
#' a statement about Latin squares, and the pair condition can hold on a
#' grid that is not one, where the claim would be meaningless.
#'
#' Euler conjectured in 1782 that no pair exists for
#' \eqn{n \equiv 2 \pmod 4}. He was right at \eqn{n = 2} and
#' \eqn{n = 6}, and wrong for every larger such \eqn{n} -- Bose,
#' Shrikhande and Parker constructed a pair of order 10 in 1960, 178
#' years later.
#'
#' @param square_a,square_b Two squares of the same order.
#' @return A list with `orthogonal`, `pair_condition_holds`,
#'   `both_are_latin`, `pairs_seen`.
#' @export
morie_are_orthogonal <- function(square_a, square_b) {
  A <- as.matrix(square_a); B <- as.matrix(square_b)
  n <- nrow(A)
  if (nrow(B) != n) {
    stop("the two squares must have the same order.", call. = FALSE)
  }
  if (ncol(A) != n || ncol(B) != n) {
    stop("both squares must be square.", call. = FALSE)
  }
  va <- morie_is_latin_square(A)$valid
  vb <- morie_is_latin_square(B)$valid
  pairs <- paste(as.vector(A), as.vector(B), sep = ",")
  tb <- table(pairs)
  cond <- length(tb) == n * n && all(tb == 1L)
  list(orthogonal = cond && va && vb, pair_condition_holds = cond,
       both_are_latin = va && vb, first_is_latin = va, second_is_latin = vb,
       pairs_seen = length(tb), pairs_needed = n * n, order = n)
}

#' The Hamming sphere-packing bound
#'
#' A code correcting \eqn{t = \lfloor (d-1)/2 \rfloor} errors has
#' disjoint balls of radius \eqn{t}, so
#' \eqn{|C| \le q^n / \sum_{i \le t} \binom{n}{i}(q-1)^i}.
#'
#' A code meeting it is PERFECT, and perfect codes barely exist: over a
#' prime power alphabet the only ones are the trivial codes, the Hamming
#' codes and the two Golay codes.
#'
#' @param n Length.
#' @param d Minimum distance.
#' @param q Alphabet size.
#' @return A list with `bound`, `ball_volume`, `is_perfect_possible`.
#' @references Hamming RW (1950) \emph{Bell Syst Tech J} 29:147-160.
#' @export
morie_hamming_bound <- function(n, d, q = 2) {
  n <- as.integer(n); d <- as.integer(d); q <- as.integer(q)
  if (n < 1L || d < 1L || q < 2L) {
    stop("n and d must be positive and q at least 2.", call. = FALSE)
  }
  if (d > n) {
    stop(sprintf("d must not exceed n; got d = %d, n = %d", d, n),
         call. = FALSE)
  }
  t <- (d - 1L) %/% 2L
  i <- seq_len(t + 1L) - 1L
  vol <- sum(choose(n, i) * (q - 1)^i)
  total <- morie_big_pow(q, n)
  dm <- morie_big_divmod_small(total, vol)
  list(bound = as.numeric(as.character(dm$quotient)),
       bound_exact = as.character(dm$quotient),
       errors_corrected = t, ball_volume = vol,
       total_words = as.character(total),
       is_perfect_possible = dm$remainder == 0,
       n = n, d = d, q = q,
       method = "Hamming sphere-packing bound (Hamming 1950)")
}

#' The Singleton bound
#'
#' \eqn{|C| \le q^{n-d+1}}. Codes meeting it are maximum distance
#' separable, and unlike perfect codes MDS codes are plentiful.
#'
#' @param n Length.
#' @param d Minimum distance.
#' @param q Alphabet size.
#' @return A list with `bound`, `hamming_bound`, `tighter`.
#' @references Singleton RC (1964) \emph{IEEE Trans Inform Theory}
#'   10(2):116-118.
#' @export
morie_singleton_bound <- function(n, d, q = 2) {
  n <- as.integer(n); d <- as.integer(d); q <- as.integer(q)
  if (n < 1L || d < 1L || q < 2L) {
    stop("n and d must be positive and q at least 2.", call. = FALSE)
  }
  if (d > n) {
    stop(sprintf("d must not exceed n; got d = %d, n = %d", d, n),
         call. = FALSE)
  }
  b <- morie_big_pow(q, n - d + 1L)
  ham <- morie_hamming_bound(n, d, q)
  bn <- as.numeric(as.character(b))
  list(bound = bn, bound_exact = as.character(b),
       hamming_bound = ham$bound, tighter = min(bn, ham$bound),
       hamming_is_tighter = ham$bound < bn, n = n, d = d, q = q,
       method = "Singleton bound (Singleton 1964)")
}

#' Verify a block design directly from its blocks
#'
#' Counts how many blocks contain each point and each pair. This checks
#' that a claimed design IS one, as opposed to having parameters that
#' would permit one.
#'
#' @param blocks A list of integer vectors, points numbered from 1.
#' @param v Number of points.
#' @return A list with `is_bibd`, `k`, `r`, `lambda`, `uncovered_pairs`.
#' @export
morie_incidence_check <- function(blocks, v) {
  v <- as.integer(v)
  if (v < 1L) stop(sprintf("v must be positive; got %d", v), call. = FALSE)
  B <- lapply(blocks, function(b) sort(unique(as.integer(b))))
  if (any(vapply(B, function(b) any(b < 1L | b > v), logical(1)))) {
    stop(sprintf("every point must lie in 1 .. %d", v), call. = FALSE)
  }
  sizes <- unique(vapply(B, length, integer(1)))
  point <- integer(v)
  for (b in B) for (p in b) point[p] <- point[p] + 1L
  pair <- new.env(parent = emptyenv())
  for (b in B) {
    if (length(b) >= 2L) {
      pr <- utils::combn(b, 2)
      for (c in seq_len(ncol(pr))) {
        key <- paste(pr[, c], collapse = "-")
        pair[[key]] <- (if (is.null(pair[[key]])) 0L else pair[[key]]) + 1L
      }
    }
  }
  vals <- unlist(as.list(pair), use.names = FALSE)
  n_pairs <- choose(v, 2)
  uncovered <- n_pairs - length(vals)
  counts <- unique(c(vals, if (uncovered > 0) 0L else integer(0)))
  reps <- unique(point)
  is_bibd <- length(sizes) == 1L && length(reps) == 1L &&
    length(counts) == 1L && uncovered == 0
  # read each ONCE; an earlier Python version popped from the set while
  # building its summary and then read an emptied set in the payload
  k_val <- if (length(sizes) == 1L) sizes else NA_integer_
  r_val <- if (length(reps) == 1L) reps else NA_integer_
  lam_val <- if (length(counts) == 1L) counts else NA_integer_
  warns <- character(0)
  if (uncovered > 0) {
    warns <- c(warns, sprintf(paste(
      "%d pairs appear in no block, so this is not a design covering every",
      "pair."), uncovered))
  }
  if (length(sizes) > 1L) {
    warns <- c(warns, sprintf(
      "Blocks have differing sizes %s; a BIBD needs them uniform.",
      paste(sort(sizes), collapse = ", ")))
  }
  list(is_bibd = is_bibd, b = length(B), v = v, k = k_val, r = r_val,
       lambda = lam_val, block_sizes = sort(sizes),
       point_replications = point, uncovered_pairs = uncovered,
       n = v, warnings = warns,
       method = "Direct verification of a block design")
}
