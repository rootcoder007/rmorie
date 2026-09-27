# Transferable-utility games: nucleolus, prenucleolus and kernel points.
# A game on n players is the vector v of length 2^n - 1 in binary order:
# v[S] is the worth of the coalition with bitmask S (player i in S when
# bit i - 1 is set).

.tu_players <- function(v) {
  n <- round(log2(length(v) + 1))
  if (2^n - 1 != length(v)) stop("v must have length 2^n - 1 (binary order)", call. = FALSE)
  as.integer(n)
}

.tu_ind <- function(S, n) as.numeric(bitwAnd(S, 2L^(seq_len(n) - 1L)) > 0)

.tu_worth <- function(v, S) if (S == 0) 0 else v[S]

.tu_excess <- function(v, S, x) {
  s <- 0
  for (i in which(.tu_ind(S, length(x)) > 0)) s <- s + x[i]
  .tu_worth(v, S) - s
}

.tu_max_surplus <- function(v, x, i, j) {
  n <- length(x)
  best <- -Inf
  for (S in seq_len(2^n - 1)) {
    if (bitwAnd(S, 2L^(i - 1L)) > 0 && bitwAnd(S, 2L^(j - 1L)) == 0) best <- max(best, .tu_excess(v, S, x))
  }
  best
}

.tu_rank <- function(rows, n) {
  M <- matrix(unlist(rows), ncol = n, byrow = TRUE)
  rank <- 0L
  for (c in seq_len(n)) {
    if (rank >= nrow(M)) break
    cand <- (rank + 1L):nrow(M)
    p <- cand[which.max(abs(M[cand, c]))]
    if (abs(M[p, c]) < 1e-9) next
    rank <- rank + 1L
    tmp <- M[rank, ]
    M[rank, ] <- M[p, ]
    M[p, ] <- tmp
    for (r in seq_len(nrow(M))) {
      if (r != rank && M[r, c] != 0) M[r, ] <- M[r, ] - M[r, c] / M[rank, c] * M[rank, ]
    }
  }
  rank
}

# min cx.x + ct*t s.t. rows (a, at, rhs): a.x + at*t <= rhs (le) or = rhs (eq);
# x = lower + p when lower is given, else p - q; t = t+ - t-.
.tu_lp <- function(n, le, eq, cx, ct, lower) {
  free <- is.null(lower)
  nx <- if (free) 2L * n else n
  rows <- c(le, eq)
  ncol <- nx + 2L + length(le)
  A <- matrix(0, length(rows), ncol)
  b <- numeric(length(rows))
  for (k in seq_along(rows)) {
    a <- rows[[k]][[1]]
    A[k, seq_len(n)] <- a
    if (free) A[k, n + seq_len(n)] <- -a
    A[k, nx + 1:2] <- c(rows[[k]][[2]], -rows[[k]][[2]])
    if (k <= length(le)) A[k, nx + 2L + k] <- 1
    lo <- 0
    if (!free) for (i in seq_len(n)) lo <- lo + a[i] * lower[i]
    b[k] <- rows[[k]][[3]] - lo
  }
  cv <- numeric(ncol)
  cv[seq_len(n)] <- cx
  if (free) cv[n + seq_len(n)] <- -cx
  cv[nx + 1:2] <- c(ct, -ct)
  sol <- .lp_simplex(cv, A, b)
  if (sol$status != "optimal") stop("linear program ", sol$status, call. = FALSE)
  z <- sol$x
  x <- if (free) z[seq_len(n)] - z[n + seq_len(n)] else lower + z[seq_len(n)]
  list(x = x, t = z[nx + 1] - z[nx + 2])
}

.tu_sequential_nucleolus <- function(v, pre, tol = 1e-9) {
  n <- .tu_players(v)
  full <- 2L^n - 1L
  lower <- if (pre) NULL else vapply(seq_len(n), function(i) .tu_worth(v, 2L^(i - 1L)), 0)
  active <- seq_len(full - 1L)
  fixed_S <- integer(0)
  fixed_e <- numeric(0)
  levels <- numeric(0)
  x <- NULL
  while (length(active)) {
    le <- lapply(active, function(S) list(-.tu_ind(S, n), -1, -.tu_worth(v, S)))
    eq <- c(list(list(.tu_ind(full, n), 0, .tu_worth(v, full))),
            Map(function(S, e) list(-.tu_ind(S, n), 0, e - .tu_worth(v, S)), fixed_S, fixed_e))
    sol <- .tu_lp(n, le, eq, numeric(n), 1, lower)
    x <- sol$x
    t <- sol$t
    levels <- c(levels, t)
    cap <- lapply(active, function(S) list(-.tu_ind(S, n), 0, t - .tu_worth(v, S)))
    newly <- integer(0)
    for (S in active) {
      if (.tu_excess(v, S, x) < t - tol) next
      y <- .tu_lp(n, cap, eq, -.tu_ind(S, n), 0, lower)$x
      if (.tu_excess(v, S, y) >= t - tol) newly <- c(newly, S)
    }
    if (!length(newly)) stop("no coalition could be fixed; tolerance too tight", call. = FALSE)
    fixed_S <- c(fixed_S, newly)
    fixed_e <- c(fixed_e, rep(t, length(newly)))
    span <- c(list(.tu_ind(full, n)), lapply(fixed_S, .tu_ind, n = n))
    r <- .tu_rank(span, n)
    if (r == n) break
    active <- Filter(function(S) !(S %in% fixed_S) && .tu_rank(c(span, list(.tu_ind(S, n))), n) > r, active)
  }
  list(x = x, levels = levels)
}

#' Nucleolus and prenucleolus of a transferable-utility game
#'
#' The imputation (or, with \code{pre = TRUE}, the efficient preimputation)
#' that lexicographically minimises the non-increasingly ordered vector of
#' coalition excesses \eqn{v(S) - x(S)} (Schmeidler 1969), computed by the
#' sequence of linear programs of Maschler, Peleg and Shapley (1979): each
#' stage minimises the largest free excess, then fixes the coalitions whose
#' excess cannot fall below that level, until the fixed coalitions determine
#' the allocation. The first level of the prenucleolus is the least-core
#' value.
#'
#' @param v Worths in binary order, length \eqn{2^n - 1}: \code{v[S]} is the
#'   worth of the coalition with bitmask \code{S}.
#' @param pre Prenucleolus (no individual-rationality bounds).
#' @param tol Tolerance for tight and fixed excesses.
#' @return List with \code{x} (the allocation) and \code{levels} (the
#'   successive maximal excesses).
#' @references Schmeidler, D. (1969). The nucleolus of a characteristic
#'   function game. SIAM Journal on Applied Mathematics 17, 1163-1170.
#'
#'   Maschler, M., Peleg, B. and Shapley, L. S. (1979). Geometric properties
#'   of the kernel, nucleolus, and related solution concepts. Mathematics of
#'   Operations Research 4, 303-338.
#' @examples
#' Nucleolus(c(0, 0, 60, 0, 60, 60, 72))
#' @export
Nucleolus <- function(v, pre = FALSE, tol = 1e-9) {
  .tu_sequential_nucleolus(as.numeric(v), pre, tol)
}

#' A kernel point of a transferable-utility game by Stearns' transfers
#'
#' The maximum surplus of i over j at x is \eqn{s_{ij}(x) = \max\{v(S) -
#' x(S) : i \in S, j \notin S\}}. An imputation is in the kernel (Davis and
#' Maschler 1965) when \eqn{s_{ij} > s_{ji}} implies \eqn{x_j = v(\{j\})};
#' the prekernel asks \eqn{s_{ij} = s_{ji}}. Starting from \code{x0}
#' (default: the equal split of \eqn{v(N) - \sum v(\{i\})} on top of the
#' singleton worths), each step moves \eqn{\min((s_{ij} - s_{ji})/2, x_j -
#' v(\{j\}))} (no bound for the prekernel) from j to i for the pair where
#' that amount is largest; Stearns (1968) shows the scheme converges to a
#' kernel point.
#'
#' @param v Worths in binary order, length \eqn{2^n - 1}.
#' @param x0 Starting imputation.
#' @param pre Prekernel instead of kernel.
#' @param tol Stop when no pair can transfer more than \code{tol}.
#' @param max_iter Maximum number of transfers.
#' @return List with \code{x}, \code{surplus} (matrix of \eqn{s_{ij}}),
#'   \code{in_kernel} (the membership test at tolerance \code{100 * tol}),
#'   \code{iterations} and \code{converged}.
#' @references Davis, M. and Maschler, M. (1965). The kernel of a
#'   cooperative game. Naval Research Logistics Quarterly 12, 223-259.
#'
#'   Stearns, R. E. (1968). Convergent transfer schemes for n-person games.
#'   Transactions of the American Mathematical Society 134, 449-459.
#' @examples
#' KernelPoint(c(0, 0, 60, 0, 60, 60, 72))$x
#' @export
KernelPoint <- function(v, x0 = NULL, pre = FALSE, tol = 1e-10, max_iter = 100000L) {
  v <- as.numeric(v)
  n <- .tu_players(v)
  single <- vapply(seq_len(n), function(i) .tu_worth(v, 2L^(i - 1L)), 0)
  x <- if (is.null(x0)) single + (.tu_worth(v, 2L^n - 1L) - sum(single)) / n else as.numeric(x0)
  it <- 0L
  converged <- FALSE
  while (it < max_iter) {
    best <- 0
    pair <- NULL
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (i == j) next
        d <- (.tu_max_surplus(v, x, i, j) - .tu_max_surplus(v, x, j, i)) / 2
        step <- if (pre) d else min(d, x[j] - single[j])
        if (step > best) {
          best <- step
          pair <- c(i, j)
        }
      }
    }
    if (is.null(pair) || best <= tol) {
      converged <- TRUE
      break
    }
    x[pair[1]] <- x[pair[1]] + best
    x[pair[2]] <- x[pair[2]] - best
    it <- it + 1L
  }
  s <- matrix(0, n, n)
  for (i in seq_len(n)) for (j in seq_len(n)) if (i != j) s[i, j] <- .tu_max_surplus(v, x, i, j)
  ok <- TRUE
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i == j) next
      good <- if (pre) abs(s[i, j] - s[j, i]) <= 100 * tol else s[i, j] <= s[j, i] + 100 * tol || x[j] - single[j] <= 100 * tol
      ok <- ok && good
    }
  }
  list(x = x, surplus = s, in_kernel = ok, iterations = it, converged = converged)
}
