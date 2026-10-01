#' Decision and factoring algorithms
#'
#' \code{SmtSolver}: lazy DPLL(T) for integer difference logic. Atom k is
#' the constraint x - y <= c; the Boolean skeleton is solved by
#' \code{\link{Dpll}}, asserted atoms (and the integer negations
#' y - x <= -c - 1 of those set false) become weighted edges, and
#' Bellman-Ford returns either potentials (a model) or a negative cycle whose
#' negation is learned as a clause. \code{ShorFactoring}: Shor's algorithm
#' with the order-finding measurement drawn (Philox) from its exact output
#' distribution, then continued fractions. Identical to the Python arm
#' \code{morie.fn.compalgo}.
#'
#' @param formula List with atoms (list of list(x, y, c)) and clauses (list
#'   of signed integer vectors, DIMACS style; indices above the number of
#'   atoms are Boolean variables).
#' @param max_iter Maximum SAT-theory rounds.
#' @param N Composite integer below 1024.
#' @param seed Philox seed.
#' @param max_attempts Maximum bases tried.
#' @return List. SmtSolver: satisfiable, model, solution, theory_conflicts,
#'   learned. ShorFactoring: factors, order, base, Q, attempts, route.
#' @references Nieuwenhuis, R., Oliveras, A. and Tinelli, C. (2006). Solving
#'   SAT and SAT modulo theories: from an abstract Davis-Putnam-Logemann-
#'   Loveland procedure to DPLL(T). Journal of the ACM 53, 937-977.
#'
#'   Shor, P. W. (1997). Polynomial-time algorithms for prime factorization
#'   and discrete logarithms on a quantum computer. SIAM Journal on
#'   Computing 26, 1484-1509.
#'
#'   Nielsen, M. A. and Chuang, I. L. (2010). Quantum Computation and
#'   Quantum Information. Cambridge University Press.
#' @examples
#' f <- list(atoms = list(list("x", "y", -1), list("y", "z", -1),
#'                        list("z", "x", -1), list("z", "x", 5)),
#'           clauses = list(1, 2, c(3, 4)))
#' SmtSolver(f)$solution
#' ShorFactoring(15, seed = 2)$factors
#' @export
SmtSolver <- function(formula, max_iter = 10000L) {
  atoms <- lapply(formula$atoms, function(a) list(x = as.character(a[[1]]), y = as.character(a[[2]]), c = as.numeric(a[[3]])))
  clauses <- lapply(formula$clauses, as.integer)
  names_ <- sort(unique(unlist(lapply(atoms, function(a) c(a$x, a$y)))), method = "radix")
  learned <- list()
  for (it in seq_len(max_iter) - 1) {
    sat <- Dpll(c(clauses, learned))
    if (!sat$satisfiable) return(list(satisfiable = FALSE, model = list(), solution = list(), theory_conflicts = it, learned = learned))
    ks <- sort(as.integer(names(sat$model)))
    model <- stats::setNames(lapply(ks, function(k) isTRUE(sat$model[[as.character(k)]])), ks)
    edges <- list()
    for (k in ks) {
      if (k > length(atoms)) next
      a <- atoms[[k]]
      edges[[length(edges) + 1]] <- if (model[[as.character(k)]]) list(u = a$y, v = a$x, w = a$c, lit = k) else list(u = a$x, v = a$y, w = -a$c - 1, lit = -k)
    }
    bf <- .ca_negcycle(names_, edges)
    if (is.null(bf$cycle)) {
      return(list(satisfiable = TRUE, model = model, solution = as.list(bf$dist), theory_conflicts = it, learned = learned))
    }
    lits <- unique(vapply(bf$cycle, function(k) -edges[[k]]$lit, 0))
    learned[[length(learned) + 1]] <- lits[order(abs(lits), lits)]
  }
  stop("SmtSolver: max_iter reached")
}

.ca_negcycle <- function(nodes, edges) {
  dist <- stats::setNames(numeric(length(nodes)), nodes)
  pred <- stats::setNames(rep(NA_integer_, length(nodes)), nodes)
  last <- NULL
  for (i in seq_along(nodes)) {
    last <- NULL
    for (k in seq_along(edges)) {
      e <- edges[[k]]
      if (dist[[e$u]] + e$w < dist[[e$v]]) {
        dist[[e$v]] <- dist[[e$u]] + e$w
        pred[[e$v]] <- k
        last <- e$v
      }
    }
    if (is.null(last)) return(list(dist = dist, cycle = NULL))
  }
  v <- last
  for (i in seq_along(nodes)) v <- edges[[pred[[v]]]]$u
  cyc <- integer(0)
  u <- v
  repeat {
    k <- pred[[u]]
    cyc <- c(cyc, k)
    u <- edges[[k]]$u
    if (u == v) break
  }
  list(dist = dist, cycle = rev(cyc))
}

.ca_powmod <- function(a, e, n) {
  r <- 1
  a <- a %% n
  while (e > 0) {
    if (e %% 2 == 1) r <- (r * a) %% n
    a <- (a * a) %% n
    e <- e %/% 2
  }
  r
}

.ca_gcd <- function(a, b) {
  a <- abs(a)
  b <- abs(b)
  while (b > 0) {
    t <- a %% b
    a <- b
    b <- t
  }
  a
}

.ca_isprime <- function(n) {
  if (n < 2) return(FALSE)
  d <- 2
  while (d * d <= n) {
    if (n %% d == 0) return(FALSE)
    d <- d + 1
  }
  TRUE
}

.ca_perfectpower <- function(n) {
  for (k in 2:max(2, floor(log2(n)) + 1)) {
    b <- round(n^(1 / k))
    for (cc in c(b - 1, b, b + 1)) if (cc > 1 && cc^k == n) return(cc)
  }
  NULL
}

.ca_convergents <- function(p, q) {
  h0 <- 0
  h1 <- 1
  k0 <- 1
  k1 <- 0
  out <- numeric(0)
  while (q > 0) {
    t <- p %/% q
    h2 <- t * h1 + h0
    h0 <- h1
    h1 <- h2
    k2 <- t * k1 + k0
    k0 <- k1
    k1 <- k2
    out <- c(out, k1)
    r <- p - t * q
    p <- q
    q <- r
  }
  out
}

.ca_orderdist <- function(r, Q) {
  M <- Q %/% r
  rem <- Q %% r
  vapply(seq_len(Q) - 1, function(y) {
    ry <- (r * y) %% Q
    if (ry == 0) {
      g1 <- (M + 1)^2
      g0 <- M * M
    } else {
      th <- pi * ry / Q
      s <- sin(th)
      g1 <- (sin((M + 1) * th) / s)^2
      g0 <- (sin(M * th) / s)^2
    }
    (rem * g1 + (r - rem) * g0) / (Q * Q)
  }, 0)
}

#' @rdname SmtSolver
#' @export
ShorFactoring <- function(N, seed = 1, max_attempts = 20L) {
  N <- as.numeric(N)
  if (N < 4) stop("N must be a composite integer >= 4")
  if (N %% 2 == 0) return(list(factors = c(2, N / 2), order = NULL, base = NULL, Q = NULL, attempts = list(), route = "even"))
  b <- .ca_perfectpower(N)
  if (!is.null(b)) return(list(factors = c(b, N / b), order = NULL, base = NULL, Q = NULL, attempts = list(), route = "perfect power"))
  if (.ca_isprime(N)) stop("N is prime")
  t <- ceiling(log2(N * N))
  if (2^t < N * N) t <- t + 1
  if (2^(t - 1) >= N * N) t <- t - 1
  Q <- 2^t
  if (Q > 2^20) stop("N too large for the state-vector simulation (Q > 2^20)")
  u <- .morie_random_uniform(2 * max_attempts, seed = seed)
  attempts <- list()
  for (k in seq_len(max_attempts) - 1) {
    a <- 2 + min(floor(u[2 * k + 1] * (N - 2)), N - 3)
    g <- .ca_gcd(a, N)
    if (g > 1) {
      attempts[[length(attempts) + 1]] <- list(a = a, y = NULL, r = NULL, outcome = "gcd")
      return(list(factors = sort(c(g, N / g)), order = NULL, base = a, Q = Q, attempts = attempts, route = "gcd"))
    }
    r <- 1
    v <- a %% N
    while (v != 1) {
      v <- (v * a) %% N
      r <- r + 1
    }
    probs <- .ca_orderdist(r, Q)
    y <- Q - 1
    acc <- 0
    for (i in seq_along(probs)) {
      acc <- acc + probs[i]
      if (u[2 * k + 2] < acc) {
        y <- i - 1
        break
      }
    }
    rr <- NULL
    for (q in .ca_convergents(y, Q)) {
      if (q > 0 && q < N && .ca_powmod(a, q, N) == 1) {
        rr <- q
        break
      }
    }
    if (is.null(rr)) {
      attempts[[length(attempts) + 1]] <- list(a = a, y = y, r = NULL, outcome = "no order from y/Q")
      next
    }
    if (rr %% 2 == 1 || .ca_powmod(a, rr / 2, N) == N - 1) {
      attempts[[length(attempts) + 1]] <- list(a = a, y = y, r = rr, outcome = "odd order or a^(r/2) = -1")
      next
    }
    h <- .ca_powmod(a, rr / 2, N)
    f <- .ca_gcd(h - 1, N)
    if (f == 1 || f == N) f <- .ca_gcd(h + 1, N)
    attempts[[length(attempts) + 1]] <- list(a = a, y = y, r = rr, outcome = "factored")
    return(list(factors = sort(c(f, N / f)), order = rr, base = a, Q = Q, attempts = attempts, route = "order finding"))
  }
  stop("ShorFactoring: no factor found within max_attempts")
}
