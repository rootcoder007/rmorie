# tail2 batch, tranche 4 -- R mirror of the Python modules
#   morie/fn/sgtsig.py   Cvetkovic-Doob-Sachs (1995) signless Laplacian
#   morie/fn/sgtspr.py   bipartite detection via spectral symmetry
#   morie/fn/satDP.py    Davis-Logemann-Loveland (1962) DPLL
#   morie/fn/epsig1.py   Dempster-Laird-Rubin (1977) EM driver
#   morie/fn/smplxs.py   Dantzig simplex, Bland (1977) pivoting rule
#
# Byte-identical between r-package/morie/R and r-morie-oss/R.
#
# Sources actually consulted are named in each function and at more
# length in the docstring of the matching Python module.  Every claim
# that could be checked against an independent implementation was:
# SimplexLP against lpSolve::lp, BipartSpec against
# igraph::bipartite_mapping, SignlessL zero-eigenvalue multiplicity
# against base::eigen on Q, and Dpll against exhaustive truth tables.
# Every routine is exact or runs a FIXED number of steps, because an
# early exit on one language arm and not the other silently breaks
# Python/R parity.

# ---- Cvetkovic-Doob-Sachs (1995) signless Laplacian ------------------

#' .morie_t2_checkadj
#'
#' A step of the tail2_t04 implementation. Called by \code{BipartSpec}, \code{SignlessL}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A A matrix; indexed by row and column.
#' @return The value of \code{A}, as built in the body.
#' @export
.morie_t2_checkadj <- function(A) {
  A <- as.matrix(A)
  storage.mode(A) <- "double"
  n <- nrow(A)
  if (n == 0L) stop("empty adjacency matrix")
  if (ncol(A) != n) stop("adjacency matrix must be square")
  for (i in seq_len(n)) {
    if (A[i, i] != 0) stop("adjacency matrix must have a zero diagonal")
    j <- i + 1L
    while (j <= n) {
      if (abs(A[i, j] - A[j, i]) > 1e-12 * (1 + abs(A[i, j]))) {
        stop("adjacency matrix must be symmetric")
      }
      j <- j + 1L
    }
  }
  A
}

#' .morie_t2_neigh
#'
#' A step of the tail2_t04 implementation. Called by \code{BipartSpec}, \code{SignlessL}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A A matrix; indexed by row and column.
#' @param n A count; the body uses it as \code{seq_len(...)}.
#' @return The value of \code{lapply}.
#' @export
.morie_t2_neigh <- function(A, n) {
  lapply(seq_len(n), function(i) {
    v <- which(A[i, ] != 0)
    v[v != i]
  })
}

# \' Signless Laplacian Q = D + A of an undirected graph
# \'
# \' D is the diagonal matrix of row sums.  The multiplicity of 0 as an
# \' eigenvalue of Q equals the number of bipartite components; that count
# \' is returned, computed combinatorially by two-colouring rather than by
# \' an eigensolver, so it is exact.
# \'
# \' @param A symmetric adjacency matrix with a zero diagonal
# \' @return list(Q, degree, n, m, trace, n_components,
# \'   bipartite_components, zero_eigenvalue_multiplicity, method)
# \' @export
#' \' Signless Laplacian Q = D + A of an undirected graph
#'
#' \' \' D is the diagonal matrix of row sums.  The multiplicity of 0
#' as an \' eigenvalue of Q equals the number of bipartite components;
#' that count \' is returned, computed combinatorially by two-colouring
#' rather than by \' an eigensolver, so it is exact. \' \' @param A
#' symmetric adjacency matrix with a zero diagonal \' @return list(Q,
#' degree, n, m, trace, n_components, \' bipartite_components,
#' zero_eigenvalue_multiplicity, method) \' @export
#'
#' @param A A matrix; indexed by row and column.
#' @return A list with \code{Q}, \code{degree}, \code{n}, \code{m}, \code{trace}, \code{n_components}, \code{bipartite_components}, \code{zero_eigenvalue_multiplicity}, \code{method}.
#' @export
SignlessL <- function(A) {
  A <- .morie_t2_checkadj(A)
  n <- nrow(A)
  deg <- vapply(seq_len(n), function(i) sum(A[i, ]), numeric(1))
  Q <- A + diag(deg, nrow = n)
  adj <- .morie_t2_neigh(A, n)
  comp <- rep(-1L, n)
  colour <- integer(n)
  bip <- logical(0)
  ncomp <- 0L
  for (s in seq_len(n)) {
    if (comp[s] != -1L) next
    stack <- s
    comp[s] <- ncomp
    colour[s] <- 1L
    ok <- TRUE
    while (length(stack)) {
      u <- stack[length(stack)]
      stack <- stack[-length(stack)]
      for (v in adj[[u]]) {
        if (comp[v] == -1L) {
          comp[v] <- ncomp
          colour[v] <- -colour[u]
          stack <- c(stack, v)
        } else if (colour[v] == colour[u]) {
          ok <- FALSE
        }
      }
    }
    bip <- c(bip, ok)
    ncomp <- ncomp + 1L
  }
  nbip <- sum(bip)
  list(
    Q = Q, degree = deg, n = n, m = sum(deg) / 2, trace = sum(deg),
    n_components = ncomp, bipartite_components = nbip,
    zero_eigenvalue_multiplicity = nbip,
    method = "signless Laplacian Q = D + A"
  )
}

# ---- bipartite detection via spectral symmetry ----------------------

# \' Decide bipartiteness, with the odd power-traces as evidence
# \'
# \' A symmetric A has a spectrum symmetric about zero exactly when
# \' trace(A^k) = 0 for every odd k, and trace(A^k) counts closed walks of
# \' length k, so the criterion is "no odd cycle".  Both the spectral and
# \' the two-colouring answer are computed and must agree.
# \'
# \' @param A symmetric adjacency matrix with a zero diagonal
# \' @return list(bipartite, evidence, max_odd_trace, colouring,
# \'   part_sizes, n_components, n, m, method)
# \' @export
#' \' Decide bipartiteness, with the odd power-traces as evidence
#'
#' \' \' A symmetric A has a spectrum symmetric about zero exactly
#' when \' trace(A^k) = 0 for every odd k, and trace(A^k) counts closed
#' walks of \' length k, so the criterion is "no odd cycle".  Both the
#' spectral and \' the two-colouring answer are computed and must
#' agree. \' \' @param A symmetric adjacency matrix with a zero
#' diagonal \' @return list(bipartite, evidence, max_odd_trace,
#' colouring, \' part_sizes, n_components, n, m, method) \' @export
#'
#' @param A A matrix; passed to \code{nrow}.
#' @return A list with \code{bipartite}, \code{evidence}, \code{max_odd_trace}, \code{colouring}, \code{part_sizes}, \code{n_components}, \code{n}, \code{m}, \code{method}.
#' @export
BipartSpec <- function(A) {
  A <- .morie_t2_checkadj(A)
  n <- nrow(A)
  adj <- .morie_t2_neigh(A, n)
  colour <- integer(n)
  ncomp <- 0L
  combinatorial <- TRUE
  for (s in seq_len(n)) {
    if (colour[s] != 0L) next
    ncomp <- ncomp + 1L
    colour[s] <- 1L
    queue <- s
    head <- 1L
    while (head <= length(queue)) {
      u <- queue[head]
      head <- head + 1L
      for (v in adj[[u]]) {
        if (colour[v] == 0L) {
          colour[v] <- -colour[u]
          queue <- c(queue, v)
        } else if (colour[v] == colour[u]) {
          combinatorial <- FALSE
        }
      }
    }
  }
  evidence <- numeric(0)
  P <- A
  k <- 1L
  repeat {
    evidence <- c(evidence, sum(diag(P)))
    if (k + 2L > n) break
    P <- P %*% A %*% A
    k <- k + 2L
  }
  max_odd <- max(abs(evidence))
  spectral <- max_odd <= 1e-9
  if (spectral != combinatorial) {
    stop(paste(
      "the spectral and combinatorial bipartiteness tests",
      "disagree; this should be impossible for a symmetric 0/1",
      "adjacency matrix and means the input is not one"
    ))
  }
  list(
    bipartite = combinatorial, evidence = evidence,
    max_odd_trace = max_odd, colouring = colour,
    part_sizes = c(sum(colour == 1L), sum(colour == -1L)),
    n_components = ncomp, n = n, m = sum(A) / 2,
    method = paste(
      "bipartite detection; spectrum symmetric about zero",
      "iff every odd trace(A^k) vanishes"
    )
  )
}

# ---- Davis-Logemann-Loveland (1962) DPLL ----------------------------

#' .morie_t2_simplify
#'
#' A step of the tail2_t04 implementation. Called by \code{Dpll}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param clauses See Usage.
#' @param lit Numeric; combined arithmetically in the body.
#' @return The value of \code{out}, as built in the body.
#' @export
.morie_t2_simplify <- function(clauses, lit) {
  out <- list()
  for (cl in clauses) {
    if (lit %in% cl) next
    if (-lit %in% cl) {
      red <- cl[cl != -lit]
      if (length(red) == 0L) {
        return(NULL)
      }
      out[[length(out) + 1L]] <- red
    } else {
      out[[length(out) + 1L]] <- cl
    }
  }
  out
}

# \' Decide satisfiability of a CNF formula by DPLL
# \'
# \' Unit propagation, pure literal elimination, then a split on the
# \' lowest-indexed unassigned variable trying TRUE first.  The index rule
# \' is deterministic, so decision and propagation counts are reproducible
# \' and match the Python arm exactly.
# \'
# \' @param cnf list of integer vectors, DIMACS literals (v, -v); 0 is not
# \'   a literal
# \' @return list(satisfiable, assignment, model, n_vars, n_clauses,
# \'   decisions, propagations, pure_literals, method)
# \' @export
#' \' Decide satisfiability of a CNF formula by DPLL
#'
#' \' \' Unit propagation, pure literal elimination, then a split on
#' the \' lowest-indexed unassigned variable trying TRUE first.  The
#' index rule \' is deterministic, so decision and propagation counts
#' are reproducible \' and match the Python arm exactly. \' \' @param
#' cnf list of integer vectors, DIMACS literals (v, -v); 0 is not \' a
#' literal \' @return list(satisfiable, assignment, model, n_vars,
#' n_clauses, \' decisions, propagations, pure_literals, method) \'
#' @export
#'
#' @param cnf Iterated over elementwise, with \code{lapply}.
#' @return A list with \code{satisfiable}, \code{assignment}, \code{model}, \code{n_vars}, \code{n_clauses}, \code{decisions}, \code{propagations}, \code{pure_literals}, \code{method}.
#' @export
Dpll <- function(cnf) {
  clauses <- lapply(cnf, function(cl) {
    c <- as.integer(cl)
    if (any(c == 0L)) stop("0 is not a literal")
    c
  })
  allv <- sort(unique(abs(unlist(clauses))))
  nvars <- if (length(allv)) max(allv) else 0L
  st <- new.env()
  st$decisions <- 0L
  st$propagations <- 0L
  st$pure <- 0L

  search <- function(cls, assign) {
    repeat {
      if (is.null(cls)) {
        return(NULL)
      }
      if (length(cls) == 0L) {
        return(assign)
      }
      unit <- NA_integer_
      for (cl in cls) {
        if (length(cl) == 1L) {
          unit <- cl[1L]
          break
        }
      }
      if (!is.na(unit)) {
        st$propagations <- st$propagations + 1L
        assign[[as.character(abs(unit))]] <- unit > 0L
        cls <- .morie_t2_simplify(cls, unit)
        next
      }
      vs <- sort(unique(abs(unlist(cls))))
      pure <- NA_integer_
      for (v in vs) {
        pos <- any(vapply(cls, function(cl) v %in% cl, logical(1)))
        neg <- any(vapply(cls, function(cl) -v %in% cl, logical(1)))
        if (pos && !neg) {
          pure <- v
          break
        }
        if (neg && !pos) {
          pure <- -v
          break
        }
      }
      if (!is.na(pure)) {
        st$pure <- st$pure + 1L
        assign[[as.character(abs(pure))]] <- pure > 0L
        cls <- .morie_t2_simplify(cls, pure)
        next
      }
      break
    }
    pick <- min(abs(unlist(cls)))
    st$decisions <- st$decisions + 1L
    for (value in c(TRUE, FALSE)) {
      lit <- if (value) pick else -pick
      sub <- .morie_t2_simplify(cls, lit)
      nxt <- assign
      nxt[[as.character(pick)]] <- value
      got <- search(sub, nxt)
      if (!is.null(got)) {
        return(got)
      }
    }
    NULL
  }

  found <- if (any(vapply(clauses, length, integer(1)) == 0L)) {
    NULL
  } else {
    search(clauses, list())
  }
  sat <- !is.null(found)
  model <- list()
  if (sat && nvars > 0L) {
    for (v in seq_len(nvars)) {
      key <- as.character(v)
      model[[key]] <- if (is.null(found[[key]])) TRUE else found[[key]]
    }
  }
  list(
    satisfiable = sat, assignment = if (sat) found else list(),
    model = model, n_vars = nvars, n_clauses = length(clauses),
    decisions = st$decisions, propagations = st$propagations,
    pure_literals = st$pure,
    method = paste(
      "DPLL: unit propagation, pure literal, split on the",
      "lowest-indexed variable, True first"
    )
  )
}

# ---- Dempster-Laird-Rubin (1977) EM driver --------------------------

# \' Run EM for a fixed number of steps and audit its monotonicity
# \'
# \' Theorem 1 of Dempster, Laird & Rubin (1977) says every EM step
# \' increases the observed-data log likelihood.  That is not assumed but
# \' checked: the log likelihood is evaluated at every iterate and the
# \' increments are returned along with a monotone flag.
# \'
# \' @param log_lik function(theta) giving the observed-data log likelihood
# \' @param Q function(theta) giving the next iterate (combined E and M step)
# \' @param x0 starting parameter vector
# \' @param steps number of EM iterations
# \' @return list(theta, loglik, trace, increments, min_increment, monotone,
# \'   steps, method)
# \' @export
#' \' Run EM for a fixed number of steps and audit its monotonicity
#'
#' \' \' Theorem 1 of Dempster, Laird & Rubin (1977) says every EM
#' step \' increases the observed-data log likelihood.  That is not
#' assumed but \' checked: the log likelihood is evaluated at every
#' iterate and the \' increments are returned along with a monotone
#' flag. \' \' @param log_lik function(theta) giving the observed-data
#' log likelihood \' @param Q function(theta) giving the next iterate
#' (combined E and M step) \' @param x0 starting parameter vector \'
#' @param steps number of EM iterations \' @return list(theta, loglik,
#' trace, increments, min_increment, monotone, \' steps, method) \'
#' @export
#'
#' @param log_lik Accepted by the signature and not used anywhere in the body.
#' @param Q Accepted by the signature and not used anywhere in the body.
#' @param x0 Coerced to numeric by the body, with \code{as.numeric}.
#' @param steps A count; the body uses it as \code{seq_len(...)}.
#' @return A list with \code{theta}, \code{loglik}, \code{trace}, \code{increments}, \code{min_increment}, \code{monotone}, \code{steps}, \code{method}.
#' @export
EmAlgo <- function(log_lik, Q, x0, steps) {
  theta <- as.numeric(x0)
  steps <- as.integer(steps)
  if (steps < 0L) stop("steps must be non-negative")
  trace <- as.numeric(log_lik(theta))
  for (t in seq_len(steps)) {
    nxt <- as.numeric(Q(theta))
    if (length(nxt) != length(theta)) {
      stop("the M-step changed the parameter length")
    }
    theta <- nxt
    trace <- c(trace, as.numeric(log_lik(theta)))
  }
  inc <- if (length(trace) > 1L) diff(trace) else numeric(0)
  mn <- if (length(inc)) min(inc) else 0
  list(
    theta = theta, loglik = trace[length(trace)], trace = trace,
    increments = inc, min_increment = mn, monotone = mn >= -1e-9,
    steps = steps,
    method = paste(
      "EM driver with a Dempster-Laird-Rubin Theorem 1",
      "monotonicity audit"
    )
  )
}

# ---- Dantzig simplex, Bland (1977) pivoting rule --------------------

# \' Maximise c\'x subject to A x <= b, x >= 0, with b >= 0
# \'
# \' Tableau simplex with Bland\'s rule: the entering column is the
# \' lowest-indexed one with a negative reduced cost in the objective row,
# \' and among rows attaining the minimum ratio the leaving row is the one
# \' whose basic variable has the smallest index.  Bland\'s rule cannot
# \' cycle, and being a pure index rule it makes the R and Python arms
# \' pivot identically.  Requires b >= 0 so the slack basis is feasible;
# \' a negative right-hand side needs a phase-1 problem, not built here.
# \' Checked against lpSolve::lp.
# \'
# \' @param c objective coefficients; the objective is MAXIMISED
# \' @param A constraint matrix, ncon x nvar
# \' @param b right-hand side, every entry non-negative
# \' @param max_iter pivot budget
# \' @param tol zero tolerance for reduced costs and pivot elements
# \' @return list(status, x, objective, slack, basis, dual, iterations,
# \'   n_var, n_con, method)
# \' @export
#' \' Maximise c\'x subject to A x <= b, x >= 0, with b >= 0
#'
#' \' \' Tableau simplex with Bland\'s rule: the entering column is
#' the \' lowest-indexed one with a negative reduced cost in the
#' objective row, \' and among rows attaining the minimum ratio the
#' leaving row is the one \' whose basic variable has the smallest
#' index.  Bland\'s rule cannot \' cycle, and being a pure index rule
#' it makes the R and Python arms \' pivot identically.  Requires b >=
#' 0 so the slack basis is feasible; \' a negative right-hand side
#' needs a phase-1 problem, not built here. \' Checked against
#' lpSolve::lp. \' \' @param c objective coefficients; the objective
#' is MAXIMISED \' @param A constraint matrix, ncon x nvar \' @param b
#' right-hand side, every entry non-negative \' @param max_iter pivot
#' budget \' @param tol zero tolerance for reduced costs and pivot
#' elements \' @return list(status, x, objective, slack, basis, dual,
#' iterations, \' n_var, n_con, method) \' @export
#'
#' @param c A vector; its length is taken.
#' @param A A matrix; passed to \code{nrow}.
#' @param b A vector; its length is taken.
#' @param max_iter Coerced to integer by the body, with \code{as.integer}. Defaults to \code{1000L}.
#' @param tol Numeric; combined arithmetically in the body. Defaults to \code{1e-12}.
#' @return A list with \code{status}, \code{x}, \code{objective}, \code{slack}, \code{basis}, \code{dual}, \code{iterations}, \code{n_var}, \code{n_con}, \code{method}.
#' @export
SimplexLP <- function(c, A, b, max_iter = 1000L, tol = 1e-12) {
  c <- as.numeric(c)
  A <- matrix(as.numeric(as.matrix(A)), nrow = length(b))
  b <- as.numeric(b)
  nvar <- length(c)
  ncon <- length(b)
  if (nrow(A) != ncon) stop("b must have one entry per row of A")
  if (ncol(A) != nvar) {
    stop("every row of A needs one entry per variable")
  }
  if (any(b < 0)) {
    stop(paste(
      "every entry of b must be non-negative; a negative",
      "right-hand side needs a phase-1 problem, which this",
      "function does not build"
    ))
  }

  total <- nvar + ncon
  T <- cbind(A, diag(1, ncon), b)
  z <- c(-c, numeric(ncon), 0)
  basis <- nvar + seq_len(ncon)

  status <- "optimal"
  it <- 0L
  budget <- as.integer(max_iter)
  done <- FALSE
  for (step in seq_len(budget)) {
    it <- step
    enter <- -1L
    for (j in seq_len(total)) {
      if (z[j] < -tol) {
        enter <- j
        break
      }
    }
    if (enter == -1L) {
      it <- step - 1L
      done <- TRUE
      break
    }
    leave <- -1L
    best <- NA_real_
    for (i in seq_len(ncon)) {
      if (T[i, enter] > tol) {
        ratio <- T[i, total + 1L] / T[i, enter]
        if (is.na(best) || ratio < best - 1e-12 ||
          (abs(ratio - best) <= 1e-12 && basis[i] < basis[leave])) {
          best <- if (is.na(best) || ratio < best) ratio else best
          leave <- i
        }
      }
    }
    if (leave == -1L) {
      status <- "unbounded"
      done <- TRUE
      break
    }
    piv <- T[leave, enter]
    T[leave, ] <- T[leave, ] / piv
    for (i in seq_len(ncon)) {
      if (i == leave) next
      f <- T[i, enter]
      if (f != 0) T[i, ] <- T[i, ] - f * T[leave, ]
    }
    f <- z[enter]
    if (f != 0) z <- z - f * T[leave, ]
    basis[leave] <- enter
  }
  if (!done) {
    stop(paste(
      "pivot budget exhausted; Bland\'s rule should have",
      "terminated, so the problem is larger than max_iter"
    ))
  }

  sol <- numeric(total)
  for (i in seq_len(ncon)) sol[basis[i]] <- T[i, total + 1L]
  x <- sol[seq_len(nvar)]
  list(
    status = status, x = x, objective = sum(c * x),
    slack = sol[nvar + seq_len(ncon)], basis = basis,
    dual = z[nvar + seq_len(ncon)], iterations = it,
    n_var = nvar, n_con = ncon,
    method = "Dantzig simplex, tableau form, Bland\'s rule"
  )
}
