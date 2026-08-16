# The simplex method, with the pivot rule made a visible choice.
#
# Native R arm mirroring morie.fn.clpopt exactly: the same
# inequality-form-to-standard-form conversion with slack variables
# appended for `<=` rows and finite upper bounds, the same phase-I
# artificial-variable construction, the same tableau with m+1 rows
# (the last being the current objective), the same Bland / Dantzig
# pivot rules, the same repeated-basis cycling detection, and the
# same RichResult-shaped return with duals, reduced costs,
# degeneracy and alternate-optima reporting. No external packages
# are loaded; no random numbers are drawn.
#
# Sources: Dantzig, G. B. (1963) Linear Programming and Extensions,
# Princeton University Press, doi:10.1515/9781400884179 (the simplex
# method, the standard form and the two-phase construction);
# Bland, R. G. (1977) "New finite pivoting rules for the simplex
# method", Mathematics of Operations Research 2(2), 103-107,
# doi:10.1287/moor.2.2.103 (smallest-subscript rule, termination
# proof); Forrest, J. & Lougee-Heimer, R. (2005) "CBC user guide",
# INFORMS TutORials in Operations Research, doi:10.1287/educ.1053.0020
# (the COIN-OR LP interface this follows).

.clpopt_eps <- 1e-9
clpopt_pivots <- c("bland", "dantzig")

# Internal: build a numeric matrix from a list-of-lists / matrix.
# Mirrors clpopt._mat.
#' Internal: build a numeric matrix from a list-of-lists / matrix
#'
#' Mirrors clpopt._mat.
#'
#' @param A A matrix; indexed by row and column.
#' @param name See Usage.
#' @param ncol Defaults to \code{NULL}.
#' @return The value of \code{do.call}.
#' @export
.clpopt_mat <- function(A, name, ncol = NULL) {
  if (is.matrix(A)) {
    M <- lapply(seq_len(nrow(A)), function(i) as.numeric(A[i, ]))
  } else {
    M <- lapply(A, function(r) as.numeric(r))
  }
  if (!is.null(ncol)) {
    bad <- which(vapply(M, length, integer(1)) != ncol)
    if (length(bad) > 0L)
      stop(sprintf("clpopt: %s has rows of differing length; every row needs %d entries",
                   name, ncol))
  }
  do.call(rbind, M)
}

# Internal: convert an inequality-form program to Ax = b, x >= 0.
# Mirrors clpopt.standard_form.
#' Internal: convert an inequality-form program to Ax = b, x >= 0
#'
#' Mirrors clpopt.standard_form.
#'
#' @param c Coerced to numeric by the body, with \code{as.numeric}.
#' @param A_ub Optional; may be \code{NULL}. Passed to \code{.clpopt_mat}.
#' @param b_ub Coerced to numeric by the body, with \code{as.numeric}.
#' @param A_eq Optional; may be \code{NULL}. Passed to \code{.clpopt_mat}.
#' @param b_eq Coerced to numeric by the body, with \code{as.numeric}.
#' @param upper Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.
#' @return A list with \code{A}, \code{b}, \code{c}, \code{n_original}, \code{n_slack}, \code{row_kinds}.
#' @export
standard_form <- function(c, A_ub = NULL, b_ub = NULL, A_eq = NULL,
                          b_eq = NULL, upper = NULL) {
  cv <- as.numeric(c)
  n <- length(cv)
  if (n == 0L)
    stop("clpopt: the objective has no variables")
  rows <- list()
  n_slack <- 0L
  if (!is.null(A_ub)) {
    M <- .clpopt_mat(A_ub, "A_ub", n)
    bb <- as.numeric(b_ub)
    if (nrow(M) != length(bb))
      stop(sprintf("clpopt: A_ub has %d rows but b_ub has %d entries",
                   nrow(M), length(bb)))
    for (i in seq_len(nrow(M))) {
      rows[[length(rows) + 1L]] <- list(row = M[i, ], b = bb[i], kind = "ub")
      n_slack <- n_slack + 1L
    }
  }
  if (!is.null(upper)) {
    for (j in seq_along(upper)) {
      u <- upper[[j]]
      if (is.null(u)) next
      if (j > n)
        stop(sprintf("clpopt: an upper bound was given for variable %d of %d",
                     j - 1L, n))
      r <- rep(0.0, n)
      r[j] <- 1.0
      rows[[length(rows) + 1L]] <- list(row = r, b = as.numeric(u), kind = "ub")
      n_slack <- n_slack + 1L
    }
  }
  if (!is.null(A_eq)) {
    M <- .clpopt_mat(A_eq, "A_eq", n)
    bb <- as.numeric(b_eq)
    if (nrow(M) != length(bb))
      stop(sprintf("clpopt: A_eq has %d rows but b_eq has %d entries",
                   nrow(M), length(bb)))
    for (i in seq_len(nrow(M))) {
      rows[[length(rows) + 1L]] <- list(row = M[i, ], b = bb[i], kind = "eq")
    }
  }
  if (length(rows) == 0L)
    stop("clpopt: the program has no constraints, so it is unbounded unless the objective is zero")
  m <- length(rows)
  A <- matrix(0.0, nrow = m, ncol = n + n_slack)
  b <- numeric(m)
  s <- 0L
  for (i in seq_len(m)) {
    rec <- rows[[i]]
    A[i, seq_len(n)] <- rec$row
    if (rec$kind == "ub") {
      s <- s + 1L
      A[i, n + s] <- 1.0
    }
    b[i] <- rec$b
  }
  for (i in seq_len(m)) {
    if (b[i] < 0.0) {
      A[i, ] <- -A[i, ]
      b[i] <- -b[i]
    }
  }
  list(A = A, b = b, c = c(cv, rep(0.0, n_slack)),
       n_original = n, n_slack = n_slack,
       row_kinds = vapply(rows, function(r) r$kind, character(1)))
}

# Internal: pivot on T[row, col]. Mirrors clpopt._pivot.
#' Internal: pivot on T[row, col]. Mirrors clpopt._pivot
#'
#' A step of the clpopt_native implementation. Called by \code{.clpopt_run}, \code{simplex}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param T A matrix; indexed by row and column.
#' @param row See Usage.
#' @param col See Usage.
#' @return The value of \code{T}, as built in the body.
#' @export
.clpopt_pivot <- function(T, row, col) {
  p <- T[row, col]
  T[row, ] <- T[row, ] / p
  m <- nrow(T)
  for (i in seq_len(m)) {
    if (i == row) next
    f <- T[i, col]
    if (f != 0.0)
      T[i, ] <- T[i, ] - f * T[row, ]
  }
  T
}

# Internal: pivot to optimality. Mirrors clpopt._run.
#' Internal: pivot to optimality. Mirrors clpopt._run
#'
#' A step of the clpopt_native implementation. Called by \code{simplex}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param T A matrix; indexed by row and column.
#' @param basis A vector; its length is taken and its elements indexed.
#' @param cols A vector; indexed elementwise.
#' @param rule Compared against \code{"bland"}.
#' @param blocked See Usage.
#' @param max_iter Coerced to integer by the body, with \code{as.integer}.
#' @return A character value.
#' @export
.clpopt_run <- function(T, basis, cols, rule, blocked, max_iter) {
  m <- length(basis)
  seen_set <- new.env(hash = TRUE, parent = emptyenv())
  for (it in seq_len(as.integer(max_iter))) {
    cand <- cols[vapply(cols, function(j) {
      !(as.character(j) %in% blocked) && T[m + 1L, j + 1L] < -.clpopt_eps
    }, logical(1))]
    if (length(cand) == 0L) return("optimal")
    if (rule == "bland") {
      j <- min(cand)
    } else {
      vals <- T[m + 1L, cand + 1L]
      ord <- order(vals, cand)
      j <- cand[ord[1L]]
    }
    eligible <- which(T[seq_len(m), j + 1L] > .clpopt_eps)
    if (length(eligible) == 0L) return("unbounded")
    ratios <- T[eligible, ncol(T)] / T[eligible, j + 1L]
    best <- order(ratios, basis[eligible], eligible)[1L]
    row <- eligible[best]
    T <- .clpopt_pivot(T, row, j + 1L)
    basis[row] <- j
    key <- paste(sort(basis), collapse = ",")
    if (exists(key, envir = seen_set, inherits = FALSE))
      return("cycling")
    assign(key, TRUE, envir = seen_set)
  }
  "iteration_limit"
}

# Internal: report a successful solution. Mirrors clpopt._report.
#' Internal: report a successful solution. Mirrors clpopt._report
#'
#' A step of the clpopt_native implementation. Called by \code{simplex}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param T A matrix; indexed by row and column.
#' @param basis A vector; indexed elementwise.
#' @param cv A vector; indexed elementwise.
#' @param n A count; the body uses it as \code{seq_len(...)}.
#' @param m A count; the body uses it as \code{seq_len(...)}.
#' @param total See Usage.
#' @param rule Compared against \code{"bland"}.
#' @return A list with \code{estimate}, \code{status}, \code{x}, \code{fun}, \code{duals}, \code{basis}, \code{reduced_costs}, \code{degenerate}, \code{multiple_optima}, \code{alternate_entering}, \code{rule}, \code{method}.
#' @export
.clpopt_report <- function(T, basis, cv, n, m, total, rule) {
  x <- rep(0.0, n)
  for (i in seq_len(m)) {
    bi <- basis[i]
    if (bi < n) {
      x[bi + 1L] <- T[i, ncol(T)]
    }
  }
  y <- -T[m + 1L, (n + 1L):(n + m)]
  fun <- sum(cv[seq_len(n)] * x)
  degenerate <- basis[vapply(seq_len(m), function(i)
    abs(T[i, ncol(T)]) < .clpopt_eps, logical(1))]
  in_basis <- basis[seq_len(m)]
  alt <- seq_len(n)[vapply(seq_len(n), function(j) {
    !(j - 1L %in% in_basis) && abs(T[m + 1L, j]) < .clpopt_eps
  }, logical(1))]
  list(
    estimate = unname(x), status = "optimal", x = unname(x),
    fun = unname(fun), duals = unname(y), basis = as.list(basis),
    reduced_costs = unname(T[m + 1L, seq_len(n)]),
    degenerate = as.list(degenerate),
    multiple_optima = length(alt) > 0L,
    alternate_entering = as.list(alt - 1L),
    rule = rule,
    method = sprintf("two-phase primal simplex (Dantzig 1963) with %s's pivot rule",
                     if (rule == "bland") "Bland" else "Dantzig")
  )
}

# Internal: report a failure. Mirrors clpopt._fail.
#' Internal: report a failure. Mirrors clpopt._fail
#'
#' A step of the clpopt_native implementation. Called by \code{simplex}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param st Compared against \code{"cycling"}.
#' @param rule Compared against \code{"dantzig"}.
#' @param phase See Usage.
#' @return A list with \code{estimate}, \code{status}, \code{x}, \code{fun}, \code{rule}, \code{message}, \code{method}.
#' @export
.clpopt_fail <- function(st, rule, phase) {
  why <- if (st == "cycling")
    "the basis repeated, so the method is cycling"
  else
    "the iteration limit was reached"
  hint <- if (rule == "dantzig")
    " -- Dantzig's rule can cycle on degenerate problems; rule='bland' is guaranteed to terminate"
  else
    ""
  list(
    estimate = NULL, status = st, x = NULL, fun = NULL,
    rule = rule,
    message = sprintf("%s in %s%s", why, phase, hint),
    method = "two-phase primal simplex (Dantzig 1963)"
  )
}

# Two-phase primal simplex on Ax = b, x >= 0. Mirrors clpopt.simplex.
#' Two-phase primal simplex on Ax = b, x >= 0. Mirrors clpopt.simplex
#'
#' A step of the clpopt_native implementation. Called by \code{morie_clpopt}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param c Coerced to numeric by the body, with \code{as.numeric}.
#' @param A Passed to \code{.clpopt_mat}.
#' @param b Coerced to numeric by the body, with \code{as.numeric}.
#' @param rule Passed to \code{.clpopt_run}. Defaults to \code{"bland"}.
#' @param max_iter Passed to \code{.clpopt_run}. Defaults to \code{10000}.
#' @param initial_basis Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @return The value of \code{.clpopt_report}.
#' @export
simplex <- function(c, A, b, rule = "bland", max_iter = 10000,
                    initial_basis = NULL) {
  if (!(rule %in% clpopt_pivots))
    stop(sprintf("clpopt: rule must be one of %s, got %r",
                 paste(clpopt_pivots, collapse = ", "), rule))
  cv <- as.numeric(c)
  n <- length(cv)
  M <- .clpopt_mat(A, "A", n)
  bb <- as.numeric(b)
  m <- nrow(M)
  if (m == 0L)
    stop("clpopt: no constraints")
  if (length(bb) != m)
    stop(sprintf("clpopt: A has %d rows but b has %d entries",
                 m, length(bb)))
  if (any(bb < -.clpopt_eps))
    stop("clpopt: every right-hand side must be non-negative in standard form")
  total <- n + m
  T <- cbind(M, diag(m), bb)
  basis <- n + seq_len(m) - 1L
  if (!is.null(initial_basis)) {
    want <- as.integer(initial_basis)
    if (length(want) != m)
      stop(sprintf("clpopt: initial_basis needs %d columns, got %d",
                   m, length(want)))
    if (any(want < 0L | want >= n) || length(unique(want)) != m)
      stop(sprintf("clpopt: initial_basis must name %d distinct structural columns in [0, %d)",
                   m, n))
    for (i in seq_len(m)) {
      j <- want[i]
      if (abs(T[i, j + 1L]) <= .clpopt_eps) {
        swapped <- FALSE
        if (i < m) {
          for (r in (i + 1L):m) {
            if (abs(T[r, j + 1L]) > .clpopt_eps) {
              tmp <- T[i, ]; T[i, ] <- T[r, ]; T[r, ] <- tmp
              swapped <- TRUE
              break
            }
          }
        }
        if (!swapped)
          stop("clpopt: the columns of initial_basis are linearly dependent")
      }
      T <- .clpopt_pivot(T, i, j + 1L)
      basis[i] <- j
    }
    if (any(T[seq_len(m), ncol(T)] < -.clpopt_eps))
      stop("clpopt: initial_basis is not feasible -- it gives a negative basic value")
    obj2 <- rep(0.0, total + 1L)
    obj2[seq_len(n)] <- cv[seq_len(n)]
    for (i in seq_len(m)) {
      bi <- basis[i]
      f <- cv[bi + 1L]
      if (f != 0.0)
        obj2 <- obj2 - f * T[i, ]
    }
    T <- rbind(T, obj2)
    blocked <- as.character(seq(n, total - 1L))
    st <- .clpopt_run(T, basis, seq_len(n) - 1L, rule, blocked, max_iter)
    if (st %in% c("cycling", "iteration_limit"))
      return(.clpopt_fail(st, rule, "phase 2"))
    if (st == "unbounded")
      return(list(
        estimate = NULL, status = "unbounded", x = NULL, fun = NULL,
        rule = rule,
        message = "the objective decreases without bound along a feasible ray",
        method = "primal simplex (Dantzig 1963) from a given basis"
      ))
    return(.clpopt_report(T, basis, cv, n, m, total, rule))
  }
  # Phase I: minimise the sum of the artificials.
  obj <- rep(0.0, total + 1L)
  for (i in seq_len(m))
    obj <- obj - T[i, ]
  for (i in seq_len(m))
    obj[n + i] <- 0.0
  T <- rbind(T, obj)
  st <- .clpopt_run(T, basis, seq_len(n) - 1L, rule, character(0), max_iter)
  if (st %in% c("cycling", "iteration_limit"))
    return(.clpopt_fail(st, rule, "phase 1"))
  if (-T[m + 1L, ncol(T)] > 1e-7)
    return(list(
      estimate = NULL, status = "infeasible", x = NULL, fun = NULL,
      message = sprintf("no point satisfies every constraint (phase 1 residual %.3g)",
                        -T[m + 1L, ncol(T)]),
      rule = rule,
      method = "two-phase primal simplex (Dantzig 1963)"
    ))
  # Drive any artificial still basic out of the basis if possible.
  for (i in seq_len(m)) {
    if (basis[i] >= n) {
      for (j in seq_len(n)) {
        if (abs(T[i, j]) > .clpopt_eps) {
          T <- .clpopt_pivot(T, i, j)
          basis[i] <- j - 1L
          break
        }
      }
    }
  }
  # Phase II: real objective, artificials blocked from re-entering.
  obj2 <- rep(0.0, total + 1L)
  obj2[seq_len(n)] <- cv[seq_len(n)]
  for (i in seq_len(m)) {
    bi <- basis[i]
    if (bi < n && cv[bi + 1L] != 0.0) {
      f <- cv[bi + 1L]
      obj2 <- obj2 - f * T[i, ]
    }
  }
  T[m + 1L, ] <- obj2
  blocked <- as.character(seq(n, total - 1L))
  st <- .clpopt_run(T, basis, seq_len(n) - 1L, rule, blocked, max_iter)
  if (st %in% c("cycling", "iteration_limit"))
    return(.clpopt_fail(st, rule, "phase 2"))
  if (st == "unbounded")
    return(list(
      estimate = NULL, status = "unbounded", x = NULL, fun = NULL,
      rule = rule,
      message = "the objective decreases without bound along a feasible ray",
      method = "two-phase primal simplex (Dantzig 1963)"
    ))
  .clpopt_report(T, basis, cv, n, m, total, rule)
}

# Main entry point: solve an inequality-form linear program.
# Mirrors clpopt.linprog.
#' Main entry point: solve an inequality-form linear program
#'
#' Mirrors clpopt.linprog.
#'
#' @param c Coerced to numeric by the body, with \code{as.numeric}.
#' @param A_ub Defaults to \code{NULL}.
#' @param b_ub Defaults to \code{NULL}.
#' @param A_eq Defaults to \code{NULL}.
#' @param b_eq Defaults to \code{NULL}.
#' @param upper Defaults to \code{NULL}.
#' @param rule Defaults to \code{"bland"}.
#' @param maximise A flag; the body branches on it. Defaults to \code{FALSE}.
#' @param max_iter Defaults to \code{10000}.
#' @return The value of \code{out}, as built in the body.
#' @export
morie_clpopt <- function(c, A_ub = NULL, b_ub = NULL, A_eq = NULL,
                         b_eq = NULL, upper = NULL, rule = "bland",
                         maximise = FALSE, max_iter = 10000) {
  sign <- if (isTRUE(maximise)) -1.0 else 1.0
  sf <- standard_form(sign * as.numeric(c), A_ub, b_ub, A_eq, b_eq, upper)
  r <- simplex(sf$c, sf$A, sf$b, rule, max_iter)
  if (r$status != "optimal")
    return(r)
  n <- sf$n_original
  x <- r$x[seq_len(n)]
  fun <- sign * r$fun
  out <- r
  out$estimate <- x
  out$x <- x
  out$fun <- fun
  out$duals <- sign * r$duals
  out$slack <- r$x[n + seq_len(sf$n_slack)]
  out$maximise <- isTRUE(maximise)
  out$n_original <- n
  out$n_slack <- sf$n_slack
  out
}
