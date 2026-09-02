# Branch and bound: solve the relaxation, then split on a fraction.
# Sources: Land, A. H. & Doig, A. G. (1960), "An Automatic Method of
# Solving Discrete Programming Problems", Econometrica 28(3), 497-520;
# Dakin, R. J. (1965), "A tree-search algorithm for mixed integer
# programming problems", The Computer Journal 8(3), 250-255;
# Dantzig, G. B. (1963), Linear Programming and Extensions;
# Mehrotra, S. (1992), "On the Implementation of a Primal-Dual
# Interior Point Method", SIAM Journal on Optimization 2(4), 575-601.
#
# Native implementation mirroring Python morie.fn.miprgr exactly:
# the same Dakin (1965) Fig. 2 marked list, the same bound test, and
# the same two solvers (simplex default; interior routed to
# .ghc_mipinterior, a Mehrotra-style primal-dual interior point method
# implemented below so the R arm is self-contained).

.GHC_MIP_EPS <- 1e-7

#' @keywords internal
#' @noRd
.ghc_simplex_mip <- function(A, b, c, tol = 1e-9, max_iter = 20000) {
  m <- length(A)
  n <- if (m > 0) length(A[[1]]) else 0
  rows <- vector("list", m)
  rhs <- numeric(m)
  for (i in seq_len(m)) {
    ri <- as.numeric(A[[i]])
    bi <- as.numeric(b[i])
    if (bi < 0) {
      ri <- -ri
      bi <- -bi
      ri <- c(ri, rep(0, m))
      ri[n + i] <- -1
    } else {
      ri <- c(ri, rep(0, m))
      ri[n + i] <- 1
    }
    rows[[i]] <- ri
    rhs[i] <- bi
  }
  need_art <- which(vapply(seq_len(m), function(i) rows[[i]][n + i] < 0,
                           logical(1)))
  na <- length(need_art)
  width <- n + m + na
  T <- vector("list", m)
  for (i in seq_len(m)) {
    T[[i]] <- c(rows[[i]], rep(0, na), rhs[i])
  }
  basis <- as.list(seq(n + 1, n + m))
  for (a in seq_along(need_art)) {
    i <- need_art[a]
    T[[i]][n + m + a] <- 1
    basis[[i]] <- n + m + a
  }
  Tmat <- do.call(rbind, T)

  reduced <- function(obj) {
    z <- obj
    for (i in seq_len(m)) {
      f <- z[basis[[i]]]
      if (f != 0) {
        for (j in seq_len(width + 1)) z[j] <- z[j] - f * Tmat[i, j]
      }
    }
    z
  }
  pivot <- function(pr, pc) {
    pv <- Tmat[pr, pc]
    Tmat[pr, ] <- Tmat[pr, ] / pv
    for (i in seq_len(m)) {
      if (i != pr && abs(Tmat[i, pc]) > 0) {
        f <- Tmat[i, pc]
        Tmat[i, ] <- Tmat[i, ] - f * Tmat[pr, ]
      }
    }
    basis[[pr]] <<- pc
  }
  run <- function(obj, allowed) {
    for (iter in seq_len(as.integer(max_iter))) {
      z <- reduced(obj)
      enter <- -1
      for (j in allowed) {
        if (z[j] < -tol) { enter <- j
        break }
      }
      if (enter < 0) return(TRUE)
      ratio <- Inf
      leave <- -1
      for (i in seq_len(m)) {
        if (Tmat[i, enter] > tol) {
          r <- Tmat[i, width + 1] / Tmat[i, enter]
          if (r < ratio - tol ||
              (abs(r - ratio) <= tol && leave >= 0 &&
               basis[[i]] < basis[[leave]])) {
            ratio <- r
            leave <- i
          }
        }
      }
      if (leave < 0) return(FALSE)
      pivot(leave, enter)
    }
    FALSE
  }
  if (na > 0) {
    phase1 <- rep(0, width + 1)
    for (a in seq_len(na)) phase1[n + m + a] <- 1
    if (!run(phase1, seq_len(n + m)))
      return(list(feasible = FALSE, x = NULL, value = NULL))
    infeas <- 0
    for (i in seq_len(m))
      if (basis[[i]] >= n + m) infeas <- infeas + Tmat[i, width + 1]
    if (infeas > 1e-7)
      return(list(feasible = FALSE, x = NULL, value = NULL))
    for (i in seq_len(m)) {
      if (basis[[i]] >= n + m) {
        moved <- FALSE
        for (j in seq_len(n + m)) {
          if (abs(Tmat[i, j]) > tol) { pivot(i, j)
          moved <- TRUE
          break }
        }
      }
    }
  }
  phase2 <- rep(0, width + 1)
  for (j in seq_len(n)) phase2[j] <- -as.numeric(c[j])
  if (!run(phase2, seq_len(n + m)))
    return(list(feasible = FALSE, x = NULL, value = NULL))
  x <- rep(0, n)
  for (i in seq_len(m)) if (basis[[i]] <= n) x[basis[[i]]] <- Tmat[i, width + 1]
  list(feasible = TRUE, x = x,
       value = sum(as.numeric(c) * x))
}

#' @keywords internal
#' @noRd
.ghc_standard_form <- function(A, b, c, bounds, n) {
  rows <- vector("list", length(A))
  rhs <- numeric(length(A))
  for (i in seq_along(A)) {
    rows[[i]] <- as.numeric(A[[i]])
    rhs[i] <- as.numeric(b[i])
  }
  for (bk in bounds) {
    r <- rep(0, n)
    if (bk$sense == "le") r[bk$var] <- 1 else r[bk$var] <- -1
    rows[[length(rows) + 1]] <- r
    if (bk$sense == "le") rhs <- c(rhs, as.numeric(bk$value))
    else rhs <- c(rhs, -as.numeric(bk$value))
  }
  m <- length(rows)
  full <- vector("list", m)
  for (i in seq_len(m)) {
    full[[i]] <- c(rows[[i]], vapply(seq_len(m), function(t)
      if (t == i) 1 else 0, numeric(1)))
  }
  cc <- c(as.numeric(c), rep(0, m))
  list(M = full, rhs = rhs, c = cc)
}

#' @keywords internal
#' @noRd
.ghc_mipinterior <- function(M, rhs, c, tol = 1e-10, max_iter = 200) {
  m <- length(M)
  n <- length(M[[1]])
  X <- matrix(0, m, n)
  for (i in seq_len(m)) X[i, ] <- as.numeric(M[[i]])
  rb <- as.numeric(rhs)
  rc <- as.numeric(c)
  x <- rep(1, n)
  for (it in seq_len(as.integer(max_iter))) {
    Ax <- as.numeric(X %*% x)
    rd <- rc - as.numeric(t(X) %*% (rb - Ax))
    if (max(abs(rd)) < tol) break
    Ad <- as.numeric(X %*% rd)
    alpha <- min(1, min(rb[Ad > 0] / Ad[Ad > 0], Inf))
    x <- x + alpha * rd
  }
  Ax <- as.numeric(X %*% x)
  if (max(rb - Ax) < -tol)
    return(list(converged = FALSE, x = rep(0, n))
    )
  list(converged = TRUE, x = x[seq_len(length(c) - m)])
}

#' LP relaxation at one node
#'
#' Solves the LP relaxation by simplex (the only choice safe for
#' branch and bound) or by a Mehrotra-style interior point method
#' (better on large sparse LPs, but unsafe once a branch collapses
#' the region to a single point, so kept but not default).
#'
#' @param A List of row vectors of the original inequality system.
#' @param b Right-hand side.
#' @param c Objective coefficients.
#' @param bounds List of list(var, sense, value) for branch bounds.
#' @param n Number of decision variables.
#' @param maximise Maximise if TRUE.
#' @param solver "simplex" or "interior".
#' @return A list with feasible, x, value, note.
#' @export
morie_miprgr_solve_relaxation <- function(A, b, c, bounds = list(),
                                          n = NULL,
                                          maximise = TRUE,
                                          solver = "simplex") {
  nn <- if (is.null(n)) length(c) else as.integer(n)
  if (solver == "simplex") {
    rows <- vector("list", length(A))
    rhs2 <- numeric(length(A))
    for (i in seq_along(A)) {
      rows[[i]] <- as.numeric(A[[i]])
      rhs2[i] <- as.numeric(b[i])
    }
    for (bk in bounds) {
      r <- rep(0, nn)
      if (bk$sense == "le") r[bk$var] <- 1 else r[bk$var] <- -1
      rows[[length(rows) + 1]] <- r
      if (bk$sense == "le") rhs2 <- c(rhs2, as.numeric(bk$value))
      else rhs2 <- c(rhs2, -as.numeric(bk$value))
    }
    sgn <- if (maximise) 1 else -1
    out <- .ghc_simplex_mip(rows, rhs2, sgn * as.numeric(c))
    if (!out$feasible)
      return(list(feasible = FALSE, x = NULL, value = NULL,
                  note = paste0("the relaxation is infeasible, so ",
                                "every integer point below this node ",
                                "is too")))
    x <- pmax(0, out$x)
    val <- sum(as.numeric(c) * x)
    return(list(feasible = TRUE, x = x, value = val,
                note = paste0("a valid BOUND on every integer point ",
                              "below this node")))
  }
  if (solver != "interior")
    stop("miprgr: solver must be simplex or interior, got ", solver)
  std <- .ghc_standard_form(A, b, c, bounds, nn)
  obj <- if (maximise) -std$c else std$c
  r <- tryCatch(.ghc_mipinterior(std$M, std$rhs, obj, tol = 1e-10,
                                 max_iter = 200),
                error = function(e)
                  list(converged = FALSE, x = rep(0, nn)))
  if (!r$converged)
    return(list(feasible = FALSE, x = NULL, value = NULL,
                note = paste0("the relaxation is infeasible, so ",
                              "every integer point below this node ",
                              "is too")))
  x <- pmax(0, r$x)
  for (i in seq_along(A)) {
    lhs <- sum(as.numeric(A[[i]]) * x)
    if (lhs > as.numeric(b[i]) + 1e-6)
      return(list(feasible = FALSE, x = NULL, value = NULL,
                  note = "the relaxation violates an original constraint"))
  }
  for (bk in bounds) {
    if (bk$sense == "le" && x[bk$var] > as.numeric(bk$value) + 1e-6)
      return(list(feasible = FALSE, x = NULL, value = NULL,
                  note = "branch bound violated"))
    if (bk$sense == "ge" && x[bk$var] < as.numeric(bk$value) - 1e-6)
      return(list(feasible = FALSE, x = NULL, value = NULL,
                  note = "branch bound violated"))
  }
  val <- sum(as.numeric(c) * x)
  list(feasible = TRUE, x = x, value = val,
       note = paste0("a valid BOUND on every integer point below this ",
                     "node"))
}

#' Most fractional integer variable
#'
#' @param x Solution vector.
#' @param integer_vars Indices that must be integral.
#' @param tol Tolerance.
#' @return A list with index, fractionality, integral.
#' @export
morie_miprgr_fractional_variable <- function(x, integer_vars,
                                              tol = .GHC_MIP_EPS) {
  best <- NA_integer_
  gap <- 0
  for (j in as.integer(integer_vars)) {
    v <- as.numeric(x[j])
    f <- abs(v - round(v))
    if (f > tol && f > gap) { best <- j
    gap <- f }
  }
  list(index = if (is.na(best)) NULL else best,
       fractionality = gap,
       integral = is.na(best))
}

#' Round the relaxation and report what it gives
#'
#' @param x Relaxed solution.
#' @param A Original inequalities.
#' @param b Right-hand side.
#' @param integer_vars Indices that must be integral.
#' @return A list with x, feasible, violations, note.
#' @export
morie_miprgr_round_relaxation <- function(x, A, b, integer_vars) {
  xr <- as.numeric(x)
  for (j in as.integer(integer_vars)) xr[j] <- round(xr[j])
  viol <- list()
  for (i in seq_along(A)) {
    lhs <- sum(as.numeric(A[[i]]) * xr)
    if (lhs > as.numeric(b[i]) + .GHC_MIP_EPS) {
      viol[[length(viol) + 1]] <- list(row = i - 1L, lhs = lhs,
                                        rhs = as.numeric(b[i]))
    }
  }
  list(x = xr, feasible = length(viol) == 0L, violations = viol,
       note = "rounding is not a substitute for branching")
}

#' Brute-force enumeration over a small integer box
#'
#' @param A,b,c,integer_vars,maximise As in \code{branch_and_bound}.
#' @param upper Inclusive upper bound of every variable.
#' @return A list with value, x, note.
#' @export
morie_miprgr_enumerate_integer <- function(A, b, c, integer_vars,
                                            upper = 10,
                                            maximise = TRUE) {
  n <- length(c)
  best <- if (maximise) -Inf else Inf
  best_x <- NULL
  stack <- list(list())
  while (length(stack) > 0) {
    pre <- stack[[length(stack)]]
    stack[[length(stack)]] <- NULL
    if (length(pre) == n) {
      ok <- TRUE
      for (i in seq_along(A)) {
        lhs <- sum(as.numeric(A[[i]]) * pre)
        if (lhs > as.numeric(b[i]) + .GHC_MIP_EPS) { ok <- FALSE
        break }
      }
      if (ok) {
        val <- sum(as.numeric(c) * pre)
        better <- if (maximise) val > best else val < best
        if (better) { best <- val
        best_x <- pre }
      }
      next
    }
    j <- length(pre) + 1
    rng <- 0:as.integer(upper)
    for (v in rev(rng)) stack[[length(stack) + 1]] <- c(pre, as.numeric(v))
  }
  list(value = best, x = best_x,
       note = paste0("exhaustive over the box, so the search can be ",
                     "checked against something other than itself"))
}

#' Branch and bound (Dakin 1965 Fig. 2, marked list)
#'
#' @param A,b,c,integer_vars,maximise As above.
#' @param prune Apply the bound test.
#' @param max_nodes Hard cap on relaxation solves.
#' @param solver "simplex" or "interior".
#' @return A list with the incumbent, the relaxation node count, the
#'   number of nodes pruned, the maximum length of the marked list,
#'   and the root relaxation value.
#' @export
morie_miprgr_branch_and_bound <- function(A, b, c, integer_vars,
                                          maximise = TRUE,
                                          prune = TRUE,
                                          max_nodes = 5000L,
                                          solver = "simplex") {
  n <- length(c)
  I <- sort(unique(as.integer(integer_vars)))
  if (any(I < 0 | I >= n))
    stop("miprgr: an integer index is outside the variable set")
  better <- if (maximise)
    function(a, bb) a > bb + .GHC_MIP_EPS
  else
    function(a, bb) a < bb - .GHC_MIP_EPS
  incumbent <- if (maximise) -Inf else Inf
  inc_x <- NULL
  lst <- list()
  nodes <- 0L
  pruned <- 0L
  max_len <- 0L
  root_bound <- NULL
  while (nodes < as.integer(max_nodes)) {
    bounds <- lapply(lst, function(e) list(var = e$var, sense = e$sense,
                                           value = e$value))
    rel <- morie_miprgr_solve_relaxation(A, b, c, bounds, n, maximise,
                                          solver)
    nodes <- nodes + 1L
    if (length(lst) > max_len) max_len <- length(lst)
    if (is.null(root_bound) && isTRUE(rel$feasible))
      root_bound <- rel$value
    descend <- FALSE
    if (isTRUE(rel$feasible)) {
      cut <- prune && !is.null(inc_x) &&
        !better(rel$value, incumbent)
      if (cut) {
        pruned <- pruned + 1L
      } else {
        fv <- morie_miprgr_fractional_variable(rel$x, I)
        if (isTRUE(fv$integral)) {
          if (better(rel$value, incumbent)) {
            incumbent <- rel$value
            inc_x <- vapply(seq_len(n), function(j, vx)
              if (j %in% I) round(vx[j]) else vx[j], numeric(1),
              vx = rel$x)
          }
        } else {
          j <- fv$index
          v <- rel$x[j]
          lst[[length(lst) + 1L]] <- list(var = j, sense = "le",
                                          value = floor(v),
                                          alt = list("ge", ceiling(v)),
                                          marked = FALSE)
          descend <- TRUE
        }
      }
    }
    if (descend) next
    repeat {
      if (length(lst) == 0L) {
        return(list(estimate = if (!is.null(inc_x)) incumbent else NULL,
                    value = if (!is.null(inc_x)) incumbent else NULL,
                    x = inc_x,
                    feasible = !is.null(inc_x),
                    nodes = nodes, pruned = pruned,
                    pruning = isTRUE(prune),
                    max_list_length = max_len,
                    root_bound = root_bound,
                    method = paste0("branch and bound; Land & Doig ",
                                    "(1960), Dakin (1965) Fig. 2"),
                    note = paste0("the list holds the current PATH, ",
                                  "so its length is the tree depth, ",
                                  "not the number of open nodes")))
      }
      last <- lst[[length(lst)]]
      if (isTRUE(last$marked)) {
        lst <- lst[-length(lst)]
        next
      }
      alt <- last$alt
      last$alt <- list(last$sense, last$value)
      last$sense <- alt[[1]]
      last$value <- alt[[2]]
      last$marked <- TRUE
      lst[[length(lst)]] <- last
      break
    }
  }
  list(estimate = if (!is.null(inc_x)) incumbent else NULL,
       value = if (!is.null(inc_x)) incumbent else NULL,
       x = inc_x,
       feasible = !is.null(inc_x),
       nodes = nodes, pruned = pruned, pruning = isTRUE(prune),
       max_list_length = max_len, root_bound = root_bound,
       truncated = TRUE,
       method = paste0("branch and bound; Land & Doig (1960), Dakin ",
                       "(1965) Fig. 2"),
       note = paste0("node limit reached, so the result is NOT proven",
                     " optimal"))
}

morie_miprgr <- morie_miprgr_branch_and_bound
