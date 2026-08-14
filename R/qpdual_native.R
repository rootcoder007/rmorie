# Frank-Wolfe (conditional gradient) for quadratic programs.
# Sources: Frank, M., & Wolfe, P. (1956) "An algorithm for quadratic
# programming", Naval Research Logistics Quarterly 3(1-2), 95-110;
# Wolfe, P. (1959) "The simplex method for quadratic programming",
# Econometrica 27(3), 382-398.
#
# Native implementation mirroring morie.fn.qpdual exactly: the same
# linear minimisation oracle over the simplex or a box, the same
# Frank-Wolfe update x += gamma (s - x), the same gap certificate
# <grad, x - s> which is a real upper bound on f(x) - f* (not a
# gradient norm), and the same two step rules: exact line search on
# the quadratic and the 2/(k+2) schedule.

.STEPS <- c("exact", "standard")
.DOMAINS <- c("simplex", "box")

# Linear minimisation oracle
.lmo <- function(gradient, domain, lower, upper) {
  n <- length(gradient)
  if (domain == "simplex") {
    j <- which.min(gradient)[1L]
    s <- rep(0.0, n)
    s[j] <- 1.0
    return(s)
  }
  vapply(seq_len(n), function(i)
    if (gradient[i] > 0) lower[i] else upper[i], numeric(1))
}

#' Frank-Wolfe for a quadratic objective on a compact convex set
#'
#' Minimises \code{1/2 x'Qx + c'x} over the probability simplex or a
#' box. Feasibility is preserved exactly: each iterate is a convex
#' combination of the previous point and a vertex of \code{C}.
#'
#' @param Q Symmetric n-by-n numeric matrix.
#' @param c Numeric vector of length \code{n}.
#' @param x0 Optional starting point (default: simplex centre, box midpoint).
#' @param domain One of \code{"simplex"} or \code{"box"}.
#' @param lower,upper Optional numeric vectors; required for \code{box}.
#' @param step One of \code{"exact"} (line search on the quadratic)
#'   or \code{"standard"} (the 2/(k+2) schedule).
#' @param max_iter Integer, maximum iterations.
#' @param tol Stop when the Frank-Wolfe gap drops below \code{tol}.
#' @return A list with \code{estimate}, \code{x}, \code{fun},
#'   \code{gap}, \code{iterations}, \code{converged}, \code{domain},
#'   \code{step}, \code{history}, \code{method}.
#' @export
morie_qpdual <- function(Q, c, x0 = NULL, domain = "simplex",
                         lower = NULL, upper = NULL, step = "exact",
                         max_iter = 1000L, tol = 1e-12) {
  Qm <- as.matrix(Q)
  storage.mode(Qm) <- "double"
  n <- nrow(Qm)
  if (ncol(Qm) != n) stop("frank_wolfe_qp: Q must be square")
  cv <- as.numeric(c)
  if (length(cv) != n)
    stop(sprintf("frank_wolfe_qp: c has length %d but Q is %dx%d",
                 length(cv), n, n))
  dom <- tolower(as.character(domain))
  if (!(dom %in% .DOMAINS))
    stop(sprintf("frank_wolfe_qp: domain must be one of %s, got %r",
                 paste(.DOMAINS, collapse = ", "), domain))
  st <- tolower(as.character(step))
  if (!(st %in% .STEPS))
    stop(sprintf("frank_wolfe_qp: step must be one of %s, got %r",
                 paste(.STEPS, collapse = ", "), step))
  lo <- hi <- NULL
  if (dom == "box") {
    if (is.null(lower) || is.null(upper))
      stop("frank_wolfe_qp: domain='box' needs lower and upper")
    lo <- as.numeric(lower); hi <- as.numeric(upper)
    if (length(lo) != n || length(hi) != n)
      stop("frank_wolfe_qp: lower and upper must have length n")
    for (i in seq_len(n))
      if (lo[i] > hi[i])
        stop(sprintf("frank_wolfe_qp: lower[%d] exceeds upper[%d]", i - 1L, i - 1L))
    if (is.null(x0)) x <- 0.5 * (lo + hi) else x <- as.numeric(x0)
  } else {
    if (is.null(x0)) x <- rep(1.0 / n, n) else x <- as.numeric(x0)
  }
  grad <- function(v) as.numeric(Qm %*% v) + cv
  obj <- function(v) 0.5 * sum(v * (Qm %*% v)) + sum(cv * v)
  gap <- Inf
  it <- 0L
  converged <- FALSE
  history <- obj(x)
  for (it in seq_len(as.integer(max_iter))) {
    g <- grad(x)
    s <- .lmo(g, dom, lo, hi)
    d <- s - x
    gap <- -sum(g * d)
    if (gap <= tol) {
      converged <- TRUE
      break
    }
    if (st == "exact") {
      dQd <- sum(d * (Qm %*% d))
      gamma <- if (dQd <= 0) 1.0 else min(1.0, max(0.0, gap / dQd))
    } else {
      # it is 1-based here, so the Frank-Wolfe index is it-1 and
      # the open-loop step is 2/((it-1)+2); using 2/(it+2) would
      # skip gamma = 1 on the first step and shift the schedule
      gamma <- 2.0 / (it + 1.0)
    }
    x <- x + gamma * d
    history <- c(history, obj(x))
  }
  list(estimate = x, x = x, fun = obj(x), gap = gap,
       iterations = as.integer(it), converged = converged,
       domain = dom, step = st, history = history,
       method = paste0("Frank-Wolfe conditional gradient ",
                       "(Frank & Wolfe 1956); ",
                       "gap bounds f(x) - f(x*)"))
}

#' @export
frank_wolfe_qp <- morie_qpdual

#' @export
qpdual <- morie_qpdual

#' @export
quadratic_program <- morie_qpdual

#' @export
qpdual_cheatsheet <- function() {
  paste0("qpdual: Frank-Wolfe, s = argmin_C <grad, s>, ",
         "x += gamma (s - x); ",
         "gap = <grad, x - s> >= f(x) - f*; ",
         "steps exact / 2/(k+2); domains simplex / box.")
}
