# Frank-Wolfe (conditional gradient) for quadratic programs.
# Sources: Frank, M., & Wolfe, P. (1956) "An algorithm for quadratic
# programming", Naval Research Logistics Quarterly 3(1-2), 95-110;
# Wolfe, P. (1959) "The simplex method for quadratic programming",
# Econometrica 27(3), 382-398. Mirroring morie.fn.qpdual: same linear
# minimisation oracle, same gap = <grad, x - s> certificate, same exact
# line search and 2/(k+2) schedule, same simplex and box domains.

.STEPS <- c("exact", "standard")
.DOMAINS <- c("simplex", "box")

.lmo <- function(gradient, domain, lower, upper) {
  n <- length(gradient)
  if (domain == "simplex") {
    j <- which.min(gradient)[1]
    s <- rep(0, n); s[j] <- 1
    return(s)
  }
  vapply(seq_len(n), function(i) if (gradient[i] > 0) lower[i] else upper[i],
         numeric(1))
}

morie_qpdual_frank_wolfe_qp <- function(Q, c, x0 = NULL, domain = "simplex",
                                        lower = NULL, upper = NULL,
                                        step = "exact", max.iter = 1000L,
                                        tol = 1e-12) {
  Qm <- as.matrix(Q)
  n <- nrow(Qm)
  if (ncol(Qm) != n) stop("frank_wolfe_qp: Q must be square")
  cv <- as.numeric(c)
  if (length(cv) != n)
    stop(paste0("frank_wolfe_qp: c has length ", length(cv),
                " but Q is ", n, "x", n))
  dom <- tolower(domain)
  if (!(dom %in% .DOMAINS))
    stop(paste0("frank_wolfe_qp: domain must be one of ",
                paste(.DOMAINS, collapse = ", "), ", got ", domain))
  st <- tolower(step)
  if (!(st %in% .STEPS))
    stop(paste0("frank_wolfe_qp: step must be one of ",
                paste(.STEPS, collapse = ", "), ", got ", step))
  if (dom == "box") {
    if (is.null(lower) || is.null(upper))
      stop("frank_wolfe_qp: domain='box' needs lower and upper")
    lo <- as.numeric(lower); hi <- as.numeric(upper)
    if (any(lo > hi))
      stop("frank_wolfe_qp: lower exceeds upper")
    x <- if (is.null(x0)) 0.5 * (lo + hi) else as.numeric(x0)
  } else {
    lo <- NULL; hi <- NULL
    x <- if (is.null(x0)) rep(1 / n, n) else as.numeric(x0)
  }
  grad <- function(v) as.numeric(Qm %*% v + cv)
  obj <- function(v) 0.5 * as.numeric(t(v) %*% Qm %*% v) + sum(cv * v)
  gap <- Inf
  it <- 0L; converged <- FALSE
  history <- obj(x)
  for (it in seq_len(as.integer(max.iter))) {
    g <- grad(x)
    s <- .lmo(g, dom, lo, hi)
    d <- s - x
    gap <- -sum(g * d)
    if (gap <= tol) { converged <- TRUE; break }
    if (st == "exact") {
      dQd <- as.numeric(t(d) %*% Qm %*% d)
      gamma <- if (dQd <= 0) 1 else min(1, max(0, gap / dQd))
    } else gamma <- 2 / (it + 2)
    x <- x + gamma * d
    history <- c(history, obj(x))
  }
  list(estimate = x, x = x, fun = as.numeric(obj(x)),
       gap = as.numeric(gap), iterations = as.integer(it),
       converged = converged, domain = dom, step = st,
       history = history,
       method = "Frank-Wolfe conditional gradient (Frank & Wolfe 1956); gap bounds f(x) - f(x*)")
}

# house entry point: the package exports one morie_<module>
morie_qpdual <- morie_qpdual_frank_wolfe_qp
