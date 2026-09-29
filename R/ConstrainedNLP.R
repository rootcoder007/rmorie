.gi_qp <- function(G, a, C, b, meq = 0, tol = 1e-12, max_iter = NULL) {
  n <- length(a)
  m <- length(b)
  Gi <- solve(G)
  x <- -as.vector(Gi %*% a)
  A <- integer(0)
  uA <- numeric(0)
  sA <- numeric(0)
  if (is.null(max_iter)) max_iter <- 50 * (n + m) + 50
  build <- function() {
    if (!length(A)) return(list(H = Gi, Ns = matrix(0, 0, n)))
    N <- t(C[A, , drop = FALSE] * sA)
    GiN <- Gi %*% N
    Mi <- solve(crossprod(N, GiN))
    Ns <- Mi %*% t(GiN)
    list(H = Gi - GiN %*% Ns, Ns = Ns)
  }
  hb <- build()
  for (it in seq_len(max_iter)) {
    slack <- as.vector(C %*% x) - b
    p <- 0
    sp <- 0
    if (meq > 0) {
      for (k in seq_len(meq)) {
        if (!(k %in% A) && abs(slack[k]) > tol * max(1, abs(b[k]))) {
          p <- k
          sp <- slack[k]
          break
        }
      }
    }
    if (p == 0 && m > meq) {
      worst <- -tol
      for (k in (meq + 1):m) {
        if (!(k %in% A) && slack[k] < worst * max(1, abs(b[k]))) {
          worst <- slack[k]
          p <- k
          sp <- slack[k]
        }
      }
    }
    if (p == 0) {
      u <- numeric(m)
      u[A] <- sA * uA
      return(list(x = x, u = u, active = sort(A)))
    }
    sgn <- if (p > meq || sp < 0) 1 else -1
    npv <- sgn * C[p, ]
    up <- 0
    repeat {
      z <- as.vector(hb$H %*% npv)
      r <- if (length(A)) as.vector(hb$Ns %*% npv) else numeric(0)
      t1 <- Inf
      kdrop <- 0
      for (i in seq_along(A)) {
        if (A[i] > meq && r[i] > 1e-14 && uA[i] / r[i] < t1) {
          t1 <- uA[i] / r[i]
          kdrop <- i
        }
      }
      zn <- sum(z * npv)
      t2 <- if (zn > 1e-11 * sum(npv * (Gi %*% npv))) -(sgn * (sum(C[p, ] * x) - b[p])) / zn else Inf
      tt <- min(t1, t2)
      if (!is.finite(tt)) stop("the constraints are inconsistent", call. = FALSE)
      if (is.finite(t2)) x <- x + tt * z
      uA <- uA - tt * r
      up <- up + tt
      if (tt == t2) {
        A <- c(A, p)
        uA <- c(uA, up)
        sA <- c(sA, sgn)
        hb <- build()
        break
      }
      A <- A[-kdrop]
      uA <- uA[-kdrop]
      sA <- sA[-kdrop]
      hb <- build()
    }
  }
  stop("Goldfarb-Idnani did not terminate", call. = FALSE)
}

.lp_simplex <- function(cv, A, b, tol = 1e-10, max_iter = 5000) {
  m <- nrow(A)
  n <- length(cv)
  W <- n + m
  sg <- ifelse(b < 0, -1, 1)
  T <- cbind(A * sg, diag(m), b * sg)
  basis <- n + seq_len(m)
  pivot <- function(r, q) {
    T[r, ] <<- T[r, ] / T[r, q]
    for (i in seq_len(m)) {
      if (i != r && T[i, q] != 0) T[i, ] <<- T[i, ] - T[i, q] * T[r, ]
    }
    basis[r] <<- q
  }
  run <- function(cost, allowed) {
    for (it in seq_len(max_iter)) {
      q <- 0
      for (j in allowed) {
        z <- cost[j]
        for (i in seq_len(m)) z <- z - cost[basis[i]] * T[i, j]
        if (z < -tol && !(j %in% basis)) {
          q <- j
          break
        }
      }
      if (q == 0) return("optimal")
      rows <- which(T[, q] > tol)
      if (!length(rows)) return("unbounded")
      rat <- T[rows, W + 1] / T[rows, q]
      best <- min(rat)
      cand <- rows[rat <= best + tol]
      r <- cand[order(basis[cand])[1]]
      pivot(r, q)
    }
    stop("simplex did not terminate", call. = FALSE)
  }
  run(c(rep(0, n), rep(1, m)), seq_len(W))
  art <- basis > n
  s0 <- 0
  for (i in which(art)) s0 <- s0 + T[i, W + 1]
  if (s0 > 1e-8 * max(1, sum(abs(b)))) return(list(x = NULL, status = "infeasible"))
  for (i in seq_len(m)) {
    if (basis[i] > n) {
      q <- which(abs(T[i, seq_len(n)]) > tol)
      if (length(q)) pivot(i, q[1])
    }
  }
  status <- run(c(cv, rep(0, m)), seq_len(n))
  x <- numeric(n)
  for (i in seq_len(m)) if (basis[i] <= n) x[basis[i]] <- T[i, W + 1]
  list(x = x, status = status)
}

.nlp_jac <- function(fs, x) if (length(fs)) do.call(rbind, lapply(fs, function(cf) .qn_num_grad(cf, x))) else matrix(0, 0, length(x))

#' Sequential quadratic programming
#'
#' Line-search SQP (Nocedal and Wright 2006, Algorithm 18.3): each QP
#' subproblem min p'Bp/2 + g'p s.t. A_E p + c_E = 0, A_I p + c_I >= 0 is solved
#' exactly by the Goldfarb-Idnani dual active-set method; the l1 merit
#' f + mu (sum |c_E| + sum max(0, -c_I)) with mu = max(mu, 1.1 max |lambda|) is
#' decreased by halving backtracking; B follows Powell's damped BFGS update on
#' the Lagrangian gradient L = f - lambda'c.
#'
#' @param f Objective.
#' @param x0 Starting point.
#' @param grad Gradient of f (central differences when NULL).
#' @param eq,ineq Lists of constraint functions (c(x) = 0 and c(x) >= 0).
#' @param eq_jac,ineq_jac Functions returning constraint Jacobians (rows), or NULL.
#' @param tol KKT tolerance.
#' @param max_iter Iteration cap.
#' @return list(x, fun, multipliers_eq, multipliers_ineq, kkt_residual,
#'   violation, n_iter, converged).
#' @references Nocedal, J. and Wright, S. J. (2006). Numerical Optimization,
#'   2nd ed., Algorithm 18.3. Goldfarb, D. and Idnani, A. (1983). A numerically
#'   stable dual method for solving strictly convex quadratic programs.
#'   Mathematical Programming 27, 1-33.
#' @examples
#' SequentialQuadraticProgramming(function(x) sum(x^2), c(2, 0),
#'   eq = list(function(x) x[1] + x[2] - 1))$x
#' @export
SequentialQuadraticProgramming <- function(f, x0, grad = NULL, eq = list(), ineq = list(), eq_jac = NULL, ineq_jac = NULL,
                                           tol = 1e-8, max_iter = 200) {
  gr <- if (is.null(grad)) function(z) .qn_num_grad(f, z) else grad
  je <- if (is.null(eq_jac)) function(z) .nlp_jac(eq, z) else function(z) matrix(eq_jac(z), nrow = length(eq))
  ji <- if (is.null(ineq_jac)) function(z) .nlp_jac(ineq, z) else function(z) matrix(ineq_jac(z), nrow = length(ineq))
  x <- as.numeric(x0)
  n <- length(x)
  me <- length(eq)
  mi <- length(ineq)
  cons <- function(z) list(e = vapply(eq, function(cf) as.numeric(cf(z)), 0), i = vapply(ineq, function(cf) as.numeric(cf(z)), 0))
  viol <- function(cc) sum(abs(cc$e)) + sum(pmax(0, -cc$i))
  lagg <- function(g, AE, AI, le, li) g - as.vector(crossprod(AE, le)) - as.vector(crossprod(AI, li))
  B <- diag(n)
  mu <- 0
  le <- numeric(me)
  li <- numeric(mi)
  fx <- as.numeric(f(x))
  g <- as.numeric(gr(x))
  cc <- cons(x)
  AE <- je(x)
  AI <- ji(x)
  it <- 0
  conv <- FALSE
  kkt <- max(abs(lagg(g, AE, AI, le, li)))
  while (it < max_iter) {
    Cm <- rbind(AE, AI)
    qp <- .gi_qp(B, g, Cm, -c(cc$e, cc$i), me)
    p <- qp$x
    le <- qp$u[seq_len(me)]
    li <- qp$u[me + seq_len(mi)]
    kkt <- max(abs(lagg(g, AE, AI, le, li)))
    v0 <- viol(cc)
    if (kkt <= tol && v0 <= tol) {
      conv <- TRUE
      break
    }
    if (max(abs(p)) <= 1e-15 * max(1, max(abs(x)))) {
      conv <- kkt <= 1e3 * tol && v0 <= 1e3 * tol
      break
    }
    mu <- max(mu, 1.1 * max(c(0, abs(qp$u))), 1e-8)
    phi0 <- fx + mu * v0
    D <- sum(g * p) - mu * v0
    a <- 1
    repeat {
      xn <- x + a * p
      fn <- as.numeric(f(xn))
      cn <- cons(xn)
      if (fn + mu * viol(cn) <= phi0 + 1e-4 * a * D || a < 1e-10) break
      a <- a / 2
    }
    gn <- as.numeric(gr(xn))
    AEn <- je(xn)
    AIn <- ji(xn)
    s <- xn - x
    y <- lagg(gn, AEn, AIn, le, li) - lagg(g, AE, AI, le, li)
    Bs <- as.vector(B %*% s)
    sBs <- sum(s * Bs)
    sy <- sum(s * y)
    if (sBs > 1e-300) {
      th <- if (sy >= 0.2 * sBs) 1 else 0.8 * sBs / (sBs - sy)
      r <- th * y + (1 - th) * Bs
      B <- B - outer(Bs, Bs) / sBs + outer(r, r) / sum(s * r)
    }
    x <- xn
    fx <- fn
    g <- gn
    cc <- cn
    AE <- AEn
    AI <- AIn
    it <- it + 1
  }
  list(x = x, fun = fx, multipliers_eq = le, multipliers_ineq = li, kkt_residual = kkt,
       violation = viol(cc), n_iter = it, converged = conv)
}

#' Sequential linear programming
#'
#' Trust-region SLP on the l1 exact penalty (Fletcher and Sainz de la Maza
#' 1989): the linearised penalty model is minimised over |p_i| <= Delta as an
#' elastic LP solved exactly by a two-phase simplex with Bland's rule; steps
#' with rho > 0.1 are accepted, Delta shrinks to ||p|| / 4 when rho < 0.25 and
#' doubles when rho > 0.75 at the boundary, and mu grows tenfold while the limit
#' is infeasible. Reaches vertex solutions finitely; first-order at non-vertex
#' optima.
#'
#' @inheritParams SequentialQuadraticProgramming
#' @param mu Initial penalty weight.
#' @param delta Initial trust-region radius.
#' @return list(x, fun, violation, penalty, radius, n_iter, converged).
#' @references Fletcher, R. and Sainz de la Maza, E. (1989). Nonlinear
#'   programming and nonsmooth optimization by successive linear programming.
#'   Mathematical Programming 43, 235-256.
#' @examples
#' SequentialLinearProgramming(function(x) sum((x - 5)^2), c(0, 0),
#'   ineq = list(function(x) 1 - x[1], function(x) 2 - x[2]))$x
#' @export
SequentialLinearProgramming <- function(f, x0, grad = NULL, eq = list(), ineq = list(), eq_jac = NULL, ineq_jac = NULL,
                                        mu = 1, delta = 1, tol = 1e-8, max_iter = 500) {
  gr <- if (is.null(grad)) function(z) .qn_num_grad(f, z) else grad
  je <- if (is.null(eq_jac)) function(z) .nlp_jac(eq, z) else function(z) matrix(eq_jac(z), nrow = length(eq))
  ji <- if (is.null(ineq_jac)) function(z) .nlp_jac(ineq, z) else function(z) matrix(ineq_jac(z), nrow = length(ineq))
  x <- as.numeric(x0)
  n <- length(x)
  me <- length(eq)
  mi <- length(ineq)
  cons <- function(z) list(e = vapply(eq, function(cf) as.numeric(cf(z)), 0), i = vapply(ineq, function(cf) as.numeric(cf(z)), 0))
  viol <- function(cc) sum(abs(cc$e)) + sum(pmax(0, -cc$i))
  D <- delta
  fx <- as.numeric(f(x))
  cc <- cons(x)
  it <- 0
  conv <- FALSE
  while (it < max_iter) {
    g <- as.numeric(gr(x))
    AE <- je(x)
    AI <- ji(x)
    nv <- 2 * n + 2 * me + 2 * mi
    cv <- c(g, rep(0, n), rep(mu, 2 * me), rep(mu, mi), rep(0, mi))
    rows <- list()
    b <- numeric(0)
    for (j in seq_len(n)) {
      row <- numeric(nv)
      row[j] <- 1
      row[n + j] <- 1
      rows[[length(rows) + 1]] <- row
      b <- c(b, 2 * D)
    }
    for (k in seq_len(me)) {
      row <- numeric(nv)
      row[seq_len(n)] <- AE[k, ]
      row[2 * n + k] <- -1
      row[2 * n + me + k] <- 1
      rows[[length(rows) + 1]] <- row
      b <- c(b, -cc$e[k] + D * sum(AE[k, ]))
    }
    for (k in seq_len(mi)) {
      row <- numeric(nv)
      row[seq_len(n)] <- AI[k, ]
      row[2 * n + 2 * me + k] <- 1
      row[2 * n + 2 * me + mi + k] <- -1
      rows[[length(rows) + 1]] <- row
      b <- c(b, -cc$i[k] + D * sum(AI[k, ]))
    }
    sol <- .lp_simplex(cv, do.call(rbind, rows), b)
    p <- pmin(pmax(sol$x[seq_len(n)] - D, -D), D)
    mp <- fx + sum(g * p) + mu * (sum(abs(cc$e + as.vector(AE %*% p))) + sum(pmax(0, -(cc$i + as.vector(AI %*% p)))))
    phi <- fx + mu * viol(cc)
    pred <- phi - mp
    if (pred <= tol * max(1, abs(phi)) || D <= tol) {
      if (viol(cc) > tol && mu < 1e8) {
        mu <- mu * 10
        D <- max(D, delta)
        it <- it + 1
        next
      }
      conv <- pred <= tol * max(1, abs(phi))
      break
    }
    xn <- x + p
    fn <- as.numeric(f(xn))
    cn <- cons(xn)
    rho <- (phi - (fn + mu * viol(cn))) / pred
    pn <- max(abs(p))
    if (rho > 0.1) {
      x <- xn
      fx <- fn
      cc <- cn
    }
    if (rho < 0.25) {
      D <- 0.25 * pn
    } else if (rho > 0.75 && pn >= 0.99 * D) {
      D <- 2 * D
    }
    it <- it + 1
  }
  list(x = x, fun = fx, violation = viol(cc), penalty = mu, radius = D, n_iter = it, converged = conv)
}
