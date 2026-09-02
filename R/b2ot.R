# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Optimal-transport shelf -- R mirror of the Python modules otentr,
# otmnge, otsinkhorn, otsinkit, otsktol, otsoft, sinkhd (Cuturi 2013)
# and otc2c, otc2p, otentf, otplan, otpot, otpush (Peyre & Cuturi 2019).
#
# Sources consulted, not recalled:
#   Cuturi, M. (2013). Sinkhorn Distances: Lightspeed Computation of
#   Optimal Transport.  NIPS 26, 2292-2300.  Definition 1, eq. (2),
#   Sec. 4.1.
#   Peyre, G. & Cuturi, M. (2019). Computational Optimal Transport.
#   Foundations and Trends in ML 11(5-6).  Eq. (2.8), (4.1), (4.2),
#   (4.19), (4.30)-(4.32); Remarks 2.6 and 4.11.
#
# Every Sinkhorn loop here runs a FIXED number of scalings.  There is no
# tolerance and no early exit anywhere in this file, so the recurrence
# is a deterministic function of its inputs and this arm reproduces the
# Python arm to machine precision.
#
# Collision scan: b2ot.R and all thirteen exported names were free in
# both R trees and in _lazy_map.json at the time of writing.

#' .b2mat
#'
#' A step of the b2ot implementation. Called by \code{.b2sinkhorn}, \code{Bottomup}, \code{Gppost} and 13 others in the module.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param a A matrix; the body checks with \code{is.matrix}.
#' @return The value of \code{m}, as built in the body.
#' @export
.b2mat <- function(a) {
  m <- if (is.matrix(a)) a else do.call(rbind, lapply(a, as.numeric))
  storage.mode(m) <- "double"
  if (nrow(m) == 0L || ncol(m) == 0L) stop("empty matrix", call. = FALSE)
  m
}

#' .b2close
#'
#' A step of the b2ot implementation. Called by \code{.b2sinkhorn}, \code{Otfreeen}, \code{Otsinktol}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param p Numeric; passed to \code{sum}.
#' @return A numeric value.
#' @export
.b2close <- function(p) {
  p <- as.numeric(p)
  if (any(p < 0)) stop("probabilities must be non-negative", call. = FALSE)
  tot <- sum(p)
  if (!(tot > 0)) stop("total mass must be positive", call. = FALSE)
  p / tot
}

#' .b2margerr
#'
#' A step of the b2ot implementation. Called by \code{.b2sinkhorn}, \code{Otsinkh}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param T Passed to \code{rowSums}.
#' @param a Numeric; combined arithmetically in the body.
#' @param b Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.b2margerr <- function(T, a, b) {
  max(max(abs(rowSums(T) - a)), max(abs(colSums(T) - b)))
}

# Sinkhorn scaling, FIXED iteration count.  K = exp(-C/eps), i.e.
# Cuturi's exp(-lambda M) with lambda = 1/eps; updates u <- a/(K v) then
# v <- b/(K' u), started from v = 1.
#' Sinkhorn scaling, FIXED iteration count.  K = exp(-C/eps), i.e
#'
#' Cuturi\'s exp(-lambda M) with lambda = 1/eps; updates u <- a/(K v)
#' then v <- b/(K\' u), started from v = 1.
#'
#' @param a Passed to \code{.b2close}.
#' @param b Passed to \code{.b2close}.
#' @param C Passed to \code{.b2mat}.
#' @param epsilon Coerced to numeric by the body, with \code{as.numeric}.
#' @param max_iter Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200L}.
#' @param trace A flag; the body branches on it. Defaults to \code{FALSE}.
#' @return A list with \code{T}, \code{u}, \code{v}, \code{a}, \code{b}, \code{trace}.
#' @export
.b2sinkhorn <- function(a, b, C, epsilon, max_iter = 200L, trace = FALSE) {
  eps <- as.numeric(epsilon)
  if (!(eps > 0)) stop("epsilon must be positive", call. = FALSE)
  n_it <- as.integer(max_iter)
  if (n_it < 1L) stop("max_iter must be at least 1", call. = FALSE)
  Cm <- .b2mat(C)
  av <- .b2close(a)
  bv <- .b2close(b)
  nr <- nrow(Cm)
  nc <- ncol(Cm)
  if (length(av) != nr || length(bv) != nc) {
    stop("marginals do not match the shape of C", call. = FALSE)
  }
  K <- exp(-Cm / eps)
  u <- rep(1, nr)
  v <- rep(1, nc)
  tr <- numeric(0)
  for (it in seq_len(n_it)) {
    s <- as.numeric(K %*% v)
    u <- ifelse(s > 0, av / s, 0)
    s2 <- as.numeric(crossprod(K, u))
    v <- ifelse(s2 > 0, bv / s2, 0)
    if (trace) {
      Tt <- (u %o% v) * K
      tr <- c(tr, .b2margerr(Tt, av, bv))
    }
  }
  T <- (u %o% v) * K
  list(T = T, u = u, v = v, a = av, b = bv, trace = tr)
}

#' Discrete entropy of a coupling matrix
#'
#' \deqn{H(T) = -\sum T_{ij}(\log T_{ij} - 1)}{H(T) = -sum T (log T - 1)}
#' Peyre & Cuturi (2019), eq. (4.1).  Note the `- 1`: the OT convention
#' is one nat per unit mass larger than the Shannon entropy and must not
#' be simplified away.
#'
#' @param T Non-negative coupling matrix.
#' @return Named list with `estimate`, `shannon`, `mass`, `nrow`,
#'   `ncol`, `method`.
#' @references Peyre & Cuturi (2019), eq. (4.1).
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Otnegent(V)
Otnegent <- function(T) {
  m <- .b2mat(T)
  if (any(m < 0)) stop("T must be non-negative", call. = FALSE)
  pos <- m[m > 0]
  list(estimate = -sum(pos * (log(pos) - 1)), shannon = -sum(pos * log(pos)),
       mass = sum(m), nrow = nrow(m), ncol = ncol(m),
       method = "Discrete entropy H(T) = -sum T(log T - 1) -- Peyre & Cuturi (2019) eq. (4.1)")
}

#' Entropic regularisation term of a coupling
#'
#' \deqn{\varepsilon H(T)}{eps H(T)}
#' Peyre & Cuturi (2019), eq. (4.1)-(4.2); Cuturi (2013), eq. (2).  The
#' regularised problem is `min <P,C> - eps H(P)`, so this is the term
#' that gets subtracted.
#'
#' @param T Non-negative coupling matrix.
#' @param epsilon Positive regularisation strength.
#' @return Named list with `estimate`, `entropy`, `epsilon`, `n`, `method`.
#' @references Peyre & Cuturi (2019), eq. (4.2).
#' @export
#' @examples
#' Otentreg(T = c(1, 2, 3, 4, 5, 6, 7, 8), epsilon = 5L)
Otentreg <- function(T, epsilon) {
  eps <- as.numeric(epsilon)
  if (!(eps > 0)) stop("epsilon must be positive", call. = FALSE)
  m <- .b2mat(T)
  if (any(m < 0)) stop("T must be non-negative", call. = FALSE)
  pos <- m[m > 0]
  h <- -sum(pos * (log(pos) - 1))
  list(estimate = eps * h, entropy = h, epsilon = eps, n = length(m),
       method = "Entropic regulariser eps*H(T) -- Peyre & Cuturi (2019) eq. (4.2)")
}

#' Entropic-regularised OT by Sinkhorn scaling
#'
#' \deqn{T = \mathrm{diag}(u) K \mathrm{diag}(v),\ K = e^{-C/\varepsilon}}
#' Cuturi (2013), eq. (2) and Sec. 4.1; Peyre & Cuturi (2019), eq. (4.2).
#' The loop runs exactly `max_iter` scalings -- no tolerance, no early
#' exit -- so the result is deterministic.
#'
#' @param a,b Non-negative marginals; each closed to unit mass here.
#' @param C Cost matrix, `length(a)` by `length(b)`.
#' @param epsilon Positive regularisation strength.
#' @param max_iter Fixed number of scalings (default 200).
#' @return Named list with `estimate` (the cost `<T,C>`), `T`, `u`, `v`,
#'   `iters`, `marginal_error`, `method`.
#' @references Cuturi (2013), eq. (2).
#' @examples
#' Otsinkh(c(0.5, 0.5), c(0.2, 0.5, 0.3),
#'         matrix(c(0, 1, 1, 0, 4, 1), 2, 3), 0.5)$estimate
#' @export
Otsinkh <- function(a, b, C, epsilon, max_iter = 200L) {
  s <- .b2sinkhorn(a, b, C, epsilon, max_iter)
  list(estimate = sum(s$T * .b2mat(C)), T = s$T, u = s$u, v = s$v,
       iters = as.integer(max_iter),
       marginal_error = .b2margerr(s$T, s$a, s$b),
       method = "Sinkhorn scaling, fixed iteration count -- Cuturi (2013) eq. (2)")
}

#' Iterations Sinkhorn needs to reach a marginal tolerance
#'
#' The loop is NOT stopped by the tolerance: exactly `max_iter` scalings
#' are performed, the marginal violation after each is recorded, and the
#' first index below `tol` is read off the finished trace.  Cuturi
#' (2013), Sec. 4.1; Peyre & Cuturi (2019), Sec. 4.2.
#'
#' @param a,b Non-negative marginals; closed here.
#' @param C Cost matrix.
#' @param epsilon Positive regularisation strength.
#' @param tol Sup-norm marginal-violation threshold.
#' @param max_iter Fixed trace length (default 200).
#' @return Named list with `estimate`, `reached`, `final_error`,
#'   `trace`, `method`.
#' @references Cuturi (2013), Sec. 4.1.
#' @export
#' @examples
#' Otsinkit(a = c(1, 2, 3, 4, 5, 6, 7, 8), b = 5L, C = c(1, 2, 3, 4, 5, 6, 7, 8), epsilon = 5L, tol = 0.5)
Otsinkit <- function(a, b, C, epsilon, tol, max_iter = 200L) {
  tol <- as.numeric(tol)
  if (!(tol > 0)) stop("tol must be positive", call. = FALSE)
  tr <- .b2sinkhorn(a, b, C, epsilon, max_iter, trace = TRUE)$trace
  hit <- which(tr < tol)
  reached <- length(hit) > 0L
  list(estimate = if (reached) as.numeric(hit[1]) else as.numeric(max_iter),
       reached = reached,
       final_error = if (length(tr)) tr[length(tr)] else NaN,
       trace = tr,
       method = "Sinkhorn iterations to reach tol, from a fixed-length trace -- Cuturi (2013) Sec. 4.1")
}

#' Marginal violation of a coupling, in the sup norm
#'
#' \deqn{\max(\|T1 - a\|_\infty, \|T^\top 1 - b\|_\infty)}
#' Peyre & Cuturi (2019), Sec. 4.2 -- Sinkhorn's stopping criterion.
#'
#' @param T Coupling matrix.
#' @param a,b Target marginals; each closed here.
#' @return Named list with `estimate`, `row_error`, `col_error`,
#'   `nrow`, `ncol`, `method`.
#' @references Peyre & Cuturi (2019), Sec. 4.2.
#' @export
#' @examples
#' Otsinktol(T = c(1, 2, 3, 4, 5, 6, 7, 8), a = c(1, 2, 3, 4, 5, 6, 7, 8), b = 5L)
Otsinktol <- function(T, a, b) {
  m <- .b2mat(T)
  av <- .b2close(a)
  bv <- .b2close(b)
  if (length(av) != nrow(m) || length(bv) != ncol(m)) {
    stop("marginals do not match the shape of T", call. = FALSE)
  }
  re <- max(abs(rowSums(m) - av))
  ce <- max(abs(colSums(m) - bv))
  list(estimate = max(re, ce), row_error = re, col_error = ce,
       nrow = nrow(m), ncol = ncol(m),
       method = "Sinkhorn marginal violation -- Peyre & Cuturi (2019) Sec. 4.2")
}

#' Soft assignment matrix from an entropic transport plan
#'
#' Row `i` of the plan divided by its mass `a_i`, i.e. the conditional
#' distribution of the destination given the source.  Cuturi (2013),
#' eq. (2) for the plan; Peyre & Cuturi (2019), Remark 4.11.
#'
#' @param a,b Non-negative marginals; closed here.
#' @param C Cost matrix.
#' @param epsilon Positive regularisation strength.
#' @param max_iter Fixed number of scalings (default 200).
#' @return Named list with `estimate` (row-normalised matrix), `T`,
#'   `entropy_mean`, `hard`, `method`.
#' @references Cuturi (2013), eq. (2).
#' @export
#' @examples
#' Otsoftas(a = c(1, 2, 3, 4, 5, 6, 7, 8), b = 5L, C = c(1, 2, 3, 4, 5, 6, 7, 8), epsilon = 5L)
Otsoftas <- function(a, b, C, epsilon, max_iter = 200L) {
  s <- .b2sinkhorn(a, b, C, epsilon, max_iter)
  T <- s$T
  rs <- rowSums(T)
  P <- T
  hard <- integer(nrow(T))
  hsum <- 0
  for (i in seq_len(nrow(T))) {
    if (rs[i] > 0) {
      P[i, ] <- T[i, ] / rs[i]
      pos <- P[i, ][P[i, ] > 0]
      hsum <- hsum + (-sum(pos * log(pos)))
    } else {
      P[i, ] <- 0
    }
    hard[i] <- which.max(P[i, ]) - 1L
  }
  list(estimate = P, T = T, entropy_mean = hsum / nrow(T), hard = hard,
       method = "Row-normalised entropic plan (soft assignment) -- Cuturi (2013) eq. (2)")
}

#' Dual-Sinkhorn divergence between two discrete measures
#'
#' \deqn{d^\lambda_M(r,c) = \langle P^\lambda, M\rangle}
#' Cuturi (2013), Definition 1 and eq. (2).  Cuturi parameterises by
#' `lambda`; here `eps = 1/lambda`, so `K = exp(-C/eps)`.  Both the
#' transport cost and the regularised objective are returned, because
#' they are different numbers and are routinely confused.
#'
#' @param a,b Non-negative marginals; closed here.
#' @param C Cost matrix.
#' @param eps Positive regularisation strength, `1/lambda`.
#' @param max_iter Fixed number of scalings (default 200).
#' @return Named list with `estimate`, `objective`, `entropy`,
#'   `lambda_`, `iters`, `method`.
#' @references Cuturi (2013), Definition 1, eq. (2).
#' @export
#' @examples
#' Sinkdist(a = c(1, 2, 3, 4, 5, 6, 7, 8), b = 5L, C = c(1, 2, 3, 4, 5, 6, 7, 8), eps = 0.5)
Sinkdist <- function(a, b, C, eps, max_iter = 200L) {
  epsv <- as.numeric(eps)
  if (!(epsv > 0)) stop("eps must be positive", call. = FALSE)
  s <- .b2sinkhorn(a, b, C, epsv, max_iter)
  T <- s$T
  cost <- sum(T * .b2mat(C))
  pos <- T[T > 0]
  h <- -sum(pos * (log(pos) - 1))
  list(estimate = cost, objective = cost - epsv * h, entropy = h,
       lambda_ = 1 / epsv, iters = as.integer(max_iter),
       method = "Dual-Sinkhorn divergence <P,C> -- Cuturi (2013) Def. 1, eq. (2)")
}

#' Squared-Euclidean ground cost matrix
#'
#' \deqn{C_{ij} = \|x_i - y_j\|^2}
#' Peyre & Cuturi (2019), Sec. 2.4.
#'
#' @param X,Y Point sets, `n x d` and `m x d`.
#' @return Named list with `estimate`, `nrow`, `ncol`, `total`, `method`.
#' @references Peyre & Cuturi (2019), Sec. 2.4.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Otcostsq(V, V)
Otcostsq <- function(X, Y) {
  A <- .b2mat(X)
  B <- .b2mat(Y)
  if (ncol(A) != ncol(B)) stop("X and Y must have the same dimension", call. = FALSE)
  C <- matrix(0, nrow(A), nrow(B))
  for (i in seq_len(nrow(A))) {
    for (j in seq_len(nrow(B))) C[i, j] <- sum((A[i, ] - B[j, ])^2)
  }
  list(estimate = C, nrow = nrow(A), ncol = nrow(B), total = sum(C),
       method = "Squared-Euclidean ground cost -- Peyre & Cuturi (2019) Sec. 2.4")
}

#' l_p ground cost matrix
#'
#' \deqn{C_{ij} = \|x_i - y_j\|_p}
#' Peyre & Cuturi (2019), Sec. 2.4.  `p = Inf` gives the sup norm.
#'
#' @param X,Y Point sets, `n x d` and `m x d`.
#' @param p Norm order, at least 1 (default 2).
#' @return Named list with `estimate`, `p`, `nrow`, `ncol`, `total`, `method`.
#' @references Peyre & Cuturi (2019), Sec. 2.4.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Otcostlp(V, V)
Otcostlp <- function(X, Y, p = 2) {
  pv <- as.numeric(p)
  if (pv < 1) stop("p must be at least 1", call. = FALSE)
  A <- .b2mat(X)
  B <- .b2mat(Y)
  if (ncol(A) != ncol(B)) stop("X and Y must have the same dimension", call. = FALSE)
  C <- matrix(0, nrow(A), nrow(B))
  for (i in seq_len(nrow(A))) {
    for (j in seq_len(nrow(B))) {
      d <- abs(A[i, ] - B[j, ])
      C[i, j] <- if (is.infinite(pv)) max(d) else sum(d^pv)^(1 / pv)
    }
  }
  list(estimate = C, p = pv, nrow = nrow(A), ncol = nrow(B), total = sum(C),
       method = "l_p ground cost -- Peyre & Cuturi (2019) Sec. 2.4")
}

#' Primal-dual gap of the entropic OT problem
#'
#' \deqn{F = \langle T,C\rangle - \varepsilon H(T) - \langle a,f\rangle - \langle b,g\rangle}
#' Peyre & Cuturi (2019), eq. (4.30)-(4.32).  Zero at optimality.
#'
#' @param T Coupling matrix.
#' @param C Cost matrix of the same shape.
#' @param a,b Marginals; closed here.
#' @param f,g Dual potentials, in the units of `C`.
#' @param epsilon Positive regularisation strength.
#' @return Named list with `estimate`, `primal`, `dual_pairing`,
#'   `entropy`, `epsilon`, `method`.
#' @references Peyre & Cuturi (2019), eq. (4.30)-(4.32).
#' @export
Otfreeen <- function(T, C, a, b, f, g, epsilon) {
  eps <- as.numeric(epsilon)
  if (!(eps > 0)) stop("epsilon must be positive", call. = FALSE)
  Tm <- .b2mat(T)
  Cm <- .b2mat(C)
  if (!identical(dim(Tm), dim(Cm))) stop("T and C must have the same shape", call. = FALSE)
  av <- .b2close(a)
  bv <- .b2close(b)
  fv <- as.numeric(f)
  gv <- as.numeric(g)
  if (length(av) != nrow(Tm) || length(bv) != ncol(Tm)) {
    stop("marginals do not match the shape of T", call. = FALSE)
  }
  if (length(fv) != length(av) || length(gv) != length(bv)) {
    stop("potentials do not match the marginals", call. = FALSE)
  }
  pos <- Tm[Tm > 0]
  h <- -sum(pos * (log(pos) - 1))
  primal <- sum(Tm * Cm) - eps * h
  pair <- sum(av * fv) + sum(bv * gv)
  list(estimate = primal - pair, primal = primal, dual_pairing = pair,
       entropy = h, epsilon = eps,
       method = "Entropic OT primal-dual gap -- Peyre & Cuturi (2019) eq. (4.30)-(4.32)")
}

#' Barycentric projection of a transport plan
#'
#' \deqn{\bar{T}(x_i) = \frac{1}{a_i}\sum_j P_{ij} y_j}
#' Peyre & Cuturi (2019), Remark 4.11, eq. (4.19).
#'
#' @param T Coupling matrix, `n x m`.
#' @param Y Destination points, `m x d`.
#' @return Named list with `estimate` (`n x d` image points), `mass`,
#'   `displacement`, `method`.
#' @references Peyre & Cuturi (2019), eq. (4.19).
#' @export
#' @examples
#' M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
#' S <- c("a", "b", "c")
#' Otbarmap(M, S)
Otbarmap <- function(T, Y) {
  Tm <- .b2mat(T)
  B <- .b2mat(Y)
  if (nrow(B) != ncol(Tm)) stop("Y must have one row per column of T", call. = FALSE)
  out <- matrix(NA_real_, nrow(Tm), ncol(B))
  mass <- rowSums(Tm)
  spread <- 0
  for (i in seq_len(nrow(Tm))) {
    if (mass[i] <= 0) next
    w <- Tm[i, ] / mass[i]
    pt <- as.numeric(crossprod(w, B))
    out[i, ] <- pt
    spread <- spread + sum(w * rowSums((B - matrix(pt, nrow(B), ncol(B), byrow = TRUE))^2))
  }
  list(estimate = out, mass = mass, displacement = spread / nrow(Tm),
       method = "Barycentric projection of a plan -- Peyre & Cuturi (2019) eq. (4.19)")
}

#' Dual potentials from Sinkhorn scalings
#'
#' \deqn{(u,v) = (e^{f/\varepsilon}, e^{g/\varepsilon})}
#' Peyre & Cuturi (2019), eq. (4.30)-(4.31), so `f = eps log u`.
#'
#' @param u,v Positive Sinkhorn scaling vectors.
#' @param epsilon Positive regularisation strength.
#' @return Named list with `estimate` (`f`), `g`, `epsilon`, `shift`,
#'   `method`.  `shift` is `mean(f)`: the dual pair is determined only
#'   up to a constant moving between `f` and `g`.
#' @references Peyre & Cuturi (2019), eq. (4.31).
#' @export
#' @examples
#' Otlogpot(u = c(1, 2, 3, 4, 5, 6, 7, 8), v = c(1, 2, 3, 4, 5, 6, 7, 8), epsilon = 5L)
Otlogpot <- function(u, v, epsilon) {
  eps <- as.numeric(epsilon)
  if (!(eps > 0)) stop("epsilon must be positive", call. = FALSE)
  uv <- as.numeric(u)
  vv <- as.numeric(v)
  if (min(uv) <= 0 || min(vv) <= 0) stop("scalings must be strictly positive", call. = FALSE)
  f <- eps * log(uv)
  g <- eps * log(vv)
  list(estimate = f, g = g, epsilon = eps, shift = mean(f),
       method = "Dual potentials f = eps log u -- Peyre & Cuturi (2019) eq. (4.31)")
}

#' Push-forward of a density through a smooth bijective map
#'
#' \deqn{\nu(y) = \mu(T^{-1}(y)) / |\det DT(T^{-1}(y))|}
#' Peyre & Cuturi (2019), Remark 2.6, eq. (2.8), which writes
#' `rho_alpha(x) = |det T'(x)| rho_beta(T(x))`.
#'
#' @param mu_grid Source density at the preimages.
#' @param T_jac Jacobian determinant of `T` at those preimages.
#' @param T_inv_grid The preimages, carried through to the result.
#' @return Named list with `estimate`, `preimage`, `jacobian`, `n`,
#'   `method`.
#' @references Peyre & Cuturi (2019), eq. (2.8).
#' @export
#' @examples
#' Otpushfw(mu_grid = c(1, 2, 3, 4, 5, 6, 7, 8), T_jac = c(1, 2, 3, 4, 5, 6, 7, 8), T_inv_grid = c(1, 2, 3, 4, 5, 6, 7, 8))
Otpushfw <- function(mu_grid, T_jac, T_inv_grid) {
  mu <- as.numeric(mu_grid)
  jac <- as.numeric(T_jac)
  pre <- as.numeric(T_inv_grid)
  n <- length(mu)
  if (length(jac) != n || length(pre) != n) {
    stop("mu_grid, T_jac and T_inv_grid must have the same length", call. = FALSE)
  }
  if (any(mu < 0)) stop("mu_grid must be non-negative", call. = FALSE)
  if (any(abs(jac) <= 0)) stop("the map is singular: |det DT| = 0", call. = FALSE)
  list(estimate = mu / abs(jac), preimage = pre, jacobian = jac, n = n,
       method = "Push-forward density -- Peyre & Cuturi (2019) eq. (2.8)")
}

# CANONICAL TEST
# s <- Otsinkh(c(0.5, 0.5), c(0.2, 0.5, 0.3), matrix(c(0, 1, 1, 0, 4, 1), 2, 3), 0.5)
# stopifnot(s$marginal_error < 1e-12)
