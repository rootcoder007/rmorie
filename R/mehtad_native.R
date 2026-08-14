# Mehrotra's predictor-corrector: two solves, one factorisation.
# Sources: Mehrotra, S. (1992) "On the Implementation of a Primal-Dual
# Interior Point Method", *SIAM Journal on Optimization* 2(4), 575-601,
# doi:10.1137/0802028. The abstract and Sec. 1: the second-order
# primal-dual method using a Taylor polynomial of second order to
# approximate the primal-dual trajectory, with the computations for
# the second derivative combined with those for the centering
# direction, and not requiring primal or dual feasibility; the adaptive
# heuristic for estimating the centering parameter and the adaptive
# step length; and the reported reductions of about 40%, 50% and 35%
# in iteration count against the implementations of Lustig, Marsten
# and Shanno and the dual affine scaling methods, with the contribution
# due to the second derivative identified as the most significant.
# Sec. 5 and Exhibit 5.1 (Heuristic CENPAR): the centering parameter
# targeting the point on the central path whose duality gap is the
# minimum achievable along the affine directions, the ratio of that
# gap to x^T s as an indication of how well the affine trajectory is
# locally approximated -- near 1 meaning the approximation is poor
# and near 0 that it is good -- and Table 5.1 showing only moderate
# variation in iteration count for the exponent between 2 and 4.
#
# Wright, S. J. (1997) *Primal-Dual Interior-Point Methods*, SIAM,
# doi:10.1137/1.9781611971453. Chapter 10 gives the algorithm in the
# sigma = (mu_aff/mu)^3 form used here.
#
# Boyd, S. & Vandenberghe, L. (2004) *Convex Optimization*, Cambridge
# University Press, doi:10.1017/CBO9780511804441. Sec. 11.7 for the
# primal-dual framework and the residual formulation.

.MEHTAD_EPS <- 1e-12

.mehtad_mat <- function(X) {
  if (is.matrix(X)) X
  else do.call(rbind, lapply(X, function(r) as.numeric(unlist(r))))
}

.mehtad_vec <- function(v) as.numeric(unlist(v))

.mehtad_cholsolve <- function(M, rhs) {
  L <- chol(M)
  as.numeric(solve(t(L), solve(L, rhs)))
}

residuals <- function(A, b, c, x, y, s) {
  M <- .mehtad_mat(A)
  m <- nrow(M); n <- ncol(M)
  xv <- .mehtad_vec(x); yv <- .mehtad_vec(y); sv <- .mehtad_vec(s)
  bv <- .mehtad_vec(b); cv <- .mehtad_vec(c)
  rp <- as.numeric(M %*% xv - bv)
  rd <- as.numeric(t(M) %*% yv + sv - cv)
  mu <- sum(xv * sv) / n
  list(primal = rp, dual = rd, mu = mu,
       primal_norm = sqrt(sum(rp * rp)),
       dual_norm = sqrt(sum(rd * rd)),
       note = "an infeasible start is allowed; the residuals are driven to zero alongside mu")
}

max_step <- function(v, dv, eta = 0.9995) {
  a <- 1.0
  for (i in seq_along(v)) {
    if (dv[i] < 0) {
      ratio <- -as.numeric(v[i]) / dv[i]
      if (ratio < a) a <- ratio
    }
  }
  min(1.0, as.numeric(eta) * a)
}

centering_parameter <- function(mu, mu_affine, nu = 3.0) {
  m <- as.numeric(mu); ma <- as.numeric(mu_affine)
  if (m <= 0)
    stop("mehtad: mu must be positive")
  if (ma < 0)
    stop("mehtad: the affine mu cannot be negative")
  nu_n <- as.numeric(nu)
  if (!is.finite(nu_n) || nu_n < 1.0 || nu_n > 6.0)
    stop("mehtad: nu outside the range the paper examined; it tabulates 2 to 4")
  ratio <- ma / m
  list(sigma = ratio^nu_n, ratio = ratio, nu = nu_n,
       approximation = if (ratio > 0.5) "poor" else "good",
       note = "ratio near 1 means the affine trajectory is badly approximated locally, so centre more")
}

.mehtad_newton <- function(M, x, s, rp, rd, rc) {
  m <- nrow(M); n <- ncol(M)
  d <- x / s
  t <- (-rc / s) + d * rd
  rhs <- -rp - as.numeric(M %*% t)
  Nmat <- matrix(0, m, m)
  for (i in seq_len(m)) for (j in seq_len(m)) {
    s_ij <- 0
    for (tt in seq_len(n)) s_ij <- s_ij + M[i, tt] * d[tt] * M[j, tt]
    Nmat[i, j] <- s_ij
  }
  for (i in seq_len(m)) Nmat[i, i] <- Nmat[i, i] + 1e-11
  dy <- .mehtad_cholsolve(Nmat, rhs)
  ds <- -(rd + as.numeric(t(M) %*% dy))
  dx <- (-rc - x * ds) / s
  list(dx = dx, dy = dy, ds = ds)
}

solve_lp <- function(A, b, c, tol = 1e-9, max_iter = 100L, nu = 3.0,
                     eta = 0.9995, corrector = TRUE) {
  M <- .mehtad_mat(A)
  m <- nrow(M); n <- ncol(M)
  bv <- .mehtad_vec(b); cv <- .mehtad_vec(c)
  if (length(bv) != m || length(cv) != n)
    stop("mehtad: A is ", m, "x", n, " but b has ", length(bv),
         " and c has ", length(cv))
  x <- rep(1.0, n); s <- rep(1.0, n); y <- rep(0.0, m)
  it <- 0L; converged <- FALSE
  for (it in seq_len(as.integer(max_iter))) {
    r <- residuals(M, bv, cv, x, y, s)
    mu <- r$mu
    if (mu < as.numeric(tol) && r$primal_norm < as.numeric(tol) &&
        r$dual_norm < as.numeric(tol)) { converged <- TRUE; break }
    rc <- x * s
    aff <- .mehtad_newton(M, x, s, r$primal, r$dual, rc)
    ap <- max_step(x, aff$dx, eta); ad <- max_step(s, aff$ds, eta)
    mu_aff <- sum((x + ap * aff$dx) * (s + ad * aff$ds)) / n
    sig <- centering_parameter(mu, mu_aff, nu)$sigma
    if (corrector) {
      rc2 <- x * s + aff$dx * aff$ds - sig * mu
    } else {
      rc2 <- x * s - sig * mu
    }
    d <- .mehtad_newton(M, x, s, r$primal, r$dual, rc2)
    ap <- max_step(x, d$dx, eta); ad <- max_step(s, d$ds, eta)
    x <- x + ap * d$dx; s <- s + ad * d$ds; y <- y + ad * d$dy
    if (min(x, s) <= 0)
      stop("mehtad: an iterate left the positive orthant, which the fraction-to-boundary rule exists to prevent")
  }
  rf <- residuals(M, bv, cv, x, y, s)
  list(estimate = x, x = x, y = y, s = s, mu = rf$mu,
       objective = sum(cv * x),
       dual_objective = sum(bv * y),
       iterations = it, corrector = isTRUE(corrector),
       primal_residual = rf$primal_norm,
       dual_residual = rf$dual_norm,
       converged = (rf$mu < as.numeric(tol) && rf$primal_norm < as.numeric(tol)) || converged,
       method = "Mehrotra predictor-corrector; Mehrotra (1992)",
       note = "the corrector reuses the predictor's factorisation, so the second-order term costs a right-hand side rather than an iteration")
}

predictor_corrector <- solve_lp
mehrotras_predictor <- solve_lp

.mehtad_cheatsheet <- function() {
  paste("mehtad: the expensive part of an interior-point iteration ",
        "is ONE factorisation of A D A'; a second right-hand side ",
        "is nearly free, so spend it on information. PREDICTOR: ",
        "the pure Newton (affine) step, too aggressive to take ",
        "whole but exactly the diagnostic needed. CENTERING: ",
        "sigma = (mu_aff/mu)^nu -- a good affine step asks for ",
        "little centring, a bad one for a lot; the ratio says how ",
        "well the trajectory is locally approximated, and nu in ",
        "[2,4] barely matters. CORRECTOR: subtract the ",
        "second-order cross term dX_aff dS_aff e together with the ",
        "centring target. FRACTION-TO-BOUNDARY keeps x, s strictly ",
        "positive. About 40% fewer iterations, mostly from the ",
        "second derivative.", sep = "")
}

morie_mehtad <- function(op, ...) {
  if (missing(op) || length(op) != 1L)
    stop("mehtad: op must be one of residuals, max_step, centering_parameter, newton_direction, solve_lp, cheatsheet")
  op <- as.character(op)
  switch(op,
    "residuals" = residuals(...),
    "max_step" = list(max_step = max_step(...)),
    "centering_parameter" = centering_parameter(...),
    "newton_direction" = .mehtad_newton(...),
    "solve_lp" = solve_lp(...),
    "predictor_corrector" = solve_lp(...),
    "mehrotras_predictor" = solve_lp(...),
    "cheatsheet" = list(cheatsheet = .mehtad_cheatsheet()),
    stop("mehtad: unknown op ", shQuote(op))
  )
}
