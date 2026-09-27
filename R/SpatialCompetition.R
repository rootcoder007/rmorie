.pme_win_index <- function(x1, x2, mu, sd) {
  d <- x2 - x1
  m <- (x1 + x2) / 2
  r <- sqrt(sum(d * d))
  num <- sum((mu - m) * d)
  list(z = num / (sd * r),
       g1 = (x1 - mu) / (sd * r) + num * d / (sd * r^3),
       g2 = (mu - x2) / (sd * r) - num * d / (sd * r^3))
}

.pme_loss <- function(y, a) -sum((y - a)^2)

.pme_utilities <- function(x1, x2, a1, a2, mu, sd) {
  p2 <- stats::pnorm(.pme_win_index(x1, x2, mu, sd)$z)
  c((1 - p2) * .pme_loss(x1, a1) + p2 * .pme_loss(x2, a1),
    p2 * .pme_loss(x2, a2) + (1 - p2) * .pme_loss(x1, a2), p2)
}

.pme_grads <- function(x1, x2, a1, a2, mu, sd) {
  w <- .pme_win_index(x1, x2, mu, sd)
  p <- stats::pnorm(w$z)
  f <- stats::dnorm(w$z)
  d1 <- f * (.pme_loss(x1, a1) - .pme_loss(x2, a1))
  d2 <- f * (.pme_loss(x2, a2) - .pme_loss(x1, a2))
  list(-d1 * w$g1 - 2 * (1 - p) * (x1 - a1), d2 * w$g2 - 2 * p * (x2 - a2))
}

#' Equilibrium of two policy-motivated candidates under electoral uncertainty
#'
#' Candidates k = 1, 2 have quadratic losses \eqn{u_k(y) = -\|y - a_k\|^2}
#' and care only about the policy that wins (Wittman 1983). The decisive
#' voter sits at \eqn{m \sim N(c, s^2 I)} and elects the nearer platform, so
#' candidate 2 wins with probability \eqn{\Phi(((c - (x_1 + x_2)/2)^\top (x_2
#' - x_1)) / (s \|x_2 - x_1\|))} (Calvert 1985). Candidate k maximises
#' \eqn{P_k u_k(x_k) + (1 - P_k) u_k(x_{other})}. The equilibrium is found by
#' alternating best responses (BFGS on the analytic gradients) from
#' \code{start} (default: each ideal point pulled towards the centre, \eqn{c +
#' t (a_k - c)} with \eqn{t = s / (s + \|a_k - c\|)}), then polished by Newton steps
#' on the joint first-order conditions. In one dimension with ideal points
#' \eqn{\pm a} and \eqn{c = 0} the platforms are \eqn{\pm a / (1 + 2 a
#' \phi(0) / s)}: divergent, and converging to the centre as \eqn{s} shrinks.
#'
#' @param ideal_1,ideal_2 Candidate ideal points (vectors of length d).
#' @param center Mean \eqn{c} of the decisive voter's position.
#' @param sd Standard deviation \eqn{s} of each coordinate of that position.
#' @param start Optional list of starting platforms.
#' @param tol Stop when no platform moves more than \code{tol}.
#' @param max_iter Maximum best-response rounds.
#' @return List with \code{x1}, \code{x2}, \code{win_prob_2},
#'   \code{utilities}, \code{gradients}, \code{rounds}, \code{converged}.
#' @references Wittman, D. (1983). Candidate motivation: a synthesis of
#'   alternative theories. American Political Science Review 77, 142-157.
#'
#'   Calvert, R. L. (1985). Robustness of the multidimensional voting model:
#'   candidate motivations, uncertainty, and convergence. American Journal of
#'   Political Science 29, 69-95.
#' @examples
#' PolicyMotivatedEquilibrium(-1, 1, 0, 0.5)[c("x1", "x2")]
#' @export
PolicyMotivatedEquilibrium <- function(ideal_1, ideal_2, center, sd, start = NULL, tol = 1e-10, max_iter = 500L) {
  a1 <- as.numeric(ideal_1)
  a2 <- as.numeric(ideal_2)
  mu <- as.numeric(center)
  if (sd <= 0) stop("sd must be positive", call. = FALSE)
  shrink <- function(a) mu + sd / (sd + sqrt(sum((a - mu)^2))) * (a - mu)
  x1 <- if (is.null(start)) shrink(a1) else as.numeric(start[[1]])
  x2 <- if (is.null(start)) shrink(a2) else as.numeric(start[[2]])
  rounds <- 0L
  converged <- FALSE
  while (rounds < max_iter) {
    rounds <- rounds + 1L
    b2 <- BfgsMinimize(function(y) -.pme_utilities(x1, y, a1, a2, mu, sd)[2], x2,
                       grad = function(y) -.pme_grads(x1, y, a1, a2, mu, sd)[[2]], gtol = 1e-12)$x
    b1 <- BfgsMinimize(function(y) -.pme_utilities(y, b2, a1, a2, mu, sd)[1], x1,
                       grad = function(y) -.pme_grads(y, b2, a1, a2, mu, sd)[[1]], gtol = 1e-12)$x
    move <- max(abs(c(b1, b2) - c(x1, x2)))
    x1 <- b1
    x2 <- b2
    if (move <= tol) {
      converged <- TRUE
      break
    }
  }
  d <- length(a1)
  foc <- function(w) unlist(.pme_grads(w[seq_len(d)], w[d + seq_len(d)], a1, a2, mu, sd))
  w <- c(x1, x2)
  for (k in 1:20) {
    F <- foc(w)
    if (max(abs(F)) <= 1e-13) break
    J <- matrix(0, 2 * d, 2 * d)
    for (cc in seq_len(2 * d)) {
      e <- numeric(2 * d)
      e[cc] <- 1e-6
      J[, cc] <- (foc(w + e) - foc(w - e)) / 2e-6
    }
    w <- w + solve(J, -F)
  }
  x1 <- w[seq_len(d)]
  x2 <- w[d + seq_len(d)]
  u <- .pme_utilities(x1, x2, a1, a2, mu, sd)
  list(x1 = x1, x2 = x2, win_prob_2 = u[3], utilities = u[1:2],
       gradients = .pme_grads(x1, x2, a1, a2, mu, sd), rounds = rounds, converged = converged)
}

.plc_shares <- function(v, w, pos) {
  out <- numeric(length(pos))
  for (i in seq_along(v)) {
    d <- abs(v[i] - pos)
    near <- which(d - min(d) <= 1e-12 * max(1, abs(v[i])))
    out[near] <- out[near] + w[i] / length(near)
  }
  out / sum(w)
}

.plc_median_interval <- function(v, w) {
  o <- order(v)
  v <- v[o]
  w <- w[o]
  total <- sum(w)
  acc <- 0
  lo <- NA
  hi <- NA
  for (i in seq_along(v)) {
    before <- acc
    acc <- acc + w[i]
    if (is.na(lo) && acc >= total / 2 - 1e-12 * total) lo <- v[i]
    if (is.na(hi) && before >= total / 2 - 1e-12 * total && before > 0) hi <- v[i]
    if (!is.na(lo) && acc > total / 2 + 1e-12 * total) {
      if (is.na(hi)) hi <- v[i]
      break
    }
  }
  c(lo, if (is.na(hi)) lo else hi)
}

#' Hotelling-Downs plurality competition on a line
#'
#' Each voter supports the nearest candidate (ties split equally);
#' candidates maximise their vote share (Hotelling 1929, Downs 1957). With a
#' finite electorate a candidate's share, as its position y varies, changes
#' only where it becomes equidistant with a rival for some voter
#' (\eqn{y = c_k} or \eqn{y = 2v - c_k}); the best response is found exactly
#' by evaluating those breakpoints and the midpoints between them. The
#' configuration is a Nash equilibrium when no candidate can raise its
#' share. Without \code{positions} the two-candidate equilibrium is
#' returned: both candidates at the median.
#'
#' @param voters Voter ideal points.
#' @param positions Candidate positions; \code{NULL} for the two-candidate
#'   Downsian equilibrium.
#' @param weights Voter weights (default 1).
#' @return List with \code{shares}, \code{positions}, \code{best_response}
#'   (location and share per candidate), \code{gain}, \code{is_equilibrium}
#'   and \code{median_interval}.
#' @references Hotelling, H. (1929). Stability in competition. Economic
#'   Journal 39, 41-57.
#'
#'   Eaton, B. C. and Lipsey, R. G. (1975). The principle of minimum
#'   differentiation reconsidered. Review of Economic Studies 42, 27-49.
#' @examples
#' PluralityCompetition(c(0, 1, 2, 7, 9), c(1, 2))$gain
#' @export
PluralityCompetition <- function(voters, positions = NULL, weights = NULL) {
  v <- as.numeric(voters)
  w <- if (is.null(weights)) rep(1, length(v)) else as.numeric(weights)
  med <- .plc_median_interval(v, w)
  pos <- if (is.null(positions)) rep(med[1], 2) else as.numeric(positions)
  shares <- .plc_shares(v, w, pos)
  best <- matrix(0, length(pos), 2)
  gain <- numeric(length(pos))
  for (k in seq_along(pos)) {
    others <- pos[-k]
    pts <- sort(unique(c(others, as.vector(outer(2 * v, others, "-")))))
    cand <- c(pts, (pts[-1] + pts[-length(pts)]) / 2, pts[1] - 1, pts[length(pts)] + 1)
    top_y <- pos[k]
    top_s <- shares[k]
    for (y in cand) {
      p <- pos
      p[k] <- y
      s <- .plc_shares(v, w, p)[k]
      if (s > top_s + 1e-12) {
        top_y <- y
        top_s <- s
      }
    }
    best[k, ] <- c(top_y, top_s)
    gain[k] <- top_s - shares[k]
  }
  list(shares = shares, positions = pos, best_response = best, gain = gain,
       is_equilibrium = all(gain <= 1e-12), median_interval = med)
}

.lgc_probs <- function(X, Z, lam, beta) {
  U <- matrix(lam, nrow(X), nrow(Z), byrow = TRUE)
  for (j in seq_len(nrow(Z))) U[, j] <- U[, j] - beta * rowSums((X - matrix(Z[j, ], nrow(X), ncol(X), byrow = TRUE))^2)
  E <- exp(U - apply(U, 1, max))
  E / rowSums(E)
}

.lgc_share_grad <- function(X, Z, lam, beta, j) {
  P <- .lgc_probs(X, Z, lam, beta)
  wts <- 2 * beta * P[, j] * (1 - P[, j])
  list(share = mean(P[, j]),
       grad = colSums(wts * (X - matrix(Z[j, ], nrow(X), ncol(X), byrow = TRUE))) / nrow(X))
}

#' Multiparty competition under logit probabilistic voting
#'
#' Voter i at \eqn{x_i} picks party j at \eqn{z_j} with probability
#' proportional to \eqn{\exp(\lambda_j - \beta \|x_i - z_j\|^2)} (valences
#' \eqn{\lambda}); party j maximises its expected share. The equilibrium is
#' found by alternating best responses (BFGS on the analytic gradients) from
#' \code{start} (default: the parties spread on a circle of radius
#' \eqn{\sqrt{tr V}/2} about the electoral mean), polished by Newton steps
#' on the joint first-order conditions. Schofield (2007): with electoral
#' covariance V (divisor n) and \eqn{\rho_j} the share of party j when all
#' sit at the mean, the Hessian of party j's share there is \eqn{2\beta
#' \rho_j (1 - \rho_j) C_j} with \eqn{C_j = 2\beta(1 - 2\rho_j) V - I}; the
#' mean is a local Nash equilibrium iff every \eqn{C_j} is negative definite.
#'
#' @param voters Voter ideal points (n x d matrix).
#' @param valence Party valences.
#' @param beta Spatial salience.
#' @param start Optional starting positions (p x d).
#' @param tol Stop when no party moves more than \code{tol}.
#' @param max_iter Maximum best-response rounds.
#' @return List with \code{positions}, \code{shares}, \code{gradients},
#'   \code{hessian_eigenvalues}, \code{is_local_nash},
#'   \code{characteristic_matrices}, \code{mean_shares},
#'   \code{convergence_coefficient}, \code{rounds}, \code{converged}.
#' @references Schofield, N. (2007). The mean voter theorem: necessary and
#'   sufficient conditions for convergent equilibrium. Review of Economic
#'   Studies 74, 965-980.
#' @examples
#' X <- rbind(c(-1, 0), c(1, 0), c(0, 2), c(0, -2))
#' LogitCompetition(X, c(0, 0, 0), 0.1)$is_local_nash
#' @export
LogitCompetition <- function(voters, valence, beta, start = NULL, tol = 1e-10, max_iter = 500L) {
  X <- as.matrix(voters)
  lam <- as.numeric(valence)
  n <- nrow(X)
  d <- ncol(X)
  p <- length(lam)
  mu <- colMeans(X)
  Xc <- X - matrix(mu, n, d, byrow = TRUE)
  V <- crossprod(Xc) / n
  rho <- exp(lam - max(lam)) / sum(exp(lam - max(lam)))
  C <- lapply(rho, function(r) 2 * beta * (1 - 2 * r) * V - diag(d))
  if (is.null(start)) {
    rad <- 0.5 * sqrt(sum(diag(V)))
    Z <- t(vapply(seq_len(p) - 1L, function(j) {
      th <- 2 * pi * j / p
      off <- if (d > 1) c(cos(th), sin(th), rep(0, d - 2)) else if (j %% 2 == 0) 1 else -1
      mu + rad * off
    }, numeric(d)))
    Z <- matrix(Z, p, d)
  } else {
    Z <- matrix(as.numeric(unlist(start)), p, d, byrow = is.list(start))
  }
  rounds <- 0L
  converged <- FALSE
  while (rounds < max_iter) {
    rounds <- rounds + 1L
    move <- 0
    for (j in seq_len(p)) {
      f <- function(z) {
        W <- Z
        W[j, ] <- z
        -.lgc_share_grad(X, W, lam, beta, j)$share
      }
      g <- function(z) {
        W <- Z
        W[j, ] <- z
        -.lgc_share_grad(X, W, lam, beta, j)$grad
      }
      b <- BfgsMinimize(f, Z[j, ], grad = g, gtol = 1e-12)$x
      move <- max(move, abs(b - Z[j, ]))
      Z[j, ] <- b
    }
    if (move <= tol) {
      converged <- TRUE
      break
    }
  }
  foc <- function(w) {
    W <- matrix(w, p, d, byrow = TRUE)
    unlist(lapply(seq_len(p), function(j) .lgc_share_grad(X, W, lam, beta, j)$grad))
  }
  w <- as.vector(t(Z))
  for (k in 1:20) {
    F <- foc(w)
    if (max(abs(F)) <= 1e-14) break
    J <- matrix(0, p * d, p * d)
    for (cc in seq_len(p * d)) {
      e <- numeric(p * d)
      e[cc] <- 1e-6
      J[, cc] <- (foc(w + e) - foc(w - e)) / 2e-6
    }
    w <- w + solve(J, -F)
  }
  Z <- matrix(w, p, d, byrow = TRUE)
  shares <- numeric(p)
  grads <- vector("list", p)
  eig <- vector("list", p)
  for (j in seq_len(p)) {
    sg <- .lgc_share_grad(X, Z, lam, beta, j)
    shares[j] <- sg$share
    grads[[j]] <- sg$grad
    H <- matrix(0, d, d)
    for (cc in seq_len(d)) {
      zp <- Z
      zm <- Z
      zp[j, cc] <- zp[j, cc] + 1e-5
      zm[j, cc] <- zm[j, cc] - 1e-5
      H[, cc] <- (.lgc_share_grad(X, zp, lam, beta, j)$grad - .lgc_share_grad(X, zm, lam, beta, j)$grad) / 2e-5
    }
    eig[[j]] <- eigen((H + t(H)) / 2, symmetric = TRUE, only.values = TRUE)$values
  }
  list(positions = Z, shares = shares, gradients = grads, hessian_eigenvalues = eig,
       is_local_nash = all(vapply(eig, max, 0) < 0) && max(abs(unlist(grads))) < 1e-8,
       characteristic_matrices = C, mean_shares = rho,
       convergence_coefficient = 2 * beta * (1 - 2 * min(rho)) * sum(diag(V)),
       rounds = rounds, converged = converged)
}
