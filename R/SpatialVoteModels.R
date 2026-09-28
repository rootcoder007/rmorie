.svm_pt <- function(v, d) if (is.null(v)) numeric(d) else as.numeric(v)

.svm_alts <- function(x, status_quo) {
  X <- if (is.matrix(x)) x else matrix(as.numeric(x), nrow = 1)
  if (!is.null(status_quo) || nrow(X) == 1) X <- rbind(X, .svm_pt(status_quo, ncol(X)))
  X
}

.svm_binary <- function(x, ideal_point, status_quo, beta, error) {
  x <- as.numeric(x)
  v <- .svm_pt(ideal_point, length(x))
  s <- .svm_pt(status_quo, length(x))
  du <- beta * (sum((v - s)^2) - sum((v - x)^2))
  list(value = .sv_cdf(du, error), utility_difference = du)
}

#' Spatial voting models for one voter
#'
#' Binary choice of \code{x} over \code{status_quo} by a voter at
#' \code{ideal_point} (both default to the origin) with quadratic utility
#' \eqn{-\beta\|v - z\|^2} and utility-difference noise: \code{LogitVote},
#' \code{ProbitVote}, \code{CauchyVote} give \eqn{F(U(x) - U(s))}.
#' \code{NormalVote} is the NOMINATE Gaussian-utility probit
#' \eqn{\Phi(\beta e^{-w^2 d_x^2/2} - \beta e^{-w^2 d_s^2/2})} (Poole and
#' Rosenthal 1997). \code{BoltzmannVote} gives softmax probabilities
#' \eqn{e^{-d_j^2/T}/\sum_k e^{-d_k^2/T}} over alternatives (rows of \code{x};
#' \code{status_quo} appended when given or when \code{x} is one point).
#' \code{MixedLogitVote} averages the logit probability over
#' \eqn{\beta_r \sim N(\beta, \sigma^2)} Philox draws (Train 2009).
#' \code{ProximityVote} is 1, 0.5 or 0 as \code{x} is nearer, equidistant or
#' farther than \code{status_quo} in the Minkowski \code{p}-norm.
#' \code{QuadUtility} is \eqn{-(x - v)^\top A (x - v)}. \code{MedianVoter2d}
#' is the Plott total-median test: the exact Tukey depth of a point among the
#' ideal points (rows of \code{x}) and whether it is at least \eqn{n/2}.
#' \code{UtilityMax} picks the alternative with the largest quadratic, linear
#' or Gaussian utility (1-based index). \code{VoteTrading} and
#' \code{VoteTrade2d} report the number of Riker-Brams trades
#' (\code{\link{VoteTradingRikerBrams}}); \code{WeightedVote} returns one
#' power index of \code{\link{PowerIndices}}. \code{DiscountUtility} is
#' Grofman's discounting utility \eqn{-\beta\|v - s - \delta(x - s)\|^2};
#' \code{MixedUtility} the Merrill-Grofman unified utility
#' \eqn{2(1-\beta)(v-n)\cdot(x-n) - \beta\|v - x\|^2}. Identical to the
#' Python modules \code{morie.fn.svlgv}, \code{svprv}, \code{svchy},
#' \code{svnrm}, \code{svblt}, \code{svmxl}, \code{svpxv}, \code{svqud},
#' \code{svmv2}, \code{svutm}, \code{svvtr}, \code{svvt2}, \code{svwvt},
#' \code{svdsc} and \code{svmxu}.
#'
#' @param x Alternative (vector), alternatives (matrix rows), or ideal points
#'   (\code{MedianVoter2d}).
#' @param ideal_point Voter ideal point (for \code{MedianVoter2d}, the point
#'   tested).
#' @param status_quo Status quo (default origin).
#' @param beta Utility scale, or proximity weight for \code{MixedUtility}.
#' @param w NOMINATE dimension weight.
#' @param temperature Softmax temperature.
#' @param beta_sd Standard deviation of the random coefficient.
#' @param n_draws Simulation draws.
#' @param seed Philox seed.
#' @param p Minkowski exponent.
#' @param salience Vector (diagonal) or matrix of issue saliences.
#' @param utility \code{"quadratic"}, \code{"linear"} or \code{"gaussian"}.
#' @param scale Gaussian utility scale.
#' @param valuations Voters x issues signed intensities.
#' @param weights Player weights.
#' @param quota Winning quota.
#' @param index Power index name.
#' @param discount Fraction of the promised move expected to be enacted.
#' @param neutral Neutral point for the directional component.
#' @return List with \code{value} and model-specific components.
#' @references Enelow, J. M. and Hinich, M. J. (1984). The Spatial Theory of
#'   Voting. Cambridge University Press.
#'
#'   Poole, K. T. and Rosenthal, H. (1997). Congress: A Political-Economic
#'   History of Roll Call Voting. Oxford University Press.
#'
#'   Plott, C. R. (1967). A notion of equilibrium and its possibility under
#'   majority rule. American Economic Review 57, 787-806.
#'
#'   Grofman, B. (1985). The neglected role of the status quo in models of
#'   issue voting. Journal of Politics 47, 230-237.
#'
#'   Merrill, S. and Grofman, B. (1999). A Unified Theory of Voting.
#'   Cambridge University Press.
#' @examples
#' LogitVote(1, ideal_point = 0, status_quo = 2)$value
#' MedianVoter2d(rbind(c(0, 0), c(1, 1), c(2, 2), c(0, 2), c(2, 0)))$is_core
#' MixedUtility(2, ideal_point = 1)$value
#' @export
LogitVote <- function(x, ideal_point = NULL, status_quo = NULL, beta = 1) {
  .svm_binary(x, ideal_point, status_quo, beta, "logit")
}

#' @rdname LogitVote
#' @export
ProbitVote <- function(x, ideal_point = NULL, status_quo = NULL, beta = 1) {
  .svm_binary(x, ideal_point, status_quo, beta, "probit")
}

#' @rdname LogitVote
#' @export
CauchyVote <- function(x, ideal_point = NULL, status_quo = NULL, beta = 1) {
  .svm_binary(x, ideal_point, status_quo, beta, "cauchy")
}

#' @rdname LogitVote
#' @export
NormalVote <- function(x, ideal_point = NULL, status_quo = NULL, beta = 1, w = 1) {
  x <- as.numeric(x)
  v <- .svm_pt(ideal_point, length(x))
  s <- .svm_pt(status_quo, length(x))
  ux <- beta * exp(-w^2 * sum((v - x)^2) / 2)
  us <- beta * exp(-w^2 * sum((v - s)^2) / 2)
  list(value = stats::pnorm(ux - us), utility_yea = ux, utility_nay = us)
}

#' @rdname LogitVote
#' @export
BoltzmannVote <- function(x, ideal_point = NULL, status_quo = NULL, temperature = 1) {
  X <- .svm_alts(x, status_quo)
  v <- .svm_pt(ideal_point, ncol(X))
  U <- -rowSums(sweep(X, 2, v)^2) / temperature
  e <- exp(U - max(U))
  P <- e / sum(e)
  list(value = P[1], probabilities = P, utilities = U)
}

#' @rdname LogitVote
#' @export
MixedLogitVote <- function(x, ideal_point = NULL, status_quo = NULL, beta = 1, beta_sd = 0.5, n_draws = 1000L,
                           seed = 1L) {
  x <- as.numeric(x)
  v <- .svm_pt(ideal_point, length(x))
  s <- .svm_pt(status_quo, length(x))
  dd <- sum((v - s)^2) - sum((v - x)^2)
  p <- stats::plogis((beta + beta_sd * .morie_random_normal(n_draws, seed = seed, stream = 0)) * dd)
  list(value = mean(p), simulation_se = stats::sd(p) / sqrt(n_draws), n_draws = n_draws)
}

#' @rdname LogitVote
#' @export
ProximityVote <- function(x, ideal_point = NULL, status_quo = NULL, p = 2) {
  x <- as.numeric(x)
  v <- .svm_pt(ideal_point, length(x))
  s <- .svm_pt(status_quo, length(x))
  dx <- sum(abs(v - x)^p)^(1 / p)
  ds <- sum(abs(v - s)^p)^(1 / p)
  list(value = if (dx < ds) 1 else if (dx == ds) 0.5 else 0, distance_x = dx, distance_status_quo = ds)
}

#' @rdname LogitVote
#' @export
QuadUtility <- function(x, ideal_point = NULL, salience = NULL) {
  x <- as.numeric(x)
  d <- x - .svm_pt(ideal_point, length(x))
  A <- if (is.null(salience)) diag(length(d)) else if (is.matrix(salience)) salience else diag(salience, length(d))
  u <- -sum(d * (A %*% d))
  list(value = u, loss = -u)
}

.svm_depth <- function(z, P, tol = 1e-12) {
  dx <- P[, 1] - z[1]
  dy <- P[, 2] - z[2]
  same <- abs(dx) <= tol & abs(dy) <= tol
  if (all(same)) return(nrow(P))
  ang <- atan2(dy[!same], dx[!same])
  br <- sort(unique(c((ang + pi / 2) %% (2 * pi), (ang - pi / 2) %% (2 * pi))))
  mids <- c((br[-1] + br[-length(br)]) / 2, (br[length(br)] + br[1] + 2 * pi) / 2)
  min(vapply(mids, function(phi) sum(cos(ang - phi) >= 0), 0)) + sum(same)
}

#' @rdname LogitVote
#' @export
MedianVoter2d <- function(x, ideal_point = NULL) {
  P <- if (is.matrix(x)) x else matrix(as.numeric(x), nrow = 1)
  n <- nrow(P)
  z <- if (!is.null(ideal_point)) as.numeric(ideal_point) else {
    med <- apply(P, 2, function(v) {
      s <- sort(v)
      if (n %% 2) s[n %/% 2 + 1] else (s[n %/% 2] + s[n %/% 2 + 1]) / 2
    })
    cands <- rbind(med, P)
    cands[which.max(apply(cands, 1, .svm_depth, P = P)), ]
  }
  d <- .svm_depth(z, P)
  list(value = d, point = unname(z), is_core = d >= n / 2, n = n)
}

#' @rdname LogitVote
#' @export
UtilityMax <- function(x, ideal_point = NULL, utility = "quadratic", scale = 1) {
  X <- if (is.matrix(x)) x else matrix(as.numeric(x), nrow = 1)
  v <- .svm_pt(ideal_point, ncol(X))
  D2 <- rowSums(sweep(X, 2, v)^2)
  U <- switch(utility, quadratic = -D2, linear = -sqrt(D2), gaussian = exp(-D2 / (2 * scale^2)),
              stop("utility must be quadratic, linear or gaussian", call. = FALSE))
  best <- which.max(U)
  list(value = best, utilities = U, choice = X[best, ])
}

#' @rdname LogitVote
#' @export
VoteTrading <- function(valuations) {
  r <- VoteTradingRikerBrams(valuations)
  c(list(value = length(r$trades)), r)
}

#' @rdname LogitVote
#' @export
VoteTrade2d <- function(valuations) {
  if (ncol(as.matrix(valuations)) != 2) stop("VoteTrade2d needs exactly two issues", call. = FALSE)
  VoteTrading(valuations)
}

#' @rdname LogitVote
#' @export
WeightedVote <- function(weights, quota = NULL, index = "shapley_shubik") {
  r <- PowerIndices(weights, quota)
  c(list(value = r[[index]]), r)
}

#' @rdname LogitVote
#' @export
DiscountUtility <- function(x, ideal_point = NULL, status_quo = NULL, discount = 0.5, beta = 1) {
  x <- as.numeric(x)
  v <- .svm_pt(ideal_point, length(x))
  s <- .svm_pt(status_quo, length(x))
  pp <- s + discount * (x - s)
  list(value = -beta * sum((v - pp)^2), perceived_position = pp)
}

#' @rdname LogitVote
#' @export
MixedUtility <- function(x, ideal_point = NULL, neutral = NULL, beta = 0.5) {
  x <- as.numeric(x)
  v <- .svm_pt(ideal_point, length(x))
  n0 <- .svm_pt(neutral, length(x))
  dirn <- sum((v - n0) * (x - n0))
  prox <- -sum((v - x)^2)
  list(value = 2 * (1 - beta) * dirn + beta * prox, directional = dirn, proximity = prox)
}
