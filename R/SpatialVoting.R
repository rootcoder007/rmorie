.sv_pts <- function(v) {
  if (is.matrix(v)) return(v * 1)
  if (is.list(v)) return(do.call(rbind, lapply(v, as.numeric)))
  matrix(as.numeric(v), ncol = 1)
}

.sv_sum <- function(v) {
  s <- 0
  for (q in v) s <- s + q
  s
}

#' Spatial voting utilities
#'
#' Utility of each voter (rows) for each alternative (columns): quadratic,
#' linear (Euclidean), city-block, Gaussian (NOMINATE), directional dot product,
#' Rabinowitz-Macdonald with a penalty beyond the region of acceptability,
#' angular (Matthews), a convex proximity-directional mixture, Grofman
#' discounting toward the status quo, and categorical proximity/directional;
#' \code{valence} is added per alternative.
#'
#' @param voters,alternatives Points (matrix rows, list of vectors, or a vector for one dimension).
#' @param model Utility model.
#' @param weights Salience per dimension.
#' @param neutral Neutral point for the directional models.
#' @param beta,mix,status_quo,discount,region Model parameters.
#' @param valence Optional valence per alternative.
#' @return list(utility, choice (1-based), total).
#' @references Enelow, J. M. and Hinich, M. J. (1984). The Spatial Theory of
#'   Voting. Cambridge University Press. Rabinowitz, G. and Macdonald, S. E.
#'   (1989). A directional theory of issue voting. American Political Science
#'   Review 83, 93-121. Grofman, B. (1985). The neglected role of the status quo in
#'   models of issue voting. Journal of Politics 47, 230-237.
#' @examples
#' VoterUtility(0, c(1, -2))$utility
#' @export
VoterUtility <- function(voters, alternatives, model = c("quadratic", "linear", "cityblock", "gaussian", "dot", "angular", "rm",
                                                            "mixed", "discount", "categorical_proximity", "categorical_directional"),
                           weights = NULL, neutral = NULL, beta = 1, mix = 0.5, status_quo = NULL, discount = 0.5, region = NULL,
                           valence = NULL) {
  model <- match.arg(model)
  X <- .sv_pts(voters)
  Z <- .sv_pts(alternatives)
  d <- ncol(X)
  if (ncol(Z) != d) stop("voters and alternatives need the same dimension", call. = FALSE)
  w <- if (is.null(weights)) rep(1, d) else as.numeric(weights)
  N <- if (is.null(neutral)) rep(0, d) else as.numeric(neutral)
  SQ <- if (is.null(status_quo)) rep(0, d) else as.numeric(status_quo)
  quad <- function(x, z) -.sv_sum(w * (x - z)^2)
  dotp <- function(x, z) .sv_sum((x - N) * (z - N))
  nrm <- function(v) sqrt(.sv_sum((v - N)^2))
  u <- function(x, z) {
    switch(model,
      quadratic = quad(x, z),
      linear = -sqrt(-quad(x, z)),
      cityblock = -.sv_sum(w * abs(x - z)),
      gaussian = beta * exp(-0.5 * .sv_sum(w^2 * (x - z)^2)),
      dot = dotp(x, z),
      rm = dotp(x, z) - (if (is.null(region)) 0 else beta * max(0, nrm(z) - region)),
      angular = {
        nx <- nrm(x)
        nz <- nrm(z)
        if (nx > 0 && nz > 0) dotp(x, z) / (nx * nz) else 0
      },
      mixed = (1 - mix) * quad(x, z) + mix * dotp(x, z),
      discount = quad(x, SQ + discount * (z - SQ)),
      categorical_proximity = -sum(x != z),
      categorical_directional = .sv_sum(x * z))
  }
  val <- if (is.null(valence)) rep(0, nrow(Z)) else as.numeric(valence)
  U <- t(vapply(seq_len(nrow(X)), function(i) vapply(seq_len(nrow(Z)), function(j) u(X[i, ], Z[j, ]) + val[j], 0), numeric(nrow(Z))))
  if (nrow(Z) == 1) U <- t(U)
  U <- matrix(U, nrow(X), nrow(Z))
  list(utility = U, choice = apply(U, 1, function(r) which(r == max(r))[1]), total = colSums(U))
}

#' Spatial vote probabilities
#'
#' Binary probabilities F((U_1 - U_2) / scale) with normal, logistic, Laplace,
#' Student-t, Gompertz or complementary log-log F; multinomial (conditional
#' logit) or exponential-distance (Luce) choice probabilities; and, for quadratic
#' utility with a normal link, the exact integral over an uncertain ideal point
#' with covariance \code{ideal_cov}.
#'
#' @inheritParams VoterUtility
#' @param link Binary link.
#' @param scale Error scale.
#' @param df Student-t degrees of freedom.
#' @param rule "binary", "multinomial" or "exponential".
#' @param ideal_cov Optional covariance of the voter ideal point.
#' @param ... Passed to \code{VoterUtility}.
#' @return list(probability, utility).
#' @references Poole, K. T. and Rosenthal, H. (1985). A spatial model for
#'   legislative roll call analysis. American Journal of Political Science 29,
#'   357-384. McFadden, D. (1974). Conditional logit analysis of qualitative
#'   choice behavior. In Frontiers in Econometrics, 105-142.
#' @examples
#' VoteProbability(0, c(1, -1))$probability
#' @export
VoteProbability <- function(voters, alternatives, model = "quadratic", link = c("normal", "logistic", "laplace", "student_t", "gompertz", "cloglog"),
                            scale = 1, df = 5, rule = c("binary", "multinomial", "exponential"), ideal_cov = NULL, ...) {
  link <- match.arg(link)
  rule <- match.arg(rule)
  U <- VoterUtility(voters, alternatives, model = model, ...)$utility
  X <- .sv_pts(voters)
  Z <- .sv_pts(alternatives)
  cdf <- function(u) switch(link,
    normal = stats::pnorm(u), logistic = stats::plogis(u), laplace = ifelse(u < 0, 0.5 * exp(u), 1 - 0.5 * exp(-u)),
    student_t = stats::pt(u, df), gompertz = exp(-exp(-u)), cloglog = 1 - exp(-exp(u)))
  if (rule == "binary") {
    if (ncol(U) != 2) stop("binary voting needs exactly two alternatives", call. = FALSE)
    s <- scale
    if (!is.null(ideal_cov)) {
      if (model != "quadratic" || link != "normal") stop("ideal_cov needs model = 'quadratic' and link = 'normal'", call. = FALSE)
      dz <- Z[1, ] - Z[2, ]
      s <- sqrt(scale^2 + 4 * as.numeric(t(dz) %*% as.matrix(ideal_cov) %*% dz))
    }
    p1 <- cdf((U[, 1] - U[, 2]) / s)
    return(list(probability = cbind(p1, 1 - p1, deparse.level = 0), utility = U))
  }
  if (rule == "multinomial") {
    P <- t(apply(U, 1, function(r) {
      e <- exp((r - max(r)) / scale)
      e / .sv_sum(e)
    }))
  } else {
    P <- t(vapply(seq_len(nrow(X)), function(i) {
      e <- exp(-sqrt(colSums((t(Z) - X[i, ])^2)) / scale)
      e / .sv_sum(e)
    }, numeric(nrow(Z))))
  }
  list(probability = matrix(P, nrow(X)), utility = U)
}
