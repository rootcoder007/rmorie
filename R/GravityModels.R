.grv_design <- function(mass_o, mass_d, dist) {
  if (!(length(mass_o) == length(mass_d) && length(mass_d) == length(dist))) {
    stop("mass_o, mass_d and dist must have the same length")
  }
  if (min(mass_o) <= 0 || min(mass_d) <= 0 || min(dist) <= 0) stop("masses and distances must be positive")
  cbind(1, log(mass_o), log(mass_d), log(dist))
}

.grv_ols <- function(X, y) {
  A <- solve(crossprod(X))
  b <- as.vector(A %*% crossprod(X, y))
  e <- y - as.vector(X %*% b)
  s2 <- sum(e^2) / (nrow(X) - ncol(X))
  list(coefficients = b, se = sqrt(s2 * diag(A)), sigma2 = s2, r2 = 1 - sum(e^2) / sum((y - mean(y))^2),
       residuals = e)
}

.grv_theta <- function(y, mu, theta, tol = 1e-13, max_iter = 100) {
  if (is.null(theta)) theta <- length(y) / sum((y / mu - 1)^2)
  for (it in seq_len(max_iter)) {
    s <- sum(digamma(y + theta) - digamma(theta) + log(theta) + 1 - log(theta + mu) - (y + theta) / (theta + mu))
    i <- -sum(trigamma(y + theta) - trigamma(theta) + 1 / theta - 2 / (theta + mu) + (y + theta) / (theta + mu)^2)
    if (!(i > 0) || theta > 1e10) stop("no overdispersion: theta diverges; fit the Poisson model instead")
    step <- s / i
    while (theta + step <= 0) step <- step / 2
    theta <- theta + step
    if (abs(step) < tol * max(1, theta)) break
  }
  theta
}

.grv_glm <- function(X, y, family = "poisson", offset = NULL, tol = 1e-13, max_iter = 200) {
  n <- nrow(X)
  off <- if (is.null(offset)) rep(0, n) else offset
  mu <- (y + mean(y)) / 2
  eta <- log(mu)
  b <- rep(0, ncol(X))
  theta <- Inf
  for (it in seq_len(max_iter)) {
    w <- if (family == "poisson") mu else mu / (1 + mu / theta)
    z <- eta - off + (y - mu) / mu
    nb <- as.vector(solve(crossprod(X, X * w), crossprod(X, w * z)))
    eta <- as.vector(X %*% nb) + off
    mu <- exp(eta)
    change <- max(abs(nb - b) / pmax(abs(nb), 1))
    b <- nb
    if (family == "negbin") {
      old <- theta
      theta <- .grv_theta(y, mu, if (it == 1) NULL else theta)
      change <- max(change, if (is.finite(old)) abs(theta - old) / theta else 1)
    }
    if (change < tol) break
  }
  w <- if (family == "poisson") mu else mu / (1 + mu / theta)
  A <- solve(crossprod(X, X * w))
  V <- A %*% crossprod(X, X * (w * (y - mu) / mu)^2) %*% A
  ll <- if (family == "poisson") {
    sum(y * log(mu) - mu - lgamma(y + 1))
  } else {
    sum(lgamma(theta + y) - lgamma(theta) - lgamma(y + 1) + theta * log(theta) +
          ifelse(y > 0, y * log(mu), 0) - (theta + y) * log(theta + mu))
  }
  out <- list(coefficients = b, se = sqrt(diag(A)), se_robust = sqrt(diag(V)), fitted = mu, loglik = ll,
              iterations = it)
  if (family == "negbin") out$theta <- theta
  out
}

#' Gravity and spatial interaction models
#'
#' R arm of the Python modules \code{morie.fn.igrav*}. \code{Igrav}:
#' log-linear gravity OLS on the positive flows. \code{Igravbl}: iterative
#' proportional fitting to origin and destination totals (as
#' \code{stats::loglin} with a start table). \code{Igravcl}: distance-decay
#' exponent by Poisson ML with the mass product as offset (reproduces total
#' flow and mean log distance). \code{Igraver}: RMSE and standardised RMSE.
#' \code{Igravfe}: PPML with origin and destination fixed effects.
#' \code{Igravlm}: LM error test on gravity residuals. \code{Igravnb}:
#' negative-binomial (NB2) gravity by ML, as \code{MASS::glm.nb}.
#' \code{Igravrt}: Huff retail probabilities. \code{Igravsq}: symmetric
#' gravity on a square OD matrix with an asymmetry index. \code{Igravvf}:
#' variance function \eqn{\phi\mu^p}. \code{Igravwl}: Wilson's doubly
#' constrained entropy model.
#'
#' @param flows Observed flows (vector of pairs; a matrix for
#'   \code{Igravwl}, or NULL there).
#' @param mass_o,mass_d Origin and destination masses (totals for
#'   \code{Igravwl}).
#' @param dist Distances (a cost matrix for \code{Igravwl}, a consumer by
#'   centre matrix for \code{Igravrt}).
#' @param flow_matrix Square or rectangular flow matrix.
#' @param row_totals,col_totals Target totals.
#' @param tol Relative tolerance.
#' @param max_iter Iteration cap.
#' @param flows_hat Predicted flows.
#' @param origin_id,dest_id Origin and destination labels.
#' @param resid Gravity residuals.
#' @param W Weights between origin-destination pairs.
#' @param mass Attractiveness (\code{Igravrt}) or masses of the places
#'   (\code{Igravsq}).
#' @param beta Distance- or cost-decay parameter.
#' @param demand Demand at each consumer location.
#' @param dist_matrix Distances between the places.
#' @param phi Dispersion.
#' @param power Variance power.
#' @return A list whose \code{statistic} is the headline value, with the
#'   components of the Python result.
#' @references Tinbergen, J. (1962). Shaping the World Economy. Twentieth
#'   Century Fund, New York.
#'
#'   Santos Silva, J. M. C. and Tenreyro, S. (2006). The log of gravity.
#'   Review of Economics and Statistics 88, 641-658.
#'
#'   Wilson, A. G. (1967). A statistical theory of spatial distribution
#'   models. Transportation Research 1, 253-269.
#'
#'   Deming, W. E. and Stephan, F. F. (1940). On a least squares adjustment
#'   of a sampled frequency table when the expected marginal totals are
#'   known. Annals of Mathematical Statistics 11, 427-444.
#'
#'   Huff, D. L. (1963). A probabilistic analysis of shopping center trade
#'   areas. Land Economics 39, 81-90.
#'
#'   Hyman, G. M. (1969). The calibration of trip distribution models.
#'   Environment and Planning 1, 105-112.
#'
#'   Anderson, J. E. and van Wincoop, E. (2003). Gravity with gravitas.
#'   American Economic Review 93, 170-192.
#'
#'   Knudsen, D. C. and Fotheringham, A. S. (1986). Matrix comparison,
#'   goodness-of-fit, and spatial interaction modeling. International
#'   Regional Science Review 10, 127-147.
#' @examples
#' Fl <- c(12, 3, 30, 7, 55, 4, 9, 21)
#' mo <- c(5, 5, 9, 9, 20, 20, 7, 7)
#' md <- c(9, 20, 5, 20, 5, 9, 20, 9)
#' d <- c(1, 3, 1, 2, 3, 2, 2.5, 1.5)
#' Igrav(Fl, mo, md, d)$coefficients
#' Igravcl(Fl, mo, md, d)$statistic
#' Igravbl(matrix(1:4, 2, byrow = TRUE), c(5, 5), c(6, 4))$balanced
#' @export
Igrav <- function(flows, mass_o, mass_d, dist) {
  flows <- as.numeric(flows)
  X <- .grv_design(mass_o, mass_d, dist)
  k <- flows > 0
  r <- .grv_ols(X[k, , drop = FALSE], log(flows[k]))
  list(statistic = r$coefficients[4], coefficients = r$coefficients, se = r$se, r2 = r$r2, sigma2 = r$sigma2,
       n_used = sum(k))
}

#' @rdname Igrav
#' @export
Igravbl <- function(flow_matrix, row_totals, col_totals, tol = 1e-12, max_iter = 10000) {
  Tm <- as.matrix(flow_matrix) * 1
  if (abs(sum(row_totals) - sum(col_totals)) > 1e-9 * max(1, sum(row_totals))) {
    stop("row and column totals must have the same sum")
  }
  for (it in seq_len(max_iter)) {
    s <- rowSums(Tm)
    Tm[s > 0, ] <- Tm[s > 0, , drop = FALSE] * (row_totals[s > 0] / s[s > 0])
    s <- colSums(Tm)
    Tm[, s > 0] <- sweep(Tm[, s > 0, drop = FALSE], 2, col_totals[s > 0] / s[s > 0], "*")
    err <- max(abs(rowSums(Tm) - row_totals))
    if (err < tol * max(1, max(row_totals))) break
  }
  list(statistic = err, balanced = unname(Tm), iterations = it)
}

#' @rdname Igrav
#' @export
Igravcl <- function(flows, mass_o, mass_d, dist) {
  X <- .grv_design(mass_o, mass_d, dist)
  g <- .grv_glm(X[, c(1, 4)], as.numeric(flows), "poisson", offset = X[, 2] + X[, 3])
  list(statistic = -g$coefficients[2], k = exp(g$coefficients[1]), se_beta = g$se[2], fitted = g$fitted,
       loglik = g$loglik)
}

#' @rdname Igrav
#' @export
Igraver <- function(flows, flows_hat) {
  if (length(flows) != length(flows_hat)) stop("flows and flows_hat must have the same length")
  rmse <- sqrt(sum((flows - flows_hat)^2) / length(flows))
  list(statistic = rmse, srmse = rmse / mean(flows))
}

#' @rdname Igrav
#' @export
Igravfe <- function(flows, origin_id, dest_id, dist) {
  flows <- as.numeric(flows)
  if (!(length(flows) == length(origin_id) && length(origin_id) == length(dest_id) &&
          length(dest_id) == length(dist))) {
    stop("all inputs must have the same length")
  }
  ol <- sort(unique(as.character(origin_id)))
  dl <- sort(unique(as.character(dest_id)))
  X <- cbind(outer(as.character(origin_id), ol, "==") * 1,
             outer(as.character(dest_id), dl[-1], "==") * 1, log(dist))
  g <- .grv_glm(X, flows, "poisson")
  k <- length(ol)
  cf <- g$coefficients
  list(statistic = cf[length(cf)], se = g$se[length(cf)], se_robust = g$se_robust[length(cf)],
       origin_effects = stats::setNames(cf[seq_len(k)], ol),
       destination_effects = stats::setNames(c(0, cf[k + seq_len(length(dl) - 1)]), dl),
       fitted = g$fitted, loglik = g$loglik)
}

#' @rdname Igrav
#' @export
Igravlm <- function(resid, W) {
  Semlm(resid, W)
}

#' @rdname Igrav
#' @export
Igravnb <- function(flows, mass_o, mass_d, dist) {
  g <- .grv_glm(.grv_design(mass_o, mass_d, dist), as.numeric(flows), "negbin")
  list(statistic = g$coefficients[4], coefficients = g$coefficients, se = g$se, se_robust = g$se_robust,
       theta = g$theta, loglik = g$loglik, fitted = g$fitted)
}

#' @rdname Igrav
#' @export
Igravrt <- function(mass, dist, beta = 2, demand = NULL) {
  D <- as.matrix(dist)
  U <- sweep(D^(-beta), 2, as.numeric(mass), "*")
  P <- U / rowSums(U)
  w <- if (is.null(demand)) rep(1, nrow(P)) else as.numeric(demand)
  pat <- as.vector(crossprod(P, w))
  list(statistic = pat[1], probabilities = unname(P), patronage = pat)
}

#' @rdname Igrav
#' @export
Igravsq <- function(flow_matrix, mass, dist_matrix) {
  Fm <- as.matrix(flow_matrix)
  D <- as.matrix(dist_matrix)
  n <- length(mass)
  k <- which(Fm > 0 & row(Fm) != col(Fm), arr.ind = TRUE)
  k <- k[order(k[, 1], k[, 2]), , drop = FALSE]
  X <- cbind(1, log(mass[k[, 1]] * mass[k[, 2]]), log(D[k]))
  r <- .grv_ols(X, log(Fm[k]))
  up <- upper.tri(Fm)
  list(statistic = r$coefficients[3], coefficients = r$coefficients, se = r$se, r2 = r$r2,
       asymmetry = sum(abs(Fm - t(Fm))[up]) / sum((Fm + t(Fm))[up]), n_used = nrow(k))
}

#' @rdname Igrav
#' @export
Igravvf <- function(flows_hat, phi = 1, power = 1) {
  v <- phi * as.numeric(flows_hat)^power
  list(statistic = mean(v), local_values = v)
}

#' @rdname Igrav
#' @export
Igravwl <- function(flows, mass_o, mass_d, dist, beta = 1, tol = 1e-12, max_iter = 10000) {
  O <- as.numeric(mass_o)
  Dt <- as.numeric(mass_d)
  C <- as.matrix(dist)
  f <- exp(-beta * C)
  B <- rep(1, length(Dt))
  for (it in seq_len(max_iter)) {
    A <- 1 / as.vector(f %*% (B * Dt))
    Bn <- 1 / as.vector(crossprod(f, A * O))
    ch <- max(abs(Bn - B) / abs(Bn))
    B <- Bn
    if (ch < tol) break
  }
  A <- 1 / as.vector(f %*% (B * Dt))
  Tm <- outer(A * O, B * Dt) * f
  out <- list(statistic = sum(Tm * C) / sum(Tm), T = unname(Tm), A = A, B = B, iterations = it)
  if (!is.null(flows)) {
    Fm <- as.matrix(flows)
    out$srmse <- sqrt(mean((Fm - Tm)^2)) / mean(Fm)
    out$observed_mean_cost <- sum(Fm * C) / sum(Fm)
  }
  out
}
