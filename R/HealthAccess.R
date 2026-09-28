#' Health inequality indices
#'
#' \code{HealthConcentrationIndex}: \eqn{2\,cov(h, r)/\bar h} with fractional ranks
#' (mean ranks for ties) and the Erreygers correction (Kakwani, Wagstaff and
#' van Doorslaer 1997). \code{SpatialGini}: Gini as \code{ineq::Gini} with
#' the Rey and Smith (2013) neighbour / non-neighbour decomposition.
#' \code{TheilDecomposition}: Theil T and L (\code{ineq::Theil}) with the
#' between/within decomposition (Shorrocks 1980). Identical to the Python arm
#' \code{morie.fn.hlthacc}.
#'
#' @param health Health variable.
#' @param rank_variable Ranking variable.
#' @param x Values.
#' @param W Optional weights (nonzero = neighbour).
#' @param groups Optional group labels.
#' @return List.
#' @references Kakwani, N., Wagstaff, A. and van Doorslaer, E. (1997).
#'   Socioeconomic inequalities in health: measurement, computation, and
#'   statistical inference. Journal of Econometrics 77, 87-103.
#'
#'   Rey, S. J. and Smith, R. J. (2013). A spatial decomposition of the Gini
#'   coefficient. Letters in Spatial and Resource Sciences 6, 55-70.
#' @examples
#' HealthConcentrationIndex(c(4, 3, 2, 1), c(10, 20, 30, 40))$index
#' SpatialGini(1:4)$gini
#' TheilDecomposition(1:4)$T
#' @export
HealthConcentrationIndex <- function(health, rank_variable) {
  n <- length(health)
  r <- (rank(rank_variable, ties.method = "average") - 0.5) / n
  C <- 2 * mean((health - mean(health)) * (r - mean(r))) / mean(health)
  rg <- diff(range(health))
  list(index = C, fractional_rank = r, erreygers = if (rg > 0) 4 * mean(health) * C / rg else NaN)
}

#' @rdname HealthConcentrationIndex
#' @export
SpatialGini <- function(x, W = NULL) {
  n <- length(x)
  A <- abs(outer(x, x, `-`))
  den <- 2 * n^2 * mean(x)
  out <- list(gini = sum(A) / den)
  if (!is.null(W)) {
    nb <- sum(A[as.matrix(W) != 0]) / den
    out$neighbour <- nb
    out$non_neighbour <- out$gini - nb
  }
  out
}

#' @rdname HealthConcentrationIndex
#' @export
TheilDecomposition <- function(x, groups = NULL) {
  pos <- x[x != 0]
  mu <- mean(pos)
  out <- list(T = sum(pos * log(pos / mu)) / sum(pos), L = if (all(pos > 0)) mean(log(mu / pos)) else NaN)
  if (!is.null(groups)) {
    tot <- sum(x)
    mall <- tot / length(x)
    lv <- sort(unique(as.character(groups)))
    gt <- vapply(lv, function(k) {
      xs <- x[as.character(groups) == k]
      sum(xs[xs > 0] * log(xs[xs > 0] / mean(xs))) / sum(xs)
    }, 0)
    sg <- vapply(lv, function(k) sum(x[as.character(groups) == k]) / tot, 0)
    mg <- vapply(lv, function(k) mean(x[as.character(groups) == k]), 0)
    out$within <- sum(sg * gt)
    out$between <- sum(sg * log(mg / mall))
    out$group_T <- gt
  }
  out
}

#' Area deprivation indices (Townsend, Carstairs)
#'
#' Sums of z-scored indicators (sample standard deviation); Townsend
#' (unemployment, no car, not owner-occupied, overcrowding) log-transforms
#' the first and last column (Townsend et al. 1988); Carstairs (overcrowding,
#' male unemployment, no car, low social class) uses raw percentages
#' (Carstairs and Morris 1991); \code{"sum"} any columns.
#'
#' @param indicators Area x indicator matrix.
#' @param method \code{"townsend"}, \code{"carstairs"} or \code{"sum"}.
#' @return List with \code{score} and \code{z}.
#' @references Townsend, P., Phillimore, P. and Beattie, A. (1988). Health and
#'   Deprivation: Inequality and the North. Croom Helm.
#' @examples
#' DeprivationIndex(rbind(c(1, 2), c(3, 2), c(5, 8)), "sum")$score
#' @export
DeprivationIndex <- function(indicators, method = "townsend") {
  X <- as.matrix(indicators)
  if (method %in% c("townsend", "carstairs") && ncol(X) != 4) stop(method, " needs 4 columns")
  if (method == "townsend") X[, c(1, 4)] <- log(X[, c(1, 4)] + 1)
  if (!method %in% c("townsend", "carstairs", "sum")) stop("method must be townsend, carstairs or sum")
  Z <- scale(X)
  list(score = unname(rowSums(Z)), z = unname(Z[, , drop = FALSE]))
}

#' Spatial accessibility: FCA family, gravity access and nearest facility
#'
#' \code{FcaAccessibility}: 2SFCA (Luo and Wang 2003), E2SFCA (Luo and Qi
#' 2009, stepwise weights), KD2SFCA (weights exp(-d^power) within \code{d0}),
#' Gaussian (Dai 2010) and 3SFCA (Wan, Zou and Sternberg 2012).
#' \code{GravityAccessibility}: Hansen (1959) sum over j of S_j exp(-beta d_ij).
#' \code{NearestFacility}: nearest supply site. \code{RadiationFlows}:
#' Simini et al. (2012) radiation model.
#'
#' @param supply Supply at facilities.
#' @param demand Demand populations.
#' @param D Demand x supply distance matrix.
#' @param d0 Catchment radius.
#' @param method FCA variant.
#' @param steps E2SFCA list of c(upper distance, weight).
#' @param power KD2SFCA exponent.
#' @param beta Gravity decay.
#' @param population Populations.
#' @param coords Coordinates.
#' @param outflow Total outflows (default the populations).
#' @return List, vector or matrix.
#' @references Luo, W. and Wang, F. (2003). Measures of spatial accessibility
#'   to health care in a GIS environment. Environment and Planning B 30,
#'   865-884.
#'
#'   Simini, F., Gonzalez, M. C., Maritan, A. and Barabasi, A.-L. (2012). A
#'   universal model for mobility and migration patterns. Nature 484, 96-100.
#' @examples
#' FcaAccessibility(c(10, 5), c(100, 200, 100), rbind(c(1, 5), c(2, 2), c(5, 1)), 3)$access
#' GravityAccessibility(c(10, 5), rbind(c(1, 5), c(2, 2)), 0.5)
#' @export
FcaAccessibility <- function(supply, demand, D, d0, method = "2SFCA", steps = NULL, power = 2) {
  if (!method %in% c("2SFCA", "E2SFCA", "KD2SFCA", "gaussian", "3SFCA")) {
    stop("method must be 2SFCA, E2SFCA, KD2SFCA, gaussian or 3SFCA")
  }
  if (method == "E2SFCA" && is.null(steps)) stop("E2SFCA needs steps")
  D <- as.matrix(D)
  wf <- function(d) {
    if (d > d0) return(0)
    switch(method,
      "2SFCA" = 1,
      E2SFCA = {
        hit <- which(vapply(steps, function(s) d <= s[1], TRUE))
        if (length(hit)) steps[[hit[1]]][2] else 0
      },
      KD2SFCA = exp(-d^power),
      (exp(-0.5 * (d / d0)^2) - exp(-0.5)) / (1 - exp(-0.5))
    )
  }
  W <- matrix(vapply(D, wf, 0), nrow(D))
  G <- if (method == "3SFCA") W / ifelse(rowSums(W) > 0, rowSums(W), 1) else matrix(1, nrow(D), ncol(D))
  den <- colSums(G * W * demand)
  R <- ifelse(den > 0, supply / den, 0)
  list(access = as.vector((G * W) %*% R), ratio = R)
}

#' @rdname FcaAccessibility
#' @export
GravityAccessibility <- function(supply, D, beta) as.vector(exp(-beta * as.matrix(D)) %*% supply)

#' @rdname FcaAccessibility
#' @export
NearestFacility <- function(D) {
  D <- as.matrix(D)
  idx <- apply(D, 1, which.min)
  list(distance = D[cbind(seq_len(nrow(D)), idx)], index = idx)
}

#' @rdname FcaAccessibility
#' @export
RadiationFlows <- function(population, coords, outflow = NULL) {
  m <- population
  Dm <- as.matrix(stats::dist(as.matrix(coords)))
  Tt <- if (is.null(outflow)) m else outflow
  n <- length(m)
  out <- matrix(0, n, n)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i == j) next
      s <- sum(m[setdiff(which(Dm[i, ] < Dm[i, j]), c(i, j))])
      out[i, j] <- Tt[i] * m[i] * m[j] / ((m[i] + s) * (m[i] + m[j] + s))
    }
  }
  out
}
