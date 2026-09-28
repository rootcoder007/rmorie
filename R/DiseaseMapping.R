#' Indirect standardisation and standardised ratios
#'
#' \code{ExpectedCounts}: \eqn{E_i = \sum_s n_{is} r_s} with pooled stratum
#' rates, as \code{SpatialEpi::expected} (areas x strata tables).
#' \code{StandardizedRatio}: \eqn{y/E} with exact Poisson limits (Garwood
#' 1936), as \code{poisson.test}. Identical to the Python arm
#' \code{morie.fn.dismap}.
#'
#' @param population,cases Areas x strata matrices.
#' @param observed,expected Counts.
#' @param conf Confidence level.
#' @return Vector or list.
#' @references Garwood, F. (1936). Fiducial limits for the Poisson
#'   distribution. Biometrika 28, 437-442.
#' @examples
#' ExpectedCounts(rbind(c(100, 50), c(200, 50)), rbind(c(1, 2), c(3, 3)))
#' StandardizedRatio(7, 5.5)
#' @export
ExpectedCounts <- function(population, cases) {
  P <- as.matrix(population)
  Y <- as.matrix(cases)
  as.vector(P %*% (colSums(Y) / colSums(P)))
}

#' @rdname ExpectedCounts
#' @export
StandardizedRatio <- function(observed, expected, conf = 0.95) {
  a <- 1 - conf
  list(ratio = observed / expected,
       lower = ifelse(observed > 0, stats::qchisq(a / 2, 2 * observed) / (2 * expected), 0),
       upper = stats::qchisq(1 - a / 2, 2 * (observed + 1)) / (2 * expected))
}

#' Empirical Bayes rate smoothing and probability maps
#'
#' \code{EbGlobal}: Marshall (1991) global estimator as \code{spdep::EBest}.
#' \code{EbLocal}: local estimator as \code{spdep::EBlocal} (neighbourhoods
#' include the unit; variance terms use each neighbour's local mean).
#' \code{ProbabilityMap}: Choynowski (1959) Poisson tail probabilities as
#' \code{spdep::probmap}.
#'
#' @param cases Counts.
#' @param population Populations at risk.
#' @param neighbours List of 1-based neighbour indices (self excluded).
#' @param alternative \code{"less"} or \code{"greater"}.
#' @return List.
#' @references Marshall, R. J. (1991). Mapping disease and mortality rates
#'   using empirical Bayes estimators. Journal of the Royal Statistical
#'   Society C 40, 283-294.
#'
#'   Choynowski, M. (1959). Maps based on probabilities. Journal of the
#'   American Statistical Association 54, 385-388.
#' @examples
#' EbGlobal(c(2, 10, 3), c(100, 200, 150))$estimate
#' EbLocal(c(2, 10, 3), c(100, 200, 150), list(2, c(1, 3), 2))$estimate
#' ProbabilityMap(c(2, 10, 3), c(100, 200, 150))$pmap
#' @export
EbGlobal <- function(cases, population) {
  p <- cases / population
  b <- sum(cases) / sum(population)
  s2 <- sum(population * (p - b)^2) / sum(population)
  a <- max(0, s2 - b / mean(population))
  list(raw = p, estimate = b + a * (p - b) / (a + b / population), a = a, b = b)
}

#' @rdname EbGlobal
#' @export
EbLocal <- function(cases, population, neighbours) {
  k <- length(cases)
  x <- cases / population
  Nb <- lapply(seq_len(k), function(i) sort(unique(c(i, neighbours[[i]]))))
  ni <- vapply(Nb, function(N) sum(population[N]), 0)
  m <- vapply(Nb, function(N) sum(cases[N]), 0) / ni
  nbar <- ni / lengths(Nb)
  C <- vapply(Nb, function(N) sum(population[N] * (x[N] - m[N])^2), 0)
  a <- pmax(0, C / ni - m / nbar)
  den <- a + m / population
  list(raw = x, estimate = ifelse(den > 0, m + (x - m) * (a / den), m), a = a, m = m)
}

#' @rdname EbGlobal
#' @export
ProbabilityMap <- function(cases, population, alternative = "less") {
  if (!alternative %in% c("less", "greater")) stop("alternative must be less or greater")
  E <- population * sum(cases) / sum(population)
  pm <- if (alternative == "less") stats::ppois(cases, E) else 1 - stats::ppois(cases - 1, E)
  list(raw = cases / population, expected = E, relrisk = 100 * cases / E, pmap = pm)
}

#' Poisson-gamma empirical Bayes relative risks
#'
#' Negative binomial maximum likelihood for \eqn{(\beta, \alpha)} (Newton on
#' \eqn{(\beta, \log\alpha)}), posterior \eqn{Gamma(\alpha + y, (\alpha +
#' E\mu)/\mu)}, its mean, median and exceedance probabilities, as
#' \code{SpatialEpi::eBayes} and \code{EBpostthresh} (Clayton and Kaldor 1987).
#'
#' @param observed,expected Counts and expected counts.
#' @param X Optional covariate matrix.
#' @param threshold Relative-risk threshold for exceedance probabilities.
#' @return List with RR, RRmed, beta, alpha, SMR and exceedance.
#' @references Clayton, D. and Kaldor, J. (1987). Empirical Bayes estimates
#'   of age-standardized relative risks for use in disease mapping.
#'   Biometrics 43, 671-681.
#' @examples
#' PoissonGammaEb(c(3, 8, 1, 12, 5, 2), c(4, 5.5, 2.5, 7, 6, 3))$alpha
#' @export
PoissonGammaEb <- function(observed, expected, X = NULL, threshold = NULL) {
  y <- observed
  E <- expected
  n <- length(y)
  Xm <- if (is.null(X)) matrix(1, n, 1) else cbind(1, as.matrix(X))
  p <- ncol(Xm)
  b <- c(log(sum(y) / sum(E)), rep(0, p - 1))
  mu <- E * exp(as.vector(Xm %*% b))
  m <- mean(mu)
  v <- mean((y - mu)^2)
  lt <- log(if (v > m) m^2 / (v - m) else 100)
  for (it in 1:500) {
    th <- exp(lt)
    mu <- E * exp(as.vector(Xm %*% b))
    gb <- as.vector(crossprod(Xm, (y - mu) * th / (th + mu)))
    gt <- sum(digamma(y + th) - digamma(th) + log(th / (th + mu)) + 1 - (y + th) / (th + mu))
    Hbb <- -crossprod(Xm * (mu * th * (y + th) / (th + mu)^2), Xm)
    Hbt <- as.vector(crossprod(Xm, mu * (y - mu) / (th + mu)^2)) * th
    htt <- sum(trigamma(y + th) - trigamma(th) + 1 / th - 2 / (th + mu) + (y + th) / (th + mu)^2)
    H <- rbind(cbind(Hbb, Hbt), c(Hbt, htt * th^2 + gt * th))
    step <- unname(solve(H, c(gb, gt * th)))
    b <- b - step[1:p]
    lt <- lt - step[p + 1]
    if (max(abs(step)) < 1e-12) break
  }
  alpha <- exp(lt)
  mu <- exp(as.vector(Xm %*% b))
  w <- E * mu / (alpha + E * mu)
  smr <- y / E
  out <- list(RR = w * smr + (1 - w) * mu, RRmed = stats::qgamma(0.5, alpha + y, (alpha + E * mu) / mu),
              beta = b, alpha = alpha, SMR = smr)
  if (!is.null(threshold)) {
    out$exceedance <- stats::pgamma(threshold, alpha + y, (alpha + E * mu) / mu, lower.tail = FALSE)
  }
  out
}
