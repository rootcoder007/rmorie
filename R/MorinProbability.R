.mrn_prob <- function(p, nm) {
  if (any(!is.finite(p)) || any(p < 0 | p > 1)) stop(nm, " must be in [0, 1]")
  invisible(p)
}

#' Probability results of Morin's Probability for the Enthusiastic Beginner
#'
#' R arm of the Python modules of the Morin shelf (\code{morie.fn.at_least_one_of_iid},
#' \code{bayes_general}, \code{sd_of_mean}, ...): counting (factorial, stars
#' and bars, card-hand suit patterns), the rules of probability (chain rule,
#' or rules, inclusion-exclusion, Bayes), independence of joint pmfs and
#' densities, moments of pmfs and their scaling and sums, standard
#' deviations of means, the exponential waiting-time density and the
#' correlation results of chapter 6 (regression slope, prediction
#' improvement, regression to the mean). Each returns the Python result's
#' payload as a list.
#'
#' @param p,k Event probability and number of independent trials.
#' @param n_suits,n_ranks,hand,k_major,k_minor Deck and hand sizes.
#' @param priors,likelihoods Prior and likelihood vectors.
#' @param p_z_given_a,p_a,p_z Terms of the simple Bayes formula.
#' @param p_b_given_a,p_b,p_a_given_b Terms of the chain rule.
#' @param sigma,mu Standard deviation and mean.
#' @param n Integer size.
#' @param r Correlation coefficient.
#' @param a,e_x,b,e_y,c Coefficients and expectations.
#' @param rate_fast,rate_slow,ratio Exponential rates and ratio.
#' @param t,lam Time and rate.
#' @param p_c,p_ab,p_ac,p_bc,p_abc Event and intersection probabilities.
#' @param grid_x,density_x,grid_y,density_y Density grids.
#' @param joint Joint pmf table.
#' @param tol Tolerance.
#' @param values,probs Pmf support and probabilities.
#' @param values_x,probs_x,values_y,probs_y Two pmfs.
#' @param ps Probabilities of exclusive events.
#' @param y1 First-test score deviation.
#' @param sigmas Standard deviations.
#' @param x,y Data vectors.
#' @param N Number of categories.
#' @param var_x,var_y,cov_xy Variances and covariance.
#' @return A named list (the Python payload).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace Independent Publishing, chapters 1-6.
#' @examples
#' AtLeastOneOfIid(1 / 6, 3)$p_at_least_one
#' BayesGeneral(c(0.5, 0.3, 0.2), c(0.1, 0.4, 0.8))$posteriors
#' StarsAndBars(3, 4)$count
#' @export
AtLeastOneOfIid <- function(p, k = 3) {
  .mrn_prob(p, "p")
  j <- seq_len(k)
  total <- sum((-1)^(j + 1) * choose(k, j) * p^j)
  list(p = p, k = k, p_at_least_one = total)
}

#' @rdname AtLeastOneOfIid
#' @export
AtMostTwoSuitsProbability <- function(n_suits = 4, n_ranks = 13, hand = 5) {
  fav <- choose(n_suits, 2) * choose(2 * n_ranks, hand) - (n_suits - 2) * n_suits * choose(n_ranks, hand)
  tot <- choose(n_suits * n_ranks, hand)
  list(favorable = fav, total = tot, probability = fav / tot)
}

#' @rdname AtLeastOneOfIid
#' @export
BayesGeneral <- function(priors, likelihoods) {
  .mrn_prob(priors, "priors")
  .mrn_prob(likelihoods, "likelihoods")
  if (length(priors) != length(likelihoods)) stop("priors and likelihoods must have the same length")
  if (abs(sum(priors) - 1) > 1e-9) stop("priors must sum to 1")
  pz <- sum(priors * likelihoods)
  if (pz == 0) stop("P(Z) = 0: no hypothesis can produce Z")
  list(posteriors = priors * likelihoods / pz, p_z = pz)
}

#' @rdname AtLeastOneOfIid
#' @export
BayesSimple <- function(p_z_given_a, p_a, p_z) {
  .mrn_prob(c(p_z_given_a, p_a, p_z), "probabilities")
  if (p_z == 0) stop("P(Z) must be positive")
  list(posterior = p_z_given_a * p_a / p_z)
}

#' @rdname AtLeastOneOfIid
#' @export
ChainRule <- function(p_a, p_b_given_a, p_b, p_a_given_b) {
  .mrn_prob(c(p_a, p_b_given_a, p_b, p_a_given_b), "probabilities")
  list(via_a = p_a * p_b_given_a, via_b = p_b * p_a_given_b, p_and = p_a * p_b_given_a)
}

#' @rdname AtLeastOneOfIid
#' @export
EXSquared <- function(sigma, mu) {
  if (sigma < 0) stop("sigma must be >= 0")
  list(e_x2 = sigma^2 + mu^2)
}

#' @rdname AtLeastOneOfIid
#' @export
ExactHalfHeads <- function(n) {
  list(n = n, probability = choose(2 * n, n) / 4^n)
}

#' @rdname AtLeastOneOfIid
#' @export
ExcessScoreFactor <- function(r) {
  if (!(r > -1 && r < 1)) stop("r must be in (-1, 1)")
  list(factor = sqrt((1 - r) / (1 + r)))
}

#' @rdname AtLeastOneOfIid
#' @export
ExpectationLinear <- function(a, e_x, b, e_y, c) {
  list(expectation = a * e_x + b * e_y + c)
}

#' @rdname AtLeastOneOfIid
#' @export
ExponentialCrossingTime <- function(rate_fast = 0.2, rate_slow = 0.05, ratio = 4) {
  if (rate_fast <= rate_slow) stop("need rate_fast > rate_slow")
  if (ratio <= 0) stop("ratio must be > 0")
  list(t = log(ratio) / (rate_fast - rate_slow))
}

#' @rdname AtLeastOneOfIid
#' @export
ExponentialWaitingDensity <- function(t, lam) {
  if (t < 0 || lam <= 0) stop("need t >= 0 and lambda > 0")
  list(t = t, lambda = lam, density = lam * exp(-lam * t))
}

#' @rdname AtLeastOneOfIid
#' @export
MorinFactorial <- function(n) {
  if (n < 0 || n != round(n)) stop("n must be a non-negative integer")
  list(n = n, factorial = prod(seq_len(n)))
}

#' @rdname AtLeastOneOfIid
#' @export
InclusionExclusion3 <- function(p_a, p_b, p_c, p_ab, p_ac, p_bc, p_abc) {
  .mrn_prob(c(p_a, p_b, p_c, p_ab, p_ac, p_bc, p_abc), "probabilities")
  list(p_or = p_a + p_b + p_c - p_ab - p_ac - p_bc + p_abc)
}

#' @rdname AtLeastOneOfIid
#' @export
JointDensityFactorizes <- function(grid_x, density_x, grid_y, density_y) {
  if (length(grid_x) != length(density_x) || length(grid_y) != length(density_y)) stop("grid/density shape mismatch")
  trap <- function(g, d) sum(diff(g) * (d[-1] + d[-length(d)]) / 2)
  joint <- outer(density_x, density_y)
  inner <- apply(joint, 1, function(r) trap(grid_y, r))
  list(total_mass = trap(grid_x, inner), shape = dim(joint))
}

#' @rdname AtLeastOneOfIid
#' @export
JointIndependent <- function(joint, tol = 1e-9) {
  joint <- as.matrix(joint)
  if (any(joint < 0) || abs(sum(joint) - 1) > 1e-9) stop("joint must be a non-negative pmf table summing to 1")
  px <- rowSums(joint)
  py <- colSums(joint)
  list(independent = max(abs(joint - outer(px, py))) <= tol, marginal_x = unname(px), marginal_y = unname(py))
}

#' @rdname AtLeastOneOfIid
#' @export
PmfSd <- function(values, probs) {
  if (length(values) != length(probs) || abs(sum(probs) - 1) > 1e-9) stop("probs must match values and sum to 1")
  mu <- sum(values * probs)
  v <- sum(probs * (values - mu)^2)
  list(sd = sqrt(v), mean = mu, variance = sqrt(v) * sqrt(v))
}

#' @rdname AtLeastOneOfIid
#' @export
PmfSumConvolution <- function(values_x, probs_x, values_y, probs_y) {
  s <- as.vector(outer(values_x, values_y, "+"))
  p <- as.vector(outer(probs_x, probs_y))
  v <- sort(unique(s))
  list(values = v, probs = vapply(v, function(a) sum(p[s == a]), numeric(1)))
}

#' @rdname AtLeastOneOfIid
#' @export
PredictionImprovement <- function(r) {
  if (r < -1 || r > 1) stop("r must be in [-1, 1]")
  list(r = r, mse_fraction_remaining = 1 - r^2)
}

#' @rdname AtLeastOneOfIid
#' @export
ProbOrExclusive <- function(ps) {
  .mrn_prob(ps, "ps")
  s <- sum(ps)
  if (s > 1 + 1e-12) stop("exclusive probabilities sum past 1; events not exclusive")
  list(ps = ps, p_or = min(s, 1))
}

#' @rdname AtLeastOneOfIid
#' @export
ProbOrGeneral <- function(p_a, p_b, p_ab) {
  .mrn_prob(c(p_a, p_b, p_ab), "probabilities")
  if (p_ab > min(p_a, p_b) + 1e-12) stop("P(A and B) cannot exceed min(P(A), P(B))")
  list(p_a = p_a, p_b = p_b, p_ab = p_ab, p_or = p_a + p_b - p_ab)
}

#' @rdname AtLeastOneOfIid
#' @export
RegressionToMeanFactor <- function(r, y1) {
  if (r < -1 || r > 1) stop("r must be in [-1, 1]")
  list(yavg = r^2 * y1, factor = r^2)
}

#' @rdname AtLeastOneOfIid
#' @export
SdBernoulli <- function(p) {
  .mrn_prob(p, "p")
  list(p = p, sd = sqrt(p * (1 - p)))
}

#' @rdname AtLeastOneOfIid
#' @export
SdFairCoinAvg <- function(n) {
  if (n < 1) stop("n must be >= 1")
  list(n = n, sd_avg = 1 / (2 * sqrt(n)))
}

#' @rdname AtLeastOneOfIid
#' @export
SdOfMean <- function(sigma, n) {
  if (sigma < 0 || n < 1) stop("need sigma >= 0 and n >= 1")
  list(sigma = sigma, n = n, sd_mean = sigma / sqrt(n))
}

#' @rdname AtLeastOneOfIid
#' @export
SdOfMeanHetero <- function(sigmas) {
  if (length(sigmas) == 0 || any(sigmas < 0)) stop("sigmas must be a non-empty vector of >= 0 values")
  list(sd_avg = sqrt(sum(sigmas^2)) / length(sigmas), n = length(sigmas))
}

#' @rdname AtLeastOneOfIid
#' @export
SdScale <- function(a, sigma) {
  if (sigma < 0) stop("sigma must be >= 0")
  list(a = a, sigma = sigma, sd_aX = abs(a) * sigma)
}

#' @rdname AtLeastOneOfIid
#' @export
SlopeFromCov <- function(x, y) {
  if (length(x) != length(y) || length(x) < 2) stop("x and y must be equal-length vectors, n >= 2")
  vx <- mean((x - mean(x))^2)
  if (vx == 0) stop("zero variance in x")
  list(slope = mean((x - mean(x)) * (y - mean(y))) / vx)
}

#' @rdname AtLeastOneOfIid
#' @export
StarsAndBars <- function(n, N) {
  if (N < 1) stop("N must be >= 1")
  list(n = n, N = N, count = choose(n + N - 1, N - 1))
}

#' @rdname AtLeastOneOfIid
#' @export
SuitFullHouseProbability <- function(n_suits = 4, n_ranks = 13, k_major = 3, k_minor = 2) {
  fav <- n_suits * choose(n_ranks, k_major) * (n_suits - 1) * choose(n_ranks, k_minor)
  tot <- choose(n_suits * n_ranks, k_major + k_minor)
  list(favorable = fav, total = tot, probability = fav / tot)
}

#' @rdname AtLeastOneOfIid
#' @export
VarScale <- function(a, var_x) {
  if (var_x < 0) stop("variance must be >= 0")
  list(a = a, var_x = var_x, var_aX = a^2 * var_x)
}

#' @rdname AtLeastOneOfIid
#' @export
VarSumWithCov <- function(var_x, var_y, cov_xy = 0) {
  if (var_x < 0 || var_y < 0) stop("variances must be >= 0")
  if (abs(cov_xy) > sqrt(var_x * var_y) + 1e-12) stop("|Cov| cannot exceed sqrt(VarX VarY)")
  list(var_sum = var_x + var_y + 2 * cov_xy, cov_xy = cov_xy)
}
