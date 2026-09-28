#' Closed-form results from Grimmett and Stirzaker
#'
#' \code{ErlangRenewal}: renewal function of gamma (lam, k) interarrivals,
#' \code{BerksonCorrelation}: Berkson's fallacy correlation,
#' \code{MillsConditionalMean}: E(Y | X > x) for a standard bivariate normal,
#' \code{BivNormDependence}: mean-square contingency and mutual information,
#' \code{MaximalCorrelation}: Renyi maximal correlation of a discrete pair,
#' \code{BetaBinomialPmf}: beta-binomial (negative hypergeometric) law,
#' \code{ArSpectralDensity} and \code{AcfSpectralDensity}: spectral densities,
#' \code{TelegraphProcess}: two-state Markov autocorrelation and spectrum,
#' \code{WienerConditionalCorrelation}: Brownian-bridge correlation,
#' \code{SimpleEpidemicDuration}: moments of the time to total infection,
#' \code{StirlingGammaRatio}: Stirling's ratio, and \code{MaAutocorrelation}:
#' moving-average autocorrelation. Identical to the Python arm
#' \code{morie.fn.probresults}.
#'
#' @param t,s,u,v Times.
#' @param lam Rate, or frequency for spectral densities.
#' @param k Erlang shape, or number of successes for \code{BetaBinomialPmf}.
#' @param gamma,a,c Probabilities of disease, other admission and admission for the disease.
#' @param x Threshold.
#' @param rho Correlation, or vector of positive-lag autocorrelations.
#' @param table Joint counts or probabilities (rows X, columns Y).
#' @param n Number of trials.
#' @param b Second beta parameter (with \code{a} the first).
#' @param alpha AR coefficients, or first telegraph rate.
#' @param beta Second telegraph rate.
#' @param sigma2 Innovation variance.
#' @param normalized Divide by the variance so the density integrates to one.
#' @param ngrid Trapezoid points for the variance.
#' @param N Number initially susceptible.
#' @param theta MA coefficients.
#' @param max_lag Largest lag.
#' @return Numeric value, vector, or list.
#' @references Grimmett, G. R. and Stirzaker, D. R. (2020). Probability and
#'   Random Processes, 4th edn. Oxford University Press.
#'
#'   Renyi, A. (1959). On measures of dependence. Acta Mathematica Academiae
#'   Scientiarum Hungaricae 10, 441-451.
#' @examples
#' ErlangRenewal(1, 2)
#' BerksonCorrelation(0.1, 0.2, 0.5)
#' MaximalCorrelation(matrix(c(0.3, 0.1, 0.2, 0.4), 2))$m
#' MaAutocorrelation(0.5, 2)
#' @export
ErlangRenewal <- function(t, lam, k = 2L) {
  if (lam <= 0 || k < 1 || t < 0) stop("need lam > 0, integer k >= 1 and t >= 0")
  s <- 0
  if (k > 1) {
    for (j in seq_len(k - 1)) {
      w <- exp(2i * pi * j / k)
      s <- s + Re(w / (1 - w) * (1 - exp(-lam * (1 - w) * t)))
    }
  }
  lam * t / k + s / k
}

#' @rdname ErlangRenewal
#' @export
BerksonCorrelation <- function(gamma, a, c) {
  vals <- c(gamma, a, c)
  if (any(vals <= 0 | vals >= 1)) stop("gamma, a and c must lie in (0, 1)")
  p <- a + c - a * c
  sqrt(gamma * p / (1 - gamma * p) * (1 - a) * (1 - gamma * c) / (a + gamma * c - a * gamma * c))
}

#' @rdname ErlangRenewal
#' @export
MillsConditionalMean <- function(x, rho) {
  if (rho < -1 || rho > 1) stop("rho must lie in [-1, 1]")
  phi <- exp(-0.5 * x * x) / sqrt(2 * pi)
  rho * phi / pnorm(-x)
}

#' @rdname ErlangRenewal
#' @export
BivNormDependence <- function(rho) {
  if (rho <= -1 || rho >= 1) stop("rho must lie in (-1, 1)")
  list(phi2 = rho * rho / (1 - rho * rho), mutual_information = -0.5 * log(1 - rho * rho))
}

.pr_ssum <- function(v) {
  s <- 0
  for (a in v) s <- s + a
  s
}

#' @rdname ErlangRenewal
#' @export
MaximalCorrelation <- function(table) {
  P <- as.matrix(table) + 0
  P <- P / .pr_ssum(t(P))
  r <- nrow(P)
  cc <- ncol(P)
  pr <- vapply(seq_len(r), function(i) .pr_ssum(P[i, ]), 0)
  pc <- vapply(seq_len(cc), function(j) .pr_ssum(P[, j]), 0)
  if (min(pr) <= 0 || min(pc) <= 0) stop("every row and column must have positive mass")
  Q <- P / sqrt(outer(pr, pc))
  QQt <- matrix(0, r, r)
  for (i in seq_len(r)) for (j in seq_len(r)) QQt[i, j] <- .pr_ssum(Q[i, ] * Q[j, ])
  if (r < 2) return(list(m = 0, f = 0, g = rep(0, cc)))
  e <- .s03jacobi(QQt)
  m <- sqrt(max(e$values[r - 1], 0))
  u <- e$vectors[, r - 1]
  f <- u / sqrt(pr)
  g <- vapply(seq_len(cc), function(j) if (m > 0) .pr_ssum(Q[, j] * u) / (m * sqrt(pc[j])) else 0, 0)
  list(m = m, f = f, g = g)
}

#' @rdname ErlangRenewal
#' @export
BetaBinomialPmf <- function(k, n, a, b) {
  if (a <= 0 || b <= 0 || k < 0 || k > n) stop("need a, b > 0 and 0 <= k <= n")
  exp(lgamma(n + 1) - lgamma(k + 1) - lgamma(n - k + 1) + lgamma(a + k) + lgamma(n + b - k) -
        lgamma(n + a + b) + lgamma(a + b) - lgamma(a) - lgamma(b))
}

.pr_ar_unnorm <- function(lam, alpha, sigma2) {
  z <- complex(real = 1, imaginary = 0)
  for (j in seq_along(alpha)) z <- z - alpha[j] * exp(-1i * j * lam)
  sigma2 / (2 * pi * (Re(z) * Re(z) + Im(z) * Im(z)))
}

#' @rdname ErlangRenewal
#' @export
ArSpectralDensity <- function(lam, alpha, sigma2 = 1, normalized = TRUE, ngrid = 4096L) {
  alpha <- as.numeric(alpha)
  f <- .pr_ar_unnorm(lam, alpha, sigma2)
  if (!normalized) return(f)
  h <- 2 * pi / ngrid
  c0 <- 0
  for (i in seq_len(ngrid) - 1) c0 <- c0 + .pr_ar_unnorm(-pi + i * h, alpha, sigma2)
  f / (c0 * h)
}

#' @rdname ErlangRenewal
#' @export
AcfSpectralDensity <- function(lam, rho) {
  s <- 1
  for (n in seq_along(rho)) s <- s + 2 * rho[n] * cos(n * lam)
  s / (2 * pi)
}

#' @rdname ErlangRenewal
#' @export
TelegraphProcess <- function(t, lam, alpha, beta) {
  if (alpha <= 0 || beta <= 0) stop("rates must be positive")
  s <- alpha + beta
  list(rho = exp(-abs(t) * s), f = s / (pi * (s * s + lam * lam)))
}

#' @rdname ErlangRenewal
#' @export
WienerConditionalCorrelation <- function(s, t, u, v) {
  if (!(s < t && t < u && u < v)) stop("need s < t < u < v")
  sqrt((v - u) * (t - s) / ((v - t) * (u - s)))
}

#' @rdname ErlangRenewal
#' @export
SimpleEpidemicDuration <- function(N, lam) {
  if (N < 1 || lam <= 0) stop("need N >= 1 and lam > 0")
  rates <- lam * seq_len(N) * (N + 1 - seq_len(N))
  list(mean = .pr_ssum(1 / rates), variance = .pr_ssum(1 / (rates * rates)))
}

#' @rdname ErlangRenewal
#' @export
StirlingGammaRatio <- function(t) {
  if (t <= 0) stop("t must be positive")
  exp(lgamma(t) - (t - 0.5) * log(t) + t - 0.5 * log(2 * pi))
}

#' @rdname ErlangRenewal
#' @export
MaAutocorrelation <- function(theta, max_lag) {
  th <- c(1, as.numeric(theta))
  q <- length(th) - 1
  den <- .pr_ssum(th * th)
  vapply(0:max_lag, function(k) if (k <= q) .pr_ssum(th[1:(q - k + 1)] * th[(1 + k):(q + 1)]) / den else 0, 0)
}
