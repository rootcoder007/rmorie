# Ghosal & van der Vaart (2017) shelf: R mirror of the morie Python
# core formulas (src/morie/fn/_bnp_core.py + gh_* modules).
# Certified equations: (3.2)-(3.3), (3.23), (4.10)-(4.12), Prop 4.8,
# Prop 4.20, Thm 4.25, Prop 4.10 (Ewens), PY EPPF (sec 14.4),
# eq (8.1), App B/G/J.

#' Stick-breaking weights from a vector of beta sticks
#'
#' Converts stick fractions \code{V} into the weights
#' \eqn{w_j = V_j \prod_{l < j} (1 - V_l)}. Only the first
#' \code{length(V)} weights are returned; the residual mass
#' \eqn{\prod_j (1 - V_j)} is not appended, so the result sums to one
#' only in the limit.
#'
#' @param V numeric vector of stick fractions, each in \eqn{\[0, 1\]};
#'   values outside the unit interval raise an error
#' @return numeric vector of the same length as \code{V} holding the
#'   stick-breaking weights
#' @references Ghosal, S. and van der Vaart, A. (2017).
#'   \emph{Fundamentals of Nonparametric Bayesian Inference}.
#'   Cambridge University Press.
#' @export
#' @examples
#' V <- c(1/2, 1/3, 1/4, 1/5)
#' morie_gh_stick_breaking(V)
morie_gh_stick_breaking <- function(V) {
  stopifnot(all(V >= 0 & V <= 1))
  out <- numeric(length(V))
  left <- 1
  for (j in seq_along(V)) {
    out[j] <- left * V[j]
    left <- left * (1 - V[j])
  }
  out
}

#' Discrete hazard rates of a probability vector
#'
#' Returns \eqn{h_j = p_j / (1 - \sum_{l < j} p_l)}, the conditional
#' probability of stopping at index \code{j} given survival past
#' \code{j - 1}. When the remaining mass has underflowed (below
#' \code{1e-300}) the hazard is set to 1 rather than dividing by zero.
#'
#' @param p numeric vector of probabilities over an ordered discrete
#'   support
#' @return numeric vector of the same length as \code{p} holding the
#'   discrete hazards
#' @references Ghosal, S. and van der Vaart, A. (2017).
#'   \emph{Fundamentals of Nonparametric Bayesian Inference}.
#'   Cambridge University Press.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_gh_discrete_hazard(V)
morie_gh_discrete_hazard <- function(p) {
  out <- numeric(length(p))
  cum <- 0
  for (j in seq_along(p)) {
    denom <- 1 - cum
    out[j] <- if (denom > 1e-300) p[j] / denom else 1
    cum <- cum + p[j]
  }
  out
}

#' Dirichlet-process posterior for the mass of a single set
#'
#' Given a Dirichlet process prior with precision \code{alpha} and base
#' measure value \code{G0_A} on a set A, and \code{n_in_A} of \code{n}
#' observations falling in A, returns the mean and variance of the
#' (beta) posterior of \eqn{G(A)}. The posterior mean is the precision-
#' weighted mixture of prior and empirical mass; the variance is that of
#' a beta with the updated precision \code{alpha + n}.
#'
#' @param G0_A prior base-measure mass \eqn{G_0(A)} of the set, in
#'   \eqn{\[0, 1\]}
#' @param alpha Dirichlet-process precision (positive)
#' @param n_in_A number of observations falling in A
#' @param n total number of observations; if zero the empirical mass is
#'   taken as 0
#' @return list with elements \code{mean} (posterior mean of
#'   \eqn{G(A)}), \code{var} (posterior variance) and \code{precision}
#'   (the updated precision \code{alpha + n})
#' @references Ghosal, S. and van der Vaart, A. (2017).
#'   \emph{Fundamentals of Nonparametric Bayesian Inference}.
#'   Cambridge University Press.
#' @export
#' @examples
#' morie_gh_dp_posterior(G0_A = c(1, 2, 3, 4, 5, 6, 7, 8), alpha = 0.5, n_in_A = c(1, 2, 3, 4, 5, 6, 7, 8), n = 5L)
morie_gh_dp_posterior <- function(G0_A, alpha, n_in_A, n) {
  pn <- if (n > 0) n_in_A / n else 0
  m <- alpha / (alpha + n) * G0_A + n / (alpha + n) * pn
  v <- m * (1 - m) / (1 + alpha + n)
  list(mean = m, var = v, precision = alpha + n)
}

#' Mean and variance of the number of distinct values in a DP sample
#'
#' For \code{n} draws from a Dirichlet process with precision
#' \code{alpha}, returns \eqn{E\[K_n\] = \sum_{i=1}^{n} \alpha /
#' (\alpha + i - 1)} and \eqn{Var\[K_n\] = \sum_{i=1}^{n} \alpha (i - 1) /
#' (\alpha + i - 1)^2}.
#'
#' @param n sample size (positive integer)
#' @param alpha Dirichlet-process precision (positive)
#' @return list with elements \code{mean} and \code{var}, the expected
#'   number of distinct values and its variance
#' @references Ghosal, S. and van der Vaart, A. (2017).
#'   \emph{Fundamentals of Nonparametric Bayesian Inference}.
#'   Cambridge University Press.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_gh_dp_ndistinct(V, V)
morie_gh_dp_ndistinct <- function(n, alpha) {
  i <- seq_len(n)
  list(
    mean = sum(alpha / (alpha + i - 1)),
    var = sum(alpha * (i - 1) / (alpha + i - 1)^2)
  )
}

#' Stick-breaking truncation level for a given residual tolerance
#'
#' Returns \eqn{2 + \alpha \log(1 / \varepsilon)}, the number of
#' stick-breaking atoms to retain so that the expected discarded tail
#' mass is of order \code{eps}. The value is not rounded.
#'
#' @param eps residual mass tolerance in \eqn{(0, 1)}
#' @param alpha Dirichlet-process precision (positive)
#' @return numeric scalar truncation level (not an integer)
#' @references Ghosal, S. and van der Vaart, A. (2017).
#'   \emph{Fundamentals of Nonparametric Bayesian Inference}.
#'   Cambridge University Press.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_gh_eps_dp_size(V, V)
morie_gh_eps_dp_size <- function(eps, alpha) {
  2 + alpha * (-log(eps))
}

#' Distribution function of the median of a Dirichlet process
#'
#' Evaluates \eqn{P(\mathrm{median}(G) \le x)} at the point where the
#' base measure has value \code{G_x}. Because \eqn{G(x)} is
#' \eqn{Beta(\alpha G_0(x), \alpha (1 - G_0(x)))} and the median of
#' \eqn{G} is at most \code{x} exactly when \eqn{G(x) \ge 1/2}, the
#' answer is the beta tail probability above \eqn{1/2}, computed here by
#' a midpoint rule with \code{n_grid} panels on \eqn{(1/2, 1)}.
#'
#' @param G_x base-measure value \eqn{G_0(x)} at the point of interest,
#'   in \eqn{(0, 1)}
#' @param alpha Dirichlet-process precision (positive)
#' @param n_grid number of midpoint quadrature panels on \eqn{(1/2, 1)};
#'   defaults to 4000
#' @return numeric scalar, the probability that the Dirichlet-process
#'   median does not exceed \code{x}
#' @references Ghosal, S. and van der Vaart, A. (2017).
#'   \emph{Fundamentals of Nonparametric Bayesian Inference}.
#'   Cambridge University Press.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_gh_dp_median_cdf(V, V)
morie_gh_dp_median_cdf <- function(G_x, alpha, n_grid = 4000L) {
  a <- alpha * G_x
  b <- alpha * (1 - G_x)
  h <- 0.5 / n_grid
  u <- 0.5 + (seq_len(n_grid) - 0.5) * h
  u <- u[u < 1]
  sum(exp(lgamma(a + b) - lgamma(a) - lgamma(b) +
    (a - 1) * log(u) + (b - 1) * log(1 - u))) * h
}

#' Log Ewens sampling formula for a partition given by multiplicities
#'
#' Computes \eqn{\log\{n! / (\alpha)_n \prod_i \alpha^{m_i} /
#' (i^{m_i} m_i!)\}}, where \code{multiplicities\[i\]} is the number of
#' blocks of size \code{i} and the sample size \eqn{n = \sum_i i m_i} is
#' recovered from the multiplicities.
#'
#' @param multiplicities numeric vector whose \code{i}-th entry is the
#'   number of blocks of size \code{i}
#' @param alpha Dirichlet-process precision (positive)
#' @return numeric scalar log-probability of the partition
#' @references Ghosal, S. and van der Vaart, A. (2017).
#'   \emph{Fundamentals of Nonparametric Bayesian Inference}.
#'   Cambridge University Press.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_gh_ewens_log(V, V)
morie_gh_ewens_log <- function(multiplicities, alpha) {
  m <- as.numeric(multiplicities)
  n <- sum(seq_along(m) * m)
  lp <- lgamma(n + 1) - sum(log(alpha + seq_len(n) - 1))
  for (i in seq_along(m)) {
    lp <- lp + m[i] * log(alpha) - m[i] * log(i) - lgamma(m[i] + 1)
  }
  lp
}

#' Log exchangeable partition probability function of a Pitman-Yor process
#'
#' Evaluates the log EPPF
#' \eqn{\log\{\prod_{j=1}^{k-1} (\theta + j d) / (\theta + 1)_{n-1}
#' \prod_j (1 - d)_{n_j - 1}\}} for a partition of \eqn{n = \sum_j n_j}
#' items into \eqn{k} blocks of the given sizes. The blocks are
#' exchangeable, so only the multiset of \code{sizes} matters.
#'
#' @param sizes numeric vector of block sizes of the partition
#' @param d discount parameter, typically in \eqn{[0, 1)}
#' @param theta concentration parameter, typically \eqn{> -d}
#' @return numeric scalar log-probability of the partition
#' @references Ghosal, S. and van der Vaart, A. (2017).
#'   \emph{Fundamentals of Nonparametric Bayesian Inference}.
#'   Cambridge University Press.
#' @export
#' @examples
#' morie_gh_py_eppf_log(sizes = c(1, 2, 3, 4, 5, 6, 7, 8), d = 5L, theta = 0.5)
morie_gh_py_eppf_log <- function(sizes, d, theta) {
  n <- sum(sizes)
  k <- length(sizes)
  lp <- 0
  if (k > 1) for (j in seq_len(k - 1)) lp <- lp + log(theta + j * d)
  for (i in seq_len(n - 1)) lp <- lp - log(theta + i)
  for (nj in sizes) {
    if (nj > 1) for (l in 0:(nj - 2)) lp <- lp + log(1 - d + l)
  }
  lp
}

#' Conjugate posterior for one white-noise (normal mean) coordinate
#'
#' For a single coordinate of the Gaussian white-noise model with unit
#' noise variance and a mean-zero normal prior of variance
#' \code{prior_var}, returns the posterior mean \eqn{n X / (n + 1 /
#' \lambda)} and variance \eqn{1 / (n + 1 / \lambda)}.
#'
#' @param X observed coordinate value (the sufficient statistic, scaled
#'   as a mean)
#' @param n effective sample size / noise precision multiplier
#' @param prior_var prior variance \eqn{\lambda} of the coordinate
#'   (positive)
#' @return list with elements \code{mean} and \code{var}, the posterior
#'   mean and variance of the coordinate
#' @references Ghosal, S. and van der Vaart, A. (2017).
#'   \emph{Fundamentals of Nonparametric Bayesian Inference}.
#'   Cambridge University Press.
#' @export
#' @examples
#' morie_gh_wn_posterior(X = c(1, 2, 3, 4, 5, 6, 7, 8), n = 5L, prior_var = c(1, 2, 3, 4, 5, 6, 7, 8))
morie_gh_wn_posterior <- function(X, n, prior_var) {
  lam <- prior_var
  list(mean = n * X / (n + 1 / lam), var = 1 / (n + 1 / lam))
}

#' Leading binary-expansion bits of a point in the unit interval
#'
#' Returns the first \code{depth} bits of the binary expansion of
#' \code{x}, which index the nested dyadic Polya-tree partition
#' containing \code{x}. The input is first clamped to
#' \eqn{\[0, 1 - 10^{-15}\]}, so values outside the unit interval are
#' silently pulled to the nearest end.
#'
#' @param x numeric scalar in \eqn{[0, 1)}
#' @param depth number of bits (Polya-tree levels) to return
#' @return integer vector of length \code{depth} of 0/1 bits, most
#'   significant first
#' @export
#' @examples
#' morie_gh_pt_bits(x = c(1, 2, 3, 4, 5, 6, 7, 8), depth = 5L)
morie_gh_pt_bits <- function(x, depth) {
  out <- integer(depth)
  v <- min(max(x, 0), 1 - 1e-15)
  for (m in seq_len(depth)) {
    v <- v * 2
    out[m] <- as.integer(v >= 1)
    v <- v - out[m]
  }
  out
}

#' Polya-tree posterior density at a point
#'
#' Locates \code{x} in the dyadic partition, counts how many of
#' \code{data} share each of its first \code{depth} bit prefixes, and
#' multiplies the level-wise posterior branching probabilities
#' \eqn{(2 a_m + 2 N_m) / (2 a_m + N_{m-1})}, with the canonical
#' level weights \eqn{a_m = m^2} and \eqn{N_0} the sample size. The
#' result is the posterior density relative to the uniform base measure.
#'
#' @param x numeric scalar in \eqn{[0, 1)} at which to evaluate the
#'   density
#' @param data numeric vector of observations in \eqn{[0, 1)}
#' @param depth number of Polya-tree levels to use; defaults to 4
#' @return numeric scalar posterior density at \code{x}
#' @references Ghosal, S. and van der Vaart, A. (2017).
#'   \emph{Fundamentals of Nonparametric Bayesian Inference}.
#'   Cambridge University Press.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_gh_pt_posterior_density(V, V)
morie_gh_pt_posterior_density <- function(x, data, depth = 4L) {
  bx <- morie_gh_pt_bits(x, depth)
  n <- length(data)
  counts <- integer(depth)
  for (m in seq_len(depth)) {
    c_ <- 0L
    for (d_ in data) {
      if (identical(morie_gh_pt_bits(d_, m), bx[seq_len(m)])) {
        c_ <- c_ + 1L
      }
    }
    counts[m] <- c_
  }
  dens <- 1
  for (m in seq_len(depth)) {
    a <- m * m
    N_here <- counts[m]
    N_parent <- if (m >= 2) counts[m - 1] else n
    dens <- dens * (2 * a + 2 * N_here) / (2 * a + N_parent)
  }
  dens
}

#' Half the squared Hellinger distance between two discrete distributions
#'
#' Renormalises both inputs to sum to one and returns
#' \eqn{1 - \sum_i \sqrt{p_i q_i}}, which is one half of the squared
#' Hellinger distance \eqn{h^2(p, q) = \sum_i (\sqrt{p_i} -
#' \sqrt{q_i})^2} and lies in \eqn{\[0, 1\]}.
#'
#' @param p numeric vector of non-negative weights; renormalised
#'   internally
#' @param q numeric vector of non-negative weights on the same support;
#'   renormalised internally
#' @return numeric scalar in \eqn{\[0, 1\]}
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_gh_hellinger2(V, V)
morie_gh_hellinger2 <- function(p, q) {
  p <- p / sum(p)
  q <- q / sum(q)
  1 - sum(sqrt(p * q))
}

#' Kullback-Leibler divergence between two discrete distributions
#'
#' Renormalises both inputs to sum to one and returns
#' \eqn{\sum_i p_i \log(p_i / q_i)} in nats, restricted to the support
#' of \code{p}. Zeros in \code{q} are floored at \code{1e-300} rather
#' than returning \code{Inf}, so the result is always finite.
#'
#' @param p numeric vector of non-negative weights (the reference
#'   measure); renormalised internally
#' @param q numeric vector of non-negative weights on the same support;
#'   renormalised internally
#' @return numeric scalar divergence in nats
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_gh_kl(V, V)
morie_gh_kl <- function(p, q) {
  p <- p / sum(p)
  q <- q / sum(q)
  keep <- p > 0
  sum(p[keep] * log(p[keep] / pmax(q[keep], 1e-300)))
}

#' Renyi divergence of order alpha between two discrete distributions
#'
#' Renormalises both inputs to sum to one and returns
#' \eqn{(\alpha - 1)^{-1} \log \sum_i p_i^{\alpha} q_i^{1 - \alpha}}.
#' The order \code{alpha} must differ from 1; no check is performed.
#'
#' @param p numeric vector of non-negative weights; renormalised
#'   internally
#' @param q numeric vector of non-negative weights on the same support;
#'   renormalised internally
#' @param alpha order of the divergence, not equal to 1; defaults to 0.5
#' @return numeric scalar divergence in nats
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_gh_renyi(V, V)
morie_gh_renyi <- function(p, q, alpha = 0.5) {
  p <- p / sum(p)
  q <- q / sum(q)
  log(sum(p^alpha * q^(1 - alpha))) / (alpha - 1)
}

#' First two moments of a Dirichlet distribution for a pair of cells
#'
#' For \eqn{(\pi_1, \ldots, \pi_k) \sim Dir(\alpha)} with
#' \eqn{A = \sum_i \alpha_i}, returns \eqn{E\[\pi_j\]},
#' \eqn{Var\[\pi_j\]} and \eqn{Cov\[\pi_j, \pi_{j'}\]}. The covariance is
#' computed from the formula for distinct cells; passing
#' \code{j == jp} therefore does not reproduce the variance.
#'
#' @param alpha numeric vector of Dirichlet concentration parameters
#' @param j index of the cell whose mean and variance are wanted
#' @param jp index of the second cell for the covariance
#' @return list with elements \code{mean}, \code{var} (both for cell
#'   \code{j}) and \code{cov} (between cells \code{j} and \code{jp})
#' @references Ghosal, S. and van der Vaart, A. (2017).
#'   \emph{Fundamentals of Nonparametric Bayesian Inference}.
#'   Cambridge University Press.
#' @export
#' @examples
#' morie_gh_dirichlet_moments(alpha = 0.5, j = c(1, 2, 3, 4, 5, 6, 7, 8), jp = c(1, 2, 3, 4, 5, 6, 7, 8))
morie_gh_dirichlet_moments <- function(alpha, j, jp) {
  A <- sum(alpha)
  vr <- alpha[j] * (A - alpha[j]) / (A^2 * (A + 1))
  # The off-diagonal formula was applied unconditionally, so asking for
  # j == jp returned -alpha_j^2 / (A^2 (A+1)) -- a NEGATIVE number that
  # disagreed with the `var` field returned beside it. The covariance of
  # a coordinate with itself is its variance.
  cv <- if (identical(as.integer(j), as.integer(jp))) {
    vr
  } else {
    -alpha[j] * alpha[jp] / (A^2 * (A + 1))
  }
  list(mean = alpha[j] / A, var = vr, cov = cv)
}

#' Laplace exponent of a gamma completely random measure
#'
#' Returns \eqn{a \log(1 + f)}, the Laplace exponent of a gamma process
#' with unit rate evaluated at the test function value \code{f}; the
#' Laplace transform itself is \eqn{\exp(-a \log(1 + f))}.
#'
#' @param f non-negative test-function value (or vector of values)
#' @param a base-measure mass of the gamma completely random measure
#' @return numeric vector of Laplace exponents, matching the length of
#'   \code{f}
#' @references Ghosal, S. and van der Vaart, A. (2017).
#'   \emph{Fundamentals of Nonparametric Bayesian Inference}.
#'   Cambridge University Press.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_gh_crm_laplace_gamma(V, V)
morie_gh_crm_laplace_gamma <- function(f, a) {
  a * log(1 + f)
}

#' Laplace transform of a discretised completely random measure
#'
#' Returns \eqn{\exp\{-\sum_i m_i (1 - e^{-f_i u})\}}, the Laplace
#' transform of a completely random measure whose Levy intensity has
#' been discretised into atoms of mass \code{m} at the test-function
#' values \code{f}, evaluated at the argument \code{u}.
#'
#' @param f numeric vector of test-function values at the discretisation
#'   atoms
#' @param m numeric vector of atom masses, same length as \code{f}
#' @param u numeric scalar argument of the Laplace transform
#' @return numeric scalar value of the Laplace transform
#' @references Ghosal, S. and van der Vaart, A. (2017).
#'   \emph{Fundamentals of Nonparametric Bayesian Inference}.
#'   Cambridge University Press.
#' @export
#' @examples
#' morie_gh_ncrm_laplace(f = c(1, 2, 3, 4, 5, 6, 7, 8), m = 5L, u = c(1, 2, 3, 4, 5, 6, 7, 8))
morie_gh_ncrm_laplace <- function(f, m, u) {
  exp(-sum(m * (1 - exp(-f * u))))
}

#' Expected number of features in an Indian buffet process
#'
#' Returns \eqn{\alpha H_n = \alpha \sum_{i=1}^{n} 1 / i}, the expected
#' number of distinct features ("dishes") sampled by \code{n} customers
#' in a one-parameter Indian buffet process.
#'
#' @param n number of customers (positive integer)
#' @param alpha mass parameter of the Indian buffet process (positive)
#' @return numeric scalar expected number of distinct features
#' @references Ghosal, S. and van der Vaart, A. (2017).
#'   \emph{Fundamentals of Nonparametric Bayesian Inference}.
#'   Cambridge University Press.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_gh_ibp_expected_dishes(V, V)
morie_gh_ibp_expected_dishes <- function(n, alpha) {
  alpha * sum(1 / seq_len(n))
}

#' Polya-urn predictive weights of a Dirichlet process
#'
#' After \code{n} observations, the next draw is a fresh value from the
#' base measure with probability \eqn{\alpha / (\alpha + n)} and equals
#' any given past observation with probability \eqn{1 / (\alpha + n)}.
#'
#' @param alpha Dirichlet-process precision (positive)
#' @param n number of observations already drawn
#' @return list with elements \code{weight_fresh} (probability of a draw
#'   from the base measure) and \code{weight_per_obs} (probability
#'   attached to each individual past observation)
#' @references Ghosal, S. and van der Vaart, A. (2017).
#'   \emph{Fundamentals of Nonparametric Bayesian Inference}.
#'   Cambridge University Press.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_gh_dp_predictive(V, V)
morie_gh_dp_predictive <- function(alpha, n) {
  list(
    weight_fresh = alpha / (alpha + n),
    weight_per_obs = 1 / (alpha + n)
  )
}
