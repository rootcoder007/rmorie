.bloo_lse <- function(v) {
  m <- max(v)
  if (m == -Inf) return(-Inf)
  m + log(sum(exp(v - m)))
}

.bloo_gpdfit <- function(x) {
  N <- length(x)
  M <- 30 + floor(sqrt(N))
  xstar <- x[floor(N / 4 + 0.5)]
  if (!(xstar > x[1])) return(list(k = NaN, sigma = NaN))
  theta <- 1 / x[N] + (1 - sqrt(M / (seq_len(M) - 0.5))) / 3 / xstar
  lt <- numeric(M)
  for (j in seq_len(M)) {
    kk <- sum(log1p(-theta[j] * x)) / N
    if (kk == 0 || !is.finite(kk) || -theta[j] / kk <= 0) return(list(k = Inf, sigma = NaN))
    lt[j] <- N * (log(-theta[j] / kk) - kk - 1)
  }
  th <- sum(theta * exp(lt - .bloo_lse(lt)))
  k <- sum(log1p(-th * x)) / N
  sigma <- -k / th
  k <- (k * N + 0.5 * 10) / (N + 10)
  if (is.nan(k)) return(list(k = Inf, sigma = NaN))
  list(k = k, sigma = sigma)
}

.bloo_psis <- function(lr) {
  S <- length(lr)
  mx <- max(lr)
  lw <- lr - mx
  M <- ceiling(min(0.2 * S, 3 * sqrt(S)))
  k <- Inf
  if (M >= 5) {
    o <- order(lw, seq_along(lw))
    tail <- o[(S - M + 1):S]
    cut <- lw[o[S - M]]
    if (abs(lw[tail[M]] - lw[tail[1]]) >= .Machine$double.eps / 100) {
      ecut <- exp(cut)
      g <- .bloo_gpdfit(exp(lw[tail]) - ecut)
      k <- g$k
      if (is.finite(g$k)) {
        p <- (seq_len(M) - 0.5) / M
        q <- if (g$k != 0) g$sigma * expm1(-g$k * log1p(-p)) / g$k else -g$sigma * log1p(-p)
        lw[tail] <- log(q + ecut)
      }
    }
  }
  list(lw = pmin(lw, 0) + mx, k = k)
}

#' PSIS leave-one-out cross-validation
#'
#' Pareto-smoothed importance-sampling LOO: for each observation the
#' largest \eqn{M = \lceil \min(0.2 S, 3 \sqrt S) \rceil} importance ratios
#' \eqn{1 / p(y_i \mid \theta_s)} are replaced by expected order statistics
#' of a generalised Pareto fitted to their exceedances (Zhang-Stephens fit,
#' weakly informative prior on k), the weights are truncated at the largest
#' raw ratio, and \eqn{elpd_i = \log \sum_s w_s p(y_i \mid \theta_s) /
#' \sum_s w_s}. Reproduces \code{loo::loo} with \code{r_eff = 1}. Identical to
#' the Python arm \code{morie.fn.bloos.psis_loo}.
#'
#' @param log_lik_matrix Draws by observations matrix of pointwise
#'   log-likelihoods.
#' @return A list with \code{elpd_loo}, \code{p_loo}, \code{looic}, \code{se},
#'   \code{k_hat}, \code{n_high_k} (k above 0.7), \code{n_obs} and
#'   \code{elpd_loo_pointwise}.
#' @references Vehtari, A., Gelman, A. and Gabry, J. (2017). Practical
#'   Bayesian model evaluation using leave-one-out cross-validation and WAIC.
#'   Statistics and Computing 27, 1413-1432.
#'
#'   Vehtari, A., Simpson, D., Gelman, A., Yao, Y. and Gabry, J. (2024).
#'   Pareto smoothed importance sampling. Journal of Machine Learning Research
#'   25(72), 1-58.
#' @examples
#' ll <- matrix(dnorm(rep(c(0.2, -1, 1.5), each = 40),
#'   mean = rep(seq(-0.5, 0.5, length.out = 40), 3), log = TRUE), 40, 3)
#' psis_loo(ll)$elpd_loo
#' @export
psis_loo <- function(log_lik_matrix) {
  L <- log_lik_matrix
  if (is.null(dim(L))) L <- matrix(as.numeric(L), nrow = 1)
  L <- as.matrix(L)
  S <- nrow(L)
  n <- ncol(L)
  elpd_i <- lppd_i <- k_hat <- numeric(n)
  for (i in seq_len(n)) {
    ll <- L[, i]
    ps <- .bloo_psis(-ll)
    k_hat[i] <- ps$k
    elpd_i[i] <- .bloo_lse(ps$lw + ll) - .bloo_lse(ps$lw)
    lppd_i[i] <- .bloo_lse(ll) - log(S)
  }
  elpd <- sum(elpd_i)
  se <- if (n > 1) sqrt(n * sum((elpd_i - elpd / n)^2) / (n - 1)) else NaN
  list(elpd_loo = elpd, p_loo = sum(lppd_i) - elpd, looic = -2 * elpd, se = se,
       k_hat = k_hat, n_high_k = sum(!(k_hat <= 0.7)), n_obs = n,
       elpd_loo_pointwise = elpd_i)
}
