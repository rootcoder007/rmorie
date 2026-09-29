#' Posterior and bootstrap summaries
#'
#' R arm of \code{morie.fn.bamse}, \code{bypvl} and \code{nocse}. \code{Bamse}:
#' posterior standard deviations (n - 1 denominator) of each column of an MCMC
#' chain. \code{Bypvl}: posterior predictive p-value, the share of replicated
#' test statistics at or above the observed one. \code{Nocse}: normal
#' confidence half-widths z times the bootstrap standard errors of NOMINATE
#' estimates.
#'
#' @param chain MCMC draws (rows) by parameter (columns), or replicated statistics.
#' @param test_stat Observed test statistic.
#' @param boot_se Bootstrap standard errors.
#' @param alpha Error rate.
#' @return A named list (the Python result's fields).
#' @references Gelman, A., Meng, X.-L. and Stern, H. (1996). Posterior predictive assessment of model fitness via realized discrepancies. Statistica Sinica 6, 733-807.
#'
#'   Poole, K. T. and Rosenthal, H. (1997). Congress: A Political-Economic History of Roll Call Voting. Oxford University Press.
#' @examples
#' Bamse(cbind(c(1, 2, 4), c(5, 3, 4)))$ses
#' Bypvl(c(0.2, 1.5, 0.9, 2.2, 1.1), 1)$value
#' @export
Bamse <- function(chain) {
  chain <- as.matrix(chain)
  if (ncol(chain) == 1 && nrow(chain) == 1) chain <- t(chain)
  ses <- apply(chain, 2, stats::sd)
  list(value = ses[1], ses = unname(ses), n_samples = nrow(chain), n_params = ncol(chain))
}

#' @rdname Bamse
#' @export
Bypvl <- function(chain, test_stat) {
  chain <- as.numeric(chain)
  bp <- mean(chain >= test_stat)
  list(value = bp, bayesian_p = bp, test_stat = test_stat, n_samples = length(chain), chain_mean = mean(chain))
}

#' @rdname Bamse
#' @export
Nocse <- function(boot_se, alpha = 0.05) {
  z <- stats::qnorm(1 - alpha / 2)
  list(value = z, z_critical = z, alpha = alpha, ci_half_widths = z * as.numeric(boot_se),
       mean_se = mean(boot_se), n_params = length(boot_se))
}
