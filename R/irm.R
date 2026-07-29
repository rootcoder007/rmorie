#' Estimate the ATE via the Interactive Regression Model (IRM)
#'
#' Native implementation of the Chernozhukov et al. (2018) interactive
#' regression model: cross-fit logistic propensity scores and GCV-ridge
#' outcome regressions, combined through the doubly-robust (AIPW) score.
#' Mirrors the Python sibling `morie.estimate_irm()`.
#'
#' Nothing beyond `stats` is required at runtime. The estimator is
#' cross-validated against \pkg{DoubleML} in the package's cross tests,
#' but \pkg{DoubleML}, \pkg{mlr3}, and \pkg{mlr3learners} are not
#' loaded or called.
#'
#' Following Chernozhukov et al. (2018), the IRM extends the partially linear
#' model by allowing fully heterogeneous treatment effects:
#' \deqn{Y = g_0(T, X) + U,\quad E\[U|T,X\] = 0}{Y = g_0(T, X) + U, E\[U|T,X\] = 0}
#' \deqn{T = m_0(X) + V,\quad E\[V|X\] = 0}{T = m_0(X) + V, E\[V|X\] = 0}
#'
#' @param data A `data.frame` containing outcome, treatment, and covariates.
#' @param treatment Column name of the binary treatment.
#' @param outcome Column name of the outcome.
#' @param covariates Character vector of covariate column names.
#' @param n_folds Number of cross-fitting folds (default 5).
#' @param random_state Random seed (default 42).
#'
#' @return A list with components: `ate`, `se`, `ci_lower`, `ci_upper`,
#'   `n`, `method` (`"IRM (DoubleML)"`).
#'
#' @section CRAN \code{Suggests}:
#' Requires the suggested packages `DoubleML`, `mlr3`, and `mlr3learners`.
#' Install with `install.packages(c("DoubleML", "mlr3", "mlr3learners"))`.
#' If any are unavailable, the function raises an informative error.
#'
#' @references
#' Chernozhukov, V., Chetverikov, D., Demirer, M., Duflo, E., Hansen, C.,
#' Newey, W., & Robins, J. (2018). Double/debiased machine learning for
#' treatment and structural parameters. \emph{The Econometrics Journal}, 21(1),
#' C1--C68. \doi{10.1111/ectj.12097}
#'
#' Bach, P., Chernozhukov, V., Kurz, M. S., & Spindler, M. (2024). DoubleML --
#' An object-oriented implementation of double machine learning in R.
#' \emph{Journal of Statistical Software}, 108(3). \doi{10.18637/jss.v108.i03}
#'
#' @export
#' @examples
#' set.seed(1)
#' n <- 200
#' X <- matrix(rnorm(n * 5), n, 5)
#' ps <- plogis(X\[, 1\] - X\[, 2\])
#' T <- rbinom(n, 1, ps)
#' Y <- 0.5 * T + X\[, 1\] + rnorm(n)
#' df <- data.frame(Y = Y, T = T, X)
#' morie_estimate_irm(df,
#'   treatment = "T", outcome = "Y",
#'   covariates = paste0("X", 1:5)
#' )
morie_estimate_irm <- function(data, treatment, outcome, covariates,
                               n_folds = 5L, random_state = 42L) {
  prep <- .dml_prepare_xy(data, treatment, outcome, covariates)
  n <- nrow(prep$frame)
  z <- 1.959964
  out <- .morie_dml_irm_native(prep$X, prep$y, prep$d,
                               n_folds = n_folds,
                               random_state = random_state)
  list(
    ate = out$theta, se = out$se,
    ci_lower = out$theta - z * out$se,
    ci_upper = out$theta + z * out$se,
    n = n, method = "IRM (morie native)"
  )
}
