% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_sschin_chained_imputation}
\alias{morie_sschin_chained_imputation}
\alias{morie_sschin}
\title{Multiple imputation by chained equations, a Cox}
\usage{
morie_sschin_chained_imputation(time, event, X, mi_iter = 5L, cycles = 10L, ties = "breslow")
}
\description{
R arm of sschin -- multiple imputation by chained equations, a Cox proportional-hazards fit per imputation and Rubin pooling.
}
\value{
A list with components \code{ncol}, \code{n}, \code{m}, \code{coefficients}, \code{variance}, \code{loglik}, \code{iterations}, \code{converged}, \code{drop}, \code{estimate}, \code{hazard_ratio}, \code{std_error}, \code{total_variance}, \code{within_variance}, \code{between_variance}, \code{ci_lower}, \code{ci_upper}, \code{t_quantile}, \code{df}, \code{relative_increase_variance}, \code{fraction_missing_info}, \code{per_imputation}, \code{complete_case_coefficients}, \code{complete_case_se}, \code{n_complete_cases}, \code{p}, \code{cycles}, \code{n_missing}, \code{columns_imputed}, \code{n_events}, \code{df_complete}, \code{method}, \code{note}.
}
\references{
van Buuren, S. (2018) Flexible Imputation of Missing Data, 2nd ed., CRC
Press, Ch. 3-4; Rubin, D. B. (1987) Multiple Imputation for Nonresponse in
Surveys, Wiley, Ch. 3; Barnard, J. & Rubin, D. B. (1999) Biometrika 86(4),
948-955; Breslow, N. E. (1974) Biometrics 30(1), 89-99.
}
\keyword{internal}
