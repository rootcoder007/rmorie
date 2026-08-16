% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sschin_chained_imputation}
\alias{morie_sschin_chained_imputation}
\title{morie_sschin_chained_imputation}
\usage{
morie_sschin_chained_imputation(time, event, X, mi_iter = 5L, cycles = 10L, ties = "breslow")
}
\arguments{
\item{time}{See Usage.}
\item{event}{See Usage.}
\item{X}{See Usage.}
\item{mi_iter}{Defaults to \code{5L}.}
\item{cycles}{Defaults to \code{10L}.}
\item{ties}{Defaults to \code{"breslow"}.}
}
\description{
Part of the sschin_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{coefficients}, \code{hazard_ratio}, \code{std_error}, \code{total_variance}, \code{within_variance}, \code{between_variance}, \code{ci_lower}, \code{ci_upper}, \code{t_quantile}, \code{df}, \code{relative_increase_variance}, \code{fraction_missing_info}, \code{per_imputation}, \code{complete_case_coefficients}, \code{complete_case_se}, \code{n_complete_cases}, \code{n}, \code{p}, \code{m}, \code{cycles}, \code{n_missing}, \code{columns_imputed}, \code{n_events}, \code{df_complete}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
morie_sschin_chained_imputation(time = c(1, 2, 3, 4, 5, 6, 7, 8), event = c(0, 1, 0, 1, 1, 0, 1, 0), X = c(1, 2, 3, 4, 5, 6, 7, 8))
}
