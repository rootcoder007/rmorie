% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{easy_only_fit}
\alias{easy_only_fit}
\title{easy_only_fit}
\usage{
easy_only_fit(X, y, difficulty, X_test, y_test, quantile = 0.5, updates = 200, 
    seed = 0, n_repeats = 50) 
}
\arguments{
\item{X}{Passed to \code{.prgrl_to_rows}.}
\item{y}{Passed to \code{.prgrl_to_vec}.}
\item{difficulty}{Passed to \code{.prgrl_to_vec}.}
\item{X_test}{Passed to \code{.prgrl_to_rows}.}
\item{y_test}{Passed to \code{.prgrl_to_vec}.}
\item{quantile}{Numeric; combined arithmetically in the body. Defaults to \code{0.5}.}
\item{updates}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200}.}
\item{seed}{Passed to \code{.prgrl_rng}. Defaults to \code{0}.}
\item{n_repeats}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{50}.}
}
\description{
A step of the prgrl_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{easy_only_error}, \code{all_examples_error}, \code{improvement}, \code{n_kept}, \code{n}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
easy_only_fit(X = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), difficulty = c(1, 2, 3, 4, 5, 6, 7, 8), X_test = c(1, 2, 3, 4, 5, 6, 7, 8), y_test = c(1, 2, 3, 4, 5, 6, 7, 8))
}
