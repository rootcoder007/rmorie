% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.new_permutation_test_result}
\alias{.new_permutation_test_result}
\title{Internal helper: New Permutation Test Result}
\usage{
.new_permutation_test_result(observed_statistic, p_value, null_distribution, n_permutations, alternative, ci_lower = NA_real_, ci_upper = NA_real_)
}
\description{
Internal helper: New Permutation Test Result
}
\value{
The value of \code{structure}.
}
\keyword{internal}
\examples{
.new_permutation_test_result(observed_statistic = c(1, 2, 3, 4, 5, 6, 7, 8), p_value = c(1, 2, 3, 4, 5, 6, 7, 8), null_distribution = c(1, 2, 3, 4, 5, 6, 7, 8), n_permutations = c(1, 2, 3, 4, 5, 6, 7, 8), alternative = c(1, 2, 3, 4, 5, 6, 7, 8))
}
