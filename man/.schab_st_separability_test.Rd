% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_st_separability_test}
\alias{.schab_st_separability_test}
\title{Sec. 6.2.3 states the rule outright: on the boundary of the parameter}
\usage{
.schab_st_separability_test(neg2_unrestricted, neg2_separable)
}
\arguments{
\item{neg2_unrestricted}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{neg2_separable}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
space the statistic is a mixture of a degenerate distribution at zero and a chi-square, so "simply divide the p-value ... by 2" (Self and Liang, 1987; Littell, Milliken, Stroup and Wolfinger, 1996).
}
\value{
A list with \code{statistic}, \code{p_value}, \code{p_value_naive_chi2_1}, \code{reference}.
}
\keyword{internal}
