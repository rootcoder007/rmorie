% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_dp_check_budget}
\alias{.morie_dp_check_budget}
\title{.morie_dp_check_budget}
\usage{
.morie_dp_check_budget(epsilon, delta = NULL)
}
\arguments{
\item{epsilon}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{delta}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the dp_native implementation. Called by \code{morie_dp_changepoint}, \code{morie_dp_count}, \code{morie_dp_covariance} and 12 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{epsilon}, \code{delta}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_dp_check_budget(V)
}
