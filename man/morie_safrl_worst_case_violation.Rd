% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_safrl_worst_case_violation}
\alias{morie_safrl_worst_case_violation}
\title{morie_safrl_worst_case_violation}
\usage{
morie_safrl_worst_case_violation(delta, gamma, epsilon)
}
\arguments{
\item{delta}{Numeric; combined arithmetically in the body.}
\item{gamma}{Numeric; combined arithmetically in the body.}
\item{epsilon}{See Usage.}
}
\description{
A step of the safrl_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
morie_safrl_worst_case_violation(delta = 0.5, gamma = 0.5, epsilon = c(1, 2, 3, 4, 5, 6, 7, 8))
}
