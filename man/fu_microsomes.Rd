% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{fu_microsomes}
\alias{fu_microsomes}
\title{fu_microsomes}
\usage{
fu_microsomes(log_pd, protein = 1)
}
\arguments{
\item{log_pd}{Passed to \code{.clrnt_binding_term}.}
\item{protein}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
}
\description{
A step of the clrnt_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
fu_microsomes(V)
}
