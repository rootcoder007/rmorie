% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_dreamr_value_update}
\alias{morie_dreamr_value_update}
\title{morie_dreamr_value_update}
\usage{
morie_dreamr_value_update(values, targets)
}
\arguments{
\item{values}{Passed to \code{.dreamr_vec}.}
\item{targets}{Passed to \code{.dreamr_vec}.}
}
\description{
A step of the dreamr_native implementation. Called by \code{morie_dreamr}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{loss}, \code{residual}, \code{grad}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_dreamr_value_update(V, V)
}
