% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{edgrn_effective_library_size}
\alias{edgrn_effective_library_size}
\title{edgrn_effective_library_size}
\usage{
edgrn_effective_library_size(library_size, factor)
}
\arguments{
\item{library_size}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{factor}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the edgrn_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{effective}, \code{offset}, \code{note}.
}
\keyword{internal}
\examples{
edgrn_effective_library_size(library_size = 5L, factor = 5L)
}
