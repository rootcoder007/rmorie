% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{narm_session_repr}
\alias{narm_session_repr}
\title{narm_session_repr}
\usage{
narm_session_repr(h_t_global, c_local) 
}
\arguments{
\item{h_t_global}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{c_local}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the narm_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
narm_session_repr(V, V)
}
