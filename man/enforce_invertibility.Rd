% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{enforce_invertibility}
\alias{enforce_invertibility}
\title{Project \code{u} so that \code{u\'w >= -1}}
\usage{
enforce_invertibility(u, w) 
}
\arguments{
\item{u,w}{Numeric vectors of equal length.}
}
\description{
Outside this region the planar flow is not invertible and the change-of-variables formula is simply wrong, so the reported bound is not a bound. The projection follows the Python arm exactly.
}
\value{
A list with \code{u, adjusted, u_dot_w, u_dot_w_after, note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
enforce_invertibility(V, V)
}
