% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{smfd_difference_matrix}
\alias{smfd_difference_matrix}
\title{Internal: the d-th order difference matrix, built by iterated}
\usage{
smfd_difference_matrix(p, order = 2L)
}
\arguments{
\item{p}{A matrix; passed to \code{diag}.}
\item{order}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{2L}.}
}
\description{
first differences starting from the identity. Mirrors Python difference_matrix(p, order) in smfd.
}
\value{
The value of \code{D}, as built in the body.
}
\keyword{internal}
\examples{
smfd_difference_matrix(10L, order = 2L)
}
