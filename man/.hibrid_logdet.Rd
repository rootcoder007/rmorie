% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.hibrid_logdet}
\alias{.hibrid_logdet}
\title{.hibrid_logdet}
\usage{
.hibrid_logdet(L)
}
\arguments{
\item{L}{A matrix; passed to \code{diag}.}
}
\description{
A step of the hibrid_native implementation. Called by \code{.hibrid_reml_at}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.hibrid_logdet(V)
}
