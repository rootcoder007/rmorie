% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.cmlmer_logdet}
\alias{.cmlmer_logdet}
\title{.cmlmer_logdet}
\usage{
.cmlmer_logdet(L)
}
\arguments{
\item{L}{A matrix; passed to \code{diag}.}
}
\description{
A step of the cmlmer_native implementation. Called by \code{.cmlmer_reml_at}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.cmlmer_logdet(V)
}
