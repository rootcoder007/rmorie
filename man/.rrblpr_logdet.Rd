% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rrblpr_logdet}
\alias{.rrblpr_logdet}
\title{.rrblpr_logdet}
\usage{
.rrblpr_logdet(L)
}
\arguments{
\item{L}{A matrix; passed to \code{diag}.}
}
\description{
A step of the rrblpr_native implementation. Called by \code{.rrblpr_reml_at}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.rrblpr_logdet(V)
}
