% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.b1_expit}
\alias{.b1_expit}
\title{.b1_expit}
\usage{
.b1_expit(x)
}
\arguments{
\item{x}{Numeric; passed to \code{exp}.}
}
\description{
A step of the b1_tmle implementation. Called by \code{.b1_target}, \code{Tmlecat}, \code{Tmlecde}. See the file header for the source the module follows.
}
\value{
The value of \code{ifelse}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.b1_expit(V)
}
