% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.phylby_clade_credibility}
\alias{.phylby_clade_credibility}
\title{.phylby_clade_credibility}
\usage{
.phylby_clade_credibility(samples)
}
\arguments{
\item{samples}{A vector; its length is taken.}
}
\description{
A step of the phylby_native implementation. Called by \code{morie_phylby_clade_credibility}. See the file header for the source the module follows.
}
\value{
The value of \code{result}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.phylby_clade_credibility(V)
}
