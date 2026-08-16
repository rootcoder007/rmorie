% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.b1_logit}
\alias{.b1_logit}
\title{.b1_logit}
\usage{
.b1_logit(p)
}
\arguments{
\item{p}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the b1_tmle implementation. Called by \code{.b1_target}, \code{Tmlecat}, \code{Tmlecde}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.b1_logit(V)
}
