% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.memb_sorted_features}
\alias{.memb_sorted_features}
\title{.memb_sorted_features}
\usage{
.memb_sorted_features(vec, top = NULL)
}
\arguments{
\item{vec}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{top}{Optional; may be \code{NULL}. Numeric; passed to \code{min}.}
}
\description{
A step of the memb_native implementation. Called by \code{memb}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.memb_sorted_features(V)
}
