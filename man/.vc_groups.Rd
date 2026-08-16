% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.vc_groups}
\alias{.vc_groups}
\title{.vc_groups}
\usage{
.vc_groups(y, group)
}
\arguments{
\item{y}{A vector; indexed elementwise.}
\item{group}{Coerced to character by the body, with \code{as.character}.}
}
\description{
A step of the ranova_native implementation. Called by \code{morie_ranova}. See the file header for the source the module follows.
}
\value{
A list with \code{keys}, \code{gs}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.vc_groups(V, V)
}
