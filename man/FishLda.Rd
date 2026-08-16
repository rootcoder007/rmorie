% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{FishLda}
\alias{FishLda}
\title{Section 10.4.2: w = S_W^-1 (m1 - m2), the direction maximizing the}
\usage{
FishLda(X, y)
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{y}{A vector; its length is taken.}
}
\description{
ratio of between- to within-class scatter of the PROJECTED data.  Two classes only.  It reduces the data to ONE number chosen for separation, not reconstruction, so unlike a principal component it is not meant to represent the data.
}
\value{
A list with \code{w}, \code{threshold}, \code{classes}, \code{means}, \code{s_within}, \code{projected}, \code{projected_means}, \code{criterion}, \code{two_class_only}, \code{not_a_reconstruction_basis}, \code{method}.
}
\keyword{internal}
