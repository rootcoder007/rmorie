% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_simplex_proj}
\alias{.morie_simplex_proj}
\title{Internal helper: Euclidean projection onto the probability simplex}
\usage{
.morie_simplex_proj(v)
}
\description{
Held-Wolfe-Crowder algorithm; exact, O(n log n).
}
\value{
The value of \code{pmax}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_simplex_proj(V)
}
