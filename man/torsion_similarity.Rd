% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{torsion_similarity}
\alias{torsion_similarity}
\title{The paper\'s similarity score S = 2 D / (d_i + d_j)}
\usage{
torsion_similarity(t1, t2) 
}
\arguments{
\item{t1}{First torsion dictionary (or iterable of codes).}
\item{t2}{Second torsion dictionary (or iterable of codes).}
}
\description{
The paper\'s similarity score S = 2 D / (d_i + d_j)
}
\value{
Scalar similarity in [0, 1].
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
torsion_similarity(V, V)
}
