% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_dimNet_angle_between}
\alias{morie_dimNet_angle_between}
\title{Angle at j between the bonds j-k and j-i}
\usage{
morie_dimNet_angle_between(r_k, r_j, r_i)
}
\arguments{
\item{r_k}{Numeric position of atom k.}
\item{r_j}{Numeric position of atom j.}
\item{r_i}{Numeric position of atom i.}
}
\description{
Two configurations can share every pairwise distance in the message\'s own pair and differ here -- which is precisely what a distance-only model cannot see.
}
\value{
Numeric angle in radians.
}
\keyword{internal}
\examples{
morie_dimNet_angle_between(r_k = c(1, 2, 3, 4, 5, 6, 7, 8), r_j = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), r_i = c(1, 2, 3, 4, 5, 6, 7, 8))
}
