% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{BWanderZ}
\alias{BWanderZ}
\title{Eq (3.133): the same filter in positive powers of z.  The book keeps}
\usage{
BWanderZ(z, T = 1, pole = 0.995)
}
\arguments{
\item{z}{See Usage.}
\item{T}{Defaults to \code{1}.}
\item{pole}{Defaults to \code{0.995}.}
}
\description{
this form because the graphical method reads it directly -- numerator is the distance to the zero at 1, denominator the distance to the pole.
}
\value{
A list with \code{H}, \code{z}, \code{T}, \code{pole}, \code{max_difference_from_eq_3_132}, \code{forms_agree}, \code{numerator_is_the_distance_to_the_zero}, \code{denominator_is_the_distance_to_the_pole}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
BWanderZ(V)
}
