% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{tie_probability}
\alias{tie_probability}
\title{Tie probability for two consecutive draws}
\usage{
tie_probability(alpha)
}
\arguments{
\item{alpha}{Concentration, positive.}
}
\description{
The probability that the second draw from the urn equals the first, \code{1/(1+alpha)}, and the complementary probability that the second draw opens a new cluster. This closed form falls straight out of the urn at \code{n=1} and is the anchor that fails if the two mixture weights in \code{urn_weights} are ever swapped.
}
\value{
A list with \code{tie}, \code{new}, \code{alpha}.
}
\keyword{internal}
