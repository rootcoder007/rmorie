% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.haldane}
\alias{.haldane}
\title{Haldane\'s map function}
\usage{
.haldane(d)
}
\arguments{
\item{d}{Numeric, Morgans.}
}
\description{
Distance in Morgans to recombination fraction \code{r = 0.5 * (1 - exp(-2 d))} and back.
}
\value{
Recombination fraction in \code{[0, 0.5]}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.haldane(V)
}
