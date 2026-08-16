% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{WienerOpt}
\alias{WienerOpt}
\title{Eq. (3.169): w_o = Phi^-1 Theta.  Written as an inverse in the book,}
\usage{
WienerOpt(phi, theta)
}
\arguments{
\item{phi}{Passed to \code{WienerHopf}.}
\item{theta}{Passed to \code{WienerHopf}.}
}
\description{
computed here by solving the system -- the inverse is never formed, which is both faster and better conditioned.
}
\value{
The value of \code{r}, as built in the body.
}
\keyword{internal}
\examples{
WienerOpt(phi = 0.5, theta = 0.5)
}
