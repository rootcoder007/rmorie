% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{alfRap}
\alias{alfRap}
\title{A frame is list(R = 3x3 matrix, t = length-3 vector); T o x = R x + t}
\usage{
alfRap(Tf, x)
}
\arguments{
\item{Tf}{A list; the body reads \code{$R}, \code{$t} from it.}
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the alf_core implementation. Called by \code{Alfipa}, \code{alfRcomp}, \code{Alfschn}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
Tf <- list(R = diag(2), t = c(1, -1))
alfRap(Tf, c(2, 3))
}
