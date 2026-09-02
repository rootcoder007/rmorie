% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{FDiffPh}
\alias{FDiffPh}
\title{Eq (3.127): slope -1/2, so half a sample of group delay plus the}
\usage{
FDiffPh(omega) 
}
\arguments{
\item{omega}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
quarter turn from the j of eq (3.125).  A half-sample delay cannot be undone by shifting samples.
}
\value{
A list with \code{phase}, \code{omega}, \code{group_delay}, \code{slope}, \code{quarter_turn_offset}, \code{linear_phase}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
FDiffPh(V)
}
