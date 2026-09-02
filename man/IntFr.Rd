% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{IntFr}
\alias{IntFr}
\title{Eq (3.116): H(w) = 1/(jw), the DC term of eq (3.115) set aside as the}
\usage{
IntFr(omega) 
}
\arguments{
\item{omega}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
book does.  The gain falls as frequency rises, so it is a lowpass, and it is unbounded at w = 0 -- refused rather than returned as Inf.
}
\value{
A list with \code{H}, \code{omega}, \code{lowpass}, \code{dc_term_set_aside}, \code{gain_falls_nonlinearly_with_frequency}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
IntFr(V)
}
