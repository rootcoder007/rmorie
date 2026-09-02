% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{round_trip}
\alias{round_trip}
\title{What the two conversions cost when chained}
\usage{
round_trip(epsilon, delta) 
}
\arguments{
\item{epsilon}{Pure DP epsilon, non-negative.}
\item{delta}{Failure probability for the conversion back to DP.}
}
\description{
\code{epsilon -> rho -> (epsilon\', delta)} does not return \code{epsilon}; the gap is reported as \code{inflation} rather than quietly absorbed. When the input is \code{epsilon = 0} the \code{to_approx_dp} step is skipped (it would refuse rho = 0) and \code{epsilon_out} is set to 0.
}
\value{
A list with \code{epsilon_in}, \code{rho},   \code{epsilon_out}, \code{inflation}, \code{delta}.
}
\keyword{internal}
\examples{
round_trip(epsilon = 5L, delta = 0.5)
}
