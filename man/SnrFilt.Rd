% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{SnrFilt}
\alias{SnrFilt}
\title{The power form of Section 3.2.1 applied to the residual against a}
\usage{
SnrFilt(clean, filtered)
}
\arguments{
\item{clean}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{filtered}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
known clean reference.  This penalises distortion as well as leftover noise -- comparing only noise power would flatter an over-smoothing filter.
}
\value{
A list with \code{snr_db}, \code{residual_power}, \code{signal_power}, \code{residual}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
SnrFilt(V, V)
}
