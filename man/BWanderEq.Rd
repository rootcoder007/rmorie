% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{BWanderEq}
\alias{BWanderEq}
\title{Baseline-wander removal as a difference equation}
\usage{
BWanderEq(x, T = 1, pole = 0.995, n = NULL) 
}
\arguments{
\item{x}{The input record, at least one sample.}
\item{T}{The sampling interval, which must be positive.}
\item{pole}{The pole radius, at least zero and strictly below one.}
\item{n}{Optional zero-based index whose filtered value is wanted; it   must lie inside the record.}
}
\description{
Equation (3.134), applied sample by sample. Note the PLUS on the feedback term: the pole\'s coefficient is already on the right-hand side, so this does not carry the minus sign of the general form in equation (3.68). Taking the sign from equation (3.68) instead puts the pole at minus the radius -- a highpass at Nyquist rather than at direct current, which is the opposite of what the filter is for.
}
\value{
A list with the filtered record \code{y}, the value at the   requested index if one was asked for, and the parameters used.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
BWanderEq(V)
}
