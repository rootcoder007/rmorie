% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{friend_summary}
\alias{friend_summary}
\title{Summarise a quantity over :math:`F_i`.}
\usage{
friend_summary(values, friends, kind = "fraction")
}
\description{
Summarise a quantity over :math:`F_i`.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
friend_summary(V, V)
}
