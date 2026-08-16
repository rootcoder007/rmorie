% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{RlsApriori}
\alias{RlsApriori}
\title{Eq. (3.225): alpha(n) = x(n) - w\'(n-1) r(n).  The A PRIORI error,}
\usage{
RlsApriori(x, rvec, w_prev)
}
\arguments{
\item{x}{See Usage.}
\item{rvec}{See Usage.}
\item{w_prev}{See Usage.}
}
\description{
made with the PREVIOUS weights.  Using the updated weights gives the a posteriori error, a different and always smaller quantity, and the recursion is not valid with it.
}
\value{
A list with \code{alpha}, \code{prediction}, \code{order}, \code{uses_previous_weights}, \code{not_the_a_posteriori_error}, \code{method}.
}
\keyword{internal}
\examples{
RlsApriori(x = c(1, 2, 3, 4, 5, 6, 7, 8), rvec = c(1, 2, 3, 4, 5, 6, 7, 8), w_prev = c(1, 2, 3, 4, 5, 6, 7, 8))
}
